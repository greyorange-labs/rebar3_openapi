%%%-------------------------------------------------------------------
%%% @doc Extract OpenAPI specification from Erlang handler code
%%% Parses Erlang modules and generates OpenAPI spec
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_extractor).

-export([
    extract_from_handler/2,
    extract_from_module/1,
    build_openapi_spec/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Extract OpenAPI spec from handler module
-spec extract_from_handler(string(), map()) -> {ok, map()} | {error, term()}.
extract_from_handler(HandlerModule, Opts) ->
    try
        %% Find and parse the handler file
        case find_handler_file(HandlerModule, Opts) of
            {ok, FilePath} ->
                extract_from_module(FilePath);
            Error ->
                Error
        end
    catch
        _:ErrorReason:Stack ->
            rebar3_openapi_utils:error("Extraction failed: ~p", [ErrorReason]),
            rebar3_openapi_utils:debug("Stack: ~p", [Stack]),
            {error, ErrorReason}
    end.

%% @doc Extract OpenAPI information from a module file
-spec extract_from_module(file:filename()) -> {ok, map()} | {error, term()}.
extract_from_module(FilePath) ->
    rebar3_openapi_utils:info("Extracting OpenAPI from: ~s", [FilePath]),

    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Parse the module
            case parse_module(Content) of
                {ok, ModuleInfo} ->
                    %% Build OpenAPI spec from extracted info
                    Spec = build_openapi_spec(ModuleInfo, #{}),
                    {ok, Spec};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Build OpenAPI spec from extracted module information
-spec build_openapi_spec(map(), map()) -> map().
build_openapi_spec(ModuleInfo, _Opts) ->
    Paths = maps:get(paths, ModuleInfo, []),
    Operations = maps:get(operations, ModuleInfo, []),

    %% Build paths section
    PathsMap = build_paths_section(Paths, Operations),

    %% Build basic OpenAPI structure
    #{
        <<"openapi">> => <<"3.0.0">>,
        <<"info">> => #{
            <<"title">> => maps:get(title, ModuleInfo, <<"Generated API">>),
            <<"version">> => maps:get(version, ModuleInfo, <<"1.0.0">>),
            <<"description">> => maps:get(description, ModuleInfo,
                <<"Generated from Erlang handler">>)
        },
        <<"servers">> => [
            #{<<"url">> => <<"http://localhost:8080">>}
        ],
        <<"paths">> => PathsMap,
        <<"components">> => #{
            <<"schemas">> => maps:get(schemas, ModuleInfo, #{})
        }
    }.

%%%===================================================================
%%% Internal Functions - Module Parsing
%%%===================================================================

-spec parse_module(binary()) -> {ok, map()} | {error, term()}.
parse_module(Content) ->
    %% Convert binary to string for parsing
    Code = binary_to_list(Content),

    %% Try to parse as Erlang forms
    case erl_scan:string(Code) of
        {ok, Tokens, _} ->
            parse_forms(Tokens);
        {error, ErrorInfo, _} ->
            {error, {scan_error, ErrorInfo}}
    end.

-spec parse_forms(list()) -> {ok, map()} | {error, term()}.
parse_forms(Tokens) ->
    %% For now, extract basic information using pattern matching
    %% Full AST parsing would be more robust

    %% Extract module info
    ModuleInfo = #{
        title => <<"Generated API">>,
        version => <<"1.0.0">>,
        paths => [],
        operations => [],
        schemas => #{}
    },

    %% Look for trails() function or route definitions
    Paths = extract_paths_from_tokens(Tokens),
    Operations = extract_operations_from_tokens(Tokens),

    {ok, ModuleInfo#{
        paths => Paths,
        operations => Operations
    }}.

-spec extract_paths_from_tokens(list()) -> [map()].
extract_paths_from_tokens(_Tokens) ->
    %% Simplified extraction - would parse AST in production
    %% Look for patterns like: {<<"/api/path">>, handler, opts}
    [].

-spec extract_operations_from_tokens(list()) -> [map()].
extract_operations_from_tokens(_Tokens) ->
    %% Simplified extraction - would parse function definitions
    %% Look for patterns like: handle_api_* functions
    [].

%%%===================================================================
%%% Internal Functions - Path Building
%%%===================================================================

-spec build_paths_section([map()], [map()]) -> map().
build_paths_section(Paths, Operations) ->
    %% Build OpenAPI paths from extracted information
    lists:foldl(
        fun(PathInfo, Acc) ->
            Path = maps:get(path, PathInfo, <<"/">>),
            Methods = maps:get(methods, PathInfo, [<<"get">>]),

            %% Build operations for each method
            PathItem = lists:foldl(
                fun(Method, PathAcc) ->
                    Op = find_operation_for_method(Method, Operations),
                    maps:put(Method, build_operation(Op), PathAcc)
                end,
                #{},
                Methods
            ),

            maps:put(Path, PathItem, Acc)
        end,
        #{},
        Paths
    ).

-spec find_operation_for_method(binary(), [map()]) -> map().
find_operation_for_method(Method, Operations) ->
    case lists:search(
        fun(Op) -> maps:get(method, Op, undefined) =:= Method end,
        Operations
    ) of
        {value, Op} -> Op;
        false -> #{method => Method}
    end.

-spec build_operation(map()) -> map().
build_operation(OpInfo) ->
    #{
        <<"summary">> => maps:get(summary, OpInfo, <<"API operation">>),
        <<"description">> => maps:get(description, OpInfo, <<>>),
        <<"operationId">> => maps:get(operation_id, OpInfo, <<"operation">>),
        <<"responses">> => #{
            <<"200">> => #{
                <<"description">> => <<"Success">>,
                <<"content">> => #{
                    <<"application/json">> => #{
                        <<"schema">> => #{
                            <<"type">> => <<"object">>
                        }
                    }
                }
            }
        }
    }.

%%%===================================================================
%%% Internal Functions - File Finding
%%%===================================================================

-spec find_handler_file(string(), map()) -> {ok, file:filename()} | {error, term()}.
find_handler_file(HandlerModule, Opts) ->
    %% Try to find the handler file
    SearchDirs = case maps:get(app_dir, Opts, undefined) of
        undefined -> ["src", "apps/*/src"];
        AppDir -> [filename:join(AppDir, "src")]
    end,

    FileName = HandlerModule ++ ".erl",

    case find_file_in_dirs(FileName, SearchDirs) of
        {ok, Path} ->
            {ok, Path};
        error ->
            {error, {handler_not_found, HandlerModule}}
    end.

-spec find_file_in_dirs(file:filename(), [file:filename()]) -> {ok, file:filename()} | error.
find_file_in_dirs(FileName, Dirs) ->
    find_file_in_dirs(FileName, Dirs, []).

-spec find_file_in_dirs(file:filename(), [file:filename()], [file:filename()]) ->
    {ok, file:filename()} | error.
find_file_in_dirs(_FileName, [], _Checked) ->
    error;
find_file_in_dirs(FileName, [Dir | Rest], Checked) ->
    %% Expand wildcards if present
    case lists:member($*, Dir) of
        true ->
            %% Expand wildcard
            case filelib:wildcard(Dir) of
                [] ->
                    find_file_in_dirs(FileName, Rest, Checked);
                ExpandedDirs ->
                    find_file_in_dirs(FileName, ExpandedDirs ++ Rest, Checked)
            end;
        false ->
            Path = filename:join(Dir, FileName),
            case filelib:is_file(Path) of
                true ->
                    {ok, Path};
                false ->
                    find_file_in_dirs(FileName, Rest, [Dir | Checked])
            end
    end.

%%%===================================================================
%%% Internal Functions - Schema Extraction
%%%===================================================================

%% @doc Extract Jesse schemas from priv directories
-spec extract_schemas(string()) -> map().
extract_schemas(AppDir) ->
    PrivDir = filename:join(AppDir, "priv"),

    case filelib:is_dir(PrivDir) of
        true ->
            %% Find all JSON schema files
            JsonFiles = filelib:wildcard(filename:join(PrivDir, "**/*.json")),

            %% Parse and convert each schema
            lists:foldl(
                fun(File, Acc) ->
                    case parse_jesse_schema(File) of
                        {ok, SchemaName, Schema} ->
                            maps:put(SchemaName, Schema, Acc);
                        {error, _} ->
                            Acc
                    end
                end,
                #{},
                JsonFiles
            );
        false ->
            #{}
    end.

-spec parse_jesse_schema(file:filename()) -> {ok, binary(), map()} | {error, term()}.
parse_jesse_schema(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            try
                Schema = jsx:decode(Content, [return_maps]),
                %% Extract schema name from file or $id field
                Name = case maps:get(<<"$id">>, Schema, undefined) of
                    undefined ->
                        list_to_binary(filename:basename(File, ".json"));
                    Id ->
                        Id
                end,
                {ok, Name, convert_jesse_to_openapi(Schema)}
            catch
                _:_ ->
                    {error, {invalid_schema, File}}
            end;
        Error ->
            Error
    end.

%% @doc Convert Jesse JSON Schema to OpenAPI schema
-spec convert_jesse_to_openapi(map()) -> map().
convert_jesse_to_openapi(JesseSchema) ->
    %% Basic conversion - extend as needed
    maps:fold(
        fun
            (<<"$schema">>, _, Acc) -> Acc;  % Remove JSON Schema meta-fields
            (<<"$id">>, _, Acc) -> Acc;
            (<<"definitions">>, Defs, Acc) ->
                %% Convert definitions to components/schemas reference style
                maps:put(<<"definitions">>, convert_definitions(Defs), Acc);
            (Key, Value, Acc) when is_map(Value) ->
                maps:put(Key, convert_jesse_to_openapi(Value), Acc);
            (Key, Value, Acc) ->
                maps:put(Key, Value, Acc)
        end,
        #{},
        JesseSchema
    ).

-spec convert_definitions(map()) -> map().
convert_definitions(Definitions) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            maps:put(Key, convert_jesse_to_openapi(Value), Acc)
        end,
        #{},
        Definitions
    ).

