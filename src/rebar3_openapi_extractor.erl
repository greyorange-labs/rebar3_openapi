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
            case parse_module(Content, FilePath) of
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

-spec parse_module(binary(), file:filename()) -> {ok, map()} | {error, term()}.
parse_module(Content, FilePath) ->
    %% Convert binary to string for parsing
    Code = binary_to_list(Content),

    %% Try to parse as Erlang forms
    case erl_scan:string(Code) of
        {ok, Tokens, _} ->
            parse_forms(Tokens, FilePath);
        {error, ErrorInfo, _} ->
            {error, {scan_error, ErrorInfo}}
    end.

-spec parse_forms(list(), file:filename()) -> {ok, map()} | {error, term()}.
parse_forms(Tokens, FilePath) ->
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
    
    %% Extract schemas that are actually used in the logic handler
    Schemas = extract_schemas(FilePath),

    {ok, ModuleInfo#{
        paths => Paths,
        operations => Operations,
        schemas => Schemas
    }}.

-spec extract_paths_from_tokens(list()) -> [map()].
extract_paths_from_tokens(Tokens) ->
    %% Parse tokens to find path definitions in get_paths/1 or trails/0
    rebar3_openapi_utils:debug("Searching for get_paths/1 in ~p tokens", [length(Tokens)]),
    case find_function_body(Tokens, get_paths, 1) of
        {ok, FunctionTokens} ->
            rebar3_openapi_utils:debug("Found get_paths/1, extracting routes from ~p tokens", 
                                       [length(FunctionTokens)]),
            Routes = extract_route_tuples(FunctionTokens),
            rebar3_openapi_utils:debug("Extracted ~p routes", [length(Routes)]),
            Routes;
        {error, Reason} ->
            rebar3_openapi_utils:debug("get_paths/1 not found: ~p, trying trails/0", [Reason]),
            case find_function_body(Tokens, trails, 0) of
                {ok, FunctionTokens} ->
                    rebar3_openapi_utils:debug("Found trails/0, extracting routes", []),
                    extract_route_tuples(FunctionTokens);
                {error, Reason2} ->
                    rebar3_openapi_utils:debug("trails/0 not found: ~p", [Reason2]),
                    []
            end
    end.

-spec extract_operations_from_tokens(list()) -> [map()].
extract_operations_from_tokens(Tokens) ->
    %% Parse tokens to find provide_callback and accept_callback functions
    ProvideOps = extract_callback_operations(Tokens, provide_callback),
    AcceptOps = extract_callback_operations(Tokens, accept_callback),
    ProvideOps ++ AcceptOps.

%%%===================================================================
%%% Internal Functions - Token Parsing Helpers
%%%===================================================================

%% @doc Find a specific function body in token list
-spec find_function_body(list(), atom(), integer()) -> {ok, list()} | {error, not_found}.
find_function_body(Tokens, FuncName, Arity) ->
    %% Look for pattern: FuncName ( Args ) -> Body .
    case find_function_start(Tokens, FuncName, Arity) of
        {ok, StartIdx} ->
            %% Extract tokens from -> to the next function or end
            Body = extract_function_tokens(Tokens, StartIdx),
            {ok, Body};
        error ->
            {error, not_found}
    end.

-spec find_function_start(list(), atom(), integer()) -> {ok, integer()} | error.
find_function_start(Tokens, FuncName, Arity) ->
    find_function_start(Tokens, FuncName, Arity, 1).

-spec find_function_start(list(), atom(), integer(), integer()) -> {ok, integer()} | error.
find_function_start([], _FuncName, _Arity, _Idx) ->
    error;
find_function_start([{atom, _, FuncName} | Rest], FuncName, Arity, Idx) ->
    %% Found function name, check if arity matches
    case count_args_until_arrow(Rest, 0) of
        {ok, Arity, ArrowIdx} ->
            {ok, Idx + ArrowIdx + 1};
        _ ->
            find_function_start(Rest, FuncName, Arity, Idx + 1)
    end;
find_function_start([_ | Rest], FuncName, Arity, Idx) ->
    find_function_start(Rest, FuncName, Arity, Idx + 1).

-spec count_args_until_arrow(list(), integer()) -> {ok, integer(), integer()} | error.
count_args_until_arrow(Tokens, StartIdx) ->
    count_args_until_arrow(Tokens, 0, 0, false, StartIdx).

-spec count_args_until_arrow(list(), integer(), integer(), boolean(), integer()) -> 
    {ok, integer(), integer()} | error.
count_args_until_arrow([], _Args, _Depth, _HasContent, _Idx) ->
    error;
count_args_until_arrow([{'(', _} | Rest], Args, 0, HasContent, Idx) ->
    count_args_until_arrow(Rest, Args, 1, HasContent, Idx + 1);
count_args_until_arrow([{')', _} | Rest], Args, 1, HasContent, Idx) ->
    %% Found closing paren at depth 1, look for arrow
    case Rest of
        [{'->', _} | _] ->
            %% If we saw content but no commas, arity is 1, otherwise it's the comma count
            Arity = case {HasContent, Args} of
                {true, 0} -> 1;  % One argument, no commas
                {false, 0} -> 0; % No arguments
                {_, N} -> N + 1  % N commas means N+1 arguments
            end,
            {ok, Arity, Idx + 2};
        _ ->
            error
    end;
count_args_until_arrow([{',', _} | Rest], Args, 1, _HasContent, Idx) ->
    count_args_until_arrow(Rest, Args + 1, 1, true, Idx + 1);
count_args_until_arrow([{'(', _} | Rest], Args, Depth, HasContent, Idx) ->
    count_args_until_arrow(Rest, Args, Depth + 1, HasContent, Idx + 1);
count_args_until_arrow([{')', _} | Rest], Args, Depth, HasContent, Idx) when Depth > 1 ->
    count_args_until_arrow(Rest, Args, Depth - 1, HasContent, Idx + 1);
count_args_until_arrow([_ | Rest], Args, 1, _HasContent, Idx) ->
    %% Any non-special token at depth 1 means we have content
    count_args_until_arrow(Rest, Args, 1, true, Idx + 1);
count_args_until_arrow([_ | Rest], Args, Depth, HasContent, Idx) ->
    count_args_until_arrow(Rest, Args, Depth, HasContent, Idx + 1).

-spec extract_function_tokens(list(), integer()) -> list().
extract_function_tokens(Tokens, StartIdx) ->
    AfterArrow = lists:nthtail(StartIdx - 1, Tokens),
    extract_until_next_function(AfterArrow, []).

-spec extract_until_next_function(list(), list()) -> list().
extract_until_next_function([], Acc) ->
    lists:reverse(Acc);
extract_until_next_function([{dot, _} | Rest], Acc) ->
    %% Check if next is a new function definition
    case is_new_function(Rest) of
        true ->
            lists:reverse(Acc);
        false ->
            extract_until_next_function(Rest, [{dot, 1} | Acc])
    end;
extract_until_next_function([Token | Rest], Acc) ->
    extract_until_next_function(Rest, [Token | Acc]).

-spec is_new_function(list()) -> boolean().
is_new_function([{atom, _, _}, {'(', _} | _]) ->
    true;
is_new_function(_) ->
    false.

%% @doc Extract route tuples from function body tokens
-spec extract_route_tuples(list()) -> [map()].
extract_route_tuples(Tokens) ->
    %% Look for patterns like: {<<"/path">>, module, {operation_id, <<"METHOD">>, ...}}
    extract_tuples(Tokens, []).

-spec extract_tuples(list(), [map()]) -> [map()].
extract_tuples([], Acc) ->
    lists:reverse(Acc);
extract_tuples([{'{', _}, {'<<', _} | Rest], Acc) ->
    %% Found potential route tuple with binary string path
    case extract_binary_string(Rest) of
        {ok, PathBin, Remaining} ->
            case extract_route_info(Remaining, PathBin) of
                {ok, RouteInfo, Remaining2} ->
                    extract_tuples(Remaining2, [RouteInfo | Acc]);
                {error, Remaining2} ->
                    extract_tuples(Remaining2, Acc)
            end;
        {error, Remaining} ->
            extract_tuples(Remaining, Acc)
    end;
extract_tuples([_ | Rest], Acc) ->
    extract_tuples(Rest, Acc).

%% @doc Extract binary string from tokens like: <<"string">> 
-spec extract_binary_string(list()) -> {ok, binary(), list()} | {error, list()}.
extract_binary_string([{string, _, Str}, {'>>', _} | Rest]) ->
    {ok, list_to_binary(Str), Rest};
extract_binary_string(Rest) ->
    {error, Rest}.

-spec extract_route_info(list(), binary()) -> {ok, map(), list()} | {error, list()}.
extract_route_info([{',', _}, {atom, _, _HandlerMod}, {',', _}, {'{', _} | Rest], PathBin) ->
    %% Pattern: {Path, HandlerModule, {OperationId, Method, ...}}
    case extract_operation_tuple(Rest) of
        {ok, OpId, Method, Remaining} ->
            %% Convert method to lowercase binary for JSON encoding
            MethodLower = list_to_binary(string:lowercase(binary_to_list(Method))),
            RouteInfo = #{
                path => PathBin,
                operation_id => atom_to_binary(OpId, utf8),
                method => MethodLower,
                methods => [MethodLower]
            },
            {ok, RouteInfo, Remaining};
        error ->
            {error, Rest}
    end;
extract_route_info(Rest, _PathBin) ->
    {error, Rest}.

-spec extract_operation_tuple(list()) -> {ok, atom(), binary(), list()} | error.
extract_operation_tuple([{atom, _, OpId}, {',', _}, {'<<', _} | Rest]) ->
    %% Pattern: OpId, <<"METHOD">>, ...
    case extract_binary_string(Rest) of
        {ok, Method, Remaining} ->
            {ok, OpId, Method, Remaining};
        {error, _} ->
            error
    end;
extract_operation_tuple([{'\'', _}, {atom, _, OpId}, {'\'', _}, {',', _}, {'<<', _} | Rest]) ->
    %% Pattern: 'OpId', <<"METHOD">>, ...
    case extract_binary_string(Rest) of
        {ok, Method, Remaining} ->
            {ok, OpId, Method, Remaining};
        {error, _} ->
            error
    end;
extract_operation_tuple(_) ->
    error.

%% @doc Extract operations from callback function definitions
-spec extract_callback_operations(list(), atom()) -> [map()].
extract_callback_operations(Tokens, CallbackName) ->
    find_all_callback_clauses(Tokens, CallbackName, []).

-spec find_all_callback_clauses(list(), atom(), [map()]) -> [map()].
find_all_callback_clauses([], _CallbackName, Acc) ->
    lists:reverse(Acc);
find_all_callback_clauses([{atom, _, CallbackName}, {'(', _} | Rest], CallbackName, Acc) ->
    case extract_callback_clause(Rest) of
        {ok, OpInfo, Remaining} ->
            find_all_callback_clauses(Remaining, CallbackName, [OpInfo | Acc]);
        {error, Remaining} ->
            find_all_callback_clauses(Remaining, CallbackName, Acc)
    end;
find_all_callback_clauses([_ | Rest], CallbackName, Acc) ->
    find_all_callback_clauses(Rest, CallbackName, Acc).

-spec extract_callback_clause(list()) -> {ok, map(), list()} | {error, list()}.
extract_callback_clause([{var, _, _}, {',', _} | Rest]) ->
    %% Pattern: CallbackName(_Class, 'operation_id', Req, Context) ->
    case Rest of
        [{'\'', _}, {atom, _, OpId}, {'\'', _} | Remaining] ->
            {ok, #{operation_id => atom_to_binary(OpId, utf8), method => <<"get">>}, Remaining};
        [{atom, _, OpId} | Remaining] ->
            {ok, #{operation_id => atom_to_binary(OpId, utf8), method => <<"get">>}, Remaining};
        _ ->
            {error, Rest}
    end;
extract_callback_clause(Rest) ->
    {error, Rest}.

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
            OperationId = maps:get(operation_id, PathInfo, <<"operation">>),

            %% Build operations for each method
            PathItem = lists:foldl(
                fun(Method, PathAcc) ->
                    %% Use the operation ID from the path info
                    OpInfo = #{
                        operation_id => OperationId,
                        method => Method
                    },
                    Op = find_operation_for_method(Method, Operations),
                    %% Merge operation info with the found operation
                    MergedOp = maps:merge(Op, OpInfo),
                    maps:put(Method, build_operation(MergedOp), PathAcc)
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

%% @doc Extract Jesse schemas that are actually used in the logic handler
-spec extract_schemas(string()) -> map().
extract_schemas(HandlerFile) ->
    %% First, find the logic handler file
    BaseDir = filename:dirname(HandlerFile),
    BaseName = filename:basename(HandlerFile, ".erl"),
    
    rebar3_openapi_utils:debug("Extracting schemas for handler: ~s", [HandlerFile]),
    rebar3_openapi_utils:debug("Base dir: ~s, Base name: ~s", [BaseDir, BaseName]),
    
    %% Logic handler should be named *_logic_handler.erl
    %% Handle both _router and _handler suffixes
    LogicHandlerName = case {string:str(BaseName, "_router"), string:str(BaseName, "_handler")} of
        {Pos, _} when Pos > 0 ->
            %% Remove _router suffix
            Prefix = string:substr(BaseName, 1, Pos - 1),
            Prefix ++ "_logic_handler";
        {0, Pos} when Pos > 0 ->
            %% Remove _handler suffix (unless it already ends with _logic_handler)
            case string:str(BaseName, "_logic_handler") of
                P when P > 0 ->
                    BaseName;  % Already a logic handler
                _ ->
                    Prefix = string:substr(BaseName, 1, Pos - 1),
                    Prefix ++ "_logic_handler"
            end;
        {0, 0} ->
            %% No suffix, just add _logic_handler
            BaseName ++ "_logic_handler"
    end,
    
    LogicHandlerFile = filename:join(BaseDir, LogicHandlerName ++ ".erl"),
    rebar3_openapi_utils:debug("Looking for logic handler: ~s", [LogicHandlerFile]),
    
    case filelib:is_file(LogicHandlerFile) of
        true ->
            rebar3_openapi_utils:debug("Logic handler found, parsing...", []),
            %% Parse logic handler to find get_schema/1 calls
            case file:read_file(LogicHandlerFile) of
                {ok, Content} ->
                    UsedSchemas = extract_used_schema_names(Content),
                    rebar3_openapi_utils:debug("Found ~p used schemas: ~p", 
                                               [length(UsedSchemas), UsedSchemas]),
                    %% If no actual get_schema/1 calls found (or only false positives),
                    %% extract all schemas from priv directory
                    Schemas = case filter_valid_schema_names(UsedSchemas) of
                        [] ->
                            rebar3_openapi_utils:debug("No actual get_schema calls found, loading all schemas", []),
                            load_all_schemas(BaseDir, BaseName);
                        ValidSchemas ->
                            load_used_schemas(BaseDir, BaseName, ValidSchemas)
                    end,
                    rebar3_openapi_utils:debug("Loaded ~p schemas", [maps:size(Schemas)]),
                    Schemas;
                {error, Reason} ->
                    rebar3_openapi_utils:debug("Failed to read logic handler: ~p", [Reason]),
                    #{}
            end;
        false ->
            rebar3_openapi_utils:debug("Logic handler not found", []),
            #{}
    end.

%% @doc Extract schema names from get_schema/1 calls in the code
-spec extract_used_schema_names(binary()) -> [binary()].
extract_used_schema_names(Content) ->
    %% Look for patterns: get_schema('SchemaName') or get_schema("SchemaName")
    Code = binary_to_list(Content),
    case erl_scan:string(Code) of
        {ok, Tokens, _} ->
            find_get_schema_calls(Tokens, []);
        {error, _} ->
            []
    end.

%% @doc Find all get_schema/1 calls in tokens
-spec find_get_schema_calls(list(), [binary()]) -> [binary()].
find_get_schema_calls([], Acc) ->
    lists:usort(Acc);  % Remove duplicates
find_get_schema_calls([{atom, _, get_schema}, {'(', _} | Rest], Acc) ->
    case extract_schema_arg(Rest) of
        {ok, SchemaName, Remaining} ->
            find_get_schema_calls(Remaining, [SchemaName | Acc]);
        {error, Remaining} ->
            find_get_schema_calls(Remaining, Acc)
    end;
find_get_schema_calls([_ | Rest], Acc) ->
    find_get_schema_calls(Rest, Acc).

%% @doc Extract the argument from get_schema(Arg)
-spec extract_schema_arg(list()) -> {ok, binary(), list()} | {error, list()}.
extract_schema_arg([{'\'', _}, {atom, _, SchemaName}, {'\'', _} | Rest]) ->
    {ok, atom_to_binary(SchemaName, utf8), Rest};
extract_schema_arg([{string, _, SchemaName} | Rest]) ->
    {ok, list_to_binary(SchemaName), Rest};
extract_schema_arg([{atom, _, SchemaName} | Rest]) ->
    {ok, atom_to_binary(SchemaName, utf8), Rest};
extract_schema_arg(Rest) ->
    {error, Rest}.

%% @doc Filter out false positive schema names
-spec filter_valid_schema_names([binary()]) -> [binary()].
filter_valid_schema_names(Names) ->
    lists:filter(
        fun(Name) ->
            %% Filter out known false positives from get_schema/1 function definition
            NameStr = binary_to_list(Name),
            not lists:member(NameStr, ["atom_to_list", "SchemaName"])
        end,
        Names
    ).

%% @doc Load only the schemas that are actually used
-spec load_used_schemas(string(), string(), [binary()]) -> map().
load_used_schemas(BaseDir, HandlerBaseName, SchemaNames) ->
    PrivDir = filename:join(BaseDir, "priv"),
    
    %% Schema directory might be named after the main handler (not router)
    %% Try multiple possible directory names
    PossibleNames = case {string:str(HandlerBaseName, "_router"), 
                         string:str(HandlerBaseName, "_handler")} of
        {Pos, _} when Pos > 0 ->
            %% If it's a router file, try both the router name and the handler name
            Prefix = string:substr(HandlerBaseName, 1, Pos - 1),
            [HandlerBaseName, Prefix ++ "_handler"];
        _ ->
            [HandlerBaseName]
    end,
    
    %% Try each possible directory name
    SchemasDir = find_schemas_dir(PrivDir, PossibleNames),
    
    rebar3_openapi_utils:debug("Looking for schemas in: ~s", [SchemasDir]),
    
    case filelib:is_dir(SchemasDir) of
        true ->
            lists:foldl(
                fun(SchemaName, Acc) ->
                    SchemaFile = filename:join(SchemasDir, 
                        binary_to_list(SchemaName) ++ ".json"),
                    case parse_jesse_schema(SchemaFile) of
                        {ok, Name, Schema} ->
                            maps:put(Name, Schema, Acc);
                        {error, _} ->
                            Acc
                    end
                end,
                #{},
                SchemaNames
            );
        false ->
            rebar3_openapi_utils:debug("Schemas directory not found: ~s", [SchemasDir]),
            #{}
    end.

%% @doc Load all schemas from the priv directory
-spec load_all_schemas(string(), string()) -> map().
load_all_schemas(BaseDir, HandlerBaseName) ->
    PrivDir = filename:join(BaseDir, "priv"),
    
    %% Try to find the schemas directory
    PossibleNames = case {string:str(HandlerBaseName, "_router"), 
                         string:str(HandlerBaseName, "_handler")} of
        {Pos, _} when Pos > 0 ->
            Prefix = string:substr(HandlerBaseName, 1, Pos - 1),
            [HandlerBaseName, Prefix ++ "_handler"];
        _ ->
            [HandlerBaseName]
    end,
    
    SchemasDir = find_schemas_dir(PrivDir, PossibleNames),
    rebar3_openapi_utils:debug("Loading all schemas from: ~s", [SchemasDir]),
    
    case filelib:is_dir(SchemasDir) of
        true ->
            %% Find all JSON files in the schemas directory
            Pattern = filename:join(SchemasDir, "*.json"),
            JsonFiles = filelib:wildcard(Pattern),
            rebar3_openapi_utils:debug("Found ~p schema files", [length(JsonFiles)]),
            
            lists:foldl(
                fun(SchemaFile, Acc) ->
                    case parse_jesse_schema(SchemaFile) of
                        {ok, Name, Schema} ->
                            maps:put(Name, Schema, Acc);
                        {error, Reason} ->
                            rebar3_openapi_utils:debug("Failed to parse schema ~s: ~p", 
                                                       [SchemaFile, Reason]),
                            Acc
                    end
                end,
                #{},
                JsonFiles
            );
        false ->
            rebar3_openapi_utils:debug("Schemas directory not found: ~s", [SchemasDir]),
            #{}
    end.

%% @doc Find the schemas directory from a list of possible names
-spec find_schemas_dir(string(), [string()]) -> string().
find_schemas_dir(PrivDir, [Name | Rest]) ->
    Dir = filename:join([PrivDir, "schemas", Name]),
    case filelib:is_dir(Dir) of
        true -> Dir;
        false -> 
            case Rest of
                [] -> Dir;  % Return the last attempt
                _ -> find_schemas_dir(PrivDir, Rest)
            end
    end;
find_schemas_dir(PrivDir, []) ->
    filename:join([PrivDir, "schemas"]).

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

