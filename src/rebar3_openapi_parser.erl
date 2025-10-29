%%%-------------------------------------------------------------------
%%% @doc OpenAPI specification parser
%%% Parses YAML and JSON OpenAPI specs
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_parser).

-export([
    parse_spec/1,
    parse_file/1,
    get_info/1,
    get_paths/1,
    get_schemas/1,
    get_operations/1,
    validate_spec/1
]).

-type openapi_spec() :: map().
-type path_item() :: map().
-type operation() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Parse OpenAPI spec from file
-spec parse_file(file:filename()) -> {ok, openapi_spec()} | {error, term()}.
parse_file(File) ->
    case filelib:is_file(File) of
        false ->
            {error, {missing_spec, File}};
        true ->
            case filename:extension(File) of
                Ext when Ext =:= ".yaml"; Ext =:= ".yml" ->
                    parse_yaml_file(File);
                ".json" ->
                    parse_json_file(File);
                Other ->
                    {error, {unsupported_format, Other}}
            end
    end.

%% @doc Parse OpenAPI spec from binary content
-spec parse_spec({yaml, binary()} | {json, binary()}) -> {ok, openapi_spec()} | {error, term()}.
parse_spec({yaml, Content}) ->
    parse_yaml(Content);
parse_spec({json, Content}) ->
    parse_json(Content).

%% @doc Get info section from spec
-spec get_info(openapi_spec()) -> map().
get_info(Spec) ->
    maps:get(<<"info">>, Spec, #{}).

%% @doc Get paths from spec
-spec get_paths(openapi_spec()) -> map().
get_paths(Spec) ->
    maps:get(<<"paths">>, Spec, #{}).

%% @doc Get schemas from spec
-spec get_schemas(openapi_spec()) -> map().
get_schemas(Spec) ->
    Components = maps:get(<<"components">>, Spec, #{}),
    maps:get(<<"schemas">>, Components, #{}).

%% @doc Extract all operations from spec with metadata
-spec get_operations(openapi_spec()) -> [{Path :: binary(), Method :: binary(), operation()}].
get_operations(Spec) ->
    Paths = get_paths(Spec),
    lists:flatmap(
        fun({Path, PathItem}) ->
            Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>,
                      <<"patch">>, <<"options">>, <<"head">>],
            lists:filtermap(
                fun(Method) ->
                    case maps:find(Method, PathItem) of
                        {ok, Operation} ->
                            {true, {Path, Method, Operation}};
                        error ->
                            false
                    end
                end,
                Methods
            )
        end,
        maps:to_list(Paths)
    ).

%% @doc Validate OpenAPI spec structure
-spec validate_spec(openapi_spec()) -> ok | {error, term()}.
validate_spec(Spec) when is_map(Spec) ->
    %% Check for required fields
    RequiredFields = [<<"openapi">>, <<"info">>, <<"paths">>],
    case check_required_fields(Spec, RequiredFields) of
        ok ->
            %% Validate OpenAPI version
            case maps:get(<<"openapi">>, Spec, undefined) of
                <<"3.", _/binary>> -> ok;
                Other -> {error, {unsupported_version, Other}}
            end;
        Error ->
            Error
    end;
validate_spec(_) ->
    {error, invalid_spec_format}.

%%%===================================================================
%%% Internal Functions - YAML Parsing
%%%===================================================================

-spec parse_yaml_file(file:filename()) -> {ok, openapi_spec()} | {error, term()}.
parse_yaml_file(File) ->
    try
        %% Ensure yamerl application is started
        application:ensure_all_started(yamerl),

        [Doc | _] = yamerl_constr:file(File, [{str_node_as_binary, true}]),
        Spec = yaml_to_map(Doc),
        case validate_spec(Spec) of
            ok -> {ok, Spec};
            Error -> Error
        end
    catch
        _:Reason ->
            {error, {invalid_yaml, File, Reason}}
    end.

-spec parse_yaml(binary()) -> {ok, openapi_spec()} | {error, term()}.
parse_yaml(Content) ->
    try
        %% Ensure yamerl application is started
        application:ensure_all_started(yamerl),

        [Doc | _] = yamerl_constr:string(binary_to_list(Content),
                                         [{str_node_as_binary, true}]),
        Spec = yaml_to_map(Doc),
        case validate_spec(Spec) of
            ok -> {ok, Spec};
            Error -> Error
        end
    catch
        _:Reason ->
            {error, {invalid_yaml, Reason}}
    end.

%% @doc Convert YAMERL document to Erlang map
-spec yaml_to_map(term()) -> map().
yaml_to_map({yaml_doc, Root}) ->
    yaml_to_map(Root);
yaml_to_map(List) when is_list(List) ->
    case is_proplist(List) of
        true ->
            maps:from_list([{K, yaml_to_map(V)} || {K, V} <- List]);
        false ->
            [yaml_to_map(Item) || Item <- List]
    end;
yaml_to_map(Other) ->
    Other.

%% @doc Check if list is a proplist (list of tuples)
-spec is_proplist(list()) -> boolean().
is_proplist([]) -> true;
is_proplist([{_, _} | Rest]) -> is_proplist(Rest);
is_proplist(_) -> false.

%%%===================================================================
%%% Internal Functions - JSON Parsing
%%%===================================================================

-spec parse_json_file(file:filename()) -> {ok, openapi_spec()} | {error, term()}.
parse_json_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            parse_json(Content);
        Error ->
            Error
    end.

-spec parse_json(binary()) -> {ok, openapi_spec()} | {error, term()}.
parse_json(Content) ->
    try
        Spec = jsx:decode(Content, [return_maps]),
        case validate_spec(Spec) of
            ok -> {ok, Spec};
            Error -> Error
        end
    catch
        _:Reason ->
            {error, {invalid_json, Reason}}
    end.

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

-spec check_required_fields(map(), [binary()]) -> ok | {error, {missing_field, binary()}}.
check_required_fields(Map, Fields) ->
    case lists:filter(fun(F) -> not maps:is_key(F, Map) end, Fields) of
        [] -> ok;
        [Missing | _] -> {error, {missing_field, Missing}}
    end.

