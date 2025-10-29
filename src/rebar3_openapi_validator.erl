%%%-------------------------------------------------------------------
%%% @doc Validation logic for OpenAPI specs and handler consistency
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_validator).

-export([
    validate_handler_against_spec/3,
    validate_spec/1,
    compare_specs/2,
    format_differences/1
]).

-type validation_result() :: ok | {error, [validation_error()]}.
-type validation_error() :: {error, term(), term()}.
-type difference() :: {path, binary(), present | missing, in_code | in_spec}
                    | {operation, binary(), binary(), present | missing, in_code | in_spec}
                    | {method, binary(), binary(), present | missing, in_code | in_spec}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Validate that handler implementation matches OpenAPI spec
-spec validate_handler_against_spec(string(), file:filename(), map()) ->
    validation_result().
validate_handler_against_spec(Handler, SpecFile, Opts) ->
    rebar3_openapi_utils:info("Validating handler against spec...", []),

    try
        %% Parse the OpenAPI spec
        {ok, Spec} = rebar3_openapi_parser:parse_file(SpecFile),

        %% Extract OpenAPI from handler code
        {ok, ExtractedSpec} = rebar3_openapi_extractor:extract_from_handler(Handler, Opts),

        %% Compare the two specs
        case compare_specs(Spec, ExtractedSpec) of
            {ok, []} ->
                rebar3_openapi_utils:info("✓ Handler matches spec perfectly", []),
                ok;
            {ok, Differences} ->
                rebar3_openapi_utils:warn("Handler differs from spec:", []),
                format_and_log_differences(Differences),
                {error, Differences};
            {error, Reason} ->
                {error, [{validation_error, Reason}]}
        end
    catch
        _:Error:Stack ->
            rebar3_openapi_utils:error("Validation failed: ~p", [Error]),
            rebar3_openapi_utils:debug("Stack: ~p", [Stack]),
            {error, [{exception, Error, Stack}]}
    end.

%% @doc Validate OpenAPI spec structure
-spec validate_spec(map()) -> validation_result().
validate_spec(Spec) ->
    case rebar3_openapi_parser:validate_spec(Spec) of
        ok ->
            %% Additional semantic validation
            validate_paths(Spec),
            validate_operations(Spec),
            ok;
        Error ->
            Error
    end.

%% @doc Compare two OpenAPI specs and find differences
-spec compare_specs(map(), map()) -> {ok, [difference()]} | {error, term()}.
compare_specs(Spec1, Spec2) ->
    try
        Differences = lists:flatten([
            compare_paths(Spec1, Spec2),
            compare_operations(Spec1, Spec2)
        ]),
        {ok, Differences}
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc Format differences for display
-spec format_differences([difference()]) -> iolist().
format_differences(Differences) ->
    lists:map(fun format_difference/1, Differences).

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

-spec validate_paths(map()) -> ok.
validate_paths(Spec) ->
    Paths = rebar3_openapi_parser:get_paths(Spec),
    maps:foreach(
        fun(Path, PathItem) ->
            validate_path(Path, PathItem)
        end,
        Paths
    ),
    ok.

-spec validate_path(binary(), map()) -> ok.
validate_path(_Path, PathItem) when is_map(PathItem) ->
    %% Validate that path has at least one operation
    Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>],
    HasOperation = lists:any(
        fun(Method) -> maps:is_key(Method, PathItem) end,
        Methods
    ),
    case HasOperation of
        true -> ok;
        false ->
            rebar3_openapi_utils:warn("Path has no operations: ~p", [_Path]),
            ok
    end;
validate_path(_Path, _) ->
    ok.

-spec validate_operations(map()) -> ok.
validate_operations(Spec) ->
    Operations = rebar3_openapi_parser:get_operations(Spec),
    lists:foreach(
        fun({Path, Method, Op}) ->
            validate_operation(Path, Method, Op)
        end,
        Operations
    ),
    ok.

-spec validate_operation(binary(), binary(), map()) -> ok.
validate_operation(Path, Method, Op) ->
    %% Check for operationId
    case maps:get(<<"operationId">>, Op, undefined) of
        undefined ->
            rebar3_openapi_utils:warn(
                "Operation missing operationId: ~s ~s",
                [Method, Path]
            );
        _ ->
            ok
    end,

    %% Check for responses
    case maps:get(<<"responses">>, Op, undefined) of
        undefined ->
            rebar3_openapi_utils:warn(
                "Operation missing responses: ~s ~s",
                [Method, Path]
            );
        Responses when map_size(Responses) =:= 0 ->
            rebar3_openapi_utils:warn(
                "Operation has empty responses: ~s ~s",
                [Method, Path]
            );
        _ ->
            ok
    end,
    ok.

%%%===================================================================
%%% Internal Functions - Comparison
%%%===================================================================

-spec compare_paths(map(), map()) -> [difference()].
compare_paths(Spec1, Spec2) ->
    Paths1 = maps:keys(rebar3_openapi_parser:get_paths(Spec1)),
    Paths2 = maps:keys(rebar3_openapi_parser:get_paths(Spec2)),

    %% Find paths in spec but not in code
    InSpecOnly = ordsets:subtract(ordsets:from_list(Paths1), ordsets:from_list(Paths2)),

    %% Find paths in code but not in spec
    InCodeOnly = ordsets:subtract(ordsets:from_list(Paths2), ordsets:from_list(Paths1)),

    [{path, Path, missing, in_code} || Path <- InSpecOnly] ++
    [{path, Path, present, in_code} || Path <- InCodeOnly].

-spec compare_operations(map(), map()) -> [difference()].
compare_operations(Spec1, Spec2) ->
    Ops1 = rebar3_openapi_parser:get_operations(Spec1),
    Ops2 = rebar3_openapi_parser:get_operations(Spec2),

    %% Create operation keys for comparison
    OpKeys1 = ordsets:from_list([{Path, Method} || {Path, Method, _} <- Ops1]),
    OpKeys2 = ordsets:from_list([{Path, Method} || {Path, Method, _} <- Ops2]),

    %% Find operations in spec but not in code
    InSpecOnly = ordsets:subtract(OpKeys1, OpKeys2),

    %% Find operations in code but not in spec
    InCodeOnly = ordsets:subtract(OpKeys2, OpKeys1),

    [{operation, Path, Method, missing, in_code} || {Path, Method} <- InSpecOnly] ++
    [{operation, Path, Method, present, in_code} || {Path, Method} <- InCodeOnly].

%%%===================================================================
%%% Internal Functions - Formatting
%%%===================================================================

-spec format_difference(difference()) -> iolist().
format_difference({path, Path, missing, in_code}) ->
    io_lib:format("  - Path in spec but not implemented: ~s~n", [Path]);
format_difference({path, Path, present, in_code}) ->
    io_lib:format("  + Path implemented but not in spec: ~s~n", [Path]);
format_difference({operation, Path, Method, missing, in_code}) ->
    io_lib:format("  - Operation in spec but not implemented: ~s ~s~n",
                  [string:uppercase(binary_to_list(Method)), Path]);
format_difference({operation, Path, Method, present, in_code}) ->
    io_lib:format("  + Operation implemented but not in spec: ~s ~s~n",
                  [string:uppercase(binary_to_list(Method)), Path]);
format_difference({method, Path, Method, missing, in_code}) ->
    io_lib:format("  - Method in spec but not implemented: ~s ~s~n",
                  [string:uppercase(binary_to_list(Method)), Path]);
format_difference({method, Path, Method, present, in_code}) ->
    io_lib:format("  + Method implemented but not in spec: ~s ~s~n",
                  [string:uppercase(binary_to_list(Method)), Path]);
format_difference(Other) ->
    io_lib:format("  ? Unknown difference: ~p~n", [Other]).

-spec format_and_log_differences([difference()]) -> ok.
format_and_log_differences(Differences) ->
    FormattedList = format_differences(Differences),
    lists:foreach(
        fun(Line) ->
            rebar3_openapi_utils:warn("~s", [Line])
        end,
        FormattedList
    ),
    ok.

%%%===================================================================
%%% Internal Functions - Strict Validation
%%%===================================================================

%% @doc Strict validation mode for CI
-spec strict_validate(map(), map()) -> ok | {error, [difference()]}.
strict_validate(Spec, ExtractedSpec) ->
    case compare_specs(Spec, ExtractedSpec) of
        {ok, []} ->
            ok;
        {ok, Differences} ->
            {error, Differences};
        Error ->
            Error
    end.

