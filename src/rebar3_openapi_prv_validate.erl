%%%-------------------------------------------------------------------
%%% @doc Provider for 'rebar3 openapi validate' command
%%% Validates handler implementation against OpenAPI spec
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_prv_validate).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, validate).
-define(NAMESPACE, openapi).
-define(DEPS, [{default, app_discovery}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 openapi validate --handler my_http_handler --spec spec.yml"},
        {opts, [
            {handler, $h, "handler", string, "Handler module name to validate"},
            {spec, $s, "spec", string, "OpenAPI specification file to validate against"},
            {app, $a, "app", string, "Application name where handler is located"},
            {strict, undefined, "strict", boolean, "Strict mode - fail on any differences (for CI)"},
            {all, undefined, "all", boolean, "Validate all handlers found in manifest"}
        ]},
        {short_desc, "Validate handler implementation against OpenAPI specification"},
        {desc, "Validates that Erlang handler implementation matches the OpenAPI specification. "
               "Reports missing endpoints, extra endpoints, and other inconsistencies."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),

        %% Validate and parse options
        case validate_args(Args, State) of
            {ok, Opts} ->
                execute_validation(Opts, State);
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        throw:Error ->
            {error, format_error(Error)};
        _:Error:Stack ->
            rebar3_openapi_utils:error("Validation failed: ~p", [Error]),
            rebar3_openapi_utils:debug("Stack: ~p", [Stack]),
            {error, format_error(Error)}
    end.

-spec format_error(term()) -> iolist().
format_error({missing_required, Field}) ->
    io_lib:format("Missing required argument: ~s", [Field]);
format_error({validation_failed, Count}) ->
    io_lib:format("Validation failed with ~p difference(s)", [Count]);
format_error({handler_not_found, Handler}) ->
    io_lib:format("Handler module not found: ~s", [Handler]);
format_error({spec_not_found, Spec}) ->
    io_lib:format("OpenAPI specification not found: ~s", [Spec]);
format_error(Reason) ->
    rebar3_openapi_utils:format_error(Reason).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec validate_args([{atom(), term()}], rebar_state:t()) -> {ok, map()} | {error, term()}.
validate_args(Args, State) ->
    ValidateAll = proplists:get_value(all, Args, false),

    case ValidateAll of
        true ->
            build_batch_opts(Args, State);
        false ->
            %% Single handler validation
            Handler = proplists:get_value(handler, Args),
            Spec = proplists:get_value(spec, Args),

            if
                Handler =:= undefined ->
                    {error, {missing_required, "--handler"}};
                Spec =:= undefined ->
                    {error, {missing_required, "--spec"}};
                true ->
                    case filelib:is_file(Spec) of
                        false ->
                            {error, {spec_not_found, Spec}};
                        true ->
                            build_single_opts(Args, State)
                    end
            end
    end.

-spec build_single_opts([{atom(), term()}], rebar_state:t()) -> {ok, map()}.
build_single_opts(Args, State) ->
    Handler = proplists:get_value(handler, Args),
    Spec = proplists:get_value(spec, Args),
    App = proplists:get_value(app, Args),
    Strict = proplists:get_value(strict, Args, false),

    %% Determine app directory
    AppDir = case App of
        undefined ->
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    rebar_app_info:dir(AppInfo);
                [] ->
                    "."
            end;
        AppName ->
            case rebar3_openapi_utils:find_app_dir(list_to_atom(AppName), State) of
                {ok, Dir} -> Dir;
                {error, not_found} -> throw({app_not_found, AppName})
            end
    end,

    Opts = #{
        mode => single,
        handler => Handler,
        spec => Spec,
        app_dir => AppDir,
        strict => Strict,
        state => State
    },

    {ok, Opts}.

-spec build_batch_opts([{atom(), term()}], rebar_state:t()) -> {ok, map()}.
build_batch_opts(Args, State) ->
    Strict = proplists:get_value(strict, Args, false),

    Opts = #{
        mode => batch,
        strict => Strict,
        state => State
    },

    {ok, Opts}.

-spec execute_validation(map(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
execute_validation(#{mode := single} = Opts, State) ->
    Handler = maps:get(handler, Opts),
    SpecFile = maps:get(spec, Opts),
    Strict = maps:get(strict, Opts, false),

    rebar3_openapi_utils:info("=== OpenAPI Validation ===", []),
    rebar3_openapi_utils:info("Handler: ~s", [Handler]),
    rebar3_openapi_utils:info("Spec: ~s", [SpecFile]),
    rebar3_openapi_utils:info("Mode: ~s", [case Strict of true -> "strict"; false -> "normal" end]),
    rebar3_openapi_utils:info("", []),

    %% Perform validation
    case rebar3_openapi_validator:validate_handler_against_spec(Handler, SpecFile, Opts) of
        ok ->
            rebar3_openapi_utils:info("✓ Validation passed!", []),
            {ok, State};
        {error, Differences} when is_list(Differences) ->
            Count = length(Differences),

            case Strict of
                true ->
                    rebar3_openapi_utils:error("✗ Validation failed in strict mode", []),
                    {error, format_error({validation_failed, Count})};
                false ->
                    rebar3_openapi_utils:warn("⚠ Validation completed with differences", []),
                    rebar3_openapi_utils:info("Found ~p difference(s) (non-strict mode)", [Count]),
                    {ok, State}
            end;
        {error, Reason} ->
            {error, format_error(Reason)}
    end;

execute_validation(#{mode := batch} = Opts, State) ->
    rebar3_openapi_utils:info("=== Batch OpenAPI Validation ===", []),

    %% Read manifest to get handler-spec mappings
    case read_manifest() of
        {ok, Manifest} ->
            Handlers = maps:get(<<"handlers">>, Manifest, #{}),

            case maps:size(Handlers) of
                0 ->
                    rebar3_openapi_utils:warn("No handlers found in manifest", []),
                    {ok, State};
                Size ->
                    rebar3_openapi_utils:info("Found ~p handler(s) in manifest", [Size]),
                    rebar3_openapi_utils:info("", []),

                    %% Validate each handler
                    Results = maps:fold(
                        fun(HandlerBin, Info, Acc) ->
                            Handler = binary_to_list(HandlerBin),
                            SpecFile = binary_to_list(maps:get(<<"spec_file">>, Info)),

                            SingleOpts = Opts#{
                                mode => single,
                                handler => Handler,
                                spec => SpecFile
                            },

                            Result = execute_validation(SingleOpts, State),
                            [Result | Acc]
                        end,
                        [],
                        Handlers
                    ),

                    %% Check results
                    Errors = [R || {error, _} = R <- Results],

                    case Errors of
                        [] ->
                            rebar3_openapi_utils:info("", []),
                            rebar3_openapi_utils:info("✓ All validations passed!", []),
                            {ok, State};
                        _ ->
                            rebar3_openapi_utils:error("", []),
                            rebar3_openapi_utils:error("✗ ~p validation(s) failed", [length(Errors)]),
                            {error, format_error({validation_failed, length(Errors)})}
                    end
            end;
        {error, enoent} ->
            rebar3_openapi_utils:error("Manifest file not found: .openapi_manifest.json", []),
            rebar3_openapi_utils:info("Run 'rebar3 openapi generate' first to create manifest", []),
            {error, "Manifest not found"};
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

%%%===================================================================
%%% Internal Functions - Manifest
%%%===================================================================

-spec read_manifest() -> {ok, map()} | {error, term()}.
read_manifest() ->
    ManifestFile = ".openapi_manifest.json",

    case file:read_file(ManifestFile) of
        {ok, Content} ->
            try
                Manifest = jsx:decode(Content, [return_maps]),
                {ok, Manifest}
            catch
                _:_ ->
                    {error, invalid_manifest}
            end;
        Error ->
            Error
    end.

