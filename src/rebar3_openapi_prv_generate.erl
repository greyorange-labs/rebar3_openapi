%%%-------------------------------------------------------------------
%%% @doc Provider for 'rebar3 openapi generate' command
%%% Generates Erlang handlers from OpenAPI specs
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_prv_generate).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, generate).
-define(NAMESPACE, openapi).
-define(DEPS, [app_discovery]).

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
        {example, "rebar3 openapi generate --spec specs/api.yml --handler my_http_handler"},
        {opts, [
            {spec, $s, "spec", string, "Path to OpenAPI specification file (YAML or JSON)"},
            {app, $a, "app", string, "Application name where handler will be generated"},
            {handler, $h, "handler", string, "Handler module name (e.g., bsh_http_handler)"},
            {logic_module, $l, "logic-module", string, "Logic handler module name (optional)"},
            {package_name, $p, "package-name", string, "Package name for openapi-generator (optional)"},
            {update, $u, "update", boolean, "Update existing handler (preserve business logic)"},
            {dry_run, $d, "dry-run", boolean, "Show what would be generated without creating files"},
            {force, $f, "force", boolean, "Force overwrite existing files"}
        ]},
        {short_desc, "Generate Erlang handlers from OpenAPI specification"},
        {desc, "Generates Erlang/Cowboy handler code from OpenAPI specification using "
               "openapi-generator with hybrid pattern (router + trails() wrapper)."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),

        %% Validate and parse options
        case validate_args(Args, State) of
            {ok, Opts} ->
                execute_generation(Opts, State);
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        throw:Error ->
            {error, format_error(Error)};
        _:Error ->
            {error, format_error(Error)}
    end.

-spec format_error(term()) -> iolist().
format_error({missing_required, Field}) ->
    io_lib:format("Missing required argument: ~s", [Field]);
format_error({spec_not_found, File}) ->
    io_lib:format("OpenAPI specification file not found: ~s", [File]);
format_error({app_not_found, App}) ->
    io_lib:format("Application not found: ~s", [App]);
format_error(openapi_generator_not_found) ->
    io_lib:format(
        "openapi-generator not found.~n"
        "Please install it:~n"
        "  npm install -g @openapitools/openapi-generator-cli~n"
        "Or use Docker:~n"
        "  docker pull openapitools/openapi-generator-cli~n", []);
format_error(Reason) ->
    rebar3_openapi_utils:format_error(Reason).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec validate_args([{atom(), term()}], rebar_state:t()) -> {ok, map()} | {error, term()}.
validate_args(Args, State) ->
    %% Check required arguments
    SpecFile = proplists:get_value(spec, Args),
    Handler = proplists:get_value(handler, Args),

    if
        SpecFile =:= undefined ->
            {error, {missing_required, "--spec"}};
        Handler =:= undefined ->
            {error, {missing_required, "--handler"}};
        true ->
            case filelib:is_file(SpecFile) of
                false ->
                    {error, {spec_not_found, SpecFile}};
                true ->
                    build_opts(Args, State)
            end
    end.

-spec build_opts([{atom(), term()}], rebar_state:t()) -> {ok, map()}.
build_opts(Args, State) ->
    SpecFile = proplists:get_value(spec, Args),
    Handler = proplists:get_value(handler, Args),
    App = proplists:get_value(app, Args),
    LogicModule = proplists:get_value(logic_module, Args, Handler ++ "_logic_handler"),
    PackageName = proplists:get_value(package_name, Args, Handler),
    Update = proplists:get_value(update, Args, false),
    DryRun = proplists:get_value(dry_run, Args, false),
    Force = proplists:get_value(force, Args, false),

    %% Determine output directory
    OutputDir = case App of
        undefined ->
            %% Use first app or current directory
            case rebar_state:project_apps(State) of
                [AppInfo | _] ->
                    filename:join(rebar_app_info:dir(AppInfo), "src");
                [] ->
                    "src"
            end;
        AppName ->
            case rebar3_openapi_utils:find_app_dir(list_to_atom(AppName), State) of
                {ok, Dir} ->
                    filename:join(Dir, "src");
                {error, not_found} ->
                    throw({app_not_found, AppName})
            end
    end,

    Opts = #{
        spec_file => SpecFile,
        handler => Handler,
        logic_module => LogicModule,
        package_name => PackageName,
        output_dir => OutputDir,
        update => Update,
        dry_run => DryRun,
        force => Force,
        state => State
    },

    {ok, Opts}.

-spec execute_generation(map(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
execute_generation(#{dry_run := true} = Opts, State) ->
    %% Dry run mode - just show what would be generated
    rebar3_openapi_utils:info("=== DRY RUN MODE ===", []),
    rebar3_openapi_utils:info("Spec file: ~s", [maps:get(spec_file, Opts)]),
    rebar3_openapi_utils:info("Handler: ~s", [maps:get(handler, Opts)]),
    rebar3_openapi_utils:info("Logic module: ~s", [maps:get(logic_module, Opts)]),
    rebar3_openapi_utils:info("Output directory: ~s", [maps:get(output_dir, Opts)]),

    %% Parse spec to show operations
    case rebar3_openapi_parser:parse_file(maps:get(spec_file, Opts)) of
        {ok, Spec} ->
            Operations = rebar3_openapi_parser:get_operations(Spec),
            rebar3_openapi_utils:info("~nOperations to generate:", []),
            lists:foreach(
                fun({Path, Method, Op}) ->
                    OpId = maps:get(<<"operationId">>, Op, <<"unknown">>),
                    rebar3_openapi_utils:info("  ~s ~s -> ~s",
                        [string:uppercase(binary_to_list(Method)), Path, OpId])
                end,
                Operations
            ),

            rebar3_openapi_utils:info("~nFiles that would be generated:", []),
            rebar3_openapi_utils:info("  ~s_router.erl", [maps:get(handler, Opts)]),
            rebar3_openapi_utils:info("  ~s.erl", [maps:get(handler, Opts)]),
            rebar3_openapi_utils:info("  ~s.erl", [maps:get(logic_module, Opts)]),
            {ok, State};
        {error, Reason} ->
            {error, format_error(Reason)}
    end;

execute_generation(Opts, State) ->
    rebar3_openapi_utils:info("Generating handler from OpenAPI spec...", []),

    %% Step 1: Check openapi-generator availability
    case rebar3_openapi_generator:check_available() of
        {ok, _} ->
            ok;
        {error, not_found} ->
            throw(openapi_generator_not_found)
    end,

    %% Step 2: Parse OpenAPI spec
    SpecFile = maps:get(spec_file, Opts),
    rebar3_openapi_utils:info("Parsing OpenAPI spec: ~s", [SpecFile]),
    {ok, Spec} = rebar3_openapi_parser:parse_file(SpecFile),

    %% Step 3: Generate code using openapi-generator
    rebar3_openapi_utils:info("Generating code with openapi-generator...", []),
    GenOpts = #{
        package_name => maps:get(package_name, Opts),
        output_dir => "/tmp/openapi_gen_" ++ rebar3_openapi_utils:uuid()
    },

    case rebar3_openapi_generator:generate(SpecFile, GenOpts) of
        {ok, TempDir} ->
            rebar3_openapi_utils:debug("Generated to: ~s", [TempDir]),

            %% Step 4: Transform generated code to hybrid pattern
            rebar3_openapi_utils:info("Transforming to hybrid pattern...", []),
            TransformOpts = Opts#{
                temp_dir => TempDir,
                spec => Spec
            },

            case rebar3_openapi_transformer:transform(TransformOpts) of
                ok ->
                    %% Step 5: Update manifest
                    update_manifest(Opts, Spec),

                    %% Cleanup temp directory
                    file:del_dir_r(TempDir),

                    rebar3_openapi_utils:info("✓ Generation completed successfully!", []),
                    rebar3_openapi_utils:info("Generated files:", []),
                    rebar3_openapi_utils:info("  ~s_router.erl", [maps:get(handler, Opts)]),
                    rebar3_openapi_utils:info("  ~s.erl", [maps:get(handler, Opts)]),
                    rebar3_openapi_utils:info("  ~s.erl", [maps:get(logic_module, Opts)]),
                    {ok, State};
                {error, Reason} ->
                    file:del_dir_r(TempDir),
                    {error, format_error(Reason)}
            end;
        {error, Reason} ->
            {error, format_error(Reason)}
    end.

-spec update_manifest(map(), map()) -> ok.
update_manifest(Opts, Spec) ->
    %% Read or create manifest
    ManifestFile = ".openapi_manifest.json",
    Manifest = case file:read_file(ManifestFile) of
        {ok, Content} ->
            jsx:decode(Content, [return_maps]);
        {error, enoent} ->
            #{<<"version">> => <<"1.0">>, <<"handlers">> => #{}}
    end,

    %% Update handler entry
    Handler = list_to_binary(maps:get(handler, Opts)),
    Info = rebar3_openapi_parser:get_info(Spec),

    HandlerInfo = #{
        <<"spec_file">> => list_to_binary(maps:get(spec_file, Opts)),
        <<"router_module">> => list_to_binary(maps:get(handler, Opts) ++ "_router"),
        <<"logic_module">> => list_to_binary(maps:get(logic_module, Opts)),
        <<"output_dir">> => list_to_binary(maps:get(output_dir, Opts)),
        <<"spec_version">> => maps:get(<<"version">>, Info, <<"unknown">>),
        <<"last_generated">> => list_to_binary(
            calendar:system_time_to_rfc3339(erlang:system_time(second))
        )
    },

    Handlers = maps:get(<<"handlers">>, Manifest, #{}),
    UpdatedHandlers = maps:put(Handler, HandlerInfo, Handlers),
    UpdatedManifest = maps:put(<<"handlers">>, UpdatedHandlers, Manifest),

    %% Write manifest
    ManifestJson = jsx:encode(UpdatedManifest, [{space, 2}, {indent, 2}]),
    file:write_file(ManifestFile, ManifestJson),

    rebar3_openapi_utils:info("Updated manifest: ~s", [ManifestFile]),
    ok.

