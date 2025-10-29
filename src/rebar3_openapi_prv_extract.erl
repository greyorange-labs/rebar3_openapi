%%%-------------------------------------------------------------------
%%% @doc Provider for 'rebar3 openapi extract' command
%%% Extracts OpenAPI specification from Erlang handlers
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_prv_extract).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, extract).
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
        {example, "rebar3 openapi extract --handler my_http_handler --output spec.yml"},
        {opts, [
            {handler, $h, "handler", string, "Handler module name to extract from"},
            {app, $a, "app", string, "Application name where handler is located"},
            {output, $o, "output", string, "Output file path for OpenAPI spec (YAML or JSON)"},
            {output_dir, $d, "output-dir", string, "Output directory for extracting all handlers"},
            {format, $f, "format", string, "Output format: yaml or json (default: yaml)"}
        ]},
        {short_desc, "Extract OpenAPI specification from Erlang handlers"},
        {desc, "Extracts OpenAPI specification from Erlang handler code by analyzing "
               "route definitions, operations, and schemas."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),

        %% Validate and parse options
        case validate_args(Args, State) of
            {ok, Opts} ->
                execute_extraction(Opts, State);
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        throw:Error ->
            {error, format_error(Error)};
        _:Error:Stack ->
            rebar3_openapi_utils:error("Extract failed: ~p", [Error]),
            rebar3_openapi_utils:debug("Stack: ~p", [Stack]),
            {error, format_error(Error)}
    end.

-spec format_error(term()) -> iolist().
format_error({missing_required, Field}) ->
    io_lib:format("Missing required argument: ~s", [Field]);
format_error({handler_not_found, Handler}) ->
    io_lib:format("Handler module not found: ~s", [Handler]);
format_error({app_not_found, App}) ->
    io_lib:format("Application not found: ~s", [App]);
format_error(Reason) ->
    rebar3_openapi_utils:format_error(Reason).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec validate_args([{atom(), term()}], rebar_state:t()) -> {ok, map()} | {error, term()}.
validate_args(Args, State) ->
    Handler = proplists:get_value(handler, Args),
    Output = proplists:get_value(output, Args),
    OutputDir = proplists:get_value(output_dir, Args),

    %% Either handler+output OR output_dir must be specified
    if
        Handler =/= undefined, Output =/= undefined ->
            build_single_opts(Args, State);
        OutputDir =/= undefined ->
            build_batch_opts(Args, State);
        Handler =:= undefined ->
            {error, {missing_required, "--handler or --output-dir"}};
        Output =:= undefined ->
            {error, {missing_required, "--output"}};
        true ->
            {error, invalid_arguments}
    end.

-spec build_single_opts([{atom(), term()}], rebar_state:t()) -> {ok, map()}.
build_single_opts(Args, State) ->
    Handler = proplists:get_value(handler, Args),
    Output = proplists:get_value(output, Args),
    App = proplists:get_value(app, Args),
    Format = determine_format(proplists:get_value(format, Args, "yaml"), Output),

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
        output => Output,
        format => Format,
        app_dir => AppDir,
        state => State
    },

    {ok, Opts}.

-spec build_batch_opts([{atom(), term()}], rebar_state:t()) -> {ok, map()}.
build_batch_opts(Args, State) ->
    OutputDir = proplists:get_value(output_dir, Args),
    App = proplists:get_value(app, Args),
    Format = proplists:get_value(format, Args, "yaml"),

    Opts = #{
        mode => batch,
        output_dir => OutputDir,
        app => App,
        format => list_to_atom(Format),
        state => State
    },

    {ok, Opts}.

-spec determine_format(string() | undefined, file:filename()) -> yaml | json.
determine_format(undefined, OutputFile) ->
    %% Infer from file extension
    case filename:extension(OutputFile) of
        ".json" -> json;
        ".yml" -> yaml;
        ".yaml" -> yaml;
        _ -> yaml  % Default to YAML
    end;
determine_format("json", _) -> json;
determine_format("yaml", _) -> yaml;
determine_format("yml", _) -> yaml;
determine_format(_, _) -> yaml.

-spec execute_extraction(map(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
execute_extraction(#{mode := single} = Opts, State) ->
    Handler = maps:get(handler, Opts),
    Output = maps:get(output, Opts),
    Format = maps:get(format, Opts),

    rebar3_openapi_utils:info("Extracting OpenAPI from handler: ~s", [Handler]),

    %% Extract OpenAPI spec from handler
    case rebar3_openapi_extractor:extract_from_handler(Handler, Opts) of
        {ok, Spec} ->
            %% Write spec to output file
            case write_spec(Output, Spec, Format) of
                ok ->
                    rebar3_openapi_utils:info("✓ OpenAPI spec written to: ~s", [Output]),
                    {ok, State};
                {error, Reason} ->
                    {error, format_error(Reason)}
            end;
        {error, Reason} ->
            {error, format_error(Reason)}
    end;

execute_extraction(#{mode := batch} = Opts, State) ->
    OutputDir = maps:get(output_dir, Opts),

    rebar3_openapi_utils:info("Extracting OpenAPI specs from all handlers", []),
    rebar3_openapi_utils:info("Output directory: ~s", [OutputDir]),

    %% Find all handlers in the project
    Handlers = find_all_handlers(State, Opts),

    case Handlers of
        [] ->
            rebar3_openapi_utils:warn("No handlers found", []),
            {ok, State};
        _ ->
            rebar3_openapi_utils:info("Found ~p handler(s)", [length(Handlers)]),

            %% Extract from each handler
            Results = lists:map(
                fun(Handler) ->
                    extract_and_write(Handler, OutputDir, Opts)
                end,
                Handlers
            ),

            %% Check if all succeeded
            case lists:all(fun(R) -> R =:= ok end, Results) of
                true ->
                    rebar3_openapi_utils:info("✓ All extractions completed successfully", []),
                    {ok, State};
                false ->
                    Failed = length(lists:filter(fun(R) -> R =/= ok end, Results)),
                    {error, io_lib:format("~p extraction(s) failed", [Failed])}
            end
    end.

-spec write_spec(file:filename(), map(), yaml | json) -> ok | {error, term()}.
write_spec(Output, Spec, json) ->
    %% Write as JSON
    Json = jsx:encode(Spec, [{space, 2}, {indent, 2}]),
    rebar3_openapi_utils:write_file(Output, Json);

write_spec(Output, Spec, yaml) ->
    %% Convert map to YAML format
    %% For now, we'll use JSON as YAML is a superset
    %% A full YAML encoder would be better but JSX output is valid YAML
    Yaml = to_yaml(Spec, 0),
    rebar3_openapi_utils:write_file(Output, Yaml).

-spec to_yaml(term(), integer()) -> iolist().
to_yaml(Map, Indent) when is_map(Map) ->
    IndentStr = lists:duplicate(Indent, " "),
    NextIndent = Indent + 2,

    lists:map(
        fun({Key, Value}) ->
            KeyStr = to_yaml_string(Key),
            case is_map(Value) orelse is_list(Value) of
                true when Value =:= #{} ->
                    [IndentStr, KeyStr, ": {}\n"];
                true when Value =:= [] ->
                    [IndentStr, KeyStr, ": []\n"];
                true ->
                    [IndentStr, KeyStr, ":\n", to_yaml(Value, NextIndent)];
                false ->
                    [IndentStr, KeyStr, ": ", to_yaml(Value, NextIndent), "\n"]
            end
        end,
        maps:to_list(Map)
    );

to_yaml(List, Indent) when is_list(List) ->
    case is_string(List) of
        true ->
            to_yaml_string(List);
        false ->
            IndentStr = lists:duplicate(Indent, " "),
            lists:map(
                fun(Item) ->
                    case is_map(Item) orelse (is_list(Item) andalso not is_string(Item)) of
                        true ->
                            [IndentStr, "- ", string:trim(to_yaml(Item, Indent + 2), leading), "\n"];
                        false ->
                            [IndentStr, "- ", to_yaml(Item, Indent), "\n"]
                    end
                end,
                List
            )
    end;

to_yaml(Binary, _Indent) when is_binary(Binary) ->
    to_yaml_string(binary_to_list(Binary));

to_yaml(Atom, _Indent) when is_atom(Atom) ->
    atom_to_list(Atom);

to_yaml(Number, _Indent) when is_number(Number) ->
    io_lib:format("~p", [Number]);

to_yaml(Other, _Indent) ->
    io_lib:format("~p", [Other]).

-spec to_yaml_string(string() | binary()) -> string().
to_yaml_string(Bin) when is_binary(Bin) ->
    to_yaml_string(binary_to_list(Bin));
to_yaml_string(Str) when is_list(Str) ->
    %% Quote strings that need it
    case needs_quoting(Str) of
        true -> io_lib:format("\"~s\"", [escape_string(Str)]);
        false -> Str
    end.

-spec needs_quoting(string()) -> boolean().
needs_quoting(Str) ->
    %% Simple heuristic - quote if contains special chars
    lists:any(fun(C) -> lists:member(C, ": \t\n\"'") end, Str).

-spec escape_string(string()) -> string().
escape_string(Str) ->
    lists:flatmap(
        fun
            ($") -> "\\\"";
            ($\\) -> "\\\\";
            (C) -> [C]
        end,
        Str
    ).

-spec is_string(list()) -> boolean().
is_string([]) -> true;
is_string([H | T]) when is_integer(H), H >= 0, H =< 1114111 ->
    is_string(T);
is_string(_) -> false.

-spec find_all_handlers(rebar_state:t(), map()) -> [string()].
find_all_handlers(State, Opts) ->
    %% Find all *_handler.erl files
    Apps = case maps:get(app, Opts, undefined) of
        undefined ->
            rebar_state:project_apps(State);
        AppName ->
            case lists:filter(
                fun(App) ->
                    rebar_app_info:name(App) =:= list_to_binary(AppName)
                end,
                rebar_state:project_apps(State)
            ) of
                [App] -> [App];
                [] -> []
            end
    end,

    %% Find handler files in each app
    lists:flatmap(
        fun(App) ->
            SrcDir = filename:join(rebar_app_info:dir(App), "src"),
            case filelib:is_dir(SrcDir) of
                true ->
                    Files = filelib:wildcard(filename:join(SrcDir, "*_handler.erl")),
                    [filename:basename(F, ".erl") || F <- Files];
                false ->
                    []
            end
        end,
        Apps
    ).

-spec extract_and_write(string(), file:filename(), map()) -> ok | {error, term()}.
extract_and_write(Handler, OutputDir, Opts) ->
    Format = maps:get(format, Opts, yaml),
    Extension = case Format of
        json -> ".json";
        yaml -> ".yml"
    end,

    OutputFile = filename:join(OutputDir, Handler ++ Extension),

    SingleOpts = Opts#{
        handler => Handler,
        output => OutputFile,
        format => Format,
        mode => single
    },

    case rebar3_openapi_extractor:extract_from_handler(Handler, SingleOpts) of
        {ok, Spec} ->
            case write_spec(OutputFile, Spec, Format) of
                ok ->
                    rebar3_openapi_utils:info("✓ Extracted: ~s -> ~s", [Handler, OutputFile]),
                    ok;
                Error ->
                    rebar3_openapi_utils:error("Failed to write: ~s", [OutputFile]),
                    Error
            end;
        {error, Reason} ->
            rebar3_openapi_utils:error("Failed to extract: ~s (~p)", [Handler, Reason]),
            {error, Reason}
    end.

