%%%-------------------------------------------------------------------
%%% @doc Wrapper for openapi-generator CLI tool
%%% Handles invoking openapi-generator and parsing its output
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_generator).

-export([
    check_available/0,
    generate/2,
    get_version/0
]).

-define(GENERATOR, "erlang-server").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Check if openapi-generator is available
-spec check_available() -> {ok, string()} | {error, not_found}.
check_available() ->
    case os:find_executable("openapi-generator") of
        false ->
            %% Try docker as fallback
            case os:find_executable("docker") of
                false ->
                    {error, not_found};
                _DockerPath ->
                    {ok, "docker"}
            end;
        Path ->
            {ok, Path}
    end.

%% @doc Get openapi-generator version
-spec get_version() -> {ok, string()} | {error, term()}.
get_version() ->
    case check_available() of
        {ok, "docker"} ->
            Cmd = "docker run --rm openapitools/openapi-generator-cli version",
            case run_command(Cmd) of
                {ok, Output} -> {ok, string:trim(Output)};
                Error -> Error
            end;
        {ok, _Path} ->
            case run_command("openapi-generator version") of
                {ok, Output} -> {ok, string:trim(Output)};
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @doc Generate Erlang server code from OpenAPI spec
-spec generate(file:filename(), map()) -> {ok, file:filename()} | {error, term()}.
generate(SpecFile, Opts) ->
    case check_available() of
        {ok, Executable} ->
            OutputDir = maps:get(output_dir, Opts, "/tmp/openapi_gen_" ++ rebar3_openapi_utils:uuid()),
            PackageName = maps:get(package_name, Opts, "generated"),

            rebar3_openapi_utils:info("Generating Erlang code from ~s...", [SpecFile]),
            rebar3_openapi_utils:debug("Output directory: ~s", [OutputDir]),
            rebar3_openapi_utils:debug("Package name: ~s", [PackageName]),

            Cmd = build_generate_command(Executable, SpecFile, OutputDir, PackageName),

            case run_command(Cmd) of
                {ok, _Output} ->
                    rebar3_openapi_utils:info("Code generation completed successfully", []),
                    {ok, OutputDir};
                {error, {exit_code, Code, Output}} ->
                    rebar3_openapi_utils:error("Generation failed with exit code ~p", [Code]),
                    rebar3_openapi_utils:error("Output: ~s", [Output]),
                    {error, {generation_failed, Code}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, openapi_generator_not_found}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec build_generate_command(string(), file:filename(), file:filename(), string()) -> string().
build_generate_command("docker", SpecFile, OutputDir, PackageName) ->
    %% Ensure absolute paths for Docker volume mounts
    AbsSpecFile = filename:absname(SpecFile),
    AbsOutputDir = filename:absname(OutputDir),
    SpecDir = filename:dirname(AbsSpecFile),
    SpecName = filename:basename(AbsSpecFile),

    lists:flatten(
        io_lib:format(
            "docker run --rm "
            "-v ~s:/spec "
            "-v ~s:/output "
            "openapitools/openapi-generator-cli generate "
            "-i /spec/~s "
            "-g ~s "
            "-o /output "
            "--additional-properties=packageName=~s",
            [SpecDir, AbsOutputDir, SpecName, ?GENERATOR, PackageName]
        )
    );
build_generate_command(_Path, SpecFile, OutputDir, PackageName) ->
    lists:flatten(
        io_lib:format(
            "openapi-generator generate "
            "-i ~s "
            "-g ~s "
            "-o ~s "
            "--additional-properties=packageName=~s",
            [SpecFile, ?GENERATOR, OutputDir, PackageName]
        )
    ).

-spec run_command(string()) -> {ok, string()} | {error, term()}.
run_command(Cmd) ->
    rebar3_openapi_utils:debug("Running command: ~s", [Cmd]),

    Port = erlang:open_port(
        {spawn, Cmd},
        [exit_status, stderr_to_stdout, {line, 16384}, binary]
    ),

    collect_output(Port, []).

-spec collect_output(port(), [binary()]) -> {ok, string()} | {error, term()}.
collect_output(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            %% Log output line by line
            rebar3_openapi_utils:debug("  ~s", [Line]),
            collect_output(Port, [Line, <<"\n">> | Acc]);
        {Port, {data, {noeol, Line}}} ->
            collect_output(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            Output = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
            {ok, Output};
        {Port, {exit_status, Code}} ->
            Output = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
            {error, {exit_code, Code, Output}}
    after 300000 -> % 5 minute timeout
        erlang:port_close(Port),
        {error, timeout}
    end.

