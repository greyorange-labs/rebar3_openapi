%%%-------------------------------------------------------------------
%%% @doc Utility functions for rebar3_openapi plugin
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi_utils).

-export([
    get_config/2,
    get_config/3,
    ensure_dir/1,
    read_file/1,
    write_file/2,
    format_error/1,
    info/2,
    warn/2,
    error/2,
    debug/2,
    abort/2,
    find_app_dir/2,
    parse_opts/2,
    uuid/0
]).

-include_lib("kernel/include/file.hrl").

%%%===================================================================
%%% Configuration
%%%===================================================================

%% @doc Get configuration value from rebar.config
-spec get_config(atom(), rebar_state:t()) -> term() | undefined.
get_config(Key, State) ->
    get_config(Key, State, undefined).

-spec get_config(atom(), rebar_state:t(), term()) -> term().
get_config(Key, State, Default) ->
    Config = rebar_state:get(State, openapi, []),
    proplists:get_value(Key, Config, Default).

%%%===================================================================
%%% File Operations
%%%===================================================================

-spec ensure_dir(file:filename()) -> ok | {error, term()}.
ensure_dir(Dir) ->
    filelib:ensure_dir(filename:join(Dir, "dummy")).

-spec read_file(file:filename()) -> {ok, binary()} | {error, term()}.
read_file(File) ->
    file:read_file(File).

-spec write_file(file:filename(), iodata()) -> ok | {error, term()}.
write_file(File, Data) ->
    ok = ensure_dir(filename:dirname(File)),
    file:write_file(File, Data).

%%%===================================================================
%%% Logging
%%%===================================================================

-spec info(string(), list()) -> ok.
info(Format, Args) ->
    rebar_api:info(Format, Args).

-spec warn(string(), list()) -> ok.
warn(Format, Args) ->
    rebar_api:warn(Format, Args).

-spec error(string(), list()) -> ok.
error(Format, Args) ->
    rebar_api:error(Format, Args).

-spec debug(string(), list()) -> ok.
debug(Format, Args) ->
    rebar_api:debug(Format, Args).

-spec abort(string(), list()) -> no_return().
abort(Format, Args) ->
    rebar_api:abort(Format, Args).

%%%===================================================================
%%% Error Formatting
%%%===================================================================

-spec format_error(term()) -> iolist().
format_error({missing_spec, Spec}) ->
    io_lib:format("OpenAPI spec file not found: ~s", [Spec]);
format_error({invalid_yaml, File, Reason}) ->
    io_lib:format("Invalid YAML in ~s: ~p", [File, Reason]);
format_error({invalid_json, File, Reason}) ->
    io_lib:format("Invalid JSON in ~s: ~p", [File, Reason]);
format_error({missing_handler, Handler}) ->
    io_lib:format("Handler module not found: ~s", [Handler]);
format_error({openapi_generator_not_found}) ->
    io_lib:format("openapi-generator CLI not found. Please install it or use Docker.", []);
format_error({generation_failed, Reason}) ->
    io_lib:format("Code generation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("Unknown error: ~p", [Reason]).

%%%===================================================================
%%% Path Helpers
%%%===================================================================

-spec find_app_dir(atom(), rebar_state:t()) -> {ok, string()} | {error, not_found}.
find_app_dir(AppName, State) ->
    Apps = rebar_state:project_apps(State),
    case lists:keyfind(atom_to_binary(AppName, utf8), 2,
                       [{rebar_app_info:name(App), rebar_app_info:dir(App)} || App <- Apps]) of
        {_, Dir} ->
            {ok, Dir};
        false ->
            {error, not_found}
    end.

%%%===================================================================
%%% Option Parsing
%%%===================================================================

-spec parse_opts([{atom(), term()}], [{atom(), term()}]) -> {ok, map()} | {error, term()}.
parse_opts(Opts, Required) ->
    try
        Map = maps:from_list(Opts),
        %% Check required options
        lists:foreach(
            fun({Key, ErrorMsg}) ->
                case maps:is_key(Key, Map) of
                    true -> ok;
                    false -> throw({missing_required, ErrorMsg})
                end
            end,
            Required
        ),
        {ok, Map}
    catch
        throw:Error -> {error, Error}
    end.

%%%===================================================================
%%% UUID Generation
%%%===================================================================

-spec uuid() -> string().
uuid() ->
    %% Simple UUID v4 generation
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(
        io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                      [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])
    ).

