#!/usr/bin/env escript
%%! -pa _build/default/lib/yamerl/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/rebar3_openapi/ebin

%%%-------------------------------------------------------------------
%%% @doc Test script for YAML parser
%%% Usage: ./test_parser.escript examples/simple_api.yml
%%% @end
%%%-------------------------------------------------------------------

main([SpecFile]) ->
    io:format("~n=== Testing YAML Parser ===~n"),
    io:format("File: ~s~n~n", [SpecFile]),
    
    %% Check file exists
    case filelib:is_file(SpecFile) of
        false ->
            io:format("✗ File not found: ~s~n", [SpecFile]),
            halt(1);
        true ->
            ok
    end,
    
    %% Test parsing
    io:format("Parsing...~n"),
    case rebar3_openapi_parser:parse_file(SpecFile) of
        {ok, Spec} ->
            io:format("✓ Parsing successful~n~n"),
            
            %% Show parsed info
            Info = rebar3_openapi_parser:get_info(Spec),
            io:format("Title: ~s~n", [maps:get(<<"title">>, Info, <<"unknown">>)]),
            io:format("Version: ~s~n", [maps:get(<<"version">>, Info, <<"unknown">>)]),
            
            Paths = rebar3_openapi_parser:get_paths(Spec),
            io:format("~nPaths (~p):~n", [maps:size(Paths)]),
            lists:foreach(
                fun(Path) ->
                    io:format("  - ~s~n", [Path])
                end,
                maps:keys(Paths)
            ),
            
            Operations = rebar3_openapi_parser:get_operations(Spec),
            io:format("~nOperations (~p):~n", [length(Operations)]),
            lists:foreach(
                fun({Path, Method, Op}) ->
                    OpId = maps:get(<<"operationId">>, Op, <<"unknown">>),
                    io:format("  - ~s ~s -> ~s~n", [string:uppercase(binary_to_list(Method)), Path, OpId])
                end,
                Operations
            ),
            
            io:format("~n✓ All tests passed!~n"),
            halt(0);
            
        {error, Reason} ->
            io:format("✗ Parsing failed: ~p~n", [Reason]),
            io:format("~nDebug info:~n"),
            io:format("  Reason: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: ./test_parser.escript <spec-file.yml>~n"),
    halt(1).

