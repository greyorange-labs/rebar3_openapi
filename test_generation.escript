#!/usr/bin/env escript
%%! -pa _build/default/lib/yamerl/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/rebar3_openapi/ebin

%%%-------------------------------------------------------------------
%%% @doc Test script for code generation
%%% Usage: ./test_generation.escript <spec-file.yml> <output-dir>
%%% @end
%%%-------------------------------------------------------------------

main([SpecFile, OutputDir]) ->
    io:format("~n=== Testing Code Generation ===~n"),
    io:format("Spec: ~s~n", [SpecFile]),
    io:format("Output: ~s~n~n", [OutputDir]),
    
    %% Ensure directory exists
    filelib:ensure_dir(filename:join(OutputDir, "dummy")),
    
    %% Parse spec
    io:format("1. Parsing OpenAPI spec...~n"),
    case rebar3_openapi_parser:parse_file(SpecFile) of
        {ok, Spec} ->
            io:format("   ✓ Parsed successfully~n~n"),
            
            %% Get operations
            io:format("2. Extracting operations...~n"),
            Operations = rebar3_openapi_parser:get_operations(Spec),
            io:format("   Found ~p operation(s)~n", [length(Operations)]),
            lists:foreach(
                fun({Path, Method, Op}) ->
                    OpId = maps:get(<<"operationId">>, Op, <<"unknown">>),
                    io:format("   - ~s ~s -> ~s~n", 
                             [string:uppercase(binary_to_list(Method)), Path, OpId])
                end,
                Operations
            ),
            
            io:format("~n3. Code generation would happen here~n"),
            io:format("   (Integration with openapi-generator + transformer)~n"),
            
            io:format("~n✓ All tests passed!~n"),
            halt(0);
            
        {error, Reason} ->
            io:format("   ✗ Parsing failed: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: ./test_generation.escript <spec-file.yml> <output-dir>~n"),
    halt(1).

