#!/usr/bin/env escript
%%! -pa _build/default/lib/yamerl/ebin

main([File]) ->
    io:format("~n=== Debug YAMERL Output ===~n"),
    io:format("File: ~s~n~n", [File]),
    
    try
        Result = yamerl_constr:file(File, [{str_node_as_binary, true}]),
        io:format("Raw result:~n~p~n~n", [Result]),
        
        case Result of
            [Doc | _] ->
                io:format("First document:~n~p~n", [Doc]);
            _ ->
                io:format("Unexpected format~n")
        end
    catch
        Error:Reason:Stack ->
            io:format("Error: ~p:~p~n~p~n", [Error, Reason, Stack])
    end;

main(_) ->
    io:format("Usage: ./debug_yaml.escript <file.yml>~n"),
    halt(1).

