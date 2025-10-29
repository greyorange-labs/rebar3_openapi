#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa _build/default/lib/rebar3_openapi/ebin

-mode(compile).

main([HandlerPath, OutputFile]) ->
    io:format("~n=== Testing rebar3_openapi OpenAPI Extraction ===~n"),
    io:format("Handler: ~s~n", [HandlerPath]),
    io:format("Output: ~s~n~n", [OutputFile]),

    %% Add code paths
    code:add_pathz("_build/default/lib/rebar3_openapi/ebin"),
    code:add_pathz("_build/default/lib/yamerl/ebin"),
    code:add_pathz("_build/default/lib/jsx/ebin"),

    %% Ensure dependencies are started
    application:ensure_all_started(yamerl),

    %% Determine format from output file extension
    Format = case filename:extension(OutputFile) of
        ".json" -> json;
        ".yml" -> yaml;
        ".yaml" -> yaml;
        _ -> yaml
    end,

    Handler = filename:basename(HandlerPath, ".erl"),

    io:format("Step 1: Extracting OpenAPI from handler...~n"),

    Opts = #{
        handler => Handler,
        handler_path => HandlerPath,
        output => OutputFile,
        format => Format
    },

    case rebar3_openapi_extractor:extract_from_module(HandlerPath) of
        {ok, Spec} ->
            io:format("   ✓ Extracted OpenAPI spec~n~n"),

            io:format("Step 2: Writing to ~s...~n", [OutputFile]),

            %% Ensure output directory exists
            OutputDir = filename:dirname(OutputFile),
            filelib:ensure_dir(filename:join(OutputDir, "dummy")),

            %% Write the spec
            Content = case Format of
                json ->
                    jsx:encode(Spec, [{space, 2}, {indent, 2}]);
                yaml ->
                    %% For now, write as JSON (YAML encoding needs yamerl which is more complex)
                    %% Or we can write the Erlang term and manually convert
                    io_lib:format("# Generated OpenAPI spec from ~s~n~n~p~n", [Handler, Spec])
            end,

            ok = file:write_file(OutputFile, Content),
            io:format("   ✓ Written to: ~s~n~n", [OutputFile]),

            %% Show summary
            Paths = maps:get(<<"paths">>, Spec, #{}),
            PathCount = maps:size(Paths),
            io:format("Extracted ~p paths:~n", [PathCount]),
            maps:foreach(
                fun(Path, Operations) ->
                    Methods = maps:keys(Operations),
                    io:format("   - ~s (~p)~n", [Path, Methods])
                end,
                Paths
            ),

            io:format("~n✓ Extraction completed successfully!~n");

        {error, Reason} ->
            io:format("~n✗ Extraction failed: ~p~n", [Reason]),
            erlang:halt(1)
    end;

main(_) ->
    io:format("Usage: ./extract_handler.escript <handler_path> <output_file>~n"),
    io:format("Example: ./extract_handler.escript generated_code/bsh_http_handler.erl extracted_api.yml~n"),
    erlang:halt(1).

