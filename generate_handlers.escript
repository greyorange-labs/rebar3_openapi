#!/usr/bin/env escript
%%! -pa _build/default/lib/yamerl/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/mustache/ebin -pa _build/default/lib/rebar3_openapi/ebin

%%%-------------------------------------------------------------------
%%% @doc Script to test rebar3_openapi generation
%%% Simulates: rebar3 openapi generate --spec SPEC --handler HANDLER
%%% @end
%%%-------------------------------------------------------------------

main([SpecFile, HandlerPath]) ->
    io:format("~n=== Testing rebar3_openapi Code Generation ===~n"),
    io:format("Spec: ~s~n", [SpecFile]),
    io:format("Handler: ~s~n~n", [HandlerPath]),

    %% Extract handler info from path
    HandlerDir = filename:dirname(HandlerPath),
    HandlerModule = filename:basename(HandlerPath, ".erl"),
    %% Logic module: remove _handler suffix if present, then add _logic_handler
    BaseModule = case lists:suffix("_handler", HandlerModule) of
        true ->
            Len = length(HandlerModule) - length("_handler"),
            lists:sublist(HandlerModule, Len);
        false ->
            HandlerModule
    end,
    LogicModule = BaseModule ++ "_logic_handler",

    io:format("Output directory: ~s~n", [HandlerDir]),
    io:format("Handler module: ~s~n", [HandlerModule]),
    io:format("Logic module: ~s~n~n", [LogicModule]),

    %% Step 1: Parse spec
    io:format("Step 1: Parsing OpenAPI spec...~n"),
    case rebar3_openapi_parser:parse_file(SpecFile) of
        {ok, Spec} ->
            io:format("   ✓ Parsed successfully~n~n"),

            %% Step 2: Check openapi-generator
            io:format("Step 2: Checking openapi-generator availability...~n"),
            case rebar3_openapi_generator:check_available() of
                {ok, Path} ->
                    io:format("   ✓ Found: ~s~n~n", [Path]),

                    %% Step 3: Generate code
                    io:format("Step 3: Generating code with openapi-generator...~n"),
                    GenOpts = #{
                        package_name => HandlerModule,
                        output_dir => "/tmp/openapi_gen_" ++ rebar3_openapi_utils:uuid()
                    },

                    case rebar3_openapi_generator:generate(SpecFile, GenOpts) of
                        {ok, TempDir} ->
                            io:format("   ✓ Generated to: ~s~n~n", [TempDir]),

                            %% Step 4: Transform to hybrid pattern
                            io:format("Step 4: Transforming to hybrid pattern...~n"),
                            TransformOpts = #{
                                temp_dir => TempDir,
                                spec => Spec,
                                output_dir => HandlerDir,
                                handler => HandlerModule,
                                logic_module => LogicModule,
                                update => false,
                                force => true
                            },

                            case rebar3_openapi_transformer:transform(TransformOpts) of
                                ok ->
                                    io:format("   ✓ Transformation complete~n~n"),

                                    %% Show what was generated
                                    io:format("Generated files:~n"),
                                    io:format("   - ~s/~s_router.erl~n", [HandlerDir, HandlerModule]),
                                    io:format("   - ~s/~s.erl~n", [HandlerDir, HandlerModule]),
                                    io:format("   - ~s/~s.erl~n", [HandlerDir, LogicModule]),

                                    io:format("~nOpenAPI Generator output (NOT deleted): ~s~n", [TempDir]),
                                    io:format("~n✓ Generation completed successfully!~n"),
                                    halt(0);
                                {error, Reason} ->
                                    io:format("   ✗ Transformation failed: ~p~n", [Reason]),
                                    halt(1)
                            end;
                        {error, Reason} ->
                            io:format("   ✗ Generation failed: ~p~n", [Reason]),
                            halt(1)
                    end;
                {error, not_found} ->
                    io:format("   ✗ openapi-generator not found~n"),
                    io:format("   Install it: npm install -g @openapitools/openapi-generator-cli~n"),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("   ✗ Parsing failed: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    io:format("Usage: ./generate_handlers.escript <spec-file.yml> <handler-path.erl>~n"),
    io:format("Example: ./generate_handlers.escript examples/butler_shared_consolidated.yml generated_code/bsh_http_handler.erl~n"),
    halt(1).


