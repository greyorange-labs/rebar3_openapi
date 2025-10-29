%%%-------------------------------------------------------------------
%%% @doc Main rebar3_openapi plugin entry point
%%% Registers all providers (generate, extract, validate)
%%% @end
%%%-------------------------------------------------------------------
-module(rebar3_openapi).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% Register all providers
    {ok, State1} = rebar3_openapi_prv_generate:init(State),
    {ok, State2} = rebar3_openapi_prv_extract:init(State1),
    {ok, State3} = rebar3_openapi_prv_validate:init(State2),
    {ok, State3}.

