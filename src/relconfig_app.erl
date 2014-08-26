-module(relconfig_app).
-author('guillermo@origamicall.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs)->
    relconfig_sup:start_link().

stop(_State) ->
    ok.
