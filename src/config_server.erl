-module(config_server).
-author('guillermo@origamicall.com').

-behaviour(gen_server).


-export([start_link/0, start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3]).

-include("relconfig.hrl").

-record(infoConfig, {dataConfig = []}).
-compile(export_all).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
	
start_link([]) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @spec  Func: init/1 ->
%%          {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------


init([]) ->
    {ok, #infoConfig{dataConfig = []}}.


handle_call({reload_config, App, Par}, _From, State) ->
    ConfigPath = get_config_path(App, Par),
    NewState = load_file(ConfigPath, State),
    {reply, ok, NewState};
    
handle_call(get_all_config, _From, State) ->
    {reply, State#infoConfig.dataConfig, State};
    
handle_call({get_config_element, Element}, _From, State) ->
    Response = proplists:get_value(Element, State#infoConfig.dataConfig),
    {reply, Response, State};
    

handle_call(_Request, _From, State) ->
	{reply, ok, State}.





%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, _State) ->
	{noreply, _State}.



%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



reload_config(App,Par) ->
    gen_server:call(?MODULE, {reload_config, App, Par}).
	

get_all_config() ->
    gen_server:call(?MODULE, get_all_config).
    
get_config_element(Element) ->
    gen_server:call(?MODULE, {get_config_element, Element}).

%%%%%%%%%%%%%%%%%%%%%%%
%%Internal functions
%%%%%%%%%%%%%%%%%%%%%%%


%% @spec get_config_path(App, Par)-> {cgf, Path} | exit(normal)
%% @doc Obtenemos el fichero de configuracion 
%% El fichero puede ser especificado  con : erl -config "/path/to/app.config"
%% @end

get_config_path(App, Par) ->
	case application:get_env(App, Par) of
		{ok, Config} -> Config;
		undefined ->
			io:format("Erro not file found ~n", []),
			ok;
		Reason ->
			io:format("Error config file:~n App: ~p Par: ~p Reason: ~p ~n", [App, Par, Reason]),
			ok
	end.





%% @spec load_file(File, State, Type) -> NewState | State
%% @doc Lee el fichero de configuracion y los agrega al State
%% @end

load_file(File, State) ->%, Type, Elements) ->
    case file:consult(File) of
	{ok, Data} ->
	    lists:foldl(fun search_c/2, State#infoConfig{dataConfig=[]}, Data);
	{error, Reason} ->
		ExitText = lists:flatten(File ++ "Line: " ++ file:format_error(Reason)),
		io:format("ERROR config file cfg ~n ~s", [ExitText]),
		exit(ExitText)
end.




%% @spec search_sw(Term, State) -> State
%% @doc Busca terminos especificos, formatea la info, la ordena y la agrega al state
%% @end

search_c(Term, State) ->
    NewData = State#infoConfig.dataConfig ++ [Term],
    State#infoConfig{dataConfig = NewData}.
