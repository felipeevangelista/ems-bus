%%******************************************************************** 
%% @title Module ems_data_loader_ctl  
%% @version 1.0.0 %%
%% @doc Module responsible for load records from database
%% @author Everton de Vargas Agilar  <evertonagilar@gmail.com> 
%% @copyright ErlangMS Team 
%%********************************************************************

-module(ems_data_loader_ctl).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, 
		 permission_to_execute/2, notify_finish_work/1]).

% estado do servidor
-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(_Service) -> 
	ets:new(ets_dataloader_working_ctl, [set, named_table, public]),
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

permission_to_execute(DataLoader, []) -> 
	?DEBUG("ems_data_loader_ctl ~s begin work now.", [DataLoader]),
	ets:insert(ets_dataloader_working_ctl, {DataLoader, true}),
	true;
permission_to_execute(DataLoader, [DataLoaderGroup|T]) ->
	case ets:lookup(ets_dataloader_working_ctl, DataLoaderGroup) of
		[] -> permission_to_execute(DataLoader, T);
		[{_, true}] -> false;
		[{_, false}] -> permission_to_execute(DataLoader, T)
	end.
 
notify_finish_work(DataLoader) ->
	?DEBUG("ems_data_loader_ctl ~s finish work.", [DataLoader]),
	ets:insert(ets_dataloader_working_ctl, {DataLoader, false}).
 
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_) ->
	{ok, #state{}}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(Msg, _From, State) ->
	{reply, Msg, State}.
		
terminate(Reason, #service{name = Name}) ->
    ems_logger:warn("~s was terminated. Reason: ~p.", [Name, Reason]),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	

%%====================================================================
%% Internal functions
%%====================================================================

