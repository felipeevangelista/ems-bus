%%********************************************************************
%% @title Module ems_stat_collector
%% @version 1.0.0
%% @doc Module responsible for the system monitor
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_stat_collector).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Server API
-export([start/1, stop/0]).

%% Client API
-export([collect/0, collect/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {timeout}). 


%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Client API
%%====================================================================
 
collect(Label) ->
	gen_server:cast(?SERVER, {collect, Label}).

collect() ->
	gen_server:cast(?SERVER, {collect, undefined}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{start_timeout = Timeout}) ->
    State = #state{timeout = Timeout},
    {ok, State, Timeout}.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({collect, Label}, State = #state{timeout = Timeout}) ->
	NewState = collect_stats(State, Label),
	{noreply, NewState, Timeout};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_info(timeout, State = #state{timeout = Timeout}) ->
   NewState = collect_stats(State, undefined),
   {noreply, NewState, Timeout}.

terminate(_, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

collect_stats(State, Label) ->
	ems_logger:info("ems_stat_collector collect system statistics."),
	Timestamp = calendar:local_time(),
	{ok, Counters} = ems_db:all(counter),
	mnesia:clear_table(counter),
	REPattern = ems_db:get_re_param(check_service_metric_re, "^service_(?<rowid>[0-9]+)_(?<type>[[:alpha:]]+$)"),
	persist_stats(Counters, Timestamp, REPattern, Label),
	State.
	
persist_stats([], _, _, _) -> ok;
persist_stats([{counter, StatName, StatValue}|T], Timestamp, REPattern, Label) ->
	case re:run(atom_to_binary(StatName, utf8), REPattern, [{capture,all_names,binary}]) of
			nomatch -> 
				ServiceName = undefined,
				ServiceUrl = undefined,
				ServiceType = undefined;
			{match, [RowidBin, _Metric]} -> 
				Rowid = binary_to_integer(RowidBin),
				case ems_catalog:find_by_rowid(Rowid) of
					{ok, #service{name = Name, url = Url, type = Type}} ->
						ServiceName = Name,
						ServiceUrl = Url,
						ServiceType = Type;
					{error, enoent} ->
						ServiceName = undefined,
						ServiceUrl = undefined,
						ServiceType = undefined
				end
	end,
	Record = #stat_counter_hist{id = ems_db:sequence(stat_counter_hist),
								stat_name = StatName, 
								stat_value = StatValue,
								stat_date = ems_util:date_to_binary(Timestamp),
								stat_time = ems_util:time_to_binary(Timestamp),
								stat_service_name = ServiceName,
								stat_service_url = ServiceUrl,
								stat_service_type = ServiceType,
								stat_label = Label},
	mnesia:dirty_write(stat_counter_hist, Record),
	persist_stats(T, Timestamp, REPattern, Label).
	
	

