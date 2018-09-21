%%********************************************************************
%% @title Module ems_daemon_service
%% @version 1.0.0
%% @doc Module responsible for the execute daemon services
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_daemon_service).

-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").


-export([start/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_daemon/0, stop_daemon/0]).


-define(SERVER, ?MODULE).


-record(state, {daemon_id, 
				name,
				pid,
				port,
				timeout,
				start,
				restart,
				stop,
				pid_file,
				pid_file_watchdog_timeout,
				state}). 


%%====================================================================
%% Server API
%%====================================================================

start(Service) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Service, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
start_daemon() ->
	gen_server:cast(?SERVER, start_daemon).

stop_daemon() ->
	gen_server:cast(?SERVER, stop_daemon).
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{name = Name,
			  timeout = Timeout,
			  start_timeout = StartTimeout,
			  properties = Props}) ->
    NameStr = binary_to_list(Name),
    Port = maps:get(<<"port">>, Props, 0),
    StartCmd = binary_to_list(maps:get(<<"start">>, Props, <<>>)) ++ " &",
    StopCmd = maps:get(<<"stop">>, Props, <<>>),
	PidFileWatchDogTimeOut = ems_util:parse_range(maps:get(<<"pid_file_watchdog_timeout">>, Props, 60), 0, 99999),
    PidFile = maps:get(<<"pid_file">>, Props, <<>>),
    State = #state{name = NameStr,
				   timeout = Timeout,
				   port = Port,
				   start = StartCmd,
				   stop = StopCmd,
				   pid_file = PidFile,
				   pid_file_watchdog_timeout = PidFileWatchDogTimeOut,
				   daemon_id = "undefined",
				   state = start},
    {ok, State, StartTimeout}.
    
handle_cast(start_daemon, State = #state{timeout = Timeout}) ->
	State2 = do_start_daemon(State),
    {noreply, State2, Timeout};

handle_cast(stop_daemon, State) ->
	State2 = do_stop_daemon(State),
    {noreply, State2};

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_info(timeout, State = #state{state = start}) ->
	State2 = do_start_daemon(State),
	{noreply, State2, State2#state.timeout};

handle_info(timeout, State = #state{state = monitoring}) ->
	State2 = do_monitoring(State),
	{noreply, State2, State2#state.timeout}.


terminate(_, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

do_start_daemon(State = #state{start = CmdStart, 
							   port = Port,
							   name = Name,
							   filename = Filename}) ->
	case ems_util:get_pid_from_port(Port) of
		{ok, CurrentPid} -> 
			ems_logger:error("ems_daemon_service ~s daemon already exist with pid ~p, stopping before start new...", [Name, CurrentPid]),
			State2 = do_stop_daemon(State#state{pid = CurrentPid, daemon_id = "undefined"}),
			fica_em_loop_ate_encerrar_pid(Port, 15),
			do_start_daemon(State2);
		_ ->
			ems_logger:info("ems_daemon_service ~s starting new daemon, please wait...", [Name]),
			DaemonId = integer_to_list(ems_util:get_milliseconds()),	
			CmdStart2 = ems_util:replace_all_vars(CmdStart, [{<<"PORT">>, integer_to_list(Port)},
															 {<<"DAEMON_ID">>, DaemonId},
															 {<<"DAEMON_SERVICE">>, Name},
															 {<<"FILENAME">>, Filename},
															 {<<"JAVA_HOME">>, binary_to_list(ems_util:get_environment_variable(<<"JAVA_HOME">>))}]),
			os:cmd(CmdStart2, #{ max_size => 0 }),
			case fica_em_loop_ate_obter_pid(Port, 15) of
				{ok, Pid} -> 
					ems_logger:info("ems_daemon_service ~s started new daemon (Pid: ~p Port: ~p, DaemonId: ~s). \033[0;32mOS Command\033[0m: ~s.", [Name, Pid, Port, DaemonId, CmdStart2]),
					State#state{pid = Pid, state = monitoring, daemon_id = DaemonId};
				{error, einvalid_port} -> 
					ems_logger:error("ems_daemon_service ~s start new daemon failed. Reason: einvalid_port"),
					State#state{state = start}
			end
	end.
	

do_stop_daemon(State = #state{stop = Cmd, 
							  name = Name,
							  port = Port,
							  pid = Pid,
							  daemon_id = DaemonId}) ->
    ems_logger:info("ems_daemon_service ~s stopping daemon (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
	Cmd2 = ems_util:replace_all_vars(Cmd, [{<<"PORT">>, integer_to_list(Port)},
										   {<<"DAEMON_PID">>, integer_to_list(Pid)}]),
    os:cmd(Cmd2, #{ max_size => 0 }),
	ems_logger:info("ems_daemon_service ~s stopped daemon (Pid: ~p Port: ~p, DaemonId: ~s). \033[0;32mOS Command\033[0m: ~s.", [Name, Pid, Port, DaemonId, Cmd2]),
    State#state{state = stopped}.


do_watchdog_reset(Message, State = #state{port = Port}) ->
	ems_logger:error(Message),
	State2 = do_stop_daemon(State),  % mata o processo anterior se ainda estiver vivo
	fica_em_loop_ate_encerrar_pid(Port, 15),
	do_start_daemon(State2).


do_monitoring(State = #state{name = Name,
							 port = Port,
							 pid = Pid,
							 pid_file = PidFile,
						     pid_file_watchdog_timeout = PidFileWatchdogTimeout,
							 daemon_id = DaemonId}) ->
	case ems_util:get_pid_from_port(Port) of
		{ok, CurrentPid} -> 
			case CurrentPid == Pid of
				true ->
					case PidFileWatchdogTimeout > 0 of
						true ->
							% o processo é responsável por atualizar seu arquivo de pid
							% se o timestamp do arquivo de pid estiver desatualiado, o processo será resetado
							PidFile2 = ems_util:replace_all_vars(PidFile, [{<<"PORT">>, integer_to_list(Port)},
									 									   {<<"DAEMON_ID">>, DaemonId},
									 									   {<<"DAEMON_PID">>, integer_to_list(Pid)},
																		   {<<"DAEMON_SERVICE">>, Name}]),
							case file:read_file_info(PidFile2, []) of
								{ok,{file_info, _FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} -> 
									case erlang:abs(calendar:datetime_to_gregorian_seconds(calendar:local_time()) - calendar:datetime_to_gregorian_seconds(MTime)) > PidFileWatchdogTimeout of
										true -> 
											do_watchdog_reset(io_lib:format("ems_daemon_service ~s pidfile ~s is outdated for ~pms, resetting the process to return to normal (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, PidFileWatchdogTimeout, Pid, Port, DaemonId]), State);
										false -> 
											State#state{pid = Pid, state = monitoring}
									end;
								{error, enoent} -> 
									do_watchdog_reset(io_lib:format("ems_daemon_service ~s pidfile ~s was deleted, resetting the process to return to normal (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, Pid, Port, DaemonId]), State);
								{error, Reason} -> 
									ems_logger:error("ems_daemon_service ~s pidfile ~s is not accessible by the ErlangMS. Reason: ~p (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, Reason, Pid, Port, DaemonId]),
									State#state{pid = Pid, state = monitoring}
							end;
						false ->
							ems_logger:info("ems_daemon_service ~s daemon is ok (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
							State#state{pid = Pid, state = monitoring}
					end;
				false ->
					% o pid não é o mesmo então outro processo foi iniciado pelo sistema operacional
					% não podemos permitir isso pois é o ems_daemon_service o responsável pelo processo
					do_watchdog_reset(io_lib:format("ems_daemon_service ~s will be restarted due new process started in the operating system with other pid (Pid: ~p, OtherPid: ~p).", [Name, Pid, CurrentPid]), State)
			end;	
		_ ->
			% O pid não existe mais, inicia um novo processo
			ems_logger:error("ems_daemon_service ~s will start again because the existing process has been terminated in the operating system. (Pid: ~p).", [Name, Pid]),
			do_start_daemon(State)
	end.




% Fica em um loop até conseguir obter o pid por meio da porta utilizada pelo processo externo
fica_em_loop_ate_obter_pid(_, 0) -> {error, einvalid_port};
fica_em_loop_ate_obter_pid(Port, Tentativas) ->
	ems_util:sleep(3000),
	case ems_util:get_pid_from_port(Port) of
		{ok, Pid} -> {ok, Pid};
		{error, einvalid_port} -> fica_em_loop_ate_obter_pid(Port, Tentativas - 1)
	end.

				
% Fica em um loop até o pid ser encerrado
fica_em_loop_ate_encerrar_pid(_, 0) -> ok;
fica_em_loop_ate_encerrar_pid(Port, Tentativas) ->
	ems_util:sleep(3000),
	case ems_util:get_pid_from_port(Port) of
		{ok, _Pid} -> fica_em_loop_ate_encerrar_pid(Port, Tentativas - 1);
		_ -> ok
	end.
									
	

