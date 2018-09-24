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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, 
		 start_daemon/0, stop_daemon/0, kill_daemon/0, restart_daemon/0, verify_daemon/0]).


-define(SERVER, ?MODULE).


-record(state, {daemon_id, 
				name,
				pid,
				port,
				timeout,
				start_cmd,
				stop_cmd,
				kill_cmd,
				pidfile,
				pidfile_watchdog_timer,
				filename,
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

kill_daemon() ->
	gen_server:cast(?SERVER, kill_daemon).

restart_daemon() ->
	gen_server:cast(?SERVER, restart_daemon).

verify_daemon() ->
	gen_server:cast(?SERVER, verify_daemon).

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(#service{name = Name,
			  timeout = Timeout,
			  start_timeout = StartTimeout,
			  filename = Filename,
			  properties = Props}) ->
    NameStr = binary_to_list(Name),
    Port = maps:get(<<"port">>, Props, 0),
    StartCmd = parse_start_cmd(maps:get(<<"start_cmd">>, Props, <<>>)),
    StopCmd = maps:get(<<"stop_cmd">>, Props, <<>>),
    KillCmd = maps:get(<<"kill_cmd">>, Props, <<>>),
	PidFileWatchDogTimeOut = ems_util:parse_range(maps:get(<<"pidfile_watchdog_timer">>, Props, 30000), 0, 86400000),
    PidFile = maps:get(<<"pidfile">>, Props, <<>>),
    State = #state{name = NameStr,
				   timeout = Timeout,
				   port = Port,
				   start_cmd = StartCmd,
				   stop_cmd = StopCmd,
				   kill_cmd = KillCmd,
				   pidfile = PidFile,
				   pidfile_watchdog_timer = PidFileWatchDogTimeOut,
				   daemon_id = "undefined",
				   filename = Filename,
				   state = start},
    {ok, State, StartTimeout}.
    
handle_cast(start_daemon, State = #state{timeout = Timeout}) ->
	State2 = do_start_daemon(State),
    {noreply, State2, Timeout};

handle_cast(stop_daemon, State) ->
	State2 = do_stop_daemon(State),
    {noreply, State2};

handle_cast(kill_daemon, State) ->
	State2 = do_kill_daemon(State),
    {noreply, State2};

handle_cast(restart_daemon, State = #state{name = Name, pid = Pid, timeout = Timeout}) ->
	Msg = io_lib:format("ems_daemon_service ~s daemon already exist with unknow pid ~p, stopping before start new...", [Name, Pid]),
	State2 = do_restart_daemon(Msg, State),
    {noreply, State2, Timeout};

handle_cast(verify_daemon, State = #state{timeout = Timeout}) ->
	State2 = do_verify_daemon(State),
    {noreply, State2, Timeout};

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
	State2 = do_verify_daemon(State),
	{noreply, State2, State2#state.timeout}.


terminate(_, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Internal functions
%%====================================================================

do_start_daemon(State = #state{start_cmd = CmdStart, 
							   port = Port,
							   name = Name,
							   pidfile = Pidfile,
							   pidfile_watchdog_timer = PidfileWatchdogTimer,
							   filename = Filename}) ->
	case ems_util:get_pid_from_port(Port) of
		{ok, CurrentPid} -> 
			Msg = io_lib:format("ems_daemon_service ~s daemon already exist with unknow pid ~p, stopping before start new...", [Name, CurrentPid]),
			do_restart_daemon(Msg, State#state{pid = CurrentPid});
		_ ->
			DaemonId = integer_to_list(ems_util:get_milliseconds()),	
			CmdStart2 = string:trim(ems_util:replace_all_vars(CmdStart, 
															[{<<"PORT">>, integer_to_list(Port)},
															 {<<"DAEMON_ID">>, DaemonId},
															 {<<"DAEMON_SERVICE">>, Name},
															 {<<"FILENAME">>, Filename},
															 {<<"PIDFILE">>, Pidfile},
															 {<<"PIDFILE_WATCHDOG_TIMER">>, integer_to_list(PidfileWatchdogTimer)},
															 {<<"JAVA_HOME">>, ems_util:get_java_home()}])),
			ems_logger:info("ems_daemon_service ~s starting new daemon. \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, CmdStart2]),
			case CmdStart2 =/= "" of
				true ->
					file:delete(Pidfile),
					case ems_util:os_command(CmdStart2, #{ max_size => 0 }) of
						{ok, _Result} ->
							case fica_em_loop_ate_obter_pid(Port, 15) of
								{ok, Pid} -> 
									ems_logger:info("ems_daemon_service ~s new daemon started (Pid: ~p Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
									State#state{pid = Pid, state = monitoring, daemon_id = DaemonId};
								_ -> 
									ems_logger:error("ems_daemon_service ~s start new daemon failed because does not get pid of process started.", [Name]),
									State#state{state = start, daemon_id = "undefined"}
							end;
						{error, einvalid_command} ->
							ems_logger:error("ems_daemon_service ~s failed to start daemon with start_cmd parameter. \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, CmdStart2])
					end;
				false ->
					ems_logger:error("ems_daemon_service ~s failed to start daemon because start_cmd parameter is empty.", [Name]),
					State#state{state = stopped, daemon_id = "undefined"}
			end
	end.
	

do_stop_daemon(State = #state{stop_cmd = CmdStop, 
							  name = Name,
							  port = Port,
							  pid = Pid,
							  pidfile = Pidfile,
							  filename = Filename,
							  daemon_id = DaemonId}) ->
	CmdStop2 = string:trim(ems_util:replace_all_vars(CmdStop, 
													  [{<<"PORT">>, integer_to_list(Port)},
													   {<<"DAEMON_PID">>, integer_to_list(Pid)},
													   {<<"DAEMON_ID">>, DaemonId},
													   {<<"DAEMON_SERVICE">>, Name},
													   {<<"FILENAME">>, Filename},
													   {<<"PIDFILE">>, Pidfile},
													   {<<"JAVA_HOME">>, ems_util:get_java_home()}])),
    ems_logger:info("ems_daemon_service ~s stopping daemon (Pid: ~p, Port: ~p, DaemonId: ~s).  \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, Pid, Port, DaemonId, CmdStop2]),
    case CmdStop2 =/= "" of
		true ->
			case ems_util:os_command(CmdStop2, #{ max_size => 0 }) of
				{ok, _Result} ->
					ems_logger:info("ems_daemon_service ~s daemon stopped (Pid: ~p Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
					State#state{state = stopped, daemon_id = "undefined"};
				{error, einvalid_command} ->
					ems_logger:error("ems_daemon_service ~s failed to stop daemon with stop_cmd parameter. \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, CmdStop2]),
					State#state{pid = Pid, state = monitoring}
			end;
		false ->
			ems_logger:error("ems_daemon_service ~s failed to stop daemon because stop_cmd parameter is empty.", [Name]),
			State#state{pid = Pid, state = monitoring}
	end.
	

do_kill_daemon(State = #state{kill_cmd = CmdStop, 
							  name = Name,
							  port = Port,
							  pid = Pid,
							  pidfile = Pidfile,
							  filename = Filename,
							  daemon_id = DaemonId}) ->
	CmdStop2 = string:trim(ems_util:replace_all_vars(CmdStop, 
													  [{<<"PORT">>, integer_to_list(Port)},
													   {<<"DAEMON_PID">>, integer_to_list(Pid)},
													   {<<"DAEMON_ID">>, DaemonId},
													   {<<"DAEMON_SERVICE">>, Name},
													   {<<"FILENAME">>, Filename},
													   {<<"PIDFILE">>, Pidfile},
													   {<<"JAVA_HOME">>, ems_util:get_java_home()}])),
    ems_logger:info("ems_daemon_service ~s killing daemon (Pid: ~p, Port: ~p, DaemonId: ~s).  \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, Pid, Port, DaemonId, CmdStop2]),
    case CmdStop2 =/= "" of
		true ->
			case ems_util:os_command(CmdStop2, #{ max_size => 0 }) of
				{ok, _Result} ->
					ems_logger:info("ems_daemon_service ~s daemon killed (Pid: ~p Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
					State#state{state = stopped, daemon_id = "undefined"};
				{error, einvalid_command} ->
					ems_logger:error("ems_daemon_service ~s failed to kill daemon with kill_cmd parameter. \033[0;32mOS Command\033[0m: \033[01;34m~s\033[0m.", [Name, CmdStop2]),
					State#state{pid = Pid, state = monitoring}
			end;
		false ->
			ems_logger:error("ems_daemon_service ~s failed to kill daemon because kill_cmd parameter is empty.", [Name]),
			State#state{pid = Pid, state = monitoring}
	end.


do_restart_daemon(Message, State = #state{port = Port, pid = Pid}) ->
	ems_logger:error(Message),
	State2 = do_stop_daemon(State),  % envia sinal para o processo ser encerrado normalmente
	fica_em_loop_ate_encerrar_pid(Port, Pid, 15),
	case ems_util:get_pid_from_port(Port) of
		{ok, Pid2} when Pid2 =:= Pid -> 
			State3 = do_kill_daemon(State2);  % mata o processo anterior se ainda estiver vivo
		_ -> State3 = State2
	end,
	do_start_daemon(State3).


% Monitora o processo externo executado por start_cmd
% Se o processo morrer outro será reiniciado
% Além disso, se pidfile_watchdog_timer > 0, testa se o pid está atualizado
do_verify_daemon(State = #state{name = Name,
							 port = Port,
							 pid = Pid,
						     pidfile_watchdog_timer = PidFileWatchdogTimeout,
							 daemon_id = DaemonId}) ->
	ems_logger:info("ems_daemon_service ~s verify daemon (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
	case ems_util:get_pid_from_port(Port) of
		{ok, CurrentPid} -> 
			case CurrentPid == Pid of
				true ->
					case PidFileWatchdogTimeout > 0 of
						true -> 
							do_pidfile_watchdog_check(State, 1);
						false ->
							ems_logger:info("ems_daemon_service ~s daemon is ok (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
							State#state{pid = Pid, state = monitoring}
					end;
				false ->
					% o pid não é o mesmo então outro processo foi iniciado pelo sistema operacional
					% não podemos permitir isso pois é o ems_daemon_service o responsável pelo processo
					do_restart_daemon(io_lib:format("ems_daemon_service ~s will be restarted due new process started in the operating system with other pid (Pid: ~p, OtherPid: ~p).", [Name, Pid, CurrentPid]), State)
			end;	
		_ ->
			% O pid não existe mais, inicia um novo processo
			ems_logger:error("ems_daemon_service ~s will start again because the existing process has been terminated in the operating system. (Pid: ~p).", [Name, Pid]),
			do_start_daemon(State)
	end.


% O processo executado pelo start_cmd é responsável por atualizar seu próprio arquivo de pid (a menos que watchdog não seja usado)
% Se o timestamp do arquivo de pid estiver desatualiado, o processo será reiniciado pois pode indicar que o processo travou
% O teste é realizado pelo menos 3 vezes para ser conservador com intervalos de 1 segundo
do_pidfile_watchdog_check(State = #state{name = Name,
								 port = Port,
								 pid = Pid,
								 pidfile = PidFile,
								 pidfile_watchdog_timer = PidFileWatchdogTimeout,
								 filename = Filename,
								 daemon_id = DaemonId}, Tentativa) ->

	ems_logger:info("ems_daemon_service ~s watchdog pidfile daemon (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, Pid, Port, DaemonId]),
	PidFile2 = ems_util:replace_all_vars(PidFile, [{<<"PORT">>, integer_to_list(Port)},
												   {<<"DAEMON_ID">>, DaemonId},
												   {<<"DAEMON_PID">>, integer_to_list(Pid)},
												   {<<"DAEMON_SERVICE">>, Name},
												   {<<"FILENAME">>, Filename},
												   {<<"JAVA_HOME">>, ems_util:get_java_home()}]),
	case file:read_file_info(PidFile2, []) of
		{ok,{file_info, _FSize, _Type, _Access, _ATime, MTime, _CTime, _Mode,_,_,_,_,_,_}} -> 
			TimeAtual = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
			TimePidfile = calendar:datetime_to_gregorian_seconds(MTime),
			case erlang:abs((TimeAtual - TimePidfile) * 1000) > PidFileWatchdogTimeout of
				true -> 
					case Tentativa == 3 of
						true ->
							Msg = io_lib:format("ems_daemon_service ~s pidfile \033[01;34m~s\033[0m is outdated for ~pms, restart the process to return to normal (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, PidFileWatchdogTimeout, Pid, Port, DaemonId]),
							do_restart_daemon(Msg, State);
						false -> 
							ems_util:sleep(1000),
							do_pidfile_watchdog_check(State, Tentativa + 1)
					end;
				false -> 
					State#state{pid = Pid, state = monitoring}
			end;
		{error, enoent} -> 
			case Tentativa == 3 of
				true -> do_restart_daemon(io_lib:format("ems_daemon_service ~s pidfile \033[01;34m~s\033[0m was deleted, restart the process to return to normal (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, Pid, Port, DaemonId]), State);
				false -> do_pidfile_watchdog_check(State, Tentativa + 1)
			end;
		{error, Reason} -> 
			ems_logger:error("ems_daemon_service ~s pidfile \033[01;34m~s\033[0m is not accessible by the ErlangMS. Reason: ~p (Pid: ~p, Port: ~p, DaemonId: ~s).", [Name, PidFile2, Reason, Pid, Port, DaemonId]),
			State#state{pid = Pid, state = monitoring}
	end.



% Fica em um loop até conseguir obter o pid por meio da porta utilizada pelo processo externo
fica_em_loop_ate_obter_pid(_, 0) -> {error, enoent};
fica_em_loop_ate_obter_pid(Port, Tentativas) ->
	ems_util:sleep(1000),
	case ems_util:get_pid_from_port(Port) of
		{ok, Pid} -> {ok, Pid};
		_ -> fica_em_loop_ate_obter_pid(Port, Tentativas - 1)
	end.

				
% Fica em um loop até o pid ser encerrado
fica_em_loop_ate_encerrar_pid(_, _, 0) -> ok;
fica_em_loop_ate_encerrar_pid(Port, Pid, Tentativas) ->
	ems_util:sleep(1000),
	case ems_util:get_pid_from_port(Port) of
		{ok, Pid2} when Pid2 =:= Pid -> fica_em_loop_ate_encerrar_pid(Port, Pid, Tentativas - 1);
		_ -> ok
	end.
									
	
% O comando cmd_start precisa ter um & no final para o barramento não ficar preso 
% com a execução do comando
parse_start_cmd(Cmd) -> 
	CmdStr = string:trim(binary_to_list(Cmd)),
	case lists:reverse(CmdStr) of
		"&" ++ _ -> CmdStr;
		_ -> CmdStr ++ " &"
	end.
			
		


