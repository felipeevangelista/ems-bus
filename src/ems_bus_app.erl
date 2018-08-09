-module(ems_bus_app).

-behaviour(application).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").


%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, StartArgs) ->
	erlang:system_flag(min_heap_size, 22000),
	{ok, Hostname} = inet:gethostname(),
	io:format("\n"),
	T1 = ems_util:get_milliseconds(),
	ems_logger:format_alert("Loading ErlangMS ~s ( Hostname: ~s  Erlang/OTP Version: ~s )", [?SERVER_NAME, Hostname, erlang:system_info(otp_release)]),
	ems_db:start(),
	case ems_config:start() of
		{ok, _Pid} ->
			Conf = ems_config:getConfig(),
			ems_logger:set_level(info),
			ems_dispatcher:start(),
			Ret = ems_bus_sup:start_link(StartArgs),
			ems_logger:info("Hosts in the cluster: ~p.", [ems_util:get_host_list()]),
			AuthorizationMode = case Conf#config.authorization of
									basic -> <<"basic, oauth2">>;
									oauth2 -> <<"oauth2">>;
									public -> <<"public">>
								end,
			ems_logger:info("rest_base_url: ~p.", [Conf#config.rest_base_url]),
			ems_logger:info("rest_auth_url: ~p.", [Conf#config.rest_auth_url]),
			case Conf#config.oauth2_with_check_constraint of
				true -> ems_logger:info("rest_authorization: ~p <<with check constraint>>.", [AuthorizationMode]);
				false -> ems_logger:info("rest_authorization: ~p.", [AuthorizationMode])
			end,
			ems_logger:info("Server ~s (PID ~s) started in ~pms.", [?SERVER_NAME, os:getpid(), ems_util:get_milliseconds() - T1]),
			Ret;
		{error, Reason} ->
			io:format("Error processing configuration file. Reason: ~p.", [Reason]),
			{error, finish}
	end.

stop(_State) ->
    ems_logger:info("Stopping server...\n"),
    ems_logger:sync(),
    ems_bus_sup:stop(),
    ems_logger:stop(),
	ems_config:stop(),
    ok.
    
    
													 
    
