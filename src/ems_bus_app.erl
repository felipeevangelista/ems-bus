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
	io:format("\n"),
	T1 = ems_util:get_milliseconds(),
	ems_db:start(),
	case ems_config:start() of
		{ok, _Pid} ->
			Conf = ems_config:getConfig(),
			ems_logger:format_alert("Loading ~s instance ( Hostname: ~s  Erlang/OTP Version: ~s )", [?SERVER_NAME, Conf#config.ems_hostname, erlang:system_info(otp_release)]),
			ems_logger:set_level(info),
			ems_dispatcher:start(),
			Ret = ems_bus_sup:start_link(StartArgs),
			ems_logger:info("Hosts in the cluster: ~p.", [ems_util:get_host_list()]),
			AuthorizationMode = case Conf#config.authorization of
									basic -> <<"basic, oauth2">>;
									oauth2 -> <<"oauth2">>;
									public -> <<"public">>
								end,
			ems_logger:info("Parameters:"),
			ems_logger:info("  hostname: ~p.", [Conf#config.ems_hostname]),
			ems_logger:info("  rest_base_url: ~p.", [Conf#config.rest_base_url]),
			ems_logger:info("  rest_auth_url: ~p.", [Conf#config.rest_auth_url]),
			ems_logger:info("  rest_login_url: ~p.", [Conf#config.rest_login_url]),
			case Conf#config.oauth2_with_check_constraint of
				true -> ems_logger:info("  rest_authorization: ~p <<with check constraint>>.", [AuthorizationMode]);
				false -> ems_logger:info("  rest_authorization: ~p.", [AuthorizationMode])
			end,
			ems_logger:info("  rest_environment: ~p.", [Conf#config.rest_environment]),
			ems_logger:info("  oauth2_refresh_token: ~p.", [Conf#config.oauth2_refresh_token]),
			ems_logger:info("  auth_allow_user_inative_credentials: ~p.", [Conf#config.auth_allow_user_inative_credentials]),
			ems_logger:info("  tcp_listen_address: ~p.", [Conf#config.tcp_listen_address]),
			ems_logger:info("  tcp_allowed_address: ~p.", [Conf#config.tcp_allowed_address]),
			ems_logger:info("  log_show_response: ~p.", [Conf#config.log_show_response]),
			ems_logger:info("  log_show_payload: ~p.", [Conf#config.log_show_payload]),
			ems_logger:info("  log_show_response_max_length: ~p bytes.", [Conf#config.log_show_response_max_length]),
			ems_logger:info("  log_show_payload_max_length: ~p bytes.", [Conf#config.log_show_payload_max_length]),
			ems_logger:info("  log_file_checkpoint: ~pms.", [Conf#config.log_file_checkpoint]),
			ems_logger:info("  log_file_max_size: ~p bytes.", [Conf#config.log_file_max_size]),
			ems_logger:info("  www_path: ~p.", [maps:get(<<"www_path">>, Conf#config.static_file_path_map, <<>>)]),
			ems_logger:info("  show_debug_response_headers: ~p.", [Conf#config.show_debug_response_headers]),
			ems_logger:info("  ems_result_cache: ~pms.", [Conf#config.ems_result_cache]),
			ems_logger:info("  http_max_content_length: ~p bytes.", [Conf#config.http_max_content_length]),
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
	ems_config:stop(),
    ok.
    
    
													 
    
