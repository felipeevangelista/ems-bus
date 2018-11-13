%%********************************************************************
%% @title Module ems_barramento_service
%% @version 1.0.0
%% @doc Gera o arquivo /sistema/barramento para consulta dos frontends
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************tn************************************************

-module(ems_barramento_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([execute/1]).
  
execute(Request = #request{timestamp = Timestamp,
						   response_header = ResponseHeader,
						   service = #service{cache_control = CacheControlService,
											  expires = ExpiresMinute}}) -> 
	Conf = ems_config:getConfig(),
	case ems_util:get_param_url(<<"name">>, undefined, Request) of
		undefined ->
			{error, Request#request{code = 400, 
									response_header = ResponseHeader#{<<"cache-control">> => ?CACHE_CONTROL_NO_CACHE},
									response_data = ?ENOENT_JSON}
			};
		AppName ->
			RestAuthUrl = binary_to_list(Conf#config.rest_auth_url),
			RestUrl = ems_util:url_mask_str(erlang:iolist_to_binary([<<"/auth/client?filter={\"name\":\"">>, AppName, <<"\"}&fields=id,version&limit=1">>])),
			RestUri = binary_to_list(Conf#config.rest_base_url) ++ RestUrl,
			GrantQuery = binary_to_list(iolist_to_binary([<<"grant_type=password&username=">>, Conf#config.rest_user, <<"&password=">>, Conf#config.rest_passwd])),
			% Tenta autenticar no barramento rest com o usuário e senha do usuário erlangms
			% Na segunda tentativa, tenta pela senha padrão (barramento utilizado pelo desenvolvedor)
			case httpc:request(post, {[RestAuthUrl], [], "application/x-www-form-urlencoded", GrantQuery}, [], []) of
				{ok, {_, _, AuthPayload}} -> ResultAuth = {ok, AuthPayload};
				_ -> 
					GrantQuery2 = binary_to_list(iolist_to_binary([<<"grant_type=password&username=">>, Conf#config.rest_user, <<"&password=">>, <<"fEqNCco3Yq9h5ZUglD3CZJT4lBs=">>])),
					case httpc:request(post, {[RestAuthUrl], [], "application/x-www-form-urlencoded", GrantQuery2}, [], []) of
						{ok, {_, _, AuthPayload2}} -> ResultAuth = {ok, AuthPayload2};
						_ -> ResultAuth = {error, einvalid_client_request_authorization}
					end
			end,
			case ResultAuth of
				{ok, ResultAuthPayload} ->
					case ems_util:json_decode_as_map(list_to_binary(ResultAuthPayload)) of
						{ok, AuthParams} -> 
							case maps:is_key(<<"error">>, AuthParams) of
								true -> 
									{error, Request#request{code = 400, 
															reason = eclient_payload_return_error,
															response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
									};
								false ->
									AccessToken = maps:get(<<"access_token">>, AuthParams),
									AuthorizationHeader = [{"Authorization", "Bearer " ++ binary_to_list(AccessToken)}],
									case httpc:request(get, {RestUri, AuthorizationHeader}, [],[]) of
										{ok,{_, _, ClientPayload}} ->
											case ems_util:json_decode_as_map(list_to_binary(ClientPayload)) of
												{ok, []} ->
													{error, Request#request{code = 400, 
																			reason = eclient_payload_isempty,
																			response_data = ?ENOENT_JSON}
													};
												{ok, [ClientParams]} -> 
													case maps:is_key(<<"error">>, ClientParams) of
														true -> 
															{error, Request#request{code = 400, 
																					reason = eclient_payload_error,
																					response_data = ?ENOENT_JSON}
															};
														false ->
															ClientId = maps:get(<<"id">>, ClientParams),
															ClientVersion = maps:get(<<"version">>, ClientParams),
															ContentData = iolist_to_binary([<<"{"/utf8>>,
																<<"\"ip\":\""/utf8>>, Conf#config.tcp_listen_main_ip, <<"\","/utf8>>,
																<<"\"base_url\":\""/utf8>>, Conf#config.rest_base_url, <<"\","/utf8>>,
																<<"\"auth_url\":\""/utf8>>, Conf#config.rest_auth_url, <<"\","/utf8>>,
																<<"\"auth_protocol\":\""/utf8>>, atom_to_binary(Conf#config.authorization, utf8), <<"\","/utf8>>,
																<<"\"app_id\":"/utf8>>, integer_to_binary(ClientId), <<","/utf8>>,
																<<"\"app_name\":\""/utf8>>, AppName, <<"\","/utf8>>,
																<<"\"app_version\":\""/utf8>>, ClientVersion, <<"\","/utf8>>,
																<<"\"server_name\":\""/utf8>>, Conf#config.ems_hostname, <<"\","/utf8>>,
																<<"\"environment\":\""/utf8>>, Conf#config.rest_environment, <<"\","/utf8>>,
																<<"\"url_mask\":"/utf8>>, ems_util:boolean_to_binary(Conf#config.rest_url_mask), <<","/utf8>>,
																<<"\"erlangms_version\":\""/utf8>>, list_to_binary(ems_util:version()), <<"\""/utf8>>,
																<<"}"/utf8>>]),
															ExpireDate = ems_util:date_add_minute(Timestamp, ExpiresMinute + 180),
															Expires = cowboy_clock:rfc1123(ExpireDate),
															{ok, Request#request{code = 200,
																				 response_header = ResponseHeader#{<<"cache-control">> => CacheControlService,
																												   <<"expires">> => Expires},
																				 response_data = ContentData}
															}
													end;
												_ -> 						
													{error, Request#request{code = 400, 
																			reason = einvalid_decode_client_json,
																			response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
													}
											end;
										_ ->
											{error, Request#request{code = 400, 
																	reason = einvalid_client_request,
																	response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
											}
									end
							end;
						_ ->
							{error, Request#request{code = 400, 
													reason = einvalid_decode_client_json,
													response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
							}
					end;
				{error, Reason} ->
					{error, Request#request{code = 400, 
											reason = Reason,
											response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
					}
			end
	end.

