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
  
execute(Request) -> 
	Conf = ems_config:getConfig(),
	case ems_util:get_param_url(<<"name">>, undefined, Request) of
		undefined ->
			{error, Request#request{code = 400, 
									response_data = ?ENOENT_JSON}
			};
		AppName ->
			case ems_client:find_by_name(AppName) of
				{ok, Client} ->
					ContentData = iolist_to_binary([<<"{"/utf8>>,
						<<"\"ip\":\""/utf8>>, Conf#config.tcp_listen_main_ip, <<"\","/utf8>>,
						<<"\"base_url\":\""/utf8>>, Conf#config.rest_base_url, <<"\","/utf8>>,
						<<"\"auth_url\":\""/utf8>>, Conf#config.rest_auth_url, <<"\","/utf8>>,
						<<"\"auth_protocol\":\""/utf8>>, atom_to_binary(Conf#config.authorization, utf8), <<"\","/utf8>>,
						<<"\"app_id\":"/utf8>>, integer_to_binary(Client#client.id), <<","/utf8>>,
						<<"\"app_name\":\""/utf8>>, AppName, <<"\","/utf8>>,
						<<"\"app_version\":\""/utf8>>, Client#client.version, <<"\","/utf8>>,
						<<"\"environment\":\""/utf8>>, Conf#config.rest_environment, <<"\","/utf8>>,
						<<"\"url_mask\":"/utf8>>, ems_util:boolean_to_binary(Conf#config.rest_url_mask), <<","/utf8>>,
						<<"\"erlangms_version\":\""/utf8>>, list_to_binary(ems_util:version()), <<"\""/utf8>>,
						<<"}"/utf8>>]),
					{ok, Request#request{code = 200,
										 response_data = ContentData}
					};
				_ ->
					case ems_user:get_admim_user() of
						{ok, AdminUser} ->
							RestAuthUrl = binary_to_list(Conf#config.rest_auth_url),
							RestUrl = binary_to_list(erlang:iolist_to_binary([Conf#config.rest_base_url, <<"/auth/client?filter={\"name\":\"">>, AppName, <<"\"}&fields=id,version&limit=1">>])),
							GrantQuery = binary_to_list(iolist_to_binary([<<"grant_type=password&username=">>, AdminUser#user.name, <<"&password=">>, <<"5outLag1">>])),
							case httpc:request(post, {[RestAuthUrl], [], "application/x-www-form-urlencoded", GrantQuery}, [], []) of
								{ok, {_, _, AuthPayload}} ->
									case ems_util:json_decode_as_map(list_to_binary(AuthPayload)) of
										{ok, AuthParams} -> 
											case maps:is_key(<<"error">>, AuthParams) of
												true -> 
													{error, Request#request{code = 400, 
																			response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
													};
												false ->
													AccessToken = maps:get(<<"access_token">>, AuthParams),
													AuthorizationHeader = [{"Authorization", "Bearer " ++ binary_to_list(AccessToken)}],
													case httpc:request(get, {[RestUrl], AuthorizationHeader}, [],[]) of
														{ok,{_, _, ClientPayload}} ->
															case ems_util:json_decode_as_map(list_to_binary(ClientPayload)) of
																{ok, []} ->
																	{error, Request#request{code = 400, 
																							response_data = ?ENOENT_JSON}
																	};
																{ok, [ClientParams]} -> 
																	case maps:is_key(<<"error">>, ClientParams) of
																		true -> 
																			{error, Request#request{code = 400, 
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
																				<<"\"environment\":\""/utf8>>, Conf#config.rest_environment, <<"\","/utf8>>,
																				<<"\"url_mask\":"/utf8>>, ems_util:boolean_to_binary(Conf#config.rest_url_mask), <<","/utf8>>,
																				<<"\"erlangms_version\":\""/utf8>>, list_to_binary(ems_util:version()), <<"\""/utf8>>,
																				<<"}"/utf8>>]),
																			{ok, Request#request{code = 200,
																								response_data = ContentData}
																			}
																	end;
																_ -> 						
																	{error, Request#request{code = 400, 
																							response_data = <<y/utf8>>}
																	}
															end;
														_ ->
															{error, Request#request{code = 400, 
																					response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
															}
													end
											end;
										_ ->
											{error, Request#request{code = 400, 
																	response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
											}
									end;
								_ ->
									{error, Request#request{code = 400, 
															response_data = <<"{\"error\": \"eunavailable_rest_server\"}"/utf8>>}
									}
							end;
						_ ->
							{error, Request#request{code = 400, 
													response_data = <<"{\"error\": \"enoent_user_admin\"}"/utf8>>}
							}
					end
			end
	end.






