%%********************************************************************
%% @title Module ems_dispatcher
%% @version 1.0.0
%% @doc Responsible for forwarding the requests to services.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dispatcher).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%% Client API
-export([start/0, dispatch_request/3, dispatch_service_work/3]).


start() -> 
	ems_cache:new(ets_result_cache_get),
	ets:new(ems_dispatcher_post_time, [set, named_table, public]),
	ets:insert(ems_dispatcher_post_time, {post_time, 0}),
	ets:new(ctrl_node_dispatch, [set, named_table, public]).


check_result_cache(ReqHash, Worker, Timestamp2) ->
	case ets:lookup(ets_result_cache_get, ReqHash) of
		[] -> false; 
		[{_, {Timestamp, _, ResultCache, _, _}}] when Timestamp2 - Timestamp > ResultCache -> false;
		[{_, {Timestamp, Request, _, req_done, _}}] ->
			[{post_time, PostTime}] = ets:lookup(ems_dispatcher_post_time, post_time),
			case PostTime < Timestamp of
				true -> {true, Request};
				false -> false
			end;
		[{_, {T1, Request, ResultCache, Status, WorkersWaiting}}] ->
			ets:insert(ets_result_cache_get, {ReqHash, {T1, Request, ResultCache, Status, [Worker | WorkersWaiting]}}),
			receive 
				Msg -> 
					case Msg of
						{ReqHash, Result} -> 
							Result;
						_ -> 
							false
					end
				after 300 -> 
					check_result_cache2(ReqHash, Worker, Timestamp2, 6)
			end
	end.

check_result_cache2(_, _, _, 0) -> false;
check_result_cache2(ReqHash, Worker, Timestamp2, Count) ->
	case ets:lookup(ets_result_cache_get, ReqHash) of
		[] -> false; 
		[{_, {Timestamp, _, ResultCache, _, _}}] when Timestamp2 - Timestamp > ResultCache -> false;
		[{_, {_, Request, _, req_done, _}}] ->
			{true, Request};
		_ ->
			receive 
				Msg -> 
					case Msg of
						{ReqHash, Result} -> 
							Result;
						_ -> 
							false
					end
				after 100 -> 
					check_result_cache2(ReqHash, Worker, Timestamp2, Count - 1)
			end
	end.
	
notity_workers_waiting_result_cache(ReqHash, RequestDone) ->
	case ets:lookup(ets_result_cache_get, ReqHash) of
		[] -> ok; 
		[{_, {T1, _, ResultCache, _, WorkersWaiting}}] ->
			ets:insert(ets_result_cache_get, {ReqHash, {T1, RequestDone, ResultCache, req_done, []}}),
			notity_workers_waiting_result_cache_(WorkersWaiting, RequestDone, ReqHash) 
	end.

notity_workers_waiting_result_cache_([], _, _) -> ok;
notity_workers_waiting_result_cache_([Worker|T], RequestDone, ReqHash) ->
	Worker ! {ReqHash, {true, RequestDone}},
	notity_workers_waiting_result_cache_(T, RequestDone, ReqHash).


dispatch_request(Request = #request{req_hash = ReqHash, 
								     ip = Ip,
								     type = Type,
								     if_modified_since = IfModifiedSince,
									 if_none_match = IfNoneMatch,
								     t1 = T1,
								     worker_send = WorkerSend},
				 Service = #service{tcp_allowed_address_t = AllowedAddress,
									 result_cache = ResultCache,
									 service_exec_metric_name = ServiceExecMetricName,
									 service_result_cache_hit_metric_name = ServiceResultCacheHitMetricName,
									 service_host_denied_metric_name = ServiceHostDeniedMetricName,
									 service_auth_denied_metric_name = ServiceAuthDeniedMetricName},
				ShowDebugResponseHeaders) -> 
	?DEBUG("ems_dispatcher lookup request ~p.", [Request]),
	ems_db:inc_counter(ServiceExecMetricName),				
	case ems_util:allow_ip_address(Ip, AllowedAddress) of
		true ->
			case ems_auth_user:authenticate(Service, Request) of
				{ok, Client, User, AccessToken, Scope} -> 
					Request2 = Request#request{client = Client,
											   user = User,
											   scope = Scope,
											   access_token = AccessToken},
					case Type of
						<<"OPTIONS">> -> 
								{ok, request, Request2#request{code = 200, 
															    content_type_out = ?CONTENT_TYPE_JSON,
															    response_data = ems_catalog:get_metadata_json(Service),
															    latency = ems_util:get_milliseconds() - T1}
								};
						"HEAD" -> 
								{ok, request, Request2#request{code = 200, 
															    latency = ems_util:get_milliseconds() - T1}
								};
						<<"GET">> ->
							case ResultCache > 0 of
								true ->
									case check_result_cache(ReqHash, WorkerSend, T1) of
										{true, RequestCache} -> 
											ems_db:inc_counter(ServiceResultCacheHitMetricName),								
											case IfNoneMatch =/= <<>> orelse IfModifiedSince =/= <<>> of
												true ->
													{ok, request, Request2#request{result_cache = true,
																					code = 304,
																					reason = enot_modified,
																					content_type_out = RequestCache#request.content_type_out,
																					response_data = <<>>,
																					response_header = RequestCache#request.response_header,
																					result_cache_rid = RequestCache#request.rid,
																					etag = RequestCache#request.etag,
																					filename = RequestCache#request.filename,
																					latency = ems_util:get_milliseconds() - T1}};
												false ->
													{ok, request, Request2#request{result_cache = true,
																					code = RequestCache#request.code,
																					reason = RequestCache#request.reason,
																					content_type_out = RequestCache#request.content_type_out,
																					response_data = RequestCache#request.response_data,
																					response_header = RequestCache#request.response_header,
																					result_cache_rid = RequestCache#request.rid,
																					etag = RequestCache#request.etag,
																					filename = RequestCache#request.filename,
																					latency = ems_util:get_milliseconds() - T1}}
											end;
										false ->
											ems_cache:add(ets_result_cache_get, ResultCache, ReqHash, {T1, Request2, ResultCache, req_wait_result, []}),
											dispatch_service_work(Request2, Service, ShowDebugResponseHeaders)
									end;
								false -> dispatch_service_work(Request2, Service, ShowDebugResponseHeaders)
							end;
						_ ->
							dispatch_service_work(Request2, Service, ShowDebugResponseHeaders)
					end;
				{error, Reason, ReasonDetail} -> 
					case Type of
						<<"OPTIONS">> -> 
								{ok, request, Request#request{code = 200, 
															  content_type_out = ?CONTENT_TYPE_JSON,
															  response_data = ems_catalog:get_metadata_json(Service),
															  latency = ems_util:get_milliseconds() - T1}
								};
						"HEAD" -> 
								{ok, request, Request#request{code = 200, 
															  latency = ems_util:get_milliseconds() - T1}
								};
						 _ -> 
							% Para finalidades de debug, tenta buscar o user pelo login para armazenar no log
							case ems_util:get_user_request_by_login(Request) of
								{ok, UserFound} -> User = UserFound;
								_ -> User = undefined
							end,
							ems_db:inc_counter(ServiceAuthDeniedMetricName),								
							Request2 = Request#request{code = 400, 
													   content_type_out = ?CONTENT_TYPE_JSON,
													   reason = Reason, 
													   reason_detail = ReasonDetail,
													   response_data = ems_schema:to_json({error, Reason}), 
													   user = User,
													  latency = ems_util:get_milliseconds() - T1},
							ems_user:add_history(case User of 
													undefined -> #user{};
													_ -> User
												 end,
												 #client{}, Service, Request2),
							{error, request, Request2}
					end
			end;
		false -> 
			% Para finalidades de debug, tenta buscar o user pelo login para armazenar no log
			case ems_util:get_user_request_by_login(Request) of
				{ok, UserFound} -> User = UserFound;
				_ -> User = undefined
			end,
			ems_db:inc_counter(ServiceHostDeniedMetricName),								
			Request2 = Request#request{code = 400, 
									   content_type_out = ?CONTENT_TYPE_JSON,
									   reason = access_denied, 
									   reason_detail = host_denied,
									   response_data = ?HOST_DENIED_JSON, 
									   user = User,
									   latency = ems_util:get_milliseconds() - T1},
			ems_user:add_history(case User of 
									undefined -> #user{};
									_ -> User
								 end,
								 #client{}, Service, Request2),
			{error, request, Request2}
	end.
	


dispatch_service_work(Request = #request{type = Type,
										  url = Url,
										  ip_bin = IpBin},
					  #service{host = '',
							    module_name = ModuleName,
							    module = Module,
							    function = Function},
 					  ShowDebugResponseHeaders) ->
	ems_logger:info(iolist_to_binary([<<"ems_dispatcher ">>, Type, <<" ">>, Url, <<" to ">>, list_to_binary(ModuleName), <<" from ">>, IpBin])),
	%% Retornos possíveis:
	%%
	%% Com processamento de middleware function e result cache
	%% {ok, #request{}}
	%% {error, #request{}}
	%%
	%% Sem processamento de middleware function e result cache
	%% {ok, request, #request{}}
	%% {error, request, #request{}}
	%% {error, atom()}
	case apply(Module, Function, [Request]) of
		{Reason, Request2} ->
			Request3 = Request2#request{reason = case Request2#request.reason of
														undefined -> Reason;
														Reason2 -> Reason2
												   end},
			dispatch_middleware_function(Request3, ShowDebugResponseHeaders);
		Request2 -> Request2
	end;
dispatch_service_work(Request = #request{rid = Rid,
										  type = Type,
										  url = Url,
										  payload = Payload,
										  client = Client,
										  user = User,
										  scope = Scope,
										  content_type_out = ContentType,  
										  params_url = ParamsMap,
										  querystring_map = QuerystringMap},
					  Service = #service{
										 module_name = ModuleName,
										 function_name = FunctionName,
										 metadata = Metadata,
										 timeout = Timeout},
					  ShowDebugResponseHeaders) ->
	case erlang:is_tuple(Client) of
		false -> 
			ClientJson = <<"{id:0, codigo:0, name:\"public\", active:true}">>;
		_ -> 
			ClientJson = ems_client:to_json(Client)
	end,
	case erlang:is_tuple(User) of
		false -> 
			UserJson = <<"{id:0, codigo:0, name:\"public\", login:null, email:null, type:null, subtype:null, cpf:null, active:true, lista_perfil:{}, lista_permission:{}}">>;
		_ -> 
			case erlang:is_tuple(Client) of
				true -> UserJson = ems_user:to_resource_owner(User, Client#client.id);
				false -> UserJson = ems_user:to_resource_owner(User)
			end
	end,
	T2 = ems_util:get_milliseconds(),
	Msg = {{Rid, Url, binary_to_list(Type), ParamsMap, QuerystringMap, Payload, ContentType, ModuleName, FunctionName, 
			ClientJson, UserJson, Metadata, Scope, T2, Timeout}, self()},
	dispatch_service_work_send(Request, Service, ShowDebugResponseHeaders, Msg, 1).


dispatch_service_work_send(_, #service{service_unavailable_metric_name = ServiceUnavailableMetricName}, _, _, 0) -> 
	ems_db:inc_counter(ServiceUnavailableMetricName),
	{error, eunavailable_service};
dispatch_service_work_send(Request = #request{t1 = T1,
											  type = Type},
						   Service = #service{host = Host,
							 				  host_name = HostName,
											  module_name = ModuleName,
											  module = Module,
											  timeout = TimeoutService,
											  service_unavailable_metric_name = ServiceUnavailableMetricName,
											  service_resend_msg1 = ServiceResendMsg},
						   ShowDebugResponseHeaders,
						   Msg,
						   Count) ->
	case get_work_node(Host, Host, HostName, ModuleName) of
		{ok, Node} ->
			{Module, Node} ! Msg,
			ems_logger:info("ems_dispatcher send msg to ~p with timeout ~pms.", [{Module, Node}, TimeoutService]),
			case Type of 
				<<"GET">> -> TimeoutConfirmation = 3500;
				_ -> TimeoutConfirmation = 35000
			end,
			receive 
				ok -> 
					case dispatch_service_work_receive(Request, Service, Node, TimeoutService, 0, ShowDebugResponseHeaders) of
						{error, etimeoutservice} ->
							{error, request, Request#request{code = 503,
															 reason = etimeout_service,
															 content_type_out = ?CONTENT_TYPE_JSON,
															 response_data = ?ETIMEOUT_SERVICE,
															 latency = ems_util:get_milliseconds() - T1}};
						Result -> Result
					end
				after TimeoutConfirmation -> 
					ems_logger:info("ems_dispatcher dispatch_service_work_send timeout confirmation ~p.", [{Module, Node}]),
					ems_db:inc_counter(ServiceResendMsg),
					dispatch_service_work_send(Request, Service, ShowDebugResponseHeaders, Msg, Count-1)
			end;
		Error ->  
			ems_db:inc_counter(ServiceUnavailableMetricName),
			Error
	end.
		
dispatch_service_work_receive(Request = #request{rid = Rid},
							  Service = #service{module = Module,
												 service_timeout_metric_name = ServiceTimeoutMetricName,
												 timeout_alert_threshold = TimeoutAlertThreshold},
							  Node,
							  Timeout, TimeoutWaited, ShowDebugResponseHeaders) ->
	case TimeoutAlertThreshold of
		0 -> TimeoutWait = Timeout;
		_ -> TimeoutWait = TimeoutAlertThreshold
	end,
	receive 
		{Code, RidRemote, {Reason, ResponseDataReceived}} when RidRemote == Rid  -> 
			case Reason == ok andalso byte_size(ResponseDataReceived) >= 27 of
				true ->
					case ResponseDataReceived of
						% Os dados recebidos do Java pode ser um array de bytes que possui um "header especial" que precisa ser removido do verdadeiro conteúdo
						<<HeaderJavaSerializable:25/binary, _H2:2/binary, DataBin/binary>> -> 
							case HeaderJavaSerializable =:= <<172,237,0,5,117,114,0,2,91,66,172,243,23,248,6,8,84,224,2,0,0,120,112,0,0>> of
								true -> ResponseData = DataBin;
								false -> ResponseData = ResponseDataReceived
							end;
						_ -> ResponseData = ResponseDataReceived
					end;
				false -> ResponseData = ResponseDataReceived
			end,
			Request2 = Request#request{code = Code,
									   reason = Reason,
									   response_data = ResponseData},
			dispatch_middleware_function(Request2, ShowDebugResponseHeaders);
		_UnknowMessage -> 
			dispatch_service_work_receive(Request, Service, Node, Timeout, TimeoutWaited, ShowDebugResponseHeaders)
		after TimeoutWait ->
			TimeoutWaited2 = TimeoutWaited + TimeoutWait,
			Timeout2 = Timeout - TimeoutWait,
			case Timeout2 =< 0 of
				true ->
					case TimeoutAlertThreshold > 0 of
						true -> ems_logger:warn("ems_dispatcher etimeout_service while waiting ~pms for ~p.", [Timeout, {Module, Node}]);
						false -> ok
					end,
					ems_db:inc_counter(ServiceTimeoutMetricName),
					{error, etimeoutservice};
				false when TimeoutAlertThreshold > 0 ->
					ems_logger:warn("ems_dispatcher is waiting ~p for more than ~pms.", [{Module, Node}, TimeoutWaited2]),
					dispatch_service_work_receive(Request, Service, Node, Timeout2, TimeoutWaited2, ShowDebugResponseHeaders);
				false -> 
					dispatch_service_work_receive(Request, Service, Node, Timeout2, TimeoutWaited2, ShowDebugResponseHeaders)
			end
	end.

-ifdef(win32_plataform).
get_work_node('', _, _, _) -> {ok, node()};
get_work_node([], _, _, _) -> {error, eunavailable_service};
get_work_node(_, _, _, _) -> 
	{ok, Hostname} = inet:gethostname(),
	Node = erlang:list_to_atom("node01@" ++ Hostname), 
	{ok, Node}.
-else.
get_work_node('', _, _, _) -> {ok, node()};
get_work_node([], _, _, _) -> {error, eunavailable_service};
get_work_node([_|T], HostList, HostNames, ModuleName) -> 
	QtdHosts = length(HostList),
	case QtdHosts == 1 of
		true -> Node = hd(HostList);
		false ->
			% ========= faz round robin ===========
			%% Localiza a entrada do módulo na tabela hash
			case ets:lookup(ctrl_node_dispatch, ModuleName) of
				[] -> 
					% não encontrou, vamos selecionar o primeiro host mas o próximo será o segundo
					Index = 2,
					Node = hd(HostList);
				[{_, Idx}] -> 
					% Se o idx não existe pega o primeiro e o próximo será o segundo
					case Idx > QtdHosts of
						true -> 
							Index = 2,
							Node = hd(HostList);
						false -> 
							Node = lists:nth(Idx, HostList),
							Index = Idx + 1
					end
			end,
			% Inserimos na tabela hash os dados de controle
			ets:insert(ctrl_node_dispatch, {ModuleName, Index})
	end,

	
	% Este node está vivo? Temos que rotear para um node existente
	Ping = net_adm:ping(Node),
	case Ping of
		pong -> {ok, Node};
		pang -> get_work_node(T, HostList, HostNames, ModuleName)
	end.
-endif.	


-spec dispatch_middleware_function(#request{}, boolean()) -> {ok, request, #request{}} | {error, request, #request{}}.
dispatch_middleware_function(Request = #request{reason = ok,
												req_hash = ReqHash,
												t1 = T1,
												type = Type,
												content_length = ContentLength,
												service = #service{middleware = Middleware,
												 				   result_cache = ResultCache,
																   service_error_metric_name = ServiceErrorMetricName}},
							 ShowDebugResponseHeaders) ->
	try
		case Middleware of 
			undefined -> Result = {ok, Request};
			_ ->
				case code:ensure_loaded(Middleware) of
					{module, _} ->
						Result = case erlang:function_exported(Middleware, onrequest, 1) of
									true -> apply(Middleware, onrequest, [Request]);
									false -> {ok, Request}
								 end;
					_ ->  Result = {error, einvalid_middleware}
				end
		end,
		T3 = ems_util:get_milliseconds(),
		case Result of
			{ok, Request2 = #request{response_header = ResponseHeader}} ->
				case Type =:= <<"GET">> of
					true -> 
						case ResultCache > 0 andalso ContentLength < ?RESULT_CACHE_MAX_SIZE_ENTRY of
							true ->
								case ShowDebugResponseHeaders of
									false ->
										Request3 = Request2#request{latency = ems_util:get_milliseconds() - T1,
																	status = req_done};
									true ->
										Request3 = Request2#request{response_header = ResponseHeader#{<<"ems-result-cache">> => integer_to_binary(ResultCache)},
																	latency = T3 - T1,
																	status = req_done}
								end,
								notity_workers_waiting_result_cache(ReqHash, Request3),
								{ok, request, Request3};
							false -> 
								case ShowDebugResponseHeaders of
									false ->
										{ok, request, Request2#request{latency = T3 - T1,
																		status = req_done}};
									true ->
										{ok, request, Request2#request{response_header = ResponseHeader#{<<"ems-result-cache">> => <<"0"/utf8>>},
																		latency = T3 - T1,
																		status = req_done}}
								end
						end;
					false ->
						ets:insert(ems_dispatcher_post_time, {post_time, T3}),
						{ok, request, Request2#request{latency = T3 - T1}}
				end;
			{error, Reason2} = Error ->
				ems_db:inc_counter(ServiceErrorMetricName),	
				{error, request, Request#request{code = 500,
												 reason = Reason2,
												 content_type_out = ?CONTENT_TYPE_JSON,
												 response_data = ems_schema:to_json(Error),
												 latency = T3 - T1}}
		end
	catch 
		_Exception:Error2 -> 
			ems_db:inc_counter(ServiceErrorMetricName),
			{error, request, Request#request{code = 500,
											 reason = Error2,
											 content_type_out = ?CONTENT_TYPE_JSON,
											 response_data = ems_schema:to_json(Error2),
											 latency = ems_util:get_milliseconds() - T1}}
	end;
dispatch_middleware_function(Request = #request{t1 = T1, 
											    service = #service{service_error_metric_name = ServiceErrorMetricName}},
							_ShowDebugResponseHeaders) ->
	ems_db:inc_counter(ServiceErrorMetricName),								
	{error, request, Request#request{content_type_out = ?CONTENT_TYPE_JSON,
									 latency = ems_util:get_milliseconds() - T1}}.

									 	
