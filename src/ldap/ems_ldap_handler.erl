%%********************************************************************
%% @title Module ems_ldap_handler
%% @version 1.0.0
%% @doc Process ldap messages
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_ldap_handler).

-behaviour(ranch_protocol).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").
-include("include/LDAP.hrl").

-record(state, {listener_name,
				server_name,
				admin,		 		%% admin ldap
				password_admin,     %% Password of admin ldap
				base_search,
				tcp_allowed_address_t,
				bind_cn_success_metric_name,
				bind_uid_success_metric_name,
				bind_success_metric_name,
				bind_cn_invalid_credential_metric_name,
				bind_uid_invalid_credential_metric_name,
				bind_invalid_credential_metric_name,
				search_invalid_credential_metric_name,
				search_unavailable_metric_name,
				search_success_metric_name,
				host_denied_metric_name,
				error_metric_name,
				request_capabilities_metric_name,
				auth_allow_user_inative_credentials	
			}).   


-export([start_link/4]).
-export([init/4, parse_name/1]).

start_link(Ref, Socket, Transport, Service) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Service]),
	{ok, Pid}.

init(Ref, Socket, Transport, [State]) ->
	ranch:accept_ack(Ref),
	loop(Socket, Transport, State).

loop(Socket, Transport, State = #state{tcp_allowed_address_t = AllowedAddress,
									   host_denied_metric_name = HostDeniedMetricName,
									   error_metric_name = ErrorMetricName}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			case inet:peername(Socket) of
				{ok, {IpTuple, Port}} ->
					IpBin = list_to_binary(inet_parse:ntoa(IpTuple)),
					case ems_util:allow_ip_address(IpTuple, AllowedAddress) of				
						true ->
							case decode_ldap_message(Data) of
								{ok, LdapMessage} ->
									ems_logger:debug2("ems_ldap_handler request: ~p\n.", [LdapMessage]),
									MessageID = LdapMessage#'LDAPMessage'.messageID,
									Result = handle_request(LdapMessage, State, IpBin, Port),
									case Result of
										{ok, unbindRequest} ->
											?DEBUG("ems_ldap_handler unbindRequest and close socket."),
											Transport:close(Socket);
										{ok, Msg} -> 
											?DEBUG("ems_ldap_handler response: ~p.", [Msg]),
											Response = [ encode_response(MessageID, M) || M <- Msg ],
											Transport:send(Socket, Response)
									end;
								{error, Reason} ->
									ems_db:inc_counter(ErrorMetricName),
									ems_logger:error("ems_ldap_handler decode invalid message. Reason: ~p.", [Reason]),
									ResultDone = make_result_done(inappropriateMatching),
									Response = [ encode_response(1, ResultDone) ],
									Transport:send(Socket, Response),
									Transport:close(Socket)
							end;
						false ->
							ems_db:inc_counter(HostDeniedMetricName),
							ems_logger:warn("ems_ldap_handler does not grant access to IP ~p. Reason: IP denied.", [IpBin]),
							ResultDone = make_result_done(insufficientAccessRights),
							Response = [ encode_response(1, ResultDone) ],
							Transport:send(Socket, Response),
							Transport:close(Socket)
					end;
				Error -> 
					ems_db:inc_counter(ErrorMetricName),
					ems_logger:error("ems_ldap_handler peername error. Reason: ~p.", [Error]),
					Transport:close(Socket)
			end,
			loop(Socket, Transport, State);		
		_ ->
			Transport:close(Socket)
	end.



encode_response(MessageID, Msg) ->
	Response = #'LDAPMessage'{messageID = MessageID,
							  protocolOp = Msg,
							  controls = asn1_NOVALUE},
    case 'LDAP':encode('LDAPMessage', Response) of
        {ok, Result} -> Result;
        {error, Reason} -> {error, Reason}
    end.


decode_ldap_message(RequestBin) ->
	case 'LDAP':decode('LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} -> {ok, LdapMsg};
		Error -> Error
    end.

  
handle_request(LDAPMessage = {'LDAPMessage', _,
								{bindRequest, #'BindRequest'{version = _Version, 
															 name = Name, 
															 authentication = {_, Password}}},
							 _}, 
			   State = #state{base_search = _BaseSearchConfig,
							   bind_cn_success_metric_name = BindCnSuccessMetricName,
							   bind_uid_success_metric_name = BindUidSuccessMetricName,
							   bind_cn_invalid_credential_metric_name = BindCnInvalidCredentialMetricName,
							   bind_uid_invalid_credential_metric_name = BindUidInvalidCredentialMetricName},
			  Ip, Port) ->
	NameSize = byte_size(Name),
	case (Name =:= <<>>) orelse (NameSize < 4) orelse (NameSize > 100) orelse (Password =:= <<>>) of
		true ->
			ems_logger:error("ems_ldap_handler handle_request parse invalid message."),
			BindResponse = make_bind_response(invalidCredentials, Name);
		false ->
			case parse_name(Name) of
				{ok, cn, UserLogin, _LdapBaseFilter} ->
					case do_authenticate_admin_with_admin_user(Name, UserLogin, Password, State, Ip, Port) orelse
						  do_authenticate_admin_with_list_users(UserLogin, Password, State, Ip, Port) of
						true -> 
							ems_db:inc_counter(BindCnSuccessMetricName),
							ems_logger:info("ems_ldap_handler bind_cn ~p success.", [Name]),
							BindResponse = make_bind_response(success, Name);
						_-> 
							ems_db:inc_counter(BindCnInvalidCredentialMetricName),
							ems_logger:error("ems_ldap_handler bind_cn ~p invalid credential.", [Name]),
							BindResponse = make_bind_response(invalidCredentials, Name)
					end,
					BindResponse;
				{ok, _, UserLogin, _LdapBaseFilter} when LDAPMessage#'LDAPMessage'.messageID > 1 ->
					case do_authenticate_user(UserLogin, Password, State, Ip, Port) of
						ok -> 
							ems_db:inc_counter(BindUidSuccessMetricName),
							ems_logger:info("ems_ldap_handler bind_uid ~p success.", [Name]),
							BindResponse = make_bind_response(success, Name);
						{error, Reason} ->	
							ems_db:inc_counter(BindUidInvalidCredentialMetricName),
							ems_logger:error("ems_ldap_handler bind_uid ~p invalid credential.", [Name]),
							case Reason of
								access_denied_inative_user -> 
									BindResponse = make_bind_response(insufficientAccessRights, Name);
								access_denied -> 
									BindResponse = make_bind_response(invalidCredentials, Name)
							end
					end,
					BindResponse;
				_ -> 
					ems_logger:error("ems_ldap_handler handle_request parse invalid message."),
					BindResponse = make_bind_response(invalidCredentials, Name)
			end
	end,
	{ok, [BindResponse]};
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}},
													attributes = _Attributes}},
				 _}, State, Ip, Port) ->
	handle_request_search_login(UsuLoginBin, State, Ip, Port);
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {present, ObjectClass},
													attributes = _Attributes}},
				 _}, #state{request_capabilities_metric_name = RequestCapabilitiesMetricName}, _Ip, _Port) ->
	ems_db:inc_counter(RequestCapabilitiesMetricName),	
	ObjectName = make_object_name(ObjectClass),
	ResultEntry = {searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"supportedCapabilities">>, vals = [<<"yes">>]},
														#'PartialAttribute'{type = <<"supportedControl">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedExtension">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedFeatures">>, vals = [<<"no">>]},
														#'PartialAttribute'{type = <<"supportedLdapVersion">>, vals = [<<"3">>]},
														#'PartialAttribute'{type = <<"supportedSASLMechanisms">>, vals = [<<"no">>]}
														]
										}
	},
	ems_logger:info("ems_ldap_handler request supported capabilities."),
	ResultDone = make_result_done(success),
	{ok, [ResultEntry, ResultDone]};
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {'and',
																[{present,<<"objectClass">>},
																	{equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}}
																]},
													attributes = _Attributes}},
				_}, State, Ip, Port) ->
	handle_request_search_login(UsuLoginBin, State, Ip, Port);
handle_request({'LDAPMessage', _, 
					{unbindRequest, _},
				 _}, _State, _Ip, _Port) ->
	{ok, unbindRequest};
handle_request({'LDAPMessage', _, 
					_UnknowMsg,
				 _} = LdapMsg, _State, _Ip, _Port) ->
	ems_logger:warn("ems_ldap_handler received unknow msg ~p\n", [LdapMsg]),
	{ok, unbindRequest}.
	

make_object_name(UsuId) ->
	R1 = [<<"uid="/utf8>>, UsuId, <<",ou=funcdis,ou=Classes,dc=unb,dc=br"/utf8>>],
	R2 = iolist_to_binary(R1),
	R2.

make_bind_response(unavailable, _) ->
	make_bind_response(unavailable, <<"">>, <<"LDAP unavailable!!!">>);

make_bind_response(ResultCode, MatchedDN) ->
	make_bind_response(ResultCode, MatchedDN, <<"">>).

make_bind_response(ResultCode, MatchedDN, DiagnosticMessage) ->
	{bindResponse, #'BindResponse'{resultCode = ResultCode,
												  matchedDN = MatchedDN,
												  diagnosticMessage = DiagnosticMessage,
												  referral = asn1_NOVALUE,
												  serverSaslCreds = asn1_NOVALUE}
	}.

make_result_entry(#user{id = UsuId, 
                        codigo = CodigoPessoa,
					    login = UsuLogin,	
					    name = UsuName, 
					    cpf = UsuCpf, 
					    email = UsuEmail, 
					    password = UsuPasswd, 
					    type = UsuType, 
					    subtype = UsuSubType,
					    type_email = UsuTypeEmail, 
					    ctrl_insert = UsuCtrlInsert, 
						ctrl_update = UsuCtrlUpdate,
					    matricula = Matricula,
						active = Active,
						endereco = Endereco,
						complemento_endereco = ComplementoEndereco,
						bairro = Bairro,
						cidade = Cidade,
						uf = UF,
						rg = RG,
						data_nascimento = DataNascimento,
						sexo = Sexo,
						telefone = Telefone,
						celular = Celular,
						ddd = DDD,
						nome_pai = NomePai,
						nome_mae = NomeMae,
						nacionalidade = Nacionalidade
}, 
				  AdminLdap) ->
	UsuId2 = format_user_field(UsuId),
	ObjectName = make_object_name(UsuLogin),
	CodigoPessoa2 = format_user_field(CodigoPessoa),
	UsuLogin2 = format_user_field(UsuLogin),
	UsuNome2 = format_user_field(UsuName),
	UsuCpf2 = format_user_field(UsuCpf),
	UsuEmail2 = format_user_field(UsuEmail),
	UsuSenha2 = format_user_field(UsuPasswd),
	UsuType2 = format_user_field(UsuType),
	UsuSubType2 = format_user_field(UsuSubType),
	UsuTypeEmail2 = format_user_field(UsuTypeEmail),
	UsuCtrlInsert2 = format_user_field(UsuCtrlInsert),
	UsuCtrlUpdate2 = format_user_field(UsuCtrlUpdate),
	Active2 = format_user_field(Active),
	Endereco2 = format_user_field(Endereco),
	ComplementoEndereco2 = format_user_field(ComplementoEndereco),
	Bairro2 = format_user_field(Bairro),
	Cidade2 = format_user_field(Cidade),
	UF2 = format_user_field(UF),
	RG2 = format_user_field(RG),
	DataNascimento2 = format_user_field(DataNascimento),
	Sexo2 = format_user_field(Sexo),
	Telefone2 = format_user_field(Telefone),
	Celular2 = format_user_field(Celular),
	DDD2 = format_user_field(DDD),
	NomePai2 = format_user_field(NomePai),
	NomeMae2 = format_user_field(NomeMae),
	Nacionalidade2 = format_user_field(Nacionalidade),
	Matricula2 = format_user_field(Matricula),
	Names = binary:split(UsuName, <<" ">>),
	SN = format_user_field(lists:last(Names)),
	GivenName = format_user_field(hd(Names)),

	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [CodigoPessoa2]},
 														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [CodigoPessoa2]},
														#'PartialAttribute'{type = <<"uidNumber">>, vals = [CodigoPessoa2]},
														#'PartialAttribute'{type = <<"usu_id">>, vals = [UsuId2]},
														
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"top">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"person">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"organizationalPerson">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"inetOrgPerson">>]},
														#'PartialAttribute'{type = <<"objectClass">>, vals = [<<"posixAccount">>]},
														
														#'PartialAttribute'{type = <<"gecos">>, vals = [UsuNome2]},
														#'PartialAttribute'{type = <<"cn">>, vals = [UsuNome2]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [GivenName]},
														#'PartialAttribute'{type = <<"sn">>, vals = [SN]},

														#'PartialAttribute'{type = <<"creatorsName">>, vals = [AdminLdap]},
														#'PartialAttribute'{type = <<"o">>, vals = [<<"UnB">>]},
														
														
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"email">>, vals = [UsuEmail2]},
														#'PartialAttribute'{type = <<"login">>, vals = [UsuLogin2]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf2]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha2]},
														
														#'PartialAttribute'{type = <<"distinguishedName">>, vals = [UsuLogin2]},
														
														#'PartialAttribute'{type = <<"active">>, vals = [Active2]},
														#'PartialAttribute'{type = <<"endereco">>, vals = [Endereco2]},
														#'PartialAttribute'{type = <<"complemento_endereco">>, vals = [ComplementoEndereco2]},
														#'PartialAttribute'{type = <<"bairro">>, vals = [Bairro2]},
														#'PartialAttribute'{type = <<"cidade">>, vals = [Cidade2]},
														#'PartialAttribute'{type = <<"uf">>, vals = [UF2]},
														#'PartialAttribute'{type = <<"rg">>, vals = [RG2]},
														#'PartialAttribute'{type = <<"dataNascimento">>, vals = [DataNascimento2]},
														#'PartialAttribute'{type = <<"sexo">>, vals = [Sexo2]},
														#'PartialAttribute'{type = <<"telefone">>, vals = [Telefone2]},
														#'PartialAttribute'{type = <<"celular">>, vals = [Celular2]},
														#'PartialAttribute'{type = <<"ddd">>, vals = [DDD2]},
														#'PartialAttribute'{type = <<"nome_pai">>, vals = [NomePai2]},
														#'PartialAttribute'{type = <<"nome_mae">>, vals = [NomeMae2]},
														#'PartialAttribute'{type = <<"nacionalidade">>, vals = [Nacionalidade2]},
														#'PartialAttribute'{type = <<"matricula">>, vals = [Matricula2]},

														#'PartialAttribute'{type = <<"type">>, vals = [UsuType2]},
														#'PartialAttribute'{type = <<"subtype">>, vals = [UsuSubType2]},
														#'PartialAttribute'{type = <<"type_email">>, vals = [UsuTypeEmail2]},

														#'PartialAttribute'{type = <<"ctrl_insert">>, vals = [UsuCtrlInsert2]},
														#'PartialAttribute'{type = <<"ctrl_update">>, vals = [UsuCtrlUpdate2]}

														]
										}
	}.


make_result_done(ResultCode) ->
	{searchResDone, #'LDAPResult'{resultCode = ResultCode, 
								  matchedDN = <<"">>, 
								  diagnosticMessage = <<"">>,
								  referral = asn1_NOVALUE}
	
	}.
	

-spec handle_request_search_login(binary(), #state{}, binary(), non_neg_integer()) -> {ok, tuple()}.
handle_request_search_login(UserLogin, 
							#state{admin = AdminLdap,
								   search_invalid_credential_metric_name = SearchInvalidCredentialMetricName,
								   search_success_metric_name = SearchSuccessMetricName,
								   auth_allow_user_inative_credentials = AuthAllowUserInativeCredentials}, 
								   Ip, Port) ->	
	case ems_user:find_by_login(UserLogin) of
		{error, enoent} ->
			ems_db:inc_counter(SearchInvalidCredentialMetricName),
			ems_logger:error("ems_ldap_handler search ~p does not exist.", [UserLogin]),
			ems_user:add_history(#user{login = UserLogin}, 
								 #service{}, 
								 #request{timestamp = ems_util:timestamp_binary(),
										   code = 49,
										   reason = access_denied,
										   host = Ip,
										   protocol = ldap,
										   port = Port}),
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{ok, User = #user{active = Active}} -> 
				case Active orelse AuthAllowUserInativeCredentials of
					true -> 
						ems_db:inc_counter(SearchSuccessMetricName),
						ems_logger:info("ems_ldap_handler search ~p ~p success.", [UserLogin, User#user.name]),
						ResultEntry = make_result_entry(User, AdminLdap),
						ResultDone = make_result_done(success),
						ems_user:add_history(User, 
											 #service{}, 
											 #request{timestamp = ems_util:timestamp_binary(),
													   code = 0,
													   reason = success,
													   host = Ip,
													   protocol = ldap,
													   port = Port}),
						{ok, [ResultEntry, ResultDone]};
					false -> 
						ems_logger:error("ems_ldap_handler search ~p does not exist.", [UserLogin]),
						ems_user:add_history(User, 
											 #service{}, 
											 #request{timestamp = ems_util:timestamp_binary(),
													   code = 50,
													   reason = access_denied_inative_user,
													   host = Ip,
													   protocol = ldap,
													   port = Port}),
						ResultDone = make_result_done(insufficientAccessRights),
						{ok, [ResultDone]}
				end
	end.
	

% Autentica users possibilitando users inativos se autenticarem se o flag AuthAllowUserInativeCredentials for true  
do_authenticate_user(UserLogin, UserPassword, #state{auth_allow_user_inative_credentials = AuthAllowUserInativeCredentials}, Ip, Port) ->
	case ems_user:find_by_login_and_password(UserLogin, UserPassword) of
		{ok, User = #user{active = Active}} -> 
			case Active orelse AuthAllowUserInativeCredentials of
				true -> 
					ems_user:add_history(User, 
										 #service{}, 
										 #request{timestamp = ems_util:timestamp_binary(),
												  code = 0,
												  reason = success,
												  host = Ip,
												  protocol = ldap,
												  port = Port}),
					ok;
				false -> 
					ems_user:add_history(User, 
										 #service{}, 
										 #request{timestamp = ems_util:timestamp_binary(),
												  code = 50,
												  reason = access_denied_inative_user,
												  host = Ip,
												  protocol = ldap,
												  port = Port}),
					{error, access_denied_inative_user}
			end;
		_ -> 
			ems_user:add_history(#user{login = UserLogin},  
								 #service{}, 
								 #request{timestamp = ems_util:timestamp_binary(),
										  code = 50,
										  reason = access_denied,
										  host = Ip,
										  protocol = ldap,
										  port = Port}),
			{error, access_denied}
	end.

% Autentica o admin a partir da base de usuários de users com flag admin = true	
do_authenticate_admin_with_list_users(UserLogin, UserPassword, #state{auth_allow_user_inative_credentials = AuthAllowUserInativeCredentials}, Ip, Port) ->
	case ems_user:find_by_login_and_password(UserLogin, UserPassword) of
		{ok, User = #user{active = Active, admin = true}} -> 
			case Active orelse AuthAllowUserInativeCredentials of
				true -> 
					ems_user:add_history(User, 
										 #service{}, 
										 #request{timestamp = ems_util:timestamp_binary(),
												  code = 0,
												  reason = success,
												  host = Ip,
												  protocol = ldap,
												  port = Port}),
					true;
				false -> 
					ems_user:add_history(User, 
										 #service{}, 
										 #request{timestamp = ems_util:timestamp_binary(),
												   code = 50,
												   reason = access_denied_inative_admin,
												   host = Ip,
												   protocol = ldap,
												   port = Port}),
					false
			end;
		_ -> 
			ems_user:add_history(#user{login = UserLogin},  
								 #service{}, 
								 #request{timestamp = ems_util:timestamp_binary(),
										   code = 50,
										   reason = access_denied_admin,
										   host = Ip,
										   protocol = ldap,
										   port = Port}),
			false
	end.

% Autentica o admin com o admin fornecido na configuração do processo ldap
do_authenticate_admin_with_admin_user(Name, LdapUser, Password, #state{admin = AdminLdap, password_admin = PasswordAdminLdap}, Ip, Port) ->
	case (Name =:= AdminLdap orelse LdapUser =:= AdminLdap) andalso 
		 (Password =:= PasswordAdminLdap orelse ems_util:criptografia_sha1(Password) =:= PasswordAdminLdap) of
		true -> 
			ems_user:add_history(#user{login = LdapUser},  
								 #service{}, 
								 #request{timestamp = ems_util:timestamp_binary(),
										   code = 0,
										   reason = success,
										   host = Ip,
										   protocol = ldap,
										   port = Port}),
			true;
		false -> 
			ems_user:add_history(#user{login = LdapUser},  
								 #service{}, 
								 #request{timestamp = ems_util:timestamp_binary(),
										   code = 0,
										   reason = access_denied_admin,
										   host = Ip,
										   protocol = ldap,
										   port = Port}),
			false
	end.
						 

format_user_field(undefined) -> <<"">>;
format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Value) when is_integer(Value) -> integer_to_binary(Value);
format_user_field(Value) when is_boolean(Value) -> ems_util:boolean_to_binary(Value);
format_user_field(Value) when is_list(Value) -> list_to_binary(Value);
format_user_field(Value) when is_binary(Value) -> Value.
	

parse_name(undefined) -> {error, einvalid_name};	
parse_name(<<>>) -> {error, einvalid_name};	
parse_name(Name) -> 	
	case binary:split(Name, <<",">>) of
		[UserFilterValue, BaseFilterValue] ->
			case UserFilterValue of
				<<"cn=", Value/binary>> -> {ok, cn, Value, BaseFilterValue};
				<<"uid=", Value/binary>> -> {ok, uid, Value, BaseFilterValue};
				_ -> {error, einvalid_name}
			end;
		[UserFilterValue] ->
			case UserFilterValue of
				<<"cn=", Value/binary>> -> {ok, cn, Value, <<>>};
				<<"uid=", Value/binary>> -> {ok, uid, Value, <<>>};
				Value -> {ok, other, Value, <<>>}
			end;
		_ -> {error, einvalid_name}
	end.
	


	
