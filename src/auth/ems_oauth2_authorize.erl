-module(ems_oauth2_authorize).

-export([execute/1]).
-export([code_request/1]).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").



execute(Request = #request{type = Type, 
						   protocol_bin = Protocol, 
						   port = Port, 
						   host = Host, 
						   user_agent = UserAgent, 
						   user_agent_version = UserAgentVersion,
						   service  = #service{oauth2_allow_client_credentials = OAuth2AllowClientCredentials}}) -> 
	try
		case Type of
			<<"GET">> -> GrantType = ems_util:get_querystring(<<"response_type">>, <<>>, Request);
			<<"POST">> -> GrantType = ems_util:get_querystring(<<"grant_type">>, <<>>, Request);
			_ -> GrantType = undefined
		end,
		Result = case GrantType of
				<<"password">> -> 
					ems_db:inc_counter(ems_oauth2_grant_type_password),
					Client = ems_util:get_client_request_by_id_and_secret(Request),
					password_grant(Request, Client);
				<<"client_credentials">> ->
					Client = ems_util:get_client_request_by_id_and_secret(Request),
					case OAuth2AllowClientCredentials of
						true ->
							ems_db:inc_counter(ems_oauth2_grant_type_client_credentials),
							client_credentials_grant(Request, Client);
						false ->
							ems_db:inc_counter(ems_oauth2_client_credentials_denied),
							{error, access_denied}	
					end;
				<<"token">> -> 
					ems_db:inc_counter(ems_oauth2_grant_type_token),
					Client = ems_util:get_client_request_by_id(Request),
					authorization_request(Request, Client);
				<<"code">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_code),
					Client = ems_util:get_client_request_by_id(Request),
					authorization_request(Request, Client);	
				<<"authorization_code">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_authorization_code),
					Client = ems_util:get_client_request_by_id_and_secret(Request),
					access_token_request(Request, Client);
				<<"refresh_token">> ->	
					ems_db:inc_counter(ems_oauth2_grant_type_refresh_token),
					Client = ems_util:get_client_request_by_id(Request),
					refresh_token_request(Request, Client);	
				 _ -> 
					Client = undefined,
					{error, access_denied}
		end, 
		case Result of
			{ok, [ {<<"access_token">>,AccessToken},
				   {<<"expires_in">>, ExpireIn},
				   {<<"resource_owner">>, User},
				   {<<"scope">>, Scope},
				   {<<"refresh_token">>, RefreshToken},
				   {<<"refresh_token_expires_in">>, RefreshTokenExpireIn},
				   {<<"token_type">>, TokenType}
				 ]
			 } ->
					% When it is authorization_code, we will record metrics for singlesignon
					case User =/= undefined of
						true -> 
							UserAgentBin = ems_util:user_agent_atom_to_binary(UserAgent),
							SingleSignonUserMetricName = binary_to_atom(iolist_to_binary([<<"ems_oauth2_singlesignon_user_">>, integer_to_binary(User#user.id)]), utf8),
							SingleSignonUserAgentMetricName = binary_to_atom(iolist_to_binary([<<"ems_oauth2_singlesignon_user_agent_">>, UserAgentBin, <<"_">>, UserAgentVersion]), utf8),
							ems_db:inc_counter(SingleSignonUserMetricName),
							ems_db:inc_counter(SingleSignonUserAgentMetricName);
						false -> ok
					end,
					case Client =/= undefined of
						true ->
							ClientJson = ems_client:to_json(Client),
							ResourceOwner = ems_user:to_resource_owner(User, Client#client.id),
							ClientProp = [<<"\"client\":"/utf8>>, ClientJson, <<","/utf8>>];
						false ->
							ResourceOwner = ems_user:to_resource_owner(User),
							ClientProp = <<"\"client\": \"public\","/utf8>>
					end,
					ResponseData2 = iolist_to_binary([<<"{"/utf8>>,
															ClientProp,
														   <<"\"access_token\":\""/utf8>>, AccessToken, <<"\","/utf8>>,
														   <<"\"expires_in\":"/utf8>>, integer_to_binary(ExpireIn), <<","/utf8>>,
														   <<"\"resource_owner\":"/utf8>>, ResourceOwner, <<","/utf8>>,
														   <<"\"scope\":\""/utf8>>, Scope, <<"\","/utf8>>,
														   <<"\"refresh_token\":\""/utf8>>, case RefreshToken of
																									undefined -> <<>>;
																									_ -> RefreshToken
																							end, <<"\","/utf8>>, 
															<<"\"refresh_token_in\":"/utf8>>, case RefreshTokenExpireIn of 
																									undefined -> <<"0">>; 
																									_ -> integer_to_binary(RefreshTokenExpireIn) 
																							 end, <<","/utf8>>,
														   <<"\"token_type\":\""/utf8>>, TokenType, <<"\""/utf8>>,
													   <<"}"/utf8>>]),
					{ok, Request#request{code = 200, 
										 response_data = ResponseData2,
										 oauth2_grant_type = GrantType,
										 oauth2_access_token = AccessToken,
										 oauth2_refresh_token = RefreshToken,
										 client = Client,
										 user = User,
										 content_type_out = ?CONTENT_TYPE_JSON}
					};		
			{redirect, #client{id = ClientId, redirect_uri = RedirectUri}} ->
					ClientIdBin = integer_to_binary(ClientId),
					ems_db:inc_counter(binary_to_atom(iolist_to_binary([<<"ems_oauth2_singlesignon_client_">>, ClientIdBin]), utf8)),
					LocationPath = iolist_to_binary([Protocol, <<"://"/utf8>>, Host, <<":"/utf8>>, integer_to_binary(Port), 
													 <<"/dados/login/index.html?response_type=code&client_id=">>, ClientIdBin, 
													 <<"&redirect_uri=">>, RedirectUri]),
					{ok, Request#request{code = 302, 
										 oauth2_grant_type = GrantType,
										 client = Client,
										 response_header = #{
																<<"location">> => LocationPath
															}
										}
					};
			_ ->
					{error, Request#request{code = 401, 
											reason = access_denied,
											oauth2_grant_type = GrantType,
											response_data = ?ACCESS_DENIED_JSON}
					}
		end
	catch
		_:_ ->
					{error, Request#request{code = 401, 
											reason = access_denied,
											response_data = ?ACCESS_DENIED_JSON}
					}
	end.

%% Requisita o código de autorização - seções 4.1.1 e 4.1.2 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w
code_request(Request) ->
    try
		Client = ems_util:get_client_request_by_id(Request),
		case Client =/= undefined of
			true ->
				User = ems_util:get_user_request_by_login_and_password(Request),
				case User =/= undefined of
					true ->
						RedirectUri = ems_util:to_lower_and_remove_backslash(ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request)),
						Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),
						Authz = oauth2:authorize_code_request(User, Client, RedirectUri, Scope, []),
						case issue_code(Authz) of
							{ok, Response} ->
								Code = element(2, lists:nth(1, Response)),
								LocationPath = iolist_to_binary([RedirectUri, <<"?code=">>, Code]),
								{ok, Request#request{code = 200, 
													 response_data = <<"{}">>,
													 response_header = #{<<"location">> => LocationPath}}
								};
							_ ->
								{error, Request#request{code = 401, 
														reason = access_denied,
														response_data = ?ACCESS_DENIED_JSON}
								}
						end;
					_ ->
						{error, Request#request{code = 401, 
												reason = access_denied,
												response_data = ?ACCESS_DENIED_JSON}
						}
				end;
			false ->
				{error, Request#request{code = 401, 
										reason = access_denied,
										response_data = ?ACCESS_DENIED_JSON}
				}
		end
	catch
		_:_ ->
			{error, Request#request{code = 401, 
									reason = access_denied,
									response_data = ?ACCESS_DENIED_JSON}
			}
	end.

	
%%%===================================================================
%%% Funções internas
%%%===================================================================


%% Cliente Credencial Grant- seção 4.4.1 do RFC 6749. 
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=client_credentials&client_id=s6BhdRkqt3&secret=qwer
-spec client_credentials_grant(#request{}, #client{}) -> {ok, list()} | {error, access_denied}.
client_credentials_grant(_, undefined) -> {error, access_denied};
client_credentials_grant(Request, Client) ->
	try
		Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),	
		Authz = oauth2:authorize_client_credentials(Client, Scope, []),
		issue_token(Authz)
	catch
		_:_ -> {error, access_denied}
	end.


%% Resource Owner Password Credentials Grant - seção 4.3.1 do RFC 6749.
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=password&username=johndoe&password=A3ddj3w
-spec password_grant(#request{}, #client{}) -> {ok, list()} | {error, access_denied}.
password_grant(Request, Client) -> 
	try
		User = ems_util:get_user_request_by_login_and_password(Request),
		case User =/= undefined of
			true ->
				Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),	
				case Client == undefined of
					true -> Authz = oauth2:authorize_password(User, Scope, []);
					false -> Authz = oauth2:authorize_password(User, Client, Scope, [])
				end,
				issue_token(Authz);
			false -> {error, access_denied}
		end
	catch
		_:_ -> {error, access_denied}
	end.

	
%% Verifica a URI do Cliente e redireciona para a página de autorização - Implicit Grant e Authorization Code Grant
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html
-spec authorization_request(#request{}, #client{}) -> {ok, list()} | {error, access_denied}.
authorization_request(_, undefined) -> {error, access_denied};
authorization_request(Request, Client) ->
    try
		RedirectUri = ems_util:to_lower_and_remove_backslash(ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request)),
		case ems_oauth2_backend:verify_redirection_uri(Client, RedirectUri, []) of
			{ok, _} -> {redirect, Client};
			_ -> {error, access_denied}
		end
	catch
		_:_ -> {error, access_denied}
	end.


%% Requisita o código de autorização - seções 4.1.1 e 4.1.2 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w
-spec refresh_token_request(#request{}, #client{}) -> {ok, list()} | {error, access_denied}.
refresh_token_request(_, undefined) -> {error, access_denied};
refresh_token_request(Request, Client) ->
	try
		case ems_util:get_querystring(<<"refresh_token">>, <<>>, Request) of
			<<>> -> {error, access_denied};
			RefleshToken ->
				Scope = ems_util:get_querystring(<<"scope">>, <<>>, Request),
				Authz = ems_oauth2_backend:authorize_refresh_token(Client, RefleshToken, Scope),
				issue_token(Authz)
		end
	catch
		_:_ -> {error, access_denied}
	end.


%% Requisita o token de acesso com o código de autorização - seções  4.1.3. e  4.1.4 do RFC 6749.
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=authorization_code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w&secret=qwer&code=dxUlCWj2JYxnGp59nthGfXFFtn3hJTqx
-spec access_token_request(#request{}, #client{}) -> {ok, list()} | {error, access_denied}.
access_token_request(_, undefined) -> {error, access_denied};
access_token_request(Request, Client) ->
	try
		case ems_util:get_querystring(<<"code">>, <<>>, Request) of
			<<>> -> {error, access_denied};
			Code -> 
				RedirectUri = ems_util:to_lower_and_remove_backslash(ems_util:get_querystring(<<"redirect_uri">>, <<>>, Request)),
				Authz = oauth2:authorize_code_grant(Client, Code, RedirectUri, []),
				issue_token_and_refresh(Authz)
		end
	catch
		_:_ -> {error, access_denied}
	end.
	

issue_token({ok, {_, Auth}}) ->
	{ok, {_, {response, AccessToken, 
						undefined,
						ExpiresIn,
						User,
						Scope, 
						RefreshToken, 
						RefreshTokenExpiresIn,
						TokenType
             }
		}} = oauth2:issue_token(Auth, []),
	{ok, [{<<"access_token">>, AccessToken},
            {<<"expires_in">>, ExpiresIn},
            {<<"resource_owner">>, User},
            {<<"scope">>, Scope},
            {<<"refresh_token">>, RefreshToken},
            {<<"refresh_token_expires_in">>, RefreshTokenExpiresIn},
            {<<"token_type">>, TokenType}]};
issue_token(_) -> {error, access_denied}.
    

issue_token_and_refresh({ok, {_, Auth}}) ->
	{ok, {_, {response, AccessToken, 
						undefined,
						ExpiresIn,
						User,
						Scope, 
						RefreshToken, 
						RefreshTokenExpiresIn,
						TokenType
             }
		}} = oauth2:issue_token_and_refresh(Auth, []),
	{ok, [{<<"access_token">>, AccessToken},
            {<<"expires_in">>, ExpiresIn},
            {<<"resource_owner">>, User},
            {<<"scope">>, Scope},
            {<<"refresh_token">>, RefreshToken},
            {<<"refresh_token_expires_in">>, RefreshTokenExpiresIn},
            {<<"token_type">>, TokenType}]};
issue_token_and_refresh(_) -> 
	{error, access_denied}.

issue_code({ok, {_, Auth}}) ->
	{ok, {_, Response}} = oauth2:issue_code(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_code(_) -> {error, access_denied}.

