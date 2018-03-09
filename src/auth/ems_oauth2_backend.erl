%%********************************************************************
%% @title ems_oauth2_backend
%% @version 1.0.0
%% @doc Backend of OAuth2 subsystem
%% @author Alyssom Ribeiro <alyssonribeiro@unb.br>
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_oauth2_backend).

-behavior(oauth2_backend).
-behavior(gen_server). 

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

%%% API
-export([start/0, start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).


-export([authenticate_user/2]).
-export([authenticate_client/2]).
-export([authorize_refresh_token/3]).
-export([get_client_identity/2]).
-export([associate_access_code/3]).
-export([associate_refresh_token/3]).
-export([associate_access_token/3]).
-export([resolve_access_code/2]).
-export([resolve_refresh_token/2]).
-export([resolve_access_token/2]).
-export([revoke_access_code/2]).
-export([revoke_access_token/2]).
-export([revoke_refresh_token/2]).
-export([get_redirection_uri/2]).
-export([verify_redirection_uri/3]).
-export([verify_client_scope/3]).
-export([verify_resowner_scope/3]).
-export([verify_scope/3]).
        
-record(a, { client   = undefined    :: undefined | term()
           , resowner = undefined    :: undefined | term()
           , scope                   :: oauth2:scope()
           , ttl      = 0            :: non_neg_integer()
           }).

-record(state, {}). 

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Teste
%%%===================================================================

start() -> 
	ok.

start(_) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> ok.
  


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init([]) ->
    Conf = ems_config:getConfig(),
    application:set_env(oauth2, backend, ems_oauth2_backend),
	application:set_env(oauth2, expiry_time, Conf#config.oauth2_refresh_token),
	NewState = #state{},
    {ok, NewState}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.
    
handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info({expire, Table, Key}, State) ->
	ems_db:delete(Table, Key),
	{noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    


%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================

authenticate_user(undefined, _) -> unauthorized_user;
authenticate_user(User, _) ->
	{ok, {<<>>, User}}.
	

authenticate_client(undefined, _) ->
	{error, unauthorized_client};
authenticate_client(Client, _) ->
	{ok, {[], Client}}.


get_client_identity(undefined, _) ->
	{error, unauthorized_client};
get_client_identity(Client, _) ->
	{ok, {[], Client}}.
        

associate_access_code(AccessCode, Context, _AppContext) ->
    mnesia:dirty_write(auth_oauth2_access_code_table, #auth_oauth2_access_code{id = AccessCode, context = Context}),
	%TimeoutExpire = ExpiryTime - ems_util:seconds_since_epoch(0) + 11000,
    %erlang:send_after(TimeoutExpire, ems_oauth2_backend, {expire, auth_oauth2_access_code_table, AccessCode}),
    {ok, Context}.

associate_refresh_token(RefreshToken, Context, _) ->
    mnesia:dirty_write(auth_oauth2_refresh_token_table, #auth_oauth2_refresh_token{id = RefreshToken, context = Context}),
    {ok, Context}.

associate_access_token(AccessToken, Context, _) ->
    mnesia:dirty_write(auth_oauth2_access_token_table, #auth_oauth2_access_token{id = AccessToken, context = Context}),
    {ok, Context}.

resolve_access_code(AccessCode, _) ->
	case ems_db:get(auth_oauth2_access_code_table, AccessCode) of
        {ok, #auth_oauth2_access_code{context = Context}} -> 	
			{ok, {[], Context}};
        _Error -> {error, invalid_code} 
    end.

resolve_refresh_token(RefreshToken, _AppContext) ->
    case ems_db:get(auth_oauth2_refresh_token_table, RefreshToken) of
       {ok, #auth_oauth2_refresh_token{context = Context}} -> 	{ok, {[], Context}};
        _Error -> {error, invalid_token} 
    end.

resolve_access_token(AccessToken, _) ->
    case ems_db:get(auth_oauth2_access_token_table, AccessToken) of
       {ok, #auth_oauth2_access_token{context = Context}} -> 	{ok, {[], Context}};
        _Error -> {error, invalid_token} 
    end.

revoke_access_code(AccessCode, _AppContext) ->
    case ems_db:get(auth_oauth2_access_code_table, AccessCode) of
		{ok, Record} -> 
			ems_db:delete(Record);
		_ -> ok
	end,
    {ok, []}.

revoke_access_token(AccessToken, _) ->
    case ems_db:get(auth_oauth2_access_token_table, AccessToken) of
		{ok, Record} -> 
			ems_db:delete(Record);
		_ -> ok
	end,
    {ok, []}.

revoke_refresh_token(RefreshToken, _) ->
    case ems_db:get(auth_oauth2_refresh_token_table, RefreshToken) of
		{ok, Record} -> 
			ems_db:delete(Record);
		_ -> ok
	end,
    {ok, []}.

get_redirection_uri(Client, _) ->
    case get_client_identity(Client, [])  of
        {ok, #client{redirect_uri = RedirectUri}} -> {ok, RedirectUri};
        _ -> {error, einvalid_uri} 
    end.


verify_redirection_uri(#client{redirect_uri = RedirUri}, ClientUri, _) ->
    case ClientUri =:= RedirUri of
		true -> 
			{ok, []};
		_Error -> 
			{error, unauthorized_client}
    end.

verify_client_scope(#client{id = ClientID}, Scope, _) ->
	case ems_client:find_by_id(ClientID) of
        {ok, #client{scope = Scope0}} ->     
			case Scope =:= Scope0 of
				true -> 
					{ok, {[], Scope0}};
				_ -> {error, unauthorized_client}
			end;
        _ -> {error, invalid_scope}
    end.
    
verify_resowner_scope(_ResOwner, Scope, _) ->
    {ok, {[], Scope}}.

verify_scope(_RegScope, Scope , _) ->
    {ok, {[], Scope}}.

    
% função criada pois a biblioteca OAuth2 não trata refresh_tokens
authorize_refresh_token(Client, RefreshToken, Scope) ->
	case resolve_refresh_token(RefreshToken, []) of
		{ok, {_, [_, {_, ResourceOwner}, _, _]}} -> 
			case verify_client_scope(Client, Scope, []) of
				{error, _} -> {error, invalid_scope};
				{ok, {Ctx3, _}} ->
					Result = {ok, {Ctx3, #a{client = Client,
								   resowner = ResourceOwner,
								   scope = Scope,
								   ttl = oauth2_config:expiry_time(password_credentials)
					}}},
					Result
			end;
		Error -> Error
	end.


