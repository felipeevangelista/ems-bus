%%********************************************************************
%% @title Módulo msbus_request
%% @version 1.0.0
%% @doc Contém funções para obter os dados de uma requisição HTTP
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright erlangMS Team
%%********************************************************************

-module(msbus_request).

-export([get_property_request/2, 
		 get_param_url/3,
		 get_querystring/3,
		 encode_request/4]).

-include("../include/msbus_config.hrl").

%% @doc Retorna a URL do request
get_property_request(<<"url">>, Request) ->
	dict:fetch("Url", Request#request.http_headers);

%% @doc Retorna o payload do request
get_property_request(<<"payload">>, Request) ->
	Request#request.payload.

%% @doc Retorna um parâmetro do request
get_param_url(NomeParam, Default, Request) ->
	ParamsUrl = Request#request.params_url,
	NomeParam2 = iolist_to_binary(NomeParam),
	Value = maps:get(NomeParam2, ParamsUrl, Default),
	binary_to_list(Value).

%% @doc Retorna uma querystring do request
get_querystring(QueryName, Default, Request) ->
	HttpHeaders = Request#request.http_headers,
	case dict:find("Query", HttpHeaders) of
		{ok, Query} ->
			io:format("aqui 1  ~p\n", [Query]),
		 maps:get(QueryName, Query, Default);
		error -> 
		io:format("aqui 2\n"),
		Default
	end.

%% @doc Gera um objeto request com os dados da requisição
encode_request(HeaderDict, Payload, Servico, ParamsUrl) ->
	Request = #request{http_headers = HeaderDict,
					   payload = Payload,
					   servico = Servico,
					   params_url = ParamsUrl},
	Request.


