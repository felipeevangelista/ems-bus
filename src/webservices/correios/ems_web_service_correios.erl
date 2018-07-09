%%********************************************************************
%% @title Module ems_web_service_correios
%% @version 1.0.2
%% @doc It provides information about location.
%% @author Renato Carauta Ribeiro <rcarauta6@gmail.com>
%% @copyright ErlangMS Team
%%********************tn************************************************

-module(ems_web_service_correios).

-include("include/ems_schema.hrl").
-include("include/ems_config.hrl").
-include("correios.hrl").

-export([busca_cep/1]).
  
get_cache_table() ->
%  case lists:member(ets_web_service_correios_cache, ets:all()) of
%	  true -> ok;
%	  false -> ems_cache:new(ets_web_service_correios_cache)
% end,
  ets_web_service_correios_cache.
  
busca_cep(Request = #request{service = #service{properties = Props, timeout = Timeout}}) -> 
	
	io:format("Entrou aqui"),
	
	 
	case ems_util:get_param_url(<<"cep">>, undefined, Request) of
		Cep ->
			Result = ems_cache:get(get_cache_table(), Timeout, Cep, 
									fun() -> 
										io:format("PEsquisar cep >>>>>>>> ~p~n~n",[Cep]),
										Result = correios_client:consultaCEP(#'P0:consultaCEP'{'cep' = binary_to_list(Cep)}, [], []),
										
											case Result of
												   {ok,200,
															_Header, %% Headers of a response
															_Args, %%  []
															Response, %% {'P0:consultaCEPResponse', {'P0:enderecoERP',"Asa Sul","70385060","Brasília",[], [],"SQS 115 Bloco F","0","DF",undefined}}
															_Args2,  %% []
															_Xml} -> %% complete xml returned
															
																{ok, Content} = convert_response(Response),
											 
																{ok, Request#request{code = 200, 
																					 response_data = ems_schema:to_json(Content)
																					}
																   };
												
														_ ->
																{error, invalid_cep}
											 end
									 end
								),
			Result;
			
		_ -> 
			{error, Request#request{code = 400, 
  								    response_data = ems_schema:to_json(<<"{\"error\" : \"einvalid_cep\"}">>)
  								    }
			}
	end.
	
	 
	 	 
%% internal function


convert_response(Response) ->
	case Response of
		{'P0:consultaCEPResponse', Content} ->
		Location = element(2,Content),
		CEP = element(3,Content),
		District = element(4,Content),
		End = element(7,Content),
		DistrictAb = element(9,Content),	
		Result = [{<<"cep">>,CEP},{<<"bairro">>,Location},{<<"localidade">>,District},{<<"emderecp">>,End},{<<"uf">>,DistrictAb}]
				
	end,
	{ok, Result}.
	
	
	
	
	
	
	
	
	


