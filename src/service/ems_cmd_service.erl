%%********************************************************************
%% @title Module ems_cmd_service
%% @version 1.0.0
%% @doc ems_cmd_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_cmd_service).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([execute/1]).

execute(Request = #request{service = #service{properties = Properties}}) ->	
	Script = maps:get(<<"script">>, Properties, []), 
	Result = execute_script(Script, []),
	{ok, Request#request{code = 200, 
						 response_data = Result}
	}.

   
execute_script([], Output) -> {ok, io_lib:iolist_to_binary("~p", [Output])};
execute_script([H|T], Output) ->
	Result = os:cmd(H),
	execute_script(T, [Result | Output]).
	
    
