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

execute(Request = #request{service = #service{properties = Properties, content_type = ContentTypeOut}}) ->	
	Script = maps:get(<<"script">>, Properties, []), 
	Result = execute_script(Script, []),
	case ContentTypeOut of
		<<"application/json; charset=utf-8">> ->
			{ok, Request#request{code = 200, 
								 response_data = ems_schema:to_json({ok, Result})}
			};
		<<"plain/text">> ->
			{ok, Request#request{code = 200, 
								 response_data = Result}
			}
	end.

   
execute_script([], Output) -> 
	try
		iolist_to_binary(lists:reverse(Output))
	catch
		_:_ -> <<"">>
	end;
execute_script([H|T], Output) ->
	case os:cmd(binary_to_list(H)) of
		"sudo: no tty present and no askpass program specified\n" -> execute_script(T, Output);
		undefined -> execute_script(T, Output); 
		Result -> execute_script(T, [Result | Output])
	end.
	
	
    
