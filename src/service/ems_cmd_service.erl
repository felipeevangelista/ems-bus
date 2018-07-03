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
	try
		case ContentTypeOut of
			<<"application/json; charset=utf-8">> ->
				{ok, Request#request{code = 200, 
									 response_data = ems_schema:to_json({ok, Result})}
				};
			_ -> 
				{ok, Request#request{code = 200, 
									 response_data = Result}
				}
		end
	catch
		_:_ -> 
			ems_logger:error("ems_cmd_service return invalid ~p. Return as text/plain.", [ContentTypeOut]),
			{ok, Request#request{code = 200, 
								 content_type_out = <<"text/plain">>,
								 response_data = Result}
			}
	end.

   
execute_script([], Output) -> 
	try
		iolist_to_binary(lists:reverse(Output))
	catch
		Exception:Reason -> 
			ems_logger:error("ems_cmd_service exception ~p. Reason: ~p.", [Exception, Reason]),
			<<"">>
	end;
execute_script([H|T], Output) ->
	case os:cmd(binary_to_list(H), #{ max_size => 8000000 }) of
		"sudo: no tty present and no askpass program specified\n" -> execute_script(T, Output);
		undefined -> execute_script(T, Output); 
		Result -> execute_script(T, [Result | Output])
	end.
	
	
    
