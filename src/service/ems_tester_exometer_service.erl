%%********************************************************************
%% @title Module ems_tester_exometer_service
%% @version 1.0.0
%% @doc ems_tester_exometer_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com> & Felipe Evangelista dos Santos <fevansantos@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_tester_exometer_service).

-export([tester/1, update/1]).

tester(Request) -> err = exometer:new([c],counter), update(Request).

  
  
update(Request)-> ok = exometer:update([c],100000).