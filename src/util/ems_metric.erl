%% ********************************************************************

%% @doc
%% @title Module ems_metric
%% @version 1.0.0
%% @module ems_metric
%% @author Felipe Evangelista dos Santos <fevansantos@gmail.com>
%% @copyright ErlangMS Team
%% @description Verbosity for a SNMP process. This specifies now much debug info is printed.
%% @params verbosity() = silence | info | log | debug | trace 
%% @end

%% ********************************************************************

-module(ems_metric).

-on_load(create_metrics/0).

%% Server API
-export([metric_verbosity/0, metric_verbosity/1]).
-export([add_reporter/0]).
-export([inc_counter_metric/1,dec_counter_metric/1,counter_metric/2]).
-export([delete_metric/1]).

%% @doc Verbosity for SNMP by Exometer
%% Levels: silence | info | log | debug | trace.
metric_verbosity()-> snmpa:verbosity(master_agent, silence).
metric_verbosity(Type)-> snmpa:verbosity(master_agent, Type).

%% @doc Reporter
%% Add a Reporter for SNMP in Exometer
add_reporter()->
	exometer_report:add_reporter(exometer_report_snmp,[]). 

%% @doc Creating metrics for SNMP by Exometer
%% The types metrics:  exometer, counter, fast_counter, ticker, uniform, histogram, spiral, netlink, and probe.
create_metrics()->	
	add_reporter(),
	exometer:new([ems_logger_write_error], counter, [{snmp, [{value, 20000}]}]),	
	exometer:new([ems_logger_write_info],  counter, [{snmp, [{value, 20000}]}]),
	exometer:new([ems_logger_write_warn],  counter, [{snmp, [{value, 20000}]}]).
	


%% @doc Update metric value by Exometer
inc_counter_metric(Name) -> exometer:update([Name], 1).
dec_counter_metric(Name) ->  exometer:update([Name], -1).
counter_metric(Name, Value) -> exometer:update([Name], Value).

%% @doc Delete a metric	
delete_metric(Name)-> exometer:delete([Name]).