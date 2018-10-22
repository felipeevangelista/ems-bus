-module(ems_bus_sup).

-behaviour(supervisor).

-include("../include/ems_schema.hrl").
-include("../include/ems_config.hrl").

%% Supervisor callbacks
-export([start_link/1, init/1]).
-export([start_process/1, start_process_sup/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("sup1\n"),
	KernelServices = ems_catalog_lookup:list_kernel_catalog(),
	io:format("sup2 kernelservices ~p\n", [KernelServices]),
    PoolSpecs = lists:map(
		fun(S = #service{name = WorkerName, 
						  pool_size = PoolSize, 
						  pool_max = PoolMax, 
						  module = Worker}) ->
					WorkerNameAtom = list_to_atom(binary_to_list(WorkerName)),
					case PoolMax == 1 of
						true -> 
							{WorkerNameAtom,
								{?MODULE, start_process, [[Worker, start, S]]},
								permanent, 10000, worker,  [WorkerNameAtom]
							};
						false ->
							{WorkerNameAtom, 
								{?MODULE, start_process_sup, [[poolboy, 
															   start_link, 
															   [[{strategy, fifo},
																 {name, {local, WorkerNameAtom}},
																 {worker_module, Worker},
																 {size, PoolSize},
																 {max_overflow, PoolMax}], S]
															   ]]},
								 permanent, 10000, worker, [poolboy]
							}
					end
		end, KernelServices),
		io:format("sup3\n"),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.
	

start_process([Module, Function, Service = #service{name = WorkerName}]) ->
	io:format("start_process1\n"),
	ems_logger:info("\033[0;33mStart\033[0m ~s.", [binary_to_list(WorkerName)]),
	Reply = apply(Module, Function, [Service]),
	io:format("start_process2\n"),
	Reply.
	
start_process_sup([Module, Function, Args = [[_,
											  {name, {local, WorkerNameAtom}},
											  _, 
											  {size, PoolSize}, 
											  {max_overflow, PoolMax}], 
											_]]) ->
	io:format("start_process_sup1\n"),
	ems_logger:info("\033[0;33mStart\033[0m ~p with ~p workers \033[0;34m(Max ~p workers)\033[0m.", [WorkerNameAtom, PoolSize, PoolMax]),
	Reply = apply(Module, Function, Args),
	io:format("start_process_sup2\n"),
	Reply.

