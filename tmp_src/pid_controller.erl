%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pid_controller).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

-define(SERVER,pid_controller_server).

-export([
	 start_loop/0,
	 control_loop/2,
	 on/0,
	 off/0,
	 calibrate/1,
	 temp/0,
	 pwm/2,
	 loop_temp/1
	 
	]).


-export([
	 ping/0,
	 get_state/0,
	 start/0,
	 stop/0
	]).

 
%% ====================================================================
%% External functions
%% ====================================================================

%% call
start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


temp()-> 
    gen_server:call(?SERVER, {temp},infinity).
on()-> 
    gen_server:call(?SERVER, {on},infinity).
off()-> 
    gen_server:call(?SERVER, {off},infinity).

%% cast(
pwm(Width,Period)-> 
    gen_server:cast(?SERVER, {pwm,Width,Period}).
calibrate(EndTemp)-> 
    gen_server:cast(?SERVER, {calibrate,EndTemp}).
loop_temp(Interval)-> 
    gen_server:cast(?SERVER, {loop_temp,Interval}).

control_loop(PreviousError,Integral)->
    gen_server:cast(?SERVER, {control_loop,PreviousError,Integral}).



ping() ->
    gen_server:call(?SERVER, {ping}).

get_state() ->
    gen_server:call(?SERVER, {get_state}).
%% cast
start_loop()->
    gen_server:cast(?SERVER, {start_loop}).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
