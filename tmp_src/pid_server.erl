%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pid_server). 
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
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
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-define(SetPoint,30).
-define(Kp,7).
-define(Ki,0.01).
-define(Kd,2).
-define(SampleInterval,15*1000).
-define(PwmWidth,60*1000).

-define(TempSensor,"sensor_temp_inglasade").
-define(HeatherBalcony,"switch_inglasade_heather_balcony").
-define(HeatherDoor,"switch_inglasade_heather_door").

-define(MaxTempDiff,6).
-define(BaseOffset,20*1000).

% Kp - proportional gain
% Ki - integral gain
% Kd - derivative gain
% sample_interval - loop interval time
% previous_error := 0
% integral := 0
% loop:
%   error := setpoint − measured_value
%   proportional := error;
%   integral := integral + error × dt
%   derivative := (error − previous_error) / dt
%   output := Kp × proportional + Ki × integral + Kd × derivative
%   previous_error := error
%   wait(dt)
%   goto loop
%   pwm_interval = 50 seconds 
%   dt= 20 sec						%
%   0 <= pwm_value <= pwm_interval  
-record(state,{
	       pwm_width,
	       sample_interval,
	       setpoint,
	       previous_error,
	       integral,
	       kp,
	       ki,
	       kd
	       
	       
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

temp()-> 
    gen_server:call(?MODULE, {temp},infinity).
%% cast(
pwm(Width,Period)-> 
    gen_server:cast(?MODULE, {pwm,Width,Period}).
calibrate(EndTemp)-> 
    gen_server:cast(?MODULE, {calibrate,EndTemp}).
loop_temp(Interval)-> 
    gen_server:cast(?MODULE, {loop_temp,Interval}).


control_loop(PreviousError,Integral)->
    gen_server:cast(?MODULE, {control_loop,PreviousError,Integral}).
on()-> 
    gen_server:cast(?MODULE, {on}).
off()-> 
    gen_server:cast(?MODULE, {off}).
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Server started"]]),
    {ok, #state{ 
	    pwm_width=?PwmWidth,
	    setpoint=?SetPoint,
	    kp=?Kp,
	    ki=?Ki,
	    kd=?Kd}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({temp},_From, State) ->
    Reply=zigbee_devices:call(?TempSensor,temp,[]),
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({on}, State) ->
    zigbee_devices:call(?HeatherBalcony,set,["on"]),
    zigbee_devices:call(?HeatherDoor,set,["on"]),
    {noreply, State};
handle_cast({off}, State) ->
    zigbee_devices:call(?HeatherBalcony,set,["off"]),
    zigbee_devices:call(?HeatherDoor,set,["off"]),
    {noreply, State};

handle_cast({control_loop,PreviousError,Integral}, State) ->
    SetPoint=State#state.setpoint,
    PwmWidth=State#state.pwm_width,
    Kp=State#state.kp,
    Kd=State#state.kd,
    Ki=State#state.ki,
    spawn(fun()->do_control_loop(SetPoint,PwmWidth,PreviousError,
				 Integral,Kp,Kd,Ki) end),
    
    {ok,Temp}=zigbee_devices:call(?TempSensor,temp,[]),
    io:format("Temp  ~p~n",[{Temp,?MODULE,?FUNCTION_NAME,?LINE}]),
   
    {noreply, State};

handle_cast({pwm,Width,Period}, State) ->
    spawn(fun()->do_pwm(Width,Period) end),
    {noreply, State};

handle_cast({calibrate,EndTemp}, State) ->
    spawn(fun()->do_calibrate(EndTemp) end),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(timeout, State) -> 
    io:format("timeout ~p~n",[{?MODULE,?LINE}]), 
    
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_control_loop(SetPoint,PwmWidth,PreviousError,Integral,Kp,Kd,Ki)->
    {ok,MeasuredValue}=zigbee_devices:call(?TempSensor,temp,[]),
    Error=SetPoint-list_to_float(MeasuredValue),

    if
	Error > ?MaxTempDiff->
	    NewIntegral=Integral*1000,
	    NewDerivative=0;
	Error < -1*?MaxTempDiff->
	    NewIntegral=Integral*1000,
	    NewDerivative=0;
	true->
	    Dt=PwmWidth,
	    NewIntegral=(Integral + Error*Dt),	  
	    NewDerivative=-1*trunc((Error-PreviousError)/Dt)
    end,
    Proportional=Error*1000,
    PidValue=(Kp*Proportional + Ki*NewIntegral + Kd*NewDerivative),
 %   io:format("PidValue, BaseOffset  ~p~n",[{PidValue,?BaseOffset}]),
    ActualWidth=trunc(PidValue+?BaseOffset),
 
 %   io:format("Proportional ~p~n",[Proportional]),
 %   io:format("NewIntegral ~p~n",[NewIntegral]),
 %   io:format("NewDerivative ~p~n",[NewDerivative]),
    io:format("Kp*Proportional ~p~n",[Kp*Proportional]),
    io:format("Ki*NewIntegral ~p~n",[Ki*NewIntegral]),
    io:format("Kd*NewDerivative ~p~n",[Kd*NewDerivative]),
    io:format("BaseOffset ~p~n",[?BaseOffset]),
    io:format("Error ~p~n",[Error]),
    io:format("ActualWidth ~p~n",[ActualWidth]),
    if
	ActualWidth>PwmWidth->
	    zigbee_devices:call(?HeatherBalcony,set,["on"]),
	    zigbee_devices:call(?HeatherDoor,set,["on"]),
	    timer:sleep(PwmWidth);
	ActualWidth<1000->
	    zigbee_devices:call(?HeatherBalcony,set,["off"]),
	    zigbee_devices:call(?HeatherDoor,set,["off"]),
	    timer:sleep(PwmWidth);
	true ->
	    zigbee_devices:call(?HeatherBalcony,set,["on"]),
	    zigbee_devices:call(?HeatherDoor,set,["on"]),
	    timer:sleep(ActualWidth),
	    zigbee_devices:call(?HeatherBalcony,set,["off"]),
	    zigbee_devices:call(?HeatherDoor,set,["off"]),
	    timer:sleep(PwmWidth-ActualWidth)
    end,
    NewPreviousError=Error,
    rpc:cast(node(),?MODULE,control_loop,[NewPreviousError,NewIntegral]).
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
do_calibrate(EndTemp)->
    StartTime=os:system_time(second),
    zigbee_devices:call(?HeatherBalcony,set,["on"]),
    zigbee_devices:call(?HeatherDoor,set,["on"]),
    loop_calibrate(StartTime,EndTemp).

loop_calibrate(StartTime,EndTemp)->
    {ok,Temp}=zigbee_devices:call(?TempSensor,temp,[]),
    DiffTemp=EndTemp-list_to_float(Temp),
    Time=os:system_time(second)-StartTime,
    if 
	DiffTemp<0->
	    io:format("STOPPED: Time ,DiffTemp  ~p~n",[{Time,DiffTemp,?MODULE,?FUNCTION_NAME,?LINE}]),
	    zigbee_devices:call(?HeatherBalcony,set,["off"]),
	    zigbee_devices:call(?HeatherDoor,set,["off"]),
	    timer:sleep(10*1000),
	    loop_calibrate(StartTime,EndTemp);
	 true->
	    io:format("Time ,DiffTemp  ~p~n",[{Time,DiffTemp,?MODULE,?FUNCTION_NAME,?LINE}]),
	    timer:sleep(15*1000),
	    loop_calibrate(StartTime,EndTemp)
    end.


do_pwm(Width,Period)->
 %   {ok,Temp1}=zigbee_devices_server:get(?TempSensor,temp,[]),
 %   io:format("Temp1  ~p~n",[{Temp1,?MODULE,?FUNCTION_NAME,?LINE}]),

    zigbee_devices:call(?HeatherBalcony,set,["on"]),
    zigbee_devices:call(?HeatherDoor,set,["on"]),
    timer:sleep(Width),
    zigbee_devices:call(?HeatherBalcony,set,["off"]),
    zigbee_devices:call(?HeatherDoor,set,["off"]),
    timer:sleep(Period-Width),
    {ok,Temp2}=zigbee_devices:call(?TempSensor,temp,[]),
    io:format("Temp2  ~p~n",[{Temp2,?MODULE,?FUNCTION_NAME,?LINE}]),
    do_pwm(Width,Period).
1
