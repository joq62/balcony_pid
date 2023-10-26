%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 26 Oct 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_pid).


-include("balcony_pid.hrl").

%% API
-export([
	 control_loop/3,
	 new_proportional_value/2,
	 new_integral_value/4,
	 new_derivate_value/4,
	 
	 get_error/1,
	 get_temp/0,
	 is_reachable/0,
	 reachable_status/0
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
control_loop(SetPoint,_PreviousErrorXXX,Integral)->
 %   NewError=get_error(SetPoint),
    NewError=3,
    PreviousError=3,

    NewProportional= 1*new_proportional_value(?Kp,NewError),
    NewIntegral= 1*new_integral_value(Integral,?Ki,NewError,?DeltaTime),
    NewDerivate= 1*new_derivate_value(?Kd,PreviousError,NewError,?DeltaTime),
 
    PidValue=NewProportional+NewDerivate+NewIntegral+?BaseOffset,
    ActualWidth=trunc(PidValue),
  %  ActualWidth=trunc(PidValue+?BaseOffset),
 
    io:format("NewError ~p~n",[{NewError,?MODULE,?LINE}]),
    io:format("NewProportional ~p~n",[{NewProportional,?MODULE,?LINE}]),
    io:format("NewIntegral ~p~n",[{NewIntegral,?MODULE,?LINE}]),
    io:format("NewDerivate ~p~n",[{NewDerivate,?MODULE,?LINE}]),
    io:format("PidValue ~p~n",[{PidValue,?MODULE,?LINE}]),
    io:format("ActualWidth ~p~n",[{ActualWidth,?MODULE,?LINE}]),
 
    if
	ActualWidth>?PwmWidth->
	    rd:call(zigbee_devices,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee_devices,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(?PwmWidth*1000);
	ActualWidth < 1 ->
	    rd:call(zigbee_devices,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee_devices,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep(?PwmWidth*1000);
	true ->
	   rd:call(zigbee_devices,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee_devices,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(ActualWidth*1000),
	    rd:call(zigbee_devices,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee_devices,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep((?PwmWidth-ActualWidth)*1000)
    end,
    rpc:cast(node(),balcony_pid,control_loop,[NewError,NewIntegral]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
new_proportional_value(Kp,NewError)->
    Kp*NewError.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
new_integral_value(IntegralValue,Ki,NewError,DeltaTime)->
    Ki*((IntegralValue+NewError)).
 %   Ki*((IntegralValue+NewError)*DeltaTime).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
new_derivate_value(Kd,PreviousError,NewError,DeltaTime)->
    Kd*((PreviousError-NewError)).
  %  Kd*((PreviousError-NewError)/DeltaTime).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_error(SetPoint)->
  SetPoint-get_temp().
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_temp()->    
    rd:call(zigbee_devices,call,[?TempSensor,temp,[]],5000).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_reachable()->    
    HB=rd:call(zigbee_devices,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee_devices,call,[?HeatherDoor,is_reachable,[]],5000),
    case {HB,HD} of
	{true,true}->
	    true;
	_->
	    false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
reachable_status()->    
    HB=rd:call(zigbee_devices,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee_devices,call,[?HeatherDoor,is_reachable,[]],5000),
    [{HB,?HeatherBalcony},{HD,?HeatherDoor}].
%%%===================================================================
%%% Internal functions
%%%===================================================================
