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
control_loop(SetPoint,_PreviousErrorXXX,TotalError)->
 %   NewError=get_error(SetPoint),
    NewError=3,
    PreviousError=3,

    T1=TotalError+NewError,
    NewTotalError=if 
		      T1>?MaxControl->
			  ?MaxControl;
		      T1<?MinControl->
			  ?MinControl;
		      true->
			  T1
		  end,
    P= ?Kp*NewError,
    I= (?Ki*?PwmWidth)*NewTotalError,
    D= (?Kd/?PwmWidth)*NewError,
 
    PidValue=P+I+D+?BaseOffset,
    ActualWidth=trunc(PidValue),
  %  ActualWidth=trunc(PidValue+?BaseOffset),
 
    io:format("NewError ~p~n",[{NewError,?MODULE,?LINE}]),
    io:format("T1 ~p~n",[{T1,?MODULE,?LINE}]),
    io:format("P ~p~n",[{P,?MODULE,?LINE}]),
    io:format("I ~p~n",[{I,?MODULE,?LINE}]),
    io:format("D ~p~n",[{D,?MODULE,?LINE}]),
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
    rpc:cast(node(),balcony_pid,control_loop,[NewError,NewTotalError]).


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
   (PreviousError-NewError)/DeltaTime.
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
