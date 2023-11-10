%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("include/balcony_pid.hrl").

-define(ControlC201,control_a@c201).
-define(TestSetpoint,21).

-define(MainLogDir,"logs").
-define(LocalLogDir,"to_be_changed.logs").
-define(LogFile,"logfile").
-define(MaxNumFiles,10).
-define(MaxNumBytes,100000).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
   
    ok=test_balcony(),
%    ok=test_1(),
%    ok=test_2(), 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
%    init:stop(),
    ok.



%
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_balcony()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok=balcony_pid:new_session(),
    loop(),

    ok.

loop()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    PidInfo=balcony_pid:pid_info(),
    io:format("PidInfo ~p~n",[{PidInfo,?MODULE,?FUNCTION_NAME}]),  
    timer:sleep(10*1000),
  
    loop().
    


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    {error,["Session not started",balcony_pid,_]}=balcony_pid:get_error(),
    ok=balcony_pid:new_session(?TestSetpoint),
    {error,["Session allready started",balcony_pid,_]}=balcony_pid:new_session(?TestSetpoint),
    Temp=balcony_pid:get_temp(),
    Error=balcony_pid:get_error(),
    Error=?TestSetpoint-Temp,
    io:format("Error ~p~n",[{Error,?MODULE,?FUNCTION_NAME}]),
      

    ok.

% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    Temp=balcony_pid:get_temp(),
    io:format("Temp ~p~n",[{Temp,?MODULE,?FUNCTION_NAME}]),
    IsReachable=balcony_pid:is_reachable(),
    io:format("IsReachable ~p~n",[{IsReachable,?MODULE,?FUNCTION_NAME}]),

    ReachableStatus=balcony_pid:reachable_status(),
    io:format("ReachableStatus ~p~n",[{ReachableStatus,?MODULE,?FUNCTION_NAME}]),

%  HeatherBalcony=rd:call(zigbee_devices,call,[?HeatherBalcony,is_reachable,[]],5000),
%    io:format("HeatherBalcony ~p~n",[{HeatherBalcony,?MODULE,?FUNCTION_NAME}]),
%    HeatherDoor=rd:call(zigbee_devices,call,[?HeatherDoor,is_reachable,[]],5000),
%    io:format("HeatherDoor ~p~n",[{HeatherDoor,?MODULE,?FUNCTION_NAME}]),
    
    

    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    pong=net_adm:ping(?ControlC201),
    NodesC201=rpc:call(?ControlC201,erlang,nodes,[],5000),
 %   io:format("NodesC201 ~p~n",[{NodesC201,?MODULE,?FUNCTION_NAME}]),
    [net_adm:ping(N)||N<-NodesC201],
 %   io:format("nodes ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME}]),
    
    file:del_dir_r(?MainLogDir),
    ok=application:start(log),
    pong=log:ping(),
    LocalLogDir=atom_to_list(node())++".logs",
    ok=log:create_logger(?MainLogDir,LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes),
    
    ok=application:start(rd),
    pong=rd:ping(),
  
    %% The applications to test
    ok=application:start(balcony_pid),
    pong=balcony_pid:ping(),
    timer:sleep(300),

   % AllResources=rd:get_all_resources(),
   % glurk=AllResources,
   % io:format("AllResources ~p~n",[{AllResources,?MODULE,?FUNCTION_NAME}]),
    
    ok.
