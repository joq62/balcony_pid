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
-include("state.hrl").
-define(ControlC201,control_a@c201).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->

    pong=net_adm:ping(?ControlC201),
    ok=dependent_apps:start(),
   
    ok=setup(),

    io:format("Local ~p~n",[{rd_store:get_local_resource_tuples(),?MODULE,?LINE}]),
    io:format("rd_store all_resources() ~p~n",[{rd_store:get_all_resources(),?MODULE,?LINE}]),
    io:format("get_all_resources() ~p~n",[{rd:get_all_resources(),?MODULE,?LINE}]),

    ok=test_balcony(),
    
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(3000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_balcony()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok=balcony_pid:stop_session(),
    loop(),

    ok.

-define(S,State#state.).

loop()->
    io:format("******************* ~p~n",[{date(),time()}]),  
    
    %% API
    io:format(" is_available ~p~n",[{balcony_pid:is_available(),?MODULE,?LINE}]),
    io:format(" get_temp ~p~n",[{balcony_pid:get_temp(),?MODULE,?LINE}]),
    io:format(" in_session ~p~n",[{balcony_pid:in_session(),?MODULE,?LINE}]),

    State=balcony_pid:pid_info(),
    io:format("time left~p~n",[State#state.max_session_time-State#state.session_elapsed_time]),
    io:format("in_session  ~p~n",[State#state.in_session]),  
    io:format("actual_temp ~p~n",[State#state.actual_temp]),  
    io:format("error ~p~n",[State#state.error]),  
    io:format("total_error ~p~n",[State#state.total_error]),  
    io:format("pid_value  ~p~n",[State#state.pid_value]),  
    io:format("actual_width  ~p~n",[State#state.actual_width]),  
    io:format("P ~p~n",[State#state.p]),  
    io:format("I  ~p~n",[State#state.i]),  
    io:format("D  ~p~n",[State#state.d]),  
  %  io:format("  ~p~n",[State#state.]),  

    io:format("------------------- END -------------------~n"),  
    timer:sleep(10*1000),
    ok=balcony_pid:new_session(),
    loop().
    





%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok=application:start(balcony_pid),
    pong=balcony_pid:ping(),
    ok.
