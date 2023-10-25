%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
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
%%% @end
%%% Created :  2 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(balcony_pid).
 
-behaviour(gen_server).  
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("balcony_pid.resource_discovery").
-include("balcony_pid.hrl").

-include("log.api").


%% API

%% Application handling API

-export([
%	 control_loop/2,
%	 on/0,
%	 off/0,
%	 calibrate/1,
	 get_temp/0
%	 pwm/2,
%	 loop_temp/1
	]).

%% Debug test API

-export([
	 is_reachable/0,
	 reachable_status/0
%	 all_nodes/0,
%	 all_providers/0,
%	 where_is/1,
%	 is_wanted_state/0
	]).



%% Debug API
-export([
%	 create_worker/1,
%	 delete_worker/1,
%	 load_provider/1,
%	 start/1,
%	 stop/1,
%	 unload/1
	
	 
	]).


-export([start/0,
	 ping/0]).


-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%% Record and Data
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

%% Table or Data models


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% This function is an user interface to be complementary to automated
%% load and start a provider at this host.
%% In v1.0.0 the deployment will not be persistant   
%% @end
%%--------------------------------------------------------------------
-spec is_reachable() -> IsReachable :: boolean | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

is_reachable()  ->
    gen_server:call(?SERVER,{is_reachable},infinity).

%%--------------------------------------------------------------------
%% @doc
%% This function is an user interface to be complementary to automated
%% load and start a provider at this host.
%% In v1.0.0 the deployment will not be persistant   
%% @end
%%--------------------------------------------------------------------
-spec reachable_status() -> {ok,[{Name :: string(), IsReachable :: boolean}]} | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

reachable_status()  ->
    gen_server:call(?SERVER,{reachable_status},infinity).

%%--------------------------------------------------------------------
%% @doc
%% This function is an user interface to be complementary to automated
%% load and start a provider at this host.
%% In v1.0.0 the deployment will not be persistant   
%% @end
%%--------------------------------------------------------------------
-spec get_temp() -> {ok,Temp :: float()} | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

get_temp()  ->
    gen_server:call(?SERVER,{get_temp},infinity).

%%--------------------------------------------------------------------
%% @doc
%% This function is an user interface to be complementary to automated
%% stop and unload a provider at this host.
%% In v1.0.0 the deployment will not be persistant   
%% @end
%%--------------------------------------------------------------------
-spec stop_unload(DeploymentId :: integer()) -> ok | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

stop_unload(DeploymentId) ->
    gen_server:call(?SERVER,{stop_unload,DeploymentId},infinity).
%--------------------------------------------------------------------
%% @doc
%% reload(DeploymentId) will stop_unload and load and start the provider   
%% @end
%%--------------------------------------------------------------------
-spec reload(DeploymentId :: integer(),ProviderSpec :: string()) -> {ok,NewDeploymentId :: integer()} | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

reload(DeploymentId,ProviderSpec) ->
    gen_server:call(?SERVER,{reload,DeploymentId,ProviderSpec},infinity).

%--------------------------------------------------------------------
%% @doc
%% get_deployments returns list of deployed providers   
%% @end
%%--------------------------------------------------------------------
-spec get_deployments() -> DeploymentList :: term() | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

get_deployments() ->
    gen_server:call(?SERVER,{get_deployments},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Create provider directory and starts the slave node 
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} | 
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

create_provider(Deployment) ->
    gen_server:call(?SERVER,{create_provider,Deployment},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Delete provider directory and stop the slave node
%% @end
%%--------------------------------------------------------------------
-spec delete_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} |
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

delete_provider(Deployment) ->
    gen_server:call(?SERVER,{delete_provider,Deployment},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Load the provider application on to the created slave node 
%% @end
%%--------------------------------------------------------------------
-spec load_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} |
	  {error, Error :: [node_not_started]} |
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

load_provider(Deployment) ->
    gen_server:call(?SERVER,{load_provider,Deployment},infinity).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
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
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->

    %% Announce to resource_discovery
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
      
   
    ?LOG_NOTICE("Server started ",[]),
    
 
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.



handle_call({get_temp}, _From, State) ->
    Reply=rd:call(zigbee_devices,call,[?TempSensor,temp,[]],5000),
    {reply, Reply, State};

handle_call({is_reachable}, _From, State) ->
    HB=rd:call(zigbee_devices,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee_devices,call,[?HeatherDoor,is_reachable,[]],5000),
    Reply=case {HB,HD} of
	      {true,true}->
		  true;
	      _->
		  false
	  end,
    {reply, Reply, State};

handle_call({reachable_status}, _From, State) ->
    HB=rd:call(zigbee_devices,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee_devices,call,[?HeatherDoor,is_reachable,[]],5000),
    Reply=[{HB,?HeatherBalcony},{HD,?HeatherDoor}],
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
