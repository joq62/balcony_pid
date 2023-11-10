-define(SetPoint,21).

-define(MaxSessionTime,1000*60*60*6). %% In seconds
%-define(MaxSessionTime,60*4). %% In seconds
-define(SampleInterval,60*1000).
%-define(DeltaTime,?SampleInterval).
%-define(PwmWidth,2*60).
-define(PwmWidth,40).  %% in seconds
-define(DeltaTime,?PwmWidth).
-define(Kp,0.1).
-define(Ki,0.002).
-define(Kd,6).
-define(MaxControl,0.5*?PwmWidth).
-define(MinControl,-?MaxControl).

-define(TempSensor,"weather_1").
-define(HeatherBalcony,"switch_inglasade_heather_balcony").
-define(HeatherDoor,"switch_inglasade_heather_door").

-define(MaxTempDiff,6).
-define(BaseOffset,0.5*?PwmWidth).
