-define(SetPoint,20).

-define(SampleInterval,60*1000).
%-define(DeltaTime,?SampleInterval).
%-define(PwmWidth,2*60).
-define(PwmWidth,10).  %% in seconds
-define(DeltaTime,?PwmWidth).
-define(Kp,0.1*?PwmWidth).
-define(Ki,0.7).
-define(Kd,0.05*?PwmWidth).

-define(TempSensor,"weather_1").
-define(HeatherBalcony,"switch_inglasade_heather_balcony").
-define(HeatherDoor,"switch_inglasade_heather_door").

-define(MaxTempDiff,6).
-define(BaseOffset,0.5*?PwmWidth).
