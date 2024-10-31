{$ifndef implementation}

const
  DefaultBroadcastIntervalMilliSeconds = 100;

type

  TPercentage = 1 .. 100;
  TRGBRange = -$7FFFFFFF-1 .. $7FFFFFFF;
  TColorMode = (cmRGB := 1,cmCT,cmHSV);
  TColorTemperature   = 1700 .. 6500;

  TTransitionEffect    = (teSmooth,teSudden);
  TTransitionDuration = 30 .. 10000; // 10 second should be fairly long, I guess
  TPowerColorMode = (pcmDefault,pcmCT,pcmRGB,pcmHSV,pcmColorFlow,pcmNightLight);

  TBulbInfo = record
    ID: String;
    IP: String;
    Model: String;
    Name: String;
    PoweredOn: Boolean;
    BrightnessPercentage: TPercentage;
    TransitionEffect: TTransitionEffect;
    TransitionDuration: TTransitionDuration;
    ColorMode: TColorMode;
    RGB: TRGBRange;
    CT: TColorTemperature;
  end;

  TConnectionErrorEvent = procedure (const AMsg: String) of object;
  TBulbFoundEvent       = procedure (const ANewBulb: TBulbInfo) of object;
  TCommandResultEvent   = procedure (const AID: Integer; AResult,AError: TJSONData) of object;

function RGBToTColor(ARGB: Longint): TColor;

{$else implementation}

function RGBToTColor(ARGB: Longint): TColor;
begin
  Result := BEtoN(RGBToColor(Red(ARGB),Green(ARGB),Blue(ARGB))) shr 8;
end;

{$endif implementation}
