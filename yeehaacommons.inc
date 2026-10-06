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
function ExtractIPFromLocation(const ALocation: String): String;
function TryParseBulbInfo(const ARawResponse: String; out ABulbInfo: TBulbInfo): Boolean;

{$else implementation}

function RGBToTColor(ARGB: Longint): TColor;
begin
  Result := BEtoN(RGBToColor(Red(ARGB),Green(ARGB),Blue(ARGB))) shr 8;
end;

function ExtractIPFromLocation(const ALocation: String): String;
var
  LPos: Integer;
begin
  Result := Trim(ALocation);
  // strip scheme (yeelight://)
  LPos := Pos('://', Result);
  if LPos > 0 then
    Delete(Result, 1, LPos + 2);
  // strip port (:55443)
  LPos := Pos(':', Result);
  if LPos > 0 then
    Delete(Result, LPos, Length(Result) - LPos + 1);
end;

function TryParseBulbInfo(const ARawResponse: String; out ABulbInfo: TBulbInfo): Boolean;
var
  LLines: TStringList;
  i, LColonPos, LIntValue: Integer;
  LLine, LKey, LValue: String;
begin
  Result := false;
  ABulbInfo := Default(TBulbInfo);
  LLines := TStringList.Create;
  try
    LLines.Text := ARawResponse;
    for i := 0 to LLines.Count - 1 do begin
      LLine := LLines[i];
      LColonPos := Pos(':', LLine);
      if LColonPos <= 0 then
        Continue;
      LKey := LowerCase(Trim(Copy(LLine, 1, LColonPos - 1)));
      LValue := Trim(Copy(LLine, LColonPos + 1, Length(LLine) - LColonPos));
      case LKey of
        'id': ABulbInfo.ID := LValue;
        'location': begin
          ABulbInfo.IP := ExtractIPFromLocation(LValue);
          // a valid bulb response must at least contain a location
          Result := ABulbInfo.IP <> '';
        end;
        'model': ABulbInfo.Model := LValue;
        'power': ABulbInfo.PoweredOn := LowerCase(LValue) = 'on';
        'bright': begin
          LIntValue := StrToIntDef(LValue, Low(TPercentage));
          if LIntValue < Low(TPercentage) then LIntValue := Low(TPercentage);
          if LIntValue > High(TPercentage) then LIntValue := High(TPercentage);
          ABulbInfo.BrightnessPercentage := LIntValue;
        end;
        'color_mode': begin
          LIntValue := StrToIntDef(LValue, Ord(cmRGB));
          if LIntValue < Ord(cmRGB) then LIntValue := Ord(cmRGB);
          if LIntValue > Ord(cmHSV) then LIntValue := Ord(cmHSV);
          ABulbInfo.ColorMode := TColorMode(LIntValue);
        end;
        'rgb': ABulbInfo.RGB := TRGBRange(StrToIntDef(LValue, 1));
        'name': ABulbInfo.Name := LValue;
        'ct': begin
          LIntValue := StrToIntDef(LValue, Low(TColorTemperature));
          if LIntValue < Low(TColorTemperature) then LIntValue := Low(TColorTemperature);
          if LIntValue > High(TColorTemperature) then LIntValue := High(TColorTemperature);
          ABulbInfo.CT := LIntValue;
        end;
      end;
    end;
  finally
    LLines.Free;
  end;
end;

{$endif implementation}
