unit yeehaa.lnet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamIO,
  fpjson,
  jsonparser,
  lNet,
  Graphics;

{$I yeehaacommons.inc}

type

  { TYeeConn }

  TYeeConn = class
  private
    FConnectionError: TConnectionErrorEvent;
    FListenPort: Word;
    FUDPConn: TLUdp;
    FBroadcastThread: TThread;
    FOnBulbFound: TBulbFoundEvent;
    FOnCommandResult: TCommandResultEvent;
    procedure CanSend(aSocket: TLSocket);
    procedure ConnectionError(const msg: string; aSocket: TLSocket);
    procedure BulbFound(aSocket: TLSocket);
    procedure SendCommand(const AIP: String; const AID: Integer; const AMethod: String; AParams: array of const);
  public
    constructor Create(const AListenPort: Word;
      const ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    destructor Destroy; override;
    procedure SetName(const AIP, AName: String);
    procedure SetPower(const AIP: String; const AIsOn: Boolean; const ATransitionEfect: TTransitionEffect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetBrightness(const AIP: String; const ABrightness: TPercentage; const ATransitionEfect: TTransitionEffect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetColorTemperature(const AIP: String; const AColorTemperature: TColorTemperature; const ATransitionEfect: TTransitionEffect; const ATransitionDuration: TTransitionDuration);
    procedure SetRGB(const AIP: String; const ARGB: TRGBRange;
      const ATransitionEfect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration);
    property OnConnectionError: TConnectionErrorEvent write FConnectionError;
    property OnBulbFound: TBulbFoundEvent write FOnBulbFound;
    property OnCommandResult: TCommandResultEvent write FOnCommandResult;
  end;

implementation

uses
  DateUtils;

const
  BroadcastAddress = '239.255.255.250';
  BroadcastPort    = 1982;
  BroadcastMessage = 'M-SEARCH * HTTP/1.1'#13#10
                   + 'MAN: "ssdp:discover"'#13#10
                   + 'ST: wifi_bulb'#13#10
                   ;

  BulbPort         = 55443;

{$define implementation}
{$I yeehaacommons.inc}

type

  { TBroadcastThread }

  TBroadcastThread = class(TThread)
    FConn: TLUdp;
    FBroadcastIntervalMillisecond: Integer;
    constructor Create(AConn: TLUdp; ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    procedure Execute; override;
  end;

{ TBroadcastThread }

constructor TBroadcastThread.Create(AConn: TLUdp; ABroadcastIntervalMillisecond: Integer);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  FConn := AConn;
  FBroadcastIntervalMillisecond := ABroadcastIntervalMillisecond;
  Start;
end;

procedure TBroadcastThread.Execute;
var
  LLastBroadcastTime: TDateTime;
begin
  LLastBroadcastTime := IncMilliSecond(Now,-FBroadcastIntervalMillisecond);
  while not Terminated do begin
    if MilliSecondsBetween(LLastBroadcastTime,Now) >= FBroadcastIntervalMillisecond then begin
      {$ifdef debug}WriteLn('Connect: ', {$endif}FConn.Connect(BroadcastAddress,BroadcastPort){$ifdef debug}){$endif};
      LLastBroadcastTime := Now;
    end;
    FConn.CallAction;
  end;
end;

{ TYeeConn }

procedure TYeeConn.BulbFound(aSocket: TLSocket);
var
  LRawResponse,LResponseLine, LKey, LValue: string;
  LRawResponseText: TextFile;
  LRawResponseStream: TStringStream;
  LBulbInfo: TBulbInfo;
  LColonPos: SizeInt;
begin
  if aSocket.GetMessage(LRawResponse) > 0 then begin
    {$ifdef debug}WriteLn(LRawResponse);{$endif}
    LRawResponseStream := TStringStream.Create(LRawResponse);
    AssignStream(LRawResponseText, LRawResponseStream);
    Reset(LRawResponseText);
    while not EOF(LRawResponseText) do begin
      ReadLn(LRawResponseText,LResponseLine);
      LColonPos := Pos(':',LResponseLine);
      if LColonPos > 0 then begin
        LKey   := Copy(LResponseLine,1,LColonPos - 1);
        LValue := Copy(LResponseLine,LColonPos + 2,Length(LResponseLine) - LColonPos + 2);

        case LKey of
          'id'        : LBulbInfo.ID                   := LValue;
          // strip away protocol and port, only address is required
          'Location'  : LBulbInfo.IP                   := Copy(LValue,12,Length(LValue) - 17);
          'model'     : LBulbInfo.Model                := LValue;
          'power'     : LBulbInfo.PoweredOn            := LValue = 'on';
          'bright'    : LBulbInfo.BrightnessPercentage := StrToIntDef(LValue,1);
          'color_mode': LBulbInfo.ColorMode            := TColorMode(StrToIntDef(LValue,2));
          'rgb'       : LBulbInfo.RGB                  := TRGBRange(StrToIntDef(LValue,1));
          'name'      : LBulbInfo.Name                 := LValue;
          'ct'        : LBulbInfo.CT                   := TColorTemperature(StrToIntDef(LValue,1));
        end;
      end;
    end;
    LRawResponseStream.Free;

    if Assigned(FOnBulbFound) then FOnBulbFound(LBulbInfo);
  end else begin
    WriteLn('Receive called with no message');
  end;
end;

procedure TYeeConn.SendCommand(const AIP: String; const AID: Integer;
  const AMethod: String; AParams: array of const);
var
  LJSONMsg, LJSONResult: TJSONObject;
  LJSONParams: TJSONArray;
  LRawResult: String;
  LJSONMSgStr: TJSONStringType;
  LJSONID: TJSONData;
  LCmdID: Integer;
begin
  with TLTcp.Create(nil) do
    try
      Timeout := 1000;
      Port := FListenPort;
      LJSONMsg := nil;
      LJSONParams := nil;
      LJSONResult := nil;

      if Connect(AIP,BulbPort) then begin
        repeat
          CallAction; // synchronizing the asynchronous
        until Connected;
        LJSONMsg := CreateJSONObject(['id',AID,'method',AMethod]);
        LJSONParams := CreateJSONArray(AParams);
        LJSONMsg['params'] := LJSONParams;
        LJSONMSgStr := LJSONMsg.AsJSON;
        {$ifdef debug}WriteLn('SendMessage (',{$endif}SendMessage(LJSONMSgStr + #13#10){$ifdef debug},'): ' + LJSONMSgStr){$endif};

        if Assigned(FOnCommandResult) then begin
          while GetMessage(LRawResult) <= 0 do CallAction;
          {$ifdef debug}WriteLn('ResultReceived: ' + LRawResult);{$endif}
          LJSONResult := TJSONObject(GetJSON(LRawResult));
          LJSONID := LJSONResult.FindPath('id');
          if Assigned(LJSONID) then LCmdID := LJSONID.AsInteger else LCmdID := -1;
          FOnCommandResult(LCmdID,LJSONResult.FindPath('result'),LJSONResult.FindPath('error'));
        end;
      end;
    finally
      LJSONResult.Free;
      LJSONMsg.Free;
      Disconnect(true);
      Free;
    end;
end;

procedure TYeeConn.ConnectionError(const msg: string; aSocket: TLSocket);
begin
  if Assigned(FConnectionError) then FConnectionError(msg);
end;

procedure TYeeConn.CanSend(aSocket: TLSocket);
begin
  {$ifdef debug}WriteLn('SendMessage: ', {$endif}FUDPConn.SendMessage(BroadcastMessage){$ifdef debug}){$endif};
end;

constructor TYeeConn.Create(const AListenPort: Word; const ABroadcastIntervalMillisecond: Integer);
begin
  FListenPort := AListenPort;

  FUDPConn := TLUdp.Create(nil);
  with FUDPConn do begin
    Timeout            := 1000;
    FUDPConn.Port      := AListenPort;
    FUDPConn.OnCanSend := @CanSend;
    FUDPConn.OnReceive := @BulbFound;
    FUDPConn.OnError   := @ConnectionError;
  end;

  FBroadcastThread := TBroadcastThread.Create(FUDPConn, ABroadcastIntervalMillisecond);
end;

destructor TYeeConn.Destroy;
begin
  FBroadcastThread.Free;

  FUDPConn.Disconnect(true);
  FUDPConn.Free;

  inherited Destroy;
end;

procedure TYeeConn.SetName(const AIP, AName: String);
begin
  SendCommand(AIP,1,'set_name',[AName])
end;

procedure TYeeConn.SetPower(const AIP: String; const AIsOn: Boolean;
  const ATransitionEfect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration;
  const AColorMode: TPowerColorMode);
var
  LPowerStateStr,LTransitionEfectStr: String;
begin
  if AIsOn then
    LPowerStateStr := 'on'
  else
    LPowerStateStr := 'off';
  case ATransitionEfect of
    teSmooth: LTransitionEfectStr := 'smooth';
    teSudden: LTransitionEfectStr := 'sudden';
  end;
  SendCommand(AIP,1,'set_power',[LPowerStateStr,LTransitionEfectStr,ATransitionDuration,Ord(AColorMode)]);
end;

procedure TYeeConn.SetBrightness(const AIP: String;
  const ABrightness: TPercentage; const ATransitionEfect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration;
  const AColorMode: TPowerColorMode);
var
  LTransitionEfectStr: String;
begin
  case ATransitionEfect of
    teSmooth: LTransitionEfectStr := 'smooth';
    teSudden: LTransitionEfectStr := 'sudden';
  end;
  SendCommand(AIP,1,'set_bright',[ABrightness,LTransitionEfectStr,ATransitionDuration]);
end;

procedure TYeeConn.SetColorTemperature(const AIP: String;
  const AColorTemperature: TColorTemperature;
  const ATransitionEfect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration);
var
  LTransitionEfectStr: String;
begin
  case ATransitionEfect of
    teSmooth: LTransitionEfectStr := 'smooth';
    teSudden: LTransitionEfectStr := 'sudden';
  end;
  SendCommand(AIP,1,'set_ct_abx',[AColorTemperature,LTransitionEfectStr,ATransitionDuration]);
end;

procedure TYeeConn.SetRGB(const AIP: String; const ARGB: TRGBRange;
  const ATransitionEfect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration);
var
  LTransitionEfectStr: String;
begin
  case ATransitionEfect of
    teSmooth: LTransitionEfectStr := 'smooth';
    teSudden: LTransitionEfectStr := 'sudden';
  end;
  SendCommand(AIP,1,'set_rgb',[NtoBE(ColorToRGB(ARGB)) shr 8,LTransitionEfectStr,ATransitionDuration]);
end;

end.

