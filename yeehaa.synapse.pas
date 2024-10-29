unit yeehaa.synapse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamIO,
  fpjson,
  jsonparser,
  blcksock, synsock,
  Graphics;

{$I yeehaacommons.inc}

type

  { TYeeConn }

  TYeeConn = class
  private
    FConnectionError: TConnectionErrorEvent;
    FListenPort: Word;
    FUDPConn: TUDPBlockSocket;
    FBroadcastThread,FBulbFoundThread: TThread;
    FOnBulbFound: TBulbFoundEvent;
    FOnCommandResult: TCommandResultEvent;
    procedure SendCommand(const AIP: String; const AID: Integer; const AMethod: String; AParams: array of const);
  public
    constructor Create(const AListenPort: Word;
      const ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    destructor Destroy; override;
    procedure SetName(const AIP, AName: String);
    procedure SetPower(const AIP: String; const AIsOn: Boolean; const ATransitionEfect: TTransitionEfect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetBrightness(const AIP: String; const ABrightness: TPercentage; const ATransitionEfect: TTransitionEfect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetColorTemperature(const AIP: String; const AColorTemperature: TColorTemperature; const ATransitionEfect: TTransitionEfect; const ATransitionDuration: TTransitionDuration);
    procedure SetRGB(const AIP: String; const ARGB: TRGBRange;
      const ATransitionEfect: TTransitionEfect;
  const ATransitionDuration: TTransitionDuration);
    property OnConnectionError: TConnectionErrorEvent write FConnectionError;
    property OnBulbFound: TBulbFoundEvent read FOnBulbFound write FOnBulbFound;
    property OnCommandResult: TCommandResultEvent write FOnCommandResult;
  end;

implementation

uses
  DateUtils;

const
  BroadcastAddress = '239.255.255.250';
  BroadcastPort    = '1982';
  BroadcastMessage = 'M-SEARCH * HTTP/1.1'#13#10
                   + 'MAN: "ssdp:discover"'#13#10
                   + 'ST: wifi_bulb'#13#10
                   ;

  BulbPort         = '55443';

{$define implementation}
{$I yeehaacommons.inc}

type

  { TBroadcastThread }

  TBroadcastThread = class(TThread)
    FConn: TUDPBlockSocket;
    FBroadcastIntervalMillisecond: Integer;
    constructor Create(AConn: TUDPBlockSocket; ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    procedure Execute; override;
  end;

{ TBroadcastThread }

constructor TBroadcastThread.Create(AConn: TUDPBlockSocket; ABroadcastIntervalMillisecond: Integer);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  FConn := AConn;
  FBroadcastIntervalMillisecond := ABroadcastIntervalMillisecond;
  FConn.Connect(BroadcastAddress,BroadcastPort);
  Start;
end;

procedure TBroadcastThread.Execute;
begin
  while not Terminated do begin
    FConn.SendString(BroadcastMessage + #13#10);
    Sleep(FBroadcastIntervalMillisecond);
  end;
end;

type

  { TBulbFoundThread }

  TBulbFoundThread = class(TThread)
    FYeeConn: TYeeConn;
    FConn: TUDPBlockSocket;
    constructor Create(AYeeConn: TYeeConn; AConn: TUDPBlockSocket);
    procedure Execute; override;
  end;

{ TBulbFoundThread }  

constructor TBulbFoundThread.Create(AYeeConn: TYeeConn; AConn: TUDPBlockSocket);
begin
  inherited Create(true);
  FYeeConn := AYeeConn;
  FConn    := AConn;
  Start;
end;

procedure TBulbFoundThread.Execute;
var
  LRawResponse,LResponseLine, LKey, LValue: string;
  LRawResponseText: TextFile;
  LRawResponseStream: TStringStream;
  LBulbInfo: TBulbInfo;
  LColonPos: SizeInt;
begin
  while not Terminated do begin
    LRawResponse := FConn.RecvBufferStr(1000, 1000);
    if Trim(LRawResponse) <> EmptyStr then begin
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

      if Assigned(FYeeConn.OnBulbFound) then FYeeConn.OnBulbFound(LBulbInfo);
    end else begin
      //WriteLn('Receive called with no message');
    end;
  end;
end;

{ TYeeConn }

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
  with TTCPBlockSocket.Create do
    try
      HTTPTunnelTimeout := 1000;
      HTTPTunnelPort := IntToStr(FListenPort);
      LJSONMsg := nil;
      LJSONParams := nil;
      LJSONResult := nil;

      Connect(AIP,BulbPort);
      LJSONMsg := CreateJSONObject(['id',AID,'method',AMethod]);
      LJSONParams := CreateJSONArray(AParams);
      LJSONMsg['params'] := LJSONParams;
      LJSONMSgStr := LJSONMsg.AsJSON;
      {$ifdef debug}WriteLn('SendMessage: ' + LJSONMSgStr){$endif};
      SendString(LJSONMSgStr + #13#10);

      if Assigned(FOnCommandResult) then begin
        LRawResult := RecvString(1000);
        {$ifdef debug}WriteLn('ResultReceived: ' + LRawResult);{$endif}
        LJSONResult := TJSONObject(GetJSON(LRawResult));
        LJSONID := LJSONResult.FindPath('id');
        if Assigned(LJSONID) then LCmdID := LJSONID.AsInteger else LCmdID := -1;
        FOnCommandResult(LCmdID,LJSONResult.FindPath('result'),LJSONResult.FindPath('error'));
      end;
    finally
      LJSONResult.Free;
      LJSONMsg.Free;
      Free;
    end;
end;

constructor TYeeConn.Create(const AListenPort: Word; const ABroadcastIntervalMillisecond: Integer);
var
  LListenPortStr: String;
begin
  FListenPort := AListenPort;
  LListenPortStr := IntToStr(FListenPort);

  FUDPConn := TUDPBlockSocket.Create;
  FUDPConn.Bind('0.0.0.0', LListenPortStr);
  if FUDPConn.LastError <> 0 then begin
    writeln('Failed to bind socket: ', FUDPConn.GetErrorDesc(FUDPConn.LastError));
    Halt(1);
  end;
  // Set up the multicast group
  FUDPConn.AddMulticast(LListenPortStr);

  FBroadcastThread := TBroadcastThread.Create(FUDPConn, ABroadcastIntervalMillisecond);
  FBulbFoundThread := TBulbFoundThread.Create(Self, FUDPConn);
end;

destructor TYeeConn.Destroy;
begin
  FBulbFoundThread.Free;
  FBroadcastThread.Free;

  FUDPConn.Free;

  inherited Destroy;
end;

procedure TYeeConn.SetName(const AIP, AName: String);
begin
  SendCommand(AIP,1,'set_name',[AName])
end;

procedure TYeeConn.SetPower(const AIP: String; const AIsOn: Boolean;
  const ATransitionEfect: TTransitionEfect;
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
  const ABrightness: TPercentage; const ATransitionEfect: TTransitionEfect;
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
  const ATransitionEfect: TTransitionEfect;
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
  const ATransitionEfect: TTransitionEfect;
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

