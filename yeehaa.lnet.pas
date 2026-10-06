unit yeehaa.lnet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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
    FBroadcastThread: TThread;
    FOnBulbFound: TBulbFoundEvent;
    FOnCommandResult: TCommandResultEvent;
    FLastCommandError: String;
    FHasCommandError: Boolean;
    procedure FireBulbFound(const ANewBulb: TBulbInfo);
    procedure FireConnectionError(const AMsg: String);
    procedure HandleSocketError(const msg: string; aSocket: TLSocket);
    procedure SendCommand(const AIP: String; const AID: Integer; const AMethod: String; AParams: array of const);
  public
    constructor Create(const AListenPort: Word;
      const ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    destructor Destroy; override;
    procedure SetName(const AIP, AName: String);
    procedure SetPower(const AIP: String; const AIsOn: Boolean; const ATransitionEffect: TTransitionEffect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetBrightness(const AIP: String; const ABrightness: TPercentage; const ATransitionEffect: TTransitionEffect; const ATransitionDuration: TTransitionDuration; const AColorMode: TPowerColorMode = pcmDefault);
    procedure SetColorTemperature(const AIP: String; const AColorTemperature: TColorTemperature; const ATransitionEffect: TTransitionEffect; const ATransitionDuration: TTransitionDuration);
    procedure SetRGB(const AIP: String; const ARGB: TRGBRange;
      const ATransitionEffect: TTransitionEffect;
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
  BroadcastPort    = 1982;
  BroadcastMessage = 'M-SEARCH * HTTP/1.1'#13#10
                   + 'MAN: "ssdp:discover"'#13#10
                   + 'ST: wifi_bulb'#13#10
                   ;

  BulbPort                = 55443;
  PollIntervalMS          = 100;   // event pump granularity
  CommandConnectTimeoutMS = 2000;  // max time to establish a bulb TCP connection
  CommandReceiveTimeoutMS = 2000;  // max time to wait for a bulb response

{$define implementation}
{$I yeehaacommons.inc}

type

  { TBroadcastThread }

  TBroadcastThread = class(TThread)
  private
    FOwner: TYeeConn;
    FListenPort: Word;
    FBroadcastIntervalMillisecond: Integer;
    procedure HandleReceive(aSocket: TLSocket);
    procedure HandleError(const msg: string; aSocket: TLSocket);
  public
    constructor Create(AOwner: TYeeConn; AListenPort: Word;
      ABroadcastIntervalMillisecond: Integer);
    procedure Execute; override;
  end;

{ TBroadcastThread }

constructor TBroadcastThread.Create(AOwner: TYeeConn; AListenPort: Word;
  ABroadcastIntervalMillisecond: Integer);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FListenPort := AListenPort;
  FBroadcastIntervalMillisecond := ABroadcastIntervalMillisecond;
end;

procedure TBroadcastThread.HandleReceive(aSocket: TLSocket);
var
  LRawResponse: String;
  LBulbInfo: TBulbInfo;
begin
  if aSocket.GetMessage(LRawResponse) > 0 then begin
    {$ifdef debug}WriteLn(LRawResponse);{$endif}
    if TryParseBulbInfo(LRawResponse, LBulbInfo) then
      FOwner.FireBulbFound(LBulbInfo);
  end;
end;

procedure TBroadcastThread.HandleError(const msg: string; aSocket: TLSocket);
begin
  FOwner.FireConnectionError(msg);
end;

procedure TBroadcastThread.Execute;
var
  LConn: TLUdp;
  LNextBroadcast: TDateTime;
begin
  // The TLUdp instance is created, used and destroyed exclusively inside
  // this thread, so all event dispatching stays in one thread.
  LConn := TLUdp.Create(nil);
  try
    LConn.Timeout := PollIntervalMS;
    LConn.OnReceive := @HandleReceive;
    LConn.OnError := @HandleError;
    if not LConn.Listen(FListenPort) then
      Exit; // the OnError handler has already reported the reason

    LNextBroadcast := 0;
    while not Terminated do begin
      if Now >= LNextBroadcast then begin
        // explicit send-to-address: no need for Connect() and no risk of
        // flooding, we send exactly once per interval
        LConn.SendMessage(BroadcastMessage, BroadcastAddress + ':' + IntToStr(BroadcastPort));
        LNextBroadcast := IncMilliSecond(Now, FBroadcastIntervalMillisecond);
      end;

      // Pumps lNet events and blocks up to PollIntervalMS, so this is not a
      // busy loop. Receive/Error events fire in this thread.
      LConn.CallAction;

      if not LConn.Connected then begin
        // socket was dropped by an error; give it a moment and try to
        // re-establish the listening socket once per failure
        Sleep(100);
        if Terminated then
          Break;
        if not LConn.Listen(FListenPort) then
          Exit; // OnError handler already reported the reason
      end;
    end;
  finally
    LConn.Free;
  end;
end;

{ TYeeConn }

procedure TYeeConn.FireBulbFound(const ANewBulb: TBulbInfo);
begin
  if Assigned(FOnBulbFound) then
    FOnBulbFound(ANewBulb);
end;

procedure TYeeConn.FireConnectionError(const AMsg: String);
begin
  if Assigned(FConnectionError) then
    FConnectionError(AMsg);
end;

procedure TYeeConn.HandleSocketError(const msg: string; aSocket: TLSocket);
begin
  FLastCommandError := msg;
  FHasCommandError := true;
end;

procedure TYeeConn.SendCommand(const AIP: String; const AID: Integer;
  const AMethod: String; AParams: array of const);
var
  LSocket: TLTcp;
  LJSONMsg, LJSONResult: TJSONObject;
  LJSONParams: TJSONArray;
  LJSONMSgStr: TJSONStringType;
  LJSONID: TJSONData;
  LCmdID: Integer;
  LBuffer, LChunk, LRawResult: String;
  LDeadline: QWord;
begin
  LSocket := TLTcp.Create(nil);
  try
    LSocket.Timeout := PollIntervalMS;
    LSocket.OnError := @HandleSocketError;
    FHasCommandError := false;
    FLastCommandError := '';
    LJSONMsg := nil;
    LJSONParams := nil;
    LJSONResult := nil;
    try
      if LSocket.Connect(AIP, BulbPort) then begin
        // lNet connects asynchronously; pump until connected, failed or the
        // deadline passes. Every path below is bounded now.
        LDeadline := GetTickCount64 + CommandConnectTimeoutMS;
        while (not LSocket.Connected) and (not FHasCommandError) and
              (GetTickCount64 < LDeadline) do
          LSocket.CallAction;

        if FHasCommandError then
          FireConnectionError(FLastCommandError)
        else if not LSocket.Connected then
          FireConnectionError('Connect to ' + AIP + ':' + IntToStr(BulbPort) + ' timed out')
        else begin
          LJSONMsg := CreateJSONObject(['id', AID, 'method', AMethod]);
          LJSONParams := CreateJSONArray(AParams);
          LJSONMsg['params'] := LJSONParams;
          LJSONMSgStr := LJSONMsg.AsJSON;
          {$ifdef debug}WriteLn('SendMessage: ' + LJSONMSgStr);{$endif}
          LSocket.SendMessage(LJSONMSgStr + #13#10);

          if Assigned(FOnCommandResult) then begin
            // TCP may deliver the JSON response in several chunks; accumulate
            // until a complete JSON object can be parsed (or time runs out).
            LBuffer := '';
            LRawResult := '';
            LDeadline := GetTickCount64 + CommandReceiveTimeoutMS;
            while (not FHasCommandError) and LSocket.Connected and
                  (GetTickCount64 < LDeadline) do begin
              LSocket.CallAction;
              LChunk := '';
              if LSocket.Connected and (LSocket.GetMessage(LChunk) > 0) then begin
                LBuffer += LChunk;
                LRawResult := Trim(LBuffer);
                try
                  LJSONResult := TJSONObject(GetJSON(LRawResult));
                except
                  LJSONResult := nil;
                end;
                if Assigned(LJSONResult) then
                  Break;
              end;
            end;

            if not Assigned(LJSONResult) and (LRawResult <> '') then begin
              // last chance: the response may have arrived without CRLF
              try
                LJSONResult := TJSONObject(GetJSON(LRawResult));
              except
                LJSONResult := nil;
              end;
            end;

            if Assigned(LJSONResult) then begin
              {$ifdef debug}WriteLn('ResultReceived: ' + LRawResult);{$endif}
              LJSONID := LJSONResult.FindPath('id');
              if Assigned(LJSONID) then
                LCmdID := LJSONID.AsInteger
              else
                LCmdID := -1;
              FOnCommandResult(LCmdID, LJSONResult.FindPath('result'), LJSONResult.FindPath('error'));
            end else if FHasCommandError then
              FireConnectionError(FLastCommandError)
            else
              FireConnectionError('No response from ' + AIP + ' within ' +
                IntToStr(CommandReceiveTimeoutMS) + ' ms');
          end;
        end;
      end else begin
        if FHasCommandError then
          FireConnectionError(FLastCommandError)
        else
          FireConnectionError('Cannot initiate connection to ' + AIP + ':' + IntToStr(BulbPort));
      end;
    finally
      LJSONResult.Free;
      LJSONMsg.Free;
    end;
  finally
    LSocket.Free;
  end;
end;

constructor TYeeConn.Create(const AListenPort: Word; const ABroadcastIntervalMillisecond: Integer);
begin
  FListenPort := AListenPort;
  FBroadcastThread := TBroadcastThread.Create(Self, FListenPort, ABroadcastIntervalMillisecond);
  TBroadcastThread(FBroadcastThread).Start;
end;

destructor TYeeConn.Destroy;
begin
  if Assigned(FBroadcastThread) then begin
    FBroadcastThread.Terminate;
    FBroadcastThread.WaitFor;
    FBroadcastThread.Free;
    FBroadcastThread := nil;
  end;
  inherited Destroy;
end;

procedure TYeeConn.SetName(const AIP, AName: String);
begin
  SendCommand(AIP, 1, 'set_name', [AName]);
end;

procedure TYeeConn.SetPower(const AIP: String; const AIsOn: Boolean;
  const ATransitionEffect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration;
  const AColorMode: TPowerColorMode);
var
  LPowerStateStr, LTransitionEffectStr: String;
begin
  if AIsOn then
    LPowerStateStr := 'on'
  else
    LPowerStateStr := 'off';
  case ATransitionEffect of
    teSmooth: LTransitionEffectStr := 'smooth';
    teSudden: LTransitionEffectStr := 'sudden';
  end;
  SendCommand(AIP, 1, 'set_power', [LPowerStateStr, LTransitionEffectStr, ATransitionDuration, Ord(AColorMode)]);
end;

procedure TYeeConn.SetBrightness(const AIP: String;
  const ABrightness: TPercentage; const ATransitionEffect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration;
  const AColorMode: TPowerColorMode);
var
  LTransitionEffectStr: String;
begin
  case ATransitionEffect of
    teSmooth: LTransitionEffectStr := 'smooth';
    teSudden: LTransitionEffectStr := 'sudden';
  end;
  SendCommand(AIP, 1, 'set_bright', [ABrightness, LTransitionEffectStr, ATransitionDuration]);
end;

procedure TYeeConn.SetColorTemperature(const AIP: String;
  const AColorTemperature: TColorTemperature;
  const ATransitionEffect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration);
var
  LTransitionEffectStr: String;
begin
  case ATransitionEffect of
    teSmooth: LTransitionEffectStr := 'smooth';
    teSudden: LTransitionEffectStr := 'sudden';
  end;
  SendCommand(AIP, 1, 'set_ct_abx', [AColorTemperature, LTransitionEffectStr, ATransitionDuration]);
end;

procedure TYeeConn.SetRGB(const AIP: String; const ARGB: TRGBRange;
  const ATransitionEffect: TTransitionEffect;
  const ATransitionDuration: TTransitionDuration);
var
  LTransitionEffectStr: String;
begin
  case ATransitionEffect of
    teSmooth: LTransitionEffectStr := 'smooth';
    teSudden: LTransitionEffectStr := 'sudden';
  end;
  SendCommand(AIP, 1, 'set_rgb', [NtoBE(ColorToRGB(ARGB)) shr 8, LTransitionEffectStr, ATransitionDuration]);
end;

end.
