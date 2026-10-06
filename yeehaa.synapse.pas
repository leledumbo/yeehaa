unit yeehaa.synapse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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
    FDiscoveryThread: TThread;
    FOnBulbFound: TBulbFoundEvent;
    FOnCommandResult: TCommandResultEvent;
    procedure FireBulbFound(const ANewBulb: TBulbInfo);
    procedure FireConnectionError(const AMsg: String);
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
  BroadcastPort    = '1982';
  BroadcastMessage = 'M-SEARCH * HTTP/1.1'#13#10
                   + 'MAN: "ssdp:discover"'#13#10
                   + 'ST: wifi_bulb'#13#10
                   ;

  BulbPort                 = '55443';
  DiscoveryReceiveTimeoutMS = 100;    // poll interval of the discovery loop
  CommandConnectTimeoutMS   = 2000;   // max time to establish a bulb TCP connection
  CommandReceiveTimeoutMS   = 2000;   // max time to wait for a bulb response

{$define implementation}
{$I yeehaacommons.inc}

type

  { TDiscoveryThread }

  TDiscoveryThread = class(TThread)
  private
    FOwner: TYeeConn;
    FListenPort: Word;
    FBroadcastIntervalMillisecond: Integer;
  public
    constructor Create(AOwner: TYeeConn; AListenPort: Word;
      ABroadcastIntervalMillisecond: Integer);
    procedure Execute; override;
  end;

{ TDiscoveryThread }

constructor TDiscoveryThread.Create(AOwner: TYeeConn; AListenPort: Word;
  ABroadcastIntervalMillisecond: Integer);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FListenPort := AListenPort;
  FBroadcastIntervalMillisecond := ABroadcastIntervalMillisecond;
end;

procedure TDiscoveryThread.Execute;
var
  LConn: TUDPBlockSocket;
  LNextBroadcast: TDateTime;
  LRawResponse: String;
  LBulbInfo: TBulbInfo;
  LSendErrorReported: Boolean;
begin
  // The socket is created, used and destroyed exclusively inside this
  // thread, so no locking/concurrent access is needed at all.
  LConn := TUDPBlockSocket.Create;
  try
    try
      LConn.Bind('0.0.0.0', IntToStr(FListenPort));
      if LConn.LastError <> 0 then begin
        FOwner.FireConnectionError('Failed to bind UDP socket on port ' +
          IntToStr(FListenPort) + ': ' + LConn.GetErrorDesc(LConn.LastError));
        Exit;
      end;
      // Best effort only; bulbs answer our broadcast with a unicast datagram,
      // so receiving does not depend on multicast membership.
      LConn.AddMulticast(BroadcastAddress);
      // For UDP this only stores the destination address used by SendString,
      // it does NOT filter incoming datagrams.
      LConn.Connect(BroadcastAddress, BroadcastPort);
      if LConn.LastError <> 0 then begin
        FOwner.FireConnectionError('Failed to set broadcast destination: ' +
          LConn.GetErrorDesc(LConn.LastError));
        Exit;
      end;
    except
      on E: Exception do begin
        FOwner.FireConnectionError('Discovery socket setup failed: ' + E.Message);
        Exit;
      end;
    end;

    LNextBroadcast := 0;
    LSendErrorReported := false;
    while not Terminated do begin
      try
        if Now >= LNextBroadcast then begin
          // RecvBufferFrom overwrites FRemoteSin with the source address of
          // every received datagram, so the multicast destination must be
          // restored before each broadcast, otherwise the next "broadcast"
          // is sent unicast to the last bulb that answered.
          LConn.SetRemoteSin(BroadcastAddress, BroadcastPort);
          LConn.SendString(BroadcastMessage + #13#10);
          if LConn.LastError <> 0 then begin
            if not LSendErrorReported then begin
              FOwner.FireConnectionError('Broadcast send error: ' +
                LConn.GetErrorDesc(LConn.LastError));
              LSendErrorReported := true;
            end;
          end else
            LSendErrorReported := false;
          LNextBroadcast := IncMilliSecond(Now, FBroadcastIntervalMillisecond);
          {$ifdef debug}WriteLn('[Discovery] broadcast sent at ', TimeToStr(Now), ', next at ', TimeToStr(LNextBroadcast));{$endif}
        end;

        LRawResponse := LConn.RecvPacket(DiscoveryReceiveTimeoutMS);
        if LRawResponse <> '' then begin
          {$ifdef debug}WriteLn('[Discovery] received ', Length(LRawResponse), ' bytes');{$endif}
          if TryParseBulbInfo(LRawResponse, LBulbInfo) then
            FOwner.FireBulbFound(LBulbInfo);
        end;
      except
        on E: Exception do
          {$ifdef debug}WriteLn('[Discovery] exception: ', E.ClassName, ': ', E.Message);{$endif}
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

procedure TYeeConn.SendCommand(const AIP: String; const AID: Integer;
  const AMethod: String; AParams: array of const);
var
  LSocket: TTCPBlockSocket;
  LJSONMsg, LJSONResult: TJSONObject;
  LJSONParams: TJSONArray;
  LJSONMSgStr: TJSONStringType;
  LJSONID: TJSONData;
  LCmdID: Integer;
  LBuffer, LChunk, LRawResult: String;
  LDeadline, LNow: QWord;
begin
  // A fresh socket per command: no stale buffered data from previous
  // commands and no interaction with the discovery socket.
  LSocket := TTCPBlockSocket.Create;
  try
    LSocket.ConnectionTimeout := CommandConnectTimeoutMS;
    LSocket.Connect(AIP, BulbPort);
    if LSocket.LastError <> 0 then begin
      FireConnectionError('Connect to ' + AIP + ':' + BulbPort + ' failed: ' +
        LSocket.GetErrorDesc(LSocket.LastError));
      Exit;
    end;

    LJSONMsg := nil;
    LJSONParams := nil;
    LJSONResult := nil;
    try
      LJSONMsg := CreateJSONObject(['id', AID, 'method', AMethod]);
      LJSONParams := CreateJSONArray(AParams);
      LJSONMsg['params'] := LJSONParams;
      LJSONMSgStr := LJSONMsg.AsJSON;
      {$ifdef debug}WriteLn('SendMessage: ' + LJSONMSgStr);{$endif}
      LSocket.SendString(LJSONMSgStr + #13#10);
      if LSocket.LastError <> 0 then begin
        FireConnectionError('Send to ' + AIP + ' failed: ' +
          LSocket.GetErrorDesc(LSocket.LastError));
        Exit;
      end;

      if Assigned(FOnCommandResult) then begin
        // TCP may deliver the JSON response in several chunks; accumulate
        // until a complete JSON object can be parsed (or time runs out).
        LBuffer := '';
        LRawResult := '';
        LDeadline := GetTickCount64 + CommandReceiveTimeoutMS;
        while GetTickCount64 < LDeadline do begin
          LNow := GetTickCount64;
          if LNow >= LDeadline then
            Break;
          LChunk := LSocket.RecvPacket(Integer(LDeadline - LNow));
          if LChunk = '' then
            Continue;
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
        end else
          FireConnectionError('No response from ' + AIP + ' within ' +
            IntToStr(CommandReceiveTimeoutMS) + ' ms');
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
  FDiscoveryThread := TDiscoveryThread.Create(Self, FListenPort, ABroadcastIntervalMillisecond);
  TDiscoveryThread(FDiscoveryThread).Start;
end;

destructor TYeeConn.Destroy;
begin
  if Assigned(FDiscoveryThread) then begin
    FDiscoveryThread.Terminate;
    FDiscoveryThread.WaitFor;
    FDiscoveryThread.Free;
    FDiscoveryThread := nil;
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
