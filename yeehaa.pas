unit Yeehaa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamIO,
  fgl,
  lNet;

const
  DefaultBroadcastIntervalMilliSeconds = 100;

type

  TBulbInfo = record
    IP: String;
    Model: String;
    Power: String;
    Brightness: String;
    RGB: String;
  end;

  TBulbInfos = specialize TFPGMap<String,TBulbInfo>;

  TBulbFoundEvent = procedure (const ANewBulb: TBulbInfo) of object;

  { TYeeConn }

  TYeeConn = class
  private
    FConn: TLUdp;
    FBroadcastThread: TThread;
    FBulbInfos: TBulbInfos;
    FOnBulbFound: TBulbFoundEvent;
    procedure CanSend(aSocket: TLSocket);
    procedure Error(const msg: string; aSocket: TLSocket);
    procedure Receive(aSocket: TLSocket);
  public
    constructor Create(const AListenPort: Word;
      const ABroadcastIntervalMillisecond: Integer = DefaultBroadcastIntervalMilliSeconds);
    destructor Destroy; override;
    procedure RefreshBulbs;
    property BulbInfos: TBulbInfos read FBulbInfos;
    property OnBulbFound: TBulbFoundEvent write FOnBulbFound;
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

procedure TYeeConn.Receive(aSocket: TLSocket);
var
  LRawResponse,LResponseLine, LKey, LValue: string;
  LRawResponseText: TextFile;
  LRawResponseStream: TStringStream;
  LBulbInfo: TBulbInfo;
  LColonPos: SizeInt;
begin
  if aSocket.GetMessage(LRawResponse) > 0 then begin
    LRawResponseStream := TStringStream.Create(LRawResponse);
    AssignStream(LRawResponseText, LRawResponseStream);
    Reset(LRawResponseText);
    while not EOF(LRawResponseText) do begin
      ReadLn(LRawResponseText,LResponseLine);
      LColonPos := Pos(':',LResponseLine);
      if LColonPos > 0 then begin
        LKey := Copy(LResponseLine,1,LColonPos - 1);
        LValue := Copy(LResponseLine,LColonPos + 2,Length(LResponseLine) - LColonPos + 2);
        case LKey of
          // strip away protocol and port, only address is required
          'Location': LBulbInfo.IP         := Copy(LValue,12,Length(LValue) - 17);
          'model'   : LBulbInfo.Model      := LValue;
          'power'   : LBulbInfo.Power      := LValue;
          'bright'  : LBulbInfo.Brightness := LValue;
          'rgb'     : LBulbInfo.RGB        := LValue;
        end;
      end;
    end;
    CloseFile(LRawResponseText);
    LRawResponseStream.Free;

    FBulbInfos[LBulbInfo.IP] := LBulbInfo;
    if Assigned(FOnBulbFound) then FOnBulbFound(LBulbInfo);
    {$ifdef debug}WriteLn(FBulbInfos.Count, ' bulb(s) found');{$endif}
  end else begin
    WriteLn('Receive called with no message');
  end;
end;

procedure TYeeConn.Error(const msg: string; aSocket: TLSocket);
begin
  WriteLn('Error: '+msg);
end;

procedure TYeeConn.CanSend(aSocket: TLSocket);
begin
  {$ifdef debug}WriteLn('SendMessage: ', {$endif}FConn.SendMessage(BroadcastMessage){$ifdef debug}){$endif};
end;

constructor TYeeConn.Create(const AListenPort: Word; const ABroadcastIntervalMillisecond: Integer);
begin
  FBulbInfos := TBulbInfos.Create;

  FConn           := TLUdp.Create(nil);
  FConn.Port      := AListenPort;
  FConn.OnCanSend := @CanSend;
  FConn.OnReceive := @Receive;
  FConn.OnError   := @Error;

  FBroadcastThread := TBroadcastThread.Create(FConn, ABroadcastIntervalMillisecond);
end;

destructor TYeeConn.Destroy;
begin
  FBroadcastThread.Free;
  FConn.Disconnect(true);
  FConn.Free;
  FBulbInfos.Free;

  inherited Destroy;
end;

procedure TYeeConn.RefreshBulbs;
begin
  FBulbInfos.Clear;
end;

end.

