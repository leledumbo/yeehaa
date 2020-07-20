library yeehaalib;

{$mode objfpc}{$H+}
{$packrecords c}

uses
  cthreads,
  SysUtils,
  Strings,
  ctypes,
  fpjson,
  fgl,
  Yeehaa;

type

  TBulbData = record
    IP                  : PChar;
    Model               : PChar;
    PoweredOn           : cbool;
    BrightnessPercentage: cuint8;
    RGB                 : cint32;
  end;

  TValueType = (vtInteger,vtString);
  TValue = record
    Key: PChar; // always nil for arrays
    case ValueType: TValueType of
      vtInteger: (IntValue: cint32);
      vtString : (StrValue: PChar);
  end;
  PValue = ^TValue;

  TCommandResult = record
    IsError: cbool;
    ValueCount: cint32;
    Values: PValue;
  end;

  TBulbFoundCallback       = procedure (constref ANewBulb: TBulbData); cdecl;
  TCommandResultCallback   = procedure (const AID: cint32; constref AResult: TCommandResult); cdecl;
  TConnectionErrorCallback = procedure (const AMsg: PChar); cdecl;

  { TYeeBroker }

  TYeeBroker = class
  private
    FConn: TYeeConn;
    FOnBulbFound: TBulbFoundCallback;
    FOnCommandResult: TCommandResultCallback;
    FOnConnectionError: TConnectionErrorCallback;
    procedure HandleBulbFound(const ANewBulb: TBulbInfo);
    procedure HandleCommandResult(const AID: Integer; AResult, AError: TJSONData);
    procedure HandleConnectionError(const AMsg: String);
  public
    constructor Create(const AConn: TYeeConn);
    destructor Destroy; override;
    property Conn: TYeeConn read FConn;
    property OnBulbFound: TBulbFoundCallback write FOnBulbFound;
    property OnCommandResult: TCommandResultCallback write FOnCommandResult;
    property OnConnectionError: TConnectionErrorCallback write FOnConnectionError;
  end;

{ TEventBroker }

procedure TYeeBroker.HandleBulbFound(const ANewBulb: TBulbInfo);
var
  LBulbData: TBulbData;
begin
  LBulbData.IP                   := PChar(ANewBulb.IP);
  LBulbData.Model                := PChar(ANewBulb.Model);
  LBulbData.PoweredOn            := ANewBulb.PoweredOn;
  LBulbData.BrightnessPercentage := ANewBulb.BrightnessPercentage;
  LBulbData.RGB                  := ANewBulb.RGB;
  if Assigned(FOnBulbFound) then FOnBulbFound(LBulbData);
end;

procedure TYeeBroker.HandleCommandResult(const AID: Integer; AResult,
  AError: TJSONData);
var
  LCommandResult: TCommandResult;
  i: Integer;

  procedure DecodeCommandResult(ACmdResult: TJSONData);
  var
    i: Integer;
    s: String;
    LValue: TValue;
    LData: TJSONData;
    LResultObj: TJSONObject;
    LResultArr: TJSONArray;
  begin
    case AResult.JSONType of
      jtArray: begin
        LResultArr := TJSONArray(ACmdResult);
        LCommandResult.ValueCount := LResultArr.Count;
        LCommandResult.Values := GetMem(LCommandResult.ValueCount * SizeOf(TValue));
        for i := 0 to LCommandResult.ValueCount - 1 do begin
          LData := LResultArr[i];
          Str(i,s);
          LValue.Key := StrAlloc(Length(s) + 1);
          StrPCopy(LValue.Key, s);

          case LData.JSONType of
            jtString: begin
              LValue.ValueType := vtString;
              LValue.StrValue := PChar(LData.AsString);
            end;
            jtNumber: begin
              LValue.ValueType := vtInteger;
              LValue.IntValue := LData.AsInteger;
            end;
            otherwise raise Exception.CreateFmt('Command Result[%d]: unexpected JSON type "%s"',[i,AResult.JSONType]);
          end;
          LCommandResult.Values[i] := LValue;
        end;
      end;
      jtObject: begin
        LResultObj := TJSONObject(ACmdResult);
        LCommandResult.ValueCount := LResultObj.Count;
        LCommandResult.Values := GetMem(LCommandResult.ValueCount * SizeOf(TValue));
        for i := 0 to LCommandResult.ValueCount - 1 do begin
          LData := LResultObj.Items[i];
          LValue.Key := StrAlloc(Length(LResultObj.Names[i]) + 1);
          StrPCopy(LValue.Key, LResultObj.Names[i]);

          case LData.JSONType of
            jtString: begin
              LValue.ValueType := vtString;
              LValue.StrValue := PChar(LData.AsString);
            end;
            jtNumber: begin
              LValue.ValueType := vtInteger;
              LValue.IntValue := LData.AsInteger;
            end;
            otherwise raise Exception.CreateFmt('Command Result[%d]: unexpected JSON type "%s"',[i,AResult.JSONType]);
          end;
          writeln(LValue.ValueType);

          LCommandResult.Values[i] := LValue;
        end;
      end;
      otherwise raise Exception.CreateFmt('Command Result: unexpected JSON type "%s"',[AResult.JSONType]);
    end;
  end;

begin
  if Assigned(FOnCommandResult) then begin
    // looks stupid, but should work... I guess
    LCommandResult.IsError := Assigned(AError);
    if Assigned(AResult) then DecodeCommandResult(AResult);
    if Assigned(AError) then DecodeCommandResult(AError);
    FOnCommandResult(AID,LCommandResult);
    for i := 0 to LCommandResult.ValueCount - 1 do
      StrDispose(LCommandResult.Values[i].Key);
    FreeMem(LCommandResult.Values);
  end;
end;

procedure TYeeBroker.HandleConnectionError(const AMsg: String);
begin
  if Assigned(FOnConnectionError) then FOnConnectionError(PChar(AMsg));
end;

constructor TYeeBroker.Create(const AConn: TYeeConn);
begin
  FConn := AConn;
  with FConn do begin
    OnConnectionError := @HandleConnectionError;
    OnBulbFound       := @HandleBulbFound;
    OnCommandResult   := @HandleCommandResult;
  end;
end;

destructor TYeeBroker.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

{ exported APIs }

function InitializeConnection(const AListenPort: cuint16; const ABroadcastIntervalMillisecond: cint32): TYeeBroker; cdecl;
begin
  Result := TYeeBroker.Create(TYeeConn.Create(AListenPort, ABroadcastIntervalMillisecond));
end;

procedure FinalizeConnection(const ABroker: TYeeBroker); cdecl;
begin
  ABroker.Free;
end;

procedure RegisterOnConnectionErrorCallback(const ABroker: TYeeBroker; const ACallbackEvent: TConnectionErrorCallback) cdecl;
begin
  ABroker.OnConnectionError := ACallbackEvent;
end;

procedure RegisterOnBulbFoundCallback(const ABroker: TYeeBroker; const ACallbackEvent: TBulbFoundCallback) cdecl;
begin
  ABroker.OnBulbFound := ACallbackEvent;
end;

procedure RegisterOnCommandResultCallback(const ABroker: TYeeBroker; const ACallbackEvent: TCommandResultCallback) cdecl;
begin
  ABroker.OnCommandResult := ACallbackEvent;
end;

procedure SetPower(const ABroker: TYeeBroker; const AIP: PChar; const AIsOn: cbool; const ATransitionEfect: TTransitionEfect; const ATransitionDuration: TTransitionDuration); cdecl;
begin
  ABroker.Conn.SetPower(String(AIP),AIsOn,ATransitionEfect,ATransitionDuration);
end;

exports
  InitializeConnection,
  FinalizeConnection,
  RegisterOnConnectionErrorCallback,
  RegisterOnBulbFoundCallback,
  RegisterOnCommandResultCallback,
  SetPower;

end.

