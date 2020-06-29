library yeehaalib;

{$mode objfpc}{$H+}
{$packrecords c}

uses
  cthreads,
  ctypes,
  Yeehaa;

type

  TBulbInfo = record
    IP: PChar;
    Model: PChar;
    Power: PChar;
    Brightness: PChar;
    RGB: PChar;
  end;

  PBulbInfo = ^TBulbInfo;

  TBulbInfoList = record
    Count: cint64;
    Data: PBulbInfo;
  end;
  PBulbInfoList = ^TBulbInfoList;

function InitializeConnection(const AListenPort: cuint16; const ABroadcastIntervalMillisecond: cint32): TYeeConn; cdecl;
begin
  Result := TYeeConn.Create(AListenPort, ABroadcastIntervalMillisecond);
end;

procedure FinalizeConnection(const AConn: TYeeConn); cdecl;
begin
  AConn.Free;
end;

function GetBulbInfos(const AConn: TYeeConn): PBulbInfoList; cdecl;
var
  LBulbInfos: TBulbInfos;
  LBulbInfo: Yeehaa.TBulbInfo;
  i: Integer;
begin
  LBulbInfos := AConn.BulbInfos;

  New(Result);
  Result^.Count := LBulbInfos.Count;
  Result^.Data := GetMem(Result^.Count * SizeOf(TBulbInfo));

  for i := 0 to Result^.Count - 1 do begin
    LBulbInfo := LBulbInfos.Data[i];
    Result^.Data[i].IP := PChar(LBulbInfo.IP);
    Result^.Data[i].Model := PChar(LBulbInfo.Model);
    Result^.Data[i].Power := PChar(LBulbInfo.Power);
    Result^.Data[i].Brightness := PChar(LBulbInfo.Brightness);
    Result^.Data[i].RGB := PChar(LBulbInfo.RGB);
  end;
end;

procedure FreeBulbInfos(const ABulbInfoList: PBulbInfoList); cdecl;
begin
  FreeMem(ABulbInfoList^.Data);
  Dispose(ABulbInfoList);
end;

exports
  InitializeConnection,
  FinalizeConnection,
  GetBulbInfos,
  FreeBulbInfos;

end.

