program yeehaacli;

{$mode objfpc}{$H+}

uses
  cthreads,
  Yeehaa;

const
  ListenPort = 9999;

procedure PrintBulbs(const ABulbInfos: TBulbInfos);
var
  i: Integer;
  LBulbInfo: TBulbInfo;
begin
  for i := 0 to ABulbInfos.Count - 1 do begin
    LBulbInfo := ABulbInfos.Data[i];
    WriteLn(
      i + 1,
      ': ip='+LBulbInfo.IP+
      ',model='+LBulbInfo.Model+
      ',power='+LBulbInfo.Power+
      ',brightness='+LBulbInfo.Brightness+
      ',rgb='+LBulbInfo.RGB
    );
  end;
end;

var
  Cmd: string;
  Quit: Boolean;
begin
  with TYeeConn.Create(ListenPort) do
    try
      Quit := false;
      repeat
        WriteLn('[P]rint bulbs');
        WriteLn('[R]efresh bulbs');
        WriteLn('[Q]uit');
        Write('Cmd: ');ReadLn(Cmd);
        if Length(Cmd) > 0 then
          case UpCase(Cmd[1]) of
            'P': PrintBulbs(BulbInfos);
            'R': RefreshBulbs;
            'Q': Quit := true;
            else WriteLn(StdErr,'Command not understood: ' + Cmd);
          end;
        WriteLn;
      until Quit;
    finally
      Free;
    end;
end.
