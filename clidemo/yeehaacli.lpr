program yeehaacli;

{$mode objfpc}{$H+}

uses cthreads
    ,SysUtils
    ,fpjson
    ,fgl
    ,yeehaa.synapse
    // ,yeehaa.lnet // uncomment to use lnet backend
    ;

const
  ListenPort = 9999;

type

  TBulbInfos = specialize TFPGMap<String,TBulbInfo>;

  { THandler }

  THandler = class
    FBulbInfos: TBulbInfos;
    FConn: TYeeConn;
  private
    procedure DisplayConnectionError(const AMsg: String);
    procedure HandleBulbFound(const ANewBulb: TBulbInfo);
    procedure DisplayResult(const AID: Integer; AResult, AError: TJSONData);
  public
    constructor Create(const AListenPort: Word);
    destructor Destroy; override;
    procedure PrintBulbs;
    procedure TogglePower(const AIndex: Integer);
  end;

{ TEventHandler }

procedure THandler.DisplayConnectionError(const AMsg: String);
begin
  WriteLn('Connection error: ' + AMsg);
end;

procedure THandler.HandleBulbFound(const ANewBulb: TBulbInfo);
begin
  FBulbInfos[ANewBulb.ID] := ANewBulb;
end;

procedure THandler.DisplayResult(const AID: Integer; AResult, AError: TJSONData
  );
begin
  Write('Command ',AID,': ');
  if Assigned(AResult) then Write('Result = ',AResult.AsJSON);
  if Assigned(AError) then Write('Error = ',AError.AsJSON);
end;

constructor THandler.Create(const AListenPort: Word);
begin
  FBulbInfos := TBulbInfos.Create;

  FConn := TYeeConn.Create(AListenPort);
  with FConn do begin
    OnBulbFound       := @HandleBulbFound;
    OnCommandResult   := @DisplayResult;
    OnConnectionError := @DisplayConnectionError;
  end;
end;

destructor THandler.Destroy;
begin
  FConn.Free;
  FBulbInfos.Free;
  inherited Destroy;
end;

procedure THandler.PrintBulbs;
var
  i: Integer;
  LBulbInfo: TBulbInfo;
begin
  for i := 0 to FBulbInfos.Count - 1 do begin
    LBulbInfo := FBulbInfos.Data[i];
    WriteLn(
      i + 1,
      ': ip='+LBulbInfo.IP+
      ',model='+LBulbInfo.Model+
      ',power=',LBulbInfo.PoweredOn,
      ',brightness=',LBulbInfo.BrightnessPercentage,
      ',rgb=',LBulbInfo.RGB
    );
  end;
end;

procedure THandler.TogglePower(const AIndex: Integer);
var
  LBulbInfo: TBulbInfo;
begin
  if (0 <= AIndex) and (AIndex < FBulbInfos.Count) then begin
    LBulbInfo := FBulbInfos.Data[AIndex];
    FConn.SetPower(LBulbInfo.IP,not LBulbInfo.PoweredOn,teSmooth,500);
  end else
    WriteLn('Invalid bulb index, print first to look for valid ones')
end;

var
  Quit: Boolean;
  Cmd: string;
begin
  with THandler.Create(ListenPort) do
    try
      Quit := false;
      repeat
        WriteLn('[P]rint bulbs');
        WriteLn('[T]oggle power');
        WriteLn('[Q]uit');
        Write('Cmd: ');ReadLn(Cmd);
        if Length(Cmd) > 0 then
          case UpCase(Cmd[1]) of
            'P': PrintBulbs;
            'T': begin
              Write('Bulb index: ');ReadLn(Cmd);
              TogglePower(StrToIntDef(Cmd,-1));
            end;
            'Q': Quit := true;
            else WriteLn(StdErr,'Command not understood: ' + Cmd);
          end;
        WriteLn;
      until Quit;
    finally
      Free;
    end;
end.
