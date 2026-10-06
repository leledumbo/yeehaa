program yeehaacli;

{$mode objfpc}{$H+}

uses cthreads
    ,SysUtils
    ,fpjson
    ,fgl
    ,syncobjs
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
    FCS: TCriticalSection;
  private
    procedure DisplayConnectionError(const AMsg: String);
    procedure HandleBulbFound(const ANewBulb: TBulbInfo);
    procedure DisplayResult(const AID: Integer; AResult, AError: TJSONData);
  public
    constructor Create(const AListenPort: Word);
    destructor Destroy; override;
    procedure PrintBulbs;
    procedure DeleteBulbs;
    procedure TogglePower(const AIndex: Integer);
  end;

{ TEventHandler }

procedure THandler.DisplayConnectionError(const AMsg: String);
begin
  WriteLn('Connection error: ' + AMsg);
end;

procedure THandler.HandleBulbFound(const ANewBulb: TBulbInfo);
begin
  // called from the discovery thread
  FCS.Enter;
  try
    FBulbInfos[ANewBulb.ID] := ANewBulb;
  finally
    FCS.Leave;
  end;
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
  FCS := TCriticalSection.Create;
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
  // FConn.Free stops the discovery thread before the shared structures go away
  FConn.Free;
  FBulbInfos.Free;
  FCS.Free;
  inherited Destroy;
end;

procedure THandler.PrintBulbs;
var
  i: Integer;
  LBulbInfo: TBulbInfo;
begin
  FCS.Enter;
  try
    for i := 0 to FBulbInfos.Count - 1 do begin
      LBulbInfo := FBulbInfos.Data[i];
      WriteLn(
        i + 1,
        ': ip='+LBulbInfo.IP+
        ',model='+LBulbInfo.Model+
        ',power=',LBulbInfo.PoweredOn,
        ',brightness=',LBulbInfo.BrightnessPercentage,
        ',rgb=',LBulbInfo.RGB,
        ',name=',LBulbInfo.Name
      );
    end;
  finally
    FCS.Leave;
  end;
end;

procedure THandler.DeleteBulbs;
begin
  FCS.Enter;
  try
    FBulbInfos.Free;
    FBulbInfos := TBulbInfos.Create;
  finally
    FCS.Leave;
  end;
end;

procedure THandler.TogglePower(const AIndex: Integer);
var
  LBulbInfo: TBulbInfo;
  LValid: Boolean;
begin
  FCS.Enter;
  try
    LValid := (0 <= AIndex) and (AIndex < FBulbInfos.Count);
    if LValid then
      LBulbInfo := FBulbInfos.Data[AIndex];
  finally
    FCS.Leave;
  end;
  if LValid then
    FConn.SetPower(LBulbInfo.IP,not LBulbInfo.PoweredOn,teSmooth,500)
  else
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
        WriteLn('[D]elete bulbs');
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
            'D': DeleteBulbs;
            'Q': Quit := true;
            else WriteLn(StdErr,'Command not understood: ' + Cmd);
          end;
        WriteLn;
      until Quit;
    finally
      Free;
    end;
end.
