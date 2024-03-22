{hint: Pascal files location: .../AppLAMWProject1/jni }
unit modmain;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  fpjson,
  AndroidWidget, Laz_And_Controls, radiogroup, linearlayout,
  menu, contextmenu, Spinner,
  yeehaa;
  
type

  { TAMMain }

  TAMMain = class(jForm)
    LLMain: jLinearLayout;
    SpBulbList: jSpinner;
    TVName: jTextView;
    CBPoweredOn: jCheckBox;
    ETLog: jEditText;
    procedure AMMainCreate(Sender: TObject);
    procedure AMMainDestroy(Sender: TObject);
    procedure CBPoweredOnClick(Sender: TObject);
    procedure SpBulbListItemSelected(Sender: TObject; itemCaption: string;
      itemIndex: integer);
  private
    procedure InsertBulb(const ANewBulb: TBulbInfo);
    procedure LogCommandResult(const AID: Integer; AResult, AError: TJSONData);
    procedure LogConnectionError(const AMsg: String);
    {private declarations}
  public
    {public declarations}
  end;

var
  AMMain: TAMMain;

implementation
  
{$R *.lfm}

uses
  syncobjs,fgl;

const
  ListenPort = 9999;
  BroadcastIntervalMillisecond = 5000;

type
  TBulbMap = TFPGMap<String,TBulbInfo>;

var
  FYeeConn: TYeeConn;
  FBulbMap: TBulbMap;
  FSelectedBulb: TBulbInfo;
  FCS: TCriticalSection;
  FAutomaticStateChange: Boolean;

{ TAMMain }

procedure TAMMain.AMMainCreate(Sender: TObject);
begin
  FBulbMap := TBulbMap.Create;
  FBulbMap.Sorted := true;

  FYeeConn := TYeeConn.Create(ListenPort, BroadcastIntervalMillisecond);
  FYeeConn.OnBulbFound := InsertBulb;
  FYeeConn.OnCommandResult := LogCommandResult;
  FYeeConn.OnConnectionError := LogConnectionError;

  FCS := TCriticalSection.Create;
end;

procedure TAMMain.AMMainDestroy(Sender: TObject);
begin
  FYeeConn.Free;
  FBulbMap.Free;
  FCS.Free;
end;

procedure TAMMain.CBPoweredOnClick(Sender: TObject);
var
  LTransitionEffect: TTransitionEfect;
begin
  if (SpBulbList.SelectedIndex >= 0) and not FAutomaticStateChange then begin
    {case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;}
    LTransitionEffect := teSmooth;
    FYeeConn.SetPower(FSelectedBulb.IP,CBPoweredOn.Checked,LTransitionEffect,500);
  end;
end;

procedure TAMMain.SpBulbListItemSelected(Sender: TObject;
  itemCaption: string; itemIndex: integer);
begin
  try
    FAutomaticStateChange := true;
    try
      FSelectedBulb := FBulbMap[SpBulbList.GetSelectedItem()];
      CBPoweredOn.Checked := FSelectedBulb.PoweredOn;
      TVName.Text := FSelectedBulb.Name;
      {SpEdBrightness.Value := FSelectedBulb.BrightnessPercentage;
      CBColor.ButtonColor := RGBToTColor(FSelectedBulb.RGB);
      SpEdTemperature.Value := FSelectedBulb.CT;}
    finally
      FAutomaticStateChange := false;
    end;
  except
     on e: EListError do ; // intentionally ignored
  end;
end;

procedure TAMMain.InsertBulb(const ANewBulb: TBulbInfo);
begin
  FCS.Enter;
  try
    if FBulbMap.IndexOf(ANewBulb.IP) < 0 then begin
      SpBulbList.Items.Add(ANewBulb.IP);
    end;
    FBulbMap[ANewBulb.IP] := ANewBulb;
  finally
    FCS.Leave;
  end;
end;

procedure TAMMain.LogCommandResult(const AID: Integer; AResult,
  AError: TJSONData);
begin
  if Assigned(AResult) then ETLog.AppendLn('[Result] ' + AResult.AsJSON);
  if Assigned(AError) then ETLog.AppendLn('[Error] ' + AError.AsJSON);
end;

procedure TAMMain.LogConnectionError(const AMsg: String);
begin
  ETLog.AppendLn('[Connection error] ' + AMsg);
end;

initialization
  FAutomaticStateChange := false;

end.
