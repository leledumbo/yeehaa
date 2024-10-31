unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, PairSplitter, JSONPropStorage, syncobjs, fgl,
  fpjson
  ,yeehaa.synapse
  // ,yeehaa.lnet // uncomment to use lnet backend
  ;

type

  TBulbMap = specialize TFPGMap<String,TBulbInfo>;

  { TMainForm }

  TMainForm = class(TForm)
    BRefresh: TButton;
    BSelectAll: TButton;
    BCopy: TButton;
    BClear: TButton;
    CBColor: TColorButton;
    CBPoweredOn: TCheckBox;
    EdModel: TEdit;
    EdName: TEdit;
    GBBulbList: TGroupBox;
    GBBulbProps: TGroupBox;
    GBLog: TGroupBox;
    ConfigStorage: TJSONPropStorage;
    GBOptions: TGroupBox;
    GBColors: TGroupBox;
    GBTransitionDuration: TGroupBox;
    LblColorMode: TLabel;
    LbBrightness: TLabel;
    LBBulbList: TListBox;
    LbModel: TLabel;
    LbName: TLabel;
    LbPoweredOn: TLabel;
    LbRGB: TLabel;
    LbTemperature: TLabel;
    MemoLog: TMemo;
    PairSplitter1: TPairSplitter;
    PairSplitterSide7: TPairSplitterSide;
    PairSplitterSide8: TPairSplitterSide;
    PMemoButtons: TPanel;
    PSBulbLog: TPairSplitter;
    PSBulbListProps: TPairSplitter;
    PSPropsColopts: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    PairSplitterSide5: TPairSplitterSide;
    PSColorsOptions: TPairSplitterSide;
    RGColorMode: TRadioGroup;
    RGTransitionEffect: TRadioGroup;
    SpEdBrightness: TSpinEdit;
    SpEdTemperature: TSpinEdit;
    SpEdTransitionDuration: TSpinEdit;
    procedure BClearClick(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
    procedure BSelectAllClick(Sender: TObject);
    procedure CBColorColorChanged(Sender: TObject);
    procedure CBPoweredOnChange(Sender: TObject);
    procedure EdNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BRefreshClick(Sender: TObject);
    procedure LBBulbListSelectionChange(Sender: TObject; User: boolean);
    procedure RGColorModeSelectionChanged(Sender: TObject);
    procedure SpEdBrightnessChange(Sender: TObject);
    procedure SpEdTemperatureChange(Sender: TObject);
  private
    FYeeConn: TYeeConn;
    FBulbMap: TBulbMap;
    FSelectedBulb: TBulbInfo;
    FCS: TCriticalSection;
    FAutomaticStateChange: Boolean;
    procedure InsertBulb(const ANewBulb: TBulbInfo);
    procedure LogCommandResult(const AID: Integer; AResult, AError: TJSONData);
    procedure LogConnectionError(const AMsg: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  ListenPort = 9999;
  BroadcastIntervalMillisecond = 5000;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBulbMap := TBulbMap.Create;
  FBulbMap.Sorted := true;

  FYeeConn := TYeeConn.Create(ListenPort, BroadcastIntervalMillisecond);
  FYeeConn.OnBulbFound := @InsertBulb;
  FYeeConn.OnCommandResult := @LogCommandResult;
  FYeeConn.OnConnectionError := @LogConnectionError;

  FCS := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FYeeConn.Free;
  FBulbMap.Free;
  FCS.Free;
end;

procedure TMainForm.CBPoweredOnChange(Sender: TObject);
var
  LTransitionEffect: TTransitionEffect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;

    FYeeConn.SetPower(FSelectedBulb.IP,CBPoweredOn.Checked,LTransitionEffect,SpEdTransitionDuration.Value);

    with FSelectedBulb do begin
      PoweredOn := CBPoweredOn.Checked
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.BSelectAllClick(Sender: TObject);
begin
  MemoLog.SelectAll;
end;

procedure TMainForm.CBColorColorChanged(Sender: TObject);
var
  LTransitionEffect: TTransitionEffect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;

    FYeeConn.SetPower(FSelectedBulb.IP,true,LTransitionEffect,SpEdTransitionDuration.Value,pcmRGB);
    FYeeConn.SetRGB(FSelectedBulb.IP,TRGBRange(CBColor.ButtonColor),LTransitionEffect,SpEdTransitionDuration.Value);

    with FSelectedBulb do begin
      PoweredOn := true;
      TransitionEffect := LTransitionEffect;
      TransitionDuration := SpEdTransitionDuration.Value;
      RGB := TRGBRange(CBColor.ButtonColor);
      ColorMode := cmRGB;
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.BCopyClick(Sender: TObject);
begin
  MemoLog.CopyToClipboard;
end;

procedure TMainForm.BClearClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

procedure TMainForm.EdNameChange(Sender: TObject);
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    FYeeConn.SetName(FSelectedBulb.IP,EdName.Text);

    with FSelectedBulb do begin
      Name := EdName.Text;
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.BRefreshClick(Sender: TObject);
begin
  FCS.Enter;
  try
    FBulbMap.Free;
    FBulbMap := TBulbMap.Create;
    FBulbMap.Sorted := true;
    LBBulbList.Clear;
  finally
    FCS.Leave;
  end;
end;

procedure TMainForm.LBBulbListSelectionChange(Sender: TObject; User: boolean);
begin
  GBBulbProps.Enabled := true;
  GBColors.Enabled := true;
  GBOptions.Enabled := true;
  try
    FAutomaticStateChange := true;
    try
      FSelectedBulb := FBulbMap[LBBulbList.GetSelectedText];
      EdModel.Text := FSelectedBulb.Model;
      EdName.Text := FSelectedBulb.Name;
      CBPoweredOn.Checked := FSelectedBulb.PoweredOn;
      SpEdBrightness.Value := FSelectedBulb.BrightnessPercentage;
      CBColor.ButtonColor := RGBToTColor(FSelectedBulb.RGB);
      SpEdTemperature.Value := FSelectedBulb.CT;
      RGColorMode.ItemIndex := Ord(FSelectedBulb.ColorMode) - 1;
    finally
      FAutomaticStateChange := false;
    end;
    // need to manually trigger due to FAutomaticStateChange check as well as no command should be sent due to bulb selection change
    RGColorModeSelectionChanged(LBBulbList);
  except
     on e: EListError do ; // intentionally ignored
  end;
end;

procedure TMainForm.RGColorModeSelectionChanged(Sender: TObject);
var
  LTransitionEffect: TTransitionEffect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;

    case RGColorMode.ItemIndex of
      0: begin
        if Sender <> LBBulbList then FYeeConn.SetRGB(FSelectedBulb.IP,TRGBRange(CBColor.ButtonColor),LTransitionEffect,SpEdTransitionDuration.Value);
        SpEdTemperature.Enabled := false;
        CBColor.Enabled := true;
      end;
      1: begin
        if Sender <> LBBulbList then FYeeConn.SetColorTemperature(FSelectedBulb.IP,SpEdTemperature.Value,LTransitionEffect,SpEdTransitionDuration.Value);
        CBColor.Enabled := false;
        SpEdTemperature.Enabled := true;
      end;
      2: begin
        if Sender <> LBBulbList then // coming soon
        CBColor.Enabled := false;
        SpEdTemperature.Enabled := false;
      end;
    end;

    with FSelectedBulb do begin
      ColorMode := TColorMode(RGColorMode.ItemIndex + 1);
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.SpEdBrightnessChange(Sender: TObject);
var
  LTransitionEffect: TTransitionEffect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;

    FYeeConn.SetBrightness(FSelectedBulb.IP,SpEdBrightness.Value,LTransitionEffect,SpEdTransitionDuration.Value);

    with FSelectedBulb do begin
      BrightnessPercentage := SpEdBrightness.Value;
      TransitionEffect := LTransitionEffect;
      TransitionDuration := SpEdTransitionDuration.Value;
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.SpEdTemperatureChange(Sender: TObject);
var
  LTransitionEffect: TTransitionEffect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;

    FYeeConn.SetPower(FSelectedBulb.IP,true,LTransitionEffect,SpEdTransitionDuration.Value,pcmCT);
    FYeeConn.SetColorTemperature(FSelectedBulb.IP,SpEdTemperature.Value,LTransitionEffect,SpEdTransitionDuration.Value);

    with FSelectedBulb do begin
      PoweredOn := true;
      TransitionEffect := LTransitionEffect;
      TransitionDuration := SpEdTransitionDuration.Value;
      CT := SpEdTemperature.Value;
      ColorMode := cmRGB;
    end;
    FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
  end;
end;

procedure TMainForm.InsertBulb(const ANewBulb: TBulbInfo);
begin
  FCS.Enter;
  try
    if FBulbMap.IndexOf(ANewBulb.IP) < 0 then
      LBBulbList.Items.Add(ANewBulb.IP);
    FBulbMap[ANewBulb.IP] := ANewBulb;

    {$ifdef debug}
    WriteLn('ID = ', ANewBulb.ID);
    WriteLn('IP = ', ANewBulb.IP);
    WriteLn('Model = ', ANewBulb.Model);
    WriteLn('Name = ', ANewBulb.Name);
    WriteLn('PoweredOn = ', ANewBulb.PoweredOn);
    WriteLn('BrightnessPercentage = ', ANewBulb.BrightnessPercentage);
    WriteLn('TransitionEffect = ', ANewBulb.TransitionEffect);
    WriteLn('TransitionDuration = ', ANewBulb.TransitionDuration);
    WriteLn('ColorMode = ', ANewBulb.ColorMode);
    WriteLn('RGB = ', ANewBulb.RGB);
    WriteLn('CT = ', ANewBulb.CT);
    WriteLn;
    {$endif debug}
  finally
    FCS.Leave;
  end;
end;

procedure TMainForm.LogCommandResult(const AID: Integer; AResult,
  AError: TJSONData);
begin
  if Assigned(AResult) then MemoLog.Lines.Add('[Result] ' + AResult.AsJSON);
  if Assigned(AError) then MemoLog.Lines.Add('[Error] ' + AError.AsJSON);
end;

procedure TMainForm.LogConnectionError(const AMsg: String);
begin
  MemoLog.Lines.Add('[Connection error] ' + AMsg);
end;

end.

