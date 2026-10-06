unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, PairSplitter, JSONPropStorage, syncobjs, fgl,
  fpjson
  ,yeehaa.synapse
  //,yeehaa.lnet // uncomment to use lnet backend
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
    BulbListTimer: TTimer;         // created in code; flushes pending data on main thread
    FPendingBulbIPs: TStringList;  // written by discovery thread, flushed by timer
    FPendingLogs: TStringList;     // written by worker threads, flushed by timer
    procedure InsertBulb(const ANewBulb: TBulbInfo);
    procedure LogCommandResult(const AID: Integer; AResult, AError: TJSONData);
    procedure LogConnectionError(const AMsg: String);
    procedure BulbListTimerTimer(Sender: TObject);
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
  // shared structures first: discovery/error events may arrive as soon as
  // TYeeConn is created below
  FCS := TCriticalSection.Create;
  FPendingBulbIPs := TStringList.Create;
  FPendingLogs := TStringList.Create;

  FBulbMap := TBulbMap.Create;
  FBulbMap.Sorted := true;

  FYeeConn := TYeeConn.Create(ListenPort, BroadcastIntervalMillisecond);
  FYeeConn.OnBulbFound := @InsertBulb;
  FYeeConn.OnCommandResult := @LogCommandResult;
  FYeeConn.OnConnectionError := @LogConnectionError;

  // Flush worker-thread data to LCL controls on the main thread.
  BulbListTimer := TTimer.Create(Self);
  BulbListTimer.Interval := 500;
  BulbListTimer.OnTimer := @BulbListTimerTimer;
  BulbListTimer.Enabled := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BulbListTimer.Enabled := false;
  // Frees the connection first: it stops the worker threads, so the shared
  // structures below are no longer touched from other threads.
  FYeeConn.Free;
  FBulbMap.Free;
  FPendingBulbIPs.Free;
  FPendingLogs.Free;
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
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
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
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
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
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
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
    FPendingBulbIPs.Clear;
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
      FCS.Enter;
      try
        FSelectedBulb := FBulbMap[LBBulbList.GetSelectedText];
      finally
        FCS.Leave;
      end;
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
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
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
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
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
      ColorMode := cmCT;
    end;
    FCS.Enter;
    try
      FBulbMap[FSelectedBulb.IP] := FSelectedBulb;
    finally
      FCS.Leave;
    end;
  end;
end;

procedure TMainForm.InsertBulb(const ANewBulb: TBulbInfo);
begin
  // This event fires on the discovery thread. Only touch structures guarded
  // by FCS here; LCL controls are updated by BulbListTimer on the main thread.
  FCS.Enter;
  try
    FBulbMap[ANewBulb.IP] := ANewBulb;
    if FPendingBulbIPs.IndexOf(ANewBulb.IP) < 0 then
      FPendingBulbIPs.Add(ANewBulb.IP);
  finally
    FCS.Leave;
  end;

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
end;

procedure TMainForm.LogCommandResult(const AID: Integer; AResult,
  AError: TJSONData);
begin
  // Commands are always sent from the main thread, so direct UI access is safe.
  if Assigned(AResult) then MemoLog.Lines.Add('[Result] ' + AResult.AsJSON);
  if Assigned(AError) then MemoLog.Lines.Add('[Error] ' + AError.AsJSON);
end;

procedure TMainForm.LogConnectionError(const AMsg: String);
begin
  // This event can fire on a worker thread; defer the UI update to the timer.
  FCS.Enter;
  try
    FPendingLogs.Add('[Connection error] ' + AMsg);
  finally
    FCS.Leave;
  end;
end;

procedure TMainForm.BulbListTimerTimer(Sender: TObject);
var
  i: Integer;
begin
  FCS.Enter;
  try
    for i := 0 to FPendingBulbIPs.Count - 1 do
      if LBBulbList.Items.IndexOf(FPendingBulbIPs[i]) < 0 then
        LBBulbList.Items.Add(FPendingBulbIPs[i]);
    FPendingBulbIPs.Clear;
    for i := 0 to FPendingLogs.Count - 1 do
      MemoLog.Lines.Add(FPendingLogs[i]);
    FPendingLogs.Clear;
  finally
    FCS.Leave;
  end;
end;

end.

