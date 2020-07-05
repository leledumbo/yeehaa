unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, PairSplitter, JSONPropStorage, syncobjs, fgl,
  fpjson, Yeehaa;

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
    GBOptions: TGroupBox;
    GBTransitionDuration: TGroupBox;
    GBLog: TGroupBox;
    ConfigStorage: TJSONPropStorage;
    LbBrightness: TLabel;
    LBBulbList: TListBox;
    LbModel: TLabel;
    LbName: TLabel;
    LbPoweredOn: TLabel;
    LbRGB: TLabel;
    MemoLog: TMemo;
    PMemoButtons: TPanel;
    PSBulbLog: TPairSplitter;
    PSBulbListProps: TPairSplitter;
    PSBulbPropsOpts: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    PairSplitterSide5: TPairSplitterSide;
    PairSplitterSide6: TPairSplitterSide;
    RGTransitionEffect: TRadioGroup;
    SpEdBrightness: TSpinEdit;
    SpEdTransitionDuration: TSpinEdit;
    procedure BClearClick(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
    procedure BSelectAllClick(Sender: TObject);
    procedure CBPoweredOnChange(Sender: TObject);
    procedure EdNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BRefreshClick(Sender: TObject);
    procedure LBBulbListSelectionChange(Sender: TObject; User: boolean);
  private
    FYeeConn: TYeeConn;
    FBulbMap: TBulbMap;
    FSelectedBulb: TBulbInfo;
    FCS: TCriticalSection;
    FAutomaticStateChange: Boolean;
    procedure InsertBulb(const ANewBulb: TBulbInfo);
    procedure LogCommandResult(const AID: Integer; AResult, AError: TJSONData);
    procedure LogConnectionError(const AMsg: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  ListenPort = 9999;
  BroadcastIntervalMillisecond = 200;

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
  LTransitionEffect: TTransitionEfect;
begin
  if (LBBulbList.ItemIndex >= 0) and not FAutomaticStateChange then begin
    case RGTransitionEffect.ItemIndex of
             0: LTransitionEffect := teSudden;
      otherwise LTransitionEffect := teSmooth;
    end;
    FYeeConn.SetPower(FSelectedBulb.IP,CBPoweredOn.Checked,LTransitionEffect,SpEdTransitionDuration.Value);
  end;
end;

procedure TMainForm.BSelectAllClick(Sender: TObject);
begin
  MemoLog.SelectAll;
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
  end;
end;

procedure TMainForm.BRefreshClick(Sender: TObject);
begin
  FCS.Enter;
  try
    FBulbMap.Clear;
    LBBulbList.Clear;
  finally
    FCS.Leave;
  end;
end;

procedure TMainForm.LBBulbListSelectionChange(Sender: TObject; User: boolean);
begin
  GBBulbProps.Enabled := true;
  try
    FAutomaticStateChange := true;
    try
      FSelectedBulb := FBulbMap[LBBulbList.GetSelectedText];
      EdModel.Text := FSelectedBulb.Model;
      CBPoweredOn.Checked := FSelectedBulb.PoweredOn;
      SpEdBrightness.Value := FSelectedBulb.BrightnessPercentage;
      CBColor.ButtonColor := FSelectedBulb.RGB;
      EdName.Text := FSelectedBulb.Name;
    finally
      FAutomaticStateChange := false;
    end;
  except
     on e: EListError do ; // intentionally ignored
  end;
end;

procedure TMainForm.InsertBulb(const ANewBulb: TBulbInfo);
begin
  FCS.Enter;
  try
    if FBulbMap.IndexOf(ANewBulb.IP) < 0 then
      LBBulbList.Items.Add(ANewBulb.IP);
    FBulbMap[ANewBulb.IP] := ANewBulb;
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

