unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, syncobjs, fgl, Yeehaa;

type

  TBulbMap = specialize TFPGMap<String,TBulbInfo>;

  { TMainForm }

  TMainForm = class(TForm)
    CBPoweredOn: TCheckBox;
    CBColor: TColorButton;
    EdModel: TEdit;
    GBBulbList: TGroupBox;
    LbModel: TLabel;
    LbPoweredOn: TLabel;
    LbBrightness: TLabel;
    LbRGB: TLabel;
    LBBulbList: TListBox;
    GBBulbProps: TGroupBox;
    BClear: TButton;
    SEBrightness: TSpinEdit;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure LBBulbListSelectionChange(Sender: TObject; User: boolean);
  private
    FYeeConn: TYeeConn;
    FBulbMap: TBulbMap;
    FCS: TCriticalSection;
    procedure InsertBulb(const ANewBulb: TBulbInfo);
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

  FCS := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FYeeConn.Free;
  FBulbMap.Free;
  FCS.Free;
end;

procedure TMainForm.BClearClick(Sender: TObject);
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
var
  LBulb: TBulbInfo;
begin
  try
    LBulb := FBulbMap[LBBulbList.GetSelectedText];
    EdModel.Text := LBulb.Model;
    CBPoweredOn.Checked := LBulb.Power = 'on';
    SEBrightness.Value := StrToIntDef(LBulb.Brightness,0);
    CBColor.ButtonColor := TColor(StrToIntDef(LBulb.RGB,0));
  except
     on e: EListError do ; // intentionally ignored
  end;
end;

procedure TMainForm.InsertBulb(const ANewBulb: TBulbInfo);
begin
  if FBulbMap.IndexOf(ANewBulb.IP) < 0 then begin
    FCS.Enter;
    try
      FBulbMap[ANewBulb.IP] := ANewBulb;
      LBBulbList.Items.Add(ANewBulb.IP);
    finally
      FCS.Leave;
    end;
  end;
end;

end.

