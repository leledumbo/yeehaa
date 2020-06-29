unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, syncobjs, fgl, Yeehaa;

type

  TBulbMap = specialize TFPGMap<String,TBulbInfo>;

  { TMainForm }

  TMainForm = class(TForm)
    GBBulbList: TGroupBox;
    LBBulbList: TListBox;
    GBBulbProps: TGroupBox;
    BClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BClearClick(Sender: TObject);
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

