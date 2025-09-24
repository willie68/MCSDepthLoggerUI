unit uloggerconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, umcslogger;

type

  { TFrmLoggerConfig }

  TFrmLoggerConfig = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnDefault: TBitBtn;
    cbBaudB: TComboBox;
    cbSeatalk: TCheckBox;
    cbBaudA: TComboBox;
    cbGyro: TCheckBox;
    cbSupply: TCheckBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
    procedure btnDefaultClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FLoggerConfig: TLoggerConfig;
    function GetVesselID: integer;
    procedure SetVesselID(AValue: integer);
  public
    function LoggerCFG(): TLoggerConfig;
    procedure SetLoggerCFG(newcfg: TLoggerConfig);
  published
    property VesselID : integer read GetVesselID write SetVesselID;
  end;

function BaudToIndex(baud: integer): integer;
function IndexToBaud(index: integer): integer;

var
  frmLoggerConfig: TFrmLoggerConfig;

implementation

function BaudToIndex(baud: integer): integer;
begin
  case baud of
    1200: Result := 1;
    2400: Result := 2;
    4800: Result := 3;
    9600: Result := 4;
    19200: Result := 5;
    else
      Result := 0;
  end;
end;

function IndexToBaud(index: integer): integer;
begin
  case index of
    1: Result := 1200;
    2: Result := 2400;
    3: Result := 4800;
    4: Result := 9600;
    5: Result := 19200;
    else
      Result := 0;
  end;
end;

{$R *.lfm}

{ TForm3 }

{ TFrmLoggerConfig }

procedure TFrmLoggerConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    FLoggerConfig.BaudA:= IndexToBaud(cbBaudA.ItemIndex);
    FLoggerConfig.BaudB:= IndexToBaud(cbBaudB.ItemIndex);
    FLoggerConfig.SeaTalk:= cbSeatalk.Checked;
    FLoggerConfig.Gyro:= cbGyro.Checked;
    FLoggerConfig.Supply:= cbSupply.Checked;
    FLoggerConfig.VesselID := sedVesselID.Value;
  end;
end;

procedure TFrmLoggerConfig.btnDefaultClick(Sender: TObject);
begin
  cbBaudA.ItemIndex := 3;
  cbSeatalk.Checked := False;
  cbBaudB.ItemIndex := 3;
  cbGyro.Checked := True;
  cbSupply.Checked := False;
  sedVesselID.Value := FLoggerConfig.VesselID;
end;

procedure TFrmLoggerConfig.FormShow(Sender: TObject);
begin
  cbBaudA.ItemIndex:= BaudToIndex(FLoggerConfig.BaudA);
  cbBaudB.ItemIndex:= BaudToIndex(FLoggerConfig.BaudB);
  cbSeatalk.Checked := FLoggerConfig.SeaTalk;
  cbGyro.Checked := FLoggerConfig.Gyro;
  cbSupply.Checked := FLoggerConfig.Supply;
  sedVesselID.Value := FLoggerConfig.VesselID;
end;

function TFrmLoggerConfig.GetVesselID: integer;
begin
  result := FLoggerConfig.VesselID;
end;

procedure TFrmLoggerConfig.SetVesselID(AValue: integer);
begin
  FLoggerConfig.VesselID:= AValue;
end;

function TFrmLoggerConfig.LoggerCFG(): TLoggerConfig;
begin
  Result := FLoggerConfig;
end;

procedure TFrmLoggerConfig.SetLoggerCFG(newcfg: TLoggerConfig);
begin
  FLoggerConfig := newcfg;
end;

end.
