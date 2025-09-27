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
    cbFormat: TCheckBox;
    edLabel: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
    procedure btnDefaultClick(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FLoggerConfig: TLoggerConfig;
    FFormat: boolean;
    FSDLabel: string;
    function GetVesselID: integer;
    procedure SetVesselID(AValue: integer);
  public
    function LoggerCFG(): TLoggerConfig;
    procedure SetLoggerCFG(newcfg: TLoggerConfig);
  published
    property VesselID: integer read GetVesselID write SetVesselID;
    property Format: boolean read FFormat;
    property SDLabel: string read FSDLabel;
  end;

function BaudToIndex(baud: integer): integer;
function IndexToBaud(index: integer): integer;

var
  frmLoggerConfig: TFrmLoggerConfig;

implementation

{$R *.lfm}

uses LCLType;

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

{ TFrmLoggerConfig }

procedure TFrmLoggerConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    FLoggerConfig.BaudA := IndexToBaud(cbBaudA.ItemIndex);
    FLoggerConfig.BaudB := IndexToBaud(cbBaudB.ItemIndex);
    FLoggerConfig.SeaTalk := cbSeatalk.Checked;
    FLoggerConfig.Gyro := cbGyro.Checked;
    FLoggerConfig.Supply := cbSupply.Checked;
    FLoggerConfig.VesselID := sedVesselID.Value;
    FFormat := cbFormat.Checked;
    FSDLabel := edLabel.Text;
  end;
end;

procedure TFrmLoggerConfig.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFrmLoggerConfig.btnDefaultClick(Sender: TObject);
begin
  cbBaudA.ItemIndex := 3;
  cbSeatalk.Checked := False;
  cbBaudB.ItemIndex := 3;
  cbGyro.Checked := True;
  cbSupply.Checked := False;
  sedVesselID.Value := FLoggerConfig.VesselID;
  edLabel.Text := '';
  cbFormat.Checked := False;
end;

procedure TFrmLoggerConfig.cbFormatChange(Sender: TObject);
begin
  if cbFormat.Checked then
  begin
    if MessageDlg('Achtung', 'Mit diesem Schalter wird die SD Karte nue formatiert. Dadurch werden alle Daten gelöscht. Möchtest du das wirklich?', mtWarning, mbOKCancel, '' ) <> mrOK then
    begin
      cbFormat.Checked:= false;
    end;
  end;
end;

procedure TFrmLoggerConfig.FormShow(Sender: TObject);
begin
  cbBaudA.ItemIndex := BaudToIndex(FLoggerConfig.BaudA);
  cbBaudB.ItemIndex := BaudToIndex(FLoggerConfig.BaudB);
  cbSeatalk.Checked := FLoggerConfig.SeaTalk;
  cbGyro.Checked := FLoggerConfig.Gyro;
  cbSupply.Checked := FLoggerConfig.Supply;
  sedVesselID.Value := FLoggerConfig.VesselID;
  edLabel.Text := '';
  cbFormat.Checked := False;
end;

function TFrmLoggerConfig.GetVesselID: integer;
begin
  Result := FLoggerConfig.VesselID;
end;

procedure TFrmLoggerConfig.SetVesselID(AValue: integer);
begin
  FLoggerConfig.VesselID := AValue;
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
