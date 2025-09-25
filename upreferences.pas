unit uPreferences;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  EditBtn, StdCtrls, Buttons, MaskEdit, Spin;

type

  { TfrmPreferences }

  TfrmPreferences = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbDebug: TCheckBox;
    deDatafolder: TDirectoryEdit;
    edtPassword: TEditButton;
    edtURL: TEdit;
    edtUsername: TEdit;
    fneDebug: TFileNameEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rgBootloader: TRadioGroup;
    sedTilesMaaxAge: TSpinEdit;
    sedVesselID: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure cbDebugChange(Sender: TObject);
    procedure edtPasswordButtonClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FData: string;
    FURL: string;
    FUser: string;
    FPassword: string;
    FBootloader: integer;
    FTilesMaxAge: integer;
    FVesselID: integer;
    FDebug: boolean;
    FDebugFile: string;

    procedure SetData(datapath: string);
  public
  published
    property AppData: string read FData write SetData;
    property URL: string read FURL write FURL;
    property Username: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Bootloader: integer read FBootloader write FBootloader;
    property TilesMaxAge: integer read FTilesMaxAge write FTilesMaxAge;
    property VesselID: integer read FVesselID write FVesselID;
    property Debug: boolean read FDebug write FDebug;
    property DebugFile: string read FDebugFile write FDebugFile;
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.lfm}

uses LCLType;

  { TfrmPreferences }

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  deDatafolder.Text := FData;
  edtURL.Text := FURL;
  edtUsername.Text := FUser;
  edtPassword.Text := FPassword;
  PageControl1.ActivePageIndex := 0;
  rgBootloader.ItemIndex := FBootloader;
  sedTilesMaaxAge.Value := FTilesMaxAge;
  sedVesselID.Value := FVesselID;
  cbDebug.Checked := FDebug;
  fneDebug.Text := FDebugFile;
  cbDebugChange(Sender);
end;

procedure TfrmPreferences.edtPasswordButtonClick(Sender: TObject);
begin
  if edtPassword.PasswordChar = '#' then
    edtPassword.PasswordChar := char(0)
  else
    edtPassword.PasswordChar := '#';
end;

procedure TfrmPreferences.cbDebugChange(Sender: TObject);
begin
  fneDebug.Enabled := cbDebug.Checked;
end;

procedure TfrmPreferences.FormHide(Sender: TObject);
begin
  FData := deDatafolder.Text;
  FURL := edtURL.Text;
  FUser := edtUsername.Text;
  FPassword := edtPassword.Text;
  FBootloader := rgBootloader.ItemIndex;
  FTilesMaxAge := sedTilesMaaxAge.Value;
  FVesselID := sedVesselID.Value;
  FDebug := cbDebug.Checked;
  FDebugFile := fneDebug.Text;
end;

procedure TfrmPreferences.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TfrmPreferences.SetData(datapath: string);
begin
  FData := datapath;
  if FData = '' then
    FData := GetAppConfigDir(True);
end;

end.
