unit uPreferences;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  EditBtn, StdCtrls, Buttons, MaskEdit;

type

  { TfrmPreferences }

  TfrmPreferences = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    deDatafolder: TDirectoryEdit;
    edtPassword: TEditButton;
    edtURL: TEdit;
    edtUsername: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rgBootloader: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure edtPasswordButtonClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FData: string;
    FURL: string;
    FUser: string;
    FPassword: string;
    FBootloader: integer;

    procedure SetData(datapath: string);
  public
  published
    property AppData: string read FData write SetData;
    property URL: string read FURL write FURL;
    property Username: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Bootloader: integer read FBootloader write FBootloader;
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.lfm}

{ TfrmPreferences }

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  deDatafolder.Text := FData;
  edtURL.Text := FURL;
  edtUsername.Text := FUser;
  edtPassword.Text := FPassword;
  PageControl1.ActivePageIndex := 0;
  rgBootloader.ItemIndex := FBootloader;
end;

procedure TfrmPreferences.edtPasswordButtonClick(Sender: TObject);
begin
  if edtPassword.PasswordChar = '#' then
    edtPassword.PasswordChar := char(0)
  else
    edtPassword.PasswordChar := '#';
end;

procedure TfrmPreferences.FormHide(Sender: TObject);
begin
  FData := deDatafolder.Text;
  FURL := edtURL.Text;
  FUser := edtUsername.Text;
  FPassword := edtPassword.Text;
  FBootloader := rgBootloader.ItemIndex;
end;

procedure TfrmPreferences.SetData(datapath: string);
begin
  FData := datapath;
  if FData = '' then
    FData := GetAppConfigDir(True);
end;

end.
