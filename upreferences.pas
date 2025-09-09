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
    mdata: string;
    murl: string;
    muser: string;
    mpassword: string;
    mbootloader: integer;

    procedure SetData(datapath: string);
  public
  published
    property AppData: string read mdata write SetData;
    property URL: string read murl write murl;
    property Username: string read muser write muser;
    property Password: string read mpassword write mpassword;
    property Bootloader: integer read mbootloader write mbootloader;
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.lfm}

{ TfrmPreferences }

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  deDatafolder.Text := mdata;
  edtURL.Text := murl;
  edtUsername.Text := muser;
  edtPassword.Text := mpassword;
  PageControl1.ActivePageIndex := 0;
  rgBootloader.ItemIndex := mbootloader;
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
  mdata := deDatafolder.Text;
  murl := edtURL.Text;
  muser := edtUsername.Text;
  mpassword := edtPassword.Text;
  mbootloader := rgBootloader.ItemIndex;
end;

procedure TfrmPreferences.SetData(datapath: string);
begin
  if mdata = '' then
    mdata := GetAppConfigDir(True);
end;

end.
