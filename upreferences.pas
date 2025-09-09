unit uPreferences;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  EditBtn, StdCtrls, Buttons;

type

  { TfrmPreferences }

  TfrmPreferences = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    deDatafolder: TDirectoryEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    TabSheet1: TTabSheet;
    procedure FormShow(Sender: TObject);
  private
    data : string;
    procedure SetData(datapath: string);
  public
  published
    property AppData : string read data write SetData;

  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.lfm}

{ TfrmPreferences }

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  deDatafolder.Text := data;
end;

procedure TfrmPreferences.SetData(datapath : string);
begin
  if data = '' then
    data := GetAppConfigDir(True);
end;

end.

