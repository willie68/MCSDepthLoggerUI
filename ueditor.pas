unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, Menus, SynEdit, SynPopupMenu, SynHighlighterAny,
  umcslogger;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    btnCancel: TBitBtn;
    btnDefault: TBitBtn;
    btnOK: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    StaticText1: TStaticText;
    edFile: TSynEdit;
    sPopup: TSynPopupMenu;
    procedure btnDefaultClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFilename: string;
    FChanges : boolean;
    procedure SetFilename(AValue: string);
  public

  published
    property Filename: string read FFilename write SetFilename;
  end;

var
  frmEditor: TfrmEditor;

implementation

{$R *.lfm}

{ TfrmEditor }

procedure TfrmEditor.FormShow(Sender: TObject);
begin
  StaticText1.Caption := FFilename;
  edFile.Lines.LoadFromFile(FFilename);
  FChanges:=False;
end;

procedure TfrmEditor.btnOKClick(Sender: TObject);
begin
  if FChanges then edFile.Lines.SaveToFile(FFilename);
end;

procedure TfrmEditor.btnDefaultClick(Sender: TObject);
begin
  edFile.Lines.LoadFromFile(FFilename);
  FChanges:=False;
end;

procedure TfrmEditor.edFileChange(Sender: TObject);
begin
  FChanges := true;
end;

procedure TfrmEditor.SetFilename(AValue: string);
begin
  if FFilename = AValue then Exit;
  FFilename := AValue;
end;

end.
