unit ufrmexport;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn;

type

  { TfrmExport }

  TfrmExport = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    cbFormat: TComboBox;
    fnOutput: TFileNameEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StaticText1: TStaticText;
    procedure cbFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    FName: string;
    FFormat: string;
    function GetPath: string;
    procedure SetFilename(AValue: string);
  public
  published
    property Name: string read FName write SetFilename;
    property Path: string read GetPath;
    property Format: string read FFormat;

  end;

var
  frmExport: TfrmExport;

implementation

{$R *.lfm}

uses LCLType;

const
  FileExtensions: array of string = ('.gpx', '.geojson', '.kml', '.nmea', '.json');
  FileFormats: array of string = ('gpx', 'geojson', 'kml', 'nmea', 'json');

  { TfrmExport }

procedure TfrmExport.FormCreate(Sender: TObject);
begin
  StaticText1.Caption := FName;
  FFormat := FileFormats[0];
end;

procedure TfrmExport.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TfrmExport.SetFilename(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
  fnOutput.FileName := FName;
  StaticText1.Caption := FName;
end;

function TfrmExport.GetPath: string;
begin
  Result := fnOutput.FileName;
end;

procedure TfrmExport.cbFormatChange(Sender: TObject);
begin
  fnOutput.FilterIndex := cbFormat.ItemIndex + 1;
  fnOutput.FileName := ChangeFileExt(fnOutput.FileName,
    FileExtensions[cbFormat.ItemIndex]);
  FFormat := FileFormats[cbFormat.ItemIndex];

end;

end.
