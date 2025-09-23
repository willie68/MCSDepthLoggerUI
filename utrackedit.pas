unit utrackedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, umcslogger;

type

  { TfrmTrackEdit }

  TfrmTrackEdit = class(TForm)
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    cbGroup: TComboBox;
    edName: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmDescription: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRoot: string;
    FVesselID: integer;
    function GetGrouppath: string;
    procedure SetRoot(root: string);
    function GetName:string;
  public
    function GetTrackinfo: TLoggerTrackInfo;
  published
    property Root: string read FRoot write SetRoot;
    property Trackname: string read GetName;
    property VesselID: integer read FVesselID write FVesselID;
    property Grouppath: string read GetGrouppath;
  end;

procedure GetSubdirectories(const RootDir, Name: string; List: TStrings);

var
  frmTrackEdit: TfrmTrackEdit;

implementation

{$R *.lfm}

{ TfrmTrackEdit }

procedure TfrmTrackEdit.FormHide(Sender: TObject);
begin
  FVesselID := sedVesselID.Value;
end;

procedure TfrmTrackEdit.FormShow(Sender: TObject);
begin
  sedVesselID.Value := FVesselID;
end;

procedure TfrmTrackEdit.SetRoot(root: string);
begin
  FRoot := root;
  cbGroup.Items.Add('/');
  GetSubdirectories(FRoot, '', cbGroup.Items);
end;

function TfrmTrackEdit.GetName: string;
begin
  Result := edName.Text;
end;

function StringsToRawByteArray(List: TStrings): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, List.Count);
  for i := 0 to List.Count - 1 do
    Result[i] := RawByteString(List[i]); // Explicit conversion
end;

function TfrmTrackEdit.GetGrouppath: string;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := cbGroup.Text;

    Result := ConcatPaths(StringsToRawByteArray(Parts));
  finally
    Parts.Free;
  end;
end;

function TfrmTrackEdit.GetTrackinfo: TLoggerTrackInfo;
begin
  Result.Name:=edName.Text;
  Result.Description:=mmDescription.Text;
  Result.VesselID:=sedVesselID.Value;
end;

procedure GetSubdirectories(const RootDir, Name: string; List: TStrings);
var
  SearchRec: TSearchRec;
  SubDir, RelPath: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(RootDir) + '*', faDirectory,
    SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
        begin
          SubDir := IncludeTrailingPathDelimiter(RootDir) + SearchRec.Name;
          // Relativen Pfad berechnen und \ durch / ersetzen
          RelPath := Name + '/' + SearchRec.Name;
          List.Add(RelPath);
          GetSubdirectories(SubDir, RelPath, List); // Rekursiver Aufruf
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

end.
