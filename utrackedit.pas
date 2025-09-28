unit utrackedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Spin, Grids, umcslogger;

type

  { TfrmTrackEdit }

  TfrmTrackEdit = class(TForm)
    btnClose: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbGroup: TComboBox;
    edName: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    mmDescription: TMemo;
    pcTracks: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sedVesselID: TSpinEdit;
    sgFiles: TStringGrid;
    tsGeneral: TTabSheet;
    tsFiles: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FEdit: boolean;
    FReadOnly: boolean;
    FRoot: string;
    FTrackInfo: TLoggerTrackInfo;
    FGroupPath: string;
    function GetGrouppath: string;
    function GetVesselID: integer;
    procedure PopulateFilesGrid();
    procedure SetEdit(AValue: boolean);
    procedure SetReadOnly(AValue: boolean);
    procedure SetRoot(root: string);
    function GetName: string;
  public
    function GetTrackinfo: TLoggerTrackInfo;
    procedure SetTrackInfo(AValue: TLoggerTrackInfo);
  published
    property Root: string read FRoot write SetRoot;
    property Trackname: string read GetName;
    property VesselID: integer read FTrackInfo.VesselID write FTrackInfo.VesselID;
    property Grouppath: string read GetGrouppath write FGroupPath;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property Edit: boolean read FEdit write SetEdit;
    //    property TrackInfo: TLoggerTrackInfo read GetTrackInfo write SetTrackInfo;
  end;

procedure GetSubdirectories(const RootDir, Name: string; List: TStrings);

var
  frmTrackEdit: TfrmTrackEdit;

implementation

{$R *.lfm}

uses LCLType, ufsinfo;

  { TfrmTrackEdit }

procedure TfrmTrackEdit.FormHide(Sender: TObject);
begin
  FTrackInfo.Name := edName.Text;
  FTrackInfo.Description := mmDescription.Text;
  FTrackInfo.VesselID := sedVesselID.Value;
end;

procedure TfrmTrackEdit.FormCreate(Sender: TObject);
begin
  btnClose.Top := btnCancel.Top;
  btnClose.Left := btnCancel.Left;
  btnClose.Visible := False;
end;

procedure TfrmTrackEdit.FormDestroy(Sender: TObject);
begin
  Finalize(FTrackInfo);
end;

procedure TfrmTrackEdit.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TfrmTrackEdit.FormShow(Sender: TObject);
begin
  pcTracks.ActivePage := tsGeneral;

  btnClose.Visible := FReadOnly;

  btnCancel.Visible := not FReadOnly;
  btnOK.Visible := btnCancel.Visible;

  sedVesselID.ReadOnly := FReadOnly;
  cbGroup.ReadOnly := FReadOnly or FEdit;
  edName.ReadOnly := FReadOnly or FEdit;
  mmDescription.ReadOnly := FReadOnly;

  cbGroup.Text := FGroupPath;
  sedVesselID.Value := FTrackInfo.VesselID;
  edName.Text := FTrackInfo.Name;
  mmDescription.Text := FTrackInfo.Description;

  PopulateFilesGrid();
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

procedure TfrmTrackEdit.SetTrackInfo(AValue: TLoggerTrackInfo);
begin
  FTrackInfo := AValue;
end;

function StringsToRawByteArray(List: TStrings): TStringArray;
var
  i: integer;
begin
  SetLength(Result, List.Count);
  for i := 0 to List.Count - 1 do
    Result[i] := rawbytestring(List[i]); // Explicit conversion
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

function TfrmTrackEdit.GetVesselID: integer;
begin
  Result := FTrackInfo.VesselID;
end;

procedure TfrmTrackEdit.SetReadOnly(AValue: boolean);
begin
  if FReadOnly = AValue then Exit;
  FReadOnly := AValue;
end;

function TfrmTrackEdit.GetTrackinfo: TLoggerTrackInfo;
begin
  Result := FTrackInfo;
end;

procedure TfrmTrackEdit.PopulateFilesGrid();
var
  i: integer;
  df: TLoggerDataFile;
begin
  sgFiles.Clean;
  sgFiles.RowCount := length(FTrackInfo.Files) + 1;
  for i := 1 to length(FTrackInfo.Files) do
  begin
    df := FTrackInfo.Files[i - 1];
    sgFiles.Cells[0, i] := df.Filename;
    sgFiles.Cells[1, i] := FormatBytes(df.Size);
    sgFiles.Cells[2, i] := FormatDateTime('yyyy-mm-dd hh:mm:ss', df.Date);
  end;
end;

procedure TfrmTrackEdit.SetEdit(AValue: boolean);
begin
  if FEdit = AValue then Exit;
  FEdit := AValue;
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
