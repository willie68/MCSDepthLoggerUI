unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, Buttons, mvMapViewer,
  mvPluginCommon, mvGeoNames, mvPlugins, Grids, ShellCtrls, mcslogger;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actAnalyse: TAction;
    actConfig: TAction;
    actImport: TAction;
    actAdd2Track: TAction;
    actExport: TAction;
    actAbout: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    actTrackEdit: TAction;
    actTrackDelete: TAction;
    actTracksReload: TAction;
    actNewTrack: TAction;
    actTimestamp: TAction;
    actMap: TAction;
    actSDManagement: TAction;
    actPreferences: TAction;
    actUpdate: TAction;
    actUpload: TAction;
    ActionList1: TActionList;
    btnAnalyse: TBitBtn;
    cbAutoAnalyse: TCheckBox;
    cbRootDrives: TComboBox;
    actExit: TFileExit;
    cbProvider: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    actHelp: THelpAction;
    ImageList: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCardInfo: TLabel;
    lblFileInfo: TLabel;
    MainMenu1: TMainMenu;
    MapView1: TMapView;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MVGeoNames1: TMVGeoNames;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    Panel1: TPanel;
    pMapBottom: TPanel;
    pMapClient: TPanel;
    pCenter: TPanel;
    pLeft: TPanel;
    pRight: TPanel;
    pTop: TPanel;
    pBottom: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    slvTracks: TShellListView;
    Splitter1: TSplitter;
    stvTracks: TShellTreeView;
    spHorizontal1: TSplitter;
    spLEft: TSplitter;
    spRight: TSplitter;
    spHorizontal: TSplitter;
    StatusBar: TStatusBar;
    sgFiles: TStringGrid;
    Timer1: TTimer;
    tbMain: TToolBar;
    tbTracks: TToolBar;
    tbMap: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    tbtnZoomIn: TToolButton;
    tbtnZoomOut: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TrayIcon1: TTrayIcon;
    procedure actAboutExecute(Sender: TObject);
    procedure actAdd2TrackExecute(Sender: TObject);
    procedure actAnalyseExecute(Sender: TObject);
    procedure actConfigExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actMapExecute(Sender: TObject);
    procedure actNewTrackExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actSDManagementExecute(Sender: TObject);
    procedure actTimestampExecute(Sender: TObject);
    procedure actTrackDeleteExecute(Sender: TObject);
    procedure actTrackEditExecute(Sender: TObject);
    procedure actTracksReloadExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure actUploadExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure cbProviderChange(Sender: TObject);
    procedure cbRootDrivesChange(Sender: TObject);
    procedure cbRootDrivesGetItems(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure JSONPropStorage1RestoringProperties(Sender: TObject);
    procedure JSONPropStorage1SavingProperties(Sender: TObject);
    procedure sgFilesSelection(Sender: TObject; aCol, aRow: integer);
    procedure tbMainResize(Sender: TObject);
    procedure tbMapResize(Sender: TObject);
    procedure tbTracksResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    init: boolean;
    procedure SetMapProvider();
    procedure RefreshRootDrives();
    procedure PopulateFilesGrid();
    procedure AnalyseFile(filename: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses fileinfo, uPreferences, uloggerconfig, MCSAbout, ufsinfo,
  LazStringUtils, mvTypes, mvGPSObj, mvEngine, uconst, usdcardimages, uwait;
  {$R *.lfm}

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  configDir: string;
  Title: string;
  FileVerInfo: TFileVersionInfo;
  Map: string;
  provider: TStringList;
  mapname: string;
  i: integer;
begin
  init := False;
  configDir := GetAppConfigDir(False);
  if not DirectoryExists(configDir) then
  begin
    MkDir(configDir);
  end;
  configDir := ConcatPaths([configDir, 'config.json']);
  JSONPropStorage1.JSONFileName := configDir;
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Title := 'MCS Logger UI V' + FileVerInfo.VersionStrings.Values['FileVersion'];
    frmMain.Caption := Title;
  finally
    FileVerInfo.Free;
  end;

  MapView1.Align := alClient;
  provider := TStringList.Create();
  MapView1.GetMapProviders(provider);
  cbProvider.Items.Clear();
  for i := 0 to Provider.Count - 1 do
  begin
    mapName := Provider.Strings[i];
    if LazStartsStr('OpenStreet', mapName) or LazStartsStr('Google Maps', mapName) then
      cbProvider.Items.Add(mapName);
  end;
  map := Provider.Strings[0];

  MapView1.MapProvider := map;
  Provider.Free();

  CreateMCSLogger();
end;

procedure TfrmMain.cbRootDrivesGetItems(Sender: TObject);
begin
  RefreshRootDrives();
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  MapView1.MapProvider := cbProvider.Text;
  if not (cbProvider.Text = '') then
    MapView1.Active := True;
end;

procedure TfrmMain.cbProviderChange(Sender: TObject);
begin
  SetMapProvider();
end;

procedure TfrmMain.SetMapProvider();
var
  mapName: string;
  legal: string;
begin
  MapView1.Active := False;
  mapName := cbProvider.Text;
  MapView1.MapProvider := mapName;
  if not (mapName = '') then
  begin
    legal := '';
    if LazStartsStr('OpenStreet', mapName) then
    begin
      legal :=
        'Map data from [https://www.openstreetmap.org/copyright OpenStreetMap and contributors]';
    end;
    if LazStartsStr('Google Maps', mapName) then
    begin
      legal := 'Map data from [https://www.google.com/help/legalnotices_maps/ Google Maps]';
    end;
    MvPluginManager1LegalNoticePlugin1.LegalNotice := legal;
    MapView1.Active := True;
    MapView1.Center := MvGeoNames1.Search('Germany', MapView1.DownloadEngine);
    MapView1.Zoom := 7;
  end;
end;

procedure TfrmMain.RefreshRootDrives();
var
  fileCollector: TFileSystemInfoCollector;
  fsInfo: TFileSystemInfo;
begin
  fileCollector := TFileSystemInfoCollector.Create();
  try
    fileCollector.Refresh;
    cbRootDrives.Items.Clear;
    for fsInfo in fileCollector.FileInfos do
    begin
      if fsInfo.DriveType = 2 then
        cbRootDrives.AddItem(fsInfo.Name + ' (' + fsInfo.DeviceID + ') ', fsInfo);
    end;
  finally
    fileCollector.Free;
  end;
end;

procedure TfrmMain.PopulateFilesGrid();
var
  i: integer;
  dfa: TLoggerDataFileArray;
  df: TLoggerDataFile;
begin
  sgFiles.Clean;
  sgFiles.RowCount := HWLogger.DataFileCount + 1;
  dfa := HWLogger.DataFiles;
  for i := 1 to length(dfa) do
  begin
    df := dfa[i - 1];
    sgFiles.Cells[0, i] := df.Filename;
    sgFiles.Cells[1, i] := FormatBytes(df.Size);
    sgFiles.Cells[2, i] := FormatDateTime('yyyy-mm-dd hh:mm:ss', df.Date);
  end;
end;

procedure TfrmMain.AnalyseFile(filename: string);
var
  res: TLoggerCheckResult;
  pc: double;
begin
  res := HWLogger.Check(filename);
  if res.DatagrammCount > 0 then
    pc := res.ErrorCount / res.DatagrammCount * 100.0;

  lblFileInfo.Caption := 'Dateiname: ' + res.Filename + LineEnding +
    'Dateigröße: ' + FormatBytes(res.Size) + LineEnding + 'Datagramme: ' +
    IntToStr(res.DatagrammCount) + LineEnding + 'Fehler: ' +
    IntToStr(res.ErrorCount) + ' (' + Format('%.1f', [pc]) + '%)' +
    LineEnding + 'Version: ' + res.Version + LineEnding + 'Zeitstempel' +
    LineEnding + 'von: ' + FormatDateTime('yyyy-mm-dd hh:mm:ss', res.FirstTimeStamp) +
    LineEnding + 'bis: ' + FormatDateTime('yyyy-mm-dd hh:mm:ss', res.LastTimeStamp);
end;

procedure TfrmMain.cbRootDrivesChange(Sender: TObject);
var
  fsInfo: TFileSystemInfo;
begin
  if cbRootDrives.ItemIndex >= 0 then
  begin
    fsInfo := cbRootDrives.Items.Objects[cbRootDrives.ItemIndex] as TFileSystemInfo;
    HWLogger.SDRoot := fsInfo.DeviceID;
    lblCardInfo.Caption := 'Name: ' + cbRootDrives.Text + LineEnding +
      'Gesamtspeicher: ' + FormatBytes(fsInfo.Size) + LineEnding +
      'Freier Speicher: ' + FormatBytes(fsInfo.FreeSpace) + LineEnding +
      'Dateisystem: ' + fsInfo.FileSystem + LineEnding +
      'Anzahl der Datendateien: ' + IntToStr(HWLogger.DataFileCount);
    PopulateFilesGrid();
  end;
end;

procedure TfrmMain.actPreferencesExecute(Sender: TObject);
var
  appData: string;
  mr: integer;
begin
  frmPreferences.AppData := ConfigPathes.Appdata;
  frmPreferences.URL := JSONPropStorage1.ReadString('upload.url', '');
  frmPreferences.Username := JSONPropStorage1.ReadString('upload.username', '');
  frmPreferences.Password := JSONPropStorage1.ReadString('upload.password', '');
  frmPreferences.Bootloader := JSONPropStorage1.ReadInteger('logger.bootloader', 1);

  mr := frmPreferences.ShowModal();
  if mr = mrOk then
  begin
    JSONPropStorage1.WriteString('track.storepath', frmPreferences.AppData);
    ConfigPathes.SetRoot(frmPreferences.AppData);
    JSONPropStorage1.WriteString('upload.url', frmPreferences.URL);
    JSONPropStorage1.WriteString('upload.username', frmPreferences.Username);
    JSONPropStorage1.WriteString('upload.password', frmPreferences.Password);
    JSONPropStorage1.WriteInteger('logger.bootloader', frmPreferences.Bootloader);
    JSONPropStorage1RestoringProperties(Sender);
  end;
end;

procedure TfrmMain.actSDManagementExecute(Sender: TObject);
begin
  frmSDCard.ShowModal;
end;

procedure TfrmMain.actTimestampExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actTrackDeleteExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actTrackEditExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actTracksReloadExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actUpdateExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actUploadExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actConfigExecute(Sender: TObject);
var
  mr: integer;
begin
  frmLoggerConfig.SetLoggerCFG(HWLogger.LoggerCFG());
  mr := frmLoggerConfig.ShowModal();
  if mr = mrOk then
  begin
    HWLogger.SetLoggerCFG(frmLoggerConfig.LoggerCFG());
    HWLogger.Write();
  end;
end;

procedure TfrmMain.actExportExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actImportExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actMapExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actNewTrackExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  Infobox.ShowModal;
end;

procedure TfrmMain.actAdd2TrackExecute(Sender: TObject);
begin
  ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actAnalyseExecute(Sender: TObject);
var
  fn: string;
begin
  if sgFiles.Row >= 1 then
  begin
    fn := sgFiles.Cells[0, sgFiles.Row];
    AnalyseFile(fn);
  end;
end;


procedure TfrmMain.acZoomInExecute(Sender: TObject);
begin
  MapView1.Zoom := MapView1.Zoom + 1;
end;

procedure TfrmMain.acZoomOutExecute(Sender: TObject);
begin
  MapView1.Zoom := MapView1.Zoom - 1;
end;

procedure TfrmMain.actHelpExecute(Sender: TObject);
begin
  frmWait.Show;
  ShowMessage('Keine Hilfe vorhanden');
  frmWait.Hide;
end;

procedure TfrmMain.JSONPropStorage1RestoringProperties(Sender: TObject);
var
  appData: string;
  mapProvider: string;
  i: integer;
begin
  for i := 0 to sgFiles.ColCount - 1 do
  begin
    sgFiles.ColWidths[i] := JSONPropStorage1.ReadInteger(
      'root.sgFiles.ColWidth.' + IntToStr(i), 100);
  end;
  mapProvider := JSONPropStorage1.ReadString('map.mapprovider', '');
  MapView1.MapProvider := mapProvider;
  cbProvider.Text := mapProvider;
  CreateConfigPathes(JSONPropStorage1.ReadString('track.storepath',
    GetAppConfigDir(True)));

  MapView1.CacheLocation := clCustom;
  MapView1.CachePath := ConfigPathes.Tilescache;
  MapView1.CacheOnDisk := True;

  stvTracks.Root:= ConfigPathes.Trackspath +'\';
  SetMapProvider();
end;

procedure TfrmMain.JSONPropStorage1SavingProperties(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to sgFiles.ColCount - 1 do
  begin
    JSONPropStorage1.WriteInteger('root.sgFiles.ColWidth.' + IntToStr(i),
      sgFiles.ColWidths[i]);
  end;
  JSONPropStorage1.WriteString('map.mapprovider', MapView1.MapProvider);
end;

procedure TfrmMain.sgFilesSelection(Sender: TObject; aCol, aRow: integer);
begin
  if cbAutoAnalyse.Checked then
    AnalyseFile(sgFiles.Cells[0, sgFiles.Row]);
  actAnalyse.Enabled := True;
end;

procedure TfrmMain.tbMainResize(Sender: TObject);
var
  w: longint;
  i: integer;
begin
  w := tbMain.ClientWidth - 1;
  for i := 0 to tbMain.ButtonCount - 1 do
  begin
    w := w - tbMain.Buttons[i].Width;
  end;
  Panel7.Width := w;
end;

procedure TfrmMain.tbMapResize(Sender: TObject);
var
  w: longint;
  i: integer;
begin
  w := tbMap.ClientWidth - cbProvider.ClientWidth - 16;
  for i := 0 to tbMap.ButtonCount - 1 do
  begin
    w := w - tbMap.Buttons[i].Width;
  end;
  Panel1.Width := w;
end;

procedure TfrmMain.tbTracksResize(Sender: TObject);
var
  w: longint;
  i: integer;
begin
  w := tbTracks.ClientWidth - Label3.Width - 24;
  for i := 0 to tbTracks.ButtonCount - 1 do
  begin
    w := w - tbTracks.Buttons[i].Width;
  end;
  Panel6.Width := w;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if not init then
  begin
    RefreshRootDrives();
    if (cbRootDrives.Items.Count > 0) and (cbRootDrives.ItemIndex < 0) then
    begin
      cbRootDrives.ItemIndex := 0;
    end;
    cbRootDrivesChange(Sender);
    init := True;
  end;
end;

end.
