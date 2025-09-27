unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, Buttons, mvMapViewer,
  mvPluginCommon, mvPlugins, Grids, ShellCtrls, CheckLst,
  LazHelpHTML, TAGraph, TASeries, TAIntervalSources, umcslogger, mvTypes,
  mvGPSObj, ugomapproxy, utilecacheutils, LazLogger, MCSDBGLog, ueditor, ufrmexport;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actAnalyse: TAction;
    actConfig: TAction;
    actAdd2Track: TAction;
    actExport: TAction;
    actAbout: TAction;
    actEdit: TAction;
    actMapZoomArea: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    actTrackEdit: TAction;
    actTrackDelete: TAction;
    actTracksReload: TAction;
    actNewTrack: TAction;
    actTimestamp: TAction;
    actMapShow: TAction;
    actSDManagement: TAction;
    actPreferences: TAction;
    actUpload: TAction;
    ActionList1: TActionList;
    cbAutoAnalyse: TCheckBox;
    cbAutoMap: TCheckBox;
    cbRootDrives: TComboBox;
    actExit: TFileExit;
    cbProvider: TComboBox;
    Chart1: TChart;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    Depth: TLineSeries;
    CheckListBox1: TCheckListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    actHelp: THelpAction;
    HTMLBrowserHelpViewer1: THTMLBrowserHelpViewer;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    ImageList: TImageList;
    ilStatus: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblCardInfo: TLabel;
    lblFileInfo: TLabel;
    MainMenu1: TMainMenu;
    MapView1: TMapView;
    MenuItem1: TMenuItem;
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
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    Panel1: TPanel;
    Panel2: TPanel;
    pMapBottom: TPanel;
    pMapClient: TPanel;
    pCenter: TPanel;
    pLeft: TPanel;
    pRight: TPanel;
    pTop: TPanel;
    pBottom: TPanel;
    Panel7: TPanel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    spHorizontal1: TSplitter;
    stvTracks: TShellTreeView;
    spLEft: TSplitter;
    spRight: TSplitter;
    spHorizontal: TSplitter;
    sbMain: TStatusBar;
    sgFiles: TStringGrid;
    timAfterStart: TTimer;
    timRefreshRoot: TTimer;
    tbMain: TToolBar;
    tbMap: TToolBar;
    timStatusbar: TTimer;
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
    tbtnZoomIn: TToolButton;
    tbtnZoomOut: TToolButton;
    tbMapsSeamarks: TToolButton;
    tbMapHabour: TToolButton;
    tbMapDepth: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton3: TToolButton;
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
    procedure actEditExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actMapShowExecute(Sender: TObject);
    procedure actMapZoomAreaExecute(Sender: TObject);
    procedure actNewTrackExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actSDManagementExecute(Sender: TObject);
    procedure actTimestampExecute(Sender: TObject);
    procedure actTrackDeleteExecute(Sender: TObject);
    procedure actTrackEditExecute(Sender: TObject);
    procedure actTracksReloadExecute(Sender: TObject);
    procedure actUploadExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure cbProviderChange(Sender: TObject);
    procedure cbRootDrivesChange(Sender: TObject);
    procedure cbRootDrivesGetItems(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JSONPropStorage1RestoringProperties(Sender: TObject);
    procedure JSONPropStorage1SavingProperties(Sender: TObject);
    procedure sgFilesClick(Sender: TObject);
    procedure sgFilesSelection(Sender: TObject; aCol, aRow: integer);
    procedure sbMainDrawPanel(Statusbar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure sbMainResize(Sender: TObject);
    procedure spHorizontal1Moved(Sender: TObject);
    procedure stvTracksClick(Sender: TObject);
    procedure stvTracksSelectionChanged(Sender: TObject);
    procedure timAfterStartTimer(Sender: TObject);
    procedure timStatusbarTimer(Sender: TObject);
    procedure stvTracksAddItem(Sender: TObject; const ABasePath: string;
      const AFileInfo: TSearchRec; var CanAdd: boolean);
    procedure tbMainResize(Sender: TObject);
    procedure tbMapResize(Sender: TObject);
    procedure tbMapHabourClick(Sender: TObject);
    procedure timRefreshRootTimer(Sender: TObject);
    procedure tbMapsSeamarksClick(Sender: TObject);
    procedure tbMapDepthClick(Sender: TObject);
  private
    FLog: TLogger;

    FInit: boolean;
    FTrackSelected: boolean;
    FArea: TRealArea;
    FAreaSelected: boolean;
    FMapProxy: TExecProxy;
    FDepthlayer: TMapLayer;
    FSeamarkslayer: TMapLayer;
    FHabourlayer: TMapLayer;
    FTrackLayer: TMapLayer;
    FCleanupThread: TFileCleanupThread;

    procedure SetMapProvider();
    procedure RefreshRootDrives();
    procedure PopulateFilesGrid();
    procedure ShowTrackOnMap(ltrack: TLoggerTrack);
    procedure PopulateLayers();
    procedure StatusMsg(line: string);
    procedure StartApplication();
    procedure StopApplication();
    procedure ShowDataFileOnMap();
    procedure ShowTrackFileOnMap();
    procedure ClearTrack();
    procedure SyncFrmTrack();
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses fileinfo, uPreferences, uloggerconfig, MCSAbout, ufsinfo, LazStringUtils,
  uconst, usdcardimages, mvDrawingEngine, mvMapProvider, uwait, utrackedit,
  LCLIntf, LazFileUtils;
  {$R *.lfm}

type

  { TAnalyseThread }
  TAnalyseThread = class(TThread)
  private
    FFilename: string;
    FResult: TLoggerCheckResult;
    FOutput: string;

  protected
    procedure Execute; override;
    procedure showLabel();
  public
    constructor Create(filename: string);
  end;

  { TAnalyseThread }

procedure TAnalyseThread.Execute;
var
  pc: double;
begin
  FResult := HWLogger.Check(FFilename);
  if FResult.DatagrammCount > 0 then
    pc := FResult.ErrorCount / FResult.DatagrammCount * 100.0;
  FOutput := 'Dateiname: ' + FResult.Filename + LineEnding +
    'Dateigröße: ' + FormatBytes(FResult.Size) + LineEnding +
    'Datagramme: ' + IntToStr(FResult.DatagrammCount) + LineEnding +
    'Fehler: ' + IntToStr(FResult.ErrorCount) + ' (' + Format('%.1f', [pc]) +
    '% A:' + IntToStr(FResult.ErrorA) + ', B:' + IntToStr(FResult.ErrorB) +
    ', I:' + IntToStr(FResult.ErrorI) + ')' + LineEnding + 'Version: ' +
    FResult.Version + LineEnding + 'Zeitstempel' + LineEnding + 'von: ' +
    FormatDateTime('yyyy-mm-dd hh:mm:ss', FResult.FirstTimeStamp) +
    LineEnding + 'bis: ' + FormatDateTime('yyyy-mm-dd hh:mm:ss', FResult.LastTimeStamp);

  Synchronize(@ShowLabel);
end;

procedure TAnalyseThread.showLabel();
begin
  frmMain.lblFileInfo.Caption := FOutput;
end;

constructor TAnalyseThread.Create(filename: string);
begin
  FFilename := filename;
  inherited Create(False);
end;

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
  DebugLogger.CloseLogFileBetweenWrites := True;
  FInit := False;
  FAreaSelected := False;
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

  FMapProxy := TExecProxy.Create(ExtractFilePath(Application.ExeName) + 'config.yaml');

  MapView1.Align := alClient;
  PopulateLayers();
  provider := TStringList.Create();
  MapProvidersToSortedStrings(provider);
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
  FTrackSelected := False;
  FLog := TLogger.Create('main');
  FLog.Info(Title + ' started');
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
    actMapZoomAreaExecute(nil);
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
        cbRootDrives.AddItem(fsInfo.Name + ' (' + fsInfo.DeviceID +
          ') ', fsInfo.Clone());
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

procedure TfrmMain.ShowTrackOnMap(ltrack: TLoggerTrack);
var
  track: TMapTrack;
  rp, rpStart, rpEnd: TRealPoint;
  i: integer;
  way: TLoggerWaypoint;
begin
  FTrackLayer.Visible := False;
  FTrackLayer.PointsOfInterest.Clear;
  FTrackLayer.Tracks.Clear;
  Depth.Clear;

  if ltrack.Start.Active then
  begin
    rpStart.InitXY(ltrack.Start.Longitude, ltrack.Start.Latitude);
    FTrackLayer.AddPointOfInterest(rpStart, ltrack.Start.Name).ImageIndex := 27;
  end;

  if ltrack.Finish.Active then
  begin
    rpEnd.InitXY(ltrack.Finish.Longitude, ltrack.Finish.Latitude);
    FTrackLayer.AddPointOfInterest(rpEnd, ltrack.Finish.Name).ImageIndex := 28;
  end;

  if length(ltrack.Waypoints) > 0 then
  begin
    track := FTrackLayer.Tracks.Add as TMapTrack;
    for i := 0 to length(ltrack.Waypoints) - 1 do
    begin
      way := ltrack.Waypoints[i];
      if way.Active then
      begin
        rp.InitXY(way.Longitude, way.Latitude);
        track.AddPoint(rp, way.Elevation, Way.Time);
        Depth.AddXY(way.Time, -way.Depth);
      end;
    end;
    track.LineWidth := 0.5;
  end;

  FTrackLayer.Visible := True;
  TGPSTrack(track.GPSObj).GetArea(FArea);
  FAreaSelected := True;
  MapView1.ZoomOnArea(FArea);
end;

procedure TfrmMain.cbRootDrivesChange(Sender: TObject);
var
  fsInfo: TFileSystemInfo;
  svalue: string;
begin
  if (cbRootDrives.ItemIndex >= 0) and (cbRootDrives.Items.Count >
    cbRootDrives.ItemIndex) then
  begin
    fsInfo := cbRootDrives.Items.Objects[cbRootDrives.ItemIndex] as TFileSystemInfo;
    HWLogger.SDRoot := fsInfo.DeviceID;
    svalue := 'keine';
    if HWLogger.IsLoggerCard then svalue := 'vorhanden';
    lblCardInfo.Caption := 'Name: ' + cbRootDrives.Text + LineEnding +
      'Gesamtspeicher: ' + FormatBytes(fsInfo.Size) + LineEnding +
      'Freier Speicher: ' + FormatBytes(fsInfo.FreeSpace) + LineEnding +
      'Dateisystem: ' + fsInfo.FileSystem + LineEnding +
      'Anzahl der Datendateien: ' + IntToStr(HWLogger.DataFileCount) +
      sLineBreak + 'Konfiguration: ' + svalue;
    PopulateFilesGrid();
    if HWLogger.HasError then
    begin
      if Sender <> timRefreshRoot then
        MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak +
          sLineBreak + HWLogger.LastError,
          mtError, [mbOK], 0);
    end;
  end
  else
    cbRootDrives.ItemIndex := -1;
end;

procedure TfrmMain.actPreferencesExecute(Sender: TObject);
var
  mr: integer;
begin
  frmPreferences.AppData := AppConfig.Appdata;
  frmPreferences.URL := JSONPropStorage1.ReadString(UPLOAD_URL, '');
  frmPreferences.Username := JSONPropStorage1.ReadString(UPLOAD_USERNAME, '');
  frmPreferences.Password := JSONPropStorage1.ReadString(UPLOAD_PASSWORD, '');
  frmPreferences.Bootloader := JSONPropStorage1.ReadInteger(LOGGER_BOOTLOADER, 1);
  frmPreferences.TilesMaxAge := JSONPropStorage1.ReadInteger(TILES_MAXAGE, 30);
  frmPreferences.VesselID := JSONPropStorage1.ReadInteger(VESSEL_ID, 0);
  frmPreferences.Debug := JSONPropStorage1.ReadBoolean(DEBUG, False);
  frmPreferences.DebugFile := JSONPropStorage1.ReadString(DEBUG_FILE,
    ConcatPaths([AppConfig.Appdata, 'debug.log']));
  mr := frmPreferences.ShowModal();
  if mr = mrOk then
  begin
    JSONPropStorage1.WriteString(TRACK_STOREPATH, frmPreferences.AppData);
    AppConfig.SetRoot(frmPreferences.AppData);
    JSONPropStorage1.WriteString(UPLOAD_URL, frmPreferences.URL);
    JSONPropStorage1.WriteString(UPLOAD_USERNAME, frmPreferences.Username);
    JSONPropStorage1.WriteString(UPLOAD_PASSWORD, frmPreferences.Password);
    JSONPropStorage1.WriteInteger(LOGGER_BOOTLOADER, frmPreferences.Bootloader);
    JSONPropStorage1.WriteInteger(TILES_MAXAGE, frmPreferences.TilesMaxAge);
    JSONPropStorage1.WriteInteger(VESSEL_ID, frmPreferences.VesselID);
    JSONPropStorage1.WriteBoolean(DEBUG, frmPreferences.Debug);
    JSONPropStorage1.WriteString(DEBUG_FILE, frmPreferences.DebugFile);
    JSONPropStorage1RestoringProperties(Sender);
  end;
end;

procedure TfrmMain.actSDManagementExecute(Sender: TObject);
begin
  frmSDCard.ShowModal;
end;

procedure TfrmMain.actTimestampExecute(Sender: TObject);
var
  fn: TStringList;
  i: integer;
begin
  if sgFiles.Row >= 1 then
  begin
    frmWait.Show();
    fn := TStringList.Create();
    try
      for i := 0 to sgFiles.RowCount - 1 do
      begin
        if sgFiles.IsCellSelected[0, i] then
          fn.Add(sgFiles.Cells[0, i]);
      end;
      HWLogger.Touch(fn);
      PopulateFilesGrid();
    finally
      frmWait.Hide();
      fn.Free();
    end;
  end;
end;

procedure TfrmMain.actTrackDeleteExecute(Sender: TObject);
var
  track: string;
begin
  track := stvTracks.GetPathFromNode(stvTracks.Selected);
  if track <> '' then
  begin
    if MessageDlg('Track löschen', 'Soll der Track ' + sLineBreak +
      track + sLineBreak + ' wirklich gelöscht werden?', mtConfirmation,
      mbOKCancel, '') = mrOk then
    begin
      DeleteFile(track);
      actTracksReload.Execute;
    end;
  end
  else
    MessageDlg('Track löschen', 'Kein Track markiert', mtInformation,
      [mbOK], '');
end;

procedure TfrmMain.SyncFrmTrack();
begin
  frmTrackEdit.Root := AppConfig.Trackspath;
  frmTrackEdit.VesselID := JSONPropStorage1.ReadInteger(VESSEL_ID, 0);
end;

procedure TfrmMain.actTracksReloadExecute(Sender: TObject);
var
  SelectedPath: string;
  i: integer;
  node: TShellTreeNode;
begin
  SelectedPath := stvTracks.GetPathFromNode(stvTracks.Selected);
  stvTracks.BeginUpdate;
  stvTracks.Root := '';
  stvTracks.Root := AppConfig.Trackspath;

  if SelectedPath <> '' then
  begin
    for i := 0 to stvTracks.Items.Count - 1 do
    begin
      Node := TShellTreeNode(stvTracks.Items[i]);
      if SameText(Node.FullFilename, SelectedPath) then
      begin
        stvTracks.Selected := Node;
        Break;
      end;
    end;
  end;
  stvTracks.EndUpdate;
end;

procedure TfrmMain.actUploadExecute(Sender: TObject);
begin
  OpenURL('https://depth.openseamap.org/');
end;

procedure TfrmMain.actConfigExecute(Sender: TObject);
var
  vid: integer;
begin
  frmLoggerConfig.SetLoggerCFG(HWLogger.LoggerCFG());
  vid := JSONPropStorage1.ReadInteger(VESSEL_ID, 0);
  if (frmLoggerConfig.VesselID = 0) and (vid <> 0) then
    frmLoggerConfig.VesselID := vid;
  if frmLoggerConfig.ShowModal() = mrOk then
  begin
    frmWait.Show();
    Application.ProcessMessages;
    try
      HWLogger.SetLoggerCFG(frmLoggerConfig.LoggerCFG());
      HWLogger.Write(frmLoggerConfig.Format, frmLoggerConfig.SDLabel);
      MessageDlg('Konfiguration',
        'Die neue Konfiguration wurde auf die Karte geschrieben.', mtInformation,
        [mbOK], '');
    finally
      frmWait.Hide;
    end;
  end;
end;

procedure TfrmMain.actEditExecute(Sender: TObject);
var
  filename: string;
  i: integer;
begin
  if sgFiles.Row > 0 then
  begin
    for i := 0 to sgFiles.RowCount - 1 do
    begin
      if sgFiles.IsCellSelected[0, i] then
      begin
        filename := sgFiles.Cells[0, i];
        Break;
      end;
    end;
    frmEditor.Filename := filename;
    frmEditor.ShowModal;
  end;
end;

procedure TfrmMain.actExportExecute(Sender: TObject);
var
  track: string;
begin
  track := stvTracks.GetPathFromNode(stvTracks.Selected);
  if track <> '' then
  begin
    frmExport.Name := ExtractFileNameOnly(track);
    if frmExport.ShowModal = mrOk then
    begin
      HWLogger.ExportTrack(track, frmExport.Path, frmExport.Format);
    end;
  end
  else
    MessageDlg('Track exportieren', 'Kein Track markiert', mtInformation,
      [mbOK], '');
end;

procedure TfrmMain.actMapShowExecute(Sender: TObject);
begin
  ClearTrack();
  if FTrackSelected then
    ShowTrackFileOnMap()
  else
    ShowDataFileOnMap();
end;

procedure TfrmMain.actMapZoomAreaExecute(Sender: TObject);
var
  germany: TRealPoint;
begin
  if FAreaSelected then
    MapView1.ZoomOnArea(FArea)
  else
  begin
    germany.InitXY(10.0, 51.0);
    MapView1.Center := Germany;
    MapView1.Zoom := 6;
  end;
  MapView1.Redraw;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  Infobox.ShowModal;
end;

procedure TfrmMain.actAdd2TrackExecute(Sender: TObject);
var
  track: string;
  fn: TStringList;
  i: integer;
begin
  SyncFrmTrack();
  track := stvTracks.GetPathFromNode(stvTracks.Selected);
  if track <> '' then
    if sgFiles.Row >= 1 then
    begin
      if MessageDlg('Daten hinzufügen', 'Soll die Daten dem Track ' +
        sLineBreak + track + sLineBreak + ' hinzugefügt werden?',
        mtConfirmation, mbOKCancel, '') = mrOk then
      begin
        SyncFrmTrack();
        frmWait.Show();
        fn := TStringList.Create();
        try
          for i := 0 to sgFiles.RowCount - 1 do
          begin
            if sgFiles.IsCellSelected[0, i] then
              fn.Add(sgFiles.Cells[0, i]);
          end;
          //        path := ConcatPaths([AppConfig.Trackspath, frmTrackEdit.Grouppath,
          //          frmTrackEdit.Trackname + '.zip']);
          HWLogger.Add2Track(track, fn);
          actTracksReloadExecute(Sender)
        finally
          frmWait.Hide();
          fn.Free();
        end;
      end;
    end
    else
      MessageDlg('Daten hinzufügen', 'Kein Track markiert', mtInformation,
        [mbOK], '');
end;

procedure TfrmMain.actNewTrackExecute(Sender: TObject);
var
  track: TLoggerTrackinfo;
  fn: TStringList;
  i: integer;
  path: string;
begin
  if sgFiles.Row >= 1 then
  begin
    SyncFrmTrack();
    if frmTrackEdit.ShowModal = mrOk then
    begin
      frmWait.Show();
      fn := TStringList.Create();
      try
        for i := 0 to sgFiles.RowCount - 1 do
        begin
          if sgFiles.IsCellSelected[0, i] then
            fn.Add(sgFiles.Cells[0, i]);
        end;
        path := ConcatPaths([AppConfig.Trackspath, frmTrackEdit.Grouppath,
          frmTrackEdit.Trackname + '.zip']);
        track := frmTrackEdit.GetTrackinfo;
        HWLogger.NewTrack(path, fn, track);
        actTracksReloadExecute(Sender)
      finally
        frmWait.Hide();
        fn.Free();
      end;
    end;
  end;
end;

procedure TfrmMain.actTrackEditExecute(Sender: TObject);
begin
  SyncFrmTrack();
  if frmTrackEdit.ShowModal = mrOk then
    ShowMessage('Nicht implementiert!');
end;

procedure TfrmMain.actAnalyseExecute(Sender: TObject);
var
  fn: string;
begin
  if sgFiles.Row >= 1 then
  begin
    fn := sgFiles.Cells[0, sgFiles.Row];
    TAnalyseThread.Create(fn);
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
  OpenURL('MCS Depth Logger UI.pdf');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  StopApplication();
  FLog.Info('Application stopped');
  FLog.Free();
end;

procedure TfrmMain.JSONPropStorage1RestoringProperties(Sender: TObject);
var
  mapProvider: string;
  i: integer;
begin
  if JSONPropStorage1.ReadBoolean(DEBUG, False) then
  begin
    DebugLogger.LogName := JSONPropStorage1.ReadString(DEBUG_FILE, 'debug.log');
    MCSDBGLog.Active := True;
    MCSDBGLog.LogLevel := TLogLevel.LvlDebug;
    if Sender.ClassNameIs('TJSONPropStorage') then
      FLog.Info('Application started');
  end;
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
  MapView1.CachePath := AppConfig.Tilescache;
  MapView1.CacheOnDisk := True;

  stvTracks.Root := AppConfig.Trackspath + '\';
  SetMapProvider();
  if frmTrackEdit <> nil then
    frmTrackEdit.Root := AppConfig.Trackspath;
  FLog.Info('Restoring Configuration finnished');
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

procedure TfrmMain.sgFilesClick(Sender: TObject);
begin
  FTrackSelected := False;
end;

procedure TfrmMain.sgFilesSelection(Sender: TObject; aCol, aRow: integer);
begin
  FTrackSelected := False;
  if cbAutoAnalyse.Checked then
    TAnalyseThread.Create(sgFiles.Cells[0, sgFiles.Row]);
  if cbAutoMap.Checked then
    actMapShowExecute(Sender);
  actAnalyse.Enabled := True;
end;

procedure TfrmMain.sbMainDrawPanel(Statusbar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel.Index = 3 then
  begin
    if FMapProxy.Started then
      ilStatus.Draw(sbMain.Canvas, Rect.Left, Rect.Top, 0)
    else
    begin
      with sbMain.Canvas do
      begin
        Brush.Color := clDefault;
        Font.Color := clDefault;
        FillRect(Rect);
      end;
    end;
  end;
end;

procedure TfrmMain.sbMainResize(Sender: TObject);
var
  size: integer;
  i: integer;
begin
  size := sbMain.ClientWidth;
  for i := 0 to sbMain.Panels.Count - 1 do
  begin
    if i <> 2 then
      size := size - sbMain.Panels[i].Width;
  end;
  sbMain.Panels[2].Width := size;
end;

procedure TfrmMain.spHorizontal1Moved(Sender: TObject);
begin
  actMapZoomAreaExecute(Sender);
end;

procedure TfrmMain.stvTracksClick(Sender: TObject);
begin
  FTrackSelected := True;
  if cbAutoMap.Checked then
  begin
    actMapShowExecute(Sender);
  end;
end;

procedure TfrmMain.stvTracksSelectionChanged(Sender: TObject);
begin
  FTrackSelected := True;
  if cbAutoMap.Checked then
  begin
    actMapShowExecute(Sender);
  end;
end;

procedure TfrmMain.timAfterStartTimer(Sender: TObject);
begin
  timAfterStart.Enabled := False;
  StartApplication();
end;

procedure TfrmMain.timStatusbarTimer(Sender: TObject);
begin
  sbMain.Panels[2].Text := '';
  timStatusbar.Enabled := False;
end;

procedure TfrmMain.stvTracksAddItem(Sender: TObject; const ABasePath: string;
  const AFileInfo: TSearchRec; var CanAdd: boolean);
begin
  // only allow directories and track zips
  CanAdd := False;
  // check if track zip
  if ExtractFileExt(AFileInfo.Name) = '.zip' then
    CanAdd := True;
  // check if directory
  if (AFileInfo.Attr and faDirectory) <> 0 then
    CanAdd := True;
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

procedure TfrmMain.timRefreshRootTimer(Sender: TObject);
begin
  timRefreshRoot.Enabled := False;
  if not FInit then
  begin
    RefreshRootDrives();
    if (cbRootDrives.Items.Count > 0) and (cbRootDrives.ItemIndex < 0) then
    begin
      cbRootDrives.ItemIndex := 0;
    end;
    cbRootDrivesChange(Sender);
    FInit := True;
  end;
  timRefreshRoot.Enabled := True;
end;

procedure TfrmMain.tbMapHabourClick(Sender: TObject);
begin
  FHabourlayer.Visible := tbMapHabour.Down;
end;

procedure TfrmMain.tbMapsSeamarksClick(Sender: TObject);
begin
  FSeamarkslayer.Visible := tbMapsSeamarks.Down;
end;

procedure TfrmMain.tbMapDepthClick(Sender: TObject);
begin
  if tbMapDepth.Down then
  begin
    if not FMapProxy.Started then
    begin
      FMapProxy.Start();
      sbMain.Invalidate;
    end;
    FDepthlayer.Visible := True;
  end
  else
  begin
    FDepthlayer.Visible := False;
    FMapProxy.Stop();
    sbMain.Invalidate;
  end;
end;

procedure TfrmMain.PopulateLayers();
begin
  RegisterMapProvider('OpenSeaMap Seamarks', ptEPSG3857,
    'https://tiles.openseamap.org/seamark/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenSeaMap Habour', ptEPSG3857,
    'https://t1.openseamap.org/habour/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenSeaMap Gebco', ptEPSG3857,
    'http://localhost:8580/gebco/tms/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);

  FDepthlayer := MapView1.Layers.Add as TMapLayer;
  FDepthlayer.Visible := False;
  FDepthlayer.MapProvider := 'OpenSeaMap Gebco';
  FDepthlayer.DrawMode := idmUseSourceAlpha;

  FSeamarkslayer := MapView1.Layers.Add as TMapLayer;
  FSeamarkslayer.Visible := False;
  FSeamarkslayer.MapProvider := 'OpenSeaMap Seamarks';
  FSeamarkslayer.DrawMode := idmUseSourceAlpha;

  FHabourlayer := MapView1.Layers.Add as TMapLayer;
  FHabourlayer.Visible := False;
  FHabourlayer.MapProvider := 'OpenSeaMap Habour';
  FHabourlayer.DrawMode := idmUseSourceAlpha;

  FTrackLayer := MapView1.Layers.Add as TMapLayer;
  FTrackLayer.Visible := False;
end;

procedure TfrmMain.StatusMsg(line: string);
begin
  sbMain.Panels[2].Text := line;
  timStatusbar.Enabled := True;
end;

procedure TfrmMain.StartApplication();
begin
  FCleanupThread := TFileCleanupThread.Create(AppConfig.Tilescache,
    JSONPropStorage1.ReadInteger(TILES_MAXAGE, 30));
end;

procedure TfrmMain.StopApplication();
begin
  if FCleanupThread <> nil then
  begin
    if not FCleanupThread.Finished then
    begin
      FCleanupThread.RequestStop;
      FCleanupThread.WaitFor;
    end;
    FCleanupThread.Free;
  end;
  if FMapProxy.Started then
    FMapProxy.Stop();
end;

procedure TfrmMain.ShowDataFileOnMap();
var
  track: TLoggerTrack;
  fn: TStringList;
  i: integer;
begin
  if sgFiles.Row >= 1 then
  begin
    frmWait.Show();
    fn := TStringList.Create();
    try
      for i := 0 to sgFiles.RowCount - 1 do
      begin
        if sgFiles.IsCellSelected[0, i] then
          fn.Add(sgFiles.Cells[0, i]);
      end;
      track := HWLogger.Convert(fn);
      if length(track.Waypoints) > 0 then
        ShowTrackOnMap(track)
      else
      begin
        frmWait.Hide();
        if cbAutoMap.Checked then
          StatusMsg('Keine kartenrelevanten Daten in der Datei gefunden.')
        else
          MessageDlg('Information',
            'Keine kartenrelevanten Daten in der Datei gefunden.',
            mtWarning, [mbOK], 0);
      end;
    finally
      frmWait.Hide();
      fn.Free();
    end;
  end;
end;

procedure TfrmMain.ShowTrackFileOnMap();
var
  FilePath: string;
  track: TLoggerTrack;
begin
  if stvTracks.Selected <> nil then
  begin
    if not TShellTreeNode(stvTracks.Selected).IsDirectory then
    begin
      FilePath := TShellTreeNode(stvTracks.Selected).FullFilename;
      track := HWLogger.ConvertFile(FilePath);
      if length(track.Waypoints) > 0 then
        ShowTrackOnMap(track)
      else
      if cbAutoMap.Checked then
        StatusMsg('Keine kartenrelevanten Daten in der Datei gefunden.')
      else
        MessageDlg('Information',
          'Keine kartenrelevanten Daten in der Datei gefunden.',
          mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmMain.ClearTrack();
begin
  FTrackLayer.Visible := False;
  FTrackLayer.PointsOfInterest.Clear;
  FTrackLayer.Tracks.Clear;
  FAreaSelected := False;
  MapView1.Invalidate;
  actMapZoomAreaExecute(nil);
  Depth.Clear;
end;


end.
