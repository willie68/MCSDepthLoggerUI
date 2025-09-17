unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, Buttons, mvMapViewer,
  mvPluginCommon, mvGeoNames, mvPlugins, Grids, ShellCtrls, CheckLst,
  TAGraph, TASeries, TAIntervalSources, umcslogger, mvTypes, mvGPSObj,
  ugomapproxy;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actAnalyse: TAction;
    actConfig: TAction;
    actImport: TAction;
    actAdd2Track: TAction;
    actExport: TAction;
    actAbout: TAction;
    actMapZoomArea: TAction;
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
    spHorizontal1: TSplitter;
    Splitter1: TSplitter;
    stvTracks: TShellTreeView;
    spLEft: TSplitter;
    spRight: TSplitter;
    spHorizontal: TSplitter;
    sbMain: TStatusBar;
    sgFiles: TStringGrid;
    Timer1: TTimer;
    tbMain: TToolBar;
    tbTracks: TToolBar;
    tbMap: TToolBar;
    StatusTimer: TTimer;
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
    tbMapsSeamarks: TToolButton;
    tbMapSports: TToolButton;
    tbMapDepth: TToolButton;
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
    procedure actExportExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actMapExecute(Sender: TObject);
    procedure actMapZoomAreaExecute(Sender: TObject);
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
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure JSONPropStorage1RestoringProperties(Sender: TObject);
    procedure JSONPropStorage1SavingProperties(Sender: TObject);
    procedure sgFilesSelection(Sender: TObject; aCol, aRow: integer);
    procedure slvTracksSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure sbMainDrawPanel(Statusbar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure sbMainResize(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure tbMainResize(Sender: TObject);
    procedure tbMapResize(Sender: TObject);
    procedure tbMapSportsClick(Sender: TObject);
    procedure tbTracksResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tbMapsSeamarksClick(Sender: TObject);
    procedure tbMapDepthClick(Sender: TObject);
  private
    FInit: boolean;
    FTrackSelected: boolean;
    FArea: TRealArea;
    FAreaSelected: boolean;
    FMapProxy: TExecProxy;
    FDepthlayer: TMapLayer;
    FSeamarkslayer: TMapLayer;
    FSportslayer: TMapLayer;
    FTrackLayer: TMapLayer;
    procedure SetMapProvider();
    procedure RefreshRootDrives();
    procedure PopulateFilesGrid();
    procedure ShowTrackOnMap(ltrack: TLoggerTrack);
    procedure PopulateLayers();
    procedure StatusMsg(line: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses fileinfo, uPreferences, uloggerconfig, MCSAbout, ufsinfo, LazStringUtils,
  uconst, usdcardimages, mvDrawingEngine, mvMapProvider, uwait;
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

  PopulateLayers();
  MapView1.MapProvider := map;
  Provider.Free();

  CreateMCSLogger();
  FTrackSelected := False;

end;

procedure TfrmMain.cbRootDrivesGetItems(Sender: TObject);
begin
  RefreshRootDrives();
end;

procedure TfrmMain.DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
  ASteps: TDateTimeStep);
begin

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
    if FAreaSelected then
      MapView1.ZoomOnArea(FArea)
    else
    begin
      MapView1.Center := MvGeoNames1.Search('Germany', MapView1.DownloadEngine);
      MapView1.Zoom := 7;
    end;
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
    FTrackLayer.AddPointOfInterest(rpStart, ltrack.Start.Name).ImageIndex:=149;
  end;

  if ltrack.Finish.Active then
  begin
    rpEnd.InitXY(ltrack.Finish.Longitude, ltrack.Finish.Latitude);
    FTrackLayer.AddPointOfInterest(rpEnd, ltrack.Finish.Name).ImageIndex:=150;
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
var
  track: TLoggerTrack;
  fn: string;
begin
  if FTrackSelected then
    ShowMessage('Nicht implementiert!')
  else
  begin
    if sgFiles.Row >= 1 then
    begin
      frmWait.Show();
      try
        fn := sgFiles.Cells[0, sgFiles.Row];
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
      end;
    end;
  end;
end;

procedure TfrmMain.actMapZoomAreaExecute(Sender: TObject);
begin
  if FAreaSelected then
  begin
    MapView1.ZoomOnArea(FArea);
    MapView1.Redraw;
  end;
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
  frmWait.Show;
  ShowMessage('Keine Hilfe vorhanden');
  frmWait.Hide;
end;

procedure TfrmMain.JSONPropStorage1RestoringProperties(Sender: TObject);
var
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

  stvTracks.Root := ConfigPathes.Trackspath + '\';
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
  FTrackSelected := False;
  if cbAutoAnalyse.Checked then
    TAnalyseThread.Create(sgFiles.Cells[0, sgFiles.Row]);
  if cbAutoMap.Checked then
    actMapExecute(Sender);
  actAnalyse.Enabled := True;
end;

procedure TfrmMain.slvTracksSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  FTrackSelected := True;
end;

procedure TfrmMain.sbMainDrawPanel(Statusbar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel.Index = 3 then
  begin
    if FMapProxy.Started then
      ImageList.Draw(sbMain.Canvas, Rect.Left, Rect.Top, 370)
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

procedure TfrmMain.StatusTimerTimer(Sender: TObject);
begin
  sbMain.Panels[2].Text := '';
  StatusTimer.Enabled := False;
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
end;

procedure TfrmMain.tbMapSportsClick(Sender: TObject);
begin
  FSportslayer.Visible := tbMapSports.Down;
end;

procedure TfrmMain.tbMapsSeamarksClick(Sender: TObject);
begin
  FSeamarkslayer.Visible := tbMapsSeamarks.Down;
end;

procedure TfrmMain.tbMapDepthClick(Sender: TObject);
begin
  if tbMapDepth.Down then
  begin
    FMapProxy.Start();
    FDepthlayer.Visible := True;
    sbMain.Invalidate;
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
  RegisterMapProvider('OpenSeaMap Sports', ptEPSG3857,
    'https://tiles.openseamap.org/sports/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
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

  FSportslayer := MapView1.Layers.Add as TMapLayer;
  FSportslayer.Visible := False;
  FSportslayer.MapProvider := 'OpenSeaMap Sports';
  FSportslayer.DrawMode := idmUseSourceAlpha;

  FTrackLayer := MapView1.Layers.Add as TMapLayer;
  FTrackLayer.Visible := False;
end;

procedure TfrmMain.StatusMsg(line: string);
begin
  sbMain.Panels[2].Text := line;
  StatusTimer.Enabled := True;
end;


end.
