unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, Buttons, mvMapViewer,
  Grids;

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
    Panel1: TPanel;
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
    spLEft: TSplitter;
    spRight: TSplitter;
    spHorizontal: TSplitter;
    StatusBar: TStatusBar;
    StringGrid1: TStringGrid;
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
    TreeView1: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actConfigExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
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
    procedure tbMainResize(Sender: TObject);
    procedure tbMapResize(Sender: TObject);
    procedure tbTracksResize(Sender: TObject);
  private
    //MapView1: TMapView;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses fileinfo, uPreferences, ulogger, uloggerconfig, MCSAbout, ufsinfo;
  {$R *.lfm}

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  configDir: string;
  Title: string;
  FileVerInfo: TFileVersionInfo;
  Map: string;
  provider: TStringList;
  i: integer;
begin
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

  //  MapView1 := TMapView.Create(pRight);
  //  MapView1.Parent := pRight;
  MapView1.Align := alClient;
  provider := TStringList.Create();
  MapView1.GetMapProviders(provider);
  cbProvider.Items.Clear();
  for i := 0 to Provider.Count - 1 do
  begin
    cbProvider.Items.Add(Provider.Strings[i]);
  end;
  map := Provider.Strings[0];

  MapView1.MapProvider := map;
  Provider.Free();
end;

procedure TfrmMain.cbRootDrivesGetItems(Sender: TObject);
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
        cbRootDrives.AddItem(fsInfo.DeviceID + ' (' + fsInfo.Name + ') ', fsInfo);
    end;
  finally
    fileCollector.Free;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  MapView1.MapProvider := cbProvider.Text;
  if not (cbProvider.Text = '') then
    MapView1.Active := True;
end;

procedure TfrmMain.cbProviderChange(Sender: TObject);
begin
  MapView1.MapProvider := cbProvider.Text;
  if not (cbProvider.Text = '') then
    MapView1.Active := True;
end;

procedure TfrmMain.cbRootDrivesChange(Sender: TObject);
var
  fsInfo: TFileSystemInfo;
begin
  if cbRootDrives.ItemIndex >= 0 then
  begin
    fsInfo := cbRootDrives.Items.Objects[cbRootDrives.ItemIndex] as TFileSystemInfo;
  end;

end;

procedure TfrmMain.actPreferencesExecute(Sender: TObject);
var
  appData: string;
  mr: integer;
begin
  appData := GetAppConfigDir(True);
  frmPreferences.AppData := JSONPropStorage1.ReadString('track.storepath', appData);
  frmPreferences.URL := JSONPropStorage1.ReadString('upload.url', '');
  frmPreferences.Username := JSONPropStorage1.ReadString('upload.username', '');
  frmPreferences.Password := JSONPropStorage1.ReadString('upload.password', '');
  frmPreferences.Bootloader := JSONPropStorage1.ReadInteger('logger.bootloader', 1);

  mr := frmPreferences.ShowModal();
  if mr = mrOk then
  begin
    JSONPropStorage1.WriteString('track.storepath', frmPreferences.AppData);
    JSONPropStorage1.WriteString('upload.url', frmPreferences.URL);
    JSONPropStorage1.WriteString('upload.username', frmPreferences.Username);
    JSONPropStorage1.WriteString('upload.password', frmPreferences.Password);
    JSONPropStorage1.WriteInteger('logger.bootloader', frmPreferences.Bootloader);
  end;
end;

procedure TfrmMain.actConfigExecute(Sender: TObject);
var
  lgConfig: TLoggerConfig;
  cfg: TLoggerParameter;
  mr: integer;
begin
  mr := frmLoggerConfig.ShowModal();

  lgConfig := TLoggerConfig.Create();
  try
    cfg.VesselID := 1234;
    cfg.WriteGyro := True;
    cfg.WriteSupply := False;
    cfg.baudA := ConvertBaudrate('2400');
    cfg.baudB := ConvertBaudrate('4800');
    cfg.SeaTalk := True;
    lgConfig.Config := cfg;
    lgConfig.Write('H:\privat\git-sourcen\MCSDepthLoggerUI\testdata\config.dat');
  finally
    lgConfig.Free;
  end;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  Infobox.ShowModal;
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
  ShowMessage('Keine Hilfe vorhanden');
end;

procedure TfrmMain.JSONPropStorage1RestoringProperties(Sender: TObject);
var
  mapProvider: string;
begin
  mapProvider := JSONPropStorage1.ReadString('map.mapprovider', '');
  MapView1.MapProvider := mapProvider;
  cbProvider.Text := mapProvider;
end;

procedure TfrmMain.JSONPropStorage1SavingProperties(Sender: TObject);
begin
  JSONPropStorage1.WriteString('map.mapprovider', MapView1.MapProvider);
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
  w := tbTracks.ClientWidth - Label3.Width - 16;
  for i := 0 to tbTracks.ButtonCount - 1 do
  begin
    w := w - tbTracks.Buttons[i].Width;
  end;
  Panel6.Width := w;
end;

end.
