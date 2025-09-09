unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, Buttons, mvMapViewer,
  Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    actAnalyse: TAction;
    actConfig: TAction;
    actImport: TAction;
    actAdd2Track: TAction;
    actExport: TAction;
    actAbout: TAction;
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
    BitBtn1: TBitBtn;
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
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
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
    procedure actConfigExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure cbProviderChange(Sender: TObject);
    procedure cbRootDrivesGetItems(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure JSONPropStorage1RestoringProperties(Sender: TObject);
    procedure JSONPropStorage1SavingProperties(Sender: TObject);
    procedure tbtnZoomOutClick(Sender: TObject);
    procedure ToolBar1Resize(Sender: TObject);
    procedure ToolBar2Resize(Sender: TObject);
    procedure tbtnZoomInClick(Sender: TObject);
  private
    MapViewer1: TMapView;
  public

  end;

var
  Form1: TForm1;

implementation

uses fileinfo, uPreferences, ulogger, uloggerconfig;
  {$R *.lfm}

  { TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
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
    form1.Caption := Title;
  finally
    FileVerInfo.Free;
  end;

  MapViewer1 := TMapView.Create(Panel3);
  MapViewer1.Parent := Panel3;
  MapViewer1.Align := alClient;
  provider := TStringList.Create();
  MapViewer1.GetMapProviders(provider);
  cbProvider.Items.Clear();
  for i := 0 to Provider.Count - 1 do
  begin
    cbProvider.Items.Add(Provider.Strings[i]);
  end;
  map := Provider.Strings[0];

  MapViewer1.MapProvider := map;
  Provider.Free();
end;

procedure TForm1.cbRootDrivesGetItems(Sender: TObject);
var
  i: integer;
  driveRoot: string;
begin
  cbRootDrives.Items.Clear;
  for i := 0 to 25 do
  begin
    driveRoot := chr(Ord('A') + i) + ':\';
    if DirectoryExists(driveRoot) then
    begin
      cbRootDrives.Items.Add(driveRoot);
    end;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  MapViewer1.Active := True;
end;

procedure TForm1.cbProviderChange(Sender: TObject);
begin
  MapViewer1.MapProvider := cbProvider.Text;
  MapViewer1.Active := True;
end;

procedure TForm1.actPreferencesExecute(Sender: TObject);
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

procedure TForm1.actConfigExecute(Sender: TObject);
var lgConfig : TLoggerConfig;
  cfg : TLoggerParameter;
  mr: integer;
begin
  mr := frmLoggerConfig.ShowModal();

  lgConfig:= TLoggerConfig.Create();
  try
     cfg.VesselID:= 1234;
     cfg.WriteGyro:= true;
     cfg.WriteSupply:= false;
     cfg.baudA:= ConvertBaudrate('2400');
     cfg.baudB:= ConvertBaudrate('4800');
     cfg.SeaTalk:= true;
     lgConfig.Config := cfg;
     lgConfig.Write('H:\privat\git-sourcen\MCSDepthLoggerUI\testdata\config.dat');
  finally
    lgConfig.Free;
  end;
end;

procedure TForm1.actUpdateExecute(Sender: TObject);
begin

end;

procedure TForm1.actHelpExecute(Sender: TObject);
begin
  ShowMessage('Keine Hilfe vorhanden');
end;

procedure TForm1.JSONPropStorage1RestoringProperties(Sender: TObject);
var
  mapProvider: string;
begin
  mapProvider := JSONPropStorage1.ReadString('map.mapprovider', '');
  MapViewer1.MapProvider := mapProvider;
  cbProvider.Text := mapProvider;
end;

procedure TForm1.JSONPropStorage1SavingProperties(Sender: TObject);
begin
  JSONPropStorage1.WriteString('map.mapprovider', MapViewer1.MapProvider);
end;

procedure TForm1.ToolBar1Resize(Sender: TObject);
var
  w: longint;
  i: integer;
begin
  w := ToolBar1.ClientWidth - 1;
  for i := 0 to ToolBar1.ButtonCount - 1 do
  begin
    w := w - ToolBar1.Buttons[i].Width;
  end;
  Panel7.Width := w;
end;

procedure TForm1.ToolBar2Resize(Sender: TObject);
var
  w: longint;
  i: integer;
begin
  w := ToolBar2.ClientWidth - Label3.Width - 1;
  for i := 0 to ToolBar2.ButtonCount - 1 do
  begin
    w := w - ToolBar2.Buttons[i].Width;
  end;
  Panel6.Width := w;
end;

procedure TForm1.tbtnZoomInClick(Sender: TObject);
begin
  MapViewer1.Zoom := MapViewer1.Zoom + 1;
end;

procedure TForm1.tbtnZoomOutClick(Sender: TObject);
begin
  MapViewer1.Zoom := MapViewer1.Zoom - 1;
end;



end.
