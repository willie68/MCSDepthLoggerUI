unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, JSONPropStorage, ActnList, StdActns, StdCtrls, EditBtn, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    actAnalyse: TAction;
    ActionList1: TActionList;
    btnAnalyse: TBitBtn;
    cbAutoAnalyse: TCheckBox;
    cbRootDrives: TComboBox;
    FileExit1: TFileExit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    HelpAction1: THelpAction;
    ImageList1: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TrayIcon1: TTrayIcon;
    procedure actAnalyseExecute(Sender: TObject);
    procedure cbRootDrivesGetItems(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpAction1Execute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses fileinfo;
  {$R *.lfm}

  { TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  configDir: string;
  Title: string;
  FileVerInfo: TFileVersionInfo;
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
end;

procedure TForm1.actAnalyseExecute(Sender: TObject);
begin

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

procedure TForm1.HelpAction1Execute(Sender: TObject);
begin
  ShowMessage('Keine Hilfe vorhanden');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin

end;


end.
