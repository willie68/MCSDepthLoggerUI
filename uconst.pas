unit uconst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  TILES_MAXAGE = 'maps.tilesmaxage';
  LOGGER_BOOTLOADER = 'logger.bootloader';
  UPLOAD_PASSWORD = 'upload.password';
  UPLOAD_USERNAME = 'upload.username';
  UPLOAD_URL = 'upload.url';
  TRACK_STOREPATH = 'track.storepath';
  VESSEL_ID = 'vesselid';
  DEBUG_FILE = 'debug.file';
  DEBUG = 'debug.active';

type

  { TAppConfig }

  TAppConfig = class
  private
    FRootpath: string;
    FTilesCache: string;
    FBackuppath: string;
    FTrackspath: string;
    FTemp: string;
    procedure EnsurePaths();
  public
    procedure SetRoot(rootFolder: string);
  published
    property Appdata: string read FRootpath;
    property Tilescache: string read FTilesCache;
    property Backuppath: string read FBackuppath;
    property Trackspath: string read FTrackspath;
    property Temp: string read FTemp;
  end;

procedure CreateConfigPathes(rootfolder: string);

var
  AppConfig: TAppConfig;

implementation


procedure CreateConfigPathes(rootfolder: string);
begin
  if AppConfig = nil then
  begin
    AppConfig := TAppConfig.Create();
  end;
  AppConfig.SetRoot(rootfolder);
end;

{ TAppConfig }

procedure TAppConfig.EnsurePaths();
begin
  ForceDirectories(FTrackspath);
  ForceDirectories(FTilesCache);
  ForceDirectories(FBackuppath);
  ForceDirectories(FTemp);
end;

procedure TAppConfig.SetRoot(rootFolder: string);
begin
  FRootpath := rootFolder;
  FTilesCache := ConcatPaths([FRootpath, 'tilescache']);
  FBackuppath := ConcatPaths([FRootpath, 'backup']);
  FTrackspath := ConcatPaths([FRootpath, 'tracks']);
  FTemp := ConcatPaths([FRootpath, 'temp']);
  EnsurePaths();
end;

end.
