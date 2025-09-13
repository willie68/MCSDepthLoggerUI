unit uconst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TConfigPathes }

  TConfigPathes = class
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
  end;

procedure CreateConfigPathes(rootfolder: string);

var
  ConfigPathes: TConfigPathes;

implementation


procedure CreateConfigPathes(rootfolder: string);
begin
  if ConfigPathes = nil then
  begin
    ConfigPathes := TConfigPathes.Create();
  end;
  ConfigPathes.SetRoot(rootfolder);
end;

{ TConfigPathes }

procedure TConfigPathes.EnsurePaths();
begin
  ForceDirectories(FTrackspath);
  ForceDirectories(FTilesCache);
  ForceDirectories(FBackuppath);
  ForceDirectories(FTemp);
end;

procedure TConfigPathes.SetRoot(rootFolder: string);
begin
  FRootpath := rootFolder;
  FTilesCache := ConcatPaths([FRootpath, 'tilescache']);
  FBackuppath := ConcatPaths([FRootpath, 'backup']);
  FTrackspath := ConcatPaths([FRootpath, 'tracks']);
  FTemp := ConcatPaths([FRootpath, 'temp']);
  EnsurePaths();
end;

end.
