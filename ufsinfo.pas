unit ufsinfo;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ActiveX, ComObj, Variants, fgl
  { you can add units after this };

type

  { TFileSystemInfo }

  TFileSystemInfo = class
    DeviceID: string;
    Name: string;
    DriveType: integer;
    FreeSpace: int64;
    Size: int64;
    FileSystem: string;
    FsType: string;
  public
    function Clone(): TFileSystemInfo;
  end;

  TFileInfoList = specialize TFPGObjectList<TFileSystemInfo>;

  { TFileSystemInfoCollector }

  TFileSystemInfoCollector = class(TObject)
  private
    FFileSystemInfos: TFileInfoList;
  protected
    procedure DoRun;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
  published
    property FileInfos: TFileInfoList read FFileSystemInfos;
  end;

function DriveTypeStr(DriveType: integer): string;
function FormatBytes(Bytes: int64): string;

implementation


{ TFileSystemInfoCollector }

const
  wbemFlagForwardOnly = $00000020;

var
  FSWbemLocator: olevariant;
  FWMIService: olevariant;
  FWbemObjectSet: olevariant;
  FWbemObject: olevariant;
  oEnum: IEnumvariant;

function TFileSystemInfo.Clone(): TFileSystemInfo;
begin
  Result := TFileSystemInfo.Create;
  Result.Name := Name;
  Result.DeviceID := DeviceID;
  Result.DriveType := DriveType;
  Result.FileSystem := FileSystem;
  Result.FreeSpace := FreeSpace;
  Result.FsType := FSType;
end;

procedure TFileSystemInfoCollector.DoRun;
var
  pUlong: longword;
  inf: TFileSystemInfo;
begin
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet := FWMIService.ExecQuery('SELECT * FROM  Win32_LogicalDisk',
    'WQL', wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;

  while oEnum.Next(1, FWbemObject, pUlong) = 0 do
  begin
    inf := TFileSystemInfo.Create();
    inf.DeviceID := string(FWbemObject.DeviceID);
    inf.DriveType := FWbemObject.DriveType;
    if not VarIsNull(FWbemObject.FreeSpace) then
      inf.FreeSpace := int64(FWbemObject.FreeSpace)
    else
      inf.FreeSpace := -1;
    if not VarIsNull(FWbemObject.VolumeName) then
      inf.Name := string(FWbemObject.VolumeName);
    if not VarIsNull(FWbemObject.Description) then
      inf.FsType := string(FWbemObject.Description);
    if not VarIsNull(FWbemObject.FileSystem) then
      inf.FileSystem := string(FWbemObject.FileSystem)
    else
      inf.FileSystem := 'unknown';
    if not VarIsNull(FWbemObject.Size) then
      inf.Size := int64(FWbemObject.Size);
    FFileSystemInfos.Add(inf);
  end;
end;

constructor TFileSystemInfoCollector.Create();
begin
  FFileSystemInfos := TFileInfoList.Create();
end;

destructor TFileSystemInfoCollector.Destroy;
begin
  FFileSystemInfos.Free();
  inherited Destroy;
end;

procedure TFileSystemInfoCollector.Refresh;
begin
  FFileSystemInfos.Clear;
  DoRun;
end;

function DriveTypeStr(DriveType: integer): string;
begin
  case DriveType of
    1: Result := 'No_Root';
    2: Result := 'Removable';
    3: Result := 'Local';
    4: Result := 'Network';
    5: Result := 'CD/DVD';
    6: Result := 'RAM';
    else
      Result := 'Unknown';
  end;
end;

function FormatBytes(Bytes: int64): string;
const
  Units: array[0..5] of string = ('B', 'KB', 'MB', 'GB', 'TB', 'PB');
var
  Size: double;
  UnitIndex: integer;
begin
  Size := Bytes;
  UnitIndex := 0;

  while (Size >= 1024) and (UnitIndex < High(Units)) do
  begin
    Size := Size / 1024;
    Inc(UnitIndex);
  end;

  Result := FormatFloat('0.0', Size) + ' ' + Units[UnitIndex];
end;

end.
