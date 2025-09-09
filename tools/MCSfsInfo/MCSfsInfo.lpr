program MCSfsInfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  ActiveX,ComObj,Variants
  { you can add units after this };

type

  { TFileSystemInfo }

  TFileSystemInfo = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFileSystemInfo }

const
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;

function DriveTypeStr(DriveType:integer): string;
begin
  case DriveType of
    0 : Result:='Unknown';
    1 : Result:='No_Root';
    2 : Result:='Removable';
    3 : Result:='Local';
    4 : Result:='Network';
    5 : Result:='CD/DVD';
    6 : Result:='RAM';
  end;
end;

procedure TFileSystemInfo.DoRun;
var
  ErrorMsg: String;
  pUlong : LongWord;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT * FROM  Win32_LogicalDisk','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  while oEnum.Next(1, FWbemObject, pUlong) = 0 do
  begin
    Write(String(FWbemObject.DeviceID));
    Write(',');
    Write(DriveTypeStr(FWbemObject.DriveType));
    Write(',');
    if not VarIsNull(FWbemObject.FreeSpace) then
      Write(Format('%d',[Int64(FWbemObject.FreeSpace)]))
    else
      Write('0');
    Write(',');
    if not VarIsNull(FWbemObject.FileSystem) then
      Write(String(FWbemObject.FileSystem))
    else
      Write('unknown');
    Writeln('');
    FWbemObject:=Unassigned;
  end;
  // stop program loop
  Terminate;
end;

constructor TFileSystemInfo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFileSystemInfo.Destroy;
begin
  inherited Destroy;
end;

procedure TFileSystemInfo.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TFileSystemInfo;

{$R *.res}

begin
  Application:=TFileSystemInfo.Create(nil);
  Application.Title:='FileSystemInfo';
  Application.Run;
  Application.Free;
end.

