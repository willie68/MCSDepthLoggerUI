unit umcslogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Process, SysUtils, fpjson, jsonparser, Forms;

type
  TProcessStringArray = array of TProcessString;

  TLoggerConfig = record
    BaudA: integer;
    BaudB: integer;
    SeaTalk: boolean;
    Supply: boolean;
    Gyro: boolean;
    VesselID: word;
  end;

  TLoggerDataFile = record
    Filename: string;
    Size: int64;
    Date: TDateTime;
  end;

  TLoggerDataFileArray = array of TLoggerDataFile;

  TLoggerCheckResult = record
    Filename: string;
    Size: int64;
    DatagrammCount: integer;
    ErrorCount: integer;
    ErrorA, ErrorB, ErrorI:integer;
    Version: string;
    FirstTimeStamp: TDateTime;
    LastTimeStamp: TDateTime;
  end;

  TThreeCoord = record
    X, Y, Z: int64;
  end;

  TLoggerWaypoint = record
    Active: boolean;
    Name: string;
    Latitude: double;
    Longitude: double;
    Time: TDateTime;
    Speed: double;
    Elevation: double;
    Depth: double;
    Acceleration: TThreeCoord;
    GyroLocation: TThreeCoord;
    Supply: integer;
 {
  Name         string       `json:"name,omitempty"`
  Lat          float64      `json:"latitude,omitempty"`
  Lon          float64      `json:"longitude,omitempty"`
  Time         time.Time    `json:"time,omitempty"`
  Speed        float64      `json:"speed,omitempty"`
  Ele          float64      `json:"elevation,omitempty"`
  Depth        float64      `json:"depth,omitempty"`
  Acceleration *ThreePoints `json:"acc,omitempty"`
  GyroLocation *ThreePoints `json:"gyro,omitempty"`
  Supply       int64        `json:"supply,omitempty"`
}
  end;

  TLoggerTrack = record
    Name: string;
    Waypoints: array of TLoggerWaypoint;
    Start: TLoggerWaypoint;
    Finish: TLoggerWaypoint;
    {
  Name      string      `json:"name,omitempty"`
  Waypoints []*Waypoint `json:"waypoints,omitempty"`
  Start     *Waypoint   `json:"start,omitempty"`
  End       *Waypoint   `json:"end,omitempty"`
  LogLines  []*LogLine  `json:"log_lines,omitempty"`
}
  end;

  { TMCSLogger }

  TMCSLogger = class
  private
    FLoggerCard: boolean;
    FDataFileCount: integer;
    FDataFiles: array of TLoggerDataFile;
    FRootpath: string;
    FCfg: TLoggerConfig;
    procedure InitCard(root: string);
    procedure ScanFolder();
    procedure HasCfg();
  public
    function Version(): string;
    procedure Read();
    procedure Write();
    function LoggerCFG(): TLoggerConfig;
    procedure SetLoggerCFG(newcfg: TLoggerConfig);
    function Check(filename: string): TLoggerCheckResult;
    function Convert(filenames: TStrings): TLoggerTrack;
    procedure Backup(backupFolder: string);
    procedure Restore(filename: string);
  published
    property IsLoggerCard: boolean read FLoggerCard;
    property SDRoot: string read FRootpath write InitCard;
    property DataFileCount: integer read FDataFileCount;
    property DataFiles: TLoggerDataFileArray read FDataFiles;
  end;

function ParseTimestamp(const S: string): TDateTime;
procedure AddParam(var params: TProcessStringArray; Value: string);
function ConvertWaypoint(way: TJSONObject): TLoggerWaypoint;

procedure CreateMCSLogger();

type

  { TExecOSMLThread }

  TExecOSMLThread = class(TThread)
  private
    FParams: TProcessStringArray;
    FOutput: string;
    FOk: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(params: TProcessStringArray);
    property Output: string read FOutput;
    property OK: boolean read FOk;
  end;

var
  HWLogger: TMCSLogger;

implementation

{ TMCSLogger }
uses
  LCLProc, Dialogs, FileUtil, DateUtils, LazFileUtils;

procedure CreateMCSLogger();
begin
  HWLogger := TMCSLogger.Create();
end;

{ getting the version of the osml tool }
function TMCSLogger.Version(): string;
var
  Output: string;
begin
  if RunCommand('osml', ['version', '--json'], Output, [poNoConsole]) then
    Result := Output
  else
    Result := '';
end;

{Reading the actual configuration from the sd card }
procedure TMCSLogger.Read();
var
  Output: string;
  Data: TJSONData;
  Obj: TJSONObject;
begin
  if FLoggerCard then
  begin
    if RunCommand('osml', ['logger', 'read', '-s', FRootpath, '--json'],
      Output, [poNoConsole]) then
    begin
      Data := GetJSON(Output);
      try
        if Data.JSONType = jtObject then
        begin
          Obj := TJSONObject(Data);
          FCfg.baudA := Obj.Get('baudA', 0);
          FCfg.baudB := Obj.Get('baudB', 0);
          FCfg.SeaTalk := Obj.Get('seatalk', False);
          FCfg.Gyro := Obj.Get('gyro', False);
          FCfg.Supply := Obj.Get('supply', False);
          FCfg.VesselID := Obj.Get('vesselID', 0);
        end;
      finally
        Data.Free;
      end;
    end
    else
      MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak + sLineBreak + Output,
        mtError, [mbOK], 0);
  end;
end;

{Writing the actual configuration to the sd card }
procedure TMCSLogger.Write();
var
  Output: string;
  Data: TJSONData;
  Obj: TJSONObject;
  params: TProcessStringArray;
begin
  SetLength(params, 9);
  params[0] := 'logger';
  params[1] := 'write';
  params[2] := '-s';
  params[3] := FRootpath;
  params[4] := '--json';
  params[5] := '-a';
  params[6] := IntToStr(FCfg.baudA);
  params[7] := '-b';
  params[8] := IntToStr(FCfg.baudB);
  if FCfg.SeaTalk then AddParam(params, '--seatalk');
  if FCfg.Gyro then AddParam(params, '--gyro');
  if FCfg.Supply then AddParam(params, '--supply');
  if FCfg.VesselID > 0 then
  begin
    AddParam(params, '--vesselid');
    AddParam(params, IntToStr(FCfg.VesselID));
  end;

  if RunCommand('osml', params, Output, [poNoConsole]) then
  begin
    Data := GetJSON(Output);
    try
      if Data.JSONType = jtObject then
      begin
        Obj := TJSONObject(Data);
        FCfg.baudA := Obj.Get('baudA', 0);
        FCfg.baudB := Obj.Get('baudB', 0);
        FCfg.SeaTalk := Obj.Get('seatalk', False);
        FCfg.Gyro := Obj.Get('gyro', False);
        FCfg.Supply := Obj.Get('supply', False);
        FCfg.VesselID := Obj.Get('vesselID', 0);
      end;
    finally
      Data.Free;
    end;
  end
  else
    MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak + sLineBreak + Output,
      mtError, [mbOK], 0);
end;

procedure TMCSLogger.InitCard(root: string);
begin
  FRootpath := root;
  HasCfg();
  ScanFolder();
  Read();
end;

procedure TMCSLogger.ScanFolder();
var
  FilteredFiles: TStringList;
  i, Count: integer;
  ldf: TLoggerDataFile;
begin
  FilteredFiles := TStringList.Create;
  try
    FindAllFiles(FilteredFiles, FRootpath, 'data*.dat', False);
    SetLength(FDataFiles, FilteredFiles.Count);
    Count := 0;
    for i := 0 to FilteredFiles.Count - 1 do
    begin
      ldf := FDataFiles[i];
      ldf.Filename := FilteredFiles[i];
      ldf.Size := FileSize(ldf.Filename);
      if not FileAge(ldf.Filename, ldf.Date, True) then
        ldf.Date := UnixToDateTime(0, False);
      FDataFiles[i] := ldf;
      Inc(Count);
    end;
    FDataFileCount := Count;
  finally
    FilteredFiles.Free;
  end;
end;

procedure TMCSLogger.HasCfg();
var
  lgConfig: string;
begin
  lgConfig := ConcatPaths([FRootpath, 'config.dat']);
  FLoggerCard := FileExists(lgConfig);
end;

function TMCSLogger.LoggerCFG(): TLoggerConfig;
begin
  Result := FCfg;
end;

procedure TMCSLogger.SetLoggerCFG(newcfg: TLoggerConfig);
begin
  FCfg := newcfg;
end;

function TMCSLogger.Check(filename: string): TLoggerCheckResult;
var
  Data: TJSONData;
  Obj: TJSONObject;
  jo: TJSONObject;
  Name, v: string;
  exec: TExecOSMLThread;
begin
  Result.Filename := filename;
  Result.DatagrammCount := -1;
  Result.LastTimeStamp := Now();
  Result.FirstTimeStamp := Now();
  Result.Version := 'n.N.';
  Result.ErrorCount := -1;

  if FLoggerCard then
  begin
    exec := TExecOSMLThread.Create(['check', '-s', filename]);
    try
      exec.WaitFor;
      if exec.Ok then
      begin
        Data := GetJSON(exec.Output);
        Name := ExtractFileName(filename);
        try
          try
            if Data.JSONType = jtObject then
            begin
              Obj := TJSONObject(Data);
              jo := Obj.Get('files', jo);
              jo := jo.Get(Name, jo);
              Result.ErrorCount := jo.Get('errorCount', -1);
              Result.DatagrammCount := jo.Get('datagramCount', -1);
              Result.Version := jo.Get('version', 'n.N.');
              v := jo.Get('firstTimestamp', '');
              Result.FirstTimeStamp := ParseTimestamp(v);
              v := jo.Get('lastTimestamp', '');
              Result.LastTimeStamp := ParseTimestamp(v);
              Result.ErrorA:= jo.Get('errorA', 0);
              Result.ErrorB:= jo.Get('errorB', 0);
              Result.ErrorI:= jo.Get('errorI', 0);
            end;
          except
            on e: Exception do
              MessageDlg('Fehler beim Parsen der osml Antwort.' +
                sLineBreak + sLineBreak + e.Message + sLineBreak +
                exec.Output, mtError, [mbOK], 0);
          end;
        finally
          Data.Free;
        end;
      end
      else
        MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak +
          sLineBreak + exec.Output,
          mtError, [mbOK], 0);
    finally
      exec.Free();
    end;
  end;
end;

function TMCSLogger.Convert(filenames: TStrings): TLoggerTrack;
var
  Data: TJSONData;
  Obj, way: TJSONObject;
  exec: TExecOSMLThread;
  arr: TJSONArray;
  i: integer;
  params: TProcessStringArray;
  filename : string;
begin
  if FLoggerCard then
  begin
    SetLength(params, 3);
    params[0] := 'convert';
    params[1] := '-s';
    params[2] := SDRoot;
    for i := 0 to filenames.Count -1 do
    begin
      filename := ExtractFileName(filenames[i]);
      AddParam(params, '-f');
      AddParam(params, filename);
    end;
    exec := TExecOSMLThread.Create(params);
    try
      exec.WaitFor;
      if exec.Ok then
      begin
        try
          try
            Result.Start.Active:=False;
            Result.Finish.Active:=False;
            Data := GetJSON(exec.Output);
            if Data.JSONType = jtObject then
            begin
              Obj := TJSONObject(Data);
              Result.Name := Obj.Get('name', '');
              if Obj.Find('start') <> nil then
                Result.Start := ConvertWaypoint(Obj.Get('start', TJSONObject.Create()));
              if Obj.FInd('end') <> nil then
                Result.Finish := ConvertWaypoint(Obj.Get('end', TJSONObject.Create()));
              if Obj.Find('waypoints', jtArray) <> nil then
              begin
                arr := TJsonArray(Obj.Find('waypoints', jtArray));
                SetLength(Result.Waypoints, arr.Count);
                for i := 0 to arr.Count - 1 do
                  if arr.Items[i].JSONType = jtObject then
                  begin
                    way := TJSONObject(arr.Items[i]);
                    Result.WayPoints[i] := ConvertWaypoint(way);
                  end;
              end;
            end;
          except
            on e: Exception do
              MessageDlg('Fehler beim Parsen der osml Antwort.' +
                sLineBreak + exec.Output + sLineBreak + e.Message, mtError, [mbOK], 0);
          end;
        finally
          Data.Free;
        end;
      end
      else
        MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak +
          sLineBreak + exec.Output, mtError, [mbOK], 0);
    finally
      exec.Free();
    end;
  end;
end;

function ConvertWaypoint(way: TJSONObject): TLoggerWaypoint;
begin
  Result.Active := False;
  if way <> nil then
  begin
    Result.Active := True;
    Result.Name := way.Get('name', '');
    Result.Latitude := way.Get('latitude', 0.0);
    Result.Longitude := way.Get('longitude', 0.0);
    Result.Speed := way.Get('speed', 0.0);
    Result.Elevation := way.Get('elevation', 0.0);
    Result.Depth := way.Get('depth', 0.0);
    Result.Supply := way.Get('supply', 0);
    Result.Time := ParseTimestamp(way.Get('time', '1970-01-01T00:00:00.000Z'));
  end;
end;

procedure TMCSLogger.Backup(backupFolder: string);
var
  Data: TJSONData;
  Obj: TJSONObject;
  filename: string;
  exec: TExecOSMLThread;
begin
  if FLoggerCard then
  begin
    exec := TExecOSMLThread.Create(['backup', '-s', FRootpath, '-o', backupFolder]);
    try
      exec.WaitFor;
      if exec.Ok then
      begin
        try
          try
            Data := GetJSON(exec.Output);
            if Data.JSONType = jtObject then
            begin
              Obj := TJSONObject(Data);
              filename := Obj.Get('filename', '');
            end;
          except
            on e: Exception do
              MessageDlg('Fehler beim Parsen der osml Antwort.' +
                sLineBreak + exec.Output + sLineBreak + e.Message, mtError, [mbOK], 0);
          end;
        finally
          Data.Free;
        end;
        MessageDlg('Backup erfolgreich' + sLineBreak + sLineBreak + filename,
          mtInformation, [mbOK], 0);
      end
      else
        MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak +
          sLineBreak + exec.Output, mtError, [mbOK], 0);
    finally
      exec.Free();
    end;
  end;
end;

procedure TMCSLogger.Restore(filename: string);
var
  Data: TJSONData;
  Obj: TJSONObject;
  exec: TExecOSMLThread;
begin
  exec := TExecOSMLThread.Create(['restore', '-s', FRootpath, '-z', filename]);
  try
    exec.WaitFor;
    if exec.Ok then
    begin
      try
        try
          Data := GetJSON(exec.Output);
          if Data.JSONType = jtObject then
          begin
            Obj := TJSONObject(Data);
            filename := Obj.Get('filename', '');
          end;
        except
          on e: Exception do
            MessageDlg('Fehler beim Parsen der osml Antwort.' +
              sLineBreak + exec.Output + sLineBreak + e.Message, mtError, [mbOK], 0);
        end;
      finally
        Data.Free;
      end;
      MessageDlg('Restore erfolgreich' + sLineBreak + sLineBreak + filename,
        mtInformation, [mbOK], 0);
    end
    else
      MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak +
        sLineBreak + exec.Output,
        mtError, [mbOK], 0);
  finally
    exec.Free;
  end;
end;

{ TExecOSMLThread }

procedure TExecOSMLThread.Execute;
begin
  AddParam(FParams, '--json');
  FOK := RunCommand('osml', FParams, FOutput, [poNoConsole]);
end;

constructor TExecOSMLThread.Create(params: TProcessStringArray);
begin
  inherited Create(True);
  FParams := params;
  FOK := False;
  Start();
end;

function ParseTimestamp(const S: string): TDateTime;
var
  BasePart, MilliPart: string;
  DT: TDateTime;
  DotPos: integer;
  Millis: integer;
begin
  DotPos := LastDelimiter('.', S);

  if DotPos > 0 then
  begin
    BasePart := Copy(S, 1, DotPos - 1);
    MilliPart := Copy(S, DotPos + 1, Length(S));
    MilliPart := LeftStr(MilliPart + '000', 3); // Auffüllen auf 3 Stellen
    Millis := StrToIntDef(MilliPart, 0);
  end
  else
  begin
    BasePart := S;
    Millis := 0;
  end;

  DT := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', BasePart);
  Result := RecodeMilliSecond(DT, Millis);
end;

procedure AddParam(var params: TProcessStringArray; Value: string);
var
  pos: integer;
begin
  pos := length(params);
  SetLength(params, pos + 1);
  params[pos] := Value;
end;

end.
