unit mcslogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TLoggerParameter = record
    baudA: integer;
    baudB: integer;
    SeaTalk: boolean;
    Supply: boolean;
    Gyro: boolean;
    VesselID: word;
  end;

  { TMCSLogger }

  TMCSLogger = class
  private
    rootpath: string;
    loggerCFG: TLoggerParameter;
  public
    function Version(): string;
    procedure Read();
    procedure WriteLoggerCFG(lcfg: TLoggerParameter);
  published
    property SDRoot: string read rootpath write rootpath;
    property Config: TLoggerParameter read loggerCFG write WriteLoggerCFG;
  end;

implementation

{ TMCSLogger }
uses
  LCLProc, Process, Dialogs, fpjson, jsonparser;

function TMCSLogger.Version(): string;
var
  Output: string;
begin
  if RunCommand('osml', ['version', '--json'], Output, [poNoConsole]) then
    Result := Output
  else
    Result := '';
end;

function TMCSLogger.Config(): TLoggerParameter;
var
  Output: string;
  Data: TJSONData;
  Obj: TJSONObject;
  cfg: TLoggerParameter;
begin
  if RunCommand('osml', ['logger', 'read', '-s', rootpath, '--json'],
    Output, [poNoConsole]) then
  begin
    ShowMessage(Output);
    Data := GetJSON(Output);
    try
      if Data.JSONType = jtObject then
      begin
        Obj := TJSONObject(Data);
        cfg.baudA := Obj.Get('baudA', 0);
        cfg.baudB := Obj.Get('baudB', 0);
        cfg.SeaTalk := Obj.Get('seatalk', False);
        cfg.Gyro := Obj.Get('gyro', False);
        cfg.Supply := Obj.Get('supply', False);
        cfg.VesselID := Obj.Get('vesselID', 0);
      end;
    finally
      Data.Free;
    end;
    Result := cfg;
  end
  else
    MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak + sLineBreak + Output,
      mtError, [mbOK], 0);
end;

procedure TMCSLogger.WriteConfig(lcfg : TLoggerParameter);
begin
  var
    Output: string;
    Data: TJSONData;
    Obj: TJSONObject;
  begin
    loggerCFG :=lcfg;
    if RunCommand('osml', ['logger', 'write', '-s', rootpath, '--json'
    '-a', ],
      Output, [poNoConsole]) then
    begin
      ShowMessage(Output);
      Data := GetJSON(Output);
      try
        if Data.JSONType = jtObject then
        begin
          Obj := TJSONObject(Data);
          cfg.baudA := Obj.Get('baudA', 0);
          cfg.baudB := Obj.Get('baudB', 0);
          cfg.SeaTalk := Obj.Get('seatalk', False);
          cfg.Gyro := Obj.Get('gyro', False);
          cfg.Supply := Obj.Get('supply', False);
          cfg.VesselID := Obj.Get('vesselID', 0);
        end;
      finally
        Data.Free;
      end;
      Result := cfg;
    end
    else
      MessageDlg('Bitte überprüfe die SD Karte!' + sLineBreak + sLineBreak + Output,
        mtError, [mbOK], 0);
end;

end.
