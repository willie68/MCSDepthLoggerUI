unit ulogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLoggerParameter = record
    baudA: byte;
    baudB: byte;
    SeaTalk: boolean;
    WriteSupply: boolean;
    WriteGyro: boolean;
    Version: string;
    VesselID: word;
    NormVoltage: integer;
    Bootloaderversion: integer;
    BootloaderCRC: integer;
  end;

  { TLoggerConfig }

  TLoggerConfig = class
  private
    mconfig: TLoggerParameter;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Read(filename: string);
    procedure Write(filename: string);

    property Config: TLoggerParameter read mconfig write mconfig;
  published
  end;

  function ConvertBaudrate(baud: string): integer ;

implementation

{ TLoggerConfig }

constructor TLoggerConfig.Create();
begin

end;

destructor TLoggerConfig.Destroy();
begin
  inherited Destroy();
end;

procedure TLoggerConfig.Read(filename: string);
begin

end;

procedure TLoggerConfig.Write(filename: string);
var
  f: TextFile;
  outputs : integer;
begin
  Assign(f, filename);
  Rewrite(f);
  try
    if mconfig.SeaTalk then system.Write(f, 's');
    WriteLn(f, IntToStr(mconfig.baudA));
    WriteLn(f, IntToStr(mconfig.baudB));
    outputs := 0;
    if mconfig.WriteSupply then outputs:= outputs + 1;
    if mconfig.WriteGyro then outputs:= outputs + 2;

    Writeln(f, IntToStr(outputs));
    if mconfig.VesselID > 0 then
      Writeln(f,LowerCase(Format('%.8X' ,[mconfig.VesselID])));
  finally
    CloseFile(f);
  end;
end;

function ConvertBaudrate(baud: string): integer;
begin
  case baud of
    '1200': Result := 1;
    '2400': Result := 2;
    '4800': Result := 3;
    '9600': Result := 4;
    '19200': Result := 5;
    else Result := 0;
  end;
end;

end.

