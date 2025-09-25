unit ugomapproxy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Process, SysUtils, MCSDBGLog;

type
  { TExecProxy }

  TExecProxy = class
  private
    FLog : TLogger;
    FParams: TProcessStrings;
    FConfig: string;
    FProc: TProcess;
    FStarted: boolean;
  protected
  public
    constructor Create(config: string);
    destructor Destroy(); override;
    procedure Start();
    procedure Stop();
  published
    property Started: boolean read FStarted;
  end;

implementation

{ TExecProxy }

constructor TExecProxy.Create(config: string);
begin
  FLog := TLogger.Create('gomapproxy');
  FConfig := config;
  FParams := TProcessStringList.Create();
  FParams.Add('-c');
  FParams.Add(FConfig);
end;

destructor TExecProxy.Destroy();
begin
  FLog.Free;
end;

procedure TExecProxy.Start();
begin
  if not FStarted then
  begin
    FProc := TProcess.Create(nil);
    FStarted := True;

    FProc.Executable := 'gomapproxy.exe';
    FProc.Options := FProc.Options + [poNoConsole];
    FProc.Parameters := FParams;
    FLog.Debugf('starting %s %s', [FProc.Executable,  StringReplace(FParams.Text , sLineBreak,  ' ' ,[rfReplaceAll, rfIgnoreCase])]);
    FProc.Execute; // start the program
  end;
end;

procedure TExecProxy.Stop();
begin
  if FStarted then
  begin
    FProc.Terminate(0);
    FProc.WaitOnExit;
    FProc.Free;
    FLog.Debugf('stopping %s', [FProc.Executable]);
    FStarted := False;
  end;
end;

end.
