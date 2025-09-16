unit ugomapproxy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Process, SysUtils;

type
  { TExecProxy }

  TExecProxy = class
  private
    FParams: TProcessStrings;
    FConfig: string;
    FProc: TProcess;
    FStarted: boolean;
  protected
  public
    constructor Create(config: string);
    procedure Start();
    procedure Stop();
  end;

implementation

{ TExecProxy }

constructor TExecProxy.Create(config: string);
begin
  FConfig := config;
  FParams := TProcessStringList.Create();
  FParams.Add('-c');
  FParams.Add(FConfig);
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
    FStarted := False;
  end;
end;

end.
