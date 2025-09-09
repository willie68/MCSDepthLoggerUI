program MCSDLUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, main, uPreferences, ulogger, uloggerconfig, MCSAbout;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPreferences, frmPreferences);
  Application.CreateForm(TfrmLoggerConfig, frmLoggerConfig);
  Infobox.AppTitel:= 'MCS Depth Logger UI';
  Infobox.AppID:= 64;
  Infobox.CopyRight:= '(C) MCS 2025';

  Application.Run;
end.

