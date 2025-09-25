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
  Forms, lazcontrols, tachartlazaruspkg, main, uPreferences, uloggerconfig,
  MCSAbout, ufsinfo, umcslogger, uconst, usdcardimages, uwait, ugomapproxy,
  utilecacheutils, utrackedit, uEditor, ufrmexport;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTrackEdit, frmTrackEdit);
  Application.CreateForm(TfrmPreferences, frmPreferences);
  Application.CreateForm(TfrmLoggerConfig, frmLoggerConfig);
  Application.CreateForm(TfrmSDCard, frmSDCard);
  Application.CreateForm(TfrmWait, frmWait);
  Application.CreateForm(TfrmEditor, frmEditor);
  Infobox.AppTitel:= 'MCS Depth Logger UI';
  Infobox.AppID:= 64;
  Infobox.CopyRight:= '(C) MCS 2025';
  Application.CreateForm(TfrmExport, frmExport);
  Application.Run;
end.

