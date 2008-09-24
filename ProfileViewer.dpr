(**
  
  This module contains the project definition for the application.

  @Version 1.0
  @Author David Hoyle
  @Date   24 Sep 2008

**)
program ProfileViewer;

{%TogetherDiagram 'ModelSupport_ProfileViewer\default.txaPackage'}

uses
  ExceptionLog,
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  About in '..\..\LIBRARY\About.pas',
  DGHLibrary in '..\..\LIBRARY\DGHLibrary.pas',
  DGHSpectrum in '..\..\Components\Source\DGHSpectrum.pas',
  ProgressForm in '..\..\LIBRARY\ProgressForm.pas' {frmProgress},
  AggregateList in 'Source\AggregateList.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  SetEurekaLogState(DebugHook = 0);
  Application.Initialize;
  Application.Title := 'Profile Viewer';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
