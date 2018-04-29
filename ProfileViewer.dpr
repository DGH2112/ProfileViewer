(**
  
  This module contains the project definition for the application.

  @Version 1.0
  @Author David Hoyle
  @Date   29 Apr 2018

**)
program ProfileViewer;

{$R 'ITHVerInfo.res' 'ITHVerInfo.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppVCL,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  DGHLibrary in 'Externals\DGHLibrary.pas',
  ProgressForm in 'Source\ProgressForm.pas' {frmProgress},
  AggregateList in 'Source\AggregateList.pas',
  Profiler in 'Source\Profiler.pas',
  VirtualTrees in 'VirtualTrees\VirtualTrees.pas',
  VTAccessibilityFactory in 'VirtualTrees\VTAccessibilityFactory.pas',
  DGHEllipsisLabel in 'Externals\DGHEllipsisLabel.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  Application.Initialize;
  Application.Title := 'Profile Viewer';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
