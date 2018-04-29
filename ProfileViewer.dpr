(**
  
  This module contains the project definition for the application.

  @Version 1.0
  @Author David Hoyle
  @Date   05 Apr 2012

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
  ProgressForm in 'Externals\ProgressForm.pas' {frmProgress},
  AggregateList in 'Source\AggregateList.pas',
  checkforupdates in 'Externals\checkforupdates.pas',
  CheckForUpdatesForm in 'Externals\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  MSXML2_TLB in 'Externals\MSXML2_TLB.pas',
  Profiler in 'Externals\Profiler.pas',
  VirtualTrees in 'VirtualTrees\VirtualTrees.pas',
  VTAccessibilityFactory in 'VirtualTrees\VTAccessibilityFactory.pas',
  DGHEllipsisLabel in 'Externals\DGHEllipsisLabel.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  DGHNumericEdit in 'Externals\DGHNumericEdit.pas',
  CheckForUpdatesOptionsForm in 'Externals\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions};

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
