(**
  
  This module contains the project definition for the application.

  @Version 1.0
  @Author David Hoyle
  @Date   05 Apr 2012

**)
program ProfileViewer;

uses
  ExceptionLog,
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  About in '..\..\LIBRARY\About.pas',
  DGHLibrary in '..\..\LIBRARY\DGHLibrary.pas',
  DGHSpectrum in '..\..\Components\Source\DGHSpectrum.pas',
  ProgressForm in '..\..\LIBRARY\ProgressForm.pas' {frmProgress},
  AggregateList in 'Source\AggregateList.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  MSXML2_TLB in '..\..\LIBRARY\MSXML2_TLB.pas',
  Profiler in '..\..\LIBRARY\Profiler.pas',
  VirtualTrees in '..\..\LIBRARY\Virtual Treeview\Source\VirtualTrees.pas',
  VTAccessibilityFactory in '..\..\LIBRARY\Virtual Treeview\Source\VTAccessibilityFactory.pas',
  DGHEllipsisLabel in '..\..\Components\Source\DGHEllipsisLabel.pas',
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  DGHNumericEdit in '..\..\Components\Source\DGHNumericEdit.pas',
  CheckForUpdatesOptionsForm in '..\..\Library\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  SetEurekaLogState(DebugHook = 0);
  Application.Initialize;
  Application.Title := 'Profile Viewer';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
