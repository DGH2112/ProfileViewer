(**
  
  This module contains the project definition for the application.

  @Version 1.0
  @Author David Hoyle
  @Date   20 Feb 2006

**)
program ProfileViewer;

{%TogetherDiagram 'ModelSupport_ProfileViewer\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\About\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\ProfileViewer\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\MainForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\DGHLibrary\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\Profiler System Use Case Diagram.txvuse'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\ProfileViewer\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\DGHLibrary\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\About\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\MainForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\Class Diagram.txvcls'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\Profiler Viewer Class Diagram.txvcls'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\Profile Viewer Use Case Diagram.txvuse'}
{%TogetherDiagram 'ModelSupport_ProfileViewer\Profile Viewer Class Diagram.txvcls'}

uses
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  About in '..\..\LIBRARY\About.pas',
  DGHLibrary in '..\..\LIBRARY\DGHLibrary.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VBA Profile Viewer';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
