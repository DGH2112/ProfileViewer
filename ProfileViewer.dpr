program ProfileViewer;

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
