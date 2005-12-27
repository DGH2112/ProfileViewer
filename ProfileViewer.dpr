program ProfileViewer;

uses
  Forms,
  MainForm in 'Source\MainForm.pas' {frmMainForm},
  DGHLibrary50 in '..\..\LIBRARY\DGHLibrary50.pas',
  About in '..\..\LIBRARY\About.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VBA Profile Viewer';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
