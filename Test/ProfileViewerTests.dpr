//: @stopdocumentation
Program ProfileViewerTests;

Uses
  TestInsight.DUnit,
  Test.ProfileViewer.Functions In 'Source\Test.ProfileViewer.Functions.pas',
  ProfileViewer.Functions In '..\Source\ProfileViewer.Functions.pas';

{$R *.RES}


Begin
  RunRegisteredTests;
End.

