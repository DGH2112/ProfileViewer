(**

  This is a generic progress dialogue for use in the ObjectPascalDocWizard.

  @version    1.0
  @date       29 Apr 2018
  @author     David Hoyle

**)
Unit ProgressForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons;

Type
  (**

    This class represents a modeless progress dialogue for use throughout the
    application.

  **)
  TfrmProgress = Class(TForm)
    pnlPanel1: TPanel;
    prbProgressBar1: TProgressBar;
    pnlButton: TPanel;
    btnCancel: TBitBtn;
    lblInfo: TLabel;
    Procedure btnCancelClick(Sender: TObject);
  Private
  Public
    Procedure Init(Const iMax: Integer; Const strTitle, strMsg: String);
    Procedure UpdateProgress(Const iPosition: Integer; Const strMsg: String);
  End;

Implementation

uses
  System.UITypes;

{$R *.DFM}


(**

  This is an OnFormCreate Event Hanlder for the TfrmProgress class.

  @precon  None.
  @postcon Creates an ellipsis path control and aligns it to the client area.

  @param   Sender as a TObject

**)
Procedure TfrmProgress.btnCancelClick(Sender: TObject);

Const
  strMsg = 'Are you sure you want to cancel the scanning and parsing?';

Begin
  If MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Abort;
End;

(**

  This method initialises the progress dialogue by adding a message to the form and setting the maximum 
  amount of progress, then show the dialogue on the screen.

  @precon  iMax is the maximum range of the progress meter, strTitle is the title of the dialogue and 
           strMsg is the initial message in the dialogue.
  @postcon Initialises the progress form.

  @param   iMax     as an Integer as a constant
  @param   strTitle as a String as a constant
  @param   strMsg   as a String as a constant

**)
Procedure TfrmProgress.Init(Const iMax: Integer; Const strTitle, strMsg: String);
Begin
  Caption := strTitle;
  lblInfo.Caption := strMsg;
  If iMax > 0 Then
    Begin
      prbProgressBar1.Style := pbstNormal;
      prbProgressBar1.Max := iMax;
    End
  Else
    prbProgressBar1.Style := pbstMarquee;
  prbProgressBar1.Position := 0;
  Show;
  Application.ProcessMessages;
End;

(**

  This method of the form updates the progress meter.

  @precon  iPosition is the updated position of the progress meter and strMsg is an updated message for 
           the dialogue.
  @postcon Updates the display with progress.

  @param   iPosition as an Integer as a constant
  @param   strMsg    as a String as a constant

**)
Procedure TfrmProgress.UpdateProgress(Const iPosition: Integer; Const strMsg: String);

Begin
  If prbProgressBar1.Style = pbstNormal Then
    Begin
      prbProgressBar1.Position := iPosition;
      prbProgressBar1.Position := iPosition - 1; //FI:W508
      prbProgressBar1.Position := iPosition; //FI:W508
      lblInfo.Caption := strMsg;
    End;
  Application.ProcessMessages;
End;

End.
