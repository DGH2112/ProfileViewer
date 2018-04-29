(**
  
  This module contains a form to configure the options.

  @Author  David Hoyle
  @Version 1.0
  @Date    29 Apr 2018

**)
Unit OptionsForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Buttons;

Type
  (** A record to describe the applications options. **)
  TOptions = Record
    FColourization: Boolean;
    FLowColour: TColor;
    FLowPercentage: TColor;
    FMediumColour: TColor;
    FMediumPercentage: TColor;
    FHighColour: TColor;
    FHighPercentage: TColor;
    FSynchronise: Boolean;
    FLifeTime: Integer;
  End;

  (** A class to represent the form interface. **)
  TfrmOptions = Class(TForm)
    chkColorization: TCheckBox;
    lblLow: TLabel;
    lblPercentage: TLabel;
    lblColour: TLabel;
    udLow: TUpDown;
    clbxLow: TColorBox;
    lblMedium: TLabel;
    udMedium: TUpDown;
    clbxMedium: TColorBox;
    lblHigh: TLabel;
    udHigh: TUpDown;
    clbxHigh: TColorBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkSynchronise: TCheckBox;
    lblLifeTime: TLabel;
    udLifeTime: TUpDown;
    edtLow: TEdit;
    edtMedium: TEdit;
    edtHigh: TEdit;
    edtLifeTime: TEdit;
    Procedure udLowChangingEx(Sender: TObject; Var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    Procedure udHighChangingEx(Sender: TObject; Var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    Procedure udMediumChangingEx(Sender: TObject; Var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
  Private
    { Private declarations }
    FINIFileName: String;
  Public
    { Public declarations }
    Class Procedure Execute(Const strINIFileName: String; Var Options: TOptions);
  End;

Implementation

{$R *.dfm}

(**

  This is the forms main interface method for invoking the options.

  @precon  None.
  @postcon If the dialogue is confirmed the options variable is updated.

  @param   strINIFileName as a String as a constant
  @param   Options        as a TOptions as a reference

**)
Class Procedure TfrmOptions.Execute(Const strINIFileName: String; Var Options: TOptions);

Var
  F: TfrmOptions;

Begin
  F := TfrmOptions.Create(Nil);
  Try
    F.FINIFileName := strINIFileName;
    F.chkColorization.Checked := Options.FColourization;
    F.udLow.Position := Options.FLowPercentage;
    F.clbxLow.Selected := Options.FLowColour;
    F.udMedium.Position := Options.FMediumPercentage;
    F.clbxMedium.Selected := Options.FMediumColour;
    F.udHigh.Position := Options.FHighPercentage;
    F.clbxHigh.Selected := Options.FHighColour;
    F.chkSynchronise.Checked := Options.FSynchronise;
    F.udLifeTime.Position := Options.FLifeTime;
    If F.ShowModal = mrOK Then
      Begin
        Options.FColourization := F.chkColorization.Checked;
        Options.FLowPercentage := F.udLow.Position;
        Options.FLowColour := F.clbxLow.Selected;
        Options.FMediumPercentage := F.udMedium.Position;
        Options.FMediumColour := F.clbxMedium.Selected;
        Options.FHighPercentage := F.udHigh.Position;
        Options.FHighColour := F.clbxHigh.Selected;
        Options.FSynchronise := F.chkSynchronise.Checked;
        Options.FLifeTime := F.udLifeTime.Position;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This is an on change event handler for the High UpDown Spin Button.

  @precon  None.
  @postcon Changes the High values as long as it is between Medium and 100 exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as an Integer
  @param   Direction   as a TUpDownDirection

**)
Procedure TfrmOptions.udHighChangingEx(Sender: TObject;
  Var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
Const
  iUpperLimit = 100;

Begin
  Case Direction Of
    updUp: AllowChange := udHigh.Position < iUpperLimit;
    updDown: AllowChange := udHigh.Position > udMedium.Position + 1;
  End;
End;

(**

  This is an on change event handler for the Low UpDown Spin Button.

  @precon  None.
  @postcon Changes the Low values as long as it is between 0 and Medium exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as an Integer
  @param   Direction   as a TUpDownDirection

**)
Procedure TfrmOptions.udLowChangingEx(Sender: TObject; Var AllowChange: Boolean;
  NewValue: Integer; Direction: TUpDownDirection);
Begin
  Case Direction Of
    updUp: AllowChange := udLow.Position < udMedium.Position - 1;
    updDown: AllowChange := udLow.Position > 0;
  End;
End;

(**

  This is an on change event handler for the Medium UpDown Spin Button.

  @precon  None.
  @postcon Changes the Medium values as long as it is between Low and High exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as an Integer
  @param   Direction   as a TUpDownDirection

**)
Procedure TfrmOptions.udMediumChangingEx(Sender: TObject;
  Var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
Begin
  Case Direction Of
    updUp: AllowChange := udMedium.Position < udHigh.Position - 1;
    updDown: AllowChange := udMedium.Position > udLow.Position + 1;
  End;
End;

End.
