(**
  
  This module contains a form to configure the options.

  @Author  David Hoyle
  @Version 1.0
  @Date    08 May 2009

**)
unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, DGHNumericEdit, Buttons;

type
  (** A record to describe the applications options. **)
  TOptions = Record
    FColourization : Boolean;
    FLowColour : TColor;
    FLowPercentage : Tcolor;
    FMediumColour : TColor;
    FMediumPercentage : Tcolor;
    FHighColour : TColor;
    FHighPercentage : Tcolor;
    FSynchronise : Boolean;
  End;

  (** A class to represent the form interface. **)
  TfrmOptions = class(TForm)
    chkColorization: TCheckBox;
    lblLow: TLabel;
    lblPercentage: TLabel;
    lblColour: TLabel;
    edtLow: TDGHEdit;
    udLow: TUpDown;
    clbxLow: TColorBox;
    lblMedium: TLabel;
    edtMedium: TDGHEdit;
    udMedium: TUpDown;
    clbxMedium: TColorBox;
    lblHigh: TLabel;
    edtHigh: TDGHEdit;
    udHigh: TUpDown;
    clbxHigh: TColorBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkSynchronise: TCheckBox;
    procedure udLowChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udHighChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udMediumChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Procedure Execute(var Options : TOptions);
  end;

implementation

{$R *.dfm}

{ TfrmOptions }

(**

  This is the forms main interface method for invoking the options.

  @precon  None.
  @postcon If the dialogue is confirmed the options variable is updated.

  @param   Options as a TOptions as a reference

**)
Class procedure TfrmOptions.Execute(var Options: TOptions);

begin
  With TfrmOptions.Create(Nil) Do
    Try
      chkColorization.Checked := Options.FColourization;
      udLow.Position := Options.FLowPercentage;
      clbxLow.Selected := Options.FLowColour;
      udMedium.Position := Options.FMediumPercentage;
      clbxMedium.Selected := Options.FMediumColour;
      udHigh.Position := Options.FHighPercentage;
      clbxHigh.Selected := Options.FHighColour;
      chkSynchronise.Checked := Options.FSynchronise;
      If ShowModal = mrOK Then
        Begin
          Options.FColourization := chkColorization.Checked;
          Options.FLowPercentage := udLow.Position;
          Options.FLowColour := clbxLow.Selected;
          Options.FMediumPercentage := udMedium.Position;
          Options.FMediumColour := clbxMedium.Selected;
          Options.FHighPercentage := udHigh.Position;
          Options.FHighColour := clbxHigh.Selected;
          Options.FSynchronise := chkSynchronise.Checked;
        End;
    Finally
      Free;
    End;
end;

(**

  This is an on change event handler for the High UpDown Spin Button.

  @precon  None.
  @postcon Changes the High values as long as it is between Medium and 100
           exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmOptions.udHighChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  Case Direction Of
    updUp  : AllowChange := udHigh.Position < 100;
    updDown: AllowChange := udHigh.Position > udMedium.Position + 1;
  End;
end;

(**

  This is an on change event handler for the Low UpDown Spin Button.

  @precon  None.
  @postcon Changes the Low values as long as it is between 0 and Medium
           exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmOptions.udLowChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: Smallint; Direction: TUpDownDirection);
begin
  Case Direction Of
    updUp  : AllowChange := udLow.Position < udMedium.Position - 1;
    updDown: AllowChange := udLow.Position > 0;
  End;
end;

(**

  This is an on change event handler for the Medium UpDown Spin Button.

  @precon  None.
  @postcon Changes the Medium values as long as it is between Low and High
           exclusive.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as a Smallint
  @param   Direction   as a TUpDownDirection

**)
procedure TfrmOptions.udMediumChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  Case Direction Of
    updUp  : AllowChange := udMedium.Position < udHigh.Position - 1;
    updDown: AllowChange := udMedium.Position > udLow.Position + 1;
  End;
end;

end.
