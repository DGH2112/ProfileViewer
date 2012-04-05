(**
  
  This module contains a form to configure the options.

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Apr 2012

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
    FLifeTime : Integer;
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
    lblLifeTime: TLabel;
    edtLifeTime: TDGHEdit;
    udLifeTime: TUpDown;
    btnCheckForUpdates: TBitBtn;
    procedure udLowChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udHighChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure udMediumChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure btnCheckForUpdatesClick(Sender: TObject);
  private
    { Private declarations }
    FINIFileName : String;
  public
    { Public declarations }
    Class Procedure Execute(strINIFileName : String; var Options : TOptions);
  end;

implementation

uses CheckForUpdatesOptionsForm;

{$R *.dfm}

{ TfrmOptions }

(**

  This is an on click event handler for the CheckForUpdates button.

  @precon  None.
  @postcon Displays a form in which the user can configure the check for updates
           functionality.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnCheckForUpdatesClick(Sender: TObject);
begin
  TfrmCheckForUpdatesOptions.Execute(FINIFileName);
end;

(**

  This is the forms main interface method for invoking the options.

  @precon  None.
  @postcon If the dialogue is confirmed the options variable is updated.

  @param   strINIFileName as a String
  @param   Options        as a TOptions as a reference

**)
Class procedure TfrmOptions.Execute(strINIFileName : String; var Options: TOptions);

begin
  With TfrmOptions.Create(Nil) Do
    Try
      FINIFileName := strINIFileName;
      chkColorization.Checked := Options.FColourization;
      udLow.Position := Options.FLowPercentage;
      clbxLow.Selected := Options.FLowColour;
      udMedium.Position := Options.FMediumPercentage;
      clbxMedium.Selected := Options.FMediumColour;
      udHigh.Position := Options.FHighPercentage;
      clbxHigh.Selected := Options.FHighColour;
      chkSynchronise.Checked := Options.FSynchronise;
      udLifeTime.Position := Options.FLifeTime;
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
          Options.FLifeTime := udLifeTime.Position;
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
