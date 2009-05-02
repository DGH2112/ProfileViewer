object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'frmOptions'
  ClientHeight = 171
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    318
    171)
  PixelsPerInch = 96
  TextHeight = 13
  object lblLow: TLabel
    Left = 8
    Top = 53
    Width = 19
    Height = 13
    Caption = '&Low'
  end
  object lblPercentage: TLabel
    Left = 82
    Top = 31
    Width = 55
    Height = 13
    Caption = 'Percentage'
  end
  object lblColour: TLabel
    Left = 143
    Top = 31
    Width = 167
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Colour'
    ExplicitWidth = 384
  end
  object lblMedium: TLabel
    Left = 8
    Top = 80
    Width = 36
    Height = 13
    Caption = '&Medium'
  end
  object lblHigh: TLabel
    Left = 8
    Top = 107
    Width = 21
    Height = 13
    Caption = '&High'
  end
  object chkColorization: TCheckBox
    Left = 8
    Top = 8
    Width = 129
    Height = 17
    Caption = '&Enable Colorization'
    TabOrder = 0
  end
  object edtLow: TDGHEdit
    Left = 65
    Top = 50
    Width = 56
    Height = 21
    ReadOnly = True
    TabOrder = 1
    Text = '0'
    Alignment = taRightJustify
  end
  object udLow: TUpDown
    Left = 121
    Top = 50
    Width = 16
    Height = 21
    Associate = edtLow
    TabOrder = 2
    OnChangingEx = udLowChangingEx
  end
  object clbxLow: TColorBox
    Left = 143
    Top = 50
    Width = 167
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 3
    ExplicitWidth = 384
  end
  object edtMedium: TDGHEdit
    Left = 65
    Top = 77
    Width = 56
    Height = 21
    ReadOnly = True
    TabOrder = 4
    Text = '50'
    Alignment = taRightJustify
  end
  object udMedium: TUpDown
    Left = 121
    Top = 77
    Width = 16
    Height = 21
    Associate = edtMedium
    Position = 50
    TabOrder = 5
    OnChangingEx = udMediumChangingEx
  end
  object clbxMedium: TColorBox
    Left = 143
    Top = 78
    Width = 167
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 6
    ExplicitWidth = 384
  end
  object edtHigh: TDGHEdit
    Left = 65
    Top = 104
    Width = 56
    Height = 21
    ReadOnly = True
    TabOrder = 7
    Text = '100'
    Alignment = taRightJustify
  end
  object udHigh: TUpDown
    Left = 121
    Top = 104
    Width = 16
    Height = 21
    Associate = edtHigh
    Position = 100
    TabOrder = 8
    OnChangingEx = udHighChangingEx
  end
  object clbxHigh: TColorBox
    Left = 143
    Top = 106
    Width = 167
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 9
    ExplicitWidth = 384
  end
  object btnOK: TBitBtn
    Left = 147
    Top = 138
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 10
    Kind = bkOK
    ExplicitLeft = 143
    ExplicitTop = 308
  end
  object btnCancel: TBitBtn
    Left = 235
    Top = 138
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 11
    Kind = bkCancel
    ExplicitLeft = 231
    ExplicitTop = 308
  end
end
