object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 215
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    396
    215)
  PixelsPerInch = 96
  TextHeight = 16
  object lblLow: TLabel
    Left = 8
    Top = 53
    Width = 23
    Height = 16
    Caption = '&Low'
  end
  object lblPercentage: TLabel
    Left = 82
    Top = 31
    Width = 64
    Height = 16
    Caption = 'Percentage'
  end
  object lblColour: TLabel
    Left = 273
    Top = 31
    Width = 115
    Height = 16
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Colour'
  end
  object lblMedium: TLabel
    Left = 8
    Top = 80
    Width = 45
    Height = 16
    Caption = '&Medium'
  end
  object lblHigh: TLabel
    Left = 8
    Top = 107
    Width = 25
    Height = 16
    Caption = '&High'
  end
  object lblLifeTime: TLabel
    Left = 8
    Top = 157
    Width = 299
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Life Time of Managed Nodes (in days)'
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
    Height = 24
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 1
    Text = '0'
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
    Width = 245
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edtMedium: TDGHEdit
    Left = 65
    Top = 77
    Width = 56
    Height = 24
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 4
    Text = '50'
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
    Width = 245
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object edtHigh: TDGHEdit
    Left = 65
    Top = 104
    Width = 56
    Height = 24
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 7
    Text = '100'
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
    Width = 245
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
  end
  object btnOK: TBitBtn
    Left = 232
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 13
  end
  object btnCancel: TBitBtn
    Left = 313
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 14
  end
  object chkSynchronise: TCheckBox
    Left = 8
    Top = 131
    Width = 380
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Synchronise Aggregate View Columns with the Profile Tree'
    TabOrder = 10
  end
  object edtLifeTime: TDGHEdit
    Left = 313
    Top = 152
    Width = 56
    Height = 24
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    ReadOnly = True
    TabOrder = 11
    Text = '100'
  end
  object udLifeTime: TUpDown
    Left = 372
    Top = 154
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtLifeTime
    Max = 365
    Position = 100
    TabOrder = 12
    OnChangingEx = udHighChangingEx
  end
end
