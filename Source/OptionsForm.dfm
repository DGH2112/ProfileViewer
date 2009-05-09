object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 215
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
    215)
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
    Left = 279
    Top = 31
    Width = 31
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Colour'
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
  object lblLifeTime: TLabel
    Left = 8
    Top = 157
    Width = 180
    Height = 13
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
  end
  object btnOK: TBitBtn
    Left = 154
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 13
    Kind = bkOK
    ExplicitTop = 206
  end
  object btnCancel: TBitBtn
    Left = 235
    Top = 182
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 14
    Kind = bkCancel
    ExplicitTop = 206
  end
  object chkSynchronise: TCheckBox
    Left = 8
    Top = 131
    Width = 302
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Synchronise Aggregate View Columns with the Profile Tree'
    TabOrder = 10
  end
  object edtLifeTime: TDGHEdit
    Left = 236
    Top = 154
    Width = 56
    Height = 21
    ReadOnly = True
    TabOrder = 11
    Text = '100'
    Alignment = taRightJustify
  end
  object udLifeTime: TUpDown
    Left = 292
    Top = 154
    Width = 16
    Height = 21
    Associate = edtLifeTime
    Max = 365
    Position = 100
    TabOrder = 12
    OnChangingEx = udHighChangingEx
  end
end
