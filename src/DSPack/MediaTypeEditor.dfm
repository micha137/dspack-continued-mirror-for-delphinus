object FormMediaType: TFormMediaType
  Left = 211
  Top = 142
  BorderStyle = bsToolWindow
  Caption = ' MediaType Editor'
  ClientHeight = 239
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Major Type'
  end
  object Label2: TLabel
    Left = 160
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Sub Type'
  end
  object Label3: TLabel
    Left = 160
    Top = 48
    Width = 58
    Height = 13
    Caption = 'Sample Size'
  end
  object Label4: TLabel
    Left = 160
    Top = 96
    Width = 59
    Height = 13
    Caption = 'Format Type'
  end
  object lblFormatSize: TLabel
    Left = 8
    Top = 96
    Width = 55
    Height = 13
    Caption = 'Format Size'
  end
  object btOK: TButton
    Left = 8
    Top = 208
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 0
    OnClick = btOKClick
  end
  object btCancel: TButton
    Left = 232
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbMajorTypes: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbMajorTypesChange
  end
  object cbSubTypes: TComboBox
    Left = 160
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbSubTypesChange
  end
  object chkFixedSize: TCheckBox
    Left = 8
    Top = 56
    Width = 145
    Height = 17
    Caption = 'Fixed Size Samples'
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 4
    OnClick = chkFixedSizeClick
  end
  object chkTempCompress: TCheckBox
    Left = 8
    Top = 72
    Width = 145
    Height = 17
    Caption = 'Temporal Compression'
    TabOrder = 5
    OnClick = chkTempCompressClick
  end
  object edSampleSize: TEdit
    Left = 160
    Top = 64
    Width = 145
    Height = 21
    TabOrder = 6
    Text = '0'
    OnChange = edSampleSizeChange
  end
  object cbFormatType: TComboBox
    Left = 160
    Top = 112
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    OnChange = cbFormatTypeChange
  end
  object Memo1: TMemo
    Left = 8
    Top = 144
    Width = 297
    Height = 59
    ReadOnly = True
    TabOrder = 8
  end
  object edFormatSize: TEdit
    Left = 8
    Top = 112
    Width = 145
    Height = 21
    Enabled = False
    TabOrder = 9
    Text = '0'
  end
end
