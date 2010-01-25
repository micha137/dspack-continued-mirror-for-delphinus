object Form1: TForm1
  Left = 192
  Top = 107
  Width = 356
  Height = 207
  Caption = 'Select media file'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label2: TLabel
    Left = 184
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Max Client'
  end
  object Edit1: TEdit
    Left = 80
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 0
    Text = '8'#160'080'
  end
  object Edit2: TEdit
    Left = 240
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '8'
  end
  object UpDown1: TUpDown
    Left = 129
    Top = 8
    Width = 15
    Height = 21
    Associate = Edit1
    Min = 0
    Max = 32767
    Position = 8080
    TabOrder = 2
    Wrap = False
  end
  object UpDown2: TUpDown
    Left = 289
    Top = 8
    Width = 15
    Height = 21
    Associate = Edit2
    Min = 0
    Position = 8
    TabOrder = 3
    Wrap = False
  end
  object Button1: TButton
    Left = 64
    Top = 48
    Width = 73
    Height = 25
    Caption = 'Source File'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 48
    Width = 73
    Height = 25
    Caption = 'Run'
    Enabled = False
    TabOrder = 5
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 109
    Width = 348
    Height = 71
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object OpenDialog: TOpenDialog
    Left = 8
    Top = 112
  end
end
