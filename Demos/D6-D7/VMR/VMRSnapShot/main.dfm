object Form1: TForm1
  Left = 198
  Top = 114
  Width = 687
  Height = 310
  Caption = 'WindowLess Sample'
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
  object SnapShot: TImage
    Left = 336
    Top = 0
    Width = 337
    Height = 241
    Stretch = True
  end
  object Label1: TLabel
    Left = 88
    Top = 248
    Width = 247
    Height = 13
    Caption = 'The VMR Snapshot only work in WindowLess Mode'
  end
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 329
    Height = 241
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowless
    VMROptions.Streams = 1
    Color = clBlack
  end
  object Button2: TButton
    Left = 440
    Top = 248
    Width = 97
    Height = 25
    Caption = 'VMR SnapShot'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 5
    Top = 247
    Width = 75
    Height = 25
    Caption = 'Open-Play'
    TabOrder = 2
    OnClick = Button1Click
  end
  object FilterGraph: TFilterGraph
    AutoCreate = True
    GraphEdit = False
    Left = 8
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Left = 8
    Top = 40
  end
end
