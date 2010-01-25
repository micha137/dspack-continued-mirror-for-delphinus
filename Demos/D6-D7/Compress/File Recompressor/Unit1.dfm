object Form1: TForm1
  Left = 219
  Top = 157
  Width = 534
  Height = 538
  Caption = 'Simple recompressor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 106
    Top = 40
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 107
    Top = 96
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 114
    Height = 13
    Caption = '1 - Select the source file'
  end
  object Label4: TLabel
    Left = 8
    Top = 64
    Width = 133
    Height = 13
    Caption = '2 - Select the destination file'
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 119
    Height = 13
    Caption = '3 - Add Compressor filters'
  end
  object Label6: TLabel
    Left = 24
    Top = 144
    Width = 27
    Height = 13
    Caption = 'Video'
  end
  object Label7: TLabel
    Left = 24
    Top = 224
    Width = 27
    Height = 13
    Caption = 'Audio'
  end
  object Label8: TLabel
    Left = 8
    Top = 304
    Width = 85
    Height = 13
    Caption = '4 - Connect Filters'
  end
  object Label9: TLabel
    Left = 8
    Top = 360
    Width = 80
    Height = 13
    Caption = '5- Run the graph'
  end
  object Button1: TButton
    Left = 24
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Source File'
    TabOrder = 0
    OnClick = Button1Click
  end
  object debug: TMemo
    Left = 0
    Top = 416
    Width = 526
    Height = 95
    Align = alBottom
    ScrollBars = ssHorizontal
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 56
    Top = 144
    Width = 201
    Height = 73
    ItemHeight = 13
    TabOrder = 2
  end
  object ListBox2: TListBox
    Left = 320
    Top = 144
    Width = 201
    Height = 153
    ItemHeight = 13
    TabOrder = 3
  end
  object Button2: TButton
    Left = 24
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Dest. File'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 264
    Top = 152
    Width = 49
    Height = 25
    Caption = '>>'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 264
    Top = 184
    Width = 49
    Height = 25
    Caption = '<<'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 328
    Width = 89
    Height = 25
    Caption = 'Connect Filters'
    TabOrder = 7
    OnClick = Button5Click
  end
  object ListBox3: TListBox
    Left = 56
    Top = 224
    Width = 201
    Height = 73
    ItemHeight = 13
    TabOrder = 8
  end
  object Button6: TButton
    Left = 264
    Top = 232
    Width = 49
    Height = 25
    Caption = '>>'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 264
    Top = 264
    Width = 49
    Height = 25
    Caption = '<<'
    TabOrder = 10
    OnClick = Button4Click
  end
  object Button8: TButton
    Left = 24
    Top = 384
    Width = 89
    Height = 25
    Caption = 'GO'
    TabOrder = 11
    OnClick = Button8Click
  end
  object CaptureGraph: TFilterGraph
    AutoCreate = True
    Mode = gmCapture
    GraphEdit = True
    OnDSEvent = CaptureGraphDSEvent
    Left = 424
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Left = 456
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 488
    Top = 8
  end
end
