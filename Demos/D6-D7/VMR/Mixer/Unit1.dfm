object FormVMRMixer: TFormVMRMixer
  Left = 198
  Top = 114
  Width = 468
  Height = 411
  Caption = 'VMRMixer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 344
    Width = 27
    Height = 13
    Caption = 'Alpha'
  end
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 425
    Height = 313
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
    VMROptions.Streams = 2
    Color = clBlack
  end
  object btRender: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Render File ...'
    TabOrder = 1
    OnClick = btRenderClick
  end
  object Alpha: TTrackBar
    Left = 128
    Top = 344
    Width = 150
    Height = 23
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = AlphaChange
  end
  object CoordX: TTrackBar
    Left = 1
    Top = 320
    Width = 424
    Height = 23
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = CoordChange
  end
  object CoordY: TTrackBar
    Left = 434
    Top = 0
    Width = 23
    Height = 313
    Max = 100
    Orientation = trVertical
    Frequency = 1
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = CoordChange
  end
  object FilterGraph: TFilterGraph
    LogFile = 'c:\log.txt'
    AutoCreate = True
    GraphEdit = True
    Left = 8
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Left = 8
    Top = 40
  end
end
