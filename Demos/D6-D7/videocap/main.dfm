object MainForm: TMainForm
  Left = 192
  Top = 114
  Width = 704
  Height = 530
  Caption = 'Video Cap'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object OutPutFileName: TLabel
    Left = 88
    Top = 296
    Width = 67
    Height = 13
    Caption = 'c:\capture.avi'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Video'
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 27
    Height = 13
    Caption = 'Audio'
  end
  object Label3: TLabel
    Left = 168
    Top = 8
    Width = 70
    Height = 13
    Caption = 'Select Formats'
  end
  object Label4: TLabel
    Left = 168
    Top = 128
    Width = 70
    Height = 13
    Caption = 'Select Formats'
  end
  object VideoWindow: TVideoWindow
    Left = 224
    Top = 248
    Width = 273
    Height = 217
    FilterGraph = CaptureGraph
    VMROptions.Mode = vmrWindowed
    VMROptions.Streams = 1
    VMROptions.Preferences = []
    Color = clBlack
  end
  object VideoCapFilters: TListBox
    Left = 8
    Top = 24
    Width = 153
    Height = 97
    ItemHeight = 13
    TabOrder = 1
    OnClick = VideoCapFiltersClick
  end
  object StartButton: TButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Start'
    Enabled = False
    TabOrder = 2
    OnClick = StartButtonClick
  end
  object CapFileButton: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Capture file'
    TabOrder = 3
    OnClick = CapFileButtonClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 484
    Width = 696
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object StopButton: TButton
    Left = 8
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = StopButtonClick
  end
  object AudioCapFilters: TListBox
    Left = 8
    Top = 144
    Width = 153
    Height = 97
    ItemHeight = 13
    TabOrder = 6
    OnClick = AudioCapFiltersClick
  end
  object VideoFormats: TListBox
    Left = 168
    Top = 24
    Width = 521
    Height = 97
    ItemHeight = 13
    TabOrder = 7
  end
  object AudioFormats: TListBox
    Left = 168
    Top = 144
    Width = 521
    Height = 97
    ItemHeight = 13
    TabOrder = 8
  end
  object InputLines: TComboBox
    Left = 8
    Top = 248
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 9
  end
  object CaptureGraph: TFilterGraph
    Mode = gmCapture
    GraphEdit = True
    Left = 232
    Top = 256
  end
  object VideoSourceFilter: TFilter
    BaseFilter.data = {00000000}
    Left = 16
    Top = 32
  end
  object SaveDialog: TSaveDialog
    Left = 168
    Top = 296
  end
  object Timer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = TimerTimer
    Left = 264
    Top = 256
  end
  object AudioSourceFilter: TFilter
    BaseFilter.data = {00000000}
    Left = 16
    Top = 152
  end
end
