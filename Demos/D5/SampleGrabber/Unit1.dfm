object Form1: TForm1
  Left = 198
  Top = 114
  Width = 367
  Height = 242
  Caption = 'Simple SnapShot'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 176
    Top = 8
    Width = 153
    Height = 121
    Stretch = True
  end
  object VideoWindow: TVideoWindow
    Left = 8
    Top = 8
    Width = 160
    Height = 120
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
    VMROptions.Streams = 1
    VMROptions.Preferences = []
    Color = clBlack
  end
  object OpenPlay: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Open - Play'
    TabOrder = 1
    OnClick = OpenPlayClick
  end
  object Snapshot: TButton
    Left = 176
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Snapshot'
    TabOrder = 2
    OnClick = SnapshotClick
  end
  object CallBack: TCheckBox
    Left = 176
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Callback Event'
    TabOrder = 3
  end
  object FilterGraph: TFilterGraph
    LogFile = 'c:\dxlog.txt'
    AutoCreate = True
    GraphEdit = True
    Left = 16
    Top = 16
  end
  object SampleGrabber: TSampleGrabber
    OnBuffer = SampleGrabberBuffer
    FilterGraph = FilterGraph
    MediaType.data = {
      7669647300001000800000AA00389B717EEB36E44F52CE119F530020AF0BA770
      FFFFFFFF0000000001000000809F580556C3CE11BF0100AA0055595A00000000
      0000000000000000}
    Left = 184
    Top = 16
  end
  object OpenDialog: TOpenDialog
    Left = 16
    Top = 80
  end
end
