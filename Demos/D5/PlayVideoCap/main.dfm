object VideoForm: TVideoForm
  Left = 198
  Top = 114
  Width = 776
  Height = 378
  Caption = 'Video Capture Devices'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 384
    Top = 0
    Width = 384
    Height = 288
    Stretch = True
  end
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 384
    Height = 288
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
    VMROptions.Streams = 1
    Color = clBlack
  end
  object SnapShot: TButton
    Left = 392
    Top = 300
    Width = 75
    Height = 25
    Caption = 'SnapShot'
    TabOrder = 1
    OnClick = SnapShotClick
  end
  object CallBack: TCheckBox
    Left = 280
    Top = 304
    Width = 97
    Height = 17
    Caption = 'CallBack'
    TabOrder = 2
  end
  object FilterGraph: TFilterGraph
    Mode = gmCapture
    GraphEdit = True
    Left = 8
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 104
    object Devices: TMenuItem
      Caption = 'Devices'
    end
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 8
    Top = 40
  end
  object SampleGrabber: TSampleGrabber
    OnBuffer = SampleGrabberBuffer
    FilterGraph = FilterGraph
    MediaType.data = {
      7669647300001000800000AA00389B717DEB36E44F52CE119F530020AF0BA770
      FFFFFFFF0000000001000000809F580556C3CE11BF0100AA0055595A00000000
      0000000000000000}
    Left = 8
    Top = 72
  end
end
