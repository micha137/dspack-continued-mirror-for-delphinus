object VideoForm: TVideoForm
  Left = 315
  Top = 209
  Width = 322
  Height = 279
  Caption = 'Video Capture Devices'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 314
    Height = 233
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrRenderless
    VMROptions.Streams = 1
    Color = clBlack
    Align = alClient
  end
  object FilterGraph: TFilterGraph
    Mode = gmCapture
    GraphEdit = True
    Left = 8
    Top = 8
  end
  object MainMenu: TMainMenu
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
end
