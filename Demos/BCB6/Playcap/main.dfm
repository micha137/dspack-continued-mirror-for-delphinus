object MainForm: TMainForm
  Left = 485
  Top = 301
  Width = 325
  Height = 282
  Caption = 'Playcap'
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 317
    Height = 236
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
    Color = clBlack
    Align = alClient
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object Devices: TMenuItem
      Caption = 'Devices'
    end
  end
  object FilterGraph: TFilterGraph
    Mode = gmCapture
    GraphEdit = True
    Left = 40
    Top = 8
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 72
    Top = 8
  end
end
