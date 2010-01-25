object VideoForm: TVideoForm
  Left = 199
  Top = 131
  Width = 319
  Height = 275
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 311
    Height = 229
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowless
    Color = clBlack
    Align = alClient
  end
  object FilterGraph: TFilterGraph
    Mode = gmCapture
    GraphEdit = True
    Left = 8
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 72
    object Devices: TMenuItem
      Caption = 'Devices'
    end
    object Blend1: TMenuItem
      Caption = 'Blend'
      object ext1: TMenuItem
        Caption = 'Text'
        OnClick = ext1Click
      end
      object Bitmap1: TMenuItem
        Caption = 'Bitmap'
        OnClick = Bitmap1Click
      end
    end
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 8
    Top = 40
  end
  object OpenDialog: TOpenDialog
    Filter = 'BMP file|*.bmp'
    Left = 8
    Top = 104
  end
end
