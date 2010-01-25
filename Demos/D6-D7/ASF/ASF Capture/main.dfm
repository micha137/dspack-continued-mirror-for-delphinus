object VideoForm: TVideoForm
  Left = 272
  Top = 197
  Width = 319
  Height = 275
  Caption = 'ASF Capture & Stream'
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
  object VideoWindow: TVideoWindow
    Left = 0
    Top = 0
    Width = 311
    Height = 229
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
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
    Left = 104
    Top = 8
    object Devices: TMenuItem
      Caption = 'Devices'
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'BMP file|*.bmp'
    Left = 136
    Top = 8
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 72
    Top = 8
  end
  object ASFWriter: TASFWriter
    FilterGraph = FilterGraph
    Profile = wmp_V80_56VideoOnly
    FileName = 'c:\tmp.asf'
    Port = 3333
    MaxUsers = 8
    Left = 40
    Top = 8
  end
end
