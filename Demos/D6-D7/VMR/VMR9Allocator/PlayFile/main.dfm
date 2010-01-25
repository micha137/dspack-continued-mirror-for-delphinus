object MainForm: TMainForm
  Left = 198
  Top = 107
  Width = 340
  Height = 324
  Caption = 'VMR 9 Allocator'
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
    Width = 332
    Height = 278
    Mode = vmVMR
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrRenderless
    VMROptions.Streams = 1
    Color = clBlack
    Align = alClient
  end
  object FilterGraph: TFilterGraph
    GraphEdit = False
    Left = 8
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Left = 40
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 72
    Top = 8
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuOpen: TMenuItem
        Caption = '&open'
        OnClick = MenuOpenClick
      end
    end
  end
end
