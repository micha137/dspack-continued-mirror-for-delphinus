object FormEditor: TFormEditor
  Left = 281
  Top = 269
  BorderStyle = bsDialog
  Caption = 'Filter Editor'
  ClientHeight = 237
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 421
    Height = 237
    ActivePage = Selector
    Align = alClient
    TabOrder = 0
    object Selector: TTabSheet
      Caption = 'Selector'
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Categories'
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 27
        Height = 13
        Caption = 'Filters'
      end
      object Label3: TLabel
        Left = 240
        Top = 8
        Width = 47
        Height = 13
        Caption = 'Interfaces'
      end
      object cbCategories: TComboBox
        Left = 8
        Top = 24
        Width = 225
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbCategoriesChange
      end
      object lbFilters: TListBox
        Left = 8
        Top = 64
        Width = 225
        Height = 137
        ItemHeight = 13
        PopupMenu = PopupMenu
        TabOrder = 1
        OnClick = lbFiltersClick
      end
      object Interfaces: TListBox
        Left = 240
        Top = 24
        Width = 169
        Height = 177
        ItemHeight = 13
        TabOrder = 2
        OnDblClick = InterfacesDblClick
      end
    end
    object Pins: TTabSheet
      Caption = 'Pins'
      ImageIndex = 1
      OnShow = PinsShow
      object Label4: TLabel
        Left = 0
        Top = 8
        Width = 20
        Height = 13
        Caption = 'Pins'
      end
      object Label5: TLabel
        Left = 152
        Top = 8
        Width = 47
        Height = 13
        Caption = 'Interfaces'
      end
      object lbPins: TListBox
        Left = 0
        Top = 24
        Width = 145
        Height = 97
        ItemHeight = 13
        PopupMenu = PinMenu
        TabOrder = 0
        OnClick = lbPinsClick
      end
      object PinInterfaces: TListBox
        Left = 152
        Top = 24
        Width = 177
        Height = 97
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = PinInterfacesDblClick
      end
      object MediaTypes: TListBox
        Left = 0
        Top = 128
        Width = 409
        Height = 73
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
  object FilterGraph: TFilterGraph
    AutoCreate = True
    GraphEdit = False
    Left = 16
    Top = 96
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 48
    Top = 96
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 80
    Top = 96
    object PropertyPage: TMenuItem
      Caption = 'Property Page'
      OnClick = PropertyPageClick
    end
    object VFWCapture: TMenuItem
      Caption = 'VFW Capture'
      object VFWFormat: TMenuItem
        Caption = 'Format'
        OnClick = VFWFormatClick
      end
      object VFWSource: TMenuItem
        Caption = 'Source'
        OnClick = VFWSourceClick
      end
      object VFWDisplay: TMenuItem
        Caption = 'Display'
        OnClick = VFWDisplayClick
      end
    end
    object VFWConfig: TMenuItem
      Caption = 'VFW Compress'
      object Config1: TMenuItem
        Caption = 'Config'
        OnClick = Config1Click
      end
      object VFWAbout: TMenuItem
        Caption = 'About'
        OnClick = VFWAboutClick
      end
    end
  end
  object PinMenu: TPopupMenu
    OnPopup = PinMenuPopup
    Left = 112
    Top = 96
    object PinProperty: TMenuItem
      Caption = 'Property Page'
      OnClick = PinPropertyClick
    end
  end
end
