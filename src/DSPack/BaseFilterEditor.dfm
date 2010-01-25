object FormBaseFilter: TFormBaseFilter
  Left = 504
  Top = 199
  Width = 419
  Height = 350
  BorderStyle = bsSizeToolWin
  Caption = 'Base Filter Editor'
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
  object InfoBtn: TSpeedButton
    Left = 9
    Top = 295
    Width = 24
    Height = 24
    Cursor = crHandPoint
    Hint = 'Show Base Filter Editor info'
    Anchors = [akRight, akBottom]
    Flat = True
    ParentShowHint = False
    ShowHint = True
    OnClick = InfoBtnClick
  end
  object btOK: TButton
    Left = 234
    Top = 295
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 322
    Top = 295
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 411
    Height = 292
    ActivePage = InfoSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object SelectorSheet: TTabSheet
      Caption = 'Selector'
      object Label1: TLabel
        Left = 8
        Top = 2
        Width = 30
        Height = 13
        Caption = 'Filters:'
      end
      object Label2: TLabel
        Left = 256
        Top = 0
        Width = 96
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Available Interfaces:'
      end
      object Label6: TLabel
        Left = 256
        Top = 150
        Width = 23
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Pins:'
      end
      object Filters: TTreeView
        Left = 0
        Top = 17
        Width = 252
        Height = 188
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ParentShowHint = False
        PopupMenu = PopupMenu
        ReadOnly = True
        ShowHint = True
        TabOrder = 0
        OnChange = FiltersChange
        OnCustomDrawItem = FiltersCustomDrawItem
        OnDblClick = FiltersDblClick
      end
      object Interfaces: TListBox
        Left = 257
        Top = 16
        Width = 145
        Height = 130
        Anchors = [akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
        OnDblClick = InterfacesDblClick
      end
      object Pins: TListBox
        Left = 257
        Top = 165
        Width = 145
        Height = 98
        Anchors = [akRight, akBottom]
        ItemHeight = 14
        PopupMenu = PinMenu
        Style = lbOwnerDrawVariable
        TabOrder = 2
        OnDrawItem = PinsDrawItem
        OnMeasureItem = PinsMeasureItem
      end
      object MonikerTag: TMemo
        Left = 0
        Top = 212
        Width = 252
        Height = 50
        Hint = 'Filter Moniker.'
        TabStop = False
        Anchors = [akLeft, akRight, akBottom]
        Color = clBtnFace
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 3
      end
    end
    object PinsSheet: TTabSheet
      Caption = 'Pins Details'
      ImageIndex = 1
      OnShow = PinsSheetShow
      object Label3: TLabel
        Left = 1
        Top = 0
        Width = 23
        Height = 13
        Caption = 'Pins:'
      end
      object Label4: TLabel
        Left = 161
        Top = 0
        Width = 50
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Interfaces:'
      end
      object Label5: TLabel
        Left = 1
        Top = 149
        Width = 61
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'MediaTypes:'
      end
      object Pins1: TListBox
        Left = 1
        Top = 16
        Width = 153
        Height = 129
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        PopupMenu = PinMenu
        Style = lbOwnerDrawVariable
        TabOrder = 0
        OnClick = Pins1Click
        OnDrawItem = PinsDrawItem
        OnMeasureItem = PinsMeasureItem
      end
      object PinInterfaces: TListBox
        Left = 161
        Top = 16
        Width = 241
        Height = 129
        Anchors = [akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = PinInterfacesDblClick
      end
      object MediaTypes: TListBox
        Left = 1
        Top = 165
        Width = 400
        Height = 97
        Anchors = [akLeft, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object InfoSheet: TTabSheet
      Caption = 'Info'
      ImageIndex = 2
      TabVisible = False
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 403
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 5
        Color = clHighlight
        TabOrder = 0
        object Label16: TLabel
          Left = 5
          Top = 5
          Width = 393
          Height = 22
          Align = alTop
          Alignment = taCenter
          Caption = 'Base Filter Editor'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 5
          Top = 27
          Width = 393
          Height = 13
          Align = alTop
          Alignment = taCenter
          Caption = 'Version 1.2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 49
        Width = 403
        Height = 215
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 2
        Color = clWindow
        TabOrder = 1
        object Label7: TLabel
          Left = 8
          Top = 8
          Width = 57
          Height = 13
          Caption = 'Filter Colors:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 52
          Top = 25
          Width = 135
          Height = 13
          Caption = 'DirectX Media Object (DMO)'
        end
        object Label9: TLabel
          Left = 52
          Top = 43
          Width = 159
          Height = 13
          Caption = 'Users Kernel Streaming (KSProxy)'
        end
        object Label10: TLabel
          Left = 52
          Top = 61
          Width = 188
          Height = 13
          Caption = 'Uses Compression Manager (ACM/ICM)'
        end
        object Label11: TLabel
          Left = 52
          Top = 79
          Width = 153
          Height = 13
          Caption = 'Plug and Play (PNP) device filter'
        end
        object Label12: TLabel
          Left = 52
          Top = 97
          Width = 83
          Height = 13
          Caption = 'Default filter Color'
        end
        object Label13: TLabel
          Left = 8
          Top = 128
          Width = 59
          Height = 13
          Caption = 'Pin Pictures:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label14: TLabel
          Left = 48
          Top = 144
          Width = 93
          Height = 13
          Caption = 'Indicate a input pin.'
        end
        object Label15: TLabel
          Left = 48
          Top = 162
          Width = 100
          Height = 13
          Caption = 'Indicate a output pin.'
        end
        object Image1: TImage
          Left = 16
          Top = 24
          Width = 30
          Height = 16
          AutoSize = True
        end
        object Image2: TImage
          Left = 16
          Top = 42
          Width = 30
          Height = 16
          AutoSize = True
        end
        object Image3: TImage
          Left = 16
          Top = 60
          Width = 30
          Height = 16
          AutoSize = True
        end
        object Image4: TImage
          Left = 16
          Top = 78
          Width = 30
          Height = 16
          AutoSize = True
        end
        object Image5: TImage
          Left = 16
          Top = 96
          Width = 30
          Height = 16
          AutoSize = True
        end
        object Image6: TImage
          Left = 24
          Top = 144
          Width = 16
          Height = 14
          AutoSize = True
          Transparent = True
        end
        object Image7: TImage
          Left = 24
          Top = 162
          Width = 16
          Height = 14
          AutoSize = True
          Transparent = True
        end
      end
    end
  end
  object FilterGraph: TFilterGraph
    AutoCreate = True
    GraphEdit = False
    Left = 280
    Top = 256
  end
  object Filter: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph
    Left = 312
    Top = 256
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 344
    Top = 256
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
    Left = 376
    Top = 256
    object PinProperty: TMenuItem
      Caption = 'Property Page'
      OnClick = PinPropertyClick
    end
  end
end
