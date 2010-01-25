object Form1: TForm1
  Left = 266
  Top = 229
  BorderStyle = bsToolWindow
  Caption = '*** Shoutcast ***'
  ClientHeight = 375
  ClientWidth = 563
  Color = clBtnFace
  UseDockManager = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 2
    Top = 2
    Width = 559
    Height = 367
    ActivePage = TabSheet2
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'URL'
      ImageIndex = 1
      object Button6: TButton
        Left = 456
        Top = 88
        Width = 73
        Height = 33
        Caption = 'connect'
        TabOrder = 0
        OnClick = Button6Click
      end
      object GroupBox4: TGroupBox
        Left = 272
        Top = 80
        Width = 177
        Height = 49
        Caption = 'MetaData'
        TabOrder = 1
        object RadioButton3: TRadioButton
          Left = 8
          Top = 16
          Width = 57
          Height = 25
          Caption = 'parse'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
        end
        object RadioButton4: TRadioButton
          Left = 72
          Top = 16
          Width = 81
          Height = 17
          Caption = 'don'#39't parse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
      object GroupBox5: TGroupBox
        Left = 272
        Top = 8
        Width = 257
        Height = 65
        Caption = 'Buffering'
        TabOrder = 2
        object Label7: TLabel
          Left = 8
          Top = 24
          Width = 46
          Height = 13
          Caption = 'Prebuffer:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 8
          Top = 44
          Width = 31
          Height = 13
          Caption = 'Buffer:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 213
          Top = 24
          Width = 20
          Height = 13
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 213
          Top = 44
          Width = 20
          Height = 13
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object TrackBar1: TTrackBar
          Left = 56
          Top = 21
          Width = 153
          Height = 20
          LineSize = 100
          Max = 1000
          PageSize = 100
          Frequency = 10
          Position = 300
          TabOrder = 0
          ThumbLength = 12
          TickMarks = tmBoth
          TickStyle = tsNone
          OnChange = TrackBar1Change
        end
        object TrackBar2: TTrackBar
          Left = 56
          Top = 40
          Width = 153
          Height = 17
          LineSize = 100
          Max = 1000
          Frequency = 10
          Position = 150
          TabOrder = 1
          ThumbLength = 12
          TickMarks = tmBoth
          TickStyle = tsNone
          OnChange = TrackBar2Change
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 184
        Width = 521
        Height = 65
        Caption = 'Server info'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        object Label20: TLabel
          Left = 8
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Name:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label21: TLabel
          Left = 8
          Top = 32
          Width = 32
          Height = 13
          Caption = 'Genre:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label22: TLabel
          Left = 8
          Top = 48
          Width = 22
          Height = 13
          Caption = 'URL'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label23: TLabel
          Left = 456
          Top = 48
          Width = 33
          Height = 13
          Caption = 'Bitrate:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label24: TLabel
          Left = 56
          Top = 16
          Width = 24
          Height = 13
          Cursor = crHandPoint
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label25: TLabel
          Left = 56
          Top = 32
          Width = 20
          Height = 13
          Cursor = crHandPoint
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label26: TLabel
          Left = 56
          Top = 48
          Width = 20
          Height = 13
          Cursor = crHandPoint
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label27: TLabel
          Left = 496
          Top = 48
          Width = 20
          Height = 13
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 259
        Width = 521
        Height = 53
        Caption = 'Stream info'
        TabOrder = 4
        object Label16: TLabel
          Left = 10
          Top = 18
          Width = 23
          Height = 13
          Caption = 'Title:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 10
          Top = 35
          Width = 25
          Height = 13
          Caption = 'URL:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label18: TLabel
          Left = 42
          Top = 18
          Width = 24
          Height = 13
          Cursor = crHandPoint
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label19: TLabel
          Left = 42
          Top = 35
          Width = 20
          Height = 13
          Cursor = crHandPoint
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object GroupBox9: TGroupBox
        Left = 272
        Top = 136
        Width = 257
        Height = 25
        TabOrder = 5
        object Label5: TLabel
          Left = 6
          Top = 8
          Width = 65
          Height = 13
          Caption = 'Current State:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 81
          Top = 8
          Width = 24
          Height = 13
          Caption = 'N/A'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object ListBox1: TListBox
        Left = 8
        Top = 8
        Width = 257
        Height = 161
        ItemHeight = 13
        Items.Strings = (
          'http://64.236.34.67:80/stream/1039'
          'http://64.236.34.4:80/stream/1040'
          'http://64.236.34.4:80/stream/1003'
          'http://62.4.21.34:8000'
          'http://64.236.34.97:80/stream/1006'
          'http://64.62.197.5:8000'
          'http://213.61.7.231:8500'
          'http://62.179.101.66:7128'
          'http://67.18.157.58:9000'
          'http://64.236.34.97:80/stream/1025')
        TabOrder = 6
        OnDblClick = ListBox1DblClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Stream Ripper'
      ImageIndex = 2
      object GroupBox6: TGroupBox
        Left = 8
        Top = 24
        Width = 521
        Height = 65
        Caption = 'used for recording if Metadata is disabled'
        TabOrder = 0
        object Label4: TLabel
          Left = 16
          Top = 32
          Width = 45
          Height = 13
          Caption = 'Filename:'
        end
        object Edit4: TEdit
          Left = 72
          Top = 28
          Width = 241
          Height = 21
          TabOrder = 0
          Text = 'recordedstream.mp3'
        end
      end
      object GroupBox7: TGroupBox
        Left = 8
        Top = 96
        Width = 521
        Height = 65
        Caption = 'recording location'
        TabOrder = 1
        object Label10: TLabel
          Left = 8
          Top = 20
          Width = 25
          Height = 13
          Caption = 'Path:'
        end
        object Label11: TLabel
          Left = 40
          Top = 20
          Width = 38
          Height = 13
          Cursor = crHandPoint
          Caption = 'Label11'
        end
      end
      object GroupBox8: TGroupBox
        Left = 8
        Top = 168
        Width = 521
        Height = 49
        Caption = 'commands'
        TabOrder = 2
        object CheckBox1: TCheckBox
          Left = 208
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Rip Stream'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = CheckBox1Click
        end
      end
    end
  end
  object TmrCloseApp: TTimer
    Enabled = False
    Interval = 1
    OnTimer = TmrCloseAppTimer
    Left = 494
    Top = 338
  end
  object TmrNilAll: TTimer
    Enabled = False
    OnTimer = TmrNilAllTimer
    Left = 526
    Top = 338
  end
  object TmrOpenUrl: TTimer
    Enabled = False
    Interval = 20
    OnTimer = TmrOpenUrlTimer
    Left = 462
    Top = 338
  end
end
