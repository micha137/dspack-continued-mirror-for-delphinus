object ColorControlForm: TColorControlForm
  Left = 557
  Top = 136
  BorderStyle = bsNone
  ClientHeight = 226
  ClientWidth = 166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clLime
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 166
    Height = 226
    Align = alClient
    Brush.Color = clBtnFace
    Pen.Color = clLime
    Pen.Width = 3
    Shape = stRoundRect
  end
  object Label1: TLabel
    Left = 56
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Brightness'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 64
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Contrast'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 72
    Top = 88
    Width = 20
    Height = 13
    Caption = 'Hue'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 56
    Top = 128
    Width = 48
    Height = 13
    Caption = 'Saturation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object SpeedButton1: TSpeedButton
    Left = 24
    Top = 192
    Width = 121
    Height = 25
    Caption = 'Reset to Default'
    Flat = True
    OnClick = SpeedButton1Click
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 24
    Width = 150
    Height = 25
    Max = 10000
    Frequency = 1000
    Position = 750
    TabOrder = 0
    ThumbLength = 10
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 8
    Top = 64
    Width = 150
    Height = 25
    Max = 20000
    Frequency = 2000
    Position = 10000
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar2Change
  end
  object TrackBar3: TTrackBar
    Left = 8
    Top = 104
    Width = 150
    Height = 25
    Max = 180
    Min = -180
    Frequency = 36
    TabOrder = 2
    ThumbLength = 10
    OnChange = TrackBar3Change
  end
  object TrackBar4: TTrackBar
    Left = 8
    Top = 144
    Width = 150
    Height = 25
    Max = 20000
    Frequency = 2000
    Position = 10000
    TabOrder = 3
    ThumbLength = 10
    OnChange = TrackBar4Change
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 172
    Width = 97
    Height = 17
    Caption = 'Utilized colors'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox1Click
  end
end
