object frmSettings: TfrmSettings
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Settings'
  ClientHeight = 363
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 101
    Height = 13
    Caption = 'Prefered Audio Driver'
  end
  object cmbDevices: TComboBox
    Left = 120
    Top = 9
    Width = 417
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbDevicesChange
  end
end
