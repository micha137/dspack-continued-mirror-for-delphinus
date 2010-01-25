object FormSelectURL: TFormSelectURL
  Left = 200
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Open URL'
  ClientHeight = 60
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btOK: TButton
    Left = 0
    Top = 34
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 224
    Top = 34
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object URL: TEdit
    Left = 0
    Top = 2
    Width = 297
    Height = 21
    TabOrder = 2
    Text = 'mms://'
  end
end
