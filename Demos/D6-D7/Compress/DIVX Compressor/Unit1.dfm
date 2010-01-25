object Form1: TForm1
  Left = 198
  Top = 114
  Width = 331
  Height = 197
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Output File'
  end
  object GO: TButton
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Caption = 'GO'
    TabOrder = 0
    OnClick = GOClick
  end
  object Output: TEdit
    Left = 8
    Top = 24
    Width = 289
    Height = 21
    TabOrder = 1
    Text = 'c:\output.avi'
  end
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 323
    Height = 89
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object FilterGraph: TFilterGraph
    AutoCreate = True
    Mode = gmCapture
    GraphEdit = True
    OnDSEvent = FilterGraphDSEvent
    Left = 240
    Top = 8
  end
  object DivXEncoder: TFilter
    BaseFilter.data = {
      8200000037D415438C5BD011BD3B00A0C911CE866E0000004000640065007600
      6900630065003A0063006D003A007B0033003300440039004100370036003000
      2D0039003000430038002D0031003100440030002D0042004400340033002D00
      3000300041003000430039003100310043004500380036007D005C0064006900
      760078000000}
    FilterGraph = FilterGraph
    Left = 240
    Top = 40
  end
  object OpenDialog: TOpenDialog
    Left = 272
    Top = 8
  end
  object FileSource: TFilter
    BaseFilter.data = {
      C600000037D415438C5BD011BD3B00A0C911CE86B20000004000640065007600
      6900630065003A00730077003A007B0030003800330038003600330046003100
      2D0037003000440045002D0031003100440030002D0042004400340030002D00
      3000300041003000430039003100310043004500380036007D005C007B004500
      34003300360045004200420035002D0035003200340046002D00310031004300
      45002D0039004600350033002D00300030003200300041004600300042004100
      3700370030007D000000}
    FilterGraph = FilterGraph
    Left = 272
    Top = 40
  end
  object MP3Enc: TFilter
    BaseFilter.data = {
      9600000037D415438C5BD011BD3B00A0C911CE86820000004000640065007600
      6900630065003A0063006D003A007B0033003300440039004100370036003100
      2D0039003000430038002D0031003100440030002D0042004400340033002D00
      3000300041003000430039003100310043004500380036007D005C0038003500
      4D0050004500470020004C0061007900650072002D0033000000}
    FilterGraph = FilterGraph
    Left = 208
    Top = 40
  end
end
