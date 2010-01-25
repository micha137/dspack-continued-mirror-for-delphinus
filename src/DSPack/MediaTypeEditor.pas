
    (*********************************************************************
     *  DSPack 2.3.3                                                     *
     *                                                                   *
     *  home page : http://www.progdigy.com                              *
     *  email     : hgourvest@progdigy.com                               *
     *   Thanks to Michael Andersen. (DSVideoWindowEx)                   *
     *                                                                   *
     *  date      : 21-02-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *********************************************************************)
     
unit MediaTypeEditor;

interface

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DSUtil, StdCtrls, DirectShow9, Mask;

type
  TFormMediaType = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    cbMajorTypes: TComboBox;
    cbSubTypes: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    chkFixedSize: TCheckBox;
    chkTempCompress: TCheckBox;
    edSampleSize: TEdit;
    Label3: TLabel;
    cbFormatType: TComboBox;
    Label4: TLabel;
    Memo1: TMemo;
    lblFormatSize: TLabel;
    edFormatSize: TEdit;
    procedure FormShow(Sender: TObject);
    procedure cbMajorTypesChange(Sender: TObject);
    procedure cbSubTypesChange(Sender: TObject);
    procedure chkFixedSizeClick(Sender: TObject);
    procedure chkTempCompressClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure cbFormatTypeChange(Sender: TObject);
    procedure edSampleSizeChange(Sender: TObject);
  public
    MediaType: TMediaType;
    procedure RefreshMediaType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGUIDDescription = record
    name: string;
    GUID: TGUID;
  end;


var
  FormMediaType: TFormMediaType;

const
  MajorTypes : array[0..15] of TGUIDDescription =
   ((name: '[Automatic]'; GUID: '{00000000-0000-0000-0000-000000000000}'),
    (name: 'Video'         ; GUID: '{73646976-0000-0010-8000-00AA00389B71}'),
    (name: 'Audio'         ; GUID: '{73647561-0000-0010-8000-00AA00389B71}'),
    (name: 'AnalogAudio'   ; GUID: '{0482DEE1-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo'   ; GUID: '{0482DDE1-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AUXLine21Data' ; GUID: '{670AEA80-3A82-11D0-B79B-00AA003767A7}'),
    (name: 'File'          ; GUID: '{656C6966-0000-0010-8000-00AA00389B71}'),
    (name: 'Interleaved'   ; GUID: '{73766169-0000-0010-8000-00AA00389B71}'),
    (name: 'LMRT'          ; GUID: '{74726c6d-0000-0010-8000-00aa00389b71}'),
    (name: 'Midi'          ; GUID: '{7364696D-0000-0010-8000-00AA00389B71}'),
    (name: 'MPEG2_PES'     ; GUID: '{e06d8020-db46-11cf-b4d1-00805f6cbbea}'),
    (name: 'ScriptCommand' ; GUID: '{73636D64-0000-0010-8000-00AA00389B71}'),
    (name: 'Stream'        ; GUID: '{E436EB83-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'Text'          ; GUID: '{73747874-0000-0010-8000-00AA00389B71}'),
    (name: 'Timecode'      ; GUID: '{0482DEE3-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'URL_STREAM'    ; GUID: '{736c7275-0000-0010-8000-00aa00389b71}'));

  SubTypes : array[0..97] of TGUIDDescription =
   ((name: '[Automatic]'; GUID: '{00000000-0000-0000-0000-000000000000}'),
    (name: 'MJPG'; GUID: '{47504A4D-0000-0010-8000-00AA00389B71}'),
    (name: 'Avi'; GUID: '{E436EB88-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'Asf'; GUID: '{3DB80F90-9412-11D1-ADED-0000F8754B99}'),
    (name: 'PCM'; GUID: '{00000001-0000-0010-8000-00AA00389B71}'),
    (name: 'WAVE'; GUID: '{E436EB8B-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'AU'; GUID: '{E436EB8C-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'AIFF'; GUID: '{E436EB8D-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'DTS'; GUID: '{E06D8033-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DOLBY_AC3'; GUID: '{E06D802C-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'RGB1'; GUID: '{E436EB78-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB4'; GUID: '{E436EB79-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB8'; GUID: '{E436EB7A-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB565'; GUID: '{E436EB7B-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB555'; GUID: '{E436EB7C-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB24'; GUID: '{E436EB7D-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'RGB32'; GUID: '{E436EB7E-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'ARGB32'; GUID: '{773C9AC0-3274-11D0-B724-00AA006C1A01}'),
    (name: 'YUYV'; GUID: '{56595559-0000-0010-8000-00AA00389B71}'),
    (name: 'IYUV'; GUID: '{56555949-0000-0010-8000-00AA00389B71}'),
    (name: 'YVU9'; GUID: '{39555659-0000-0010-8000-00AA00389B71}'),
    (name: 'Y411'; GUID: '{31313459-0000-0010-8000-00AA00389B71}'),
    (name: 'Y41P'; GUID: '{50313459-0000-0010-8000-00AA00389B71}'),
    (name: 'YUY2'; GUID: '{32595559-0000-0010-8000-00AA00389B71}'),
    (name: 'YVYU'; GUID: '{55595659-0000-0010-8000-00AA00389B71}'),
    (name: 'UYVY'; GUID: '{59565955-0000-0010-8000-00AA00389B71}'),
    (name: 'Y211'; GUID: '{31313259-0000-0010-8000-00AA00389B71}'),
    (name: 'YV12'; GUID: '{32315659-0000-0010-8000-00AA00389B71}'),
    (name: 'CLPL'; GUID: '{4C504C43-0000-0010-8000-00AA00389B71}'),
    (name: 'CLJR'; GUID: '{524A4C43-0000-0010-8000-00AA00389B71}'),
    (name: 'IF09'; GUID: '{39304649-0000-0010-8000-00AA00389B71}'),
    (name: 'CPLA'; GUID: '{414C5043-0000-0010-8000-00AA00389B71}'),
    (name: 'TVMJ'; GUID: '{4A4D5654-0000-0010-8000-00AA00389B71}'),
    (name: 'WAKE'; GUID: '{454B4157-0000-0010-8000-00AA00389B71}'),
    (name: 'CFCC'; GUID: '{43434643-0000-0010-8000-00AA00389B71}'),
    (name: 'IJPG'; GUID: '{47504A49-0000-0010-8000-00AA00389B71}'),
    (name: 'Plum'; GUID: '{6D756C50-0000-0010-8000-00AA00389B71}'),
    (name: 'DVCS'; GUID: '{53435644-0000-0010-8000-00AA00389B71}'),
    (name: 'DVSD'; GUID: '{44535644-0000-0010-8000-00AA00389B71}'),
    (name: 'MDVF'; GUID: '{4656444D-0000-0010-8000-00AA00389B71}'),
    (name: 'Overlay'; GUID: '{E436EB7F-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1Packet'; GUID: '{E436EB80-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1Payload'; GUID: '{E436EB81-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1AudioPayload'; GUID: '{00000050-0000-0010-8000-00AA00389B71}'),
    (name: 'MPEG1System'; GUID: '{E436EB84-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1VideoCD'; GUID: '{E436EB85-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1Video'; GUID: '{E436EB86-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'MPEG1Audio'; GUID: '{E436EB87-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'QTMovie'; GUID: '{E436EB89-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'QTRpza'; GUID: '{617A7072-0000-0010-8000-00AA00389B71}'),
    (name: 'QTSmc'; GUID: '{20636D73-0000-0010-8000-00AA00389B71}'),
    (name: 'QTRle'; GUID: '{20656C72-0000-0010-8000-00AA00389B71}'),
    (name: 'QTJpeg'; GUID: '{6765706A-0000-0010-8000-00AA00389B71}'),
    (name: 'PCMAudio_Obsolete'; GUID: '{E436EB8A-524F-11CE-9F53-0020AF0BA770}'),
    (name: 'dvsd_'; GUID: '{64737664-0000-0010-8000-00AA00389B71}'),
    (name: 'dvhd'; GUID: '{64687664-0000-0010-8000-00AA00389B71}'),
    (name: 'dvsl'; GUID: '{6C737664-0000-0010-8000-00AA00389B71}'),
    (name: 'Line21_BytePair'; GUID: '{6E8D4A22-310C-11D0-B79A-00AA003767A7}'),
    (name: 'Line21_GOPPacket'; GUID: '{6E8D4A23-310C-11D0-B79A-00AA003767A7}'),
    (name: 'Line21_VBIRawData'; GUID: '{6E8D4A24-310C-11D0-B79A-00AA003767A7}'),
    (name: 'DRM_Audio'; GUID: '{00000009-0000-0010-8000-00AA00389B71}'),
    (name: 'IEEE_FLOAT'; GUID: '{00000003-0000-0010-8000-00AA00389B71}'),
    (name: 'DOLBY_AC3_SPDIF'; GUID: '{00000092-0000-0010-8000-00AA00389B71}'),
    (name: 'RAW_SPORT'; GUID: '{00000240-0000-0010-8000-00AA00389B71}'),
    (name: 'SPDIF_TAG_241h'; GUID: '{00000241-0000-0010-8000-00AA00389B71}'),
    (name: 'DssVideo'; GUID: '{A0AF4F81-E163-11D0-BAD9-00609744111A}'),
    (name: 'DssAudio'; GUID: '{A0AF4F82-E163-11D0-BAD9-00609744111A}'),
    (name: 'VPVideo'; GUID: '{5A9B6A40-1A22-11D1-BAD9-00609744111A}'),
    (name: 'VPVBI'; GUID: '{5A9B6A41-1A22-11D1-BAD9-00609744111A}'),
    (name: 'AnalogVideo_NTSC_M'; GUID: '{0482DDE2-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_B'; GUID: '{0482DDE5-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_D'; GUID: '{0482DDE6-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_G'; GUID: '{0482DDE7-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_H'; GUID: '{0482DDE8-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_I'; GUID: '{0482DDE9-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_M'; GUID: '{0482DDEA-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_N'; GUID: '{0482DDEB-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_PAL_N_COMBO'; GUID: '{0482DDEC-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_B'; GUID: '{0482DDF0-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_D'; GUID: '{0482DDF1-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_G'; GUID: '{0482DDF2-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_H'; GUID: '{0482DDF3-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_K'; GUID: '{0482DDF4-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_K1'; GUID: '{0482DDF5-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'AnalogVideo_SECAM_L'; GUID: '{0482DDF6-7817-11CF-8A03-00AA006ECB65}'),
    (name: 'MPEG2_VIDEO'; GUID: '{E06D8026-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'MPEG2_PROGRAM'; GUID: '{E06D8022-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'MPEG2_TRANSPORT'; GUID: '{E06D8023-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'MPEG2_AUDIO'; GUID: '{E06D802B-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DVD_SUBPICTURE'; GUID: '{E06D802D-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DVD_LPCM_AUDIO'; GUID: '{E06D8032-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'SDDS'; GUID: '{E06D8034-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DVD_NAVIGATION_PCI'; GUID: '{E06D802F-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DVD_NAVIGATION_DSI'; GUID: '{E06D8030-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'DVD_NAVIGATION_PROVIDER'; GUID: '{E06D8031-DB46-11CF-B4D1-00805F6CBBEA}'),
    (name: 'MP42'; GUID: '{3234504D-0000-0010-8000-00AA00389B71}'),
    (name: 'DIVX'; GUID: '{58564944-0000-0010-8000-00AA00389B71}'),
    (name: 'VOXWARE'; GUID: '{00000075-0000-0010-8000-00AA00389B71}'));

  FormatTypes: array[0..4] of TGUIDDescription =
   ((name: '[Automatic]';  GUID: '{00000000-0000-0000-0000-000000000000}'),
    (name: 'None';         GUID: '{0F6417D6-C318-11D0-A43F-00A0C9223196}'),
    (name: 'VideoInfo';    GUID: '{05589F80-C356-11CE-BF01-00AA0055595A}'),
    (name: 'VideoInfo2';   GUID: '{F72A76A0-EB0A-11D0-ACE4-0000C0CC16BA}'),
    (name: 'WaveFormatEx'; GUID: '{05589F81-C356-11CE-BF01-00AA0055595A}'));

implementation
 uses ActiveX;
{$R *.dfm}

  constructor TFormMediaType.Create(AOwner: TComponent);
  var i: byte;
  begin
    inherited Create(AOwner);
    MediaType:= TMediaType.Create;
    for i := 0 to 15 do cbMajorTypes.Items.Add(MajorTypes[i].name);
    for i := 0 to 97 do cbSubTypes.Items.Add(SubTypes[i].name);
    for i := 0 to 4  do cbFormatType.Items.Add(FormatTypes[i].name);
  end;

  destructor TFormMediaType.Destroy;
  begin
    MediaType.Free;

    inherited Destroy;
  end;

//    majortype            : TGUID;
//    subtype              : TGUID;
//    bFixedSizeSamples    : BOOL;
//    bTemporalCompression : BOOL;
//    lSampleSize          : ULONG;
//    formattype           : TGUID;
//    pUnk                 : IUnknown;
//    cbFormat             : ULONG;
//    pbFormat             : Pointer;

  procedure TFormMediaType.FormShow(Sender: TObject);
  begin
    RefreshMediaType;
  end;

  procedure TFormMediaType.RefreshMediaType;
  var i: byte;
  begin
    for i := 0 to 15 do
      if IsEqualGUID(MajorTypes[i].GUID, MediaType.MajorType) then
      begin
        cbMajorTypes.ItemIndex := i;
        Break;
      end;
    for i := 0 to 97 do
      if IsEqualGUID(SubTypes[i].GUID, MediaType.SubType) then
      begin
        cbSubTypes.ItemIndex := i;
        Break;
      end;
     if cbMajorTypes.ItemIndex = -1 then cbMajorTypes.ItemIndex := 0;
     if cbSubTypes.ItemIndex   = -1 then cbSubTypes.ItemIndex := 0;
     chkFixedSize.Checked    := MediaType.IsFixedSize;
     chkTempCompress.Checked := MediaType.IsTemporalCompressed;
     edSampleSize.Text := inttostr(MediaType.GetSampleSize);

    for i := 0 to 4 do
      if IsEqualGUID(FormatTypes[i].GUID, MediaType.FormatType) then
      begin
        cbFormatType.ItemIndex := i;
        Break;
      end;
    edFormatSize.Text := inttostr(MediaType.FormatLength);


    memo1.Clear;  
    memo1.lines.Add(GetMediaTypeDescription(MediaType.AMMediaType));

  end;

  procedure TFormMediaType.btOKClick(Sender: TObject);
  begin
    try
      MediaType.SetSampleSize(StrToInt(edSampleSize.Text));
    except
      MessageBox(Handle,PChar('Invalid Sample Size.'), PChar('Error'), mb_ok);
      exit;
    end;
    ModalResult := mrOK;
  end;

  procedure TFormMediaType.cbMajorTypesChange(Sender: TObject);
  begin
    MediaType.MajorType := MajorTypes[cbMajorTypes.ItemIndex].GUID;
    RefreshMediaType;
  end;

  procedure TFormMediaType.cbSubTypesChange(Sender: TObject);
  begin
    MediaType.SubType := SubTypes[cbSubTypes.ItemIndex].GUID;
    RefreshMediaType;
  end;

  procedure TFormMediaType.chkFixedSizeClick(Sender: TObject);
  begin
    MediaType.AMMediaType.bFixedSizeSamples := chkFixedSize.Checked;
    RefreshMediaType;
  end;

  procedure TFormMediaType.chkTempCompressClick(Sender: TObject);
  begin
    MediaType.AMMediaType.bTemporalCompression := chkTempCompress.Checked;
    RefreshMediaType;
  end;


  procedure TFormMediaType.cbFormatTypeChange(Sender: TObject);
  begin
    MediaType.FormatType := FormatTypes[cbFormatType.ItemIndex].GUID;
    RefreshMediaType;
  end;

  procedure TFormMediaType.edSampleSizeChange(Sender: TObject);
  begin
    try
      if edSampleSize.Text = '' then MediaType.SetSampleSize(0) else
        MediaType.SetSampleSize(StrToInt(edSampleSize.Text));
    finally
      RefreshMediaType;
    end;
  end;

end.
