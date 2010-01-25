
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

{$IFDEF VER140} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
{$IFDEF VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit BaseFilterEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSUTIL, DirectShow9, ComCtrls, DSPack, Menus, ExtCtrls,
  Buttons;

type
  TFormBaseFilter = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    FilterGraph: TFilterGraph;
    Filter: TFilter;
    PopupMenu: TPopupMenu;
    PropertyPage: TMenuItem;
    VFWDisplay: TMenuItem;
    VFWFormat: TMenuItem;
    VFWSource: TMenuItem;
    VFWConfig: TMenuItem;
    VFWCapture: TMenuItem;
    Config1: TMenuItem;
    VFWAbout: TMenuItem;
    PinMenu: TPopupMenu;
    PinProperty: TMenuItem;
    PageControl1: TPageControl;
    SelectorSheet: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Filters: TTreeView;
    Interfaces: TListBox;
    Pins: TListBox;
    PinsSheet: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Pins1: TListBox;
    PinInterfaces: TListBox;
    MediaTypes: TListBox;
    MonikerTag: TMemo;
    InfoBtn: TSpeedButton;
    InfoSheet: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Label16: TLabel;
    Label17: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PropertyPageClick(Sender: TObject);
    procedure VFWDisplayClick(Sender: TObject);
    procedure VFWFormatClick(Sender: TObject);
    procedure VFWSourceClick(Sender: TObject);
    procedure Config1Click(Sender: TObject);
    procedure VFWAboutClick(Sender: TObject);
    procedure PinMenuPopup(Sender: TObject);
    procedure PinPropertyClick(Sender: TObject);
    procedure PinsSheetShow(Sender: TObject);
    procedure Pins1Click(Sender: TObject);
    procedure InterfacesDblClick(Sender: TObject);
    procedure PinInterfacesDblClick(Sender: TObject);
    procedure PinsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FiltersChange(Sender: TObject; Node: TTreeNode);
    procedure PinsMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure FiltersDblClick(Sender: TObject);
    procedure FiltersCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure InfoBtnClick(Sender: TObject);
  public
    SysDevEnum: TSysDevEnum;
    PinList: TPinList;
    InPinPic,
    OutPinPic : TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure RefreshTree;
//    procedure FiltersClick(Sender: TObject);
    procedure GetFilterInfo;
  end;

  TDSitf = record
    name : string;
    itf  : TGUID;
  end;

  pNodeItem = ^TNodeItem;
  TNodeItem = Record
    Index      : integer;
    Color      : TColor;
    MonikerStr : WideString;
  End;

const
  FilterColors : array[0..4] of TColor = (clBlack, clPurple, clBlue, clRed, clGreen);

  DSItfs : array[0..382] of TDSitf =
((name: 'IPin'; itf: '{56A86891-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IEnumPins'; itf: '{56A86892-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IEnumMediaTypes'; itf: '{89C31040-846B-11CE-97D3-00AA0055595A}'),
 (name: 'IFilterGraph'; itf: '{56A8689F-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IEnumFilters'; itf: '{56A86893-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaFilter'; itf: '{56A86899-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IBaseFilter'; itf: '{56A86895-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IReferenceClock'; itf: '{56A86897-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IReferenceClock2'; itf: '{36B73885-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'IMediaSample'; itf: '{56A8689A-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaSample2'; itf: '{36B73884-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'IMemAllocator'; itf: '{56A8689C-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMemInputPin'; itf: '{56A8689D-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IAMovieSetup'; itf: '{A3D8CEC0-7E5A-11CF-BBC5-00805F6CEF20}'),
 (name: 'IMediaSeeking'; itf: '{36B73880-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'IEnumRegFilters'; itf: '{56A868A4-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IFilterMapper'; itf: '{56A868A3-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IFilterMapper2'; itf: '{B79BB0B0-33C1-11D1-ABE1-00A0C905F375}'),
 (name: 'IQualityControl'; itf: '{56A868A5-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IOverlayNotify'; itf: '{56A868A0-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IOverlay'; itf: '{56A868A1-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaEventSink'; itf: '{56A868A2-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IFileSourceFilter'; itf: '{56A868A6-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IFileSinkFilter'; itf: '{A2104830-7C70-11CF-8BCE-00AA00A3F1A6}'),
 (name: 'IFileSinkFilter2'; itf: '{00855B90-CE1B-11D0-BD4F-00A0C911CE86}'),
 (name: 'IFileAsyncIO'; itf: '{56A868A7-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IGraphBuilder'; itf: '{56A868A9-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'ICaptureGraphBuilder'; itf: '{BF87B6E0-8C27-11D0-B3F0-00AA003761C5}'),
 (name: 'IAMCopyCaptureFileProgress'; itf: '{670D1D20-A068-11D0-B3F0-00AA003761C5}'),
 (name: 'IFilterGraph2'; itf: '{36B73882-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'IStreamBuilder'; itf: '{56A868BF-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IAsyncReader'; itf: '{56A868AA-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IGraphVersion'; itf: '{56A868AB-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IResourceConsumer'; itf: '{56A868AD-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IResourceManager'; itf: '{56A868AC-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IDistributorNotify'; itf: '{56A868AF-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IAMStreamControl'; itf: '{36B73881-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'ISeekingPassThru'; itf: '{36B73883-C2C8-11CF-8B46-00805F6CEF60}'),
 (name: 'IAMStreamConfig'; itf: '{C6E13340-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IConfigInterleaving'; itf: '{BEE3D220-157B-11D0-BD23-00A0C911CE86}'),
 (name: 'IConfigAviMux'; itf: '{5ACD6AA0-F482-11CE-8B67-00AA00A3F1A6}'),
 (name: 'IAMVideoCompression'; itf: '{C6E13343-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMVfwCaptureDialogs'; itf: '{D8D715A0-6E5E-11D0-B3F0-00AA003761C5}'),
 (name: 'IAMVfwCompressDialogs'; itf: '{D8D715A3-6E5E-11D0-B3F0-00AA003761C5}'),
 (name: 'IAMDroppedFrames'; itf: '{C6E13344-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMAudioInputMixer'; itf: '{54C39221-8380-11D0-B3F0-00AA003761C5}'),
 (name: 'IAMAnalogVideoDecoder'; itf: '{C6E13350-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMVideoProcAmp'; itf: '{C6E13360-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMCameraControl'; itf: '{C6E13370-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMCrossbar'; itf: '{C6E13380-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IAMTuner'; itf: '{211A8761-03AC-11D1-8D13-00AA00BD8339}'),
 (name: 'IAMTunerNotification'; itf: '{211A8760-03AC-11D1-8D13-00AA00BD8339}'),
 (name: 'IAMTVTuner'; itf: '{211A8766-03AC-11D1-8D13-00AA00BD8339}'),
 (name: 'IBPCSatelliteTuner'; itf: '{211A8765-03AC-11D1-8D13-00AA00BD8339}'),
 (name: 'IAMTVAudio'; itf: '{83EC1C30-23D1-11D1-99E6-00A0C9560266}'),
 (name: 'IAMTVAudioNotification'; itf: '{83EC1C33-23D1-11D1-99E6-00A0C9560266}'),
 (name: 'IAMAnalogVideoEncoder'; itf: '{C6E133B0-30AC-11D0-A18C-00A0C9118956}'),
 (name: 'IMediaPropertyBag'; itf: '{6025A880-C0D5-11D0-BD4E-00A0C911CE86}'),
 (name: 'IPersistMediaPropertyBag'; itf: '{5738E040-B67F-11D0-BD4D-00A0C911CE86}'),
 (name: 'IAMPhysicalPinInfo'; itf: '{F938C991-3029-11CF-8C44-00AA006B6814}'),
 (name: 'IAMExtDevice'; itf: '{B5730A90-1A2C-11CF-8C23-00AA006B6814}'),
 (name: 'IAMExtTransport'; itf: '{A03CD5F0-3045-11CF-8C44-00AA006B6814}'),
 (name: 'IAMTimecodeReader'; itf: '{9B496CE1-811B-11CF-8C77-00AA006B6814}'),
 (name: 'IAMTimecodeGenerator'; itf: '{9B496CE0-811B-11CF-8C77-00AA006B6814}'),
 (name: 'IAMTimecodeDisplay'; itf: '{9B496CE2-811B-11CF-8C77-00AA006B6814}'),
 (name: 'IAMDevMemoryAllocator'; itf: '{C6545BF0-E76B-11D0-BD52-00A0C911CE86}'),
 (name: 'IAMDevMemoryControl'; itf: '{C6545BF1-E76B-11D0-BD52-00A0C911CE86}'),
 (name: 'IAMStreamSelect'; itf: '{C1960960-17F5-11D1-ABE1-00A0C905F375}'),
 (name: 'IAMovie'; itf: '{359ACE10-7688-11CF-8B23-00805F6CEF60}'),
 (name: 'ICreateDevEnum'; itf: '{29840822-5B84-11D0-BD3B-00A0C911CE86}'),
 (name: 'IDvdControl'; itf: '{A70EFE61-E2A3-11D0-A9BE-00AA0061BE93}'),
 (name: 'IDvdControl2'; itf: '{33BC7430-EEC0-11D2-8201-00A0C9D74842}'),
 (name: 'IDvdInfo'; itf: '{A70EFE60-E2A3-11D0-A9BE-00AA0061BE93}'),
 (name: 'IDvdInfo2'; itf: '{34151510-EEC0-11D2-8201-00A0C9D74842}'),
 (name: 'IDvdGraphBuilder'; itf: '{FCC152B6-F372-11D0-8E00-00C04FD7C08B}'),
 (name: 'IDvdState'; itf: '{86303D6D-1C4A-4087-AB42-F711167048EF}'),
 (name: 'IDvdCmd'; itf: '{5A4A97E4-94EE-4A55-9751-74B5643AA27D}'),
 (name: 'IVideoFrameStep'; itf: '{E46A9787-2B71-444D-A4B5-1FAB7B708D6A}'),
 (name: 'IFilterMapper3'; itf: '{B79BB0B1-33C1-11D1-ABE1-00A0C905F375}'),
 (name: 'IOverlayNotify2'; itf: '{680EFA10-D535-11D1-87C8-00A0C9223196}'),
 (name: 'ICaptureGraphBuilder2'; itf: '{93E5A4E0-2D50-11D2-ABFA-00A0C9C6E38D}'),
 (name: 'IMemAllocatorCallbackTemp'; itf: '{379A0CF0-C1DE-11D2-ABF5-00A0C905F375}'),
 (name: 'IMemAllocatorNotifyCallbackTemp'; itf: '{92980B30-C1DE-11D2-ABF5-00A0C905F375}'),
 (name: 'IAMVideoControl'; itf: '{6A2E0670-28E4-11D0-A18C-00A0C9118956}'),
 (name: 'IKsPropertySet'; itf: '{31EFAC30-515C-11D0-A9AA-00AA0061BE93}'),
 (name: 'IAMResourceControl'; itf: '{8389D2D0-77D7-11D1-ABE6-00A0C905F375}'),
 (name: 'IAMClockAdjust'; itf: '{4D5466B0-A49C-11D1-ABE8-00A0C905F375}'),
 (name: 'IAMFilterMiscFlags'; itf: '{2DD74950-A890-11D1-ABE8-00A0C905F375}'),
 (name: 'IDrawVideoImage'; itf: '{48EFB120-AB49-11D2-AED2-00A0C995E8D5}'),
 (name: 'IDecimateVideoImage'; itf: '{2E5EA3E0-E924-11D2-B6DA-00A0C995E8DF}'),
 (name: 'IAMVideoDecimationProperties'; itf: '{60D32930-13DA-11D3-9EC6-C4FCAEF5C7BE}'),
 (name: 'IAMLatency'; itf: '{62EA93BA-EC62-11D2-B770-00C04FB6BD3D}'),
 (name: 'IAMPushSource'; itf: '{F185FE76-E64E-11D2-B76E-00C04FB6BD3D}'),
 (name: 'IAMDeviceRemoval'; itf: '{F90A6130-B658-11D2-AE49-0000F8754B99}'),
 (name: 'IDVEnc'; itf: '{D18E17A0-AACB-11D0-AFB0-00AA00B67A42}'),
 (name: 'IIPDVDec'; itf: '{B8E8BD60-0BFE-11D0-AF91-00AA00B67A42}'),
 (name: 'IDVRGB219'; itf: '{58473A19-2BC8-4663-8012-25F81BABDDD1}'),
 (name: 'IDVSplitter'; itf: '{92A3A302-DA7C-4A1F-BA7E-1802BB5D2D02}'),
 (name: 'IAMAudioRendererStats'; itf: '{22320CB2-D41A-11D2-BF7C-D7CB9DF0BF93}'),
 (name: 'IAMGraphStreams'; itf: '{632105FA-072E-11D3-8AF9-00C04FB6BD3D}'),
 (name: 'IAMOverlayFX'; itf: '{62FAE250-7E65-4460-BFC9-6398B322073C}'),
 (name: 'IAMOpenProgress'; itf: '{8E1C39A1-DE53-11CF-AA63-0080C744528D}'),
 (name: 'IMpeg2Demultiplexer'; itf: '{436EEE9C-264F-4242-90E1-4E330C107512}'),
 (name: 'IEnumStreamIdMap'; itf: '{945C1566-6202-46FC-96C7-D87F289C6534}'),
 (name: 'IMPEG2StreamIdMap'; itf: '{D0E04C47-25B8-4369-925A-362A01D95444}'),
 (name: 'IRegisterServiceProvider'; itf: '{7B3A2F01-0751-48DD-B556-004785171C54}'),
 (name: 'IAMDecoderCaps'; itf: '{C0DFF467-D499-4986-972B-E1D9090FA941}'),
 (name: 'IAMClockSlave'; itf: '{9FD52741-176D-4B36-8F51-CA8F933223BE}'),
 (name: 'IDDrawExclModeVideo'; itf: '{153ACC21-D83B-11D1-82BF-00A0C9696C8F}'),
 (name: 'IDDrawExclModeVideoCallback'; itf: '{913C24A0-20AB-11D2-9038-00A0C9697298}'),
 (name: 'IPinConnection'; itf: '{4A9A62D3-27D4-403D-91E9-89F540E55534}'),
 (name: 'IPinFlowControl'; itf: '{C56E9858-DBF3-4F6B-8119-384AF2060DEB}'),
 (name: 'IGraphConfig'; itf: '{03A1EB8E-32BF-4245-8502-114D08A9CB88}'),
 (name: 'IGraphConfigCallback'; itf: '{ADE0FD60-D19D-11D2-ABF6-00A0C905F375}'),
 (name: 'IFilterChain'; itf: '{DCFBDCF6-0DC2-45F5-9AB2-7C330EA09C29}'),
 (name: 'IVMRImagePresenter'; itf: '{CE704FE7-E71E-41FB-BAA2-C4403E1182F5}'),
 (name: 'IVMRSurfaceAllocator'; itf: '{31CE832E-4484-458B-8CCA-F4D7E3DB0B52}'),
 (name: 'IVMRSurfaceAllocatorNotify'; itf: '{AADA05A8-5A4E-4729-AF0B-CEA27AED51E2}'),
 (name: 'IVMRWindowlessControl'; itf: '{0EB1088C-4DCD-46F0-878F-39DAE86A51B7}'),
 (name: 'IVMRMixerControl'; itf: '{1C1A17B0-BED0-415D-974B-DC6696131599}'),
 (name: 'IVMRMonitorConfig'; itf: '{9CF0B1B6-FBAA-4B7F-88CF-CF1F130A0DCE}'),
 (name: 'IVMRFilterConfig'; itf: '{9E5530C5-7034-48B4-BB46-0B8A6EFC8E36}'),
 (name: 'IVMRMixerBitmap'; itf: '{1E673275-0257-40AA-AF20-7C608D4A0428}'),
 (name: 'IVMRImageCompositor'; itf: '{7A4FB5AF-479F-4074-BB40-CE6722E43C82}'),
 (name: 'IVMRVideoStreamControl'; itf: '{058D1F11-2A54-4BEF-BD54-DF706626B727}'),
 (name: 'IVMRSurface'; itf: '{A9849BBE-9EC8-4263-B764-62730F0D15D0}'),
 (name: 'IVPManager'; itf: '{AAC18C18-E186-46D2-825D-A1F8DC8E395A}'),
 (name: 'IVMRImagePresenterConfig'; itf: '{9F3A1C85-8555-49BA-935F-BE5B5B29D178}'),
 (name: 'IVMRImagePresenterExclModeConfig'; itf: '{E6F7CE40-4673-44F1-8F77-5499D68CB4EA}'),
 (name: 'IAMBufferNegotiation'; itf: '{56ED71A0-AF5F-11D0-B3F0-00AA003761C5}'),
 (name: 'IMultiMediaStream'; itf: '{B502D1BC-9A57-11D0-8FDE-00C04FD9189D}'),
 (name: 'IMediaStream'; itf: '{B502D1BD-9A57-11D0-8FDE-00C04FD9189D}'),
 (name: 'IStreamSample'; itf: '{B502D1BE-9A57-11D0-8FDE-00C04FD9189D}'),
 (name: 'IDirectShowStream'; itf: '{7DB01C96-C0C3-11D0-8FF1-00C04FD9189D}'),
 (name: 'IAMMultiMediaStream'; itf: '{BEBE595C-9A6F-11D0-8FDE-00C04FD9189D}'),
 (name: 'IAMMediaStream'; itf: '{BEBE595D-9A6F-11D0-8FDE-00C04FD9189D}'),
 (name: 'IMediaStreamFilter'; itf: '{BEBE595E-9A6F-11D0-8FDE-00C04FD9189D}'),
 (name: 'IDirectDrawMediaSampleAllocator'; itf: '{AB6B4AFC-F6E4-11D0-900D-00C04FD9189D}'),
 (name: 'IDirectDrawMediaSample'; itf: '{AB6B4AFE-F6E4-11D0-900D-00C04FD9189D}'),
 (name: 'IAMMediaTypeStream'; itf: '{AB6B4AFA-F6E4-11D0-900D-00C04FD9189D}'),
 (name: 'IAMMediaTypeSample'; itf: '{AB6B4AFB-F6E4-11D0-900D-00C04FD9189D}'),
 (name: 'IDirectDrawMediaStream'; itf: '{F4104FCE-9A70-11D0-8FDE-00C04FD9189D}'),
 (name: 'IDirectDrawStreamSample'; itf: '{F4104FCF-9A70-11D0-8FDE-00C04FD9189D}'),
 (name: 'IAudioMediaStream'; itf: '{F7537560-A3BE-11D0-8212-00C04FC32C45}'),
 (name: 'IAudioStreamSample'; itf: '{345FEE00-ABA5-11D0-8212-00C04FC32C45}'),
 (name: 'IMemoryData'; itf: '{327FC560-AF60-11D0-8212-00C04FC32C45}'),
 (name: 'IAudioData'; itf: '{54C719C0-AF60-11D0-8212-00C04FC32C45}'),
 (name: 'IMixerPinConfig'; itf: '{593CDDE1-0759-11D1-9E69-00C04FD7C15B}'),
 (name: 'IMixerPinConfig2'; itf: '{EBF47182-8764-11D1-9E69-00C04FD7C15B}'),
 (name: 'IAMCollection'; itf: '{56A868B9-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaControl'; itf: '{56A868B1-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaEvent'; itf: '{56A868B6-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaEventEx'; itf: '{56A868C0-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaPosition'; itf: '{56A868B2-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IBasicAudio'; itf: '{56A868B3-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IVideoWindow'; itf: '{56A868B4-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IBasicVideo'; itf: '{56A868B5-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IBasicVideo2'; itf: '{329BB360-F6EA-11D1-9038-00A0C9697298}'),
 (name: 'IDeferredCommand'; itf: '{56A868B8-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IQueueCommand'; itf: '{56A868B7-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IFilterInfo'; itf: '{56A868BA-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IRegFilterInfo'; itf: '{56A868BB-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IMediaTypeInfo'; itf: '{56A868BC-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IPinInfo'; itf: '{56A868BD-0AD4-11CE-B03A-0020AF0BA770}'),
 (name: 'IAMStats'; itf: '{BC9BCF80-DCD2-11D2-ABF6-00A0C905F375}'),
 (name: 'IAMNetShowConfig'; itf: '{FA2AA8F1-8B62-11D0-A520-000000000000}'),
 (name: 'IAMChannelInfo'; itf: '{FA2AA8F2-8B62-11D0-A520-000000000000}'),
 (name: 'IAMNetworkStatus'; itf: '{FA2AA8F3-8B62-11D0-A520-000000000000}'),
 (name: 'IAMExtendedSeeking'; itf: '{FA2AA8F9-8B62-11D0-A520-000000000000}'),
 (name: 'IAMNetShowExProps'; itf: '{FA2AA8F5-8B62-11D0-A520-000000000000}'),
 (name: 'IAMExtendedErrorInfo'; itf: '{FA2AA8F6-8B62-11D0-A520-000000000000}'),
 (name: 'IAMMediaContent'; itf: '{FA2AA8F4-8B62-11D0-A520-000000000000}'),
 (name: 'IAMMediaContent2'; itf: '{CE8F78C1-74D9-11D2-B09D-00A0C9A81117}'),
 (name: 'IAMNetShowPreroll'; itf: '{AAE7E4E2-6388-11D1-8D93-006097C9A2B2}'),
 (name: 'IDShowPlugin'; itf: '{4746B7C8-700E-11D1-BECC-00C04FB6E937}'),
 (name: 'IAMPlayListItem'; itf: '{56A868FF-0AD4-11CE-B0A3-0020AF0BA770}'),
 (name: 'IAMPlayList'; itf: '{56A868FE-0AD4-11CE-B0A3-0020AF0BA770}'),
 (name: 'ISpecifyParticularPages'; itf: '{4C437B91-6E9E-11D1-A704-006097C4E476}'),
 (name: 'IAMRebuild'; itf: '{02EF04DD-7580-11D1-BECE-00C04FB6E937}'),
 (name: 'IDirectDrawVideo'; itf: '{36D39EB0-DD75-11CE-BF0E-00AA0055595A}'),
 (name: 'IQualProp'; itf: '{1BD0ECB0-F8E2-11CE-AAC6-0020AF0B99A3}'),
 (name: 'IFullScreenVideo'; itf: '{DD1D7110-7836-11CF-BF47-00AA0055595A}'),
 (name: 'IFullScreenVideoEx'; itf: '{53479470-F1DD-11CF-BC42-00AA00AC74F6}'),
 (name: 'IBaseVideoMixer'; itf: '{61DED640-E912-11CE-A099-00AA00479A58}'),
 (name: 'IAMDirectSound'; itf: '{546F4260-D53E-11CF-B3F0-00AA003761C5}'),
 (name: 'IVPConfig'; itf: '{BC29A660-30E3-11D0-9E69-00C04FD7C15B}'),
 (name: 'IVPVBIConfig'; itf: '{EC529B00-1A1F-11D1-BAD9-00609744111A}'),
 (name: 'IVPNotify'; itf: '{C76794A1-D6C5-11D0-9E69-00C04FD7C15B}'),
 (name: 'IVPVBINotify'; itf: '{EC529B01-1A1F-11D1-BAD9-00609744111A}'),
 (name: 'IVPNotify2'; itf: '{EBF47183-8764-11D1-9E69-00C04FD7C15B}'),
 (name: 'IMpegAudioDecoder'; itf: '{B45DD570-3C77-11D1-ABE1-00A0C905F375}'),
 (name: 'IAMLine21Decoder'; itf: '{6E8D4A21-310C-11D0-B79A-00AA003767A7}'),
 (name: 'IVPObject'; itf: '{CE292862-FC88-11D0-9E69-00C04FD7C15B}'),
 (name: 'IVPControl'; itf: '{25DF12C1-3DE0-11D1-9E69-00C04FD7C15B}'),
 (name: 'IVPVBIObject'; itf: '{814B9802-1C88-11D1-BAD9-00609744111A}'),
 (name: 'IAMWstDecoder'; itf: '{C056DE21-75C2-11D3-A184-00105AEF9F33}'),
 (name: 'IKsInterfaceHandler'; itf: '{D3ABC7E0-9A61-11D0-A40D-00A0C9223196}'),
 (name: 'IKsDataTypeHandler'; itf: '{5FFBAA02-49A3-11D0-9F36-00AA00A216A1}'),
 (name: 'IKsPin'; itf: '{B61178D1-A2D9-11CF-9E53-00AA00A216A1}'),
 (name: 'IKsControl'; itf: '{28F54685-06FD-11D2-B27A-00A0C9223196}'),
 (name: 'IKsPinFactory'; itf: '{CD5EBE6B-8B6E-11D1-8AE0-00A0C9223196}'),
 (name: 'IAMVideoAcceleratorNotify'; itf: '{256A6A21-FBAD-11D1-82BF-00A0C9696C8F}'),
 (name: 'IAMVideoAccelerator'; itf: '{256A6A22-FBAD-11D1-82BF-00A0C9696C8F}'),
 (name: 'ICreatePropBagOnRegKey'; itf: '{8A674B48-1F63-11D3-B64C-00C04F79498E}'),
 (name: 'ITuningSpaces'            ; itf: '{901284E4-33FE-4b69-8D63-634A596F3756}'),
 (name: 'ITuningSpaceContainer'    ; itf: '{5B692E84-E2F1-11d2-9493-00C04F72D980}'),
 (name: 'ITuningSpace'             ; itf: '{061C6E30-E622-11d2-9493-00C04F72D980}'),
 (name: 'IEnumTuningSpaces'        ; itf: '{8B8EB248-FC2B-11d2-9D8C-00C04F72D980}'),
 (name: 'IDVBTuningSpace'          ; itf: '{ADA0B268-3B19-4e5b-ACC4-49F852BE13BA}'),
 (name: 'IAnalogTVTuningSpace'     ; itf: '{2A6E293C-2595-11d3-B64C-00C04F79498E}'),
 (name: 'IATSCTuningSpace'         ; itf: '{0369B4E2-45B6-11d3-B650-00C04F79498E}'),
 (name: 'IAnalogRadioTuningSpace'  ; itf: '{2A6E293B-2595-11d3-B64C-00C04F79498E}'),
 (name: 'ITuneRequest'             ; itf: '{07DDC146-FC3D-11d2-9D8C-00C04F72D980}'),
 (name: 'IChannelTuneRequest'      ; itf: '{0369B4E0-45B6-11d3-B650-00C04F79498E}'),
 (name: 'IATSCChannelTuneRequest'  ; itf: '{0369B4E1-45B6-11d3-B650-00C04F79498E}'),
 (name: 'IDVBTuneRequest'          ; itf: '{0D6F567E-A636-42bb-83BA-CE4C1704AFA2}'),
 (name: 'ITuner'                   ; itf: '{28C52640-018A-11d3-9D8E-00C04F72D980}'),
 (name: 'IScanningTuner'           ; itf: '{1DFD0A5C-0284-11d3-9D8E-00C04F72D980}'),
 (name: 'ITunerEvents'             ; itf: '{68481420-0280-11d3-9D8E-00C04F72D980}'),
 (name: 'ISignalEvents'            ; itf: '{85E2439E-0E23-11d3-9D8E-00C04F72D980}'),
 (name: 'IComponentType'           ; itf: '{6A340DC0-0311-11d3-9D8E-00C04F72D980}'),
 (name: 'ILanguageComponentType'   ; itf: '{B874C8BA-0FA2-11d3-9D8E-00C04F72D980}'),
 (name: 'IMPEG2ComponentType'      ; itf: '{2C073D84-B51C-48c9-AA9F-68971E1F6E38}'),
 (name: 'IATSCComponentType'       ; itf: '{FC189E4D-7BD4-4125-B3B3-3A76A332CC96}'),
 (name: 'IEnumComponentTypes'      ; itf: '{8A674B4A-1F63-11d3-B64C-00C04F79498E}'),
 (name: 'IComponentTypes'          ; itf: '{0DC13D4A-0313-11d3-9D8E-00C04F72D980}'),
 (name: 'IComponent'               ; itf: '{1A5576FC-0E19-11d3-9D8E-00C04F72D980}'),
 (name: 'IMPEG2Component'          ; itf: '{1493E353-1EB6-473c-802D-8E6B8EC9D2A9}'),
 (name: 'IEnumComponents'          ; itf: '{2A6E2939-2595-11d3-B64C-00C04F79498E}'),
 (name: 'IComponents'              ; itf: '{FCD01846-0E19-11d3-9D8E-00C04F72D980}'),
 (name: 'ILocator'                 ; itf: '{286D7F89-760C-4F89-80C4-66841D2507AA}'),
 (name: 'IATSCLocator'             ; itf: '{BF8D986F-8C2B-4131-94D7-4D3D9FCC21EF}'),
 (name: 'IDVBTLocator'             ; itf: '{8664DA16-DDA2-42ac-926A-C18F9127C302}'),
 (name: 'IDVBSLocator'             ; itf: '{3D7C353C-0D04-45f1-A742-F97CC1188DC8}'),
 (name: 'IDVBCLocator'             ; itf: '{6E42F36E-1DD2-43c4-9F78-69D25AE39034}'),
 (name: 'IDVBTuningSpace2'         ; itf: '{843188B4-CE62-43db-966B-8145A094E040}'),
 (name: 'IDVBSTuningSpace'         ; itf: '{CDF7BE60-D954-42fd-A972-78971958E470}'),
 (name: 'IMPEG2TuneRequest'        ; itf: '{EB7D987F-8A01-42AD-B8AE-574DEEE44D1A}'),
 (name: 'IMPEG2TuneRequestFactory' ; itf: '{14E11ABD-EE37-4893-9EA1-6964DE933E39}'),
 (name: 'IMPEG2TuneRequestSupport' ; itf: '{1B9D5FC3-5BBC-4b6c-BB18-B9D10E3EEEBF}'),
 (name: 'IBroadcastEvent'          ; itf: '{3B21263F-26E8-489d-AAC4-924F7EFD9511}'),
 (name: 'IDXBaseObject'        ; itf: '{17B59B2B-9CC8-11D1-9053-00C04FD9189D}'),
 (name: 'IDXTransformFactory'  ; itf: '{6A950B2B-A971-11D1-81C8-0000F87557DB}'),
 (name: 'IDXTransform'         ; itf: '{30A5FB78-E11F-11D1-9064-00C04FD9189D}'),
 (name: 'IDXSurfacePick'       ; itf: '{30A5FB79-E11F-11d1-9064-00C04FD9189D}'),
 (name: 'IDXTBindHost'         ; itf: '{D26BCE55-E9DC-11d1-9066-00C04FD9189D}'),
 (name: 'IDXTaskManager'       ; itf: '{254DBBC1-F922-11D0-883A-3C8B00C10000}'),
 (name: 'IDXSurfaceFactory'    ; itf: '{144946F5-C4D4-11D1-81D1-0000F87557DB}'),
 (name: 'IDXSurfaceModifier'   ; itf: '{9EA3B637-C37D-11D1-905E-00C04FD9189D}'),
 (name: 'IDXSurface'           ; itf: '{B39FD73F-E139-11D1-9065-00C04FD9189D}'),
 (name: 'IDXSurfaceInit'       ; itf: '{9EA3B639-C37D-11d1-905E-00C04FD9189D}'),
 (name: 'IDXARGBSurfaceInit'   ; itf: '{9EA3B63A-C37D-11d1-905E-00C04FD9189D}'),
 (name: 'IDXARGBReadPtr'       ; itf: '{EAAAC2D6-C290-11d1-905D-00C04FD9189D}'),
 (name: 'IDXARGBReadWritePtr'  ; itf: '{EAAAC2D7-C290-11d1-905D-00C04FD9189D}'),
 (name: 'IDXDCLock'            ; itf: '{0F619456-CF39-11D1-905E-00C04FD9189D}'),
 (name: 'IDXTScaleOutput'      ; itf: '{B2024B50-EE77-11D1-9066-00C04FD9189D}'),
 (name: 'IDXGradient'          ; itf: '{B2024B51-EE77-11D1-9066-00C04FD9189D}'),
 (name: 'IDXTScale'            ; itf: '{B39FD742-E139-11D1-9065-00C04FD9189D}'),
 (name: 'IDXEffect'            ; itf: '{E31FB81B-1335-11d1-8189-0000F87557DB}'),
 (name: 'IDXLookupTable'       ; itf: '{01BAFC7F-9E63-11D1-9053-00C04FD9189D}'),
 (name: 'IDXRawSurface'        ; itf: '{09756C8A-D96A-11d1-9062-00C04FD9189D}'),
 (name: 'IHTMLDXTransform'     ; itf: '{30E2AB7D-4FDD-4159-B7EA-DC722BF4ADE5}'),
 (name: 'IMpegVideoDecoder'    ; itf: '{EB1BB270-F71F-11CE-8E85-02608C9BABA2}'),
 (name: 'IConfigAsfWriter'     ; itf: '{45086030-F7E4-486A-B504-826BB5792A3B}'),
 (name: 'IAMParse'             ; itf: '{C47A3420-005C-11D2-9038-00A0C9697298}'),
 (name: 'IMediaParamInfo'      ; itf: '{6D6CBB60-A223-44AA-842F-A2F06750BE6D}'),
 (name: 'IMediaParams'         ; itf: '{6D6CBB61-A223-44AA-842F-A2F06750BE6E}'),
 (name: 'IMediaBuffer'         ; itf: '{59EFF8B9-938C-4A26-82F2-95CB84CDC837}'),
 (name: 'IMediaObject'         ; itf: '{D8AD0F58-5494-4102-97C5-EC798E59BCF4}'),
 (name: 'IEnumDMO'             ; itf: '{2C3CD98A-2BFA-4A53-9C27-5249BA64BA0F}'),
 (name: 'IMediaObjectInPlace'  ; itf: '{651B9AD0-0FC7-4AA9-9538-D89931010741}'),
 (name: 'IDMOQualityControl'   ; itf: '{65ABEA96-CF36-453F-AF8A-705E98F16260}'),
 (name: 'IDMOVideoOutputOptimizations'; itf: '{BE8F4F4E-5B16-4D29-B350-7F6B5D9298AC}'),
 (name: 'IDMOWrapperFilter'       ; itf: '{52D6F586-9F0F-4824-8FC8-E32CA04930C2}'),
 (name: 'IKsObject'               ; itf: '{423C13A2-2070-11D0-9EF7-00AA00A216A1}'),
 (name: 'IKsPinEx'                ; itf: '{7BB38260-D19C-11D2-B38A-00A0C95EC22E}'),
 (name: 'IKsPinPipe'              ; itf: '{E539CD90-A8B4-11D1-8189-00A0C9062802}'),
 (name: 'IKsDataTypeCompletion'   ; itf: '{827D1A0E-0F73-11D2-B27A-00A0C9223196}'),
 (name: 'IKsClockPropertySet'     ; itf: '{5C5CBD84-E755-11D0-AC18-00A0C9223196}'),
 (name: 'IKsAllocator'            ; itf: '{8DA64899-C0D9-11D0-8413-0000F822FE8A}'),
 (name: 'IKsAllocatorEx'          ; itf: '{091BB63A-603F-11D1-B067-00A0C9062802}'),
 (name: 'IKsTopology'             ; itf: '{28F54683-06FD-11D2-B27A-00A0C9223196}'),
 (name: 'IKsAggregateControl'     ; itf: '{7F40EAC0-3947-11D2-874E-00A0C9223196}'),
 (name: 'IKsQualityForwarder'     ; itf: '{97EBAACB-95BD-11D0-A3EA-00A0C9223196}'),
 (name: 'IPropertySetter'         ; itf: '{AE9472BD-B0C3-11D2-8D24-00A0C9441E20}'),
 (name: 'IDxtCompositor'          ; itf: '{BB44391E-6ABD-422F-9E2E-385C9DFF51FC}'),
 (name: 'IDxtAlphaSetter'         ; itf: '{4EE9EAD9-DA4D-43D0-9383-06B90C08B12B}'),
 (name: 'IDxtJpeg'                ; itf: '{DE75D011-7A65-11D2-8CEA-00A0C9441E20}'),
 (name: 'IDxtKey'                 ; itf: '{3255DE56-38FB-4901-B980-94B438010D7B}'),
 (name: 'IMediaLocator'           ; itf: '{288581E0-66CE-11D2-918F-00C0DF10D434}'),
 (name: 'IMediaDet'               ; itf: '{65BD0710-24D2-4FF7-9324-ED2E5D3ABAFA}'),
 (name: 'IGrfCache'               ; itf: '{AE9472BE-B0C3-11D2-8D24-00A0C9441E20}'),
 (name: 'IRenderEngine'           ; itf: '{6BEE3A81-66C9-11D2-918F-00C0DF10D434}'),
 (name: 'IFindCompressorCB'       ; itf: '{F03FA8DE-879A-4D59-9B2C-26BB1CF83461}'),
 (name: 'ISmartRenderEngine'      ; itf: '{F03FA8CE-879A-4D59-9B2C-26BB1CF83461}'),
 (name: 'IAMTimelineObj'          ; itf: '{78530B77-61F9-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineEffectable'   ; itf: '{EAE58537-622E-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineEffect'       ; itf: '{BCE0C264-622D-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineTransable'    ; itf: '{378FA386-622E-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineSplittable'   ; itf: '{A0F840A0-D590-11D2-8D55-00A0C9441E20}'),
 (name: 'IAMTimelineTrans'        ; itf: '{BCE0C265-622D-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineSrc'          ; itf: '{78530B79-61F9-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineTrack'        ; itf: '{EAE58538-622E-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineVirtualTrack' ; itf: '{A8ED5F80-C2C7-11D2-8D39-00A0C9441E20}'),
 (name: 'IAMTimelineComp'         ; itf: '{EAE58536-622E-11D2-8CAD-00A024580902}'),
 (name: 'IAMTimelineGroup'        ; itf: '{9EED4F00-B8A6-11D2-8023-00C0DF10D434}'),
 (name: 'IAMTimeline'             ; itf: '{78530B74-61F9-11D2-8CAD-00A024580902}'),
 (name: 'IXml2Dex'                ; itf: '{18C628ED-962A-11D2-8D08-00A0C9441E20}'),
 (name: 'IAMErrorLog'             ; itf: '{E43E73A2-0EFA-11D3-9601-00A0C9441E20}'),
 (name: 'IAMSetErrorLog'          ; itf: '{963566DA-BE21-4EAF-88E9-35704F8F52A1}'),
 (name: 'ISampleGrabberCB'        ; itf: '{0579154A-2B53-4994-B0D0-E773148EFF85}'),
 (name: 'ISampleGrabber'          ; itf: '{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}'),
 (name: 'IBDA_NetworkProvider'    ; itf: '{fd501041-8ebe-11ce-8183-00aa00577da2}'),
 (name: 'IBDA_EthernetFilter'     ; itf: '{71985F43-1CA1-11d3-9CC8-00C04F7971E0}'),
 (name: 'IBDA_IPV4Filter'         ; itf: '{71985F44-1CA1-11d3-9CC8-00C04F7971E0}'),
 (name: 'IBDA_IPV6Filter'         ; itf: '{E1785A74-2A23-4fb3-9245-A8F88017EF33}'),
 (name: 'IBDA_DeviceControl'      ; itf: '{FD0A5AF3-B41D-11d2-9C95-00C04F7971E0}'),
 (name: 'IBDA_PinControl'         ; itf: '{0DED49D5-A8B7-4d5d-97A1-12B0C195874D}'),
 (name: 'IBDA_SignalProperties'   ; itf: '{D2F1644B-B409-11d2-BC69-00A0C9EE9E16}'),
 (name: 'IBDA_SignalStatistics'   ; itf: '{1347D106-CF3A-428a-A5CB-AC0D9A2A4338}'),
 (name: 'IBDA_Topology'           ; itf: '{79B56888-7FEA-4690-B45D-38FD3C7849BE}'),
 (name: 'IBDA_VoidTransform'      ; itf: '{71985F46-1CA1-11d3-9CC8-00C04F7971E0}'),
 (name: 'IBDA_NullTransform'      ; itf: '{DDF15B0D-BD25-11d2-9CA0-00C04F7971E0}'),
 (name: 'IBDA_FrequencyFilter'    ; itf: '{71985F47-1CA1-11d3-9CC8-00C04F7971E0}'),
 (name: 'IBDA_LNBInfo'            ; itf: '{992CF102-49F9-4719-A664-C4F23E2408F4}'),
 (name: 'IBDA_AutoDemodulate'     ; itf: '{DDF15B12-BD25-11d2-9CA0-00C04F7971E0}'),
 (name: 'IBDA_DigitalDemodulator' ; itf: '{EF30F379-985B-4d10-B640-A79D5E04E1E0}'),
 (name: 'IBDA_IPSinkControl'      ; itf: '{3F4DC8E2-4050-11d3-8F4B-00C04F7971E2}'),
 (name: 'IBDA_IPSinkInfo'         ; itf: '{A750108F-492E-4d51-95F7-649B23FF7AD7}'),
 (name: 'IEnumPIDMap'             ; itf: '{afb6c2a2-2c41-11d3-8a60-0000f81e0e4a}'),
 (name: 'IMPEG2PIDMap'            ; itf: '{afb6c2a1-2c41-11d3-8a60-0000f81e0e4a}'),
 (name: 'IMPEG2_TIF_CONTROL'       ; itf: '{F9BAC2F9-4149-4916-B2EF-FAA202326862}'),
 (name: 'ITuneRequestInfo'         ; itf: '{A3B152DF-7A90-4218-AC54-9830BEE8C0B6}'),
 (name: 'IGuideDataEvent'          ; itf: '{EFDA0C80-F395-42c3-9B3C-56B37DEC7BB7}'),
 (name: 'IGuideDataProperty'       ; itf: '{88EC5E58-BB73-41d6-99CE-66C524B8B591}'),
 (name: 'IEnumGuideDataProperties' ; itf: '{AE44423B-4571-475c-AD2C-F40A771D80EF}'),
 (name: 'IEnumTuneRequests'        ; itf: '{1993299C-CED6-4788-87A3-420067DCE0C7}'),
 (name: 'IGuideData'               ; itf: '{61571138-5B01-43cd-AEAF-60B784A0BF93}'),
 (name: 'IGuideDataLoader'         ; itf: '{4764ff7c-fa95-4525-af4d-d32236db9e38}'),
 (name: 'IVPEConfig' ; itf: '{BC29A660-30E3-11d0-9E69-00C04FD7C15B}'),
 (name: 'IVPE'       ; itf: '{BC29A661-30E3-11d0-9E69-00C04FD7C15B}'),
 (name: 'IDivXFilterInterface' ; itf: '{D132EE97-3E38-4030-8B17-59163B30A1F5}'),

 (name: 'IVMRAspectRatioControl9'     ; itf: '{00d96c29-bbde-4efc-9901-bb5036392146}'),
 (name: 'IVMRDeinterlaceControl9'     ; itf: '{a215fb8d-13c2-4f7f-993c-003d6271a459}'),
 (name: 'IVMRFilterConfig9'           ; itf: '{5a804648-4f66-4867-9c43-4f5c822cf1b8}'),
 (name: 'IVMRImageCompositor9'        ; itf: '{4a5c89eb-df51-4654-ac2a-e48e02bbabf6}'),
 (name: 'IVMRImagePresenter9'         ; itf: '{69188c61-12a3-40f0-8ffc-342e7b433fd7}'),
 (name: 'IVMRImagePresenterConfig9'   ; itf: '{45c15cab-6e22-420a-8043-ae1f0ac02c7d}'),
 (name: 'IVMRMixerBitmap9'            ; itf: '{ced175e5-1935-4820-81bd-ff6ad00c9108}'),
 (name: 'IVMRMixerControl9'           ; itf: '{1a777eaa-47c8-4930-b2c9-8fee1c1b0f3b}'),
 (name: 'IVMRMonitorConfig9'          ; itf: '{46c2e457-8ba0-4eef-b80b-0680f0978749}'),
 (name: 'IVMRSurface9'                ; itf: '{dfc581a1-6e1f-4c3a-8d0a-5e9792ea2afc}'),
 (name: 'IVMRSurfaceAllocator9'       ; itf: '{8d5148ea-3f5d-46cf-9df1-d1b896eedb1f}'),
 (name: 'IVMRSurfaceAllocatorNotify9' ; itf: '{dca3f5df-bb3a-4d03-bd81-84614bfbfa0c}'),
 (name: 'IVMRWindowlessControl9'      ; itf: '{8f537d09-f85e-4414-b23b-502e54c79927}'),
 (name: 'IVMRVideoStreamControl9'     ; itf: '{d0cfe38b-93e7-4772-8957-0400c49a4485}'),

 (name: 'IEnumStreamBufferRecordingAttrib' ; itf: '{C18A9162-1E82-4142-8C73-5690FA62FE33}'),
 (name: 'IStreamBufferConfigure'           ; itf: '{ce14dfae-4098-4af7-bbf7-d6511f835414}'),
 (name: 'IStreamBufferInitialize'          ; itf: '{9ce50f2d-6ba7-40fb-a034-50b1a674ec78}'),
 (name: 'IStreamBufferMediaSeeking'        ; itf: '{f61f5c26-863d-4afa-b0ba-2f81dc978596}'),
 (name: 'IStreamBufferRecComp'             ; itf: '{9E259A9B-8815-42ae-B09F-221970B154FD}'),
 (name: 'IStreamBufferRecordControl'       ; itf: '{ba9b6c99-f3c7-4ff2-92db-cfdd4851bf31}'),
 (name: 'IStreamBufferRecordingAttribute'  ; itf: '{16CA4E03-FE69-4705-BD41-5B7DFC0C95F3}'),
 (name: 'IStreamBufferSink'                ; itf: '{afd1f242-7efd-45ee-ba4e-407a25c9a77a}'),
 (name: 'IStreamBufferSource'              ; itf: '{1c5bd776-6ced-4f44-8164-5eab0e98db12}'),

 // ole
 (name: 'IPersist'              ; itf: '{0000010C-0000-0000-C000-000000000046}'),
 (name: 'IPersistStream'        ; itf: '{00000109-0000-0000-C000-000000000046}'),
 (name: 'IPersistFile'          ; itf: '{0000010B-0000-0000-C000-000000000046}'),
 (name: 'IPersistPropertyBag'   ; itf: '{37D84F60-42CB-11CE-8135-00AA004BB851}'),
 (name: 'IPersistStorage'       ; itf: '{0000010A-0000-0000-C000-000000000046}'),
 (name: 'IMoniker'              ; itf: '{0000000F-0000-0000-C000-000000000046}'),
 (name: 'IStream'               ; itf: '{0000000C-0000-0000-C000-000000000046}'),
 (name: 'IBindCtx'              ; itf: '{0000000E-0000-0000-C000-000000000046}'),
 (name: 'IRunningObjectTable'   ; itf: '{00000010-0000-0000-C000-000000000046}'),
 (name: 'IPropertyBag'          ; itf: '{55272A00-42CB-11CE-8135-00AA004BB851}'),
 (name: 'IPropertyPage'         ; itf: '{B196B28D-BAB4-101A-B69C-00AA00341D07}'),
 (name: 'IPropertyPage2'        ; itf: '{01E44665-24AC-101B-84ED-08002B2EC713}'),
 (name: 'IPropertyStorage'      ; itf: '{00000138-0000-0000-C000-000000000046}'),
 (name: 'IServiceProvider'      ; itf: '{6d5140c1-7436-11ce-8034-00aa006009fa}'),
 (name: 'ISpecifyPropertyPages' ; itf: '{B196B28B-BAB4-101A-B69C-00AA00341D07}'),
 (name: 'IErrorLog'             ; itf: '{3127CA40-446E-11CE-8135-00AA004BB851}'),
 (name: 'IErrorInfo'            ; itf: '{1CF2B120-547D-101B-8E65-08002B2BD119}'),
 (name: 'IStorage'              ; itf: '{0000000B-0000-0000-C000-000000000046}'));


var
  FormBaseFilter: TFormBaseFilter;


implementation
uses Activex, ComObj;

{$R *.dfm}
{$R BaseFilterEditor.res}

  constructor TFormBaseFilter.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    SysDevEnum := TSysDevEnum.Create;
    PinList:= TPinList.Create;
    InPinPic := TBitmap.Create;
    OutPinPic := TBitmap.Create;
    InPinPic.LoadFromResourceName(hinstance, 'INPUT');
    OutPinPic.LoadFromResourceName(hinstance, 'OUTPUT');
    InfoBtn.Glyph.LoadFromResourceName(hinstance, 'INFO');
    Image1.Picture.Bitmap.LoadFromResourceName(hinstance, 'GREEN_MARK');
    Image2.Picture.Bitmap.LoadFromResourceName(hinstance, 'RED_MARK');
    Image3.Picture.Bitmap.LoadFromResourceName(hinstance, 'BLUE_MARK');
    Image4.Picture.Bitmap.LoadFromResourceName(hinstance, 'PURPLE_MARK');
    Image5.Picture.Bitmap.LoadFromResourceName(hinstance, 'BLACK_MARK');
    Image6.Picture.Bitmap.LoadFromResourceName(hinstance, 'INPUT');
    Image7.Picture.Bitmap.LoadFromResourceName(hinstance, 'OUTPUT');
  end;

  destructor  TFormBaseFilter.Destroy;
  begin
    SysDevEnum.Free;
    PinList.Free;
    InPinPic.Free;
    OutPinPic.Free;
    inherited destroy;
  end;

procedure TFormBaseFilter.FormShow(Sender: TObject);
var
  i, j: integer;
  a, b : integer;
  AMoniker, MyMoniker: IMoniker;
  PropBag: IPropertyBag;
  AVariant: OleVariant;
  CLSID: TGUID;
  Found: boolean;
  NodeItem : pNodeItem;
  TmpItem : TTreeNode;
begin
  PageControl1.ActivePageIndex := 0;
  RefreshTree;
  Found := False;
  j := 0;
  MyMoniker := Filter.BaseFilter.Moniker;
  if MyMoniker = nil then exit;
  MyMoniker.BindToStorage(nil,nil,IPropertyBag, PropBag);
  if PropBag.Read('CLSID',AVariant,nil) = S_OK then
       CLSID := StringToGUID(AVariant)
  else CLSID := GUID_NULL;
  for i := 0 to SysDevEnum.CountCategories - 1 do
  begin
    SysDevEnum.SelectIndexCategory(i);
    if SysDevEnum.CountFilters > 0 then
      for j := 0 to SysDevEnum.CountFilters - 1 do
      begin
        if IsEqualGUID(CLSID, SysDevEnum.Filters[j].CLSID) then
          begin
            AMoniker := SysDevEnum.GetMoniker(j);
            Found := AMoniker.IsEqual(MyMoniker) = S_OK;
            AMoniker := nil;
          end;
        if Found then Break;
      end;
    Filters.SetFocus;
    if Found then
    begin
      for a := 0 to Filters.Items.Count -1 do
      begin
        NodeItem := pNodeItem(Filters.Items[a].Data);
        if (Filters.Items[a].Level = 0) and (NodeItem^.Index = I) then
        begin
          TmpItem := Filters.Items[a];
          for b := 0 to TmpItem.Count -1 do
          begin
            NodeItem := pNodeItem(TmpItem.Item[b].data);
            If NodeItem^.Index = J then
            begin
              Filters.Selected := TmpItem.Item[b];
              Filterschange(Self, TmpItem.Item[b]);
            end;
          end;
        end;
      end;
      break;
    end;
  end;
  PropBag := nil;
  MyMoniker := nil;
end;

procedure TFormBaseFilter.GetFilterInfo;
var
  i: integer;
  unk: IUnknown;
  PinInfo: TPinInfo;
  BaseF: IBaseFilter;
begin
  BaseF := nil;
  try
    if Filter.BaseFilter.CreateFilter <> nil then
    begin
      for i := 0 to length(DSItfs)-1 do
      begin
        unk := nil;
        if Succeeded(IFilter(Filter).QueryInterface(DSItfs[i].itf, unk)) then
          Interfaces.Items.Add(DSItfs[i].name);
      end;

      if Succeeded(Filter.QueryInterface(IBaseFilter, BaseF)) then
      begin
        PinList.Assign(BaseF);
        if PinList.Count > 0 then
          for i := 0 to PinList.Count - 1 do
          begin
            PinInfo := PinList.PinInfo[i];
            case PinInfo.dir of
              PINDIR_INPUT  : Pins.Items.AddObject(format('%s',[PinInfo.achName]), InPinPic);
              PINDIR_OUTPUT : Pins.Items.AddObject(format('%s',[PinInfo.achName]), OutPinPic);
            end;
            PinInfo.pFilter := nil;
          end;
      End;
    end;
  finally
    unk := nil;
    BaseF := nil;
  end
end;

procedure TFormBaseFilter.RefreshTree;
var
  i, j : Integer;
  TmpNode : TTreeNode;
  NodeItem : pNodeItem;
  MonikerStr : PWideChar;
  Tmp : WideString;
begin
  Filters.Items.BeginUpdate;
  Filters.Items.Clear;
  for i := 0 to SysDevEnum.CountCategories - 1 do
  begin
    New(NodeItem);
    NodeItem^.Index := i;
    TmpNode := Filters.Items.AddChildObject(nil, SysDevEnum.Categories[i].FriendlyName, NodeItem);
    SysDevEnum.SelectIndexCategory(i);
    if SysDevEnum.CountFilters > 0 then
      for j := 0 to SysDevEnum.CountFilters - 1 do
      begin
        SysDevEnum.GetMoniker(j).GetDisplayName(nil, nil, MonikerStr);
        New(NodeItem);
        NodeItem^.Index := j;
        NodeItem^.Color := FilterColors[0];
        Tmp := MonikerStr;
        Tmp := Copy(Tmp, Pos(':', Tmp)+1, 255);
        Tmp := uppercase(Copy(Tmp, 1, pos(':', Tmp) -1));
        If Tmp = 'PNP' then NodeItem^.Color := FilterColors[1];
        If Tmp = 'CM' then NodeItem^.Color := FilterColors[2];
        If Tmp = 'KS' then NodeItem^.Color := FilterColors[3];
        If Tmp = 'DMO' then NodeItem^.Color := FilterColors[4];
        Filters.Items.AddChildObject(TmpNode, SysDevEnum.Filters[j].FriendlyName, NodeItem);
      end;
  end;
  {$IFDEF VER130}
  Filters.AlphaSort;
  {$ELSE}
  Filters.AlphaSort(True);
  {$ENDIF}
  Filters.Items.EndUpdate;
end;

procedure TFormBaseFilter.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  PinList.Clear;
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
end;

procedure TFormBaseFilter.PopupMenuPopup(Sender: TObject);
var
  AFilter: IBaseFilter;
begin
  PopupMenu.Items.Items[0].Enabled := false;
  PopupMenu.Items.Items[1].Items[0].Enabled := false;
  PopupMenu.Items.Items[1].Items[1].Enabled := false;
  PopupMenu.Items.Items[1].Items[2].Enabled := false;
  PopupMenu.Items.Items[2].Items[0].Enabled := false;
  PopupMenu.Items.Items[2].Items[1].Enabled := false;
  If (Filters.Selected = Nil) or (Filters.Selected.Level = 0) then Abort;

  Filter.QueryInterface(IBaseFilter, AFilter);
  if HaveFilterPropertyPage(AFilter, ppDefault)       then PopupMenu.Items.Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapFormat)  then PopupMenu.Items.Items[1].Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapSource)  then PopupMenu.Items.Items[1].Items[1].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCapDisplay) then PopupMenu.Items.Items[1].Items[2].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCompConfig) then PopupMenu.Items.Items[2].Items[0].Enabled := true;
  if HaveFilterPropertyPage(AFilter, ppVFWCompAbout)  then PopupMenu.Items.Items[2].Items[1].Enabled := true;
  AFilter := nil;
end;

procedure TFormBaseFilter.PropertyPageClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppDefault);
  AFilter := nil;
end;

procedure TFormBaseFilter.PinMenuPopup(Sender: TObject);
begin
  If PageControl1.ActivePageIndex = 0 then
  Begin
    if Pins.ItemIndex = -1 then abort;
  End
  else
    if Pins1.ItemIndex = -1 then abort;
end;

procedure TFormBaseFilter.PinPropertyClick(Sender: TObject);
begin
  If PageControl1.ActivePageIndex = 0 then
    ShowPinPropertyPage(Self.Handle, PinList.Items[Pins.ItemIndex])
  else
    ShowPinPropertyPage(Self.Handle, PinList.Items[Pins1.ItemIndex]);
end;

procedure TFormBaseFilter.PinsSheetShow(Sender: TObject);
begin
  Pins1.Items := Pins.Items;
end;

procedure TFormBaseFilter.Pins1Click(Sender: TObject);
var
  i: integer;
  unk: IUnknown;
  EnumMT : TEnumMediaType;
begin
  PinInterfaces.Clear;
  if Pins1.ItemIndex <> -1 then
  try
    with PinList.Items[Pins1.ItemIndex] do
      for i := 0 to length(DSItfs)-1 do
        if Succeeded(QueryInterface(DSItfs[i].itf, unk)) then
          PinInterfaces.Items.Add(DSItfs[i].name);
  finally
    unk := nil;
  end;

  MediaTypes.Clear;
  if Pins1.ItemIndex <> -1 then
  begin
    EnumMT:= TEnumMediaType.Create(PinList.Items[Pins1.ItemIndex]);
    try
      if EnumMT.Count > 0 then
        for i := 0 to EnumMT.Count - 1 do
          MediaTypes.Items.Add(EnumMt.MediaDescription[i]);
    finally
      EnumMT.Free;
    end;
  end;
end;

procedure TFormBaseFilter.VFWFormatClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCapFormat);
  AFilter := nil;
end;

procedure TFormBaseFilter.VFWSourceClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter , ppVFWCapSource);
  AFilter := nil;
end;

procedure TFormBaseFilter.VFWDisplayClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCapDisplay);
  AFilter := nil;
end;

procedure TFormBaseFilter.Config1Click(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCompConfig);
  AFilter := nil;
end;

procedure TFormBaseFilter.VFWAboutClick(Sender: TObject);
var AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  ShowFilterPropertyPage(Self.Handle, AFilter, ppVFWCompAbout);
  AFilter := nil;
end;

procedure TFormBaseFilter.InterfacesDblClick(Sender: TObject);
var
  AFilter: IBaseFilter;
begin
  Filter.QueryInterface(IBaseFilter, AFilter);
  if Interfaces.ItemIndex <> -1 then
  if Interfaces.Items.Strings[Interfaces.ItemIndex] = 'ISpecifyPropertyPages' then
    ShowFilterPropertyPage(Self.Handle, AFilter);
  AFilter := nil;
end;

procedure TFormBaseFilter.PinInterfacesDblClick(Sender: TObject);
begin
  if PinInterfaces.ItemIndex <> -1 then
  if PinInterfaces.Items.Strings[PinInterfaces.ItemIndex] = 'ISpecifyPropertyPages' then
    ShowPinPropertyPage(Self.Handle, PinList.Items[Pins1.ItemIndex]);
end;

procedure TFormBaseFilter.PinsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	Bitmap: TBitmap;      { temporary variable for the item’s bitmap }
	Offset: Integer;      { text offset width }
begin
  with (Control as TListBox).Canvas do  { draw on control canvas, not on the form }
  begin
    Brush.Color := (Control as TListBox).Color;
    Brush.Style := bsSolid;
    FillRect(Rect);       { clear the rectangle }
    Offset := 2;          { provide default offset }
    Bitmap := TBitmap((Control as TListBox).Items.Objects[Index]);  { get the bitmap }
    if Bitmap <> nil then
    begin
      BrushCopy(Bounds(Rect.Left + Offset, Rect.Top, Bitmap.Width, Bitmap.Height),
                Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clOlive);  {render bitmap}
      Offset := Bitmap.width + 6;    { add four pixels between bitmap and text}
    end;
    If odSelected	in State then
      Brush.Color := clHighLight;
    TextOut(Rect.Left + Offset, Rect.Top, (Control as TListBox).Items[Index]);  { display the text }
    If odFocused in state then
    Begin
      Drawfocusrect(Rect);
      Drawfocusrect(Bounds(Rect.Left + Offset, Rect.Top, TextWidth((Control as TListBox).Items[Index])+1, Rect.Bottom - Rect.Top));
    End;
  end;
end;

procedure TFormBaseFilter.FiltersChange(Sender: TObject;
  Node: TTreeNode);
var
  NodeItem : pNodeItem;
  Tmp : pWidechar;
begin
  if Node = nil then Exit;
  Interfaces.Items.BeginUpdate;
  Interfaces.Items.Clear;
  Pins.Items.BeginUpdate;
  Pins.Items.Clear;
  MonikerTag.Clear;
  Try
    if Node.Level = 1 then
    begin
      NodeItem := pNodeItem(Node.Parent.Data);
      If NodeItem = nil then exit;
      SysDevEnum.SelectIndexCategory(NodeItem^.Index);
      NodeItem := pNodeItem(Node.Data);
      SysDevEnum.GetMoniker(NodeItem^.Index).GetDisplayName(nil, nil, Tmp);
      MonikerTag.Text := Tmp;
      Filter.BaseFilter.Moniker := SysDevEnum.GetMoniker(NodeItem^.Index);
      IFilter(Filter).NotifyFilter(foRefresh);
      GetFilterInfo;
    end;
  finally
    Interfaces.Items.EndUpdate;
    Pins.Items.EndUpdate;
  end;
end;

procedure TFormBaseFilter.PinsMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  (Control as TListBox).Canvas.Font := (Control as TListBox).Font;
  If Height < (Control as TListBox).Canvas.TextHeight((Control as TListBox).Items[Index]) then
    Height := (Control as TListBox).Canvas.TextHeight((Control as TListBox).Items[Index]);
end;

procedure TFormBaseFilter.FiltersDblClick(Sender: TObject);
begin
  If Filters.Selected = nil then Exit;
  If Filters.Selected.Level = 1 then btok.Click;
end;

procedure TFormBaseFilter.FiltersCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeItem : pNodeItem;
begin
  Filters.Canvas.Font.Color := FilterColors[0];
  if Node = nil then Exit;
  if Node.Level < 1 then exit;
  NodeItem := pNodeItem(Node.Data);
  Filters.Canvas.Font.Color := NodeItem^.Color;
end;

procedure TFormBaseFilter.InfoBtnClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 2;
end;

end.


