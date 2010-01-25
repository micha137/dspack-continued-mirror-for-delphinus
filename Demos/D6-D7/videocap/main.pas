unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSPack, DSUtil, DirectShow9, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    CaptureGraph: TFilterGraph;
    VideoWindow: TVideoWindow;
    VideoCapFilters: TListBox;
    VideoSourceFilter: TFilter;
    StartButton: TButton;
    CapFileButton: TButton;
    SaveDialog: TSaveDialog;
    OutPutFileName: TLabel;
    StatusBar: TStatusBar;
    Timer: TTimer;
    StopButton: TButton;
    AudioCapFilters: TListBox;
    AudioSourceFilter: TFilter;
    Label1: TLabel;
    Label2: TLabel;
    VideoFormats: TListBox;
    AudioFormats: TListBox;
    Label3: TLabel;
    Label4: TLabel;
    InputLines: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VideoCapFiltersClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure CapFileButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure AudioCapFiltersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  CapEnum: TSysDevEnum;
  VideoMediaTypes, AudioMediaTypes: TEnumMediaType;
  CapFile: WideString = 'c:\capture.avi';
implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var i: integer;
begin
  CapEnum := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);
  for i := 0 to CapEnum.CountFilters - 1 do
    VideoCapFilters.Items.Add(CapEnum.Filters[i].FriendlyName);

  CapEnum.SelectGUIDCategory(CLSID_AudioInputDeviceCategory);
  for i := 0 to CapEnum.CountFilters - 1 do
    AudioCapFilters.Items.Add(CapEnum.Filters[i].FriendlyName);

  VideoMediaTypes := TEnumMediaType.Create;
  AudioMediaTypes := TEnumMediaType.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CapEnum.Free;
  VideoMediaTypes.Free;
  AudioMediaTypes.Free;
end;

// Select the video Source
procedure TMainForm.VideoCapFiltersClick(Sender: TObject);
var
  PinList: TPinList;
  i: integer;
begin
  CapEnum.SelectGUIDCategory(CLSID_VideoInputDeviceCategory);
  if VideoCapFilters.ItemIndex <> -1 then
  begin
    VideoSourceFilter.BaseFilter.Moniker := CapEnum.GetMoniker(VideoCapFilters.ItemIndex);
    VideoSourceFilter.FilterGraph := CaptureGraph;
    CaptureGraph.Active := true;
    PinList := TPinList.Create(VideoSourceFilter as IBaseFilter);
    VideoFormats.Clear;
    VideoMediaTypes.Assign(PinList.First);
    for i := 0 to VideoMediaTypes.Count - 1 do
      VideoFormats.Items.Add(VideoMediaTypes.MediaDescription[i]);
    CaptureGraph.Active := false;
    PinList.Free;
    StartButton.Enabled := true;
  end;
end;

// Select the audio Source
procedure TMainForm.AudioCapFiltersClick(Sender: TObject);
var
  PinList: TPinList;
  i, LineIndex: integer;
  ABool: LongBool;
begin
  CapEnum.SelectGUIDCategory(CLSID_AudioInputDeviceCategory);
  if AudioCapFilters.ItemIndex <> -1 then
  begin
    AudioSourceFilter.BaseFilter.Moniker := CapEnum.GetMoniker(AudioCapFilters.ItemIndex);
    AudioSourceFilter.FilterGraph := CaptureGraph;
    CaptureGraph.Active := true;
    PinList := TPinList.Create(AudioSourceFilter as IBaseFilter);
    AudioFormats.Clear;
    i := 0;
    while i < PinList.Count do
      if PinList.PinInfo[i].dir = PINDIR_OUTPUT then
        begin
          AudioMediaTypes.Assign(PinList.Items[i]);
          PinList.Delete(i);
        end else inc(i);

    for i := 0 to AudioMediaTypes.Count - 1 do
    begin
      AudioFormats.Items.Add(AudioMediaTypes.MediaDescription[i]);
    end;

    CaptureGraph.Active := false;
    InputLines.Clear;
    LineIndex := -1;
    for i := 0 to PinList.Count - 1 do
    begin
      InputLines.Items.Add(PinList.PinInfo[i].achName);
      with (PinList.Items[i] as IAMAudioInputMixer) do get_Enable(ABool);
      if ABool then LineIndex := i;
    end;
    InputLines.ItemIndex := LineIndex;
    PinList.Free;
    StartButton.Enabled := true;
  end;
end;

// Start Capture
procedure TMainForm.StartButtonClick(Sender: TObject);
var
  multiplexer: IBaseFilter;
  Writer: IFileSinkFilter;
  PinList: TPinList;
  i: integer;
begin

  // Activate the filter graph, at this stage the source filters are added to the graph
  CaptureGraph.Active := true;

  // configure output Audio media type + source
  if AudioSourceFilter.FilterGraph <> nil then
  begin
    PinList := TPinList.Create(AudioSourceFilter as IBaseFilter);
    i := 0;
    while i < PinList.Count do
      if PinList.PinInfo[i].dir = PINDIR_OUTPUT then
        begin
          if AudioFormats.ItemIndex <> -1 then
            with (PinList.Items[i] as IAMStreamConfig) do
              SetFormat(AudioMediaTypes.Items[AudioFormats.ItemIndex].AMMediaType^);
          PinList.Delete(i);
        end else inc(i);
    if InputLines.ItemIndex <> -1 then
      with (PinList.Items[InputLines.ItemIndex] as IAMAudioInputMixer) do
        put_Enable(true);
    PinList.Free;
  end;

  // configure output Video media type
  if VideoSourceFilter.FilterGraph <> nil then
  begin
    PinList := TPinList.Create(VideoSourceFilter as IBaseFilter);
    if VideoFormats.ItemIndex <> -1 then
      with (PinList.First as IAMStreamConfig) do
        SetFormat(VideoMediaTypes.Items[VideoFormats.ItemIndex].AMMediaType^);
    PinList.Free;
  end;


  // now render streams
  with CaptureGraph as IcaptureGraphBuilder2 do
  begin
    // set the output filename
    SetOutputFileName(MEDIASUBTYPE_Avi, PWideChar(CapFile), multiplexer, Writer);

    // Connect Video preview (VideoWindow)
    if VideoSourceFilter.BaseFilter.DataLength > 0 then
      RenderStream(@PIN_CATEGORY_PREVIEW, nil, VideoSourceFilter as IBaseFilter,
        nil , VideoWindow as IBaseFilter);

    // Connect Video capture streams
    if VideoSourceFilter.FilterGraph <> nil then
      RenderStream(@PIN_CATEGORY_CAPTURE, nil, VideoSourceFilter as IBaseFilter,
        nil, multiplexer as IBaseFilter);

    // Connect Audio capture streams
    if AudioSourceFilter.FilterGraph <> nil then
    begin

      RenderStream(nil, nil, AudioSourceFilter as IBaseFilter,
        nil, multiplexer as IBaseFilter);
    end;
  end;
  CaptureGraph.Play;
  StopButton.Enabled := true;
  StartButton.Enabled := false;
  AudioFormats.Enabled := false;
  AudioCapFilters.Enabled := false;
  VideoFormats.Enabled := false;
  VideoCapFilters.Enabled := false;
  Timer.Enabled := true;
end;

// Select the Ouput file
procedure TMainForm.CapFileButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    CapFile := SaveDialog.FileName;
    OutPutFileName.Caption := 'c:\capture.avi';
  end;
end;

// Stop Capture
procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  Timer.Enabled := false;
  StopButton.Enabled := false;
  StartButton.Enabled := true;
  CaptureGraph.Stop;
  CaptureGraph.Active := False;
  AudioFormats.Enabled := true;
  AudioCapFilters.Enabled := true;
  VideoFormats.Enabled := true;
  VideoCapFilters.Enabled := true;

end;

// Timer
procedure TMainForm.TimerTimer(Sender: TObject);
var
  position: int64;
  Hour, Min, Sec, MSec: Word;
const MiliSecInOneDay = 86400000;
begin
  if CaptureGraph.Active then
  begin
    with CaptureGraph as IMediaSeeking do
      GetCurrentPosition(position);
    DecodeTime(position div 10000 / MiliSecInOneDay, Hour, Min, Sec, MSec);
    StatusBar.SimpleText := Format('%d:%d:%d:%d',[Hour, Min, Sec, MSec]);
  end;

end;


end.
