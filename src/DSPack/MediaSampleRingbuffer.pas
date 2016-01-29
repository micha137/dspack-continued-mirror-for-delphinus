unit MediaSampleRingbuffer;

interface

uses
    BaseClass
  , DSPack
  , Winapi.DirectShow9
  , Winapi.Windows
  , System.Classes
  , Generics.Collections
  ;

const
  CLSID_MediaSampleRingbuffer: TGUID = '{D496495C-7079-4CC5-8464-8EE37684ADAE}';

type

  TMediaSampleRingbufferRendererInputPin = class(TBCRendererInputPin)
    function GetAllocatorRequirements(out pProps: TAllocatorProperties): HRESULT; override;
    function NotifyAllocator(pAllocator: IMemAllocator; bReadOnly: BOOL): HRESULT; override;
  end;

  TMediaSampleRingbufferComponent = class;

  TMediaSampleRingbuffer = class(TBCBaseRenderer, IAMFilterMiscFlags)
  protected
    function GetMiscFlags: ULONG; stdcall;
  public
    FMediaSampleRingbufferComponent: TMediaSampleRingbufferComponent;
    function DoRenderSample(MediaSample: IMediaSample): HResult;
      override;
    function CheckMediaType(MediaType: PAMMediaType): HResult;
      override;
    function SetMediaType(MediaType: PAMMediaType): HResult; override;
    // we need to adjust the allocator requirements, which is
    // not possible with the TBCRendererInputPin created
    // by TBCBaseRenderer.GetPin()
    function GetPin(n: integer): TBCBasePin; override;
  end;

  TCheckMediaTypeNotify = function(MediaType: PAMMediaType): HRESULT of object;

  // wrapper class for TThreadedQueueIMediaSampleHelper,
  // since helper classes don't go with generics
  TThreadedQueueIMediaSample = class(TThreadedQueue<IMediaSample>);

  TArrayIMediaSample = Array of IMediaSample;

  // http://stackoverflow.com/questions/9410485/how-do-i-use-class-helpers-to-access-strict-private-members-of-a-class
  TThreadedQueueIMediaSampleHelper = class helper for TThreadedQueueIMediaSample
    function GetQueue: TArrayIMediaSample;
    function GetQueueOffset: Integer;
  end;

  TMediaSampleRingbufferComponent = class(TComponent, DSPack.IFilter)
  protected
    FFilterGraph : TFilterGraph;
    FOnCheckMediaType: TCheckMediaTypeNotify;
    FBaseFilter: IBaseFilter;
    FRingbufferSize: Integer;
    FInFlightBuffers: Integer;

    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    procedure SetRingbufferSize(ARingbufferSize: Integer);
    procedure SetInFlightBuffers(AInFlightBuffers: Integer);

    // override TComponent.Notification
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // implement DSPack.IFilter
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0); virtual;
    // implement DSPack.IFilter
    function GetFilter: IBaseFilter;
    // implement DSPack.IFilter
    function GetName: string;
  public
    FRingBuffer: TThreadedQueueIMediaSample;
    FMediaType: TAMMediaType;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  published
    property FilterGraph: TFilterGraph
      read FFilterGraph write SetFilterGraph;
    property OnCheckMediaType: TCheckMediaTypeNotify read FOnCheckMediaType write FOnCheckMediaType;
    property RingbufferSize: Integer read FRingbufferSize write
    SetRingbufferSize;
    property InFlightBuffers: Integer read FInFlightBuffers write SetInFlightBuffers;
  end;

implementation

uses
    DXSUtil
  , SysUtils
  ;

function TMediaSampleRingbuffer.CheckMediaType(MediaType: PAMMediaType): HResult;
begin
  if Not Assigned(FMediaSampleRingbufferComponent) then Exit(E_FAIL);
  Result := S_OK;
  {
  Possible implementation of this callback:

  if Not IsEqualGUID(MediaType.majortype, MEDIATYPE_Video) then Exit(E_FAIL);
  if Not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_YUYV) then Exit(E_FAIL);
  Exit(S_OK);

  }
  if Assigned(FMediaSampleRingbufferComponent.OnCheckMediaType) then
    Result := FMediaSampleRingbufferComponent.OnCheckMediaType(MediaType);
end;

function TMediaSampleRingbuffer.DoRenderSample(
  MediaSample: IMediaSample): HResult;
var oldest: IMediaSample;
begin
  if Not Assigned(FMediaSampleRingbufferComponent) then Exit(E_FAIL);
  if FMediaSampleRingbufferComponent.FRingBuffer.QueueSize = FMediaSampleRingbufferComponent.FRingbufferSize then begin
    oldest := FMediaSampleRingbufferComponent.FRingBuffer.PopItem;
  end;
  FMediaSampleRingbufferComponent.FRingBuffer.PushItem(MediaSample);
  Result := S_OK;
end;

function TMediaSampleRingbuffer.GetMiscFlags: ULONG;
begin
  Result := AM_FILTER_MISC_FLAGS_IS_RENDERER;
end;

function TMediaSampleRingbuffer.GetPin(n: integer): TBCBasePin;
var
  hr: HResult;
begin
  FObjectCreationLock.Lock;
  try
    // Should only ever be called with zero
    Assert(n = 0);

    if (n <> 0) then
    begin
      Result := nil;
      Exit;
    end;

    // Create the input pin if not already done so

    if (FInputPin = nil) then
    begin
      // hr must be initialized to NOERROR because
      // CRendererInputPin's constructor only changes
      // hr's value if an error occurs.
      hr := NOERROR;

      FInputPin := TMediaSampleRingbufferRendererInputPin.Create(Self, hr, 'In');
      if (FInputPin = nil) then
      begin
        Result := nil;
        Exit;
      end;

      if Failed(hr) then
      begin
        FreeAndNil(FInputPin);
        Result := nil;
        Exit;
      end;
    end;

    Result := FInputPin;
  finally
    FObjectCreationLock.UnLock;
  end;
end;

function TMediaSampleRingbuffer.SetMediaType(MediaType: PAMMediaType): HResult;
begin
  FreeMediaType(@FMediaSampleRingbufferComponent.FMediaType);
  CopyMediaType(@FMediaSampleRingbufferComponent.FMediaType, MediaType);
  result := NOERROR;
end;

constructor TMediaSampleRingbufferComponent.Create(AOwner: TComponent);
begin
  inherited;
  InFlightBuffers := 10;
  RingbufferSize := 100;
end;

destructor TMediaSampleRingbufferComponent.Destroy;
begin
  FreeAndNil(FRingBuffer);
  FBaseFilter := nil;
  inherited;
end;

function TMediaSampleRingbufferComponent.GetFilter: IBaseFilter;
begin
  Result := FBaseFilter;
end;

function TMediaSampleRingbufferComponent.GetName: string;
begin
  Result := Name;
end;

procedure TMediaSampleRingbufferComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FFilterGraph) and (Operation = opRemove) then begin
    FFilterGraph.RemoveFilter(self);
    FFilterGraph := nil;
  end;
end;

procedure TMediaSampleRingbufferComponent.NotifyFilter(
  operation: TFilterOperation; Param: integer);
var hr: HRESULT; m: TMediaSampleRingbuffer;
begin
  case operation of
    foAdding:
      begin
        hr := S_OK;
        FBaseFilter :=
          TMediaSampleRingbuffer.Create(CLSID_MediaSampleRingbuffer, PChar(Name), nil, hr);
        CheckDSError(hr);
{$IFDEF COMPILER2006_UP}
        m := FBaseFilter as TMediaSampleRingbuffer;
{$ELSE}
        m := TMediaSampleRingbuffer(FBaseFilter);
{$ENDIF}
        m.FMediaSampleRingbufferComponent := self;
      end;
    foRemoving:
      begin
        if Assigned(FBaseFilter) then begin
          CheckDSError(FBaseFilter.Stop);
{$IFDEF COMPILER2006_UP}
          m := FBaseFilter as TMediaSampleRingbuffer;
{$ELSE}
          m := TMediaSampleRingbuffer(FBaseFilter);
{$ENDIF}
          if Assigned(m.FInputPin) then
            CheckDSError(m.FInputPin.Disconnect);
        end;
      end;
    foRemoved:
      begin
        FBaseFilter := nil;
      end;
  end;
end;

function TMediaSampleRingbufferComponent.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Failed(Result) and Assigned(FBaseFilter) then
    Result := FBaseFilter.QueryInterface(IID, Obj);
end;

procedure TMediaSampleRingbufferComponent.SetFilterGraph(
  AFilterGraph: TFilterGraph);
begin
  if AFilterGraph = FFilterGraph then exit;
  if FFilterGraph <> nil then begin
    FFilterGraph.RemoveFilter(self);
    FFilterGraph.RemoveFreeNotification(Self);
  end;
  if AFilterGraph <> nil then begin
    AFilterGraph.InsertFilter(self);
    // we want to be notified by a call to our Notification() when our FilterGraph is freed
    AFilterGraph.FreeNotification(Self);
  end;
  FFilterGraph := AFilterGraph;
end;

procedure TMediaSampleRingbufferComponent.SetInFlightBuffers(
  AInFlightBuffers: Integer);
begin
  if AInFlightBuffers <= 0 then raise Exception.Create('TMediaSampleRingbufferComponent.SetInFlightBuffers invalid');
  FInFlightBuffers := AInFlightBuffers;
end;

procedure TMediaSampleRingbufferComponent.SetRingbufferSize(
  ARingbufferSize: Integer);
begin
  if ARingbufferSize <= 0 then raise Exception.Create('TMediaSampleRingbufferComponent.SetRingbufferSize invalid');
  FRingbufferSize := ARingbufferSize;
  FreeAndNil(FRingBuffer);
  FRingBuffer := TThreadedQueueIMediaSample.Create(RingbufferSize);
end;

function TMediaSampleRingbufferRendererInputPin.GetAllocatorRequirements(
  out pProps: TAllocatorProperties): HRESULT;
var m: TMediaSampleRingbuffer;
begin
  m := FRenderer as TMediaSampleRingbuffer;
  pProps.cBuffers := m.FMediaSampleRingbufferComponent.FRingbufferSize +
    m.FMediaSampleRingbufferComponent.InFlightBuffers;
  Result := S_OK;
end;

function TMediaSampleRingbufferRendererInputPin.NotifyAllocator(
  pAllocator: IMemAllocator; bReadOnly: BOOL): HRESULT;
var p, actual: TAllocatorProperties;
var m: TMediaSampleRingbuffer;
begin
  m := FRenderer as TMediaSampleRingbuffer;
  Result := inherited;
  pAllocator.GetProperties(p);
  // if the allocator properties requested in
  // GetAllocatorRequirements() above were not honored
  // by the upstream filter, we set them here again
  if p.cBuffers < m.FMediaSampleRingbufferComponent.FRingbufferSize+1 then begin
    p.cBuffers := m.FMediaSampleRingbufferComponent.FRingbufferSize+1;
    Result := pAllocator.SetProperties(p, actual);
    if Failed(Result) then Exit;
    if actual.cBuffers < m.FMediaSampleRingbufferComponent.FRingbufferSize+1 then begin
      Result := E_FAIL;
      DebugBreak;
    end;
  end;
end;

function TThreadedQueueIMediaSampleHelper.GetQueue: TArrayIMediaSample;
begin
  Result := TArrayIMediaSample(FQueue);
end;

function TThreadedQueueIMediaSampleHelper.GetQueueOffset: Integer;
begin
  Result := FQueueOffset;
end;

end.

