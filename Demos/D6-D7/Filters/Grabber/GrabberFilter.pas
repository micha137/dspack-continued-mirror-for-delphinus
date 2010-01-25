//------------------------------------------------------------------------------
// File: Grabber.cpp
//
// Desc: DirectShow sample code - Implementation file for the SampleGrabber
//       example filter
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

// Delphi Conversion (C) Milenko Mitrovic (dcoder@dsp-worx.de)

unit GrabberFilter;

interface

uses
  BaseClass, DirectShow9, Windows, Classes, DSUtil, SysUtils,
  ActiveX;

//------------------------------------------------------------------------------
// Define new GUID and IID for the sample grabber example so that they do NOT
// conflict with the official DirectX SampleGrabber filter
//------------------------------------------------------------------------------
const
  CLSID_GrabberSample: TGUID = '{077AFE31-CAA9-40AA-B9E4-286D31A66FA9}';
  IID_IGrabberSample: TGUID = '{0C1B1F70-4735-42D6-8E10-DDC9617BA165}';

// We define a callback typedef for this example.
// Normally, you would make the SampleGrabber support a COM interface,
// and in one of its methods you would pass in a pointer to a COM interface
// used for calling back. See the DirectX documentation for the SampleGrabber
// for more information.

type
  TSAMPLECALLBACK = function (pSample: IMediaSample;
                              StartTime, StopTime: PReferenceTime;
                              TypeChanged: BOOL): HRESULT of Object;

// We define the interface the app can use to program us
  IGrabberSample = interface(IUnknown)
  ['{0C1B1F70-4735-42D6-8E10-DDC9617BA165}']
    function SetAcceptedMediaType(pType: PAMMediaType): HRESULT; stdcall;
    function GetConnectedMediaType(out pType: PAMMediaType): HRESULT; stdcall;
    function SetCallback(Callback: TSAMPLECALLBACK): HRESULT; stdcall;
    function SetDeliveryBuffer(props: TAllocatorProperties; pBuffer: PByte): HRESULT; stdcall;
  end;

  TBCSampleGrabberInPin = class;
  TBCSampleGrabber = class;

//----------------------------------------------------------------------------
// This is a special allocator that KNOWS that the person who is creating it
// will only create one of them. It allocates CMediaSamples that only
// reference the buffer location that is set in the pin's renderer's
// data variable
//----------------------------------------------------------------------------

  TBCSampleGrabberAllocator = class(TBCMemAllocator)
  protected
    // our pin who created us
    FPin: TBCSampleGrabberInPin;
  public
    constructor Create(Parent: TBCSampleGrabberInPin; out hr: HRESULT);
    destructor Destroy; override;
    function Alloc: HRESULT; override;
    procedure ReallyFree;
    // Override this to reject anything that does not match the actual buffer
    // that was created by the application
    function SetProperties(var Request: TAllocatorProperties; out Actual: TAllocatorProperties): HResult; override; stdcall;
  end;

//----------------------------------------------------------------------------
// we override the input pin class so we can provide a media type
// to speed up connection times. When you try to connect a filesourceasync
// to a transform filter, DirectShow will insert a splitter and then
// start trying codecs, both audio and video, video codecs first. If
// your sample grabber's set to connect to audio, unless we do this, it
// will try all the video codecs first. Connection times are sped up x10
// for audio with just this minor modification!
//----------------------------------------------------------------------------

  TBCSampleGrabberInPin = class(TBCTransInPlaceInputPin)
  private
    FPrivateAllocator: TBCSampleGrabberAllocator;
    FAllocprops: ALLOCATOR_PROPERTIES;
    FBuffer: PByte;
    FMediaTypeChanged: BOOL;
  protected
    function SampleGrabber: TBCSampleGrabber;
    function SetDeliveryBuffer(props: TAllocatorProperties; pBuffer: PByte): HRESULT;
  public
    constructor Create(pFilter: TBCTransInPlaceFilter; out hr: HRESULT);
    destructor Destroy; override;
    // override to provide major media type for fast connects
    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT; override;
    // override this or GetMediaType is never called
    function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT; override; stdcall;
    // override this to refuse any allocators besides
    // the one the user wants, if this is set
    function NotifyAllocator(pAllocator: IMemAllocator; bReadOnly: BOOL): HRESULT; override; stdcall;
    // override this so we always return the special allocator, if necessary
    function GetAllocator(out ppAllocator: IMemAllocator): HRESULT; override; stdcall;
    function SetMediaType(mt: PAMMediaType): HRESULT; override;
    // we override this to tell whoever's upstream of us what kind of
    // properties we're going to demand to have
    function GetAllocatorRequirements(out props: TAllocatorProperties): HRESULT; override; stdcall;
  end;

//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------

  TBCSampleGrabber = class(TBCTransInPlaceFilter, IGrabberSample, IPersist)
  protected
    m_mtAccept: TAMMediaType;
    m_callback: TSAMPLECALLBACK;
    m_Lock: TBCCritSec; // serialize access to our data
    function IsReadOnly: BOOL;
  public
    // PURE, override this to ensure we get
    // connected with the right media type
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    // PURE, override this to callback
    // the user when a sample is received
    function Transform(Sample: IMediaSample): HRESULT; override;
    // override this so we can return S_FALSE directly.
    // The base class CTransInPlace
    // Transform( ) method is called by it's
    // Receive( ) method. There is no way
    // to get Transform( ) to return an S_FALSE value
    // (which means "stop giving me data"),
    // to Receive( ) and get Receive( ) to return S_FALSE as well.
    function Receive(Sample: IMediaSample): HRESULT; override;
    constructor Create(unk: IUnKnown; out hr: HRESULT; ModifiesData: BOOL);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;
    // Expose ISampleGrabber
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    // IGrabberSample
    function SetAcceptedMediaType(pType: PAMMediaType): HRESULT; stdcall;
    function GetConnectedMediaType(out pType: PAMMediaType): HRESULT; stdcall;
    function SetCallback(Callback: TSAMPLECALLBACK): HRESULT; stdcall;
    function SetDeliveryBuffer(props: TAllocatorProperties; pBuffer: PByte): HRESULT; stdcall;
  end;

const
  SudPins : array[0..1] of TRegFilterPins =
  (
    (
      strName: 'Input';
      bRendered: FALSE;
      bOutput: FALSE;
      bZero: FALSE;
      bMany: FALSE;
      oFilter: @GUID_NULL;
      strConnectsToPin: '';
      nMediaTypes: 0;
      lpMediaType: nil
    ),
    (
      strName: 'Output';
      bRendered: FALSE;
      bOutput: TRUE;
      bZero: FALSE;
      bMany: FALSE;
      oFilter: @GUID_NULL;
      strConnectsToPin: '';
      nMediaTypes: 0;
      lpMediaType: nil
    )
  );

implementation

(*** TBCSampleGrabberAllocator ************************************************)

constructor TBCSampleGrabberAllocator.Create(Parent: TBCSampleGrabberInPin; out hr: HRESULT);
begin
  inherited Create('SampleGrabberAllocator', nil, hr);
  FPin := Parent;
end;

destructor TBCSampleGrabberAllocator.Destroy;
begin
  FBuffer := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------
// don't allocate the memory, just use the buffer the app provided
//----------------------------------------------------------------------------

function TBCSampleGrabberAllocator.Alloc: HRESULT;
var
  lAlignedSize: Integer;
  lRemainder: Integer;
  pNext: PByte;
  pSample: TBCMediaSample;
begin
  // look at the base class code to see where this came from!
  FAllocatorLock.Lock;
  try
    // Check he has called SetProperties
    Result :=  InheritedAlloc;
    if (FAILED(Result))
      then Exit;

    // If the requirements haven't changed then don't reallocate
    if (Result = S_FALSE) then
    begin
      ASSERT(FBuffer <> nil);
      Result := NOERROR;
      Exit;
    end;

    ASSERT(Result = S_OK); // we use this fact in the loop below

    // Free the old resources
    if Assigned(FBuffer)
      then ReallyFree();

    // Compute the aligned size
    lAlignedSize := FSize + FPrefix;
    if (FAlignment > 1) then
    begin
      lRemainder := lAlignedSize mod FAlignment;
      if (lRemainder <> 0)
        then inc(lAlignedSize, FAlignment - lRemainder);
    end;

    // Create the contiguous memory block for the samples
    // making sure it's properly aligned (64K should be enough!)
    ASSERT(lAlignedSize mod FAlignment = 0);

    // don't create the buffer - use what was passed to us
    FBuffer := FPin.FBuffer;

    if (FBuffer = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    pNext := FBuffer;

    ASSERT(FAllocated = 0);

    // Create the new samples - we have allocated m_lSize bytes for each sample
    // plus m_lPrefix bytes per sample as a prefix. We set the pointer to
    // the memory after the prefix - so that GetPointer() will return a pointer
    // to m_lSize bytes.
    while (FAllocated < FCount) do
    begin
      pSample := TBCMediaSample.Create('Sample Grabber memory media sample',
                                      Self,
                                      Result,
                                      @PByteArray(pNext)[FPrefix],  // GetPointer() value
                                      FSize);                       // not including prefix

      ASSERT(SUCCEEDED(Result));
      if (pSample = nil) then
      begin
        Result := E_OUTOFMEMORY;
        Exit;
      end;

      // This CANNOT fail
      FFree.Add(pSample);

      inc(FAllocated);
      inc(pNext, lAlignedSize);
    end;

    FChanged := FALSE;
    Result := NOERROR;
  finally
    FAllocatorLock.UnLock;
  end;
end;

//----------------------------------------------------------------------------
// don't really free the memory
//----------------------------------------------------------------------------

procedure TBCSampleGrabberAllocator.ReallyFree;
var
  pSample: TBCMediaSample;
begin
  // look at the base class code to see where this came from!
  // Should never be deleting this unless all buffers are freed

  ASSERT(FAllocated = FFree.GetCount);

  // Free up all the CMediaSamples

  while (true) do
  begin
    pSample := FFree.RemoveHead();
    if Assigned(pSample)
      then FreeAndNil(pSample)
      else break;
  end;

  FAllocated := 0;
  // don't free the buffer - let the app do itend;
end;

function TBCSampleGrabberAllocator.SetProperties(var Request: TAllocatorProperties; out Actual: TAllocatorProperties): HResult;
var
  pRequired: PAllocatorProperties;
begin
  Result := inherited SetProperties(Request, Actual);

  if (FAILED(Result))
    then Exit;

  pRequired := @FPin.FAllocprops;
  if (Request.cbAlign <> pRequired.cbAlign) then
  begin
    Result := VFW_E_BADALIGN;
    Exit;
  end;

  if (Request.cbPrefix <> pRequired.cbPrefix) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if (Request.cbBuffer > pRequired.cbBuffer) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if (Request.cBuffers > pRequired.cBuffers) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  Actual := pRequired^;

  FCount := pRequired.cBuffers;
  FSize := pRequired.cbBuffer;
  FAlignment := pRequired.cbAlign;
  FPrefix := pRequired.cbPrefix;

  Result := S_OK;
end;

(*** TBCSampleGrabberInPin ****************************************************)

function TBCSampleGrabberInPin.SampleGrabber: TBCSampleGrabber;
begin
  Result := TBCSampleGrabber(FFilter);
end;

//----------------------------------------------------------------------------
// inform the input pin of the allocator buffer we wish to use. See the
// input pin's SetDeliverBuffer method for comments. 
//----------------------------------------------------------------------------

function TBCSampleGrabberInPin.SetDeliveryBuffer(props: TAllocatorProperties; pBuffer: PByte): HRESULT;
begin
  // don't allow more than one buffer

  if (props.cBuffers <> 1) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if not Assigned(pBuffer) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FAllocprops := props;
  FBuffer := pBuffer;

  // If there is an existing allocator, make sure that it is released
  // to prevent a memory leak
  if Assigned(FPrivateAllocator)
    then FPrivateAllocator := nil;

  Result := S_OK;

  FPrivateAllocator := TBCSampleGrabberAllocator.Create(Self, Result);
  if not Assigned(FPrivateAllocator) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  FPrivateAllocator._AddRef; // DCODER: really needed ?
end;

constructor TBCSampleGrabberInPin.Create(pFilter: TBCTransInPlaceFilter; out hr: HRESULT);
begin
  inherited Create('SampleGrabberInputPin', pFilter, hr, 'Input');
  FPrivateAllocator := nil;
  FBuffer := nil;
  FMediaTypeChanged := False;
  FillChar(FAllocprops, sizeof(FAllocprops), 0);
end;

destructor TBCSampleGrabberInPin.Destroy;
begin
  if Assigned(FPrivateAllocator)
    then FreeAndnil(FPrivateAllocator);
end;

//----------------------------------------------------------------------------
// used to help speed input pin connection times. We return a partially
// specified media type - only the main type is specified. If we return
// anything BUT a major type, some codecs written improperly will crash
//----------------------------------------------------------------------------

function TBCSampleGrabberInPin.GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT;
begin
  if not Assigned(@MediaType) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if (Position < 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if (Position > 0) then
  begin
    Result := VFW_S_NO_MORE_ITEMS;
    Exit;
  end;

  // DCODER: unsure about that. the correct Translation would be:
  // MediaType.majortype := TBCSampleGrabber(FFilter).m_mtAccept.majortype;
  // However, it´s possible that MediaType = NULL
  if not Assigned(MediaType)
    then MediaType := CreateMediaType(@TBCSampleGrabber(FFilter).m_mtAccept)
    else MediaType.majortype := TBCSampleGrabber(FFilter).m_mtAccept.majortype;

  Result := S_OK;
end;

//----------------------------------------------------------------------------
// override the CTransInPlaceInputPin's method, and return a new enumerator
// if the input pin is disconnected. This will allow GetMediaType to be
// called. If we didn't do this, EnumMediaTypes returns a failure code
// and GetMediaType is never called. 
//----------------------------------------------------------------------------

function TBCSampleGrabberInPin.EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT;
begin
  if not Assigned(@ppEnum) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // if the output pin isn't connected yet, offer the possibly
  // partially specified media type that has been set by the user
  if( not TBCSampleGrabber(FTIPFilter).OutputPin.IsConnected) then
  begin
    // Create a new reference counted enumerator
    ppEnum := TBCEnumMediaTypes.Create(Self, nil);
    if Assigned(ppEnum)
      then Result := NOERROR
      else Result := E_OUTOFMEMORY;
    Exit;
  end;

  // if the output pin is connected, offer it's fully qualified media type
  Result := TBCSampleGrabber(FTIPFilter).OutputPin.GetConnected.EnumMediaTypes(ppEnum);
end;

function TBCSampleGrabberInPin.NotifyAllocator(pAllocator: IMemAllocator; bReadOnly: BOOL): HRESULT;
begin
  if Assigned(FPrivateAllocator) then
  begin
    if (Pointer(pAllocator) <> Pointer(FPrivateAllocator)) then
    begin
      Result := E_FAIL;
      Exit;
    end else
    begin
      // if the upstream guy wants to be read only and we don't, then that's bad
      // if the upstream guy doesn't request read only, but we do, that's okay
      if (bReadOnly and not SampleGrabber.IsReadOnly) then
      begin
        Result := E_FAIL;
        Exit;
      end;
    end;
  end;

  Result := inherited NotifyAllocator(pAllocator, bReadOnly);
end;

function TBCSampleGrabberInPin.GetAllocator(out ppAllocator: IMemAllocator): HRESULT;
begin
  if Assigned(FPrivateAllocator) then
  begin
    if not Assigned(@ppAllocator) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    Result := FPrivateAllocator.QueryInterface(IID_IMemAllocator, ppAllocator);;
    // m_pPrivateAllocator->AddRef( );
    Exit;
  end else
  begin
    Result := inherited GetAllocator(ppAllocator);
  end;
end;

function TBCSampleGrabberInPin.SetMediaType(mt: PAMMediaType): HRESULT;
begin
  FMediaTypeChanged := TRUE;
  Result := inherited SetMediaType(mt);
end;

function TBCSampleGrabberInPin.GetAllocatorRequirements(out props: TAllocatorProperties): HRESULT;
begin
  if not Assigned(@props) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if Assigned(FPrivateAllocator) then
  begin
    props := FAllocprops;
    Result := S_OK;
  end else
  begin
    Result := inherited GetAllocatorRequirements(props);
  end;
end;

(*** TBCSampleGrabber *********************************************************)

constructor TBCSampleGrabber.Create(unk: IUnKnown; out hr: HRESULT; ModifiesData: BOOL);
begin
  inherited Create('SampleGrabber', unk, CLSID_GrabberSample, hr, ModifiesData);
  FLock := TBCCritSec.Create;
  m_callback := nil;

  // this is used to override the input pin with our own
  FInput := TBCSampleGrabberInPin.Create(Self, hr);
  if not Assigned(FInput)
    then hr := E_OUTOFMEMORY;

  // Ensure that the output pin gets created.  This is necessary because our
  // SetDeliveryBuffer() method assumes that the input/output pins are created, but
  // the output pin isn't created until GetPin() is called.  The
  // CTransInPlaceFilter::GetPin() method will create the output pin, since we
  // have not already created one.
  FOutput := TBCTransInPlaceOutputPin.Create('TransInPlace output pin',
      self,      // Owner filter
      hr,        // Result code
      'Output');
end;

constructor TBCSampleGrabber.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Controller, hr, False);
end;

function TBCSampleGrabber.IsReadOnly: BOOL;
begin
  Result := not FModifiesData;
end;

destructor TBCSampleGrabber.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

//----------------------------------------------------------------------------
// This is where you force the sample grabber to connect with one type
// or the other. What you do here is crucial to what type of data your
// app will be dealing with in the sample grabber's callback. For instance,
// if you don't enforce right-side-up video in this call, you may not get
// right-side-up video in your callback. It all depends on what you do here.
//----------------------------------------------------------------------------

function TBCSampleGrabber.CheckInputType(mtIn: PAMMediaType): HRESULT;
var
  g: TGuid;
begin
  if not Assigned(@mtIn) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    // if the major type is not set, then accept anything


    g := m_mtAccept.majortype;
    if IsEqualGUID(g, GUID_NULL) then
    begin
      Result := NOERROR;
      Exit;
    end;

    // if the major type is set, don't accept anything else
    if not IsEqualGUID(g, mtIn.majortype) then
    begin
      Result := VFW_E_INVALID_MEDIA_TYPE;
      Exit;
    end;

    // subtypes must match, if set. if not set, accept anything
    g := m_mtAccept.subtype;
    if IsEqualGUID(g, GUID_NULL) then
    begin
      Result := NOERROR;
      Exit;
    end;

    if not IsEqualGUID(g, mtIn.subtype) then
    begin
      Result := VFW_E_INVALID_MEDIA_TYPE;
      Exit;
    end;

    // format types must match, if one is set
    g := m_mtAccept.formattype;
    if IsEqualGUID(g, GUID_NULL) then
    begin
      Result := NOERROR;
      Exit;
    end;

    if not IsEqualGUID(g, mtIn.formattype) then
    begin
      Result := VFW_E_INVALID_MEDIA_TYPE;
      Exit;
    end;

    // at this point, for this sample code, this is good enough,
    // but you may want to make it more strict
    Result := NOERROR;
  finally
    FLock.UnLock;
  end;
end;

function TBCSampleGrabber.Transform(Sample: IMediaSample): HRESULT;
var
  StartTime, StopTime: TReferenceTime;
  pTypeChanged: PBOOL;
begin
  if not Assigned(Sample) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FLock.Lock;
  try
    if Assigned(m_callback) then
    begin
      Sample.GetTime(StartTime, StopTime);

      StartTime := StartTime + FInput.CurrentStartTime;
      StopTime  := StopTime  + FInput.CurrentStartTime;

      pTypeChanged := @TBCSampleGrabberInPin(FInput).FMediaTypeChanged;

      Result := m_callback(Sample, @StartTime, @StopTime, pTypeChanged^);

      pTypeChanged^ := FALSE; // now that we notified user, we can clear it

      Exit;
    end;

    Result := NOERROR;
  finally
    FLock.UnLock;
  end;
end;

//----------------------------------------------------------------------------
// This bit is almost straight out of the base classes.
// We override this so we can handle Transform( )'s error
// result differently.
//----------------------------------------------------------------------------

function TBCSampleGrabber.Receive(Sample: IMediaSample): HRESULT;
var
  props: PAMSample2Properties;
begin
  if not Assigned(Sample) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  props := FInput.SampleProps;

  if (props.dwStreamId <> AM_STREAM_MEDIA) then
  begin
    if(FOutput.IsConnected)
      then Result := FOutput.Deliver(Sample)
      else Result := NOERROR;
    Exit;
  end;

  if (UsingDifferentAllocators) then
  begin
    // We have to copy the data.
    Sample := Copy(Sample);
    if (Sample = nil) then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;
  end;

  // have the derived class transform the data
  Result := Transform(Sample);

  if (FAILED(Result)) then
  begin
    if (UsingDifferentAllocators())
      then Sample := nil;
    Exit;
  end;

  if (Result = NOERROR)
    then Result := FOutput.Deliver(Sample);

  // release the output buffer. If the connected pin still needs it,
  // it will have addrefed it itself.
  if (UsingDifferentAllocators())
    then Sample := nil;
end;

function TBCSampleGrabber.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if not Assigned(@Obj) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if IsEqualGUID(IID, IID_IGrabberSample) then
  begin
    if (GetInterface(IID, Obj))
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
    Result := inherited NonDelegatingQueryInterface(IID, Obj);
end;

function TBCSampleGrabber.SetAcceptedMediaType(pType: PAMMediaType): HRESULT;
begin
  FLock.Lock;
  try
    if not Assigned(pType) then
    begin
      Result := NOERROR;
      Exit;
    end;

    CopyMediaType(@m_mtAccept, pType);
    Result := S_OK;
  finally
    FLock.UnLock;
  end;
end;

function TBCSampleGrabber.GetConnectedMediaType(out pType: PAMMediaType): HRESULT;
begin
  if (not Assigned(FInput) or not FInput.IsConnected)
    then Result := VFW_E_NOT_CONNECTED
    else Result := FInput.ConnectionMediaType(pType^);
end;

function TBCSampleGrabber.SetCallback(Callback: TSAMPLECALLBACK): HRESULT;
begin
  FLock.Lock;
  try
    m_callback := Callback;
    Result := NOERROR;
  finally
    FLock.UnLock;
  end;
end;

//----------------------------------------------------------------------------
// inform the input pin of the allocator buffer we wish to use. See the
// input pin's SetDeliverBuffer method for comments. 
//----------------------------------------------------------------------------

function TBCSampleGrabber.SetDeliveryBuffer(props: TAllocatorProperties; pBuffer: PByte): HRESULT;
begin
  // have the input/output pins been created?
  if not Assigned(FInput) or not Assigned(FOutput) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // they can't be connected if we're going to be changing delivery buffers
  if (InputPin.IsConnected or OutputPin.IsConnected) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := TBCSampleGrabberInPin(FInput).SetDeliveryBuffer(props, pBuffer);
end;

initialization
  TBCClassFactory.CreateFilter(TBCSampleGrabber, StringToOleStr('_SampleGrabber Example'), CLSID_GrabberSample,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE +1, 2, @SudPins);


end.
