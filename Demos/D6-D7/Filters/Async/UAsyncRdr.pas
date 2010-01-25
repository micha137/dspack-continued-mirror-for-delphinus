//------------------------------------------------------------------------------
// File: UAsyncRdr.pas
// Original files: asyncrdr.h, asyncrdr.c
//
// Desc: Defines an IO source filter.
//
// This filter (CAsyncReader) supports IBaseFilter and IFileSourceFilter interfaces from the
// filter object itself. It has a single output pin (CAsyncOutputPin)
// which supports IPin and IAsyncReader.
//
// This filter is essentially a wrapper for the CAsyncFile class that does
// all the work.
//
//
// Portions created by Microsoft are
// Copyright (c) 2000-2002  Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
unit UAsyncRdr;

interface
uses
  BaseClass, DirectShow9, DSUtil, ActiveX, Windows, SysUtils,
  UAsyncIo;

const
  CLSID_AsyncSample: TGUID = '{10F9E8F1-FE54-470C-9F1A-FE587DF7BD0B}';

type
  // the filter class (defined below)
  TBCAsyncReader = class;

  // the output pin class
  TBCAsyncOutputPin = class(TBCBasePin, IAsyncReader)
  protected
    FReader: TBCAsyncReader;
    FIo: TBCAsyncIo;

    //  This is set every time we're asked to return an IAsyncReader
    //  interface
    //  This allows us to know if the downstream pin can use
    //  this transport, otherwise we can hook up to thinks like the
    //  dump filter and nothing happens
    FQueriedForAsyncReader: Boolean;

    function InitAllocator(out AAlloc: IMemAllocator): HRESULT; virtual;

  public
    // constructor and destructor
    constructor Create(out hr: HResult; Reader: TBCAsyncReader;
      IO: TBCAsyncIo; Lock: TBCCritSec);

    destructor Destroy; override;

    // --- CUnknown ---

    // need to expose IAsyncReader
    function NonDelegatingQueryInterface(const IID: TGUID;
      out Obj): HResult; override; stdcall;

    // --- IPin methods ---
    function Connect(AReceivePin: IPin;
      const pmt: PAMMediaType): HResult; reintroduce; stdcall;

    // --- CBasePin methods ---

    // return the types we prefer - this will return the known
    // file type
    function GetMediaType(Position: Integer;
      out MediaType: PAMMediaType): HResult; override;

    // can we support this type?
    function CheckMediaType(AType: PAMMediaType): HResult; override;

    // Clear the flag so we see if IAsyncReader is queried for
    function CheckConnect(Pin: IPin): HResult; override;

    // See if it was asked for
    function CompleteConnect(ReceivePin: IPin): HResult; override;

    //  Remove our connection status
    function BreakConnect: HResult; override;

    // --- IAsyncReader methods ---
    // pass in your preferred allocator and your preferred properties.
    // method returns the actual allocator to be used. Call GetProperties
    // on returned allocator to learn alignment and prefix etc chosen.
    // this allocator will be not be committed and decommitted by
    // the async reader, only by the consumer.
    function RequestAllocator(APreferred: IMemAllocator;
        AProps: PAllocatorProperties;
        out AActual: IMemAllocator): HResult; stdcall;

    // queue a request for data.
    // media sample start and stop times contain the requested absolute
    // byte position (start inclusive, stop exclusive).
    // may fail if sample not obtained from agreed allocator.
    // may fail if start/stop position does not match agreed alignment.
    // samples allocated from source pin's allocator may fail
    // GetPointer until after returning from WaitForNext.
    function Request(ASample: IMediaSample; AUser: DWord): HResult; stdcall;

    // block until the next sample is completed or the timeout occurs.
    // timeout (millisecs) may be 0 or INFINITE. Samples may not
    // be delivered in order. If there is a read error of any sort, a
    // notification will already have been sent by the source filter,
    // and STDMETHODIMP will be an error.
    function WaitForNext(ATimeout: DWord; out ASample: IMediaSample;
        out AUser: DWord): HResult; stdcall;

    // sync read of data. Sample passed in must have been acquired from
    // the agreed allocator. Start and stop position must be aligned.
    // equivalent to a Request/WaitForNext pair, but may avoid the
    // need for a thread on the source filter.
    function SyncReadAligned(ASample: IMediaSample): HResult; stdcall;


    // sync read. works in stopped state as well as run state.
    // need not be aligned. Will fail if read is beyond actual total
    // length.
    function SyncRead(APosition: int64; ALength: Longint;
      ABuffer: PByte): HResult; stdcall;

    // return total length of stream, and currently available length.
    // reads for beyond the available length but within the total length will
    // normally succeed but may block for a long period.
    function Length(out ATotal, AAvailable: int64): HResult; stdcall;

    // cause all outstanding reads to return, possibly with a failure code
    // (VFW_E_TIMEOUT) indicating they were cancelled.
    // these are defined on IAsyncReader and IPin
    function BeginFlush: HResult; override;
    function EndFlush: HResult; override;
  end;

  //
  // The filter object itself. Supports IBaseFilter through
  // CBaseFilter and also IFileSourceFilter directly in this object

  TBCAsyncReader = class(TBCBaseFilter)
  protected
    // filter-wide lock
    FCSFilter: TBCCritSec;

    // all i/o done here
    FIO: TBCAsyncIo;

    // our output pin
    FOutputPin: TBCAsyncOutputPin;

    // Type we think our data is
    Fmt: TAMMediaType;

  public

    // construction / destruction
    constructor Create(Name: String; Unk: IUnknown;
      Stream: TBCAsyncStream; out hr: HResult);

    destructor Destroy; override;


    // --- CBaseFilter methods ---
    function GetPinCount: Integer; override;
    function GetPin(n: Integer): TBCBasePin; override;

    // --- Access our media type
    function LoadType: PAMMediaType; virtual;

    function Connect(pReceivePin: IPin;
      const pmt: PAMMediaType): HRESULT; virtual;
  end;


implementation

// --- TBCCAsyncReader ---

constructor TBCAsyncReader.Create(Name: String; Unk: IUnknown;
  Stream: TBCAsyncStream; out hr: HResult);
begin
  FCSFilter := TBCCritSec.Create;
  ZeroMemory(@Fmt, SizeOf(TAMMediaType));

  Inherited Create(Name, Unk, FCSFilter, CLSID_AsyncSample, hr);
  FIO := TBCAsyncIo.Create(Stream);
  FOutputPin := TBCAsyncOutputPin.Create(hr, Self, FIO, FCSFilter);
end;

destructor TBCAsyncReader.Destroy;
begin
  Inherited;

  // FCSFilter is destroyed by parent class
end;

function TBCAsyncReader.GetPinCount: Integer;
begin
  Result := 1;
end;

function TBCAsyncReader.GetPin(n: Integer): TBCBasePin;
begin
  if ((GetPinCount > 0) and (n = 0)) then
    Result := FOutputPin
  else
    Result := nil;
end;

function TBCAsyncReader.LoadType: PAMMediaType;
begin
  Result := @Fmt;
end;

function TBCAsyncReader.Connect(pReceivePin: IPin;
  const pmt: PAMMediaType): HRESULT;
begin
  Result := FOutputPin.Connect(pReceivePin, pmt);
end;

// --- TBCAsyncOutputPin ---

constructor TBCAsyncOutputPin.Create(out hr: HResult; Reader: TBCAsyncReader;
  IO: TBCAsyncIo; Lock: TBCCritSec);
begin
  Inherited Create('Async output pin', Reader, Lock, hr, 'Output',
    PINDIR_OUTPUT);
  FReader := Reader;
  FIO := IO;
end;

destructor TBCAsyncOutputPin.Destroy;
begin
  Inherited;
end;

function TBCAsyncOutputPin.InitAllocator(out AAlloc: IMemAllocator): HRESULT;
begin
  // Create a default memory allocator
  Result := CreateMemoryAllocator(AAlloc);
  if Failed(Result) then
    Exit;

  if (AAlloc = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
end;

function TBCAsyncOutputPin.NonDelegatingQueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IID_IAsyncReader) then
  begin
    FQueriedForAsyncReader := True;
    if GetInterface(IID_IAsyncReader, Obj) then
      Result := S_OK
    else
      Result := E_FAIL;
  end
    else
      Result := Inherited NonDelegatingQueryInterface(IID, Obj);
end;

function TBCAsyncOutputPin.Connect(AReceivePin: IPin;
  const pmt: PAMMediaType): HResult;
begin
  Result := FReader.Connect(AReceivePin, pmt);
end;

function TBCAsyncOutputPin.GetMediaType(Position: Integer;
  out MediaType: PAMMediaType): HResult;
begin
  if (Position < 0) then
    Result := E_INVALIDARG
  else
    if (Position > 0) then
      Result := VFW_S_NO_MORE_ITEMS
    else
      begin
        if (FReader = nil) then
        begin
          Result := E_UNEXPECTED;
          Exit;
        end;

        CopyMemory(MediaType, FReader.LoadType, SizeOf(TAMMediaType));
        Result := S_OK;
      end;
end;

function TBCAsyncOutputPin.CheckMediaType(AType: PAMMediaType): HResult;
begin
  FLock.Lock;
  try
    // We treat MEDIASUBTYPE_NULL subtype as a wild card
    with FReader do
    if (IsEqualGUID(LoadType.majortype, AType.majortype) and
      (IsEqualGUID(LoadType.subtype, MEDIASUBTYPE_NULL) or
      IsEqualGUID(LoadType.subtype, AType.subtype))) then
      Result := S_OK
    else
      Result := S_FALSE;
  finally
    FLock.Unlock;
  end;
end;

function TBCAsyncOutputPin.CheckConnect(Pin: IPin): HResult;
begin
  FQueriedForAsyncReader := False;
  Result := Inherited CheckConnect(Pin);
end;

function TBCAsyncOutputPin.CompleteConnect(ReceivePin: IPin): HResult;
begin
  if FQueriedForAsyncReader then
    Result := inherited CompleteConnect(ReceivePin)
  else
    {$IFDEF VFW_E_NO_TRANSPORT}
    Result := VFW_E_NO_TRANSPORT;
    {$ELSE}
    Result := E_FAIL;
    {$ENDIF}
end;

function TBCAsyncOutputPin.BreakConnect: HResult;
begin
  FQueriedForAsyncReader := False;
  Result := Inherited BreakConnect;
end;

function TBCAsyncOutputPin.RequestAllocator(APreferred: IMemAllocator;
  AProps: PAllocatorProperties; out AActual: IMemAllocator): HResult;
var
  Actual: TAllocatorProperties;
  Alloc: IMemAllocator;
begin
  // we need to return an addrefed allocator, even if it is the preferred
  // one, since he doesn't know whether it is the preferred one or not.
  Assert(Assigned(FIO));

  // we care about alignment but nothing else
  if ((Not Boolean(AProps.cbAlign)) or
    (Not FIo.IsAligned(AProps.cbAlign))) then
    AProps.cbAlign := FIo.Alignment;

  if Assigned(APreferred) then
  begin
    Result := APreferred.SetProperties(AProps^, Actual);

    if (Succeeded(Result) and FIo.IsAligned(Actual.cbAlign)) then
    begin
        AActual := APreferred;
        Result := S_OK;
        Exit;
    end;
  end;

  // create our own allocator
  Result := InitAllocator(Alloc);
  if Failed(Result) then
    Exit;

  //...and see if we can make it suitable
  Result := Alloc.SetProperties(AProps^, Actual);
  if (Succeeded(Result) and FIo.IsAligned(Actual.cbAlign)) then
  begin
    // we need to release our refcount on pAlloc, and addref
    // it to pass a refcount to the caller - this is a net nothing.
    AActual := Alloc;
    Result := S_OK;
    Exit;
  end;

  // failed to find a suitable allocator
  Alloc._Release;

  // if we failed because of the IsAligned test, the error code will
  // not be failure
  if Succeeded(Result) then
    Result := VFW_E_BADALIGN;
end;

function TBCAsyncOutputPin.Request(ASample: IMediaSample; AUser: DWord): HResult;
var
  Start, Stop: TReferenceTime;
  Pos, Total, Available: LONGLONG;
  Length, Align: Integer;
  Buffer: PByte;
begin
  // queue an aligned read request. call WaitForNext to get
  // completion.

  Result := ASample.GetTime(Start, Stop);
  if Failed(Result) then
    Exit;

  Pos := Start div UNITS;
  Length := (Stop - Start) div UNITS;

  Total := 0;
  Available := 0;

  FIO.Length(Total, Available);
  if (Pos + Length > Total) then
  begin
    // the end needs to be aligned, but may have been aligned
    // on a coarser alignment.
    FIo.Alignment(Align);

    Total := (Total + Align -1) and (Not (Align-1));

    if (Pos + Length > Total) then
    begin
      Length := Total - Pos;

      // must be reducing this!
      Assert((Total * UNITS) <= Stop);
      Stop := Total * UNITS;
      ASample.SetTime(@Start, @Stop);
    end;
  end;

  Result := ASample.GetPointer(Buffer);
  if Failed(Result) then
    Exit;

  Result := FIO.Request(Pos, Length, True, Buffer, Pointer(ASample), AUser);
end;

function TBCAsyncOutputPin.WaitForNext(ATimeout: DWord; out ASample: IMediaSample;
  out AUser: DWord): HResult;
var
  Actual: Integer;
  Sample: IMediaSample;
begin
  Sample := nil;
  Result := FIo.WaitForNext(ATimeout, @Sample, AUser, Actual);
  if Succeeded(Result) then
    Sample.SetActualDataLength(Actual);
  ASample := Sample;
end;

function TBCAsyncOutputPin.SyncReadAligned(ASample: IMediaSample): HResult;
var
  Start, Stop: TReferenceTime;
  Pos, Total, Available: LONGLONG;
  Length, Align, Actual: Integer;
  Buffer: PByte;
begin
  Result := ASample.GetTime(Start, Stop);
  if Failed(Result) then
    Exit;

  Pos := Start div UNITS;
  Length := (Stop - Start) div UNITS;

  FIo.Length(Total, Available);
  if (Pos + Length > Total) then
  begin
    // the end needs to be aligned, but may have been aligned
    // on a coarser alignment.
    FIo.Alignment(Align);

    Total := (Total + Align - 1) and (Not (Align - 1));

    if (Pos + Length > Total) then
    begin
        Length := Total - Pos;

        // must be reducing this!
        Assert((Total * UNITS) <= Stop);
        Stop := Total * UNITS;
        ASample.SetTime(@Start, @Stop);
    end;
  end;

  Result := ASample.GetPointer(Buffer);
  if Failed(Result) then
    Exit;

  Result := FIO.SyncReadAligned(Pos, Length, Buffer, Actual, Pointer(ASample));
  ASample.SetActualDataLength(Actual);
end;

function TBCAsyncOutputPin.SyncRead(APosition: Int64; ALength: Longint;
  ABuffer: Pbyte): HResult;
begin
  Result := FIO.SyncRead(APosition, ALength, ABuffer);
end;

function TBCAsyncOutputPin.Length(out ATotal, AAvailable: int64): HResult;
begin
  Result := FIO.Length(ATotal, AAvailable);
end;

function TBCAsyncOutputPin.BeginFlush: HResult;
begin
  Result := FIO.BeginFlush;
end;

function TBCAsyncOutputPin.EndFlush: HResult;
begin
  Result := FIO.EndFlush;
end;

end.
