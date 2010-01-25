unit main;
{$DEFINE DEBUG}
{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
interface
uses BaseClass, ActiveX, DirectShow9, Windows, DSUtil;

const

  CLSID_WavDest : TGUID = '{3C78B8E2-6C4D-11d1-ADE2-0000F8754B99}';

type

  TWavDestOutputPin = class(TBCTransformOutputPin)
  public
    constructor Create(Filter: TBCTransformFilter; out hr: HRESULT);
    function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT; override; stdcall;
    function CheckMediaType(pmt: PAMMediaType): HRESULT; override;
  end;

  TWavDestFilter = class(TBCTransformFilter)
  private
    FWavData: Cardinal;
    FHeader : Cardinal;
  public
    constructor Create(Unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;

    function Copy(Source, dest: IMediaSample): HRESULT;

    function Transform(pIn, pOut: IMediaSample): HRESULT; overload; override;
    function Receive(Sample: IMediaSample): HRESULT; override;

    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function CheckTransform(mtIn, mtOut: PAMMediaType): HRESULT; override;
    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT; override;

    function DecideBufferSize(Alloc: IMemAllocator; Properties: PAllocatorProperties): HRESULT; override;

    function StartStreaming: HRESULT; override;
    function StopStreaming: HRESULT; override;

    function CompleteConnect(direction: TPinDirection; ReceivePin: IPin): HRESULT; override;
  end;


implementation


{ TWavDestOutputPin }

function TWavDestOutputPin.CheckMediaType(pmt: PAMMediaType): HRESULT;
begin
  if IsEqualGUID(pmt.majortype, MEDIATYPE_Stream) and IsEqualGUID(pmt.subtype, MEDIASUBTYPE_WAVE) then
       result := S_OK
  else result := S_FALSE;
end;

constructor TWavDestOutputPin.Create(Filter: TBCTransformFilter;
  out hr: HRESULT);
begin
  inherited Create('WavDest output pin', Filter, hr, 'Out');
end;

function TWavDestOutputPin.EnumMediaTypes(
  out ppEnum: IEnumMediaTypes): HRESULT;
begin
  result := inherited EnumMediaTypes(ppEnum);
end;

{ TWavDestFilter }

function TWavDestFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  if IsEqualGUID(mtIn.formattype, FORMAT_WaveFormatEx) then
       result := S_OK
  else result := S_FALSE;
end;

function TWavDestFilter.CheckTransform(mtIn,
  mtOut: PAMMediaType): HRESULT;
begin
  result := CheckInputType(mtIn);
  if FAILED(result) then exit;
  result := NOERROR;
end;

function TWavDestFilter.CompleteConnect(direction: TPinDirection;
  ReceivePin: IPin): HRESULT;
begin
  result := S_OK;
end;

function TWavDestFilter.Copy(Source, dest: IMediaSample): HRESULT;
var
  SourceBuffer, DestBuffer: PBYTE;
  SourceSize: LongInt;
  TimeStart, TimeEnd: TReferenceTime;
  MediaStart, MediaEnd: int64;
  MediaType: PAMMediaType;
  DataLength: Integer;
begin
  // Copy the sample data
  SourceSize := Source.GetActualDataLength;

{$ifdef DEBUG}
  ASSERT(Dest.GetSize >= SourceSize);
{$endif}

  Source.GetPointer(SourceBuffer);
  Dest.GetPointer(DestBuffer);

  CopyMemory(DestBuffer, SourceBuffer, SourceSize);

  // Copy the sample times

  if (NOERROR = Source.GetTime(TimeStart, TimeEnd)) then
    Dest.SetTime(@TimeStart, @TimeEnd);

  if (Source.GetMediaTime(MediaStart,MediaEnd) = NOERROR) then
    Dest.SetMediaTime(@MediaStart, @MediaEnd);

  // Copy the media type
  Source.GetMediaType(MediaType);
  Dest.SetMediaType(MediaType^);
  DeleteMediaType(MediaType);

  // Copy the actual data length
  DataLength := Source.GetActualDataLength;
  Dest.SetActualDataLength(DataLength);
  result := NOERROR;
end;

constructor TWavDestFilter.Create(Unk: IInterface; out hr: HRESULT);
var
  pOut: TWavDestOutputPin;
  pIn : TBCTransformInputPin;
begin
  inherited Create('WavDest filter', Unk, CLSID_WavDest);
  ASSERT(FOutput = nil);
  if SUCCEEDED(hr) then
  begin
      // Create an output pin so we can have control over the connection
      // media type.
      pOut := TWavDestOutputPin.Create(self, hr);
      if(pOut <> nil) then
        begin
          if SUCCEEDED(hr) then
               FOutput := pOut
          else pOut.Free;
        end
      else
        hr := E_OUTOFMEMORY;
      //
      // NOTE!: If we've created our own output pin we must also create
      // the input pin ourselves because the CTransformFilter base class
      // will create an extra output pin if the input pin wasn't created.
      //
      pIn := TBCTransformInputPin.Create('Transform input pin',
                                          self,  // Owner filter
                                          hr,    // Result code
                                          'In'); // Pin name
      // a failed return code should delete the object
      if (pIn <> nil) then
        begin
          if SUCCEEDED(hr) then
               FInput := pIn
          else pIn.Free;
        end
      else
        hr := E_OUTOFMEMORY;
  end;
end;

constructor TWavDestFilter.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var hr: HRESULT;
begin
  Create(Controller, hr);
end;

function TWavDestFilter.DecideBufferSize(Alloc: IMemAllocator;
  Properties: PAllocatorProperties): HRESULT;
var
  InProps, Actual: TAllocatorProperties;
  InAlloc: IMemAllocator;
begin
  // Is the input pin connected
  if not FInput.IsConnected then
    begin
      result := E_UNEXPECTED;
      exit;
    end;

  ASSERT(Alloc <> nil);
  ASSERT(Properties <> nil);

  Properties.cBuffers := 1;
  Properties.cbAlign  := 1;

  // Get input pin's allocator size and use that
  result := FInput.GetAllocator(InAlloc);
  if SUCCEEDED(result) then
  begin
    result := InAlloc.GetProperties(InProps);
    if SUCCEEDED(result) then
      Properties.cbBuffer := InProps.cbBuffer;
    InAlloc := nil;
  end;

  if FAILED(result) then exit;

  ASSERT(Properties.cbBuffer <> 0);

  // Ask the allocator to reserve us some sample memory, NOTE the function
  // can succeed (that is return NOERROR) but still not have allocated the
  // memory that we requested, so we must check we got whatever we wanted

  result := Alloc.SetProperties(Properties^, Actual);
  if FAILED(result) then exit;

  ASSERT(Actual.cBuffers = 1);

  if (Properties.cBuffers > Actual.cBuffers) or
     (Properties.cbBuffer > Actual.cbBuffer) then
  result := E_FAIL else
  result := NOERROR;
end;

function TWavDestFilter.GetMediaType(Position: integer;
  out MediaType: PAMMediaType): HRESULT;
begin
  ASSERT((Position = 0) or (Position = 1));
  if(Position = 0) then
  begin
    MediaType.majortype := MEDIATYPE_Stream;
    MediaType.Subtype   := MEDIASUBTYPE_WAVE;
    result := S_OK;
    exit;
  end;
  result := VFW_S_NO_MORE_ITEMS;
end;

function TWavDestFilter.Receive(Sample: IMediaSample): HRESULT;
var Old: Cardinal;
begin
  Old := FWavData;
  result := inherited Receive(Sample);
  // don't update the count if Deliver() downstream fails.
  if(result <> S_OK) then FWavData := Old;
end;

function TWavDestFilter.StartStreaming: HRESULT;
begin
  // leave space for the header
  FHeader := sizeof(TRIFFLIST) +
               sizeof(TRIFFCHUNK) +
               FInput.AMMEdiaType.cbFormat +
               sizeof(TRIFFCHUNK);

  FWavData := 0;
  result := S_OK;
end;

function TWavDestFilter.StopStreaming: HRESULT;
type TByteDynArray = array of Byte;
var
  Stream: IStream;
  DwnstrmInputPin: IPin;
  pb: PByte;
  RiffWave: PRIFFLIST;
  RiffFmt, RiffData : PRIFFCHUNK;
  li, newposition: Int64;
begin
    if not FOutput.IsConnected then
      begin
        result := E_FAIL;
        exit;
      end;

    DwnstrmInputPin := FOutput.GetConnected;

    if (DwnstrmInputPin = nil) then
      begin
        result := E_FAIL;
        exit;
      end;

    result := DwnstrmInputPin.QueryInterface(IStream, Stream);
    if SUCCEEDED(result) then
    begin
        GetMem(pb, FHeader);

        RiffWave := PRIFFLIST(pb);
        RiffFmt  := PRIFFCHUNK(Cardinal(RiffWave) + SizeOf(TRIFFLIST));
        RiffData := PRIFFCHUNK((Cardinal(RiffFmt) + SizeOf(TRIFFCHUNK)) + FInput.ammediatype.cbFormat);

        RiffData.fcc := FCC('data');
        RiffData.cb := FWavData;

        RiffFmt.fcc := FCC('fmt ');
        RiffFmt.cb  := FInput.AMMediaType.cbFormat;
        CopyMemory(Pointer(Cardinal(RiffFmt) + SizeOf(TRIFFCHUNK)), FInput.AMMediaType.pbFormat, RiffFmt.cb);

        RiffWave.fcc := FCC('RIFF');
        RiffWave.cb  := FWavData + FHeader - sizeof(TRIFFCHUNK);
        RiffWave.fccListType := FCC('WAVE');

        ZeroMemory(@li, sizeof(li));

        newposition := 0;
        result := Stream.Seek(li, STREAM_SEEK_SET, newposition);
        if SUCCEEDED(result) then
          result := Stream.Write(pb, FHeader, nil);
        Stream := nil;
        freemem(pb);
    end;
end;

function TWavDestFilter.Transform(pIn, pOut: IMediaSample): HRESULT;
var
  rtStart, rtEnd: TReferenceTime;
  Actual: Cardinal;
begin
  // First just copy the data to the output sample
  result := Copy(pIn, pOut);
  if FAILED(result) then exit;

  // Prepare it for writing
  Actual := pOut.GetActualDataLength;

  if (FWavData + FHeader + Actual) < (FWavData + FHeader ) then
    begin // overflow
      result := E_FAIL;
      exit;
    end;

  rtStart := FWavData + FHeader;
  rtEnd   := rtStart + Actual;
  FWavData := FWavData + Actual;

  ASSERT(pOut.SetTime(@rtStart, @rtEnd) = S_OK);

  result := S_OK;
end;

initialization
  TBCClassFactory.CreateFilter(TWavDestFilter, 'WAV Dest', CLSID_WavDest,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE, 0, nil);
end.
