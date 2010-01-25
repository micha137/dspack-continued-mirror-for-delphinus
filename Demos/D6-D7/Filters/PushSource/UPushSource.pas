//------------------------------------------------------------------------------
// File:              UPushSource.pas
// Original file(s):  Setup.cpp, PushSource.h,
//                    PushSourceBitmap.cpp, PushSourceBitmapSet.cpp,
//                    PushSourceDesktop.cpp, PushSource.h,
//                    DibHelper.cpp, DibHelper.h, PushGuids.h
// Converted to Delphi by Nevhasymyy Andriy (E-Mail: a.n@email.com)
// Thanks to Henri Gourvest (hgourvest@progdigy.com)
//
// Desc: DirectShow sample code - In-memory push mode source filter
//
// Portions created by Microsoft are
// Copyright (c) 1992-2002 Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

{
  @abstract(In-memory push mode source filter Delphi conversion)
  @author(Andriy Nevhasymyy: a.n@email.com)
  @created(Jun 28, 2003)
  @lastmod(Aug 04, 2003)
}
unit UPushSource;

interface
uses
  BaseClass, DirectShow9, DSUtil,
  Windows, ActiveX, SysUtils;

const
  //
  // GUID definitions for PushSource filter set
  //
  CLSID_PushSourceBitmap: TGUID = '{E446D455-7C13-492A-9C96-38F948687E8A}';
  CLSID_PushSourceBitmapSet: TGUID = '{D5070569-3C5F-4988-A77C-4DF44E850C43}';
  CLSID_PushSourceDesktop: TGUID = '{570757C1-D2D8-42D1-BA0C-24E1BED3F62F}';

  // Setup information
  sudPinTypes: TRegPinTypes =
  (
    // video stream connection
    clsMajorType: @MEDIATYPE_Video;
    // all available
    clsMinorType: @MEDIASUBTYPE_NULL
    );

  // pins info
  sudOutputPinBitmap: array[0..0] of TRegFilterPins =
  (
    (
    strName: 'Output'; // Pins string name
    bRendered: FALSE; // Is it rendered
    bOutput: TRUE; // Is it an output
    bZero: FALSE; // Are we allowed none
    bMany: FALSE; // And allowed many
    oFilter: nil; // Connects to filter
    strConnectsToPin: nil; // Connects to pin
    nMediaTypes: 1; // Number of types
    lpMediaType: @sudPinTypes // Pin information
    )
    );
  sudOutputPinBitmapSet: array[0..0] of TRegFilterPins =
  (
    (
    strName: 'Output'; // Pins string name
    bRendered: FALSE; // Is it rendered
    bOutput: TRUE; // Is it an output
    bZero: FALSE; // Are we allowed none
    bMany: FALSE; // And allowed many
    oFilter: nil; // Connects to filter
    strConnectsToPin: nil; // Connects to pin
    nMediaTypes: 1; // Number of types
    lpMediaType: @sudPinTypes // Pin information
    )
    );

  sudOutputPinDesktop: array[0..0] of TRegFilterPins =
  (
    (
    strName: 'Output'; // Pins string name
    bRendered: FALSE; // Is it rendered
    bOutput: TRUE; // Is it an output
    bZero: FALSE; // Are we allowed none
    bMany: FALSE; // And allowed many
    oFilter: nil; // Connects to filter
    strConnectsToPin: nil; // Connects to pin
    nMediaTypes: 1; // Number of types
    lpMediaType: @sudPinTypes // Pin information
    )
    );

  UNITS = 10000000;
  FPS_30 = UNITS div 30;
  FPS_20 = UNITS div 20;
  FPS_10 = UNITS div 10;
  FPS_5 = UNITS div 5;
  FPS_4 = UNITS div 4;
  FPS_3 = UNITS div 3;
  FPS_2 = UNITS div 2;
  FPS_1 = UNITS div 1;

  DefaultFrameLength: TReferenceTime = FPS_10;

  // Filter name strings
  PushBitmapName: WideString = '_ PushSource Bitmap Filter';
  PushBitmapSetName: WideString = '_ PushSource BitmapSet Filter';
  PushDesktopName: WideString = '_ PushSource Desktop Filter';

  // default bitmap file name
  BITMAP_NAME: string = 'sample.bmp';

  // Number of bitmap files to load in the CPushPinBitmapSet class
  Num_Files = 5;

type
  (**********************************************
   *
   *  Class declarations
   *
   **********************************************)

  TBCPushPinBitmap = class(TBCSourceStream)
  protected
    // To track where we are in the file
    FFramesWritten: Integer;
    // Do we need to clear the buffer?
    FZeroMemory: Boolean;
    // The time stamp for each sample
    FSampleTime: TRefTime;

    // Pointer to the bitmap header
    FBmi: PBitmapInfo;
    // Size of the bitmap header
    FBitmapInfo: DWord;

    // File opening variables
     // Handle returned from CreateFile
    FFileHandle: THandle;
    // Points to beginning of file buffer
    FFileBuffer: PByte;
    // Points to pixel bits
    FImage: PByte;

    // How many frames have been displayed
    FFrameNumber: Integer;
    // Duration of one frame
    FFrameLength: TReferenceTime;

    // Protects our internal state
    FSharedState: TBCCritSec;

  public
    constructor Create(out hr: HResult; Filter: TBCSource);
    destructor Destroy; override;

    // Override the version that offers exactly one media type
    function GetMediaType(MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator;
      Properties: PAllocatorProperties): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;

    // Quality control
   // Not implemented because we aren't going in real time.
   // If the file-writing filter slows the graph down, we just do nothing, which means
   // wait until we're unblocked. No frames are ever dropped.
    // Quality control notifications sent to us
    function Notify(Filter: IBaseFilter; q: TQuality): HRESULT; override;
      stdcall;
  end;

  TBCPushPinBitmapSet = class(TBCSourceStream)
  protected
    // To track where we are in the file
    FFramesWritten: Integer;
    // Do we need to clear the buffer?
    FZeroMemory: Boolean;
    // The time stamp for each sample
    FSampleTime: TRefTime;

    // Array of bitmap headers pointers
    FBmi: array[0..Num_Files - 1] of PBitmapInfo;
    // Sizes of the bitmap headers
    FBitmapInfo: array[0..Num_Files - 1] of DWord;

    // File opening variables
     // Handles returned from CreateFile
    FFileHandle: array[0..Num_Files - 1] of THandle;
    // Points to beginning of files buffer
    FFileBuffer: array[0..Num_Files - 1] of PByte;
    // Points to pixel bits
    FImage: array[0..Num_Files - 1] of PByte;

    // number of files loaded
    FFilesLoaded: Boolean;

    // Which bitmap is being displayed
    FCurrentBitmap,
      // How many frames have been displayed
    FFrameNumber: Integer;
    // Duration of one frame
    FFrameLength: TReferenceTime;

    // Protects our internal state
    FSharedState: TBCCritSec;

  public
    constructor Create(out hr: HResult; Filter: TBCSource);
    destructor Destroy; override;

    // Override the version that offers exactly one media type
    function GetMediaType(MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator;
      Properties: PAllocatorProperties): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;

    // Quality control
   // Not implemented because we aren't going in real time.
   // If the file-writing filter slows the graph down, we just do nothing, which means
   // wait until we're unblocked. No frames are ever dropped.
    // Quality control notifications sent to us
    function Notify(Filter: IBaseFilter; q: TQuality): HRESULT; override;
      stdcall;
  end;

  TBCPushPinDesktop = class(TBCSourceStream)
  protected
    // To track where we are in the file
    FFramesWritten: Integer;
    // Do we need to clear the buffer?
    FZeroMemory: Boolean;
    // The time stamp for each sample
    FSampleTime: TRefTime;

    // How many frames have been displayed
    FFrameNumber: Integer;
    // Duration of one frame
    FFrameLength: TReferenceTime;

    // Rect containing entire screen coordinates
    FScreenRect: TRect;

    // The current image height
    FImageHeight,
      // And current image width
    FImageWidth,
      // Time in msec between frames
    FRepeatTime,
      // Screen bit depth
    FCurrentBitDepth: Integer;

    FMediaType: TAMMediaType;

    // Protects our internal state
    FSharedState: TBCCritSec;

  public
    constructor Create(out hr: HResult; Filter: TBCSource);
    destructor Destroy; override;

    // Override the version that offers exactly one media type
    function GetMediaType(iPosition: Integer;
      out MediaType: PAMMediaType): HResult; override;
    // We will accept 8, 16, 24 or 32 bit video formats, in any
    // image size that gives room to bounce.
    // Returns E_INVALIDARG if the mediatype is not acceptable
    function CheckMediaType(MediaType: PAMMediaType): HResult; override;
    function DecideBufferSize(Allocator: IMemAllocator;
      Properties: PAllocatorProperties): HRESULT; override;
    function SetMediaType(MediaType: PAMMediaType): HRESULT; override;
    function FillBuffer(Sample: IMediaSample): HResult; override;

    // Quality control
   // Not implemented because we aren't going in real time.
   // If the file-writing filter slows the graph down, we just do nothing, which means
   // wait until we're unblocked. No frames are ever dropped.
    // Quality control notifications sent to us
    function Notify(Filter: IBaseFilter; q: TQuality): HRESULT; override;
      stdcall;
  end;

  // In-memory push mode source filter
  // Provides a static bitmap as the video output stream.
  TBCPushSourceBitmap = class(TBCSource)
  private
    FPin: TBCPushPinBitmap;

  public
    constructor Create(ObjName: string; Unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;
    destructor Destroy; override;
  end;

  // In-memory push mode source filter
  // Provides a rotating set of bitmaps as the video output stream.
  TBCPushSourceBitmapset = class(TBCSource)
  private
    FPin: TBCPushPinBitmapSet;

  public
    constructor Create(ObjName: string; Unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;
    destructor Destroy; override;
  end;

  // In-memory push mode source filter
  // Provides an image of the user's desktop as a continuously updating stream.
  TBCPushSourceDesktop = class(TBCSource)
  private
    FPin: TBCPushPinDesktop;

  public
    constructor Create(ObjName: string; Unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;
    destructor Destroy; override;
  end;

implementation

// --- TBCPushPinBitmap ---------------

constructor TBCPushPinBitmap.Create(out hr: HResult; Filter: TBCSource);
var
  CurrentDir: array[0..MAX_PATH - 1] of Char;
  CurrentFileName, MediaFileName: AnsiString;
  MsgText: AnsiString;
  FileSize, BytesRead: DWord;
  FileHeaderSize: Integer;
  BmpFileHeader: PBITMAPFILEHEADER;
  pb: PByte;

begin
  inherited Create('_ Push Source Bitmap', hr, Filter, 'Out');

  FFramesWritten := 0;
  FZeroMemory := False;
  FBmi := nil;
  FBitmapInfo := 0;
  FFileHandle := INVALID_HANDLE_VALUE;
  FFileBuffer := nil;
  FImage := nil;
  FFrameNumber := 0;
  // Display 5 bitmap frames per second
  FFrameLength := FPS_5;
  FSharedState := TBCCritSec.Create;

  // The main point of this sample is to demonstrate how to take a DIB
  // in host memory and insert it into a video stream.
  // To keep this sample as simple as possible, we just read a single 24 bpp bitmap
  // from a file and copy it into every frame that we send downstream.

  // In the filter graph, we connect this filter to the AVI Mux, which creates
  // the AVI file with the video frames we pass to it. In this case,
  // the end result is a still image rendered as a video stream.

  // Your filter will hopefully do something more interesting here.
  // The main point is to set up a buffer containing the DIB pixel bits.
  // This must be done before you start running.

  // First look for the bitmap in the current directory
  GetCurrentDirectory(MAX_PATH - 1, CurrentDir);
  CurrentFileName := Format('%s\%s', [CurrentDir, Bitmap_Name]);

  FFileHandle := CreateFile(PChar(CurrentFileName), GENERIC_READ, 0, nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);

  if (FFileHandle = INVALID_HANDLE_VALUE) then
  begin
    // File was not in the application's current directory,
    // so look in the DirectX SDK media path instead.
    MediaFileName := Format('%s\%s'#0, [GetDXSDKMediaPath, Bitmap_Name]);

    FFileHandle := CreateFile(PChar(MediaFileName), GENERIC_READ, 0, nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);

    if (FFileHandle = INVALID_HANDLE_VALUE) then
    begin
      MsgText := Format('Could not open bitmap source file ' +
        'in the application directory:'#13#10 + '%s'#13#10 +
        'or in the DirectX SDK Media folder:'#13#10 + '%s'#13#10 +
        'Please copy this file either to the application''s folder'#13#10 +
        'or to the DirectX SDK Media folder, then recreate this filter'#13#10 +
        'Otherwise, you will not be able to render the output pin',
        [CurrentFileName, MediaFileName]);

      OutputDebugString(PChar(MsgText));
      MessageBox(0, PChar(MsgText), 'PushSource filter error',
        MB_ICONERROR or MB_OK);
      hr := HRESULTFROMWIN32(GetLastError);
      Exit;
    end;
  end;

  FileSize := GetFileSize(FFileHandle, nil);
  if (FileSize = INVALID_FILE_SIZE) then
  begin
{$IFDEF DEBUG}
    DbgLog(Self, 'Invalid file size');
{$ENDIF}
    hr := HRESULTFROMWIN32(GetLastError());
    Exit;
  end;

  FFileBuffer := CoTaskMemAlloc(FileSize);
  if (FFileBuffer = nil) then
  begin
    OutputDebugString('Could not allocate FImage');
    hr := E_OUTOFMEMORY;
    Exit;
  end;

  BytesRead := 0;
  if not (ReadFile(FFileHandle, FFileBuffer^, FileSize, BytesRead, nil)) then
  begin
    hr := HRESULTFROMWIN32(GetLastError());
    OutputDebugString('ReadFile failed');
    Exit;
  end;

  // WARNING - This code does not verify that the file is a valid bitmap file.
  // In your own filter, you would check this or else generate the bitmaps
  // yourself in memory.

  FileHeaderSize := SizeOf(BITMAPFILEHEADER);

  // Store the size of the BITMAPINFO
  BmpFileHeader := PBITMAPFILEHEADER(FFileBuffer);
  FBitmapInfo := Integer(BmpFileHeader.bfOffBits) - FileHeaderSize;

  // Store a pointer to the BITMAPINFO
  pb := PByte(FFileBuffer);
  Inc(pb, FileHeaderSize);
  FBmi := PBITMAPINFO(pb);

  // Store a pointer to the starting address of the pixel bits
  Inc(pb, FBitmapInfo);
  FImage := pb;

  // Close and invalidate the file handle, since we have copied its bitmap data
  CloseHandle(FFileHandle);
  FFileHandle := INVALID_HANDLE_VALUE;
end;

destructor TBCPushPinBitmap.Destroy;
begin
{$IFDEF DEBUG}
  DbgLog(self, Format('Frames written %d', [FFrameNumber]));
{$ENDIF}

  if Assigned(FFileBuffer) then
  begin
    //FreeMem(FFileBuffer);
    CoTaskMemFree(FFileBuffer);
    FFileBuffer := nil;
  end;

  // The constructor might quit early on error and not close the file...
  if (FFileHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(FFileHandle);
  if (FSharedState <> nil) then
    FreeAndNil(FSharedState);

  inherited;
end;

// GetMediaType: This method tells the downstream pin what types we support.

// Here is how CSourceStream deals with media types:
//
// If you support exactly one type, override GetMediaType(MediaType : PAMMediaType).
// It will then be called when (a) our filter proposes a media type,
// (b) the other filter proposes a type and we have to check that type.
//
// If you support > 1 type, override GetMediaType(iPosition : Integer;
//  out MediaType : PAMMediaType) AND CheckMediaType.
//
// In this case we support only one type, which we obtain from the bitmap file.

function TBCPushPinBitmap.GetMediaType(MediaType: PAMMediaType): HResult;
var
  pvi: PVIDEOINFOHEADER;

begin
  FFilter.StateLock.Lock;
  try
    if (MediaType = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    // If the bitmap file was not loaded, just fail here.
    if (FImage = nil) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    // Allocate enough room for the VIDEOINFOHEADER and the color tables
    MediaType.cbFormat := SIZE_PREHEADER + FBitmapInfo;
    pvi := CoTaskMemAlloc(MediaType.cbFormat);
    if (pvi = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    ZeroMemory(pvi, MediaType.cbFormat);
    pvi.AvgTimePerFrame := FFrameLength;

    // Copy the header info
    CopyMemory(@pvi.bmiHeader, FBmi, FBitmapInfo);

    // Set image size for use in FillBuffer
    pvi.bmiHeader.biSizeImage := GetBitmapSize(@pvi.bmiHeader);

    // Clear source and target rectangles
    // we want the whole image area rendered
    SetRectEmpty(pvi.rcSource);
    // no particular destination rectangle
    SetRectEmpty(pvi.rcTarget);

    MediaType.majortype := MEDIATYPE_Video;
    MediaType.formattype := FORMAT_VideoInfo;
    // Work out the GUID for the subtype from the header info.
    MediaType.subtype := GetBitmapSubtype(@pvi.bmiHeader);
    MediaType.bTemporalCompression := False;
    MediaType.bFixedSizeSamples := True;
    MediaType.pbFormat := pvi;
    MediaType.lSampleSize := pvi.bmiHeader.biSizeImage;

    Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

function TBCPushPinBitmap.DecideBufferSize(Allocator: IMemAllocator;
  Properties: PAllocatorProperties): HRESULT;
var
  pvi: PVIDEOINFOHEADER;
  Actual: ALLOCATOR_PROPERTIES;

begin
  if (Allocator = nil) or (Properties = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FFilter.StateLock.Lock;
  try
    // If the bitmap file was not loaded, just fail here.
    if (FImage = nil) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    pvi := AMMediaType.pbFormat;

    // Ensure a minimum number of buffers
    if (Properties.cBuffers = 0) then
      Properties.cBuffers := 2;
    Properties.cbBuffer := pvi.bmiHeader.biSizeImage;

    Result := Allocator.SetProperties(Properties^, Actual);
    if Failed(Result) then
      Exit;

    // Is this allocator unsuitable?
    if (Actual.cbBuffer < Properties.cbBuffer) then
      Result := E_FAIL
    else
      Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

// This is where we insert the DIB bits into the video stream.
// FillBuffer is called once for every sample in the stream.

function TBCPushPinBitmap.FillBuffer(Sample: IMediaSample): HResult;
var
  pData: PByte;
  cbData: Longint;
  pvi: PVIDEOINFOHEADER;
  Start, Stop: REFERENCE_TIME;

  function min(v1, v2: DWord): DWord;
  begin
    if v1 <= v2 then
      Result := v1
    else
      Result := v2;
  end;

begin
  if (Sample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // If the bitmap file was not loaded, just fail here.
  if (FImage = nil) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  FSharedState.Lock;
  try
    // Access the sample's data buffer
    Sample.GetPointer(pData);
    cbData := Sample.GetSize;

    // Check that we're still using video
    Assert(IsEqualGUID(AMMediaType.formattype, FORMAT_VideoInfo));

    pvi := AMMediaType.pbFormat;

    // If we want to change the contents of our source buffer (FImage)
    // at some interval or based on some condition, this is where to do it.
    // Remember that the new data has the same format that we specified in GetMediaType.
    // For example:
    // if(FFrameNumber > SomeValue)
    //    LoadNewBitsIntoBuffer(FImage)

    // Copy the DIB bits over into our filter's output buffer.
    // Since sample size may be larger than the image size, bound the copy size.
    CopyMemory(pData, FImage, min(pvi.bmiHeader.biSizeImage, cbData));

    // Set the timestamps that will govern playback frame rate.
    // If this file is getting written out as an AVI,
    // then you'll also need to configure the AVI Mux filter to
    // set the Average Time Per Frame for the AVI Header.
    // The current time is the sample's start
    Start := FFrameNumber * FFrameLength;
    Stop := Start + FFrameLength;

    Sample.SetTime(@Start, @Stop);
    Inc(FFrameNumber);

    // Set TRUE on every sample for uncompressed frames
    Sample.SetSyncPoint(True);

    Result := S_OK;

  finally
    FSharedState.UnLock;
  end;
end;

function TBCPushPinBitmap.Notify(Filter: IBaseFilter; q: TQuality): HRESULT;
begin
  Result := E_FAIL;
end;

// --- TBCPushSourceBitmap ------------

constructor TBCPushSourceBitmap.Create(ObjName: string; Unk: IUnKnown;
  out hr: HRESULT);
begin
  inherited Create(ObjName, Unk, CLSID_PushSourceBitmap);

  // The pin magically adds itself to our pin array.
  FPin := TBCPushPinBitmap.Create(hr, Self);

  if (hr <> S_OK) then
    if (FPin = nil) then
      hr := E_OUTOFMEMORY;
end;

constructor TBCPushSourceBitmap.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TBCPushSourceBitmap.Destroy;
begin
  FreeAndNil(FPin);
  inherited;
end;

// --- TBCPushPinBitmapSet ------------

constructor TBCPushPinBitmapSet.Create(out hr: HResult; Filter: TBCSource);
var
  CurrentDir: array[0..MAX_PATH - 1] of Char;
  CurrentFileName, MediaFileName: AnsiString;
  MsgText: AnsiString;
  FileSize, BytesRead: DWord;
  FilesLoaded, FileHeaderSize: Integer;
  BmpFileHeader: PBITMAPFILEHEADER;
  pb: PByte;
  i: Integer;

begin
  inherited Create('_ Push Source Bitmap Set', hr, Filter, 'Out');

  FFramesWritten := 0;
  FZeroMemory := False;
  FFrameNumber := 0;

  // Display 5 bitmap frames per second
  FFrameLength := FPS_2;
  FSharedState := TBCCritSec.Create;
  FCurrentBitmap := 0;
  FFilesLoaded := False;

  FilesLoaded := 0;
  // Initialize member data arrays
  ZeroMemory(@FBitmapInfo, NUM_FILES * sizeof(DWord));
  ZeroMemory(@FBmi, NUM_FILES * sizeof(PBitmapInfo));
  ZeroMemory(@FFileHandle, NUM_FILES * sizeof(THandle));
  ZeroMemory(@FFileBuffer, NUM_FILES * sizeof(Byte));
  ZeroMemory(@FImage, NUM_FILES * sizeof(Byte));

  // The main point of this sample is to demonstrate how to take a DIB
  // in host memory and insert it into a video stream.
  // We read a set of bitmaps from files and copy one bitmap
  // into every frame that we send downstream.

  // In the filter graph, we connect this filter to the AVI Mux, which creates
  // the AVI file with the video frames we pass to it. In this case,
  // the end result is a rotating set of images rendered as a video stream.

  // First look for the bitmap in the current directory
  GetCurrentDirectory(MAX_PATH - 1, CurrentDir);

  for i := 0 to NUM_FILES - 1 do
  begin
    // Assume that the bitmap in the application's directory
    CurrentFileName := Format('%s\BitmapSet%d.bmp', [CurrentDir, i]);
    FFileHandle[i] := CreateFile(PChar(CurrentFileName), GENERIC_READ, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if (FFileHandle[i] = INVALID_HANDLE_VALUE) then
    begin
      // File was not in the application's current directory,
      // so look in the DirectX SDK media path instead.  The path contained
      // in szMediaDir will already have a trailing backslash '\'.
      MediaFileName := Format('%sBitmapSet%d.bmp', [GetDXSDKMediaPath, i]);

      FFileHandle[i] := CreateFile(PChar(MediaFileName), GENERIC_READ, 0, nil,
        OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

      if (FFileHandle[i] = INVALID_HANDLE_VALUE) then
      begin
        MsgText := Format('Could not open bitmap source file (#%d of %d) ' +
          'in the application directory:'#13#10 + '%s'#13#10 +
          'or in the DirectX SDK Media folder:'#13#10 + '%s'#13#10 +
          'Please copy this file either to the application''s folder'#13#10 +
          'or to the DirectX SDK Media folder, then recreate this filter'#13#10 +
          'Otherwise, you will not be able to render the output pin',
          [i + 1, NUM_FILES, CurrentFileName, MediaFileName]);

        OutputDebugString(PChar(MsgText));
        MessageBox(0, PChar(MsgText), 'PushSource filter error',
          MB_ICONERROR or MB_OK);
        hr := HRESULTFROMWIN32(GetLastError());
        Exit;
      end;
    end;

    FileSize := GetFileSize(FFileHandle[i], nil);
    if (FileSize = INVALID_FILE_SIZE) then
    begin
{$IFDEF DEBUG}
      DbgLog(Self, 'Invalid file size');
{$ENDIF}
      hr := HRESULTFROMWIN32(GetLastError());
      Exit;
    end;

    FFileBuffer[i] := CoTaskMemAlloc(FileSize);
    if (FFileBuffer[i] = nil) then
    begin
      OutputDebugString('Could not allocate FImage');
      hr := E_OUTOFMEMORY;
      Exit;
    end;

    BytesRead := 0;
    if not (ReadFile(FFileHandle[i], FFileBuffer[i]^, FileSize, BytesRead, nil))
      then
    begin
      hr := HRESULTFROMWIN32(GetLastError());
      OutputDebugString('ReadFile failed');
      Exit;
    end;

    // WARNING - This code does not verify that the file is a valid bitmap file.
    // In your own filter, you would check this or else generate the bitmaps
    // yourself in memory.

    FileHeaderSize := SizeOf(BITMAPFILEHEADER);

    // Store the size of the BITMAPINFO
    BmpFileHeader := PBITMAPFILEHEADER(FFileBuffer[i]);
    FBitmapInfo[i] := Integer(BmpFileHeader.bfOffBits) - FileHeaderSize;

    // Store a pointer to the BITMAPINFO
    pb := PByte(FFileBuffer[i]);
    Inc(pb, FileHeaderSize);
    FBmi[i] := PBITMAPINFO(pb);

    // Store a pointer to the starting address of the pixel bits
    Inc(pb, FBitmapInfo[i]);
    FImage[i] := pb;

    // Close and invalidate the file handle, since we have copied its bitmap data
    CloseHandle(FFileHandle[i]);
    FFileHandle[i] := INVALID_HANDLE_VALUE;
    // Count this is a successful file load.  If not all files load
    // properly, then the filter will not operate correctly.
    Inc(FilesLoaded);
  end;

  // Make sure that ALL files were properly loaded
  if (FilesLoaded <> NUM_FILES) then
    hr := E_FAIL
  else
  begin
    FFilesLoaded := TRUE;
  end;
end;

destructor TBCPushPinBitmapSet.Destroy;
var
  i: Integer;
begin
{$IFDEF DEBUG}
  DbgLog(self, Format('Frames written %d', [FFrameNumber]));
{$ENDIF}

  for i := 0 to NUM_FILES - 1 do
  begin
    if Assigned(FFileBuffer[i]) then
    begin
      CoTaskMemFree(FFileBuffer[i]);
      FFileBuffer[i] := nil;
    end;

    // The constructor might quit early on error and not close the file...
    if (FFileHandle[i] <> INVALID_HANDLE_VALUE) then
      CloseHandle(FFileHandle[i]);
  end;

  if (FSharedState <> nil) then
    FreeAndNil(FSharedState);

  inherited;
end;

// GetMediaType: This method tells the downstream pin what types we support.

// Here is how CSourceStream deals with media types:
//
// If you support exactly one type, override GetMediaType(MediaType : PAMMediaType).
// It will then be called when (a) our filter proposes a media type,
// (b) the other filter proposes a type and we have to check that type.
//
// If you support > 1 type, override GetMediaType(iPosition : Integer;
//  out MediaType : PAMMediaType) AND CheckMediaType.
//
// In this case we support only one type, which we obtain from the bitmap file.

function TBCPushPinBitmapSet.GetMediaType(MediaType: PAMMediaType): HResult;
var
  pvi: PVIDEOINFOHEADER;

begin
  FFilter.StateLock.Lock;
  try
    if (MediaType = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    // If the bitmap files were not loaded, just fail here.
    if not (FFilesLoaded) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    // Allocate enough room for the VIDEOINFOHEADER and the color tables
    MediaType.cbFormat := SIZE_PREHEADER + FBitmapInfo[FCurrentBitmap];
    pvi := CoTaskMemAlloc(MediaType.cbFormat);
    if (pvi = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    ZeroMemory(pvi, MediaType.cbFormat);
    pvi.AvgTimePerFrame := FFrameLength;

    // Copy the header info
    CopyMemory(@pvi.bmiHeader, FBmi[FCurrentBitmap],
      FBitmapInfo[FCurrentBitmap]);

    // Set image size for use in FillBuffer
    pvi.bmiHeader.biSizeImage := GetBitmapSize(@pvi.bmiHeader);

    // Clear source and target rectangles
    // we want the whole image area rendered
    SetRectEmpty(pvi.rcSource);
    // no particular destination rectangle
    SetRectEmpty(pvi.rcTarget);

    MediaType.majortype := MEDIATYPE_Video;
    MediaType.formattype := FORMAT_VideoInfo;
    // Work out the GUID for the subtype from the header info.
    MediaType.subtype := GetBitmapSubtype(@pvi.bmiHeader);
    MediaType.bTemporalCompression := False;
    MediaType.bFixedSizeSamples := True;
    //MediaType.cbFormat := SizeOf(TBitmapInfo);
    MediaType.pbFormat := pvi;
    MediaType.lSampleSize := pvi.bmiHeader.biSizeImage;

    Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

function TBCPushPinBitmapSet.DecideBufferSize(Allocator: IMemAllocator;
  Properties: PAllocatorProperties): HRESULT;
var
  pvi: PVIDEOINFOHEADER;
  Actual: ALLOCATOR_PROPERTIES;

begin
  if (Allocator = nil) or (Properties = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  FFilter.StateLock.Lock;
  try
    // If the bitmap files were not loaded, just fail here.
    if not (FFilesLoaded) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    pvi := AMMediaType.pbFormat;

    // Ensure a minimum number of buffers
    if (Properties.cBuffers = 0) then
      Properties.cBuffers := 2;
    Properties.cbBuffer := pvi.bmiHeader.biSizeImage;

    Result := Allocator.SetProperties(Properties^, Actual);
    if Failed(Result) then
      Exit;

    // Is this allocator unsuitable?
    if (Actual.cbBuffer < Properties.cbBuffer) then
      Result := E_FAIL
    else
      Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

// This is where we insert the DIB bits into the video stream.
// FillBuffer is called once for every sample in the stream.

function TBCPushPinBitmapSet.FillBuffer(Sample: IMediaSample): HResult;
var
  pData: PByte;
  cbData: Longint;
  pvi: PVIDEOINFOHEADER;
  Start, Stop: REFERENCE_TIME;

  function min(v1, v2: DWord): DWord;
  begin
    if v1 <= v2 then
      Result := v1
    else
      Result := v2;
  end;

begin
  // If the bitmap files were not loaded, just fail here.
  if not (FFilesLoaded) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if (Sample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FSharedState.Lock;
  try
    // Access the sample's data buffer
    Sample.GetPointer(pData);
    cbData := Sample.GetSize;

    // Check that we're still using video
    Assert(IsEqualGUID(AMMediaType.formattype, FORMAT_VideoInfo));

    pvi := AMMediaType.pbFormat;

    // Copy the DIB bits over into our filter's output buffer.
    // Since sample size may be larger than the image size, bound the copy size.
    // Remember that the new data has the same format
    // that we specified in GetMediaType.
    CopyMemory(pData, FImage[FCurrentBitmap],
      min(pvi.bmiHeader.biSizeImage, cbData));

    // Set the timestamps that will govern playback frame rate.
    // If this file is getting written out as an AVI,
    // then you'll also need to configure the AVI Mux filter to
    // set the Average Time Per Frame for the AVI Header.
    // The current time is the sample's start
    Start := FFrameNumber * FFrameLength;
    Stop := Start + FFrameLength;

    Sample.SetTime(@Start, @Stop);
    Inc(FFrameNumber);

    // Set TRUE on every sample for uncompressed frames
    Sample.SetSyncPoint(True);

    // Increment the current buffer so that the next FillBuffer() call
    // will use the bits from the next bitmap in the set.
    Inc(FCurrentBitmap);
    FCurrentBitmap := FCurrentBitmap mod NUM_FILES;

    Result := S_OK;

  finally
    FSharedState.UnLock;
  end;
end;

function TBCPushPinBitmapSet.Notify(Filter: IBaseFilter; q: TQuality): HRESULT;
begin
  Result := E_FAIL;
end;

(**********************************************
 *
 *  CPushSourceBitmapSet Class
 *
 **********************************************)

constructor TBCPushSourceBitmapSet.Create(ObjName: string; Unk: IUnKnown;
  out hr: HRESULT);
begin
  inherited Create(ObjName, Unk, CLSID_PushSourceBitmapSet);

  // The pin magically adds itself to our pin array.
  FPin := TBCPushPinBitmapSet.Create(hr, Self);

  if (hr <> S_OK) then
    if (FPin = nil) then
      hr := E_OUTOFMEMORY;
end;

constructor TBCPushSourceBitmapSet.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TBCPushSourceBitmapSet.Destroy;
begin
  FreeAndNil(FPin);
  inherited;
end;

// --- TBCPushPinDesktop --------------

constructor TBCPushPinDesktop.Create(out hr: HResult; Filter: TBCSource);
var
  DC: HDC;

begin
  inherited Create('_ Push Source Desktop', hr, Filter, 'Out');

  FFramesWritten := 0;
  FZeroMemory := False;
  FFrameNumber := 0;
  // Display 5 bitmap frames per second
  FFrameLength := FPS_5;
  FSharedState := TBCCritSec.Create;
  FCurrentBitDepth := 32;

  // The main point of this sample is to demonstrate how to take a DIB
  // in host memory and insert it into a video stream.

  // To keep this sample as simple as possible, we just read the desktop image
  // from a file and copy it into every frame that we send downstream.

  // In the filter graph, we connect this filter to the AVI Mux, which creates
   // the AVI file with the video frames we pass to it. In this case,
   // the end result is a screen capture video (GDI images only, with no
   // support for overlay surfaces).

   // Get the device context of the main display
  DC := CreateDC('DISPLAY', nil, nil, nil);

  // Get the dimensions of the main desktop window
  FScreenRect.Left := 0;
  FScreenRect.Top := 0;
  FScreenRect.Right := GetDeviceCaps(DC, HORZRES);
  FScreenRect.Bottom := GetDeviceCaps(DC, VERTRES);

  // Save dimensions for later use in FillBuffer()
  FImageWidth := FScreenRect.Right - FScreenRect.Left;
  FImageHeight := FScreenRect.Bottom - FScreenRect.Top;

  // Release the device context
  DeleteDC(DC);

  hr := S_OK;
end;

destructor TBCPushPinDesktop.Destroy;
begin
{$IFDEF DEBUG}
  DbgLog(self, Format('Frames written %d', [FFrameNumber]));
{$ENDIF}
  inherited;
end;

// GetMediaType
//
// Prefer 5 formats - 8, 16 (*2), 24 or 32 bits per pixel
//
// Prefered types should be ordered by quality, with zero as highest quality.
// Therefore, iPosition =
//      0    Return a 32bit mediatype
//      1    Return a 24bit mediatype
//      2    Return 16bit RGB565
//      3    Return a 16bit mediatype (rgb555)
//      4    Return 8 bit palettised format
//      >4   Invalid
//

function TBCPushPinDesktop.GetMediaType(iPosition: Integer;
  out MediaType: PAMMediaType): HResult;
var
  pvi: PVIDEOINFO;
  i: Integer;

begin
  FFilter.StateLock.Lock;
  try
    if (MediaType = nil) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    if (iPosition < 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

    // Have we run off the end of types?
    if (iPosition > 4) then
    begin
      Result := VFW_S_NO_MORE_ITEMS;
      Exit;
    end;

    MediaType.cbFormat := SizeOf(TVideoInfo);
    pvi := CoTaskMemAlloc(MediaType.cbFormat);
    if (pvi = nil) then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    // Initialize the VideoInfo structure before configuring its members
    ZeroMemory(pvi, MediaType.cbFormat);

    case iPosition of
      0:
        begin
          // Return our highest quality 32bit format

          // Since we use RGB888 (the default for 32 bit), there is
          // no reason to use BI_BITFIELDS to specify the RGB
          // masks. Also, not everything supports BI_BITFIELDS
          pvi.bmiHeader.biCompression := BI_RGB;
          pvi.bmiHeader.biBitCount := 32;
        end;
      1:
        begin
          // Return our 24bit format
          pvi.bmiHeader.biCompression := BI_RGB;
          pvi.bmiHeader.biBitCount := 24;
        end;
      2:
        begin
          // 16 bit per pixel RGB565

          // Place the RGB masks as the first 3 doublewords in the palette area
          for i := 0 to 2 do
            pvi.TrueColorInfo.dwBitMasks[i] := bits565[i];

          pvi.bmiHeader.biCompression := BI_BITFIELDS;
          pvi.bmiHeader.biBitCount := 16;
        end;
      3:
        begin
          // 16 bits per pixel RGB555

          // Place the RGB masks as the first 3 doublewords in the palette area
          for i := 0 to 2 do
            pvi.TrueColorInfo.dwBitMasks[i] := bits555[i];

          pvi.bmiHeader.biCompression := BI_BITFIELDS;
          pvi.bmiHeader.biBitCount := 16;
        end;
      4:
        begin
          // 8 bit palettised

          pvi.bmiHeader.biCompression := BI_RGB;
          pvi.bmiHeader.biBitCount := 8;
          pvi.bmiHeader.biClrUsed := iPALETTE_COLORS;
        end;
    end;

    // Adjust the parameters common to all formats
    pvi.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    pvi.bmiHeader.biWidth := FImageWidth;
    pvi.bmiHeader.biHeight := FImageHeight;
    pvi.bmiHeader.biPlanes := 1;
    pvi.bmiHeader.biSizeImage := GetBitmapSize(@pvi.bmiHeader);
    pvi.bmiHeader.biClrImportant := 0;

    // Clear source and target rectangles
    // we want the whole image area rendered
    SetRectEmpty(pvi.rcSource);
    // no particular destination rectangle
    SetRectEmpty(pvi.rcTarget);

    MediaType.majortype := MEDIATYPE_Video;
    MediaType.formattype := FORMAT_VideoInfo;
    MediaType.bTemporalCompression := False;
    MediaType.bFixedSizeSamples := True;

    // Work out the GUID for the subtype from the header info.
    MediaType.subtype := GetBitmapSubtype(@pvi.bmiHeader);
    MediaType.pbFormat := pvi;
    MediaType.lSampleSize := pvi.bmiHeader.biSizeImage;

    Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

// We will accept 8, 16, 24 or 32 bit video formats, in any
// image size that gives room to bounce.
// Returns E_INVALIDARG if the mediatype is not acceptable

function TBCPushPinDesktop.CheckMediaType(MediaType: PAMMediaType): HResult;
var
  pvi: PVIDEOINFO;
  SubType: TGUID;

begin
  // we only output video
  if not (IsEqualGUID(MediaType.majortype, MEDIATYPE_Video)) or
    // in fixed size samples
  not (MediaType.bFixedSizeSamples) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  // Check for the subtypes we support
  SubType := MediaType.subtype;
  if IsEqualGUID(SubType, GUID_NULL) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  if not (
    IsEqualGUID(SubType, MEDIASUBTYPE_RGB8) or
    IsEqualGUID(SubType, MEDIASUBTYPE_RGB565) or
    IsEqualGUID(SubType, MEDIASUBTYPE_RGB555) or
    IsEqualGUID(SubType, MEDIASUBTYPE_RGB24) or
    IsEqualGUID(SubType, MEDIASUBTYPE_RGB32)
    ) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  // Get the format area of the media type
  pvi := MediaType.pbFormat;

  if (pvi = nil) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  // Check if the image width & height have changed
  if (pvi.bmiHeader.biWidth <> FImageWidth) or
    (abs(pvi.bmiHeader.biHeight) <> FImageHeight) then
    // If the image width/height is changed, fail CheckMediaType() to force
    // the renderer to resize the image.
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  // Don't accept formats with negative height, which would cause the desktop
  // image to be displayed upside down.
  if (pvi.bmiHeader.biHeight < 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := S_OK; // This format is acceptable.
end;

//
// DecideBufferSize
//
// This will always be called after the format has been sucessfully
// negotiated. So we have a look at AMMediaType to see what size image we agreed.
// Then we can ask for buffers of the correct size to contain them.
//

function TBCPushPinDesktop.DecideBufferSize(Allocator: IMemAllocator;
  Properties: PAllocatorProperties): HRESULT;
var
  pvi: PVIDEOINFOHEADER;
  Actual: ALLOCATOR_PROPERTIES;

begin
  if (Allocator = nil) or (Properties = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FFilter.StateLock.Lock;
  try
    pvi := AMMediaType.pbFormat;

    Properties.cBuffers := 1;
    Properties.cbBuffer := pvi.bmiHeader.biSizeImage;

    Assert(Properties.cbBuffer <> 0);

    // Ask the allocator to reserve us some sample memory. NOTE: the function
    // can succeed (return NOERROR) but still not have allocated the
    // memory that we requested, so we must check we got whatever we wanted.
    Result := Allocator.SetProperties(Properties^, Actual);
    if Failed(Result) then
      Exit;

    // Is this allocator unsuitable?
    if (Actual.cbBuffer < Properties.cbBuffer) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    // Make sure that we have only 1 buffer
    Assert(Actual.cBuffers = 1);

    Result := S_OK;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

//
// SetMediaType
//
// Called when a media type is agreed between filters
//

function TBCPushPinDesktop.SetMediaType(MediaType: PAMMediaType): HRESULT;
var
  pvi: PVIDEOINFOHEADER;

begin
  FFilter.StateLock.Lock;
  try
    // Pass the call up to my base class
    Result := inherited SetMediaType(MediaType);

    if Succeeded(Result) then
    begin
      pvi := AMMediaType.pbFormat;

      if (pvi = nil) then
      begin
        Result := E_UNEXPECTED;
        Exit;
      end;

      // 8-bit palettized,
      // RGB565, RGB555,
      // RGB24,
      // RGB32
      if pvi.bmiHeader.biBitCount in [8, 16, 24, 32] then
      begin
        // Save the current media type and bit depth
        FMediaType := MediaType^;
        FCurrentBitDepth := pvi.bmiHeader.biBitCount;
      end
      else
      begin
        // We should never agree any other media types
        Assert(False);
        Result := E_INVALIDARG;
      end;
    end;

  finally
    FFilter.StateLock.UnLock;
  end;
end;

// This is where we insert the DIB bits into the video stream.
// FillBuffer is called once for every sample in the stream.

function TBCPushPinDesktop.FillBuffer(Sample: IMediaSample): HResult;
var
  pData: PByte;
  cbData: Longint;
  hDib: HBitmap;
  pvih: PVIDEOINFOHEADER;
  Start, Stop: REFERENCE_TIME;

  function min(v1, v2: DWord): DWord;
  begin
    if v1 <= v2 then
      Result := v1
    else
      Result := v2;
  end;

begin
  if (Sample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  FSharedState.Lock;
  try
    // Access the sample's data buffer
    Sample.GetPointer(pData);
    cbData := Sample.GetSize;

    // Check that we're still using video
    Assert(IsEqualGUID(AMMediaType.formattype, FORMAT_VideoInfo));

    pvih := AMMediaType.pbFormat;

    // Copy the DIB bits over into our filter's output buffer.
     // Since sample size may be larger than the image size, bound the copy size.
    pVih.bmiHeader.biSizeImage := min(pVih.bmiHeader.biSizeImage, cbData);
    hDib := CopyScreenToBitmap(FScreenRect, pData, @pVih.bmiHeader);

    if (hDib <> 0) then
      DeleteObject(hDib);

    // Set the timestamps that will govern playback frame rate.
    // If this file is getting written out as an AVI,
    // then you'll also need to configure the AVI Mux filter to
    // set the Average Time Per Frame for the AVI Header.
    // The current time is the sample's start
    Start := FFrameNumber * FFrameLength;
    Stop := Start + FFrameLength;

    Sample.SetTime(@Start, @Stop);
    Inc(FFrameNumber);

    // Set TRUE on every sample for uncompressed frames
    Sample.SetSyncPoint(True);

    Result := S_OK;

  finally
    FSharedState.UnLock;
  end;
end;

function TBCPushPinDesktop.Notify(Filter: IBaseFilter; q: TQuality): HRESULT;
begin
  Result := E_FAIL;
end;

// --- TBCPushSourceBitmap ------------

constructor TBCPushSourceDesktop.Create(ObjName: string; Unk: IUnKnown;
  out hr: HRESULT);
begin
  inherited Create(ObjName, Unk, CLSID_PushSourceDesktop);

  // The pin magically adds itself to our pin array.
  FPin := TBCPushPinDesktop.Create(hr, Self);

  if (hr <> S_OK) then
    if (FPin = nil) then
      hr := E_OUTOFMEMORY;
end;

constructor TBCPushSourceDesktop.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TBCPushSourceDesktop.Destroy;
begin
  FreeAndNil(FPin);
  inherited;
end;

initialization
  // provide entries in the CFactoryTemplate array
  TBCClassFactory.CreateFilter(TBCPushSourceBitmap, PushBitmapName,
    CLSID_PushSourceBitmap, CLSID_LegacyAmFilterCategory,
    MERIT_DO_NOT_USE, 1, @sudOutputPinBitmap
    );
  TBCClassFactory.CreateFilter(TBCPushSourceBitmapSet, PushBitmapSetName,
    CLSID_PushSourceBitmapSet, CLSID_LegacyAmFilterCategory,
    MERIT_DO_NOT_USE, 1, @sudOutputPinBitmapSet
    );
  TBCClassFactory.CreateFilter(TBCPushSourceDesktop, PushDesktopName,
    CLSID_PushSourceDesktop, CLSID_LegacyAmFilterCategory,
    MERIT_DO_NOT_USE, 1, @sudOutputPinDesktop
    );
end.

