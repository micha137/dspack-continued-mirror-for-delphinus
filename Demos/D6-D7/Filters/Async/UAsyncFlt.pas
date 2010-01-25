//------------------------------------------------------------------------------
// File: UAsyncFlt.pas
// Original files: AsyncFlt.h, AsyncFlt.c
//
// Desc: AsyncFilter implementation
//
// Portions created by Microsoft are
// Copyright (c) 2000-2002  Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
unit UAsyncFlt;

interface
uses
  BaseClass, DirectShow9, Windows, SysUtils, MMSystem, Math, ActiveX,
  UAsyncRdr, UAsyncIo, DSUtil;

const
  // Setup data for filter registration
  sudPinTypes : TRegPinTypes =
  (
  clsMajorType: @MEDIATYPE_Stream;
  clsMinorType: @MEDIASUBTYPE_NULL
  );

  // pins info
  sudpPins : array[0..0] of TRegFilterPins =
  (
   (
    strName: 'Output';         // Pins string name
    bRendered: False;          // Is it rendered
    bOutput: True;             // Is it an output
    bZero: False;              // Are we allowed none
    bMany: False;              // And allowed many
    oFilter: @GUID_NULL;       // Connects to filter
    strConnectsToPin: 'Input'; // Connects to pin
    nMediaTypes: 1;            // Number of types
    lpMediaType: @sudPinTypes  // Pin information
   )
  );

type
  //  NOTE:  This filter does NOT support AVI format

  //
  //  Define an internal filter that wraps the base CBaseReader stuff
  //

  TBCMemStream = class(TBCAsyncStream)
  public
    constructor Create;
    destructor Destroy; override;

    //  Initialization
    procedure Init(AData: PByte; ALength: LONGLONG; AKBPerSec: DWord = INFINITE);

    function SetPointer(APos: LONGLONG): HResult; override;
    function Read(ABuffer: PByte; ABytesToRead: DWord;
      AAlign: Boolean; out ABytesRead: DWord): HResult; override;
    function Size(out ASizeAvailable: LONGLONG): LONGLONG; override;
    function Alignment: DWord; override;
    procedure Lock; override;
    procedure UnLock; override;

  private
    FCSLock: TBCCritSec;
    FData: PByte;
    FLength: LONGLONG;
    FPosition: LONGLONG;
    FKBPerSec: DWord;
    FTimeStart: DWord;
  end;

  TBCAsyncFilter = class(TBCAsyncReader, IFileSourceFilter)
  public
    // construction / destruction
    constructor Create(ObjName: string; Unk: IUnKnown; out hr : HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;

    destructor Destroy; override;

    function NonDelegatingQueryInterface(const IID: TGUID;
      out Obj): HResult; override;

    //  IFileSourceFilter methods

    //  Load a (new) file
    function Load(AFileName: PWideChar; const Amt: PAMMediaType): HResult; stdcall;

    // Modeled on IPersistFile::Load
    // Caller needs to CoTaskMemFree or equivalent.
    function GetCurFile(out AFileName: PWideChar; Amt: PAMMediaType): HResult;
      stdcall;
  private
    FFileName: PWideChar;
    FSize: LONGLONG;
    FData: PByte;
    FStream: TBCMemStream;

    function ReadTheFile(AFileName: PChar): Boolean;
  end;

implementation

// --- TBCMemStream ---

constructor TBCMemStream.Create;
begin
  Inherited;

  FCSLock := TBCCritSec.Create;
  FPosition := 0;
end;

destructor TBCMemStream.Destroy;
begin
  if Assigned(FCSLock) then
    FreeAndNil(FCSLock);

  Inherited Destroy;
end;

procedure TBCMemStream.Init(AData: PByte; ALength: LONGLONG;
  AKBPerSec: DWord = INFINITE);
begin
  FData := AData;
  FLength := ALength;
  FKBPerSec := AKBPerSec;
  FTimeStart := timeGetTime;
end;

function TBCMemStream.SetPointer(APos: LONGLONG): HResult;
begin
  if (APos < 0) or (APos > FLength) then
    Result := S_FALSE
  else
    begin
      FPosition := APos;
      Result := S_OK;
    end;
end;

function TBCMemStream.Read(ABuffer: PByte; ABytesToRead: DWord;
  AAlign: Boolean; out ABytesRead: DWord): HResult;
var
  _ReadLength, _Time, _TimeToArrive: DWord;
  _Data: PByte;
begin
//  Result := E_FAIL;

  FCSLock.Lock;
  try
    //  Wait until the bytes are here!
    _Time := timeGetTime;

    if (FPosition + ABytesToRead > FLength) then
        _ReadLength := FLength - FPosition
    else
      _ReadLength := ABytesToRead;
    _TimeToArrive := (FPosition + _ReadLength) div FKBPerSec;

    if (_Time - FTimeStart < _TimeToArrive) then
      Sleep(_TimeToArrive - _Time + FTimeStart);

    _Data := FData;
    Inc(_Data, FPosition);
    CopyMemory(ABuffer, _Data, _ReadLength);

    Inc(FPosition, _ReadLength);
    ABytesRead := _ReadLength;
    Result := S_OK;

  finally
    FCSLock.UnLock;
  end;
end;

function TBCMemStream.Size(out ASizeAvailable: LONGLONG): LONGLONG;
var
  _CurrentAvailable: LONGLONG;
begin
  _CurrentAvailable := UInt32x32To64(timeGetTime - FTimeStart, FKBPerSec);

  ASizeAvailable := min(FLength, _CurrentAvailable);
  Result := FLength;
end;

function TBCMemStream.Alignment: DWord;
begin
  Result := 1;
end;

procedure TBCMemStream.Lock;
begin
  FCSLock.Lock;
end;

procedure TBCMemStream.UnLock;
begin
  FCSLock.UnLock;
end;

// --- TBCAsyncFilter ---

constructor TBCAsyncFilter.Create(ObjName: string; Unk: IUnKnown; out hr : HRESULT);
begin
  try
    FStream := TBCMemStream.Create;

    Inherited Create(ObjName, Unk, FStream, hr);

    FFileName := '';
    FData := nil;
    hr := NOERROR;
  except
    hr := E_OUTOFMEMORY;
  end;
end;

constructor TBCAsyncFilter.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TBCAsyncFilter.Destroy;
begin
  if Assigned(FData) then
    FreeMem(FData);
  FFileName := '';

  Inherited Destroy;
end;

function TBCAsyncFilter.NonDelegatingQueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IID_IFileSourceFilter) then
    if GetInterface(IID_IFileSourceFilter, Obj) then
      Result := S_OK
    else
      Result := E_FAIL
  else
    Result := Inherited NonDelegatingQueryInterface(IID, Obj);
end;

function TBCAsyncFilter.Load(AFileName: PWideChar;
  const Amt: PAMMediaType): HResult;
var
  cch: Integer;
  {$IFNDEF UNICODE}
  _FileName: PChar;
  {$ELSE}
  _FileName: array[0..MAX_PATH - 1] of Char;
  {$ENDIF}
  _mt: TAMMediaType;
begin
  if (AFileName = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // lstrlenW is one of the few Unicode functions that works on win95
  cch := lstrlenW(AFileName) + 1;

  {$IFNDEF UNICODE}
  try
    _FileName := nil;
    GetMem(_FileName, cch * 2);
  except
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  WideCharToMultiByte(GetACP, 0, AFileName, -1,
    _FileName, cch, nil, nil);
  {$ELSE}
  ZeroMemory(@_FileName[0], MAX_PATH, 0);
  lstrcpy(_FileName, AFileName);
  {$ENDIF}

  FCSFilter.Lock;
  try
    //  Check the file type
    if (Amt = nil) then
    begin
      ZeroMemory(@_mt, SizeOf(TAMMediaType));
      _mt.majortype := MEDIATYPE_Stream;
{$IFDEF AVI}
      _mt.subtype := MEDIASUBTYPE_AVI;
{$ELSE}
      _mt.subtype := MEDIASUBTYPE_NULL;
{$ENDIF}
    end
      else
        CopyMemory(@_mt, Amt, SizeOf(TAMMediaType));

    if Not ReadTheFile(_FileName) then
    begin
      {$IFNDEF UNICODE}
        FreeMem(_FileName);
      {$ENDIF}
      Result := E_FAIL;
      Exit;
    end;

    FStream.Init(FData, FSize);

    try
      GetMem(FFileName, SizeOf(WideChar) * cch);
    except
      Result := E_OUTOFMEMORY;
      Exit;
    end;

    CopyMemory(FFileName, AFileName, cch * SizeOf(WideChar));

    // this is not a simple assignment... pointers and format
    // block (if any) are intelligently copied
    CopyMemory(@Fmt, @_mt, SizeOf(TAMMediaType));

    Fmt.bTemporalCompression := True;
    Fmt.lSampleSize := 1;

    Result := S_OK;
  finally
    FCSFilter.UnLock;
  end;
end;

function TBCAsyncFilter.GetCurFile(out AFileName: PWideChar;
  Amt: PAMMediaType): HResult;
var
  n: DWord;
begin
  AFileName := nil;

  if Assigned(FFileName) then
  begin
    n := SizeOf(WideChar) * (1 + lstrlenW(FFileName));

    AFileName := CoTaskMemAlloc(n);
    if Assigned(AFileName) then
      CopyMemory(AFileName, FFileName, n);
  end;

  if Assigned(Amt) then
    CopyMemory(Amt, @Fmt, SizeOf(TAMMediaType));

  Result := NOERROR;
end;

function TBCAsyncFilter.ReadTheFile(AFileName: PChar): Boolean;
var
  _BytesRead: DWord;
  _File: THandle;
  _Size: ULARGE_INTEGER;
  _Mem: PByte;
begin
  // Open the requested file
  _File := CreateFile(AFileName, GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, 0, 0);
  if (_File = INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF DEBUG}
    DbgLog(Format('Could not open %s', [AFileName]));
    {$ENDIF}
    Result := False;
    Exit;
  end;
  // Determine the file size
  _Size.LowPart := GetFileSize(_File, @_Size.HighPart);

  try
    _Mem := nil;
    GetMem(_Mem, _Size.LowPart);
  except
    CloseHandle(_File);
    Result := False;
    Exit;
  end;

  // Read the data from the file
  if (Not ReadFile(_File, _Mem^, _Size.LowPart, _BytesRead, nil)) or
    (_BytesRead <> _Size.LowPart) then
  begin
    {$IFDEF DEBUG}
    DbgLog(Format('Could not read %s', [AFileName]));
    {$ENDIF}

    FreeMem(_Mem);
    CloseHandle(_File);
    Result := False;
    Exit;
  end;


  // Save a pointer to the data that was read from the file
  FData := _Mem;
  FSize := _Size.QuadPart;

  // Close the file
  CloseHandle(_File);

  Result := True;
end;

initialization
  // provide an entry in the CFactoryTemplate array
  TBCClassFactory.CreateFilter(TBCAsyncFilter,
    StringToOleStr('_ Sample File Source (Async.)'),
    CLSID_AsyncSample, CLSID_LegacyAmFilterCategory, MERIT_UNLIKELY,
    1, @sudpPins);
end.

