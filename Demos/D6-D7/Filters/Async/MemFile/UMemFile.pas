//------------------------------------------------------------------------------
//
// Desc: DirectShow sample code - header file for application using async
//       filter.
//
// Portions created by Microsoft are
// Copyright (c) 1992 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
unit UMemFile;

interface
uses
  Windows, SysUtils, DSUtil, BaseClass, DirectShow9, MMSystem, Math,
  UAsyncIO, UAsyncRdr;

type
  //
  //  Define an internal filter that wraps the base CBaseReader stuff
  //

  TBCMemFileStream = class(TBCAsyncStream)
  private
    FLock: TBCCritSec;
    FData: PByte;
    FLength: LONGLONG;
    FPosition: LONGLONG;
    FKBPerSec: DWord;
    FTimeStart: DWord;

  public
    constructor Create(AData: PByte; ALength: LONGLONG;
      AKBPerSec: DWord = Infinite);
    destructor Destroy; override;
    function SetPointer(APos: LONGLONG): HResult; override;
    function Read(ABuffer: PByte; ABytesToRead: DWord;
      AAlign: Boolean; out ABytesRead: DWord): HResult; override;
    function Size(out ASizeAvailable: LONGLONG): LONGLONG; override;
    function Alignment: DWord; override;
    procedure Lock; override;
    procedure Unlock; override;
  end;

  TBCMemFileReader = class(TBCAsyncReader)
  public
    //  We're not going to be CoCreate'd so we don't need registration
    //  stuff etc
    function Register: HResult; override; stdcall;

    function UnRegister: HResult; override; stdcall;

    // constructor and destructor
    constructor Create(AStream: TBCMemFileStream; Amt: PAMMediaType;
      out hr: HResult);
  end;

implementation

// --- TBCMemFileStream ---

constructor TBCMemFileStream.Create(AData: PByte; ALength: LONGLONG;
  AKBPerSec: DWord = Infinite);
begin
  FData     := AData;
  FLength   := ALength;
  FPosition := 0;
  FKBPerSec := AKBPerSec;
  FTimeStart:= timeGetTime;

  FLock := TBCCritSec.Create;

  Inherited Create;
end;

destructor TBCMemFileStream.Destroy;
begin
  if Assigned(FLock) then
    FreeAndNil(FLock);

  Inherited Destroy;
end;

function TBCMemFileStream.SetPointer(APos: LONGLONG): HResult;
begin
  if (APos < 0) or (APos > FLength) then
    Result := S_FALSE
  else
    begin
      FPosition := APos;
      Result := S_OK;
    end;
end;

function TBCMemFileStream.Read(ABuffer: PByte; ABytesToRead: DWord;
  AAlign: Boolean; out ABytesRead: DWord): HResult;
var
  _ReadLength, _Time, _TimeToArrive: DWord;
  _Buffer: PByte;
begin
  FLock.Lock;
  try
    //  Wait until the bytes are here!
    _Time := timeGetTime;
    if (FPosition + ABytesToRead > FLength) then
      _ReadLength := FLength - FPosition
    else
      _ReadLength := ABytesToRead;

    if FKBPerSec = 0 then
      _TimeToArrive :=  0
    else
      _TimeToArrive := (FPosition + _ReadLength) div FKBPerSec;
    if (_Time - FTimeStart < _TimeToArrive) then
      Sleep(_TimeToArrive - _Time + FTimeStart);

    _Buffer := FData;
    Inc(_Buffer, FPosition);
    CopyMemory(ABuffer, _Buffer, _ReadLength);
    Inc(FPosition, _ReadLength);
    ABytesRead := _ReadLength;
    Result := S_OK;
  finally
    FLock.UnLock;
  end;
end;

function TBCMemFileStream.Size(out ASizeAvailable: LONGLONG): LONGLONG;
var
  _CurrentAvailable: LONGLONG;
begin
  _CurrentAvailable :=
      Int32x32To64((timeGetTime - FTimeStart), FKBPerSec);

  ASizeAvailable := Min(FLength, _CurrentAvailable);
  Result := FLength;
end;

function TBCMemFileStream.Alignment: DWord;
begin
  Result := 1;
end;

procedure TBCMemFileStream.Lock;
begin
  FLock.Lock;
end;

procedure TBCMemFileStream.Unlock;
begin
  FLock.UnLock;
end;

// --- TBCMemFileReader ---

constructor TBCMemFileReader.Create(AStream: TBCMemFileStream;
  Amt: PAMMediaType; out hr: HResult);
begin
  Inherited Create('Mem reader', nil, AStream, hr);

  CopyMemory(@Fmt, Amt, SizeOf(TAMMediaType));
end;

function TBCMemFileReader.Register: HResult;
begin
  Result := S_OK;
end;

function TBCMemFileReader.UnRegister: HResult;
begin
  Result := S_OK;
end;

end.


