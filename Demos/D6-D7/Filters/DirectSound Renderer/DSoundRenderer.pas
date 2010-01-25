
    (*********************************************************************
     *                                                                   *
     * The contents of this file are used with permission, subject to    *
     * the Mozilla Public License Version 1.1 (the "License"); you may   *
     * not use this file except in compliance with the License. You may  *
     * obtain a copy of the License at                                   *
     * http://www.mozilla.org/MPL/MPL-1.1.html                           *
     *                                                                   *
     * Software distributed under the License is distributed on an       *
     * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
     * implied. See the License for the specific language governing      *
     * rights and limitations under the License.                         *
     *                                                                   *
     * (C) 2004 Milenko Mitrovic <dcoder@dsp-worx.de>                    *
     *                                                                   *
     *********************************************************************)

unit DSoundRenderer;

interface
uses
  BaseClass, DirectSound, ActiveX, Math, Windows, SysUtils, Classes,
  DirectShow9, MMSystem, PropSettings, DSoundDevices, DXSUtil;

const
  CLSID_DelphiDSoundRenderer: TGUID = '{093A27B5-3AA1-4375-9389-71EB9D62BA97}';

  PREBUFFER_SIZE_MS = 1500; // Buffersize in MilliSeconds of PreBuffering
  BUFFER_SIZE_MS    = 5000; // Buffersize in MilliSeconds

type
  TDelphiDSoundRenderer = class(TBCBaseRenderer, IReferenceClock, IBasicAudio,
                                IPersist, IMediaFilter, ISpecifyPropertyPages)
  private
    fDispatch : TBCBaseDispatch;
    fFormat : TWaveFormatExtensible;
    fCurrentBufferSize : DWORD;
    fPreBufferSize : DWORD;
    fRefClock : TBCSystemClock;
    fVolume : integer;
    fBalance : integer;
    fDevice : IDirectSound8;
    fBuffer : IDirectSoundBuffer8;
    fPos : DWORD;
    fBufferSize : DWORD;
    (*** Misc methods ***)
    function CreateDirectSoundBuffer(MediaType : PAMMediaType) : HRESULT;
  public
    (*** TBCBaseRenderer methods ***)
    constructor Create(ObjName: String; Unk: IUnknown; out hr : HResult);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;
    function CheckMediaType(MediaType: PAMMediaType): HResult; override;
    function DoRenderSample(MediaSample: IMediaSample): HResult; override;
    function SetMediaType(MediaType: PAMMediaType): HResult; override;
    function Run(StartTime: TReferenceTime): HResult; override; stdcall;
    function Stop: HResult; override; stdcall;
    function Pause: HResult; override; stdcall;
    function Receive(MediaSample: IMediaSample): HResult; override;
    function BeginFlush: HResult; override;
    function PrepareReceive(MediaSample: IMediaSample): HResult; override;
    (*** IDispatch methods ***)
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    (*** IBasicAudio methods ***)
    function put_Volume(lVolume: Longint): HResult; stdcall;
    function get_Volume(out plVolume: Longint): HResult; stdcall;
    function put_Balance(lBalance: Longint): HResult; stdcall;
    function get_Balance(out plBalance: Longint): HResult; stdcall;
    (*** IReferenceClock methods ***)
    function GetTime(out pTime: int64): HResult; stdcall;
    function AdviseTime(rtBaseTime, rtStreamTime: int64; hEvent: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
    function AdvisePeriodic(const rtStartTime, rtPeriodTime: int64; hSemaphore: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
    function Unadvise(dwAdviseCookie: DWORD): HResult; stdcall;
    (*** ISpecifyPropertyPages methods ***)
    function GetPages(out pages: TCAGUID): HResult; stdcall;
  end;

implementation
{*** IDispatch methods *** taken from CBasicAudio *** ctlutil.cpp *************}
function TDelphiDSoundRenderer.GetTypeInfoCount(out Count: Integer): HResult; stdcall;
begin
  Result := fDispatch.GetTypeInfoCount(Count);
end;

function TDelphiDSoundRenderer.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
begin
  Result := fDispatch.GetTypeInfo(IID_IBasicAudio,Index,LocaleID,TypeInfo);
end;

function TDelphiDSoundRenderer.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
begin
  Result := fDispatch.GetIDsOfNames(IID_IBasicAudio,Names,NameCount,LocaleID,DispIDs);
end;

function TDelphiDSoundRenderer.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
var
  pti : ITypeInfo;
begin
  if not IsEqualGUID(GUID_NULL,IID) then
  begin
    Result := DISP_E_UNKNOWNINTERFACE;
    Exit;
  end;

  Result := GetTypeInfo(0, LocaleID, pti);

  if FAILED(Result) then Exit;

  Result :=  pti.Invoke(Pointer(Self as IBasicAudio),DispID,Flags,
                        TDispParams(Params),VarResult,ExcepInfo,ArgErr);
  pti := nil;
end;
{*** IBasicAudio methods ******************************************************}
function TDelphiDSoundRenderer.put_Volume(lVolume: Longint): HResult; stdcall;
begin
  fVolume := EnsureRange(lVolume,-10000,0);
  if Assigned(fBuffer) then fBuffer.SetVolume(fVolume);
  Result := S_OK;
end;

function TDelphiDSoundRenderer.get_Volume(out plVolume: Longint): HResult; stdcall;
begin
  plVolume := fVolume;
  Result := S_OK;
end;

function TDelphiDSoundRenderer.put_Balance(lBalance: Longint): HResult; stdcall;
begin
  fBalance := EnsureRange(lBalance,-10000,10000);
  if Assigned(fBuffer) then fBuffer.SetPan(fBalance);
  Result := S_OK;
end;

function TDelphiDSoundRenderer.get_Balance(out plBalance: Longint): HResult; stdcall;
begin
  plBalance := fBalance;
  Result := S_OK;
end;
{*** IReferenceClock methods **************************************************}
function TDelphiDSoundRenderer.GetTime(out pTime: int64): HResult; stdcall;
begin
  Result := fRefClock.GetTime(pTime);
end;

function TDelphiDSoundRenderer.AdviseTime(rtBaseTime, rtStreamTime: int64; hEvent: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
begin
  Result := fRefClock.AdviseTime(rtBaseTime,rtStreamTime,hEvent,pdwAdviseCookie);
end;

function TDelphiDSoundRenderer.AdvisePeriodic(const rtStartTime, rtPeriodTime: int64; hSemaphore: THandle; out pdwAdviseCookie: DWORD): HResult; stdcall;
begin
  Result := fRefClock.AdvisePeriodic(rtStartTime,rtPeriodTime,hSemaphore,pdwAdviseCookie);
end;

function TDelphiDSoundRenderer.Unadvise(dwAdviseCookie: DWORD): HResult; stdcall;
begin
  Result := fRefClock.Unadvise(dwAdviseCookie);
end;
{*** ISpecifyPropertyPages methods ********************************************}
function TDelphiDSoundRenderer.GetPages(out pages: TCAGUID): HResult; stdcall;
begin
  Pages.cElems := 1;
  Pages.pElems := CoTaskMemAlloc(sizeof(TGUID) * Pages.cElems);
  if (Pages.pElems = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  Pages.pElems^[0] := CLSID_PropPageSettings;
  Result := S_OK;
end;
{*** TBCBaseRenderer methods **************************************************}
constructor TDelphiDSoundRenderer.Create(ObjName: String; Unk: IUnknown; out hr: HResult);
var
  Res : HRESULT;
  Guid : TGUID;
begin
  inherited Create(CLSID_DelphiDSoundRenderer, 'Delphi DirectSound Renderer', Unk, hr);
  Guid := GetSavedDevice;
  Res := DirectSoundCreate8(@Guid,fDevice,nil);
  if (Res <> DS_OK) or not Assigned(fDevice) then
  begin
    hr := E_FAIL;
    Exit;
  end;
  fDevice.SetCooperativeLevel(GetDesktopWindow,DSSCL_PRIORITY);
  fDispatch := TBCBaseDispatch.Create;
  fRefClock := TBCSystemClock.Create('TBCSystemClock',nil,hr);
  fPos := 0;
  fVolume := 0;
  fBalance := 0;
  fCurrentBufferSize := 0;
end;

constructor TDelphiDSoundRenderer.CreateFromFactory(Factory: TBCClassFactory;  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TDelphiDSoundRenderer.Destroy;
begin
  if Assigned(fBuffer) then fBuffer := nil;
  if Assigned(fDevice) then fDevice := nil;
  if Assigned(fRefClock) then FreeAndNil(fRefClock);
  if Assigned(fDispatch) then FreeAndNil(fDispatch);
  inherited Destroy;
end;

function TDelphiDSoundRenderer.PrepareReceive(MediaSample: IMediaSample): HResult;
begin
  Result := inherited PrepareReceive(MediaSample);
  FSignalTime := FSignalTime + (PREBUFFER_SIZE_MS * UNITS div MILLISECONDS);
end;

function TDelphiDSoundRenderer.Receive(MediaSample: IMediaSample): HResult;
var
  lStatus : DWORD;
  mi, mo : TReferenceTime;
  lDelayMS : Int64;
begin
  if fPreBufferSize > fCurrentBufferSize then
  begin
    MediaSample.SetTime(nil,nil);
    fCurrentBufferSize := fCurrentBufferSize + int64(MediaSample.GetActualDataLength);
    Result := inherited Receive(MediaSample);
    Exit;
  end;

  if MediaSample.GetTime(mi,mo) = S_OK then
  begin
    lDelayMS := fCurrentBufferSize * UNITS div fFormat.Format.nAvgBytesPerSec;
    mo := mo - lDelayMS;
    mi := mi - lDelayMS;
    MediaSample.SetTime(@mi,@mo);
  end;

  Result := inherited Receive(MediaSample);
  if fPreBufferSize < fCurrentBufferSize then
  begin
    if Assigned(fBuffer) then
    begin
      fBuffer.GetStatus(lStatus);
      if lStatus and DSBSTATUS_PLAYING = 0 then
      begin
        if (FState = State_Running) and Assigned(fBuffer)
          then fBuffer.Play(0,0,DSBPLAY_LOOPING);
      end;
    end;
  end;
end;

function TDelphiDSoundRenderer.BeginFlush: HResult;
begin
  fBuffer.Stop;
  fPos := 0;
  fBuffer.SetCurrentPosition(0);
  fCurrentBufferSize := 0;
  Result := inherited BeginFlush;
end;

function TDelphiDSoundRenderer.Run(StartTime: TReferenceTime): HResult;
begin
  Result := inherited Run(StartTime);
end;

function TDelphiDSoundRenderer.Stop: HResult;
begin
  Result := inherited Stop;
  if Assigned(fBuffer) then fBuffer.Stop;
end;

function TDelphiDSoundRenderer.Pause: HResult;
begin
  Result := inherited Pause;
  if Assigned(fBuffer) then fBuffer.Stop;
end;

function TDelphiDSoundRenderer.CheckMediaType(MediaType: PAMMediaType): HResult;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not IsEqualGUID(MediaType.majortype, MEDIATYPE_Audio) or
     (not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_PCM) and
      not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_IEEE_FLOAT) and
      not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_DOLBY_AC3_SPDIF) and
      not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_RAW_SPORT) and
      not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_SPDIF_TAG_241h) and
      not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_DRM_Audio)) or
     not IsEqualGUID(MediaType.formattype, FORMAT_WaveFormatEx) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := S_OK;
end;

function TDelphiDSoundRenderer.DoRenderSample(MediaSample: IMediaSample): HResult;
var
  pmt : PAMMediaType;
  Buffer : PByte;
  Size : Integer;
  hr : HRESULT;
  p1, p2 : Pointer;
  s1, s2 : Cardinal;
begin
  p1 := nil;
  p2 := nil;
  FLock.Lock;
  try
    if MediaSample.GetMediaType(pmt) = S_OK then
    begin
      hr := CheckMediaType(pmt);
      if hr = S_OK then hr := CreateDirectSoundBuffer(pmt);
      if hr <> DS_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;
      fCurrentBufferSize := 0;
      fPos := 0;
    end;

    MediaSample.GetPointer(Buffer);
    Size := MediaSample.GetActualDataLength;

    if Assigned(fBuffer) then
    begin
      // Lock the Buffer
      hr := fBuffer.Lock(fPos,Size,p1,@s1,p2,@s2,0);

      // If the Buffer is lost, then Restore it and Lock it again
      if hr = DSERR_BUFFERLOST then
      begin
        fBuffer.Restore;
        fBuffer.Lock(fPos,Size,p1,@s1,p2,@s2,0);
      end;

      // store the next WriteCursor Position
      inc(fPos, Size);
      if fPos > fBufferSize then dec(fPos,  fBufferSize);

      // Copy the Buffer from the MediaSample to the Buffer(s)
      Move(Buffer^,p1^,s1);
      if s2 > 0 then Move(PChar(Buffer)[s1],p2^,s2);

      fBuffer.Unlock(p1,s1,p2,s2);
    end;
  finally
    fLock.UnLock;
  end;

  Result := NOERROR;
end;

function TDelphiDSoundRenderer.SetMediaType(MediaType: PAMMediaType): HResult;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := CreateDirectSoundBuffer(MediaType);
end;

function TDelphiDSoundRenderer.CreateDirectSoundBuffer(MediaType : PAMMediaType) : HRESULT;
var
  lDesc : TDSBufferDesc;
  lBuffer : IDirectSoundBuffer;
begin
  fLock.Lock;
  try
    ZeroMemory(@lDesc,SizeOf(TDSBufferDesc));

    if Assigned(fBuffer) then
    begin
      fBuffer.Stop;
      fBuffer := nil;
    end;

    lDesc.dwSize := SizeOf(TDSBufferDesc);
    lDesc.dwFlags := DSBCAPS_GLOBALFOCUS or  DSBCAPS_CTRLFREQUENCY or DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLPAN;

    lDesc.lpwfxFormat := CoTaskMemAlloc(MediaType.cbFormat);
    Move(MediaType.pbFormat^, lDesc.lpwfxFormat^ ,MediaType.cbFormat);
    lDesc.guid3DAlgorithm := GUID_NULL;
    fBufferSize  := PWaveformatEx(MediaType.pbFormat)^.nSamplesPerSec *
                    PWaveformatEx(MediaType.pbFormat)^.nChannels *
                    (PWaveformatEx(MediaType.pbFormat)^.wBitsPerSample div 8) *
                    BUFFER_SIZE_MS div 1000;

    lDesc.dwBufferBytes := fBufferSize;
    fPreBufferSize := PWaveformatEx(MediaType.pbFormat)^.nAvgBytesPerSec * PREBUFFER_SIZE_MS div 1000;

    Result := fDevice.CreateSoundBuffer(lDesc,lBuffer,nil);

    if (Result = DS_OK) and Assigned(lBuffer) then
    begin
      Result := lBuffer.QueryInterface(IID_IDirectSoundBuffer8,fBuffer);
      if (Result = DS_OK) and Assigned(fBuffer) then
      begin
        fBuffer.SetVolume(fVolume);
        fBuffer.SetPan(fBalance);
        fBuffer.GetFormat(@fFormat,SizeOf(fFormat), nil);
      end;
      lBuffer := nil;
    end;
  finally
    fLock.UnLock;
  end;
end;

initialization

  TBCClassFactory.CreateFilter(
    TDelphiDSoundRenderer, 'Delphi DirectSound Renderer', CLSID_DelphiDSoundRenderer,
    CLSID_AudioRendererCategory, MERIT_DO_NOT_USE, 0, nil
  );

end.

