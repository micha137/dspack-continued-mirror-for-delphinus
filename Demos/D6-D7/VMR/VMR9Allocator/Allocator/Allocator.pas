//------------------------------------------------------------------------------
// File: Allocator.h & Allocator.cpp
//
// Desc: DirectShow sample code - interface for the TAllocator class
//
//  Portions created by Microsoft are
//  Copyright (C) 2002 Microsoft Corporation.
//  All Rights Reserved.
//
//  The initial developer of the Pascal code is Henri GOURVEST
//    Email    : hgourvest@progdigy.com
//    WebSite  : http://www.progdigy.com
//------------------------------------------------------------------------------

unit Allocator;

interface
uses Windows, DirectShow9, Direct3D9, SyncObjs, PlaneScene, DSPack;

type
  TAllocator = class(TAbstractAllocator, IVMRSurfaceAllocator9, IVMRImagePresenter9)
  private
    // needed to make this a thread safe object
    FObjectLock            : TCriticalSection;
    Fwindow                : HWND;
    FD3D                   : IDirect3D9;
    FD3DDev                : IDirect3DDevice9;
    FlpIVMRSurfAllocNotify : IVMRSurfaceAllocatorNotify9;
    Fsurfaces              : array of IDirect3DSurface9;
    FrenderTarget          : IDirect3DSurface9;
    FprivateTexture        : IDirect3DTexture9;
    Fscene                 : TPlaneScene;
  protected
    function CreateDevice: HResult;
    // a helper function to erase every surface in the vector
    procedure DeleteSurfaces;
    function NeedToHandleDisplayChange: bool;
    // This function is here so we can catch the loss of surfaces.
    // All the functions are using the FAIL_RET macro so that they exit
    // with the last error code.  When this returns with the surface lost
    // error code we can restore the surfaces.
    function PresentHelper(lpPresInfo: PVMR9PresentationInfo): HRESULT;
  public
    constructor Create(out hr: HResult; wnd: THandle; d3d: IDirect3D9 = nil; d3dd: IDirect3DDevice9 = nil); override;
    destructor Destroy; override;

    // IVMRSurfaceAllocator9
    function InitializeDevice(dwUserID: DWORD; lpAllocInfo: PVMR9AllocationInfo;
      var lpNumBuffers: DWORD): HResult; stdcall;
    function TerminateDevice(dwID: DWORD): HResult; stdcall;
    function GetSurface(dwUserID: DWORD; SurfaceIndex: DWORD; SurfaceFlags: DWORD;
      out lplpSurface: IDirect3DSurface9): HResult; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult; stdcall;

    // IVMRImagePresenter9
    function StartPresenting(dwUserID: DWORD): HResult; stdcall;
    function StopPresenting(dwUserID: DWORD): HResult; stdcall;
    function PresentImage(dwUserID: DWORD; lpPresInfo: PVMR9PresentationInfo): HResult; stdcall;
  end;

implementation

{ TAllocator }

function TAllocator.AdviseNotify(
  lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult;
var
  hr: HResult;
  AMonitor: HMONITOR;
  function FailRet(hr: HResult): boolean;
  begin
    AdviseNotify := hr;
    Result := Failed(hr);
  end;
begin
  FObjectLock.Enter;
  try
    FlpIVMRSurfAllocNotify := lpIVMRSurfAllocNotify;
    AMonitor := FD3D.GetAdapterMonitor(D3DADAPTER_DEFAULT);
    hr := FlpIVMRSurfAllocNotify.SetD3DDevice(FD3DDev, AMonitor);
    result := hr;
  finally
    FObjectLock.Leave;
  end;
end;

constructor TAllocator.Create(out hr: HResult; wnd: THandle; d3d: IDirect3D9 = nil; d3dd: IDirect3DDevice9 = nil);
begin
  FD3D    := d3d;
  FD3DDev := d3dd;
  Fwindow := wnd;
  Fscene := TPlaneScene.Create;
  FObjectLock := TCriticalSection.Create;
  FObjectLock.Enter;
  try
    hr := E_FAIL;
    if not IsWindow(wnd) then
    begin
      hr := E_INVALIDARG;
      exit;
    end;

    if (FD3D = nil) then
    begin
      ASSERT(d3dd =  nil);
      FD3D := Direct3DCreate9(D3D_SDK_VERSION);
      if (FD3D = nil) then
      begin
        hr := E_FAIL;
        exit;
      end;
    end;

    if (FD3DDev = nil) then
      hr := CreateDevice;
  finally
    FObjectLock.Leave;
  end;
end;

function TAllocator.CreateDevice: HResult;
var
  dm: TD3DDisplayMode;
  pp: TD3DPresentParameters;

  function FailRet(hr: HResult): boolean;
  begin
    CreateDevice := hr;
    Result := Failed(hr);
  end;
begin
//    HRESULT hr;
  FD3DDev := nil;

  result := FD3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, dm);
  ZeroMemory(@pp, sizeof(pp));
  pp.Windowed := TRUE;
  pp.hDeviceWindow := Fwindow;
  pp.SwapEffect := D3DSWAPEFFECT_COPY;
  pp.BackBufferFormat := dm.Format;

  if FailRet(FD3D.CreateDevice(D3DADAPTER_DEFAULT,
                               D3DDEVTYPE_HAL,
                               Fwindow,
                               D3DCREATE_SOFTWARE_VERTEXPROCESSING or
                               D3DCREATE_MULTITHREADED,
                               @pp,
                               FD3DDev)) then exit;

  FrenderTarget := nil;
  Result := FD3DDev.GetRenderTarget(0, FrenderTarget);
end;

procedure TAllocator.DeleteSurfaces;
var i: integer;
begin
  FObjectLock.Enter;
  try
    // clear out the private texture
    FprivateTexture := nil;
    for i := 0 to Length(FSurfaces) - 1 do
      FSurfaces[i] := nil;
  finally
    FObjectLock.Leave;
  end;
end;

destructor TAllocator.Destroy;
begin
  DeleteSurfaces;
  Fscene.Free;
  FObjectLock.Destroy;
  inherited;
end;

function TAllocator.GetSurface(dwUserID, SurfaceIndex, SurfaceFlags: DWORD;
  out lplpSurface: IDirect3DSurface9): HResult;
begin
  if (@lplpSurface = nil) then
  begin
    result := E_POINTER;
    Exit;
  end;

  if (SurfaceIndex >= Cardinal(Length(Fsurfaces))) then
  begin
    result := E_FAIL;
    Exit;
  end;

  FObjectLock.Enter;
  try
    try
      lplpSurface := Fsurfaces[SurfaceIndex];
      result := S_OK;
    except
      result := E_FAIL;
    end;
  finally
    FObjectLock.Leave;
  end;
end;

function TAllocator.InitializeDevice(dwUserID: DWORD;
  lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult;
var
  d3dcaps: TD3DCaps9;
  dwWidth: DWORD;
  dwHeight: DWORD;
  fTU: Single;
  fTV: Single;
  hr: HRESULT;
  dm: TD3DDisplayMode;
  function FailRet(hr: HResult): boolean;
  begin
    InitializeDevice := hr;
    Result := Failed(hr);
  end;
begin
  dwWidth  := 1;
  dwHeight := 1;

  if (lpNumBuffers = 0) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if (FlpIVMRSurfAllocNotify = nil) then
  begin
    result := E_FAIL;
    Exit;
  end;

  FD3DDev.GetDeviceCaps(d3dcaps);
  if LongBool(d3dcaps.TextureCaps and D3DPTEXTURECAPS_POW2) then
  begin
    while (dwWidth < lpAllocInfo.dwWidth) do
      dwWidth := dwWidth shl 1;
    while (dwHeight < lpAllocInfo.dwHeight) do
      dwHeight := dwHeight shl 1;
    fTU := (lpAllocInfo.dwWidth) / (dwWidth);
    fTV := (lpAllocInfo.dwHeight) / (dwHeight);
    Fscene.SetSrcRect(fTU, fTV);
    lpAllocInfo.dwWidth := dwWidth;
    lpAllocInfo.dwHeight := dwHeight;
  end;

  // NOTE:
  // we need to make sure that we create textures because
  // surfaces can not be textured onto a primitive.
  lpAllocInfo.dwFlags := lpAllocInfo.dwFlags or VMR9AllocFlag_TextureSurface;

  DeleteSurfaces;
  SetLength(Fsurfaces, lpNumBuffers);
  hr := FlpIVMRSurfAllocNotify.AllocateSurfaceHelper(lpAllocInfo, lpNumBuffers, Fsurfaces[0]);

  // If we couldn't create a texture surface and
  // the format is not an alpha format,
  // then we probably cannot create a texture.
  // So what we need to do is create a private texture
  // and copy the decoded images onto it.
  if (FAILED(hr) and not LongBool(lpAllocInfo.dwFlags and VMR9AllocFlag_3DRenderTarget)) then
  begin
      DeleteSurfaces;

      // is surface YUV ?
      if (lpAllocInfo.Format > D3DFMT_UNKNOWN) then
      begin
          if FailRet(FD3DDev.GetDisplayMode(0, dm)) then exit;

          // create the private texture
          if FailRet(FD3DDev.CreateTexture(lpAllocInfo.dwWidth, lpAllocInfo.dwHeight,
                                  1,
                                  D3DUSAGE_RENDERTARGET,
                                  dm.Format,
                                  D3DPOOL_DEFAULT, // default pool - usually video memory
                                  FprivateTexture, nil)) then exit;
      end;


      lpAllocInfo.dwFlags := lpAllocInfo.dwFlags and not VMR9AllocFlag_TextureSurface;
      lpAllocInfo.dwFlags := lpAllocInfo.dwFlags or VMR9AllocFlag_OffscreenSurface;

      if FailRet(FlpIVMRSurfAllocNotify.AllocateSurfaceHelper(lpAllocInfo, lpNumBuffers, Fsurfaces[0])) then exit;
  end;

  Result := Fscene.Init(FD3DDev);
end;

function TAllocator.NeedToHandleDisplayChange: bool;
var
  Parameters: TD3DDeviceCreationParameters;
  currentMonitor, AMonitor: HMONITOR;
begin
  if (FlpIVMRSurfAllocNotify <> nil) then
  begin
    result := false;
    exit;
  end;

  if (Failed(FD3DDev.GetCreationParameters(Parameters))) then
  begin
    Assert(false);
    result := false;
    exit;
  end;

  currentMonitor := FD3D.GetAdapterMonitor(Parameters.AdapterOrdinal);
  AMonitor := FD3D.GetAdapterMonitor(D3DADAPTER_DEFAULT);
  result := AMonitor <> currentMonitor;
end;

function TAllocator.PresentHelper(
  lpPresInfo: PVMR9PresentationInfo): HRESULT;
var
  surface: IDirect3DSurface9;
  texture: IDirect3DTexture9;
  function FailRet(hr: HResult): boolean;
  begin
    PresentHelper := hr;
    Result := Failed(hr);
  end;
begin
  // parameter validation
  if (lpPresInfo = nil) then
  begin
    result := E_POINTER;
    exit;
  end else
  if (lpPresInfo.lpSurf = nil) then
  begin
    result := E_POINTER;
    exit;
  end;

  FObjectLock.Enter;
  try
    FD3DDev.SetRenderTarget(0, FrenderTarget);
    // if we created a  private texture
    // blt the decoded image onto the texture.
    if(FprivateTexture <> nil) then
    begin
      if FailRet(FprivateTexture.GetSurfaceLevel(0 , surface)) then exit;

      // copy the full surface onto the texture's surface
      if FailRet(FD3DDev.StretchRect(lpPresInfo.lpSurf, nil,
                           surface, nil,
                           D3DTEXF_NONE)) then exit;

      if FailRet(Fscene.DrawScene(FD3DDev, FprivateTexture)) then exit;
    end
    else // this is the case where we have got the textures allocated by VMR
         // all we need to do is to get them from the surface
    begin
      if FailRet(lpPresInfo.lpSurf.GetContainer(IID_IDirect3DTexture9, Pointer(texture))) then exit;
      if FailRet(Fscene.DrawScene(FD3DDev, texture)) then exit;
    end;
    if FailRet(FD3DDev.Present(nil, nil, 0, nil)) then exit;
//    result := hr;
  finally
    Pointer(texture) := nil;
    FObjectLock.leave;
  end;
end;

function TAllocator.PresentImage(dwUserID: DWORD;
  lpPresInfo: PVMR9PresentationInfo): HResult;
var
  hr: HRESULT;
  AMonitor: HMONITOR;
  function FailRet(hr: HResult): boolean;
  begin
    PresentImage := hr;
    Result := Failed(hr);
  end;
begin
  FObjectLock.Enter;
  try
    // if we are in the middle of the display change
    if NeedToHandleDisplayChange then
    begin
        // NOTE: this piece of code is left as a user exercise.
        // The D3DDevice here needs to be switched
        // to the device that is using another adapter
    end;

    hr := PresentHelper(lpPresInfo);

    // IMPORTANT: device can be lost when user changes the resolution
    // or when (s)he presses Ctrl + Alt + Delete.
    // We need to restore our video memory after that
    if (hr = D3DERR_DEVICELOST) then
    begin
      if (FD3DDev.TestCooperativeLevel = D3DERR_DEVICENOTRESET) then
      begin
        DeleteSurfaces;
        if FailRet(CreateDevice) then exit;
        AMonitor := FD3D.GetAdapterMonitor(D3DADAPTER_DEFAULT);
        if FailRet(FlpIVMRSurfAllocNotify.ChangeD3DDevice(FD3DDev, AMonitor)) then exit;
      end;
      hr := S_OK;
    end;
    result := hr;
  finally
    FObjectLock.Leave;
  end;
end;

function TAllocator.StartPresenting(dwUserID: DWORD): HResult;
begin
  FObjectLock.Enter;
  try
    ASSERT(assigned(FD3DDev));
    if (FD3DDev = nil) then
    begin
      result := E_FAIL;
      exit;
    end;
    result := S_OK;
  finally
    FObjectLock.Leave;
  end;
end;

function TAllocator.StopPresenting(dwUserID: DWORD): HResult;
begin
  result := S_OK;
end;

function TAllocator.TerminateDevice(dwID: DWORD): HResult;
begin
  DeleteSurfaces;
  result := S_OK;
end;

end.
