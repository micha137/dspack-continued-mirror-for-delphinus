//------------------------------------------------------------------------------
// File: PlaneScene.h & PlaneScene.cpp
//
// Desc: DirectShow sample code - interface for the TPlaneScene class
//
//  Portions created by Microsoft are
//  Copyright (C) 2002 Microsoft Corporation.
//  All Rights Reserved.
//
//  The initial developer of the Pascal code is Henri GOURVEST
//    Email    : hgourvest@progdigy.com
//    WebSite  : http://www.progdigy.com
//------------------------------------------------------------------------------

unit PlaneScene;

interface
uses Direct3D9;
const
  NUM_VERTICES = 4;

type
  TPosition = record
    x,y,z: Single;
  end;

  TCustomVertex = record
    Position: TPosition;
    Color: D3DCOLOR; // The color
    tu, tv: Single;  // The texture coordinates
  end;

  TPlaneScene = class
  private
    Fvertices: array[0..NUM_VERTICES-1] of TCustomVertex;
    FvertexBuffer: IDirect3DVertexBuffer9;
    Ftime: Int64;
  public
    constructor Create;
    function Init(d3ddev: IDirect3DDevice9): HRESULT;
    function DrawScene(d3ddev: IDirect3DDevice9; texture: IDirect3DTexture9): HRESULT;
    procedure SetSrcRect(fTU, fTV: Single);
  end;
implementation
uses Windows, D3DX9;

const
  D3DFVF_CUSTOMVERTEX = D3DFVF_XYZ or D3DFVF_DIFFUSE or D3DFVF_TEX1;

function MakePosition(x, y, z: Single): TPosition;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

{ TPlaneScene }

constructor TPlaneScene.Create;
begin
  Fvertices[0].position := MakePosition(-1.0,  1.0, 0.0); // top left
  Fvertices[1].position := MakePosition(-1.0, -1.0, 0.0); // bottom left
  Fvertices[2].position := MakePosition( 1.0,  1.0, 0.0); // top right
  Fvertices[3].position := MakePosition( 1.0, -1.0, 0.0); // bottom right

  // set up diffusion:
  Fvertices[0].color := $ffffffff;
  Fvertices[1].color := $ff0000ff;
  Fvertices[2].color := $ffffffff;
  Fvertices[3].color := $ff0000ff;

  // set up texture coordinates
  Fvertices[0].tu := 0.0; Fvertices[0].tv := 0.0; // low left
  Fvertices[1].tu := 0.0; Fvertices[1].tv := 1.0; // high left
  Fvertices[2].tu := 1.0; Fvertices[2].tv := 0.0; // low right
  Fvertices[3].tu := 1.0; Fvertices[3].tv := 1.0; // high right
end;

function TPlaneScene.DrawScene(d3ddev: IDirect3DDevice9;
  texture: IDirect3DTexture9): HRESULT;
  function FailRet(hr: HResult): boolean;
  begin
    DrawScene := hr;
    Result := Failed(hr);
  end;
var
  dwCurrentTime: DWord;
  difference: Int64;
  x, y, z: Single;
  mask0, mask3: DWord;
  pData: Pointer;
begin
  if ((d3ddev = nil) or (texture = nil)) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if( FvertexBuffer = nil) then
  begin
    Result := D3DERR_INVALIDCALL;
    Exit;
  end;

  // get the difference in time
  dwCurrentTime := GetTickCount;
  difference := Ftime - dwCurrentTime;

  // figure out the rotation of the plane
  x := -cos(difference / 2000);
  y :=  cos(difference / 2000);
  z :=  sin(difference / 2000);

  // update the two rotating vertices with the new position
  Fvertices[0].position := MakePosition(x,   y,  z); // top left
  Fvertices[3].position := MakePosition(-x, -y, -z); // bottom right

  // Adjust the color so the blue is always on the bottom.
  // As the corner approaches the bottom, get rid of all the other
  // colors besides blue
  mask0 := Trunc((255 * (( y + 1) / 2)));
  mask3 := Trunc((255 * ((-y + 1) / 2)));
  Fvertices[0].color := $ff0000ff or (mask0 shl 16) or (mask0 shl 8);
  Fvertices[3].color := $ff0000ff or (mask3 shl 16) or (mask3 shl 8);

  // write the new vertex information into the buffer
  if FailRet(FvertexBuffer.Lock(0, sizeof(pData), pData, 0)) then exit;
  move(Fvertices, pData^ , sizeof(Fvertices));
  if FailRet(FvertexBuffer.Unlock) then exit;

  // clear the scene so we don't have any articats left
  d3ddev.Clear(0, nil, D3DCLEAR_TARGET, D3DCOLOR_XRGB(255,255,255), 1.0, 0);

  if FailRet(d3ddev.BeginScene) then exit;
  if FailRet(d3ddev.SetTexture(0, texture)) then exit;

  if FailRet(d3ddev.SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE)) then exit;
  if FailRet(d3ddev.SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE)) then exit;
  if FailRet(d3ddev.SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE)) then exit;
  if FailRet(d3ddev.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE)) then exit;

  if FailRet(d3ddev.SetStreamSource(0, FvertexBuffer, 0, sizeof(TCustomVertex))) then exit;            //set next source ( NEW )
  if FailRet(d3ddev.SetFVF(D3DFVF_CUSTOMVERTEX)) then exit;
  if FailRet(d3ddev.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2)) then exit;  //draw quad
  if FailRet(d3ddev.SetTexture(0, nil)) then exit;
  if FailRet(d3ddev.EndScene) then exit;
end;

function TPlaneScene.Init(d3ddev: IDirect3DDevice9): HRESULT;
  function FailRet(hr: HResult): boolean;
  begin
    Init := hr;
    Result := Failed(hr);
  end;
var
  backBuffer: IDirect3DSurface9;
  backBufferDesc: TD3DSurfaceDesc;
  matProj, matView: TD3DXMatrix;
  fAspect: Single;
  from, at, up: TD3DXVector3;
begin
  if(d3ddev = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if FailRet(d3ddev.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE)) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_LIGHTING, Cardinal(FALSE))) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_ALPHABLENDENABLE, Cardinal(TRUE))) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA)) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA)) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_ALPHATESTENABLE, Cardinal(TRUE))) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_ALPHAREF, $10)) then exit;
  if FailRet(d3ddev.SetRenderState(D3DRS_ALPHAFUNC, D3DCMP_GREATER)) then exit;

  if FailRet(d3ddev.SetSamplerState(0, D3DSAMP_ADDRESSU, D3DTADDRESS_CLAMP)) then exit;
  if FailRet(d3ddev.SetSamplerState(0, D3DSAMP_ADDRESSV, D3DTADDRESS_CLAMP)) then exit;
  if FailRet(d3ddev.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR)) then exit;
  if FailRet(d3ddev.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR)) then exit;
  if FailRet(d3ddev.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR)) then exit;

  FvertexBuffer := nil;

  d3ddev.CreateVertexBuffer(sizeof(Fvertices),D3DUSAGE_WRITEONLY,D3DFVF_CUSTOMVERTEX,D3DPOOL_MANAGED, FvertexBuffer, nil);

  if FailRet(d3ddev.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, backBuffer)) then exit;

  backBuffer.GetDesc(backBufferDesc);

  // Set the projection matrix

  fAspect := backBufferDesc.Width / backBufferDesc.Height;
  D3DXMatrixPerspectiveFovLH(matProj, D3DX_PI/4, fAspect, 1.0, 100.0);
  if FailRet(d3ddev.SetTransform(D3DTS_PROJECTION, matProj)) then exit;


  from := D3DXVECTOR3(1.0, 1.0, -3.0);
  at   := D3DXVECTOR3(0.0, 0.0,  0.0);
  up   := D3DXVECTOR3(0.0, 1.0,  0.0);

  D3DXMatrixLookAtLH(matView, from, at, up);
  if FailRet(d3ddev.SetTransform(D3DTS_VIEW, matView)) then exit;

  Ftime := GetTickCount;
end;

procedure TPlaneScene.SetSrcRect(fTU, fTV: Single);
begin
  Fvertices[0].tu := 0.0; Fvertices[0].tv := 0.0; // low left
  Fvertices[1].tu := 0.0; Fvertices[1].tv := fTV; // high left
  Fvertices[2].tu := fTU; Fvertices[2].tv := 0.0; // low right
  Fvertices[3].tu := fTU; Fvertices[3].tv := fTV; // high right
end;

end.
