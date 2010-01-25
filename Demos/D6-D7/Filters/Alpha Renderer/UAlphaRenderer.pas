//------------------------------------------------------------------------------
// File:              UAlphaRenderer.pas
// Original file(s): AlphaRenderer.h, AlphaRenderer.c
//
// Desc: DirectShow sample filter: Alpha Renderer
//
// Portions created by Microsoft are
// Copyright (c) Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
{
  @abstract(Alpha Renderer Sample Filter from DS SDK)
  @author(Andriy Nevhasymyy: a.n@email.com)
  @created(Aug 11, 2003)
  @lastmod(Aug 12, 2003)
}

unit UAlphaRenderer;

interface
uses
  BaseClass, DirectShow9,
  Windows, SysUtils, Classes, ActiveX;

const
  CLSID_AlphaRenderer: TGUID = '{A63A8661-EEB3-4036-9964-EACD50973E4F}';

type
  TBCAlphaRenderer = class(TBCBaseRenderer)
  private
    FWnd      : HWND;
    FDC       : HDC;
    FWidth    : Integer;
    FHeight   : Integer;
    FBmi      : TBITMAPINFOHEADER;
    FCheckers : PDWord;

    procedure _Clear;

  public
    constructor Create(ObjName: String; Unk: IUnknown; out hr : HResult);
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;

    destructor Destroy; override;

    // make sure media type is what we want
    //
    function CheckMediaType(MediaType: PAMMediaType): HResult; override;
    // have to ovverride this
    //
    function DoRenderSample(MediaSample: IMediaSample): HResult; override;
    // have to override this
    //
    function SetMediaType(MediaType: PAMMediaType): HResult; override;
    // override these to receive indication of when we change
    // to Pause/Play (Active) or Stop (Inactive) state.
    function Active: HResult; override;
    function Inactive: HResult; override;
  end;

implementation
{$BOOLEVAL OFF}

//
// CreateInstance
//
constructor TBCAlphaRenderer.Create(ObjName: String; Unk: IUnknown;
  out hr: HResult);
begin
  inherited Create(CLSID_AlphaRenderer, 'AlphaRenderer', Unk, hr);
  FWnd      := 0;
  FDC       := 0;
  FWidth    := 0;
  FHeight   := 0;
  FCheckers := nil;
end;

constructor TBCAlphaRenderer.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TBCAlphaRenderer.Destroy;
begin
  OutputDebugString('TBCAlphaRenderer.Destroy');
  _Clear;
  inherited Destroy;
end;

// throw away the window and the checkerboard pattern
//

procedure TBCAlphaRenderer._Clear;
begin
  if (FDC <> 0) then
  begin
    ReleaseDC(FWnd, FDC);
    FDC := 0;
  end;

  if (FWnd <> 0) then
  begin
    DestroyWindow(FWnd);
    FWnd := 0;
  end;

  if Assigned(FCheckers) then
  begin
    FreeMem(FCheckers);
    FCheckers := nil;
  end;
end;

// Called when we go paused or running

function TBCAlphaRenderer.Active: HResult;
begin
  // Make our renderer window visible
  ShowWindow(FWnd, SW_SHOWNORMAL);

  Result := inherited Active;
end;

// Called when we go into a stopped state

function TBCAlphaRenderer.Inactive: HResult;
begin
  // Make our renderer window visible
  ShowWindow(FWnd, SW_HIDE);

  Result := inherited Inactive;
end;

// make sure media type is what we want
//

function TBCAlphaRenderer.CheckMediaType(MediaType: PAMMediaType): HResult;
var
  VIH: PVIDEOINFOHEADER;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // the major type must match
  if (not IsEqualGUID(MediaType.majortype, MEDIATYPE_Video)) or
    // the sub type must match
  (not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_ARGB32)) or
    // the format must match
  (not IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo)) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  VIH := PVIDEOINFOHEADER(MediaType.pbFormat);
  Assert(Assigned(VIH));

  // we could do more here to ensure the image is right-side up
  // by looking at the bitmap info header in the VIDEOINFO struct
  //
  Result := NOERROR;
end;

// have to ovverride this, render the incoming sample onto the checkerboard
//

function TBCAlphaRenderer.DoRenderSample(MediaSample: IMediaSample): HResult;
var
  Bits: PByte;
  len, x: Integer;
  Source, Checkers: PRGBQUAD;
  d: RGBQUAD;

begin
  if (MediaSample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  MediaSample.GetPointer(Bits);

  len := MediaSample.GetActualDataLength;
  Assert(len = FWidth * FHeight * SizeOf(DWord));

  // now blend checkerboard into bits before we blit them

  // the incoming source
  //
  Source := PRGBQUAD(Bits);

  // the checkerboard buffer
  //
  Checkers := PRGBQUAD(FCheckers);

  // blend them
  //
  for x := 0 to FWidth * FHeight - 1 do
  begin
    d.rgbRed := Byte((Source.rgbRed * Source.rgbReserved div 256 +
      Checkers.rgbRed * (256 - Source.rgbReserved) div 256));
    d.rgbGreen := Byte((Source.rgbGreen * Source.rgbReserved div 256 +
      Checkers.rgbGreen * (256 - Source.rgbReserved) div 256));
    d.rgbBlue := Byte((Source.rgbBlue * Source.rgbReserved div 256 +
      Checkers.rgbBlue * (256 - Source.rgbReserved) div 256));

    CopyMemory(Source, @d, SizeOf(RGBQUAD));
    Inc(Source);
    Inc(Checkers);
  end;

  // put the bits into the window
  //
  StretchDIBits(FDC,
    0, 0, FWidth, FHeight,
    0, 0, FWidth, FHeight,
    Bits, PBitmapInfo(@FBMI)^,
    DIB_RGB_COLORS, SRCCOPY);

  Result := NOERROR;
end;

// Must override this. We create the window here
//

function TBCAlphaRenderer.SetMediaType(MediaType: PAMMediaType): HResult;
var
  VIH: PVIDEOINFOHEADER;
  Width, Height, x, y: Integer;
  OnOff: Boolean;
  p: PDWord;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  _Clear;

  // we know it's a VIDEOINFOHEADER, since we demanded one
  // in CheckMediaType
  //
  VIH := PVIDEOINFOHEADER(MediaType.pbFormat);
  if (VIH = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  FWidth  := VIH.bmiHeader.biWidth;
  Width   := FWidth;
  FHeight := VIH.bmiHeader.biHeight;
  Height  := FHeight;

  // save this off for lookin' at it later
  //
  CopyMemory(@FBMI, @VIH.bmiHeader, SizeOf(TBitmapInfoHeader));
  //FBMI := @VIH.bmiHeader;

  // create the window
  //
  FWnd := CreateWindow(
    'STATIC', 'Video Renderer',
    WS_POPUP, // NOT Visible
    0, 0, Width, Height,
    0, 0, hInstance, nil
    );

  // get the DC
  //
  FDC := GetDC(FWnd);

  // create a checker buffer
  //
  try
    GetMem(FCheckers, SizeOf(DWord) * Width * Height);
    // draw the checkers
    //
    for x := 0 to Width - 1 do
      for y := 0 to Height - 1 do
      begin
        OnOff := False;

        if (x div 8 mod 2 = 0) then
          OnOff := not OnOff;

        if (y div 8 mod 2 = 0) then
          OnOff := not OnOff;

        p := FCheckers;
        Inc(p, y * Width);
        Inc(p, x);

        if not OnOff then
          p^ := 0
        else
          p^ := $FFFFFF;
      end;
    Result := NOERROR;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

initialization
  // provide an entry in the CFactoryTemplate array
  TBCClassFactory.CreateFilter(TBCAlphaRenderer, '_ AlphaRenderer',
    CLSID_AlphaRenderer, CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE,
    0, nil
    );
end.

