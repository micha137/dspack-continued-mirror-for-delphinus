
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

unit VidRenderer;

interface

uses
  BaseClass, DirectShow9, Windows, SysUtils, Classes, Forms, ActiveX, Graphics,
  Messages, formRenderer;

const
  CLSID_DelphiVideoRenderer: TGUID = '{DB2CF44E-B672-4F18-B407-9169FE84D1EB}';

  DEFWIDTH = 320;                    // Initial window width
  DEFHEIGHT = 240;                   // Initial window height


type
  TVideoRenderer = class(TBCBaseVideoRenderer, IPersist, IVideoWindow, IDispatch,
                         IBasicVideo, IBasicVideo2, IAMFilterMiscFlags)
  private
    fAutoShow : Boolean;
    fDispatch : TBCBaseDispatch;
    fFormat   : TVideoInfoHeader;
    fRenderer : TfrmRenderer;
  public
    constructor Create(ObjName: String; Unk: IUnknown; out hr : HResult);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;
    function CheckMediaType(MediaType: PAMMediaType): HResult; override;
    function DoRenderSample(MediaSample: IMediaSample): HResult; override;
    procedure OnReceiveFirstSample(MediaSample: IMediaSample); override;
    function SetMediaType(MediaType: PAMMediaType): HResult; override;
    function Active: HResult; override;
    function Inactive: HResult; override;
    (*** IDispatch methods ***)
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    (*** IVideoWindow methods ***)
    function put_Caption(strCaption: WideString): HResult; stdcall;
    function get_Caption(out strCaption: WideString): HResult; stdcall;
    function put_WindowStyle(WindowStyle: Longint): HResult; stdcall;
    function get_WindowStyle(out WindowStyle: Longint): HResult; stdcall;
    function put_WindowStyleEx(WindowStyleEx: Longint): HResult; stdcall;
    function get_WindowStyleEx(out WindowStyleEx: Longint): HResult; stdcall;
    function put_AutoShow(AutoShow: LongBool): HResult; stdcall;
    function get_AutoShow(out AutoShow: LongBool): HResult; stdcall;
    function put_WindowState(WindowState: Longint): HResult; stdcall;
    function get_WindowState(out WindowState: Longint): HResult; stdcall;
    function put_BackgroundPalette(BackgroundPalette: Longint): HResult; stdcall;
    function get_BackgroundPalette(out pBackgroundPalette: Longint): HResult; stdcall;
    function put_Visible(Visible: LongBool): HResult; stdcall;
    function get_Visible(out pVisible: LongBool): HResult; stdcall;
    function put_Left(Left: Longint): HResult; stdcall;
    function get_Left(out pLeft: Longint): HResult; stdcall;
    function put_Width(Width: Longint): HResult; stdcall;
    function get_Width(out pWidth: Longint): HResult; stdcall;
    function put_Top(Top: Longint): HResult; stdcall;
    function get_Top(out pTop: Longint): HResult; stdcall;
    function put_Height(Height: Longint): HResult; stdcall;
    function get_Height(out pHeight: Longint): HResult; stdcall;
    function put_Owner(Owner: OAHWND): HResult; stdcall;
    function get_Owner(out Owner: OAHWND): HResult; stdcall;
    function put_MessageDrain(Drain: OAHWND): HResult; stdcall;
    function get_MessageDrain(out Drain: OAHWND): HResult; stdcall;
    function get_BorderColor(out Color: Longint): HResult; stdcall;
    function put_BorderColor(Color: Longint): HResult; stdcall;
    function get_FullScreenMode(out FullScreenMode: LongBool): HResult; stdcall;
    function put_FullScreenMode(FullScreenMode: LongBool): HResult; stdcall;
    function SetWindowForeground(Focus: Longint): HResult; stdcall;
    function NotifyOwnerMessage(hwnd: Longint; uMsg, wParam, lParam: Longint): HResult; stdcall;
    function SetWindowPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetWindowPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function GetMinIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
    function GetMaxIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
    function GetRestorePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function HideCursor(HideCursor: LongBool): HResult; stdcall;
    function IsCursorHidden(out CursorHidden: LongBool): HResult; stdcall;
    (*** IBasicVideo methods ***)
    function get_AvgTimePerFrame(out pAvgTimePerFrame: TRefTime): HResult; stdcall;
    function get_BitRate(out pBitRate: Longint): HResult; stdcall;
    function get_BitErrorRate(out pBitErrorRate: Longint): HResult; stdcall;
    function get_VideoWidth(out pVideoWidth: Longint): HResult; stdcall;
    function get_VideoHeight(out pVideoHeight: Longint): HResult; stdcall;
    function put_SourceLeft(SourceLeft: Longint): HResult; stdcall;
    function get_SourceLeft(out pSourceLeft: Longint): HResult; stdcall;
    function put_SourceWidth(SourceWidth: Longint): HResult; stdcall;
    function get_SourceWidth(out pSourceWidth: Longint): HResult; stdcall;
    function put_SourceTop(SourceTop: Longint): HResult; stdcall;
    function get_SourceTop(out pSourceTop: Longint): HResult; stdcall;
    function put_SourceHeight(SourceHeight: Longint): HResult; stdcall;
    function get_SourceHeight(out pSourceHeight: Longint): HResult; stdcall;
    function put_DestinationLeft(DestinationLeft: Longint): HResult; stdcall;
    function get_DestinationLeft(out pDestinationLeft: Longint): HResult; stdcall;
    function put_DestinationWidth(DestinationWidth: Longint): HResult; stdcall;
    function get_DestinationWidth(out pDestinationWidth: Longint): HResult; stdcall;
    function put_DestinationTop(DestinationTop: Longint): HResult; stdcall;
    function get_DestinationTop(out pDestinationTop: Longint): HResult; stdcall;
    function put_DestinationHeight(DestinationHeight: Longint): HResult; stdcall;
    function get_DestinationHeight(out pDestinationHeight: Longint): HResult; stdcall;
    function SetSourcePosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetSourcePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function SetDefaultSourcePosition: HResult; stdcall;
    function SetDestinationPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetDestinationPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function SetDefaultDestinationPosition: HResult; stdcall;
    function GetVideoSize(out pWidth, Height: Longint): HResult; stdcall;
    function GetVideoPaletteEntries(StartIndex, Entries: Longint; out pRetrieved: Longint; out pPalette): HResult; stdcall;
    function GetCurrentImage(var BufferSize: Longint; var pDIBImage): HResult; stdcall;
    function IsUsingDefaultSource: HResult; stdcall;
    function IsUsingDefaultDestination: HResult; stdcall;
    (*** IBasicVideo2 methods ***)
    function GetPreferredAspectRatio(out plAspectX, plAspectY: Longint): HResult; stdcall;
    (*** IAMFilterMiscFlags methods ***)
    function GetMiscFlags: ULONG; stdcall;
  end;

implementation

function CheckConnected(Pin : TBCBasePin; out Res : HRESULT) : Boolean;
begin
  if not Pin.IsConnected then
  begin
    Res := VFW_E_NOT_CONNECTED;
    Result := False;
  end else
  begin
    Res := S_OK;
    Result := True;
  end;
end;

constructor TVideoRenderer.Create(ObjName: String; Unk: IUnknown; out hr: HResult);
begin
  inherited Create(CLSID_DelphiVideoRenderer, 'Delphi Video Renderer', Unk, hr);
  fDispatch := TBCBaseDispatch.Create;
  fRenderer := TfrmRenderer.Create(nil);
  fAutoShow := True;
end;

constructor TVideoRenderer.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TVideoRenderer.Destroy;
begin
  if Assigned(fDispatch) then FreeAndNil(fDispatch);
  if Assigned(fRenderer) then FreeAndNil(fRenderer);
  inherited Destroy;
end;

function TVideoRenderer.Active: HResult;
begin
  if fAutoShow then fRenderer.Show;
  Result := inherited Active;
end;

function TVideoRenderer.Inactive: HResult;
begin
  Result := inherited Inactive;
end;

function TVideoRenderer.CheckMediaType(MediaType: PAMMediaType): HResult;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not IsEqualGUID(MediaType.majortype, MEDIATYPE_Video) or
     not IsEqualGUID(MediaType.subtype, MEDIASUBTYPE_RGB24) or
     not IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := NOERROR;
end;

function TVideoRenderer.DoRenderSample(MediaSample: IMediaSample): HResult;
begin
  if (MediaSample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  fRenderer.DoRenderSample(MediaSample);
  Result := NOERROR;
end;

procedure TVideoRenderer.OnReceiveFirstSample(MediaSample: IMediaSample);
begin
  DoRenderSample(MediaSample);
end;

function TVideoRenderer.SetMediaType(MediaType: PAMMediaType): HResult;
var
  VIH: PVIDEOINFOHEADER;
begin
  if (MediaType = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  VIH := PVIDEOINFOHEADER(MediaType.pbFormat);
  if (VIH = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  CopyMemory(@fFormat,VIH,SizeOf(TVideoInfoHeader));
  fRenderer.DoInitializeDirectDraw(@fFormat);
  Result := S_OK;
end;
{*** IDispatch methods *** taken from CBaseVideoWindow *** ctlutil.cpp ********}
function TVideoRenderer.GetTypeInfoCount(out Count: Integer): HResult; stdcall;
begin
  Result := fDispatch.GetTypeInfoCount(Count);
end;

function TVideoRenderer.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
begin
  Result := fDispatch.GetTypeInfo(IID_IVideoWindow,Index,LocaleID,TypeInfo);
end;

function TVideoRenderer.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
begin
  Result := fDispatch.GetIDsOfNames(IID_IVideoWindow,Names,NameCount,LocaleID,DispIDs);
end;

function TVideoRenderer.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
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

  Result :=  pti.Invoke(Pointer(Self as IVideoWindow),DispID,Flags,
                        TDispParams(Params),VarResult,ExcepInfo,ArgErr);
  pti := nil;
end;
(*** IVideoWindow methods *****************************************************)
function TVideoRenderer.put_Caption(strCaption: WideString): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  fRenderer.Caption := strCaption;
end;

function TVideoRenderer.get_Caption(out strCaption: WideString): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  strCaption := fRenderer.Caption;
end;

function TVideoRenderer.put_WindowStyle(WindowStyle: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // These styles cannot be changed dynamically
  if (Bool(WindowStyle and WS_DISABLED) or
      Bool(WindowStyle and WS_ICONIC) or
      Bool(WindowStyle and WS_MAXIMIZE) or
      Bool(WindowStyle and WS_MINIMIZE) or
      Bool(WindowStyle and WS_HSCROLL) or
      Bool(WindowStyle and WS_VSCROLL)) then
      begin
        Result := E_INVALIDARG;
        Exit;
      end;
      
  Result := fRenderer.DoSetWindowStyle(WindowStyle,GWL_STYLE);
end;

function TVideoRenderer.get_WindowStyle(out WindowStyle: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := fRenderer.DoGetWindowStyle(WindowStyle,GWL_STYLE);
end;

function TVideoRenderer.put_WindowStyleEx(WindowStyleEx: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Should we be taking off WS_EX_TOPMOST
  if (GetWindowLong(fRenderer.Handle,GWL_EXSTYLE) and WS_EX_TOPMOST > 0) then
  begin
    if ((WindowStyleEx and WS_EX_TOPMOST) = 0) then
    begin
//      SendMessage(fRenderer.Handle,m_ShowStageTop,WPARAM(FALSE),0);
    end;
  end;

  // Likewise should we be adding WS_EX_TOPMOST
  if (WindowStyleEx and WS_EX_TOPMOST > 0) then
  begin
//    SendMessage(m_hwnd,m_ShowStageTop,(WPARAM) TRUE,(LPARAM) 0);
    WindowStyleEx := WindowStyleEx and not WS_EX_TOPMOST;
    if (WindowStyleEx = 0) then
    begin
      Result := NOERROR;
      Exit;
    end;
  end;

  Result := fRenderer.DoSetWindowStyle(WindowStyleEx,GWL_EXSTYLE);
end;

function TVideoRenderer.get_WindowStyleEx(out WindowStyleEx: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := fRenderer.DoGetWindowStyle(WindowStyleEx,GWL_EXSTYLE);
end;

function TVideoRenderer.put_AutoShow(AutoShow: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  fAutoShow := AutoShow;
end;

function TVideoRenderer.get_AutoShow(out AutoShow: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  AutoShow := fAutoShow;
end;

function TVideoRenderer.put_WindowState(WindowState: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := fRenderer.DoShowWindow(WindowState);
end;

function TVideoRenderer.get_WindowState(out WindowState: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  WindowState := 0;

  // Is the window visible, a window is termed visible if it is somewhere on
  // the current desktop even if it is completely obscured by other windows
  // so the flag is a style for each window set with the WS_VISIBLE bit

  if fRenderer.Visible then
  begin
    // Is the base window iconic
    if IsIconic(fRenderer.Handle) then
    begin
      WindowState := WindowState or SW_MINIMIZE;
    end
    // Has the window been maximised
    else if IsZoomed(fRenderer.Handle) then
    begin
      WindowState := WindowState or SW_MAXIMIZE;
    end
    // Window is normal
    else
    begin
      WindowState := WindowState or SW_SHOW;
    end

  end else
  begin
    WindowState := WindowState or SW_HIDE;
  end;
  Result := NOERROR;
end;

function TVideoRenderer.put_BackgroundPalette(BackgroundPalette: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_BackgroundPalette(out pBackgroundPalette: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_Visible(Visible: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  fRenderer.Visible := Visible;
end;

function TVideoRenderer.get_Visible(out pVisible: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pVisible := fRenderer.Visible;
end;

function TVideoRenderer.put_Left(Left: Longint): HResult; stdcall;
var
  bSuccess : Boolean;
  WindowRect : TRect;
  WindowFlags : Cardinal;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Get the current window position in a RECT
  GetWindowRect(fRenderer.Handle,WindowRect);

  if (fRenderer.ParentWindow > 0) then
    MapWindowPoints(HWND_DESKTOP, fRenderer.ParentWindow, WindowRect, 2);

  // Adjust the coordinates ready for SetWindowPos, the window rectangle we
  // get back from GetWindowRect is in left,top,right and bottom while the
  // coordinates SetWindowPos wants are left,top,width and height values

  WindowRect.bottom := WindowRect.bottom - WindowRect.top;
  WindowRect.right := WindowRect.right - WindowRect.left;
  WindowFlags := SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOACTIVATE;

  bSuccess := SetWindowPos(fRenderer.Handle,                // Window handle
                           HWND_TOP,              // Put it at the top
                           Left,                  // New left position
                           WindowRect.top,        // Leave top alone
                           WindowRect.right,      // The WIDTH (not right)
                           WindowRect.bottom,     // The HEIGHT (not bottom)
                           WindowFlags);          // Show window options

  if not bSuccess then Result := E_INVALIDARG
                  else Result := NOERROR;
end;

function TVideoRenderer.get_Left(out pLeft: Longint): HResult; stdcall;
var
  WindowRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  GetWindowRect(fRenderer.Handle,WindowRect);
  pLeft := WindowRect.left;
  Result := S_OK;
end;

function TVideoRenderer.put_Width(Width: Longint): HResult; stdcall;
var
  bSuccess : Boolean;
  WindowRect : TRect;
  WindowFlags : Cardinal;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Adjust the coordinates ready for SetWindowPos, the window rectangle we
  // get back from GetWindowRect is in left,top,right and bottom while the
  // coordinates SetWindowPos wants are left,top,width and height values

  GetWindowRect(fRenderer.Handle,WindowRect);

  if (fRenderer.ParentWindow > 0)
    then MapWindowPoints(HWND_DESKTOP, fRenderer.ParentWindow, WindowRect, 2);


  WindowRect.bottom := WindowRect.bottom - WindowRect.top;
  WindowFlags := SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOACTIVATE;

    // This seems to have a bug in that calling SetWindowPos on a window with
    // just the width changing causes it to ignore the width that you pass in
    // and sets it to a mimimum value of 110 pixels wide (Windows NT 3.51)

  bSuccess := SetWindowPos(fRenderer.Handle,                // Window handle
                           HWND_TOP,              // Put it at the top
                           WindowRect.left,       // Leave left alone
                           WindowRect.top,        // Leave top alone
                           Width,                 // New WIDTH dimension
                           WindowRect.bottom,     // The HEIGHT (not bottom)
                           WindowFlags);          // Show window options

  if not bSuccess then Result := E_INVALIDARG
                  else Result := NOERROR;
end;

function TVideoRenderer.get_Width(out pWidth: Longint): HResult; stdcall;
var
  WindowRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  GetWindowRect(fRenderer.Handle,WindowRect);
  pWidth := WindowRect.right - WindowRect.left;
  Result := NOERROR;
end;

function TVideoRenderer.put_Top(Top: Longint): HResult; stdcall;
var
  bSuccess : Boolean;
  WindowRect : TRect;
  WindowFlags : Cardinal;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Get the current window position in a RECT
  GetWindowRect(fRenderer.Handle,WindowRect);

  if (fRenderer.ParentWindow > 0) then
     MapWindowPoints(HWND_DESKTOP, fRenderer.ParentWindow, WindowRect, 2);


  // Adjust the coordinates ready for SetWindowPos, the window rectangle we
  // get back from GetWindowRect is in left,top,right and bottom while the
  // coordinates SetWindowPos wants are left,top,width and height values

  WindowRect.bottom := WindowRect.bottom - WindowRect.top;
  WindowRect.right := WindowRect.right - WindowRect.left;
  WindowFlags := SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOACTIVATE;

  bSuccess := SetWindowPos(fRenderer.Handle,                // Window handle
                           HWND_TOP,              // Put it at the top
                           WindowRect.left,       // Leave left alone
                           Top,                   // New top position
                           WindowRect.right,      // The WIDTH (not right)
                           WindowRect.bottom,     // The HEIGHT (not bottom)
                           WindowFlags);          // Show window flags

  if not bSuccess then Result := E_INVALIDARG
                  else Result := NOERROR;
end;

function TVideoRenderer.get_Top(out pTop: Longint): HResult; stdcall;
var
  WindowRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  GetWindowRect(fRenderer.Handle,WindowRect);
  pTop := WindowRect.Top;
  Result := NOERROR;
end;

function TVideoRenderer.put_Height(Height: Longint): HResult; stdcall;
var
  bSuccess : Boolean;
  WindowRect : TRect;
  WindowFlags : Cardinal;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Adjust the coordinates ready for SetWindowPos, the window rectangle we
  // get back from GetWindowRect is in left,top,right and bottom while the
  // coordinates SetWindowPos wants are left,top,width and height values

  GetWindowRect(fRenderer.Handle,WindowRect);

  if (fRenderer.ParentWindow > 0) then
     MapWindowPoints(HWND_DESKTOP, fRenderer.ParentWindow, WindowRect, 2);

  WindowRect.right := WindowRect.right - WindowRect.left;
  WindowFlags := SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOACTIVATE;

  bSuccess := SetWindowPos(fRenderer.Handle,                // Window handle
                           HWND_TOP,              // Put it at the top
                           WindowRect.left,       // Leave left alone
                           WindowRect.top,        // Leave top alone
                           WindowRect.right,      // The WIDTH (not right)
                           Height,                // New height dimension
                           WindowFlags);          // Show window flags

  if not bSuccess then Result := E_INVALIDARG
                  else Result := NOERROR;
end;

function TVideoRenderer.get_Height(out pHeight: Longint): HResult; stdcall;
var
  WindowRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  GetWindowRect(fRenderer.Handle,WindowRect);
  pHeight := WindowRect.bottom - WindowRect.top;
  Result := NOERROR;
end;

function TVideoRenderer.put_Owner(Owner: OAHWND): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  fRenderer.ParentWindow := Owner;

  // Don't call this with the filter locked
  fRenderer.DoPaintWindow(True);
  {$IFDEF DEBUG}
//  DbgLog(Self,'Changed parent to $' + inttohex(hwndParent,8));
  {$ENDIF}
  Result := NOERROR;
end;

function TVideoRenderer.get_Owner(out Owner: OAHWND): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Owner := fRenderer.ParentWindow;
end;

function TVideoRenderer.put_MessageDrain(Drain: OAHWND): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  fRenderer.MessageDrain := Drain;
end;

function TVideoRenderer.get_MessageDrain(out Drain: OAHWND): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Drain := fRenderer.MessageDrain;
end;

function TVideoRenderer.get_BorderColor(out Color: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_BorderColor(Color: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_FullScreenMode(out FullScreenMode: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_FullScreenMode(FullScreenMode: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.SetWindowForeground(Focus: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  SendMessage(fRenderer.Handle,WM_SHOWWINDOW,Focus,0);
end;

function TVideoRenderer.NotifyOwnerMessage(hwnd: Longint; uMsg, wParam, lParam: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Only interested in these Windows messages
  case uMsg of
    WM_SYSCOLORCHANGE,
    WM_PALETTECHANGED,
    WM_PALETTEISCHANGING,
    WM_QUERYNEWPALETTE,
    WM_DEVMODECHANGE,
    WM_DISPLAYCHANGE,
    WM_ACTIVATEAPP:
    begin
      // If we do not have an owner then ignore
      if (fRenderer.ParentWindow = 0) then
      begin
        Result := NOERROR;
        Exit;
      end;
      SendMessage(fRenderer.Handle,uMsg,wParam,lParam);
    end;
    // do NOT fwd WM_MOVE. the parameters are the location of the parent
    // window, NOT what the renderer should be looking at.  But we need
    // to make sure the overlay is moved with the parent window, so we
    // do this.
    WM_MOVE: PostMessage(fRenderer.Handle,WM_PAINT,0,0);
  end;
end;

function TVideoRenderer.SetWindowPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
var
  bSuccess : Boolean;
  WindowFlags : Cardinal;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Set the new size and position
  WindowFlags := SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOACTIVATE;

  ASSERT(IsWindow(fRenderer.Handle));
  bSuccess := SetWindowPos(fRenderer.Handle,         // Window handle
                           HWND_TOP,       // Put it at the top
                           Left,           // Left position
                           Top,            // Top position
                           Width,          // Window width
                           Height,         // Window height
                           WindowFlags);   // Show window flags
  ASSERT(bSuccess);
  {$IFDEF DEBUG}
    DbgLog(Self,'SWP failed error : ' + inttohex(GetLastError,8));
  {$ENDIF}
  if not bSuccess then Result := E_INVALIDARG
                  else Result := NOERROR;
end;

function TVideoRenderer.GetWindowPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
var
  WindowRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Get the current window coordinates

  GetWindowRect(fRenderer.Handle,WindowRect);

  // Convert the RECT into left,top,width and height values

  pLeft := WindowRect.left;
  pTop := WindowRect.top;
  pWidth := WindowRect.right - WindowRect.left;
  pHeight := WindowRect.bottom - WindowRect.top;

  Result := NOERROR;
end;

function TVideoRenderer.GetMinIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
var
  State : TFilterState;
  DefaultRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Must not be stopped for this to work correctly
  GetState(0,State);
  if (State = State_Stopped) then
  begin
    Result := VFW_E_WRONG_STATE;
    Exit;
  end;

  DefaultRect := Rect(0,0,DEFWIDTH,DEFHEIGHT);
  pWidth := DefaultRect.Right - DefaultRect.Left;
  pHeight := DefaultRect.Bottom - DefaultRect.Top;
  Result := NOERROR;
end;

function TVideoRenderer.GetMaxIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
var
  State : TFilterState;
  DefaultRect : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Must not be stopped for this to work correctly
  GetState(0,State);
  if (State = State_Stopped) then
  begin
    Result := VFW_E_WRONG_STATE;
    Exit;
  end;

  DefaultRect := Rect(0,0,DEFWIDTH,DEFHEIGHT);
  pWidth := DefaultRect.Right - DefaultRect.Left;
  pHeight := DefaultRect.Bottom - DefaultRect.Top;
  Result := NOERROR;
end;

function TVideoRenderer.GetRestorePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
var
  Place : TWindowPlacement;
  WorkArea : TRect;
begin
  if not CheckConnected(FInputPin,Result) then Exit;

  // Use GetWindowPlacement to find the restore position

  Place.length := sizeof(TWindowPlacement);
  GetWindowPlacement(fRenderer.Handle,@Place);

  // We must take into account any task bar present

  if SystemParametersInfo(SPI_GETWORKAREA,0,@WorkArea,0) then
  begin
    if (fRenderer.ParentWindow = 0) then
    begin
      inc(Place.rcNormalPosition.top,WorkArea.top);
      inc(Place.rcNormalPosition.bottom,WorkArea.top);
      inc(Place.rcNormalPosition.left,WorkArea.left);
      inc(Place.rcNormalPosition.right,WorkArea.left);
    end;
  end;

  // Convert the RECT into left,top,width and height values

  pLeft := Place.rcNormalPosition.left;
  pTop := Place.rcNormalPosition.top;
  pWidth := Place.rcNormalPosition.right - Place.rcNormalPosition.left;
  pHeight := Place.rcNormalPosition.bottom - Place.rcNormalPosition.top;

  Result := NOERROR;
end;

function TVideoRenderer.HideCursor(HideCursor: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.IsCursorHidden(out CursorHidden: LongBool): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;
(*** IBasicVideo methods ******************************************************)
function TVideoRenderer.get_AvgTimePerFrame(out pAvgTimePerFrame: TRefTime): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pAvgTimePerFrame := fFormat.AvgTimePerFrame;
  Result := NOERROR;
end;

function TVideoRenderer.get_BitRate(out pBitRate: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pBitRate := fFormat.dwBitRate;
  Result := NOERROR;
end;

function TVideoRenderer.get_BitErrorRate(out pBitErrorRate: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pBitErrorRate := fFormat.dwBitErrorRate;
  Result := NOERROR;
end;

function TVideoRenderer.get_VideoWidth(out pVideoWidth: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pVideoWidth := fFormat.bmiHeader.biWidth;
  Result := NOERROR;
end;

function TVideoRenderer.get_VideoHeight(out pVideoHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pVideoHeight := fFormat.bmiHeader.biHeight;
  Result := NOERROR;
end;

function TVideoRenderer.put_SourceLeft(SourceLeft: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_SourceLeft(out pSourceLeft: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_SourceWidth(SourceWidth: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_SourceWidth(out pSourceWidth: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_SourceTop(SourceTop: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_SourceTop(out pSourceTop: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_SourceHeight(SourceHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_SourceHeight(out pSourceHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_DestinationLeft(DestinationLeft: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_DestinationLeft(out pDestinationLeft: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_DestinationWidth(DestinationWidth: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_DestinationWidth(out pDestinationWidth: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_DestinationTop(DestinationTop: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_DestinationTop(out pDestinationTop: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.put_DestinationHeight(DestinationHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.get_DestinationHeight(out pDestinationHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.SetSourcePosition(Left, Top, Width, Height: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.GetSourcePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.SetDefaultSourcePosition: HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.SetDestinationPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.GetDestinationPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.SetDefaultDestinationPosition: HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.GetVideoSize(out pWidth, Height: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  pWidth := fFormat.bmiHeader.biWidth;
  Height := fFormat.bmiHeader.biHeight;
  Result := NOERROR;
end;

function TVideoRenderer.GetVideoPaletteEntries(StartIndex, Entries: Longint; out pRetrieved: Longint; out pPalette): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.GetCurrentImage(var BufferSize: Longint; var pDIBImage): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.IsUsingDefaultSource: HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;

function TVideoRenderer.IsUsingDefaultDestination: HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;
(*** IBasicVideo2 methods *****************************************************)
function TVideoRenderer.GetPreferredAspectRatio(out plAspectX, plAspectY: Longint): HResult; stdcall;
begin
  if not CheckConnected(FInputPin,Result) then Exit;
  Result := E_NOTIMPL;
end;
(*** IAMFilterMiscFlags methods ***********************************************)
function TVideoRenderer.GetMiscFlags: ULONG; stdcall;
begin
  Result := AM_FILTER_MISC_FLAGS_IS_RENDERER;
end;
(******************************************************************************)
initialization

  TBCClassFactory.CreateFilter(TVideoRenderer, '_Delphi Video Renderer',
    CLSID_DelphiVideoRenderer, CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE,
    0, nil
  );

end.

