unit Filter;

   (*********************************************************************
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
    * (C) 2004-2005 Martin Offenwanger: coder@dsplayer.de               *
    *********************************************************************)
{
@author(Martin Offenwanger: coder@dsplayer.de)
@created(Apr 22, 2004)
@lastmod(Sep 09, 2004)
}

interface

uses
  WorkerThread, ICYParser, ActiveX, Classes, DirectShow9, BaseClass, Windows,
  Config, StringQueue, Forms;

type
  TAsyncEx = class(TBCBaseFilter, IFileSourceFilter, IAsyncExControl)
  private
    // the actual playback location "supported: URL, File, stream"
    FFileName: string;
    // all loaded filtes will be wrapped into a Filestream
    FFilestream: TFileStream;
    // Basepin Object
    FPin: TBCBasePin;
    FLock: TBCCritSec;
    FStreamsize: int64;
    // Pin state flag
    FPinActive: boolean;
    // Prebuffer loop flag
    Fexitloop: boolean;
    // Ripper Flag
    FRipstream: boolean;
    FPath: string;
    FFile: string;
    FState: _FilterState;
    function GetOutPin: IPin;
    // creates a Filter pin if streamEnabled=true (URL or IStream)
    procedure CreateFilterPin(AStream: IStream; StreamEnabled: boolean = false;
      URLPin: boolean = false); overload;
    // helper function
    function ParseUrl(Url: string; out host: string; out port: string;
      out Location: string): boolean;
  public
    constructor Create;
    constructor CreateFromFactory(Factory: TBCClassFactory;
      const Controller: IUnknown); override;
    destructor Destroy; override;
    function GetPin(n: Integer): TBCBasePin; override;
    function GetPinCount: Integer; override;
    function Run(tStart: TReferenceTime): HRESULT; override; stdcall;
    function Stop: HRESULT; virtual; stdcall;
    function Pause: HRESULT; virtual; stdcall;
    function NonDelegatingRelease: Integer; override; stdcall;
    // IFileSourceFilter
    function Load(pszFileName: PWCHAR;
      const pmt: PAMMediaType): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: PWideChar;
      pmt: PAMMediaType): HRESULT; stdcall;
    // IDSPlayerAsyncSourceControl
    function SetLoadFromStream(Stream: IStream; Length: int64): HRESULT;
      stdcall;
    function SetConnectToIp(Host: PChar; Port: PChar; Location: PChar;
      PreBuffersize: integer; MetaData: LongBool): HRESULT; stdcall;
    function SetConnectToURL(URL: PChar; PreBuffersize: integer;
      MetaData: LongBool): HRESULT; stdcall;
    function SetBuffersize(BufferSize: integer): HRESULT; stdcall;
    function GetBuffersize(out BufferSize: integer): HRESULT; stdcall;
    function SetRipStream(Ripstream: LongBool; Path: PChar;
      Filename: PChar): HRESULT; stdcall;
    function GetRipStream(out Ripstream: LongBool;
      out FileO: PChar): HRESULT; stdcall;
    function SetCallBack(CallBack: IAsyncExCallBack): HRESULT; stdcall;
    function FreeCallback(): HRESULT; stdcall;
    function ExitAllLoops(): HRESULT; stdcall;
    // properties
    property OutPin: IPin read GetOutPin;
    // returns current state
    function GetState(MSecs: DWord; out State: TFilterState): HResult;
      override; stdcall;
  end;

implementation

uses
  SysUtils, StreamOutPin;

function TAsyncEx.GetState(MSecs: DWord; out State: TFilterState): HResult; stdcall;
begin
  State := FState;
  Result := S_OK;
end;

function TAsyncEx.ExitAllLoops(): HRESULT; stdcall;
begin
   //     FLock.Lock;
  GFExit := true;
end;

function TAsyncEx.Run(tStart: TReferenceTime): HRESULT; stdcall;
begin
  if (FGRaph <> nil) and (FPin <> nil) and (GFConnected) then
  begin
    TStreamOutPin(FPin).setActiveGraph(FGRaph);
    RESULT := S_OK;
  end
  else
    RESULT := E_FAIL;
  if result = S_OK then
    result := (inherited Run(tStart));
  if result = S_OK then
    FState := State_Running;
end;

function TAsyncEx.Stop: HRESULT; stdcall;
begin
  FState := State_Stopped;
  result := (inherited Stop);
end;

function TAsyncEx.Pause: HRESULT; stdcall;
begin
  FState := State_Paused;
  result := (inherited Stop);
end;

// IDSPlayerAsyncSourceControl begin

function TAsyncEx.SetConnectToURL(URL: PChar; PreBuffersize: integer; MetaData:
  LongBool): HRESULT; stdcall;
var
  Host, Port, Location, URLO: string;
begin
  FLock.lock;
  URLO := copy(URL, 1, length(URL));
  if not ParseUrl(URLO, Host, Port, Location) then
  begin
    result := E_FAIL;
    exit;
  end;
  FLock.unlock;
  result := SetConnectToIp(PChar(Host), PChar(Port), PChar(Location),
    PreBuffersize, MetaData);
end;

function TAsyncEx.SetConnectToIp(Host: PChar; Port: PChar; Location: PChar;
  PreBuffersize: integer; MetaData: LongBool): HRESULT; stdcall;
var
  Datawritten: boolean;
  Application: TApplication;
  i: integer;
  Buffer: string;
  Avdata: int64;
begin
  if GFExit then
  begin
    Result := E_FAIL;
    exit;
  end;
  if GFConnected then
  begin
    Result := E_FAIL;
    exit;
  end;
  try
    FLock.Lock;
    GFPreBufferSize := PreBuffersize;
    GFStringQueue := TStringQueue.Create;
    Datawritten := false;
    Application := TApplication.Create(nil);
    i := 0;
    Avdata := 0;
    Buffer := '';
    GFFileName := 'N/A';
    if GFExit then
    begin
      Result := E_FAIL;
      exit;
    end;
    if GFConnected then
    begin
      Result := E_FAIL;
      exit;
    end;
    CreateFilterPin(TStreamAdapter.Create(nil, soOwned), true, true);
    if FPin <> nil then
      TStreamOutPin(FPin).DoConnect(copy(Host, 0, system.length(Host)),
        copy(Port, 0, system.length(Port)),
        copy(Location, 0, system.length(Location)),
        MetaData);
    SetRipStream(FRipstream, PChar(FPath), PChar(FFile));

    while not Datawritten do
    begin
      if GFExit then
      begin
        Result := E_FAIL;
        FLock.UnLock;
        exit;
      end;
      if g_threadedShoutcastStream = nil then
      begin
        Result := E_FAIL;
        FLock.UnLock;
        exit;
      end;
      if GFStringQueue = nil then
      begin
        Result := E_FAIL;
        FLock.UnLock;
        exit;
      end;
      Sleep(1);
      if GFConnected then
      begin
        Result := E_FAIL;
        FLock.UnLock;
        exit;
      end;

      if (GFFilterCallBack <> nil) and
        (PreBuffersize > 0) and
        (g_threadedShoutcastStream <> nil) then
        GFFilterCallBack.AsyncExFilterState(false, true, false,
          false, (trunc((Avdata * 100) / PreBuffersize)));

      Application.ProcessMessages;

      if GFExit then
      begin
        result := E_FAIL;
        Application.Destroy;
        FLock.UnLock;
        exit;
      end
      else if GFStringQueue = nil then
      begin
        Result := E_FAIL;
        Application.Destroy;
        FLock.UnLock;
        exit;
      end;
      if GFStringQueue.getcount > i then
      begin
        Buffer := Buffer + GFStringQueue.getitem(i);
        inc(i);
      end;
      if (PreBuffersize <= Avdata) then
        Datawritten := true
      else
        Avdata := system.length(Buffer);
    end;
    Application.Destroy;
    Result := S_OK;
  except
    result := E_FAIL;
  end;
  FLock.UnLock;
end;

function TAsyncEx.SetBuffersize(BufferSize: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  { if the buffersize is too small and when the min buffersize is not available
    the min buffersize will be automaticly set in TAsyncIO.SyncRead.
    Reason: at this point the min buffersize might not known }
  if GFMinBuffersize < BufferSize then
    // copy the value is slower but more savety to prevent crashes
    GFBufferSize := strtoint(copy(inttostr(BufferSize), 1,
      length(inttostr(BufferSize))));
end;

function TAsyncEx.GetBuffersize(out BufferSize: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  // copy the value is slower but more safety, to prevent crashes
  BufferSize := strtoint(copy(inttostr(GFBufferSize), 1,
    length(inttostr(GFBufferSize))));
end;

function TAsyncEx.SetRipStream(Ripstream: LongBool; Path: PChar;
  Filename: PChar): HRESULT; stdcall;
begin
  FRipstream := Ripstream;
  FPath := copy(Path, 1, length(Path));
  FFile := copy(Filename, 1, length(Filename));
  RESULT := S_OK;
  if g_threadedShoutcastStream <> nil then
  begin
    g_threadedShoutcastStream.SetRipStream(Ripstream, Path, FFile);
    RESULT := S_OK;
  end;
end;

// TAsyncEx.GetRipStream is not implemented yet

function TAsyncEx.GetRipStream(out Ripstream: LongBool;
  out FileO: PChar): HRESULT; stdcall;
var
  fileL: string;
  {*l_ripstream: boolean;*}
begin
  fileL := '';
  {*l_ripstream := false;*}
  RESULT := E_FAIL;
  {*  if g_shoutCastStream <> nil then
      begin
                g_shoutCastStream.get_ripStream(l_ripstream,l_file);
                Ripstream := l_ripstream;
                FileO := copy(l_file,1,length(l_file));
                RESULT := S_OK;
      end;   *}
end;

function TAsyncEx.SetLoadFromStream(Stream: IStream; Length: int64): HRESULT; stdcall;
begin
  FStreamsize := Length;
  CreateFilterPin(Stream, true);
//  CreateFilterPin(TStreamAdapter.Create(@Stream, soOwned), true);
  GFFileName := 'In TStream Mode is Filename not available';
  Result := S_OK;
end;

function TAsyncEx.SetCallBack(CallBack: IAsyncExCallBack): HRESULT; stdcall;
begin
  GFFilterCallBack := CallBack;
  Result := S_OK;
end;

function TAsyncEx.FreeCallback(): HRESULT; stdcall;
begin
  if Assigned(GFFilterCallBack) then
  begin
    GFFilterCallBack.AsyncExICYNotice(ICYName, 'N/A');
    GFFilterCallBack.AsyncExICYNotice(ICYGenre, 'N/A');
    GFFilterCallBack.AsyncExICYNotice(ICYURL, 'N/A');
    GFFilterCallBack.AsyncExICYNotice(ICYBitrate, 'N/A');
    GFFilterCallBack.AsyncExFilterState(false, false, false, false, 0);
    GFFilterCallBack := nil;
  end;
  result := S_OK;
end;
// IDSPlayerAsyncSourceControl end

// IFileSourceFilter begin

function TAsyncEx.Load(pszFileName: PWCHAR;
  const pmt: PAMMediaType): HRESULT; stdcall;
begin
  if Length(pszFileName) > MAX_PATH then
  begin
    result := ERROR_FILENAME_EXCED_RANGE;
    exit;
  end;
  FFileName := GCFFilterID + ' (' + ExtractFileName(pszFileName) + ')';
  FFilestream := TFileStream.Create(pszFileName, fmOpenRead or
    fmShareDenyWrite);
  FStreamsize := FFilestream.Size;
  CreateFilterPin(TStreamAdapter.Create(FFilestream, soOwned), true);
  GFFileName := pszFileName;
  if FFileName = pszFileName then
    Result := E_OUTOFMEMORY
  else
    result := S_OK;
end;

function TAsyncEx.GetCurFile(out ppszFileName: PWideChar;
  pmt: PAMMediaType): HRESULT;
begin
  // no need to set a Mediatype at this point
  ppszFileName := StringToOleStr(copy(FFileName, 1, Length(FFileName)));
  result := S_OK;
end;
// IFileSourceFilter end

constructor TAsyncEx.Create;
begin
  FLock := TBCCritSec.Create;
  FState := State_Stopped;
  FFilestream := nil;
  FFile := '';
  g_threadedShoutcastStream := nil;
  GFFilterCallBack := nil;
  // 300kb as default
  GFBufferSize := 300 * 1000;
  GFMinBuffersize := 0;
  GFStringQueue := nil;
  GFConnected := false;
  GFStreamLength := 0;
  GFFileName := '';
  GFMayjorType := '';
  Fexitloop := false;
  GFExit := false;
  // create the Filter Pin without Stream (blank pin)
  CreateFilterPin(TStreamAdapter.Create(TMemoryStream.Create, soOwned), false);
end;

constructor TAsyncEx.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
begin
  inherited CreateFromFactory(Factory, Controller);
  FLock := TBCCritSec.Create;
  FState := State_Stopped;
  FFilestream := nil;
  FFile := '';
  g_threadedShoutcastStream := nil;
  GFFilterCallBack := nil;
  // 300kb as default
  GFBufferSize := 300 * 1000;
  GFMinBuffersize := 0;
  GFStringQueue := nil;
  GFConnected := false;
  GFStreamLength := 0;
  GFFileName := '';
  GFMayjorType := '';
  Fexitloop := false;
  GFExit := false;
  // create the Filter Pin without Stream (blank pin)
  CreateFilterPin(TStreamAdapter.Create(TMemoryStream.Create, soOwned), false);
end;

procedure TAsyncEx.CreateFilterPin(AStream: IStream;
  streamEnabled: boolean = false; URLPin: boolean = false);
var
  Hr: HRESULT;
begin
  inherited Create(GCFFilterID, nil, TBCCritSec.Create, GUID_NULL, Hr);
  if streamEnabled then
  begin
    if URLPin then
      // create a URL stream pin
      FPin := TStreamOutPin.Create(GCFPinID, Self, FLock, Hr,
        GCFPinID, nil, true, 0, true, true)
    else
      // create a filestream pin
      FPin := TStreamOutPin.Create(GCFPinID, Self, FLock, Hr,
        GCFPinID, AStream, true, FStreamsize, true);
    // maintain a ref on the pin
    FPin.NonDelegatingAddRef;
    // destructor flag
    FPinActive := true;
  end
  else
  begin
    FPin := nil;
    FPinActive := false;
  end;
end;

destructor TAsyncEx.Destroy;
begin
  GFExit := true;
  if FFilestream <> nil then
  begin
    FFilestream.Destroy;
    FFilestream := nil;
  end;
  if g_threadedShoutcastStream <> nil then
  begin
    g_threadedShoutcastStream.Destroy;
    g_threadedShoutcastStream := nil;
  end;
  if GFStringQueue <> nil then
  begin
    GFStringQueue.Destroy;
    GFStringQueue := nil;
  end;
  if FPin <> nil then begin
    TStreamOutPin(FPin).Destroy;
    FPin := nil;
  end;
  FLock.Destroy;
  inherited Destroy;
end;

function TAsyncEx.GetOutPin: IPin;
begin
  if FPin <> nil then
    Result := FPin;
end;

function TAsyncEx.GetPin(n: Integer): TBCBasePin;
begin
  if n = 0 then
    Result := FPin
  else
    Result := nil;
end;

function TAsyncEx.GetPinCount: Integer;
begin
  if FPin <> nil then
    Result := 1
  else
    Result := 0;
end;

function TAsyncEx.NonDelegatingRelease: Integer;
begin            
  Result := inherited NonDelegatingRelease;
  if ((Result = 1) and (FPin <> nil)) then
      FPin.NonDelegatingRelease;
end;

// helper functions

function TAsyncEx.ParseUrl(URL: string; out Host: string; out Port: string;
  out Location: string): boolean;
var
  Pos1: integer;
  Pos2: integer;
  Temp: string;
begin
  result := false;
  if length(URL) = 0 then
    exit;
  // check for http string
  Pos1 := pos('http://', URL);
  if Pos1 = 0 then
    exit;
  result := true;
  Temp := copy(URL, Pos1 + length('http://'), length(URL) - Pos1);
  // look for port offset
  Pos1 := pos(':', Temp);
  // check if a port is given
  if Pos1 = 0 then
  begin
    // no port.. , set def. port and location
    Host := Temp;
    Port := '80';
    Location := '/';
    exit;
  end;
  Host := copy(Temp, 1, Pos1 - 1);
  // look for location offset
  Pos2 := pos('/', Temp);
  // check if location is given
  if Pos2 = 0 then
  begin
    // no location.. , set def. location
    Temp := copy(Temp, Pos1 + 1, length(Temp) - Pos1);
    Port := Temp;
    Location := '/';
    exit;
  end;
  Port := copy(Temp, Pos1 + 1, Pos2 - Pos1 - 1);
  Location := copy(Temp, Pos2, length(Temp) - Pos2 + 1);
end;

initialization
  TBCClassFactory.CreateFilter(TAsyncEx, GCFFilterID, CLSID_AsyncEx,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE, 1, @Pins);

end.

