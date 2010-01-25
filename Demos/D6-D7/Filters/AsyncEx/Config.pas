unit config;

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
    * (C) 2004 Martin Offenwanger: coder@dsplayer.de                    *
    *********************************************************************)
{
@author(Martin Offenwanger: coder@dsplayer.de)
@created(Apr 22, 2004)
@lastmod(Sep 09, 2004)
}

interface

uses DirectShow9, Classes, StringQueue, ShoutCastStream, BaseClass, ActiveX;

// Global Filter Identifiers
const
  GCFInt64max = 999999999999999999;
  GCFFilterID = 'AsyncEx';
  GCFPinID = 'StreamOut';
  // GUIDS
  CLSID_AsyncEx: TGUID = '{3E0FA044-926C-42d9-BA12-EF16E980913B}';
  CLSID_PropMonitor: TGUID = '{3E0FA066-929C-43d9-BA18-EF16E980913B}';
  CLSID_PropPage: TGUID = '{3E0FA055-926C-43d9-BA18-EF16E980913B}';
  // Interface ID'S
  IID_IDSPlayerAsyncSourceControl: TGUID =
  '{3E0FA056-926C-43d9-BA18-EF16E980913B}';
  IID_IDSPlayerAsyncSourceCallBack: TGUID =
  '{3E0FB667-956C-43d9-BA18-EF16E980913B}';
  { ogg media
    Tobias Ogg Splitter implementation causes a lot of problems with the source,
    so i disabled Ogg palyback for now..... }
  // MEDIASUBTYPE_OGGAudio    : TGUID = '{D2855FA9-61A7-4db0-B979-71F297C17A04}';

// Proposed supported SubTypes
  ProposedTypes:
  array[0..9] of PGuid = (
    @MEDIASUBTYPE_Avi,
    @MEDIASUBTYPE_AIFF,
    @MEDIASUBTYPE_AU,
    @MEDIASUBTYPE_DssAudio,
    @MEDIASUBTYPE_DssVideo,
    @MEDIASUBTYPE_MPEG1Audio,
    @MEDIASUBTYPE_MPEG1System,
    @MEDIASUBTYPE_MPEG1Video,
    @MEDIASUBTYPE_MPEG1VideoCD,
    @MEDIASUBTYPE_WAVE
    );
  // Stream as Majortype
  PinType: TRegPinTypes =
  (clsMajorType: @MEDIATYPE_Stream);

  // one Pin
  Pins: array[0..0] of TRegFilterPins =
  ((strName: GCFPinID; bRendered: FALSE; bOutput: TRUE;
    bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: nil;
    nMediaTypes: 1; lpMediaType: @PinType));

  // Interfaces, that can Queried on the Filter
type
  IAsyncExCallBack = interface(IUnknown)
    ['{3E0FB667-956C-43d9-BA18-EF16E980913B}']
    function AsyncExFilterState(Buffering: LongBool; PreBuffering: LongBool;
      Connecting: LongBool; Playing: LongBool;
      BufferState: integer): HRESULT; stdcall;
    function AsyncExICYNotice(IcyItemName: PChar;
      ICYItem: PChar): HRESULT; stdcall;
    function AsyncExMetaData(Title: PChar; URL: PChar): HRESULT; stdcall;
    function AsyncExSockError(ErrString: PChar): HRESULT; stdcall;
  end;

type
  IAsyncExControl = interface(IUnknown)
    ['{3E0FA056-926C-43d9-BA18-EF16E980913B}']
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
  end;

  { it is not the best way to use this objects as global,
    but for now it works ;)
   todo: move the Global Objects to private }
var
  // Filter callBack
  GFFilterCallBack: IAsyncExCallBack;
  GFPreBufferSize: integer;
  GFBufferSize: integer;
  GFMinBuffersize: integer;
  // external URl Stream Class
  GFStringQueue: TStringQueue; // global queue for all received data
  { we handle the queue from different classes
    tofo: rewmove this global var              }
  // flags
  GFConnected: boolean;
  GFExit: boolean;
  // state informations
  GFStreamPos: int64;
  GFStreamLength: int64;
  GFFileName: string;
  GFMayjorType: string;

implementation

end.

