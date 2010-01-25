unit Definitions;

interface

uses
  Classes, ActiveX;

const
  FilterID = 'AsyncEx';
  PinID = 'StreamOut';
  // GUIDS
  CLSID_AsyncEx: TGUID = '{3E0FA044-926C-42d9-BA12-EF16E980913B}';
  // Interface ID'S
  IID_IAsyncExControl: TGUID = '{3E0FA056-926C-43d9-BA18-EF16E980913B}';
  IID_IAsyncExCallBack: TGUID = '{3E0FB667-956C-43d9-BA18-EF16E980913B}';
  // Mpeg1 splitter
  CLSID_Mpeg1Split: TGUID = '{336475D0-942A-11CE-A870-00AA002FEAB5}';

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

implementation

end.

