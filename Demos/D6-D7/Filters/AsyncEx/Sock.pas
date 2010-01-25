unit Sock;

// *****************************************************************************
// Sock.Pas (TSock)
// Freeware Windows Socket Component For Delphi & C++ Builder
// Version 1.0k, tested with Delphi 2.0, 3.0 & 4.0
// Written By Tom Bradford
// Maintained By Ward van Wanrooij
//   (ward@ward.nu, http://www.ward.nu)
//
// Copyright (C) 1997-2000, Beach Dog Software, Inc.
// Copyright (C) 2000-2003, Ward van Wanrooij
// All Rights Reserved
// Latest version can be obtained at http://www.ward.nu/computer/tsock
// *****************************************************************************

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WinSock, BaseClass;

type
  TSocketInfo = (siLookUp, siConnect, siClose, siListen, siReceive, siSend,
    siAccept, siError);
  TSocketType = (stStream, stDatagram);
  TLineBreak = (lbCRLF, lbCR, lbLF, lbSmart);

const
  WM_SOCK = WM_USER + 75; // Hopefully, Your App Won't Use This Message

type
  TSock = class; // Forward Declared For Event Types

  ESockException = class(Exception);
  TNotifyReadEvent = procedure(Sender: TObject; Count: Integer) of object;
  TNotifyAutoEvent = procedure(Sender: TObject; NewSock: TSock) of object;
  TNotifyInfoEvent = procedure(sender: TObject; SocketInfo: TSocketInfo; Msg:
    string) of object;

  TSock = class(TCustomControl)
  private
    FSockAddrIn: TSockAddrIn; // Address Information Block
    FRecvAddrIn: TSockAddrIn; // Address Information Block For RecvFrom
    FLastChar: Char; // Last Character Read For Line-Input

    FPicture: TBitmap; // Holder For Design-Time Image
    FBmp_TCP: TBitmap; // TCP Bitmap
    FBmp_UDP: TBitmap; // UDP Bitmap
    FBmp_Listen: TBitmap; // Listening Bitmap

    // Character Buffer (Most WINSOCK.DLLs Max At 32k)
    //  FCharBuf    : Array[1..32768] Of Char;
    FCharBuf: array[1..750] of Char; // small buffer works more stable
    FSocketType: TSocketType; // Socket Type (Stream Or Datagram)
    FLineBreak: TLineBreak; // Line Break Style For Line Input
    FHostName: string; // Host Name Or IP Address
    FPortName: string; // Port Name Or Well-Known Number
    FLocalPortName: string;
      // Local Port Name Or Well-Known Number, Defaults To 1 (=FPortName) For Backward Compatibility
    FSocket: TSocket; // Socket Handle
    FInBuffer: string; // Input Buffer
    FOutBuffer: string; // Output Buffer For Non-Blocking
    FListen: Boolean; // Socket Listens?
    FBlocking: Boolean; // Do Blocking Calls?
    FAutoAccept: Boolean; // Automatically Accept Incomings
    FConnected: Boolean; // Are We Connected?
    FBlockTime: Integer; // How Long To Wait For Blocking Operation
    FStream: TStream; // Associated TSockStream Object
    FFreeOnClose: Boolean;
      // Free after closure of socket? (Non-blocking, auto-accepted sockets!)

    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnInfo: TNotifyInfoEvent;
    FOnRead: TNotifyReadEvent;
    FOnWrite: TNotifyEvent;
    FOnAccept: TNotifyEvent;
    FOnAutoAccept: TNotifyAutoEvent;

    m_receiveForm: TForm;
    m_lock: TBCCritSec;

    // Property Set/Get Routines
    procedure SetHostName(Value: string);
    procedure SetPortName(Value: string);
    procedure SetLocalPortName(Value: string);
    function GetText: string;
    procedure SetText(Value: string);
    procedure SetListen(Value: Boolean);
    procedure SetBlocking(Value: Boolean);
    procedure SetAutoAccept(Value: Boolean);
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
    procedure SetSocket(Value: TSocket);
    procedure SetSocketType(Value: TSocketType);
    function GetRemoteHost: string;
    function GetEOF: Boolean;

    // Private Support Methods
    procedure DoInfo(SocketInfo: TSocketInfo; Msg: string);
    procedure SetBitmap;
  protected
    // Event Handlers
    procedure WMSock(var Message: TMessage); message WM_SOCK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    // Loaded Handles Starting Listening Mode After Streaming The Properties
    procedure Loaded; override;

    // Protected Constructor Can Only Be Called By TSock Class
    constructor CreateWithSocket(AOwner: TComponent; NewSocket: TSocket);
      virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: Boolean;
    function Close: Boolean;
    function Send(Value: string): Boolean;
    function SendLine(Value: string): Boolean;
    function ReceiveCount(Count: Integer): string;
    function Receive: string;
    function ReceiveLine: string;
    function SendDatagram(Value, HostName: string): Boolean;
    function ReceiveDatagram(var HostName: string): string;

    // The Accept Method Will Create NewSock, But User Must Free
    function Accept(var NewSock: TSock): Boolean;

    // Public Support Methods
    function HostLookup(Value: string): TInAddr;
    function PortLookup(Value: string): U_Short;

    // StartListen And StopListen Are A Robust Form Of Setting Listen
    function StartListen: Boolean;
    function StopListen: Boolean;

    property Text: string read GetText write SetText;
    property Connected: Boolean read GetConnected write SetConnected;
      // Used To Read FConnected

    property EndOfFile: Boolean read GetEOF;
    property Socket: TSocket read FSocket write SetSocket;

    property Stream: TStream read FStream;

    // RemoteHost Returns The Remote IP If SocketType=stStream
    // And Will Return The Most Recent Incoming Datagram IP If
    // SocketType=stDatagram
    property RemoteHost: string read GetRemoteHost;
    // RemoteHost = INet_NToA(RecvAddrIn.SIn_Addr); Provided as property for easy-of-use and backward compatibility
    property RecvAddrIn: TSockAddrIn read FRecvAddrIn;

  published
    property SocketType: TSocketType read FSocketType write SetSocketType;
    property HostName: string read FHostName write SetHostName;
    property PortName: string read FPortName write SetPortName;
    property LocalPortName: string read FLocalPortName write SetLocalPortName;
    property Blocking: Boolean read FBlocking write SetBlocking;
    property AutoAccept: Boolean read FAutoAccept write SetAutoAccept;
    property Listen: Boolean read FListen write SetListen;
    property LineBreak: TLineBreak read FLineBreak write FLineBreak;
    property BlockingTimeout: Integer read FBlockTime write FBlockTime;

    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnInfo: TNotifyInfoEvent read FOnInfo write FOnInfo;
    property OnRead: TNotifyReadEvent read FOnRead write FOnRead;
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnAutoAccept: TNotifyAutoEvent read FOnAutoAccept write
      FOnAutoAccept;
  end;

  // Global IP Caching Mechanism.  Uses A String List That Stores The 32-Bit IP
  // Address Of It's Associated Hostname In The Object Property Of The List.  You
  // Should Never Have To Manipulate This Object Directly, But It Is Made Public
  // For The Purpose Of Calling The Clear Method To Empty It.
var
  IPCache: TStringList;

function WSDescription: string; // Returns A Description Of The WinSock Driver
function WSSystemStatus: string; // Returns System Status From The WinSock Driver
function GetLocalHostname: string; // Return Local Hostname
function SocketInfoText(Value: TSocketInfo): string;
  // Converts TSocketInfo Values To Text
function ErrToStr(Value: Integer): string; // Converts A WinSock Error To Text
function Base64Encode(Value: string): string;
  // Converts Passed Value To MIME Base64
function Base64Decode(Value: string): string;
  // Converts Passed Value From MIME Base64
function URLEncode(Value: string): string;
  // Converts String To A URLEncoded String
function URLDecode(Value: string): string;
  // Converts String From A URLEncoded String

procedure Register;

implementation

uses config;

const
  Base64Table =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  ValidURLChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$-_@.&+-!*"''(),;/#?:';
  SocketInfoMsg: array[siLookUp..siError] of string = ('Lookup', 'Connect',
    'Close', 'Listen', 'Receive', 'Send', 'Accept', 'Error');

type
  TSockStream = class(TStream)
  private
    Sock: TSock;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    constructor Create(Sock: TSock); virtual;
  end;

type
  TSockThread = class(TThread)
  private
    ParentSock: TSock;
    ClientSock: TSock;
  public
    procedure Execute; override;
    procedure ThreadTerminate(Sender: TObject);
    procedure RunThread(ParentSock, ClientSock: TSock);
  end;

  // WinSock Initialization Data
var
  WSAData: TWSAData;

  //*** TSockStream Methods ******************************************************

constructor TSockStream.Create(Sock: TSock);
begin
  Self.Sock := Sock;
end;

function TSockStream.Read(var Buffer; Count: Longint): Longint;
var
  Temp: string;
begin
  Temp := Sock.ReceiveCount(Count);
  Move(Temp[1], Buffer, Length(Temp));
  Result := Length(Temp);
end;

function TSockStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: string;
begin
  SetLength(Temp, Count);
  Move(Buffer, Temp[1], Count);
  Sock.Send(Temp);
  Result := Count;
end;

function TSockStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

//*** TSockThread Methods ******************************************************

procedure TSockThread.Execute;
begin
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  ParentSock.OnAutoAccept(ParentSock, ClientSock);
  Terminate;
end;

procedure TSockThread.ThreadTerminate(Sender: TObject);
begin
  ClientSock.Free;
end;

procedure TSockThread.RunThread(ParentSock, ClientSock: TSock);
begin
  Self.ParentSock := ParentSock;
  Self.ClientSock := ClientSock;
  Resume;
end;

//*** Property Set/Get Procedures **********************************************

procedure TSock.SetHostName(Value: string);
begin
  if (FSocketType = stStream) and FConnected then
    DoInfo(SiLookup, 'Setting HostName While Connected Has No Effect');
  FHostName := Value;
  if (FSocketType = stDatagram) and FConnected then
    FSockAddrIn.SIn_Addr := HostLookup(Value);
end;

procedure TSock.SetPortName(Value: string);
begin
  if FConnected then
    DoInfo(SiLookup, 'Setting PortName While Connected Has No Effect');
  FPortName := Value;
end;

procedure TSock.SetLocalPortName(Value: string);
begin
  if FConnected then
    DoInfo(SiLookup, 'Setting LocalPortName While Connected Has No Effect');
  FLocalPortName := Value;
end;

function TSock.GetText: string;
begin
  // Just Call The Receive Method
  Result := Receive;
end;

procedure TSock.SetText(Value: string);
begin
  // Just Call The Send Method And Ignore The Boolean Result
  Send(Value);
end;

procedure TSock.SetListen(Value: Boolean);
var
  WasListen: Boolean;
  Addr: TSockAddr;
  Res: Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    FListen := Value;
    if Value and (FSocketType = stDatagram) then
      // Listening Sockets Must Be Stream Sockets
      SetSocketType(stStream)
    else
      SetBitmap;
    Exit;
  end
  else if (csReading in ComponentState) then
  begin
    // If We Haven't Loaded Yet, Just Set The Value And Exit
    FListen := Value;
    Exit;
  end;
  WasListen := FListen;
  if (FSocket <> INVALID_SOCKET) and (not WasListen) then
  begin
    FListen := False;
    raise ESockException.Create('Listen - Socket Already In Use');
  end;
  if (FSocketType = stDatagram) and Value then
  begin
    FListen := False;
    raise ESockException.Create('Listen - Cannot Listen On A Datagram Socket');
  end;
  FListen := Value;
  if FListen then
  begin
    if not WasListen then
    begin
      // Have To Create A Socket Start Asynchronous Listening
      FListen := True;
      FSocket := WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
      FillChar(Addr, SizeOf(Addr), #0);
      Addr.SIn_Family := AF_INET;
      Addr.SIn_Port := PortLookup(FPortName);
      Addr.SIn_Addr.S_Addr := HToNL(INADDR_ANY);
      // SetBlocking Will Set The Asynchronous Mode
      SetBlocking(FBlocking);
      FListen := False;
      Res := WinSock.Bind(FSocket, Addr, SizeOf(Addr));
      if Res <> 0 then
        raise ESockException.Create('Listen - Error Binding Socket');
      Res := WinSock.Listen(FSocket, 5);
      if Res <> 0 then
        raise ESockException.Create('Listen - Error Starting Listen');
      FListen := True;
      DoInfo(SiListen, 'Listening Started');
    end
    else
      DoInfo(SiListen, 'Listening Already Running');
  end
  else
  begin
    Close;
    DoInfo(SiListen, 'Listening Stopped');
  end;
end;

procedure TSock.SetBlocking(Value: Boolean);
var
  Il: U_Long;
  Ev: U_Long;
begin
  if (not (csDesigning in ComponentState)) and (csReading in ComponentState)
    then
  begin
    // If We Haven't Fully Loaded Yet, Just Set The Value And Exit
    FBlocking := Value;
    Exit;
  end;
  if FSocket = INVALID_SOCKET then
    FBlocking := Value
  else
  begin
    Ev := 0;
    FBlocking := Value;
    if (Parent = nil) then
    begin
      // If The Component Has No Parent (Dynamically Created) We Adopt It
      Parent := Screen.Forms[0];
      HandleNeeded;
    end;
    if FBlocking and (not FListen) then
    begin
      Il := 0;
      // Turn Off Async Checking And Set Blocking On
      WinSock.WSAAsyncSelect(FSocket, Handle, WM_SOCK, Ev);
      WinSock.IOCtlSocket(FSocket, FIONBIO, Il);
    end
    else
    begin
      if FListen then
        // If We're Listening, We Only Care About Accept Messages
        Ev := FD_ACCEPT
      else
      begin
        Ev := FD_READ; // Datagram Sockets Only Care About Read Messages
        if FSocketType = stStream then
          Ev := Ev or FD_CLOSE or FD_CONNECT or FD_WRITE or FD_READ;
      end;
      WinSock.WSAAsyncSelect(FSocket, Handle, WM_SOCK, Ev);
    end;
  end;
end;

procedure TSock.SetAutoAccept(Value: Boolean);
begin
  FAutoAccept := Value;
end;

procedure TSock.SetConnected(Value: Boolean);
begin
  if Value then
    Open
  else
    Close;
end;

function TSock.GetConnected: Boolean;
begin
  if FSocket = INVALID_SOCKET then
    FConnected := False;
  Result := FConnected;
end;

function TSock.GetEOF: Boolean;
begin
  Result := (FInBuffer = '') and (not FConnected);
end;

procedure TSock.SetSocket(Value: TSocket);
var
  Buf: array[1..10] of Char;
  Len: Integer;
  Res: Integer;
begin
  FSocket := Value;
  if FSocket = INVALID_SOCKET then
  begin
    // If The Socket Is Unassigned Then Who Cares
    FConnected := False;
    FListen := False;
  end
  else
  begin
    // Otherwise, We Need To Check To See If It's Already Listening
    Len := SizeOf(Buf);
    Res := WinSock.GetSockOpt(FSocket, IPPROTO_TCP, SO_ACCEPTCONN, PChar(@Buf),
      Len);
    if (Res = 0) and (Buf[1] <> #0) then
    begin
      FSocket := INVALID_SOCKET;
      raise ESockException.Create('Socket - Can''t Assign A Listening Socket');
    end
    else
      FConnected := True;
  end;
end;

procedure TSock.SetSocketType(Value: TSocketType);
begin
  if csDesigning in ComponentState then
  begin
    // At Design-Time, stDatagram And Listen Are Mutually Exclusive
    if (Value = stDatagram) and FListen then
      SetListen(False);
    FSocketType := Value;
    SetBitmap;
  end
  else
  begin
    if FListen then
      raise
        ESockException.Create('SocketType - Can''t Assign Socket Type While Listening');
    if FConnected then
      raise
        ESockException.Create('SocketType - Can''t Assign Socket Type While Connected');
    FSocketType := Value;
  end
end;

function TSock.GetRemoteHost: string;
begin
  // Convert FRecvAddrIn To A String IP Address
  Result := INet_NToA(FRecvAddrIn.SIn_Addr);
end;

procedure TSock.DoInfo(SocketInfo: TSocketInfo; Msg: string);
begin
  if Assigned(FOnInfo) then
    FOnInfo(Self, SocketInfo, Msg);
end;

procedure TSock.SetBitmap;
begin
  // Determine The Design-Time Bitmap To Use
  if FSocketType = stDatagram then
    FPicture := FBmp_UDP
  else if FListen then
    FPicture := FBmp_Listen
  else
    FPicture := FBmp_TCP;
  Invalidate;
end;

//*** Constructor/Destructor ***************************************************

constructor TSock.Create(AOwner: TComponent);
begin
  m_receiveForm := TForm.Create(nil);
  inherited Create(m_receiveForm);
  m_lock := TBCCritSec.Create;
  Parent := TWinControl(m_receiveForm);
    // <<--- added by blacktrip, wild cast but
  // prevent crashes !!!
  if WinSock.WSAStartup($0101, WSAData) <> 0 then
    raise ESockException.Create('WSAStartup - Could Not Initialize WinSock');
  IPCache := TStringList.Create;
  IPCache.Clear;

  if (csDesigning in ComponentState) then
  begin
    // Get Bitmaps For Design-Time Image
    FBmp_TCP := TBitmap.Create;
    FBmp_UDP := TBitmap.Create;
    FBmp_Listen := TBitmap.Create;
    FBmp_TCP.Handle := LoadBitmap(hInstance, 'TCP');
    FBmp_UDP.Handle := LoadBitmap(hInstance, 'UDP');
    FBmp_Listen.Handle := LoadBitmap(hInstance, 'LISTEN');
    FPicture := FBmp_TCP;
    Width := FPicture.Width;
    Height := FPicture.Height;
    SetZOrder(True);
  end
  else
  begin
    Width := 0;
    Height := 0;
    SetZOrder(False);
    Visible := False;
  end;
  FHostName := '';
  FPortName := '';
  FLocalPortName := '-1';
  FSocket := INVALID_SOCKET;
  FLineBreak := lbSmart;
  FLastChar := #0;
  FInBuffer := '';
  FOutBuffer := '';
  FListen := False;
  FBlocking := False;
  FAutoAccept := False;
  FConnected := False;
  FStream := TSockStream.Create(Self);
  FFreeOnClose := False;
end;

// This Constructor Assumes NewSocket Is A Valid Socket Handle

constructor TSock.CreateWithSocket(AOwner: TComponent; NewSocket: TSocket);
begin
  Create(AOwner);
  FSocket := NewSocket;
  SetBlocking(TSock(AOwner).Blocking);
  FBlockTime := TSock(AOwner).BlockingTimeout;
  FOnRead := TSock(AOwner).OnRead;
  FOnWrite := TSock(AOwner).OnWrite;
  FOnDisconnect := TSock(AOwner).OnDisconnect;
  FOnInfo := TSock(AOwner).OnInfo;
  FConnected := True;
  FLineBreak := TSock(AOwner).LineBreak;
  FRecvAddrIn := TSock(AOwner).RecvAddrIn;
  FFreeOnClose := not FBlocking;
end;

destructor TSock.Destroy;
begin
  if FListen or FConnected then
    Close;
  if (csDesigning in ComponentState) then
  begin
    FBmp_TCP.Free;
    FBmp_UDP.Free;
    FBmp_Listen.Free;
  end;
  FStream.Free;
  IPCache.Free;
  WinSock.WSACleanup;
  inherited Destroy;
end;

procedure TSock.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    // If Component Has Been Loaded At Run-Time And Listen Then Start Listening
    SetBlocking(FBlocking);
    if FListen then
    begin
      FListen := False;
      SetListen(True);
    end;
  end;
end;

//*** Event Handling ***********************************************************

procedure TSock.WMSock(var Message: TMessage);
var
  Event: Word;
  Error: Word;
  Res: Integer;
  AcSck: TSocket;
  Addr: TSockAddrIn;
  AddrL: Integer;
  CSock: TSock;
  Spawn: TSockThread;
begin
  m_lock.Lock;
  inherited;
  // Message Handling For Non-Blocking Sockets
  Event := WinSock.WSAGetSelectEvent(Message.LParam);
  Error := WinSock.WSAGetSelectError(Message.LParam);
  if (Error > WSABASEERR) then
    DoInfo(SiError, 'Error #' + IntToStr(Error) + ' (' + ErrToStr(Error) + ')');
  if (Error <= WSABASEERR) or (Event = FD_CLOSE) then
    // Messages Mean Different Things Depending On Whether You're Listening Or Not
    case Event of
      FD_ACCEPT:
        begin
          // Incoming Socket
          if FAutoAccept and Assigned(FOnAutoAccept) then
          begin
            // If AutoAccept Is Set To True And OnAutoAccept Is Set...
            // Create A New Socket Based On The Accepted One And Begin
            // AutoAccept As If It Were A Thread... The AutoAccept
            // Routine Is Responsible For Destroying The New Socket
            // Component.
            AddrL := SizeOf(Addr);
            FillChar(Addr, SizeOf(Addr), #0);
{$IFDEF VER93}
            AcSck := WinSock.Accept(FSocket, Addr, AddrL);
{$ELSE}
{$IFDEF WIN32}
            AcSck := WinSock.Accept(FSocket, @Addr, @AddrL);
{$ELSE}
            AcSck := WinSock.Accept(FSocket, Addr, AddrL);
{$ENDIF}
{$ENDIF}
            FRecvAddrIn := Addr;
            CSock := TSock.CreateWithSocket(Self, AcSck);
            CSock.PortName := FPortName;
            CSock.LocalPortName := FLocalPortName;
            CSock.HostName := INet_NToA(Addr.SIn_Addr);
            if FBlocking then
            begin
              Spawn := TSockThread.Create(True);
              Spawn.RunThread(Self, CSock);
            end
            else
              FOnAutoAccept(Self, CSock);
          end
          else if Assigned(FOnAccept) then
            FOnAccept(Self);
        end;
      FD_CONNECT:
        begin
          FConnected := True;
          DoInfo(SiConnect, 'Non-Blocking Socket Connected');
          if Assigned(FOnConnect) then
            FOnConnect(Self);
        end;
      FD_CLOSE:
        begin
          if Assigned(FOnDisconnect) then
            FOnDisconnect(Self);
          Close;
        end;
      FD_READ:
        begin
          if FSocketType = stStream then
          begin
            Res := WinSock.Recv(FSocket, FCharBuf, SizeOf(FCharBuf), 0);
            if Res > 0 then
              FInBuffer := FInBuffer + Copy(FCharBuf, 1, Res);
            DoInfo(SiReceive, 'Non-Blocking Incoming Data');
            if Assigned(FOnRead) then
            begin
              FOnRead(Self, Length(FInBuffer));
            end;
          end
          else if Assigned(FOnRead) then
            FOnRead(Self, Length(FInBuffer));
        end;
      FD_WRITE:
        begin
          if FOutBuffer <> '' then
            Send('');
          DoInfo(SiSend, 'Non-Blocking Outgoing Data');
          if Assigned(FOnWrite) then
            FOnWrite(Self);
        end;
    end;
  Message.Result := 0;
  m_lock.UnLock;
end;

procedure TSock.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Canvas.Draw(0, 0, FPicture);
  Message.Result := 0;
end;

procedure TSock.WMSize(var Message: TWMSize);
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    if Width <> FPicture.Width then
      Width := FPicture.Width;
    if Height <> FPicture.Height then
      Height := FPicture.Height;
  end;
  Message.Result := 0;
end;

//*** Support Methods **********************************************************

function TSock.Open: Boolean;
var
  Res: Integer;
  ST: Integer;
  LAddrIn: TSockAddrIn;
  //optval: integer;
begin
  if FSocket = INVALID_SOCKET then
  begin
    if FSocketType = stStream then
      ST := SOCK_STREAM
    else
      ST := SOCK_DGRAM;

    // Create The Socket
    FSocket := WinSock.Socket(AF_INET, ST, IPPROTO_IP);

    SetBlocking(FBlocking);

    // Set local options
    LAddrIn.SIn_Family := AF_INET;
    if FLocalPortName = '-1' then
      LAddrIn.SIn_Port := PortLookup(FPortName)
        // Default behaviour for backward compatibility
    else
      LAddrIn.SIn_Port := PortLookup(FLocalPortName);
    LAddrIn.SIn_Addr.S_Addr := HToNL(INADDR_ANY);
      // No HostLookup(...) Because INADDR_ANY Is A Windows Constant

    // Set Up The Remote Address And Port
    FSockAddrIn.SIn_Family := AF_INET;
    FSockAddrIn.SIn_Port := PortLookup(FPortName);
    FSockAddrIn.SIn_Addr := HostLookup(FHostName);

    if FSocketType = stStream then
    begin
      // Stream Sockets Require A Connect
      Res := WinSock.Bind(FSocket, LAddrIn, SizeOf(LAddrIn)) +
        WinSock.Connect(FSocket, FSockAddrIn, SizeOf(TSockAddrIn));
      if FBlocking then
      begin
        if Res = 0 then
        begin
          FConnected := True;
          DoInfo(SiConnect, 'Blocking Socket Connected');
          if Assigned(FOnConnect) then
            FOnConnect(Self);
        end
        else
        begin
          DoInfo(SiClose, 'Blocking Socket Can''t Connect');
          Close;
        end;
      end;
    end
    else
    begin
      //Datagram Sockets are connectionless, so they don't get connected.
      //It is possible to call WinSock.Connect, but it would produce extra overhead
      //as it only sets the default destination.
      Res := WinSock.Bind(FSocket, LAddrIn, SizeOf(LAddrIn));
      if Res = 0 then
      begin
        FConnected := True;
        DoInfo(SiConnect, 'Datagram Socket Connected');
        if Assigned(FOnConnect) then
          FOnConnect(Self);
      end
      else
      begin
        DoInfo(SiClose, 'Datagram Socket Can''t Connect');
        Close;
      end;
    end;
  end;
  Result := FConnected;
end;

function TSock.Close: Boolean;
begin
  Result := (WinSock.CloseSocket(FSocket) = 0);
  FSocket := INVALID_SOCKET;
  FConnected := False;
  if not FListen then
    DoInfo(SiClose, 'Socket Closed');
  FListen := False;
  if FFreeOnClose then
    Free;
end;

function TSock.Send(Value: string): Boolean;
var
  Remain: Integer;
begin
  Result := True;
  if FSocket = INVALID_SOCKET then
    raise ESockException.Create('Send - Socket Not Connected');
  if FListen then
    raise ESockException.Create('Send - Cannot Send On A Listener Socket');
  if FSocketType = stStream then
  begin
    FOutBuffer := FOutBuffer + Value;
    if FOutBuffer = '' then
      Exit;
    if FBlocking then
    begin
      Remain := Length(FOutBuffer);
      // While Any Content Remains Or No Errors Have Happened, Then Loop
      while Remain > 0 do
      begin
        Remain := WinSock.Send(FSocket, FOutBuffer[1], Length(FOutBuffer), 0);
        if (Remain = SOCKET_ERROR) and (WinSock.WSAGetLastError <>
          WSAEINPROGRESS) then
        begin
          DoInfo(SiError, 'Socket Error On Send');
          raise ESockException.Create('Send - Socket Error');
        end
        else
        begin
          if Remain > 0 then
            Delete(FOutBuffer, 1, Remain);
          Remain := Length(FOutBuffer);
          DoInfo(SiSend, 'Blocking Outgoing Data');
        end;
      end;
      FOutBuffer := '';
    end
    else
    begin
      // Do Not Loop For A Non-Blocking Socket
      DoInfo(SiSend, 'Non-Blocking Outgoing Data');
      Remain := WinSock.Send(FSocket, FOutBuffer[1], Length(FOutBuffer), 0);
      if Remain > 0 then
        Delete(FOutBuffer, 1, Remain);
    end;
  end
  else
    SendDatagram(Value, FHostName);
end;

function TSock.SendLine(Value: string): Boolean;
var
  Break: string;
begin
  case FLineBreak of
    lbCR: Break := #13;
    lbLF: Break := #10;
  else
    Break := #13#10;
  end;
  Result := Send(Value + Break);
end;

function TSock.Receive: string;
begin
  Result := ReceiveCount(-1);
end;

function TSock.ReceiveCount(Count: Integer): string;
var
  Res: Integer;
  FDSet: PFDSet;
  TV: PTimeVal;
  Err: Integer;
  HostN: string;
  Cnt: Integer;   
begin                    
  if (FSocket = INVALID_SOCKET) and (FInBuffer = '') then
    raise ESockException.Create('Receive - Socket Not Connected');
  if FListen then
    raise
      ESockException.Create('Receive - Cannot Receive On A Listener Socket');
  Cnt := Count;
  if (Cnt = -1) or (Cnt > SizeOf(FCharBuf)) then
    Cnt := SizeOf(FCharBuf);
  if FSocketType = stStream then
  begin
    if FBlocking then
    begin
      FDSet := New(PFDSet);
      FDSet^.FD_Count := 1;
      FDSet^.FD_Array[0] := FSocket;
      if FBlockTime >= 0 then
      begin
        TV := New(PTimeVal);
        TV^.tv_sec := FBlockTime;
      end
      else
        TV := nil;
      // Used To Loop While We're Connected And Anything Is In The Input Queue
      if FConnected and (WinSock.Select(FSocket, FDSet, nil, nil, TV) > 0) then
      begin
        DoInfo(SiReceive, 'Blocking Incoming Data');
        Res := WinSock.Recv(FSocket, FCharBuf, Cnt, 0);
        if (Res = SOCKET_ERROR) then
        begin
          Err := WSAGetLastError;
          Result := '';
          FInBuffer := '';
          Dispose(FDSet);
          Dispose(TV);
          DoInfo(SiError, 'Socket Error On Receive');
          if (not (Err - WSABASEERR in [WSAEINTR - WSABASEERR, WSAEINPROGRESS -
            WSABASEERR, WSAEOPNOTSUPP - WSABASEERR, WSAEWOULDBLOCK - WSABASEERR,
            WSAEMSGSIZE - WSABASEERR])) then
          begin
            DoInfo(siClose, 'Socket Disconnected On Error On Receive');
            Close;
            if Assigned(FOnDisconnect) then
              FOnDisconnect(Self);
          end;
          raise ESockException.Create('Receive - Socket Error ' +
            ErrToStr(Err));
        end
        else
        begin
          if Res > 0 then
            FInBuffer := FInBuffer + Copy(FCharBuf, 1, Res)
          else if Res = 0 then
          begin
            DoInfo(siClose, 'Socket Disconnected On Receive');
            Close;
            if Assigned(FOnDisconnect) then
              FOnDisconnect(Self);
          end;
        end;
      end;
      Result := FInBuffer;
      FInBuffer := '';
      Dispose(FDSet);
      Dispose(TV);
    end
    else
    begin
      if ((Count <> -1) and (Length(FInBuffer) > Count)) then
      begin
        Result := Copy(FInBuffer, 1, Count);
        Delete(FInBuffer, 1, Count);
      end
      else
      begin
        Result := FInBuffer;
        FInBuffer := '';
      end;
    end;
  end
  else
    Result := ReceiveDatagram(HostN);
end;

function TSock.ReceiveLine: string;
var
  CPos, CLen: LongInt;
  Temp: string;
begin
  CPos := 0;
  Result := '';
  if FSocketType = stStream then
  begin
    if (FBlocking and FConnected) then
    begin
      Temp := FInBuffer;
      FInBuffer := '';
      Temp := Temp + Receive;
      FInBuffer := Temp;
    end;
    if (FLastChar = #13) and (FLineBreak = lbSmart) and (FInBuffer[1] = #10)
      then
    begin
      Delete(FInBuffer, 1, 1);
      FLastChar := #0;
    end;
    case FLineBreak of
      lbCR: CPos := Pos(#13, FInBuffer);
      lbLF: CPos := Pos(#10, FInBuffer);
      lbCRLF: CPos := Pos(#13#10, FInBuffer);
      lbSmart:
        begin
          CPos := Pos(#13, FInBuffer);
          if (CPos = 0) or (Pos(#10, FInBuffer) < CPos) then
            CPos := Pos(#10, FInBuffer);
          if CPos > 0 then
            FLastChar := FInBuffer[CPos]
          else
            FLastChar := #0;
        end;
    end;
    if FLineBreak = lbCRLF then
      CLen := 2
    else
      CLen := 1;
    if (CPos > 0) or (not FConnected) then
    begin
      if CPos > 0 then
      begin
        Result := Copy(FInBuffer, 1, CPos - 1);
        Delete(FInBuffer, 1, CPos + (CLen - 1));
      end
      else
      begin
        Result := FInBuffer;
        FInBuffer := '';
      end;
    end;
  end
  else
    Result := Receive;
end;

function TSock.SendDatagram(Value, HostName: string): Boolean;
begin
  if FSocket = INVALID_SOCKET then
    raise ESockException.Create('SendDatagram - Socket Not Connected');
  if FSocketType = stStream then
    raise
      ESockException.Create('SendDatagram - Datagram Send Not Supported On Stream Sockets');
  Result := True;
  SetHostName(HostName);
  if Value = '' then
    Exit;
  WinSock.SendTo(FSocket, Value[1], Length(Value), 0, FSockAddrIn,
    SizeOf(TSockAddrIn));
end;

function TSock.ReceiveDatagram(var HostName: string): string;
var
  Res: Integer;
  FDSet: PFDSet;
  TV: PTimeVal;
  FLen: Integer;
begin
  if FSocket = INVALID_SOCKET then
    raise ESockException.Create('ReceiveDatagram - Socket Not Connected');
  if FSocketType = stStream then
    raise
      ESockException.Create('ReceiveDatagram - Datagram Receive Not Supported On Stream Sockets');
  FDSet := New(PFDSet);
  FDSet^.FD_Count := 1;
  FDSet^.FD_Array[0] := FSocket;
  Result := '';
  HostName := '';
  if FBlockTime >= 0 then
  begin
    TV := New(PTimeVal);
    TV^.tv_sec := FBlockTime;
  end
  else
    TV := nil;
  if WinSock.Select(FSocket, FDSet, nil, nil, TV) > 0 then
  begin
    FLen := Sizeof(FRecvAddrIn);
    Res := WinSock.RecvFrom(FSocket, FCharBuf, SizeOf(FCharBuf), 0, FRecvAddrIn,
      FLen);
    if Res > 0 then
    begin
      Result := Copy(FCharBuf, 1, Res);
      HostName := GetRemoteHost;
    end
    else
      raise ESockException.Create('Socket Error while Receiving Datagram:' +
        IntToStr(WSAGetLastError));
  end;
  Dispose(FDSet);
  Dispose(TV);
end;

function TSock.Accept(var NewSock: TSock): Boolean;
var
  AcSck: TSocket;
  AddrL: Integer;
  Addr: TSockAddrIn;
begin
  // Accept Creates A New Instance Of A TSock Component And Returns It To The
  // User Application.  The User Is Responsible For Freeing The Component.
  if not FListen then
    raise ESockException.Create('Accept - Socket Not In Listening Mode');
  if FBlocking then
    DoInfo(SiAccept, 'Blocking Accept');
  AddrL := SizeOf(Addr);
{$IFDEF VER93}
  AcSck := WinSock.Accept(FSocket, Addr, AddrL);
{$ELSE}
{$IFDEF WIN32}
  AcSck := WinSock.Accept(FSocket, @Addr, @AddrL);
{$ELSE}
  AcSck := WinSock.Accept(FSocket, Addr, AddrL);
{$ENDIF}
{$ENDIF}
  FRecvAddrIn := Addr;
  if AcSck <> INVALID_SOCKET then
  begin
    NewSock := TSock.CreateWithSocket(Self, AcSck);
    NewSock.PortName := FPortName;
    NewSock.LocalPortName := FLocalPortName;
    NewSock.HostName := INet_NToA(Addr.SIn_Addr);
    Result := True;
    DoInfo(SiAccept, 'Created New TSock Structure');
  end
  else
  begin
    Result := False;
    DoInfo(SiAccept, 'Could Not Accept Connection');
  end;
end;

function TSock.HostLookup(Value: string): TInAddr;
type
  PLongInt = ^LongInt;
var
  PHost: PHostEnt;
  Res, I: Integer;
  AllNumeric: Boolean;
begin
  if Value = '' then
    Exit;
  DoInfo(SiLookUp, 'Lookup Of Host ' + Value);
  FillChar(Result, SizeOf(TInAddr), #0);
  AllNumeric := True;
  for I := 1 to Length(Value) do
    if not (Value[I] in ['0'..'9', '.']) then
    begin
      AllNumeric := False;
      Break;
    end;
  if AllNumeric then
    Result := TInAddr(WinSock.Inet_Addr(PChar(Value)))
      // If It's Dot-Notation, Just Convert It From An IP Address
  else
  begin
    Res := IPCache.IndexOf(Value);
    if Res >= 0 then
      // It's Cached... Don't Bother Doing A Lookup
      Result.S_Addr := U_Long(IPCache.Objects[Res])
    else
    begin
      // Isn't Cached, Have To Do A GetHostByName
      if Value <> '' then
      begin
        PHost := WinSock.GetHostByName(PChar(Value));
        if PHost <> nil then
        begin
          Result.S_Addr := LongInt(PLongInt(PHost^.H_Addr_List^)^);
          IPCache.AddObject(Value, Pointer(Result.S_Addr));
        end
        else
        begin
          // If Assigned(FOnInfo) then   // added by coder@dsplayer.de
            //       FOnInfo(self,siError,'Host Lookup - Could Not Find Host Entry');
           //Raise ESockException.Create('Host Lookup - Could Not Find Host Entry');
        end;
      end
      else
        Result.S_Addr := HToNL(INADDR_ANY);
    end;
  end;
end;

function TSock.PortLookup(Value: string): U_Short;
var
  PEnt: PServEnt;
  Prot: string;
begin
  DoInfo(SiLookUp, 'Lookup Of Port ' + Value);
  if Pos(Value[1], '0123456789') > 0 then
    // It's Numeric, Just Convert It To A Network Byte Order Integer
    Result := HToNS(StrToInt(Value))
  else
  begin
    // Otherwise, Perform A GetServByName Based On The Protocol
    if FSocketType = stStream then
      Prot := 'tcp'
    else
      Prot := 'udp';
    PEnt := WinSock.GetServByName(PChar(Value), PChar(Prot));
    if PEnt <> nil then
      Result := PEnt^.S_Port
    else
      raise ESockException.Create('Port Lookup - Could Not Find Service Entry');
  end;
end;

function TSock.StartListen: Boolean;
begin
  SetListen(True);
  Result := FListen;
end;

function TSock.StopListen: Boolean;
begin
  Result := True;
  SetListen(False);
end;

//*** Additional General-Purpose Support Functions *****************************

function WSDescription: string;
begin
  Result := StrPas(WSAData.szDescription);
end;

function WSSystemStatus: string;
begin
  Result := StrPas(WSAData.szSystemStatus);
end;

function GetLocalHostname: string;
var
  CharHostname: array[0..255] of Char;
begin
  Result := 'localhost';
  if WinSock.GetHostname(CharHostname, SizeOf(CharHostname)) = 0 then
    Result := CharHostname
  else
    raise
      ESockException.Create('GetLocalHostname - Could Not Retrieve Hostname');
end;

function SocketInfoText(Value: TSocketInfo): string;
begin
  Result := SocketInfoMsg[Value];
end;

function ErrToStr(Value: Integer): string;
begin
  Result := 'UNKNOWN ERROR';
  case Value of
    WSABASEERR + 4: Result := 'WSAEINTR';
    WSABASEERR + 9: Result := 'WSAEBADF';
    WSABASEERR + 13: Result := 'WSAEACCES';
    WSABASEERR + 14: Result := 'WSAEFAULT';
    WSABASEERR + 22: Result := 'WSAEINVAL';
    WSABASEERR + 24: Result := 'WSAEMFILE';
    WSABASEERR + 35: Result := 'WSAEWOULDBLOCK';
    WSABASEERR + 36: Result := 'WSAEINPROGRESS';
    WSABASEERR + 37: Result := 'WSAEALREADY';
    WSABASEERR + 38: Result := 'WSAENOTSOCK';
    WSABASEERR + 39: Result := 'WSAEDESTADDRREQ';
    WSABASEERR + 40: Result := 'WSAEMSGSIZE';
    WSABASEERR + 41: Result := 'WSAEPROTOTYPE';
    WSABASEERR + 42: Result := 'WSAENOPROTOOPT';
    WSABASEERR + 43: Result := 'WSAEPROTONOSUPPORT';
    WSABASEERR + 44: Result := 'WSAESOCKTNOSUPPORT';
    WSABASEERR + 45: Result := 'WSAEOPNOTSUPP';
    WSABASEERR + 46: Result := 'WSAEPFNOSUPPORT';
    WSABASEERR + 47: Result := 'WSAEAFNOSUPPORT';
    WSABASEERR + 48: Result := 'WSAEADDRINUSE';
    WSABASEERR + 49: Result := 'WSAEADDRNOTAVAIL';
    WSABASEERR + 50: Result := 'WSAENETDOWN';
    WSABASEERR + 51: Result := 'WSAENETUNREACH';
    WSABASEERR + 52: Result := 'WSAENETRESET';
    WSABASEERR + 53: Result := 'WSAECONNABORTED';
    WSABASEERR + 54: Result := 'WSAECONNRESET';
    WSABASEERR + 55: Result := 'WSAENOBUFS';
    WSABASEERR + 56: Result := 'WSAEISCONN';
    WSABASEERR + 57: Result := 'WSAENOTCONN';
    WSABASEERR + 58: Result := 'WSAESHUTDOWN';
    WSABASEERR + 59: Result := 'WSAETOOMANYREFS';
    WSABASEERR + 60: Result := 'WSAETIMEDOUT';
    WSABASEERR + 61: Result := 'WSAECONNREFUSED';
    WSABASEERR + 62: Result := 'WSAELOOP';
    WSABASEERR + 63: Result := 'WSAENAMETOOLONG';
    WSABASEERR + 64: Result := 'WSAEHOSTDOWN';
    WSABASEERR + 65: Result := 'WSAEHOSTUNREACH';
    WSABASEERR + 66: Result := 'WSAENOTEMPTY';
    WSABASEERR + 67: Result := 'WSAEPROCLIM';
    WSABASEERR + 68: Result := 'WSAEUSERS';
    WSABASEERR + 69: Result := 'WSAEDQUOT';
    WSABASEERR + 70: Result := 'WSAESTALE';
    WSABASEERR + 71: Result := 'WSAEREMOTE';
    WSABASEERR + 91: Result := 'WSASYSNOTREADY';
    WSABASEERR + 92: Result := 'WSAVERNOTSUPPORTED';
    WSABASEERR + 93: Result := 'WSANOTINITIALISED';
    WSABASEERR + 101: Result := 'WSAEDISCON';
    WSABASEERR + 1001: Result := 'WSAHOST_NOT_FOUND';
    WSABASEERR + 1002: Result := 'WSATRY_AGAIN';
    WSABASEERR + 1003: Result := 'WSANO_RECOVERY';
    WSABASEERR + 1004: Result := 'WSANO_DATA';
  end;
end;

// Base-64 Encoding Is The Process Of Taking An Input Stream And Converting
// Every 3 Bytes Into 4 Bytes, Each Of Which Whose ASCII Value Fits Within
// A 64-Bit Range.  Base-64 Is Often Used For Encoding Binary Streams For
// Attaching To Email, But Is Perfect For Converting Binary To A Character
// Set That Can Be Used For URL-Encoding.  The Base-64 Character Set Does Not
// Include Characters That URLs Use For Delimiting Such As '=', '&', Carriage
// Returns, Etc...

function Base64Encode(Value: string): string;
var
  AIn: array[1..3] of Byte;
  AOut: array[1..4] of Byte;
  AWork: array[1..3] of Byte;
  I: Integer;
  O: LongInt;
begin
  Result := '';
  I := 1;
  O := Length(Value);
  case Length(Value) mod 3 of
    1: Value := Value + #0 + #0;
    2: Value := Value + #0;
  end;
  while I < Length(Value) do
  begin
    AIn[1] := Byte(Value[I]);
    AIn[2] := Byte(Value[I + 1]);
    AIn[3] := Byte(Value[I + 2]);

    AOut[1] := Byte(AIn[1] shr 2);
    AWork[1] := Byte(AIn[1] shl 4);
    AWork[2] := Byte(AWork[1] and $30);
    AWork[3] := Byte(AIn[2] shr 4);
    AOut[2] := Byte(AWork[2] or AWork[3]);
    AWork[1] := Byte(AIn[2] shl 2);
    AWork[2] := Byte(AWork[1] and $3C);
    AWork[3] := Byte(AIn[3] shr 6);
    AOut[3] := Byte(AWork[2] or AWork[3]);
    AOut[4] := Byte(AIn[3] and $3F);

    Inc(I, 3);
    Result := Result + Base64Table[AOut[1] + 1] + Base64Table[AOut[2] + 1] +
      Base64Table[AOut[3] + 1] + Base64Table[AOut[4] + 1];
  end;
  if O mod 3 > 0 then
    Result[Length(Result)] := '=';
  if O mod 3 = 1 then
    Result[Length(Result) - 1] := '=';
end;

function Base64Decode(Value: string): string;
var
  AIn: array[1..4] of Byte;
  AOut: array[1..3] of Byte;
  AWork: array[1..3] of Byte;
  I: Integer;
  C: Integer;
begin
  Result := '';
  I := 1;
  while I < Length(Value) do
  begin
    C := 3;
    FillChar(AWork, SizeOf(AWork), #0);
    FillChar(AOut, SizeOf(AWork), #0);
    AIn[1] := Byte(Pos(Value[I], Base64Table) - 1);
    AIn[2] := Byte(Pos(Value[I + 1], Base64Table) - 1);
    AIn[3] := Byte(Pos(Value[I + 2], Base64Table) - 1);
    AIn[4] := Byte(Pos(Value[I + 3], Base64Table) - 1);
    if Value[I + 3] = '=' then
    begin
      C := 2;
      AIn[4] := 0;
      if Value[I + 2] = '=' then
      begin
        C := 1;
        AIn[3] := 0;
      end;
    end;
    AWork[2] := Byte(AIn[1] shl 2);
    AWork[3] := Byte(AIn[2] shr 4);
    AOut[1] := Byte(AWork[2] or AWork[3]);
    AWork[2] := Byte(AIn[2] shl 4);
    AWork[3] := Byte(AIn[3] shr 2);
    AOut[2] := Byte(AWork[2] or AWork[3]);
    AWork[2] := Byte(AIn[3] shl 6);
    AOut[3] := Byte(AWork[2] or AIn[4]);
    Result := Result + Char(AOut[1]);
    if C > 1 then
      Result := Result + Char(AOut[2]);
    if C > 2 then
      Result := Result + Char(AOut[3]);
    Inc(I, 4);
  end;
end;

// This function converts a string into a RFC 1630 compliant URL,
// provided that the string does not contain illegal characters at illegal
// places, for example this URL is invalid because of the ! sign in the password:
// ftp://ward:pass!word@ftp.ward.nu/my_documents/ward@mymail?

function URLEncode(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    if Pos(UpperCase(Value[I]), ValidURLChars) > 0 then
      Result := Result + Value[I]
    else
    begin
      if Value[I] = ' ' then
        Result := Result + '+'
      else
      begin
        Result := Result + '%';
        Result := Result + IntToHex(Byte(Value[I]), 2);
      end;
    end;
  end;
end;

function URLDecode(Value: string): string;
const
  HexChars = '0123456789ABCDEF';
var
  I: Integer;
  Ch, H1, H2: Char;
begin
  Result := '';
  I := 1;
  while I <= Length(Value) do
  begin
    Ch := Value[I];
    case Ch of
      '%':
        begin
          H1 := Value[I + 1];
          H2 := Value[I + 2];
          Inc(I, 2);
          Result := Result + Chr(((Pos(H1, HexChars) - 1) * 16) + (Pos(H2,
            HexChars) - 1));
        end;
      '+': Result := Result + ' ';
      '&': Result := Result + #13 + #10;
    else
      Result := Result + Ch;
    end;
    Inc(I);
  end;
end;

//*** Registration And Initialization ******************************************

procedure Register;
begin
  RegisterComponents('Ward', [TSock]);
end;

initialization // (moved to create)
  // We're Looking To Use Version 1.1 Of WinSock Here
{  If WinSock.WSAStartup($0101, WSAData) <> 0 Then
     Raise ESockException.Create('WSAStartup - Could Not Initialize WinSock');
  IPCache := TStringList.Create;
  IPCache.Clear; }
finalization // moved to destroy
  { IPCache.Free;
   WinSock.WSACleanup;  }
end.

