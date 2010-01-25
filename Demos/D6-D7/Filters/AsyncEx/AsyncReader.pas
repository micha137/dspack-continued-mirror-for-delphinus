unit AsyncReader;

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
  ActiveX, Classes, DirectShow9, BaseClass, Windows, Queue, Config, Forms,
  ShoutCastStream, SysUtils, Dialogs, ExtCtrls;

type
  TAsyncIO = class(TInterfacedObject, IAsyncReader)
  private
    FStream: IStream;
    FStop,
      FWaiting,
      FFlushing,
      FFwdStream: boolean;
    FReaderLock,
      FListsLock: TBCCritSec;
    FWorkList,
      FDoneList: TQueue;
    FWorkEvent,
      FDoneEvent,
      FAllDoneEv: TBCAMEvent;
    FOutCount: Longint;
    FStrmSize: Int64;
    FThread: TThread;
    FURLMode: boolean;
    FMediaControl: IMediaControl;
    { the pause and run commands called with FMediaControl in Syncread
      must called via a timer, otherwise ondestroy in unit filter won't called }
    FTimerPlay: TTimer;
    FTimerPause: TTimer;
    procedure OnTimerPlay(Sender: TObject);
    procedure OnTimerPause(Sender: TObject);
    procedure PutDoneItem(AItem: PAsyncRequest);
    function GetDoneItem: PAsyncRequest;
    function PutWorkItem(AItem: PAsyncRequest): HRESULT;
    function GetWorkItem: PAsyncRequest;
    function SetPosition(const APos: Int64): HResult;
    procedure InitStreamLen;
    function SetStreamPos(const APos: Int64): HResult;
    function GetStreamPos: Int64;
    function CreateRequest(llPos: LONGLONG; lLength: Integer;
      bAligned: BOOL; pBuffer: Pointer; pContext: Pointer;
      dwUser: DWORD): PAsyncRequest;
    procedure CompleteRequest(Req: PAsyncRequest);
    function InitAllocator(out Alloc: IMemAllocator): HRESULT; virtual;
    function DoRequest(llPos: LONGLONG; lLength: Longint;
      bAligned: BOOL; pBuffer: Pointer; pContext: Pointer;
      dwUser: DWORD): HResult;
    function DoWaitForNext(dwTimeout: DWORD; var ppContext: Pointer;
      var pdwUser: DWORD; var pcbActual: Longint): HRESULT;
  protected
    // IAsyncReader methods
    function RequestAllocator(pPreferred: IMemAllocator;
      pProps: PAllocatorProperties;
      out ppActual: IMemAllocator): HResult; stdcall;
    function Request(pSample: IMediaSample; dwUser: DWORD): HResult; stdcall;
    function WaitForNext(dwTimeout: DWORD; out ppSample: IMediaSample;
      out pdwUser: DWORD): HResult; stdcall;
    function SyncReadAligned(pSample: IMediaSample): HResult; stdcall;
    function SyncRead(llPosition: int64; lLength: Longint;
      pBuffer: Pbyte): HResult; stdcall;
    function Length(out pTotal, pAvailable: int64): HResult; stdcall;
  public
    constructor Create(AStream: IStream; FwdOnly: boolean = false;
      const StreamSize: Int64 = 0; URLMode: boolean = false);
    // calling the destructor causes crashes
    destructor Destroy; override;
    // we use this function to detroy memeber objects
    procedure FreeAllObjects;
    // the graph object for full control during buffering URL stream
    procedure SetActiveGraph(var FilterGraph: IFilterGraph);
    procedure Addref;
    procedure Release;
    procedure Process;
    // IAsyncReader methods
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
    // FURLMode methods
    procedure Connect(Adress: string; Port: string;
      Location: string; MetaData: boolean);
  end;

implementation

uses WorkerThread, filter;

procedure TAsyncIO.setActiveGraph(var FilterGraph: IFilterGraph);
begin
  // In URlmode we need to control the Graph during buffering
  if (FURLMode) and (FMediaControl = nil) then
  begin
    FilterGraph.QueryInterface(IID_IMediaControl, FMediaControl);
    FTimerPlay := TTimer.Create(nil);
    FTimerPlay.Enabled := false;
    FTimerPlay.Interval := 1;
    // makes shure that run is always called after pause
    FTimerPlay.OnTimer := OnTimerPlay;
    FTimerPause := TTimer.Create(nil);
    FTimerPause.Enabled := false;
    FTimerPause.Interval := 1;
    FTimerPause.OnTimer := OnTimerPause;
  end;
end;

procedure TAsyncIO.Connect(Adress: string; Port: string; Location: string;
  MetaData: boolean);
begin
  GFExit := false;
  g_threadedShoutCastStream := TThreadedShoutcastStream.Create(Adress, Port,
    Location, MetaData);
end;

procedure TAsyncIO.Release;
begin
  FreeAllObjects;
 // _Release;
end;

procedure TAsyncIO.Addref;
begin
  _AddRef;
end;

constructor TAsyncIO.Create(AStream: IStream; FwdOnly: boolean = false;
  const StreamSize: Int64 = 0; URLMode: boolean = false);
begin
  inherited Create;
  FTimerPlay := nil;
  if g_threadedShoutCastStream <> nil then
  begin
    g_threadedShoutCastStream.Destroy;
    g_threadedShoutCastStream := nil;
  end;
  FURLMode := URLMode;
  FStream := AStream;
  FListsLock := TBCCritSec.Create;
  FReaderLock := TBCCritSec.Create;
  FWorkList := TQueue.Create;
  FDoneList := TQueue.Create;
  FWorkEvent := TBCAMEvent.Create(true);
  FDoneEvent := TBCAMEvent.Create(true);
  FAllDoneEv := TBCAMEvent.Create(true);
  FFwdStream := FwdOnly;
  FStrmSize := StreamSize;
  FWorkEvent.Reset;
  FThread := TWorkThread.Create(Self);
  FThread.Resume;
end;

procedure TAsyncIO.FreeAllObjects;
var
  Req: PAsyncRequest;
begin
  if g_threadedShoutCastStream <> nil then
  begin
    g_threadedShoutCastStream.Destroy;
    g_threadedShoutCastStream := nil;
  end;
  if GFStringQueue <> nil then
  begin
    GFStringQueue.destroy;
    GFStringQueue := nil;
  end;
  FStop := true;
  FThread.Terminate;
  FWorkEvent.SetEv;
  FThread.WaitFor;
  FThread.Free;
  Req := GetDoneItem;
  while Req <> nil do
  begin
    Dispose(Req);
    Req := GetDoneItem;
  end;
//  FStream := nil; // crashes...
  FReaderLock.Free;
  FListsLock.Free;
  FWorkList.Free;
  FDoneList.Free;
  FWorkEvent.Free;
  FDoneEvent.Free;
  FAllDoneEv.Free;
  FTimerPlay.Free;
  FTimerPause.Free;
end;

destructor TAsyncIO.Destroy;
begin
  inherited destroy;
end;

function TAsyncIO.BeginFlush: HRESULT;
var  
  Req: PAsyncRequest;
begin
//  if FMediaControl <> nil then
  GFExit := true;
 // Application.HandleMessage;
  { need to nil here IMediaControl,
    if not, the destructor in TFilter will not executed }
  FListsLock.Lock;
  FMediaControl := nil;
  Result := S_OK;
  // we nil here and in the filter destructor
  if g_threadedShoutCastStream <> nil then
  begin
    g_threadedShoutCastStream.Destroy;
    g_threadedShoutCastStream := nil;
  end;
  if GFStringQueue <> nil then
  begin
    GFStringQueue.destroy;
    GFStringQueue := nil;
  end;
  try
    FFlushing := true;
    Req := GetWorkItem;
    while Req <> nil do
    begin
      PutDoneItem(Req);
      Req := GetWorkItem;
    end;
    if FOutCount > 0 then
    begin
      Assert(not FWaiting);
      FAllDoneEv.Reset;
      FWaiting := true;
    end
    else
    begin
      FDoneEvent.SetEv;
      FWorkEvent.SetEv;
    end;
  finally
    FListsLock.UnLock;
  end;
//  Assert(FWaiting);
  while FWaiting do
  begin
    FAllDoneEv.Wait();
    FListsLock.Lock;
    try
      if FOutCount = 0 then
      begin
        FWaiting := false;
        FDoneEvent.SetEv;
      end;
    finally
      FListsLock.UnLock;
    end;
  end;
end;

function TAsyncIO.EndFlush: HRESULT;
begin
  GFExit := true;
  FListsLock.Lock;
  FFlushing := false;
  Assert(not FWaiting);

  if FDoneList.Count > 0 then
    FDoneEvent.SetEv
  else
    FDoneEvent.Reset;

  Result := S_OK;
  FListsLock.UnLock;
end;

procedure TAsyncIO.Process;
var
  Req: PAsyncRequest;
begin
  while true do
  begin
    FWorkEvent.Wait;
    FListsLock.Lock;
    Req := GetWorkItem;
    if Req <> nil then
      Inc(FOutCount);
    FListsLock.UnLock;

    if Req <> nil then
    begin
      CompleteRequest(Req);
      FListsLock.Lock;
      PutDoneItem(Req);
      Dec(FOutCount);
      if (FOutCount = 0) and FWaiting then
        FAllDoneEv.SetEv;
      FListsLock.UnLock;
    end;
    if FStop then
      break;
  end;
end;

function TAsyncIO.DoRequest(
  llPos: LONGLONG; lLength: Integer; bAligned: BOOL; pBuffer,
  pContext: Pointer; dwUser: DWORD): HResult;
var
  Req: PAsyncRequest;
begin
  Req := CreateRequest(llPos, lLength, bAligned, pBuffer, pContext, dwUser);
  Result := PutWorkItem(Req);
  if not Succeeded(Result) then
    Dispose(Req);
end;

function TAsyncIO.DoWaitForNext(dwTimeout: DWORD; var ppContext: Pointer;
  var pdwUser: DWORD; var pcbActual: Integer): HRESULT;
var
  Req: PAsyncRequest;
begin
  Result := S_OK;
  ppContext := nil;
  pdwUser := 0;
  pcbActual := 0;
  while true do
  begin
    if (not FDoneEvent.Wait(dwTimeout)) then
    begin
      Result := VFW_E_TIMEOUT;
      Break;
    end;
    Req := GetDoneItem;
    if Req <> nil then
    begin
      ppContext := Req.FContext;
      pdwUser := Req.FUser;
      pcbActual := Req.FLength;
      Result := Req.Fhr;
      Dispose(Req);
      Break;
    end
    else
    begin
      FListsLock.Lock;
      try
        if FFlushing {and not FWaiting} then
        begin
          Result := VFW_E_WRONG_STATE;
          Break;
        end;
      finally
        FListsLock.UnLock;
      end;
    end;
  end;
end;

procedure TAsyncIO.OnTimerPlay(Sender: TObject);
begin
  if FMediaControl <> nil then
    FMediaControl.Run;
  FTimerPlay.Enabled := false;
end;

procedure TAsyncIO.OnTimerPause(Sender: TObject);
begin
  if FMediaControl <> nil then
    FMediaControl.Pause;
  FTimerPause.Enabled := false;
end;

function TAsyncIO.SyncRead(llPosition: int64; lLength: Longint;
  pBuffer: Pbyte): HResult;
var
  Req: PAsyncRequest;
  DataWritten: boolean;
  i: integer;
  StringStream: TStringStream;
  Buffer: string;
  Tempbuffer: string;
  Avdata: int64;
  Application: TApplication;
  Buffering: boolean;
  Count: integer;
begin
  // we do not accept a Nil buffer
  if pBuffer = nil then
  begin
    result := E_FAIL;
    exit;
  end;
  Result := S_OK;
  // the URL buffer control for Dirctshow is added here
  // buffering during the playback
  if FURLMode then
  begin
    // the min. buffersize must be equal to the requested length
    if GFBufferSize < lLength then
      GFBufferSize := lLength;
    // Mpeg1 splitter requests same samples during connection process and
    // after starting the graph.
    StringStream := nil;
    GFStreamPos := llPosition;
    DataWritten := false;
    Buffer := '';
    Tempbuffer := '';
    Avdata := 0;
    Buffering := false;
    Count := 0;
    Application := TApplication.Create(nil);
    if not GFConnected then
    begin
      if assigned(GFFilterCallBack) then
        GFFilterCallBack.AsyncExFilterState(false, false, true, false, 0);

      // since XP ServicePack2 rc2 the mpeg splitter requests a end sample
      // of the stream during pin connection process,
      // we skip this sample because we can't send it
      if (llPosition > (GCFInt64max - lLength - 2)) then
      begin
        result := E_FAIL;
        exit;
      end;
      i := 0;
      if GFStringQueue = nil then
      begin
        result := E_FAIL;
        exit;
      end;
      while not Datawritten do
      begin
        if GFStringQueue <> nil then
          Count := GFStringQueue.getcount;
        if ((GFExit) or (GFStringQueue = nil) or (Count <= i)) then
        begin
          Application.Destroy;
          if g_threadedShoutCastStream <> nil then
          begin
            g_threadedShoutCastStream.Destroy;
            g_threadedShoutCastStream := nil;
          end;
          if GFStringQueue <> nil then
          begin
            if assigned(GFFilterCallBack) then
              GFFilterCallBack.AsyncExSockError('Your prebuffer is too small for the pin connection process. Raise the pebuffer!')
            else
              ShowMessage('TAsyncIO.SyncRead: Your prebuffer is too small for the pin connection process. Raise the prebuffer!');
            GFStringQueue.Destroy;
            GFStringQueue := nil;
          end;
          result := E_FAIL;
          exit;
        end;
        Buffer := Buffer + GFStringQueue.getitem(i);
        inc(i);
        if (llPosition + lLength <= Avdata) then
        begin
          StringStream := TStringStream.Create(Buffer);
          StringStream.Position := llPosition;
          Result := StringStream.Read(pBuffer^, lLength);
          freeandnil(StringStream);
          break;
        end
        else
          Avdata := system.length(Buffer);
      end;
    end
    else
    begin
      if assigned(GFFilterCallBack) then
        GFFilterCallBack.AsyncExFilterState(false, false, false, true, 0);
      while not Datawritten do
      begin
        // we need to free some cpu time for other processes -> sleep(1)
        Sleep(1);
        if GFExit then
        begin
          result := E_FAIL;
          Application.destroy;
          if GFStringQueue <> nil then
          begin
            GFStringQueue.Destroy;
            GFStringQueue := nil;
          end;
          exit;
        end;
        while not Buffering do
        begin
          // we need to free some cpu time for other processes -> sleep(1)
          Sleep(1);
          if GFExit then
          begin
            result := E_FAIL;
            Application.destroy;
            if GFStringQueue <> nil then
            begin
              GFStringQueue.Destroy;
              GFStringQueue := nil;
            end;
            exit;
          end;
          Application.ProcessMessages;
          // we needed to process the onsock read events
        // during waiting for the data
          while (llength > Avdata) do
          begin
            // we need to free some cpu time for other processes -> sleep(1)
            Sleep(1);
            if GFExit then
            begin
              result := E_FAIL;
              Application.destroy;
              if GFStringQueue <> nil then
              begin
                GFStringQueue.Destroy;
                GFStringQueue := nil;
              end;
              exit;
            end;
            Application.ProcessMessages;
            // we needed to process the onsock read events
          // during waiting for the data
            if GFStringQueue.getcount > 0 then
            begin
              Buffer := Buffer + GFStringQueue.pop;
              Avdata := system.length(Buffer);
            end
            else
            begin
              Buffering := true;
              if (FTimerPause <> nil) then
                FTimerPause.Enabled := true;
              break;
            end;
          end;
          if (llength <= Avdata) then
          begin
            StringStream := TStringStream.Create(Buffer);
            StringStream.Position := 0;
            Result := StringStream.Read(pBuffer^, llength);
            freeandnil(StringStream);
            if (Avdata - llength > 0) then
            begin
              Tempbuffer := copy(Buffer, llength + 1, system.length(Buffer));
              GFStringQueue.InsertItem(Tempbuffer, 0);
            end;
            Application.Destroy;
            if assigned(GFFilterCallBack) then
              GFFilterCallBack.AsyncExFilterState(false, false, false, true, 0);
            // we can not call Fmediacontrol.play directly at this point,
            // because destroy in uniot Filter won't called if we do,
            // so we call the Fmediacontrol.play via a timer control
            if (FTimerPlay <> nil) then
              FTimerPlay.Enabled := true;
            exit;
          end;
        end;
        if assigned(GFFilterCallBack) then
          GFFilterCallBack.AsyncExFilterState(true, false, false, false,
            trunc((Avdata * 100) / (GFBufferSize)));
        if GFStringQueue.getcount > 0 then
          Buffer := Buffer + GFStringQueue.pop;
        Avdata := system.length(Buffer);
        if ((GFBufferSize) <= Avdata) then
        begin
          if assigned(GFFilterCallBack) then
            GFFilterCallBack.AsyncExFilterState(true, false, false, false, 100);
          StringStream := TStringStream.Create(Buffer);
          StringStream.Position := 0;
          Result := StringStream.Read(pBuffer^, llength);
          freeandnil(StringStream);
          if (Avdata - llength > 0) then
          begin
            Tempbuffer := copy(Buffer, llength + 1, system.length(Buffer));
            GFStringQueue.InsertItem(Tempbuffer, 0);
          end;
          if assigned(GFFilterCallBack) then
            GFFilterCallBack.AsyncExFilterState(false, false, false, true, 0);
          if (FTimerPlay <> nil) then
            FTimerPlay.Enabled := true;
          break;
        end;
      end;
    end;
    Application.Destroy;
  end
  else
  begin
    FListsLock.Lock;
    try
      if FFlushing then
        Result := VFW_E_WRONG_STATE
      else
      begin
        Req := CreateRequest(llPosition, lLength, false, pBuffer, nil, 0);
        CompleteRequest(Req);
        Result := Req.Fhr;
        Dispose(Req);
      end;
    finally
      FListsLock.UnLock;
    end;
  end;
end;

function TAsyncIO.PutWorkItem(AItem: PAsyncRequest): HRESULT;
begin
  FListsLock.Lock;
  try
    if FFlushing then
      Result := VFW_E_WRONG_STATE
    else
    begin
      FWorkList.Push(AItem);
      FWorkEvent.SetEv;
      Result := S_OK;
    end;
  finally
    FListsLock.UnLock;
  end;
end;

function TAsyncIO.GetWorkItem: PAsyncRequest;
begin
  FListsLock.Lock;
  Result := FWorkList.Pop;
  if FWorkList.Count = 0 then
    FWorkEvent.Reset;
  FListsLock.UnLock;
end;

function TAsyncIO.GetDoneItem: PAsyncRequest;
begin
  FListsLock.Lock;
  Result := FDoneList.Pop;
  if (FDoneList.Count = 0) and (not FFlushing or FWaiting) then
    FDoneEvent.Reset;
  FListsLock.UnLock;
end;

procedure TAsyncIO.PutDoneItem(AItem: PAsyncRequest);
begin
  Assert(FListsLock.CritCheckIn);
  FDoneList.Push(AItem);
  FDoneEvent.SetEv;
end;

function TAsyncIO.Length(out pTotal, pAvailable: int64): HResult;
begin
  FReaderLock.Lock;
  try
    if FURLMode then
    begin
      // we return the max int64 value
      pTotal := GCFInt64max;
      GFStreamLength := pTotal;
      FStrmSize := pTotal;
      Result := S_OK; //VFW_S_ESTIMATED;
    end
    else
    begin
      if FStrmSize = 0 then
        InitStreamLen;
      pTotal := FStrmSize;
      GFStreamLength := FStrmSize;
      pAvailable := pTotal;
      Result := S_OK;
      exit;
    end;
  finally
    FReaderLock.UnLock;
  end;
end;

function TAsyncIO.SetPosition(const APos: Int64): HResult;
var
  CPos: Int64;
begin
  FReaderLock.Lock;
  Result := S_OK;
  try
    if FStrmSize = 0 then
      InitStreamLen;
    CPos := GetStreamPos;
    if not FFwdStream then
    try
      if CPos <> APos then
        Result := SetStreamPos(APos);
    except
      //sometimes it's not working
      //try from the begining
      Result := S_FALSE;
    end
    else
    begin
      try
        if Apos <> CPos then
        begin
          if APos < CPos then
            SetStreamPos(0);
          Result := SetStreamPos(APos);
        end;
      except
        Result := S_FALSE;
      end;
    end;
  finally
    FReaderLock.UnLock;
  end;
end;

procedure TAsyncIO.InitStreamLen;
begin
  if not FFwdStream then
  try
    FFwdStream := FStream.Seek(0, STREAM_SEEK_END, FStrmSize) <> S_OK;
  except
    FStrmSize := 0;
    FFwdStream := true;
  end;
  if FFwdStream then
  try
    SetStreamPos(0);
    FStrmSize := 32768;
    try
      while SetStreamPos(FStrmSize) = S_OK do
        FStrmSize := 2 * FStrmSize;
    except
    end;
    FStrmSize := GetStreamPos;
    SetStreamPos(0);
  except
    FStrmSize := 10000; //fake
  end;
end;

function TAsyncIO.GetStreamPos: Int64;
begin
  FStream.Seek(0, STREAM_SEEK_CUR, Result);
  GFStreamPos := Result;
end;

function TAsyncIO.SetStreamPos(const APos: Int64): HResult;
var
  NewPos: Int64;
begin
  Result := FStream.Seek(APos, STREAM_SEEK_SET, NewPos);
end;

procedure TAsyncIO.CompleteRequest(Req: PAsyncRequest);
var
  R: integer;
begin
  FReaderLock.Lock;
  with Req^ do
  try
    Fhr := SetPosition(FPos);
    R := 0;
    if Fhr = S_OK then
    begin
      Fhr := FStream.Read(FBuffer, FLength, @R);
      if FLength <> R then
      begin
        Fhr := S_FALSE;
        FLength := R;
      end;
    end;
  finally
    FReaderLock.UnLock;
  end;
end;

function TAsyncIO.CreateRequest(
  llPos: LONGLONG; lLength: Integer; bAligned: BOOL; pBuffer,
  pContext: Pointer; dwUser: DWORD): PAsyncRequest;
begin
  New(Result);
  with Result^ do
  begin
    FPos := llPos;
    FAligned := bAligned;
    FLength := lLength;
    FBuffer := pBuffer;
    FContext := pContext;
    FUser := dwUser;
    Fhr := VFW_E_TIMEOUT;
  end;
end;

function TAsyncIO.InitAllocator(out Alloc: IMemAllocator): HRESULT;
begin
  Result := CoCreateInstance(CLSID_MemoryAllocator, nil, CLSCTX_INPROC_SERVER,
    IID_IMemAllocator, Alloc);
end;

function TAsyncIO.WaitForNext(dwTimeout: DWORD; out ppSample: IMediaSample;
  out pdwUser: DWORD): HResult;
var
  cbActual: Longint;
begin
  result := DoWaitForNext(dwTimeout, Pointer(ppSample), pdwUser, cbActual);
end;

function TAsyncIO.RequestAllocator(pPreferred: IMemAllocator;
  pProps: PAllocatorProperties; out ppActual: IMemAllocator): HResult; stdcall;
var
  P, PA: TAllocatorProperties;
begin
  P := pProps^;
  P.cbAlign := 1;
  if pPreferred <> nil then
  begin
    Result := pPreferred.SetProperties(P, PA);
    if Succeeded(Result) and (P.cbAlign = PA.cbAlign) then
    begin
      ppActual := pPreferred;
      exit;
    end;
  end;
  InitAllocator(ppActual);
  Result := ppActual.SetProperties(P, PA);
  if Succeeded(Result) and (P.cbAlign = PA.cbAlign) then
  begin
    Result := S_OK;
    exit;
  end;
  if Succeeded(Result) then
    Result := VFW_E_BADALIGN;

  ppActual := nil;
end;

function TAsyncIO.SyncReadAligned(pSample: IMediaSample): HResult;
var
  T1, T2: TReferenceTime;
  Start, Total: LONGLONG;
  Length: Longint;
  Buffer: PByte;
begin
  pSample.GetTime(T1, T2);
  if not FURLMode then
    Self.Length(Total, Start)
  else
    Buffer := nil;
  Start := T1 div NANOSECONDS;
  Length := (T2 - T1) div NANOSECONDS;

  if not FURLMode then
    if Start + Length > Total then
    begin
      Length := Total - Start;
      T2 := Total * NANOSECONDS;
      pSample.SetTime(@T1, @T2);
    end;

  Result := pSample.GetPointer(Buffer);
  if (FAILED(Result)) then
    exit;

  Result := SyncRead(Start, Length, Buffer);
end;

function TAsyncIO.Request(pSample: IMediaSample; dwUser: DWORD): HResult;
var
  T1, T2: TReferenceTime;
  Start, Total: LONGLONG;
  Length: Longint;
  Buffer: PByte;
begin
  pSample.GetTime(T1, T2);
  self.Length(Total, Start);
  Start := T1 div NANOSECONDS;
  Length := (T2 - T1) div NANOSECONDS;

  if Start + Length > Total then
  begin
    Length := Total - Start;
    T2 := Total * NANOSECONDS;
    pSample.SetTime(@T1, @T2);
  end;

  Result := pSample.GetPointer(Buffer);
  if (FAILED(Result)) then
    exit;

  Result := DoRequest(Start, Length,
    false, Buffer, Pointer(pSample), dwUser);
end;

end.

