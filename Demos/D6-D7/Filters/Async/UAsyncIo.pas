//------------------------------------------------------------------------------
// File: UAsyncIO.pas
// Original files: asyncio.h, asyncio.c
//
// Desc: Base library with I/O functionality.
//
// Portions created by Microsoft are
// Copyright (c) 2000-2002  Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
unit UAsyncIo;

interface
uses
  Windows, Contnrs, BaseClass, DirectShow9, DSUtil;
type
  //
  // definition of CAsyncFile object that performs file access. It provides
  // asynchronous, unbuffered, aligned reads from a file, using a worker thread
  // on win95 and potentially overlapped i/o if available.

  // !!! Need to use real overlapped i/o if available
  // currently only uses worker thread, not overlapped i/o


  TBCAsyncIo = class;
  TBCAsyncStream = class;

  LONGLONG = Int64;
  PLONGLONG = ^LONGLONG;
  //
  //  Model the stream we read from based on a file-like interface
  //
  TBCAsyncStream = class
  public
    function SetPointer(APos: LONGLONG): HResult; virtual; abstract;
    function Read(ABuffer: PByte; ABytesToRead: DWord;
      AAlign: Boolean; out ABytesRead: DWord): HResult; virtual; abstract;

    function Size(out AAvailable: LONGLONG): LONGLONG; overload; virtual; abstract;
    function Size: LONGLONG; overload; virtual;
    function Alignment: DWord; virtual; abstract;
    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;
    //procedure SetStopHandle(hevStop: THandle); virtual
  end;

  // represents a single request and performs the i/o. Can be called on either
  // worker thread or app thread, but must hold pcsFile across file accesses.
  // (ie across SetFilePointer/ReadFile pairs)
  TBCAsyncRequest = class
  private
    FIO: TBCAsyncIo;
    FStream: TBCAsyncStream;
    FPos: LONGLONG;
    FAligned: Boolean;
    FLength: Integer;
    FBuffer: PByte;
    FContext: Pointer;
    FUser: DWord;
    Fhr: HResult;

  public
    // init the params for this request. Issue the i/o
    // if overlapped i/o is possible.
    function Request(AIO: TBCAsyncIo; AStream: TBCAsyncStream;
      APos: LONGLONG; ALength: Integer; AAligned: Boolean;
      ABuffer: PByte;
      // filter's context
      AContext: Pointer;
      // downstream filter's context
      AUser: DWord): HResult;

    // issue the i/o if not overlapped, and block until i/o complete.
    // returns error code of file i/o
    function Complete: HResult;

    // cancels the i/o. blocks until i/o is no longer pending
    function Cancel: HResult;

    // accessor functions
    function GetContext: Pointer;

    function GetUser: DWord;

    function GetHResult: HResult;

    // we set FLength to the actual length
    function GetActualLength: Integer;

    function GetStart: LONGLONG;
  end;

  TBCRequestList = class(TObjectList);

  // this class needs a worker thread, but the ones defined in classes\base
  // are not suitable (they assume you have one message sent or posted per
  // request, whereas here for efficiency we want just to set an event when
  // there is work on the queue).
  //
  // we create CAsyncRequest objects and queue them on m_listWork. The worker
  // thread pulls them off, completes them and puts them on m_listDone.
  // The events m_evWork and m_evDone are set when the corresponding lists are
  // not empty.
  //
  // Synchronous requests are done on the caller thread. These should be
  // synchronised by the caller, but to make sure we hold m_csFile across
  // the SetFilePointer/ReadFile code.
  //
  // Flush by calling BeginFlush. This rejects all further requests (by
  // setting m_bFlushing within m_csLists), cancels all requests and moves them
  // to the done list, and sets m_evDone to ensure that no WaitForNext operations
  // will block. Call EndFlush to cancel this state.
  //
  // we support unaligned calls to SyncRead. This is done by opening the file
  // twice if we are using unbuffered i/o (m_dwAlign > 1).
  // !!!fix this to buffer on top of existing file handle?
  TBCAsyncIo = class
  private
    FReader: TBCCritSec;
    FStream: TBCAsyncStream;

    // locks access to the list and events
    FCSLists: TBCCritSec;
    // true if between BeginFlush/EndFlush
    FFlushing: Boolean;

    FListWork: TBCRequestList;
    FListDone: TBCRequestList;

    // set when list is not empty
    FOnWork: TBCAMEvent;
    FOnDone: TBCAMEvent;

    // for correct flush behaviour: all protected by m_csLists
    // nr of items not on listDone or listWork
    FItemsOut: Integer;
    // TRUE if someone waiting for m_evAllDone
    FWaiting: Boolean;
    // signal when FItemsOut goes to 0 if FWaiting
    FOnAllDone: TBCAMEvent;

    // set when thread should exit
    FOnStop: TBCAMEvent;
    FThread: THandle;
    FThreadProc: TThreadProc;

    function Size: LONGLONG;

    // start the thread
    function StartThread: HResult;

    // stop the thread and close the handle
    function CloseThread: HResult;

    // manage the list of requests. hold m_csLists and ensure
    // that the (manual reset) event hevList is set when things on
    // the list but reset when the list is empty.
    // returns null if list empty
    function GetWorkItem: TBCAsyncRequest;

    // get an item from the done list
    function GetDoneItem: TBCAsyncRequest;

    // put an item on the work list
    function PutWorkItem(ARequest: TBCAsyncRequest): HResult;

    // put an item on the done list
    function PutDoneItem(ARequest: TBCAsyncRequest): HResult;

    // called on thread to process any active requests
    // ??? void ProcessRequests(void);
    procedure ProcessRequests;

    function ThreadProc: DWord; virtual;
  public

    constructor Create(AStream: TBCAsyncStream);
    destructor Destroy; override;

    // open the file
    function Open(AName: PChar): HResult; virtual;

    // ready for async activity - call this before
    // calling Request
    function AsyncActive: HResult;

    // call this when no more async activity will happen before
    // the next AsyncActive call
    function AsyncInactive: HResult;

    // queue a requested read. must be aligned.
    function Request(APos: LONGLONG; ALength: Integer;
      AAligned: Boolean; ABuffer: PByte;
      AContext: Pointer; AUser: DWord): HResult;

    // wait for the next read to complete
    function WaitForNext(ATimeout: DWord; AContext: PPointer;
      out AUser: DWord; out AActual: Integer): HResult;

    // perform a read of an already aligned buffer
    function SyncReadAligned(APos: LONGLONG; ALength: Integer;
      ABuffer: PByte; out AActual: Integer; AContext: Pointer): HResult;

    // perform a synchronous read. will be buffered
    // if not aligned.
    function SyncRead(APos: LONGLONG; ALength: Integer;
      ABuffer: PByte): HResult;

    // return length
    function Length(out ATotal: LONGLONG;
      out AAvailable: LONGLONG): HResult;

    // all Reader positions, read lengths and memory locations must
    // be aligned to this.
    function Alignment(out Al: Integer): HResult; overload;

    function BeginFlush: HResult;
    function EndFlush: HResult;

    function Alignment: Integer; overload;

    function IsAligned(Al: Integer): Boolean; overload;
    function IsAligned(Al: LONGLONG): Boolean; overload;

    //  Accessor
    function StopEvent: THandle;
  end;

implementation

// initial static thread proc calls ThreadProc with DWORD
// param as this
function AsyncIoInitialThreadProc(pv: Pointer): DWord; stdcall;
begin
  Result := TBCAsyncIo(pv).ThreadProc;
end;

// --- TBCAsyncStream ---

function TBCAsyncStream.Size: LONGLONG;
var
  Available: LONGLONG;
begin
  Result := Size(Available);
end;

// --- TBCAsyncRequest ---

function TBCAsyncRequest.Request(AIO: TBCAsyncIo; AStream: TBCAsyncStream;
  APos: LONGLONG; ALength: Integer; AAligned: Boolean;
  ABuffer: PByte; AContext: Pointer; AUser: DWord): HResult;
begin
  FIo     := AIo;
  FStream := AStream;
  FPos    := APos;
  FLength := ALength;
  FAligned:= AAligned;
  FBuffer := ABuffer;
  FContext:= AContext;
  FUser   := AUser;
  Fhr     := VFW_E_TIMEOUT;   // not done yet

  Result  := S_OK;
end;

function TBCAsyncRequest.Complete: HResult;
var
  Actual: DWord;
  Sample: IMediaSample;
begin
  FStream.Lock;
  try
    Fhr := FStream.SetPointer(FPos);
    if (S_OK = Fhr) then
    begin
      Fhr := FStream.Read(FBuffer, FLength, FAligned, Actual);
      if (Fhr = OLE_S_FIRST) then
      begin
        if Assigned(FContext) then
        begin
          Sample := IMediaSample(FContext);
          Sample.SetDiscontinuity(True);
          Fhr := S_OK;
        end;
      end;

      if (Failed(Fhr)) then
      else
        if (Actual <> DWord(FLength)) then
        begin
          // tell caller size changed - probably because of EOF
          FLength := Integer(Actual);
          Fhr := S_FALSE;
        end
        else
          Fhr := S_OK;
    end;

  finally
    FStream.Unlock;
    Result := Fhr;
  end;
end;

function TBCAsyncRequest.Cancel: HResult;
begin
  Result := S_OK;
end;

function TBCAsyncRequest.GetContext: Pointer;
begin
  Result := FContext;
end;

function TBCAsyncRequest.GetUser: DWord;
begin
  Result := FUser;
end;

function TBCAsyncRequest.GetHResult: HResult;
begin
  Result := Fhr;
end;

function TBCAsyncRequest.GetActualLength: Integer;
begin
  Result := FLength;
end;

function TBCAsyncRequest.GetStart: LONGLONG;
begin
  Result := FPos;
end;

// --- TBCAsyncIo ---

constructor TBCAsyncIo.Create(AStream: TBCAsyncStream);
begin
  FReader := TBCCritSec.Create;
  FStream := AStream;

  FCSLists := TBCCritSec.Create;
  FFlushing := False;

  FListWork := TBCRequestList.Create;
  FListWork.OwnsObjects := False;
  FListDone := TBCRequestList.Create;
  FListDone.OwnsObjects := False;

  FOnWork := TBCAMEvent.Create(True);
  FOnDone := TBCAMEvent.Create(True);
  FOnAllDone := TBCAMEvent.Create(True);
  FOnStop := TBCAMEvent.Create(True);

  FItemsOut := 0;
  FWaiting := False;

  // set when thread should exit
  FThread := 0;
  FThreadProc := nil;
end;

destructor TBCAsyncIo.Destroy;
begin
  // move everything to the done list
  BeginFlush();

  // shutdown worker thread
  CloseThread();

  // empty the done list
  FListDone.Clear;
  FListDone.Free;
  FListDone := nil;
end;

function TBCAsyncIo.Open(AName: PChar): HResult;
begin
  Result := NOERROR;
end;

function TBCAsyncIo.Size: LONGLONG;
begin
  Assert(Assigned(FStream));
  Result := FStream.Size;
end;

function TBCAsyncIo.StartThread: HResult;
var
  dwThreadID, dwErr: DWord;
begin
  if (FThread <> 0) then
  begin
    Result := S_OK;
    Exit;
  end;

  // clear the stop event before starting
  FOnStop.Reset;

  FThread := CreateThread(nil, 0, @AsyncIoInitialThreadProc,
    Self, 0, dwThreadID);
  if (FThread = 0) then
  begin
    dwErr := GetLastError;
    Result := HResultFromWin32(dwErr);
    Exit;
  end;

  Result := S_OK;
end;

function TBCAsyncIo.CloseThread: HResult;
begin
  // signal the thread-exit object
  FOnStop.SetEv;

  if (FThread <> 0) then
  begin
    WaitForSingleObject(FThread, INFINITE);
    CloseHandle(FThread);
    FThread := 0;
  end;

  Result := S_OK;
end;

function TBCAsyncIo.GetWorkItem: TBCAsyncRequest;
var
  Req: TBCAsyncRequest;
begin
//  Result := nil;
  Req := nil;

  FCSLists.Lock;
  with FListWork do
  try
    if (Count <> 0) then
    begin
      Req := TBCAsyncRequest(Items[0]);
      Delete(0);
    end;

    // force event set correctly
    if (Count = 0) then
      FOnWork.Reset;

    Result := Req;
  finally
    FCSLists.UnLock;
  end;
end;

function TBCAsyncIo.GetDoneItem: TBCAsyncRequest;
var
  Req: TBCAsyncRequest;
begin
//  Result := nil;
  Req := nil;

  FCSLists.Lock;
  with FListDone do
  try
    if (Count <> 0) then
    begin
      Req := TBCAsyncRequest(Items[0]);
      Delete(0);
    end;

    Result := Req;
    // force event set correctly if list now empty
    // or we're in the final stages of flushing
    // Note that during flushing the way it's supposed to work is that
    // everything is shoved on the Done list then the application is
    // supposed to pull until it gets nothing more
    //
    // Thus we should not set m_evDone unconditionally until everything
    // has moved to the done list which means we must wait until
    // cItemsOut is 0 (which is guaranteed by m_bWaiting being TRUE).
    if (Count = 0) and ((Not FFlushing) or FWaiting) then
      FOnDone.Reset;
  finally
    FCSLists.UnLock;
  end;
end;

function TBCAsyncIo.PutWorkItem(ARequest: TBCAsyncRequest): HResult;
begin
  FCSLists.Lock;
  try
    if (FFlushing) then
      Result := VFW_E_WRONG_STATE
    else
      try
        FListWork.Add(ARequest);
        // event should now be in a set state - force this
        FOnWork.SetEv;

        // start the thread now if not already started
        Result := StartThread;
      except
        Result := E_OUTOFMEMORY;
      end;
  finally
    FCSLists.UnLock;
  end;
end;

function TBCAsyncIo.PutDoneItem(ARequest: TBCAsyncRequest): HResult;
begin
  // put an item on the done list - ok to do this when
  // flushing
  Assert(FCSLists.CritCheckIn);

  try
    FListDone.Add(ARequest);
    // event should now be in a set state - force this
    FOnDone.SetEv;
    Result := S_OK;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

procedure TBCAsyncIo.ProcessRequests;
var
  Req: TBCAsyncRequest;
begin
  // lock to get the item and increment the outstanding count

  repeat
    FCSLists.Lock;
    try
      Req := GetWorkItem;
      if (Req = nil) then
        // done
        Exit;
      // one more item not on the done or work list
      Inc(FItemsOut);
    finally
      FCSLists.UnLock;
    end;

    Req.Complete;

    // regain critsec to replace on done list
    FCSLists.Lock;
    try
      PutDoneItem(Req);
      Dec(FItemsOut);
      if (FItemsOut = 0) then
        if (FWaiting) then
          FOnAllDone.SetEv;
    finally
      FCSLists.UnLock;
    end;
  until False;
end;

function TBCAsyncIo.ThreadProc: DWord;
const
  EvCount = 2;
var
  Events: array[0..EvCount - 1] of THandle;
begin
  // the thread proc - assumes that DWORD thread param is the
  // Self pointer
  Events[0] := FOnStop.Handle;
  Events[1] := FOnWork.Handle;

  repeat
    case WaitForMultipleObjects(2, @Events, False, Infinite) of
    WAIT_OBJECT_0+1:
      // requests need processing
      ProcessRequests;
    else
      begin
        // any error or stop event - we should exit
        Result := 0;
        Exit;
      end;
    end;
  until False;
end;

function TBCAsyncIo.AsyncActive: HResult;
begin
  Result := StartThread;
end;

function TBCAsyncIo.AsyncInactive: HResult;
begin
  Result := CloseThread;
end;

function TBCAsyncIo.Request(APos: LONGLONG; ALength: Integer;
  AAligned: Boolean; ABuffer: PByte;
  AContext: Pointer; AUser: DWord): HResult;
var
  Request: TBCAsyncRequest;
begin
  if AAligned then
  begin
    if (Not IsAligned(APos)) or (Not IsAligned(ALength)) or
      (Not IsAligned(Integer(ABuffer))) then
    begin
      Result := VFW_E_BADALIGN;
      Exit;
    end;
  end;

  try
    Request := TBCAsyncRequest.Create;
  except
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  Result := Request.Request(Self, FStream, APos, ALength,
    AAligned, ABuffer, AContext, AUser);
  if (Succeeded(Result)) then
  begin
    // might fail if flushing
    Result := PutWorkItem(Request);
  end;
  if Failed(Result) then
    Request.Free;
end;

function TBCAsyncIo.WaitForNext(ATimeout: DWord; AContext: PPointer;
  out AUser: DWord; out AActual: Integer): HResult;
var
  Request: TBCAsyncRequest;
  hr: HResult;
begin
  Result := E_UNEXPECTED;

  if (AContext = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // some errors find a sample, others don't. Ensure that
  // *ppContext is NULL if no sample found
  AContext^ := nil;

  // wait until the event is set, but since we are not
  // holding the critsec when waiting, we may need to re-wait
  repeat
    if (Not FOnDone.Wait(ATimeout)) then
    begin
      // timeout occurred
      Result := VFW_E_TIMEOUT;
      Exit;
    end;

    // get next event from list
    Request := GetDoneItem;
    if Assigned(Request) then
    begin
      // found a completed request

      // check if ok
      hr := Request.GetHResult;
      if (hr = S_FALSE) then
      begin
        // this means the actual length was less than
        // requested - may be ok if he aligned the end of file
        if ((Request.GetActualLength +
          Request.GetStart) = Size) then
          hr := S_OK
        else
          // it was an actual read error
          hr := E_FAIL;
     end;

      // return actual bytes read
      AActual := Request.GetActualLength;

      // return his context
      AContext^ := Request.GetContext;
      AUser := Request.GetUser;

      Request.Free;
      Result := hr;
      Exit;
    end
    else
      try
        //  Hold the critical section while checking the list state
        FCSLists.Lock;
        if (FFlushing and (Not FWaiting)) then
        begin
          // can't block as we are between BeginFlush and EndFlush

          // but note that if m_bWaiting is set, then there are some
          // items not yet complete that we should block for.

          Result := VFW_E_WRONG_STATE;
          Exit;
        end;
      finally
        FCSLists.UnLock;
      end;
    // done item was grabbed between completion and
    // us locking m_csLists.
  until False;
end;

function TBCAsyncIo.SyncReadAligned(APos: LONGLONG; ALength: Integer;
  ABuffer: PByte; out AActual: Integer; AContext: Pointer): HResult;
var
  Request: TBCAsyncRequest;
begin
  if (Not IsAligned(APos)) or (Not IsAligned(ALength)) or
    (Not IsAligned(Integer(ABuffer))) then
  begin
    Result := VFW_E_BADALIGN;
    Exit;
  end;

  try
    Request := TBCAsyncRequest.Create;
  except
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  Result := Request.Request(Self, FStream, APos, ALength,
    True, ABuffer, AContext, 0);
  if Failed(Result) then
    Exit;

  Result := Request.Complete;

  // return actual data length
  AActual := Request.GetActualLength;

  Request.Free;
end;

function TBCAsyncIo.SyncRead(APos: LONGLONG; ALength: Integer;
  ABuffer: PByte): HResult;
var
  Unused: Integer;
  Req: TBCAsyncRequest;
begin
  // perform a synchronous read request on this thread.
  // may not be aligned - so we will have to buffer.
  if (IsAligned(APos) and IsAligned(ALength) and IsAligned(Integer(ABuffer))) then
  begin
    Result := SyncReadAligned(APos, ALength, ABuffer, Unused, nil);
    Exit;
  end;

  // not aligned with requirements - use buffered file handle.
  //!!! might want to fix this to buffer the data ourselves?
  Req := TBCAsyncRequest.Create;
  Result := Req.Request(Self, FStream, APos, ALength, False, ABuffer, nil, 0);
  if Failed(Result) then
    Exit;

  Result := Req.Complete;
  Req.Free;
end;

function TBCAsyncIo.Length(out ATotal: LONGLONG;
  out AAvailable: LONGLONG): HResult;
begin
  ATotal := FStream.Size(AAvailable);
  Result := S_OK;
end;

function TBCAsyncIo.Alignment(out Al: Integer): HResult;
begin
  Al := Alignment;
  Result := S_OK;
end;

// cancel all items on the worklist onto the done list
// and refuse further requests or further WaitForNext calls
// until the end flush
//
// WaitForNext must return with NULL only if there are no successful requests.
// So Flush does the following:
// 1. set m_bFlushing ensures no more requests succeed
// 2. move all items from work list to the done list.
// 3. If there are any outstanding requests, then we need to release the
//    critsec to allow them to complete. The m_bWaiting as well as ensuring
//    that we are signalled when they are all done is also used to indicate
//    to WaitForNext that it should continue to block.
// 4. Once all outstanding requests are complete, we force m_evDone set and
//    m_bFlushing set and m_bWaiting false. This ensures that WaitForNext will
//    not block when the done list is empty.
function TBCAsyncIo.BeginFlush: HResult;
var
  Req: TBCAsyncRequest;
begin
  Result := E_UNEXPECTED;
  
  // hold the lock while emptying the work list
  FCSLists.Lock;
  try
    // prevent further requests being queued.
    // Also WaitForNext will refuse to block if this is set
    // unless m_bWaiting is also set which it will be when we release
    // the critsec if there are any outstanding).
    FFlushing := True;

    repeat
      Req := GetWorkItem;
      if Not Assigned(Req) then
        Break;

      Req.Cancel;
      PutDoneItem(Req);
    until False;

    // now wait for any outstanding requests to complete
    if (FItemsOut > 0) then
      begin
          // can be only one person waiting
          Assert(Not FWaiting);

          // this tells the completion routine that we need to be
          // signalled via m_evAllDone when all outstanding items are
          // done. It also tells WaitForNext to continue blocking.
          FWaiting := True;
      end
    else
      begin
        // all done

        // force m_evDone set so that even if list is empty,
        // WaitForNext will not block
        // don't do this until we are sure that all
        // requests are on the done list.
        FOnDone.SetEv;
        Result := S_OK;
        Exit;
      end;
  finally
    FCSLists.UnLock;
  end;

  Assert(FWaiting);

  // wait without holding critsec
  repeat
    FOnAllDone.Wait;

    // hold critsec to check
    FCSLists.Lock;
    try
      if (FItemsOut = 0) then
      begin
        // now we are sure that all outstanding requests are on
        // the done list and no more will be accepted
        FWaiting := False;

        // force m_evDone set so that even if list is empty,
        // WaitForNext will not block
        // don't do this until we are sure that all
        // requests are on the done list.
        FOnDone.SetEv;

        Result := S_OK;
        Exit;
      end;
    finally
      FCSLists.UnLock;
    end;
  until False;
end;

function TBCAsyncIo.EndFlush: HResult;
begin
  FCSLists.Lock;
  try
    FFlushing := False;

    Assert(Not FWaiting);

    // m_evDone might have been set by BeginFlush - ensure it is
    // set IFF m_listDone is non-empty
    if (FListDone.Count > 0) then
      FOnDone.SetEv
    else
      FOnDone.Reset;

    Result := S_OK;
  finally
    FCSLists.UnLock;
  end;
end;

function TBCAsyncIo.Alignment: Integer;
begin
  Result := FStream.Alignment;
end;

function TBCAsyncIo.IsAligned(Al: Integer): Boolean;
begin
  Result := ((Al and (Alignment - 1)) = 0);
end;

function TBCAsyncIo.IsAligned(Al: LONGLONG): Boolean;
begin
  Result := IsAligned(Integer(Al and $FFFFFFFF));
end;

function TBCAsyncIo.StopEvent: THandle;
begin
  Result := FOnDone.Handle;
end;

end.
