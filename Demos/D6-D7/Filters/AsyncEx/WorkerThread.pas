unit WorkerThread;

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
@lastmod(23 May, 2005)
}

interface

uses Windows, Classes, Asyncreader, config, ShoutCastStream, forms, baseclass;

// TAsyncIO FilePlayback instance
type
  TWorkThread = class(TThread)
  private
    FIO: TAsyncIO;
  protected
    procedure Execute; override;
  public
    constructor Create(AIO: TAsyncIO);
  end;

  // This is the Thread instance of the TShoutcastStream class
type
  TThreadedShoutcastStream = class(TThread)
  private
    FMetaData: boolean;
    FExitThread: boolean;
    FTerminated: boolean;
    FRipStream: boolean;
    FFile: string;
    FPath: string;
    Factualpath: string;
    FLock: TBCCritSec;
    FAdress, FPort, FLocation: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Adress, Port, Location: string; MetaData: boolean);
    destructor Destroy; override;
    function SetRipStream(RipStream: boolean; Path: string;
      FileName: string): HRESULT;
    function GetRipStream(out RipStream: boolean; out Path: string): HRESULT;
  end;

var
  g_threadedShoutCastStream: TThreadedShoutcastStream;

implementation

constructor TWorkThread.Create(AIO: TAsyncIO);
begin
  inherited Create(True);
  FreeOnTerminate := false;
  FIO := AIO;
end;

procedure TWorkThread.Execute;
begin
  FIO.Process;
end;

destructor TThreadedShoutcastStream.Destroy;
var
  Application: TApplication;
begin // no need to protect this function
  // a protect will cause a deadlock !
  FExitThread := true;
  Application := TApplication.Create(nil);
  while not FTerminated do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  FLock.Free; // freeandnil seems to be more savety here
  inherited Destroy;
end;

// TThreadedShoutcastStream.get_ripStream is not implemented yet

function TThreadedShoutcastStream.GetRipStream(out RipStream: boolean;
  out Path: string): HRESULT;
//var l_ripstream: boolean;
//    l_path: string;
begin
  FLock.Lock; // protect our member objects
  {  if g_shoutCastStream <> nil then begin
        g_shoutCastStream.get_ripStream(l_ripstream,l_path);
        RipStream := l_ripstream;
        Path := copy(l_path,1,length(l_path));
        RESULT := S_OK;
    end else    }
  RESULT := E_FAIL;
  FLock.UnLock;
end;

function TThreadedShoutcastStream.SetRipStream(RipStream: boolean; Path: string;
  FileName: string): HRESULT;
begin
  FLock.Lock; // protect our member objects
  FRipStream := RipStream;
  FPath := copy(Path, 1, system.length(Path));
  FFile := copy(FileName, 1, system.length(FileName));
  Result := S_OK;
  FLock.UnLock;
end;

constructor TThreadedShoutcastStream.Create(Adress, Port, Location: string;
  MetaData: boolean);
begin
  inherited Create(false);
  FLock := TBCCritSec.Create;
  FMetaData := MetaData;
  FRipStream := false;
  FPath := '';
  FExitThread := false;
  FTerminated := false;
  FAdress := Adress;
  FPort := Port;
  FLocation := Location;
end;

procedure TThreadedShoutcastStream.Execute;
var
  Application: TApplication;
  RipStream: boolean;
  ShoutCastStream: TShoutcastStream;
  Temp: string;
begin
  FTerminated := false;
  Temp := '';
  ShoutCastStream := TShoutcastStream.Create;
  ShoutCastStream.SetConnectToIp(FAdress, FPort, FLocation, FMetaData);
  Application := TApplication.Create(nil);
  // this is the mainloop of the tread
  Priority := tpLowest;
  while not FExitThread do
  begin
    FLock.Lock; // protect our member objects
    sleep(1);
    Priority := tpTimeCritical;
    Application.ProcessMessages;
    Priority := tpLowest;
    ShoutCastStream.GetRipStream(RipStream, Temp);
    if (RipStream <> FRipStream) or (Factualpath <> FPath) then
    begin
      ShoutCastStream.SetRipStream(FRipStream, FPath, FFile);
      Factualpath := FPath;
    end;
    FLock.UnLock;
  end;
  if ShoutCastStream <> nil then
  begin
    ShoutCastStream.Destroy;
  end;
  FTerminated := true;
end;

end.

