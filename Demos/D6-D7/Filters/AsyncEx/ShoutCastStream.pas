unit ShoutCastStream;

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

uses
  Windows, Controls, Sock, Forms, SysUtils, BaseClass, Dialogs,
  WinSock, ICYParser;

{ how to use tip:
  we are running a async. winsock
  The Winsock sends its event handling trough the Windows Message Queue
  You should create this class in a Thread and/or use
  TApplication.processmessages in external buffering loops }

type
  TShoutcastStream = class
  public
    FApplication: TApplication;
    constructor Create;
    destructor Destroy; override;
    procedure SetConnectToIp(Adress: string; Port: string;
      Location: string; Meta: boolean);
    function SetRipStream(RipStream: boolean; Path: string;
      FileO: string): HRESULT;
    function GetRipStream(out RipStream: boolean; out Path: string): HRESULT;
  private
    FLock: TBCCritSec;
    FSock: TSock; // Winsock class
    { sock message receiver
      ( we are running a async winsock.
      " requires a TForm listener "  ) }
    FReceiveForm: TForm;
    FLocation: string; // host Location (Path only)
    FHeaderFound: boolean; // header flag
    FICYHeader: string; // the header itself
    // ripper feature Objects
    FPath: string; // used filePath
    FFile: string; // Location and filename
    FFileNoMetaData: string; // file to record in NoMetaData Mode
    FFileObject: TextFile; // FileObject
    FRipStream: boolean; // ripper state flag
    FFileCreated: boolean; // file state flag
    // Metadata count
    FMetaInterval: integer;
    FMetaCount: integer;
    FMetaStartFound: boolean;
    FTempSave: string;
    FOutOfSync: boolean;
    FMetadataEnabled: boolean;
    // connect message receiver
    procedure OnSockConnect(Sender: TObject);
    // read message receiver
    procedure OnSockRead(Sender: TObject; Count: Integer);
    procedure OnSockInfo(Sender: TObject; SocketInfo: TSocketInfo; Msg: string);
    // metadata format: "StreamTitle='content;StreamURL='content';"
    function getStreamTitle(Metadata: string): string;
    function getStreamURl(Metadata: string): string;
    // ripper
    procedure createNewFileIfNeeded(Metadata: string);
    procedure createFileNoMeataInt(FileO: string);
  protected
  end;

implementation

uses config;

function TShoutcastStream.GetRipStream(out RipStream: boolean;
  out Path: string): HRESULT;
begin
  FLock.Lock;
  RipStream := FRipStream;
  Path := copy(FPath, 1, length(FPath));
  RESULT := S_OK;
  FLock.UnLock;
end;

function TShoutcastStream.SetRipStream(RipStream: boolean; Path: string;
  FileO: string): HRESULT;
begin
  FLock.Lock;
  FRipStream := RipStream;
  FPath := copy(Path, 1, length(Path));
  if not RipStream then
            FFile := '';
  FFileNoMetaData := copy(FileO, 1, length(FileO));
  RESULT := S_OK;
  FLock.UnLock;
end;

function TShoutcastStream.GetStreamTitle(Metadata: string): string;
var
  Pos1: integer;
  Temp: string;
begin
  Pos1 := Pos('''', Metadata);
  Temp := copy(Metadata, Pos1 + 1, length(Metadata) - Pos1 - 1);
  Pos1 := Pos('''', Temp);
  Result := copy(Temp, 1, Pos1 - 1);
end;

function TShoutcastStream.GetStreamURl(Metadata: string): string;
var
  Pos1: integer;
  Temp: string;
begin
  // search for the first offset
  Pos1 := Pos(';', Metadata);
  Temp := copy(Metadata, Pos1 + 1, length(Metadata) - Pos1 - 1);
  Result := getStreamTitle(Temp);
end;

procedure TShoutcastStream.createNewFileIfNeeded(metadata: string);
var
  Title: string;
  Pos1: integer;
begin
  Title := getStreamTitle(Metadata);
  if (Title <> FFile) then
    GFFileName := Title + '.mp3';
  if FRipStream then
  begin
    if (Title <> FFile) then
    begin
      FFile := Title;
      if FPath <> '' then
        SetCurrentDir(FPath);
      if FFileCreated then
        CloseFile(FFileObject);
      // check if the file name is supported ( \/:*?"<>| )
      Pos1 := Pos('\', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('/', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos(':', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('*', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('?', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('"', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('<', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('>', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      Pos1 := Pos('|', Title);
      if Pos1 <> 0 then
        Title := copy(Title, 1, Pos1 - 1);
      // rewrite existing files to reduce overhead :/
      if FPath <> '' then
        SetCurrentDir(FPath);
      if FileExists(Title + '.mp3') then
        DeleteFile(Title + '.mp3');
      try
        if FPath <> '' then
          SetCurrentDir(FPath);
        AssignFile(FFileObject, Title + '.mp3');
        ReWrite(FFileObject);
        FFileCreated := true;
      except
        ShowMessage('A bug has been found in ASyncEx Filter' +
          'please post the folowing line to: coder@dsplayer.de  : ' +
          Title + '.mp3');
      end;
    end;
  end
  else
  begin
    if FFileCreated then
      CloseFile(FFileObject);
    FFileCreated := false;
  end;
end;

procedure TShoutcastStream.createFileNoMeataInt(FileO: string);
begin
  if FRipStream then
  begin
    if not FFileCreated then
    begin
      if FileExists(FileO) then
        // we rewrite existing files to reduce overhead :/
        DeleteFile(FileO);
      try
        if FPath <> '' then
          SetCurrentDir(FPath);
        if FFileCreated then
          CloseFile(FFileObject);
        AssignFile(FFileObject, FileO);
        ReWrite(FFileObject);
        FFileCreated := true;
      except
        ShowMessage('A bug has been found in DSPlayer ASync.Source' +
          'please post the folowing line to: coder@dsplayer.de  : ' +
          FileO);
      end;
    end;
  end
  else
  begin
    if FFileCreated then
      CloseFile(FFileObject);
    FFileCreated := false;
  end;
end;

procedure TShoutcastStream.OnSockRead(Sender: TObject; Count: Integer);
var
  Temp: string;
  Temp2: string;
  MyPos: integer;
  Subi: integer;
  Pos1, Pos2: integer;
  MetaString: string;
  LengthO: byte;
  CharO: char;
  TempSave: string;
  MetaTitle: string;
  MetaUrl: string;
  ErrMsg: string;
begin
  { -> This section includes the streamparser,fileripper and buffer abilties <-
               todo: - code cleaning
                     - better helper functions or helper classes
  }
  try
    FLock.Lock;
    Temp := FSock.Receive; // get the received data from winsock buffer
    // get the end of url header count
    MyPos := 0;
    if not FHeaderFound then
    begin
      FTempSave := FTempSave + Temp;
      Temp := FTempSave;
      MyPos := Pos(#13#10#13#10, Temp);
    end;
    if MyPos <> 0 then
    begin
      // cut the header and save it into FICYHeader
      Temp2 := Temp;
      Temp := Copy(Temp, MyPos + 4, StrLen(@MyPos) - 4); // get mp3 data
      Temp2 := Copy(Temp2, 0, MyPos + 2); // get the URL header
      FICYHeader := Temp2; // save the URL header
      // get the Metadata count:
      FMetaInterval := GetServerICYInt(Temp2);
      // header callback
      if GFFilterCallBack <> nil then
      begin
        if not GetICYSuccessfullyConnected(FICYHeader, ErrMsg) then
        begin
          GFFilterCallBack.AsyncExICYNotice(ICYError, PChar(ErrMsg));
        end;
        // try to get icy informations
        GFFilterCallBack.AsyncExICYNotice(PChar(ICYName),
          PChar(GetServerICYName(FICYHeader)));
        GFFilterCallBack.AsyncExICYNotice(PChar(ICYGenre),
          PChar(GetServerICYGenre(FICYHeader)));
        GFFilterCallBack.AsyncExICYNotice(PChar(ICYURL),
          PChar(GetServerICYURL(FICYHeader)));
        GFFilterCallBack.AsyncExICYNotice(PChar(ICYBitrate),
          PChar(GetServerICYBitRate(FICYHeader)));
      end
      else
      begin
        if not GetICYSuccessfullyConnected(FICYHeader, ErrMsg) then
          showmessage('Can not receive the Stream.'#13#10#13#10 +
            'Reason:'#13#10 + ErrMsg);
      end;
      // push the mp3 data to queue
      if GFStringQueue <> nil then
        GFStringQueue.Push(Temp);
      if (not FMetadataEnabled) and (FMetaInterval = 0) then
      begin
        createFileNoMeataInt(FFileNoMetaData);
        if FPath <> '' then
          SetCurrentDir(FPath);
        if FFileCreated and FRipStream then
          Write(FFileObject, Temp);
      end;
      if FMetaInterval <> 0 then
      begin
        FMetaCount := length(Temp);
      end;
      // set header found state flag
      FHeaderFound := true;
      FMetaStartFound := false;
      FTempSave := '';
      FLock.UnLock;
      exit;
    end;

    // if found and cutted the URLheader start to add mp3 data to the queue
    if FHeaderFound then
    begin
      if FTempSave <> '' then
      begin // completion of metadatablock is done here
        TempSave := copy(FTempSave, 1, length(FTempSave));
        Temp := copy(TempSave, 1, length(TempSave)) + copy(Temp, 1,
          length(Temp));
        FTempSave := '';
      end;
      Pos1 := Pos('StreamTitle', Temp);
      if Pos1 <> 0 then
      begin
        CharO := (copy(Temp, Pos1 - 1, 1))[1];
        LengthO := ((byte(CharO)) * 16);
        if length(Temp) < Pos1 + LengthO - 1 then
        begin
          // found a incomlete metadata block
          FTempSave := FTempSave + copy(Temp, 1, length(Temp));
          FLock.UnLock;
          exit;
        end;
      end;
      if FMetaInterval <> 0 then
      begin
        FMetaCount := FMetaCount + length(Temp);
        { some servers send the first Metatag at a unspezified point! ,
          so try to get the first sended Meta Info, and count the received
          mp3 data                                                          }
        Pos1 := Pos('StreamTitle', Temp);
        if Pos1 <> 0 then
        begin
          Pos2 := length(Temp) - Pos1;
          CharO := (copy(Temp, Pos1 - 1, 1))[1];
          LengthO := ((byte(CharO)) * 16);
          MetaString := copy(Temp, Pos1, LengthO - 1);
          if MetaString <> '' then
          begin
            // MetaData Callback
            if GFFilterCallBack <> nil then
            begin
              // parse stream Title & streamUrl
              MetaTitle := getStreamTitle(MetaString);
              MetaUrl := getStreamURl(MetaString);
              // Stream MetaData Callback (parsed)
              if length(MetaTitle) = 0 then
                MetaTitle := 'N/A';
              if length(MetaUrl) = 0 then
                MetaUrl := 'N/A';
              GFFilterCallBack.AsyncExMetaData(PChar(MetaTitle),
                PChar(MetaUrl));
            end;
            FOutOfSync := false;
            createNewFileIfNeeded(MetaString);
          end;
          // set the remaining data
          Temp2 := copy(Temp, 0, Pos1 - 2);
          Temp := Temp2 + copy(Temp, Pos1 - 1 + LengthO + 1, Pos2 - LengthO +
            1);
          // calculate the remaining mp3 data
          FMetaCount := Pos2 - LengthO + 1;
          if (GFStringQueue <> nil) and (not FOutOfSync) then
            GFStringQueue.Push(Temp); // push the received mp3 data to the queue
          if FPath <> '' then
            SetCurrentDir(FPath);
          if (FRipStream and FFileCreated and not FOutOfSync) then
            Write(FFileObject, Temp);
          FLock.UnLock;
          exit;
        end;
        if FMetaCount > FMetaInterval then
        begin
          // calculate the start and end of the meta data in current block
          Subi := FMetaCount - FMetaInterval;
          Pos1 := length(Temp) - Subi + 1;
          // get the length of the MetaData
          CharO := (copy(Temp, Pos1, 1))[1];
          LengthO := ((byte(CharO)) * 16);
          if length(Temp) < Pos1 + LengthO - 1 then
          begin
            // found a incomlete metadata block
            FTempSave := FTempSave + copy(Temp, 1, length(Temp));
            FLock.UnLock;
            exit;
          end;
          if LengthO <> 0 then
            if Pos('Stream', MetaString) = 0 then
            begin
              // Server is out of Sync.!
              if GFFilterCallBack <> nil then
                // >ToDO: error callback
                GFFilterCallBack.AsyncExMetaData('Server is out of sync, trying to resync', 'Server is out of sync, trying to resync');
              FOutOfSync := true;
              FLock.UnLock;
              exit;
            end;
          MetaString := copy(Temp, Pos1, LengthO);
          if MetaString <> '' then
          begin // a metastring has been found
            if GFFilterCallBack <> nil then
            begin
              // parse stream Title & streamUrl
              MetaTitle := getStreamTitle(MetaString);
              MetaUrl := getStreamURl(MetaString);
              // Stream MetaData Callback (parsed)
              if length(MetaTitle) = 0 then
                MetaTitle := 'N/A';
              if length(MetaUrl) = 0 then
                MetaUrl := 'N/A';
              GFFilterCallBack.AsyncExMetaData(PChar(MetaTitle), PChar(MetaUrl));
            end;
            if not (FOutOfSync) then
              createNewFileIfNeeded(MetaString);
          end;
          // set the remaining data
          Temp2 := copy(Temp, 0, Pos1 - 1);
          Temp := Temp2 + copy(Temp, Pos1 + LengthO + 1, Subi - LengthO - 1);
          // calculate the remaining mp3 data
          FMetaCount := Subi - LengthO - 1
        end;
      end;
      if (GFStringQueue <> nil) and (not FOutOfSync) then
        GFStringQueue.Push(Temp); // pop the received mp3 data to the queue
      // file ripper feature
      if (not FMetadataEnabled) and (FMetaInterval = 0) then
        createFileNoMeataInt(FFileNoMetaData);
      if FPath <> '' then
        SetCurrentDir(FPath);
      if (FRipStream) and (FFileCreated) and (not FOutOfSync) then
        Write(FFileObject, Temp);
    end;
    FLock.UnLock;
  except
    FLock.UnLock;
    // no exception handling at present :(
    // during prebuffering and during minimizing the app an exception is thrown:
    // -> nil pointer acces
  end;
end;

procedure TShoutcastStream.OnSockConnect(Sender: TObject);
begin
  FLock.Lock;
  if FMetadataEnabled then
    // send the official connect string (metadata)
    FSock.Send('GET ' + FLocation + ' HTTP/1.0'#13#10
      + 'User-Agent: DSPlayer'#13#10
      + 'Host: '#13#10
      + 'icy-MetaData:1'#13#10#13#10)
  else
    // send the official connect string (no metadata)
    FSock.Send('GET ' + FLocation + ' HTTP/1.0'#13#10
      + 'User-Agent: DSPlayer'#13#10
      + 'Host: '#13#10#13#10#13#10);
  FLock.UnLock;
end;

procedure TShoutcastStream.SetConnectToIP(Adress: string; Port: string;
  Location: string; Meta: boolean);
begin
  FLock.Lock;
  FMetadataEnabled := Meta;
  FSock.HostName := Adress;
  FSock.PortName := Port;
  FLocation := Location;
  FSock.Connected := true;
  FLock.UnLock;
end;

procedure TShoutcastStream.OnSockInfo(Sender: TObject; SocketInfo: TSocketInfo;
  Msg: string);
begin
  FLock.Lock;
  FApplication.ProcessMessages;
  if SocketInfo = siError then
  begin
    GFExit := true;
    // Error Handling
    //..
    // Somtimes when we connect to a still well connected Adress
    // the sock api is a little slow and needs some time to free the
    // used address. -> error WSAEADDRINUSE
    if Assigned(GFFilterCallBack) then
      GFFilterCallBack.AsyncExSockError(PChar(Msg))
    else
      ShowMessage(Msg);
  end;
  FLock.UnLock;
end;

constructor TShoutcastStream.Create;
begin
  FMetadataEnabled := true;
  FMetaCount := 0;
  FMetaInterval := 0;
  FFile := '';
  FRipStream := false;
  FFileCreated := false;
  FLock := TBCCritSec.Create;
  FReceiveForm := TForm.Create(nil);
  FReceiveForm.Hide;
  FSock := TSock.Create(FReceiveForm);
  FSock.OnConnect := OnSockConnect;
  FSock.OnInfo := OnSockInfo;
  FSock.OnRead := OnSockRead;
  FHeaderFound := false;
  FOutOfSync := false;
  FTempSave := '';
  FFileNoMetaData := '';
  FApplication := TApplication.Create(nil);
  FICYHeader := 'No Header aviailble at present';
end;

destructor TShoutcastStream.Destroy;
var
  Application: TApplication;
begin
  FLock.Lock;
  Application := TApplication.Create(nil);
  FApplication.Destroy;
  if FFileCreated then
    CloseFile(FFileObject);
 // FSock.Connected := true;
 // FSock.Connected := false;
 // FSock.Close;
//  FSock.Destroy; //  buggy, if detroy is called Sock Adress might be still in use
  FSock := nil;
  Application.Destroy;
  FReceiveForm.Free;
  FLock.UnLock;
  FLock.Free;
  inherited Destroy;    
end;

end.

