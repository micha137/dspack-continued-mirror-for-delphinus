unit NetWrite;

interface
uses Windows, wmf9, SysUtils, activex, Classes;

const
  NETWRITE_ASYNC_EVENT	: PCHAR = '{6d12fe9b-d029-4d08-b2eb-92c8cab323c7}';

type

  TWMFNetWrite = class(TObject, IWMReaderCallback, IWMReaderCallbackAdvanced)
  public
    constructor Create;

    destructor  Destroy; override;
    function    Configure(dwPortNum: DWORD; const pwszFile: PWideChar; nMaxClient: cardinal): HRESULT;
    function    WritetoNet: HRESULT;
    function    Init: HRESULT;

    //Methods of IWMReaderCallback

    function OnSample(dwOutputNum: DWORD; cnsSampleTime, cnsSampleDuration: int64;
               dwFlags: DWORD; pSample: INSSBuffer; pvContext: pointer): HRESULT; stdcall;
    function OnStatus(Status: TWMTSTATUS; hr: HRESULT; dwType: TWMTATTRDATATYPE;
               pValue: PBYTE; pvContext: pointer): HRESULT; stdcall;

    //Methhods of IWMReaderCallbackAdvanced

    // Receive a sample directly from the ASF. To get this call, the user
    // must register himself to receive samples for a particular stream.
    function OnStreamSample(wStreamNum: WORD; cnsSampleTime, cnsSampleDuration: int64;
               dwFlags: DWORD; pSample: INSSBuffer; pvContext: pointer): HRESULT; stdcall;

    // In some cases, the user may want to get callbacks telling what the
    // reader thinks the current time is. This is interesting in 2 cases:
    // - If the ASF has gaps in it; say no audio for 10 seconds. This call
    //   will continue to be called, while OnSample won't be called.
    // - If the user is driving the clock, the reader needs to communicate
    //   back to the user its time, to avoid the user overrunning the reader.
    function OnTime(cnsCurrentTime: int64; pvContext: pointer): HRESULT; stdcall;

    // The user can also get callbacks when stream selection occurs.
    function OnStreamSelection(wStreamCount: Word; pStreamNumbers: PWord;
                               pSelections: PWMTSTREAMSELECTION; pvContext: Pointer): HResult; stdcall;
    // Will be called if the user got an async result from their
    // call to SetOutputProps.  The next sample you receive for
    // this output will have these properties.  The contents of the
    // media type after calling SetOutputProps and before receiving
    // an OutputPropsChanged notification are undefined.
    function OnOutputPropsChanged(dwOutputNum: DWORD; pMediaType: PWMMediaType;
               pvContext: pointer): HRESULT; stdcall;


    // If the user has registered to allocate buffers, this is where he must
    // do it.
    function AllocateForStream(wStreamNum: WORD; cbBuffer: DWORD; out ppBuffer: INSSBuffer;
               pvContext: pointer): HRESULT; stdcall;

    function AllocateForOutput(dwOutputNum, cbBuffer: DWORD; out ppBuffer: INSSBuffer;
               pvContext: pointer): HRESULT; stdcall;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    function WriteHeader (const pwszName: PWideChar): HRESULT;
    function WriteScript: HRESULT;
  private
    m_hEvent            : THANDLE;
    m_hrAsync           : HRESULT;
    m_qwTime            : Int64;
    m_pWriterAdvanced   : IWMWriterAdvanced;
    m_pReaderAdvanced   : IWMReaderAdvanced;
    m_pReader           : IWMReader;
    m_pWriter           : IWMWriter;
    m_pNetSink          : IWMWriterNetworkSink;
    m_bEOF              : bool;
    m_pReaderHeaderInfo : IWMHeaderInfo;
    m_pWriterHeaderInfo : IWMHeaderInfo;
  public
    function CloseAll: HRESULT;
  end;

implementation

  constructor TWMFNetWrite.Create;
  begin
    m_pReaderHeaderInfo := nil;
    m_pWriterHeaderInfo := nil;
    m_pWriterAdvanced   := nil;
    m_pReaderAdvanced   := nil;
    m_pReader           := nil;
    m_pWriter           := nil;
    m_pNetSink          := nil;
    m_hEvent            := 0;
    m_bEOF              := false;
    m_qwTime            := 0;
    m_hrAsync           := S_OK;
  end;

  destructor TWMFNetWrite.Destroy;
  begin
    CloseAll;
    CloseHandle(m_hEvent);
    m_pWriterAdvanced := nil;
    m_pWriter := nil;
    m_pNetSink := nil;
    m_pReaderAdvanced := nil;
    m_pReader := nil;
    inherited destroy;
  end;

  function TWMFNetWrite.Configure(dwPortNum: DWORD; const pwszFile: PWideChar; nMaxClient: cardinal): HRESULT;
  var
    pProfile : IWMProfile;
    pStream  : IWMStreamConfig;
    err, cchURL, dwStreams, i, cInputs : DWORD;
    pwszURL : PWideChar;
    wStreamNumber: WORD;
  begin
    if((dwPortNum = 0) or (pwszFile = nil)) then
    begin
      result := E_INVALIDARG;
      exit;
    end;

    if ((m_pWriterAdvanced = nil) or (m_pReaderAdvanced = nil) or (m_pNetSink = nil)) then
    begin
      result := E_UNEXPECTED;
      exit;
    end;

    // Create event for handling asynchronous calls
    result   := S_OK;
    pProfile := nil;
    pStream  := nil;

    m_hrAsync := S_OK;

    m_hEvent := CreateEvent(nil, FALSE, FALSE, NETWRITE_ASYNC_EVENT);
    if (m_hEvent = 0) then
    begin
      err := GetLastError;
      writeln(format('Could not Create Event: (hr=$%x)',[err]));
      result := err;
      exit;
    end;

    // Configure the Net Sink
    result := m_pNetSink.SetNetworkProtocol(WMT_PROTOCOL_HTTP);
    if (FAILED(result)) then
    begin
      writeln('Could not Set Network protocol');
      exit;
    end;

    result := m_pNetSink.Open(dwPortNum);
    if (FAILED(result)) then
    begin
      writeln(format('Network sink failed to open port no %d',[dwPortNum]));
      exit;
    end; 

    cchURL := 0;

    result := m_pNetSink.GetHostURL(nil, cchURL);
    if(FAILED(result)) then
    begin
      writeln('Could not get the host URL from IWMWriterNEtworkSink');
      exit;
    end;
     

    getmem(pwszURL, cchURL * sizeof(WCHAR));
    if (pwszURL = nil) then
    begin
      result := E_OUTOFMEMORY; // Insufficient Memory
      exit;
    end;

    result := m_pNetSink.GetHostURL(pwszURL, cchURL);
    if (FAILED(result)) then
    begin
      writeln('Could not get the host URL from IWMWriterNEtworkSink');
      FreeMem(pwszURL);
      exit;
    end;

    writeln('Connect to '+pwszURL);
 //   Sleep(1000);

    FreeMem(pwszURL);

    // Set the max no of clients that can connect to the port
    result := m_pNetSink.SetMaximumClients(nMaxClient);
    if (FAILED(result)) then
    begin
      writeln('Could not Set maximum clients');
      exit;
    end;

    // Add the network sink to the Writer Advanced
    result := m_pWriterAdvanced.AddSink(m_pNetSink);
    if (FAILED(result)) then
    begin
      writeln('Could not Add Sink');
      exit;
    end;   

    // Open the requested file
    result := m_pReader.Open(pwszFile, self, nil);
    if (FAILED(result)) then
    begin
      writeln('Could not open file');
      exit;
    end;

    // Wait for the open to finish
    WaitForSingleObject(m_hEvent, INFINITE);
    if (FAILED(m_hrAsync)) then
    begin
      writeln(format('Open failed (hr=$%x)',[m_hrAsync]));
      result := m_hrAsync;
      exit;
    end;

    // Turn on manual stream selection, so we get all streams.
    result := m_pReaderAdvanced.SetManualStreamSelection(TRUE);
    if (FAILED(result)) then
    begin
      writeln('Failed to set manual stream selection');
      exit;
    end; // 

    // Get the profile interface, loop thru all the
    // streams and request the reader to deliver compressed samples
    result := m_pReader.QueryInterface(IID_IWMProfile, pProfile);
    if (FAILED(result)) then
    begin
      writeln('Could not Query for IWMProfile');
      exit;
    end;   


    dwStreams := 0;
    result := pProfile.GetStreamCount(dwStreams);
    if (FAILED(result)) then
    begin
      writeln(format('GetStreamCount on IWMProfile failed (hr=$%x)', [result]));
      exit;
    end; 

    for i := 0 to dwStreams - 1 do
    begin
      result := pProfile.GetStream(i, pStream);
      if (FAILED(result)) then
      begin
        writeln(format('Could not get Stream %d of %d from IWMProfile (hr=0x%08x)',[i,dwStreams,result]));
        break;
      end;
      wStreamNumber := 0;
      //Get the stream number of the current stream
      result := pStream.GetStreamNumber(wStreamNumber);
      if (FAILED(result)) then
      begin
        writeln(format('Could not get stream number from IWMStreamConfig %d of %d (hr=$%x)',
			[i, dwStreams, result]));
        break;
      end;

      pStream := nil;

      //Set the stream to be recieved in compressed mode
      result := m_pReaderAdvanced.SetReceiveStreamSamples(wStreamNumber, TRUE);
      if (FAILED(result)) then
      begin
        writeln(format('Could not SetReceivedStreamSamples for stream number %d (hr=$%x)',
                       [wStreamNumber, result]));
        break;
      end;
    end;
    pStream := nil;
    if (FAILED(result)) then exit;

    // Turn on the user clock
    result := m_pReaderAdvanced.SetUserProvidedClock(TRUE);
    if (FAILED(result)) then
    begin
      writeln(format('SetUserProvidedClock failed (hr=$%x)', [result]));
      exit;
    end; 

    // Now set the writers properties
    result := m_pWriter.SetProfile(pProfile);
    if(FAILED(result)) then
    begin
      writeln(format('Could not set profile on IWMWriter (hr=$%x)',[result]));
      exit;
    end;

    pProfile := nil;

    cInputs := 0;

    result := m_pWriter.GetInputCount(cInputs);
    if(FAILED(result)) then
    begin
      writeln(format('Could not get input count from IWMWriter (hr=$%x)',[result]));
      exit;
    end;

    for i := 0 to cInputs -1 do
      // Set the input props to NULL to indicate that we don't need a codec
      // because we are writing compressed samples to the port
       m_pWriter.SetInputProps(i, nil);

    // Write all the header attributes, which can be set, from the
    // input file to the output port.
    result := WriteHeader(g_wszWMTitle);
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMAuthor) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMDescription) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMRating) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMCopyright) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMAlbumTitle) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMTrack) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMPromotionURL) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMAlbumCoverURL) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMGenre) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMYear) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMGenreID) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMMCDI) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMBannerImageType ) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMBannerImageData ) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMBannerImageURL ) ;
    if(FAILED(result)) then exit;

    result := WriteHeader( g_wszWMCopyrightURL ) ;
    if(FAILED(result)) then exit;

    //Header has been written. Lets write the script
    result := WriteScript;
  end;

  function TWMFNetWrite.WritetoNet: HRESULT;
  begin
    if ((m_hEvent          = 0)   or
        (m_pWriterAdvanced = nil) or
        (m_pReaderAdvanced = nil) or
        (m_pNetSink        = nil)) then
    begin
      result := E_UNEXPECTED;
      exit;
    end;
    // Start Writing
    result := m_pWriter.BeginWriting;
    if (FAILED(result)) then
    begin
      writeln(format('BeginWriting on IWMWriter failed (hr=$%x)',[result]));
      exit;
    end;

    result := m_pReader.Start(0, 0, 1.0, nil);
    if (FAILED(result)) then
    begin
      writeln(format('Could not start IWMReader (hr=$%x)',[result]));
      exit;
    end;

   // not usefull with Windowed app
  { // Wait for it to finish
    WaitForSingleObject(m_hEvent, INFINITE);
    if (FAILED(m_hrAsync)) then
    begin
      result := m_hrAsync;
      exit; // Net writing failed  ????? not logic for hresult
    end;}
  end;

  function TWMFNetWrite.CloseAll: HRESULT;
  begin

    // Stop stuff
    if assigned(m_pReader) then
    begin
      result := m_pReader.Stop;
      if (FAILED(result)) then
      begin
        writeln(format('Could not Stop IWMReader (hr=$%x)',[result]));
        exit;
      end; // Could not Stop IWMReader
    end;

    if assigned(m_pWriter) then
    begin
      result := m_pWriter.Flush;
      if (FAILED(result)) then
      begin
        writeln(format('Could not Flush on IWMWriter (hr=$%x)',[result]));
        exit;
      end;
      result := m_pWriter.EndWriting;
      if (FAILED(result)) then
      begin
        writeln(format('Could not EndWriting on IWMWriter (hr=$%x)',[result]));
        exit;
      end;
    end;

    if assigned(m_pReader) then
    begin
      result := m_pReader.Close;
      if (FAILED(result)) then
      begin
        writeln(format('Could not close the file (hr=$%x)',[result]));
        exit;
      end; 
    end;

    if assigned(m_pWriterAdvanced) then
    begin
      result := m_pWriterAdvanced.RemoveSink(m_pNetSink);
      if (FAILED(result)) then
      begin
        writeln(format('Could not remove the Network Sink (hr=$%x)',[result]));
        exit;
      end;
    end;

    if assigned(m_pNetSink) then
    begin
      result := m_pNetSink.Close;
      if (FAILED(result)) then
      begin
        writeln(format('Could not close on IWMWriterNetworkSink (hr=$%x)',[result]));
        exit;
      end;
    end;

    result := s_ok;
    //Wait for sometime till all the data gets read from the port
    //Sleep(20000);
  end;

  function TWMFNetWrite.Init: HRESULT;
  begin
    // Create the reader, writer and network sink.
    result := WMCreateReader( nil, 0, m_pReader);
    if (FAILED(result)) then
    begin
      writeln(format('Could not create reader (hr=$%x)',[result]));
      exit;
    end; 

    result := m_pReader.QueryInterface(IID_IWMReaderAdvanced, m_pReaderAdvanced);
    if (FAILED(result)) then
    begin
      writeln(format('Could not QI for IWMReaderAdvanced (hr=$%x)',[result]));
      exit;
    end;

    result := WMCreateWriter(nil, m_pWriter);
    if (FAILED(result)) then
    begin
      writeln(format('Could not create Writer (hr=$%x)',[result]));
      exit;
    end;

    result := m_pWriter.QueryInterface(IID_IWMWriterAdvanced, m_pWriterAdvanced);
    if (FAILED(result)) then
    begin
      writeln(format('Could not QI for IWMWriterAdvanced (hr=$%x)',[result]));
      exit;
    end;  

    result := WMCreateWriterNetworkSink(m_pNetSink);
    if (FAILED(result)) then
    begin
      writeln(format('Could not create Writer Network Sink (hr=$%x)',[result]));
      exit;
    end; 

    result := m_pReader.QueryInterface(IID_IWMHeaderInfo, m_pReaderHeaderInfo);
    if (FAILED(result)) then
    begin
      writeln(format('Could not QI for IWMHeaderInfo (hr=$%x)',[result]));
      exit;
    end;

    result := m_pWriter.QueryInterface(IID_IWMHeaderInfo, m_pWriterHeaderInfo);
    if (FAILED(result)) then
    begin
      writeln(format('Could not QI for IWMHeaderInfo (hr=$%x)',[result]));
      exit;
    end; 
  end;

  function TWMFNetWrite.OnSample(dwOutputNum: DWORD; cnsSampleTime, cnsSampleDuration: int64;
             dwFlags: DWORD; pSample: INSSBuffer; pvContext: pointer): HRESULT;
  begin
    if (m_hEvent <> 0) then
    begin
      //The samples are expected in OnStreamSample
      writeln('Error: Received a decompressed sample from the reader');
      m_hrAsync := E_UNEXPECTED;
      SetEvent(m_hEvent);
    end;
    result := S_OK;
  end;

  function TWMFNetWrite.OnStatus(Status: TWMTSTATUS; hr: HRESULT; dwType: TWMTATTRDATATYPE;
             pValue: PBYTE; pvContext: pointer): HRESULT;
  begin
    case Status of
      WMT_OPENED:
        begin
          m_hrAsync := hr;
          SetEvent(m_hEvent);
        end;
      WMT_END_OF_FILE:
        begin
          m_bEOF := true;
          writeln('EndOfStream detected in reader');
          m_hrAsync := hr;
          SetEvent(m_hEvent);
        end;
      WMT_STARTED:
        begin
          //Ask for the specific duration of the stream to be delivered
          m_qwTime := 0;
          m_qwTime := m_qwTime + (1000 * 10000);
          hr := m_pReaderAdvanced.DeliverTime(m_qwTime);
          assert(SUCCEEDED(hr));
        end;
      end;
      result :=  S_OK;
  end;

  function TWMFNetWrite.OnStreamSample(wStreamNum: WORD; cnsSampleTime, cnsSampleDuration: int64;
               dwFlags: DWORD; pSample: INSSBuffer; pvContext: pointer): HRESULT;
  begin
     writeln(format('StreamSample: num=%d, time=%d, duration=%d, flags=%d',
                     [wStreamNum, cnsSampleTime, cnsSampleDuration, dwFlags]));
    //We've got a sample. Lets write it
    m_pWriterAdvanced.WriteStreamSample( wStreamNum, cnsSampleTime, 0, cnsSampleDuration, dwFlags, pSample);
    result := S_OK;
  end;

  function TWMFNetWrite.OnTime(cnsCurrentTime: int64; pvContext: pointer): HRESULT;
  begin
    //Keep asking for the specific duration of the stream till EOF
    if( not m_bEOF) then
    begin
      m_qwTime := m_qwTime + 10000000;
      m_pReaderAdvanced.DeliverTime(m_qwTime);
    end;
    result := S_OK;
  end;

  function TWMFNetWrite.OnStreamSelection( wStreamCount: Word; pStreamNumbers: PWORD;
               pSelections: PWMTSTREAMSELECTION; pvContext: pointer): HRESULT;
  begin
    result := S_OK;
  end;

  function TWMFNetWrite.OnOutputPropsChanged(dwOutputNum: DWORD; pMediaType: PWMMEDIATYPE;
               pvContext: pointer): HRESULT;
  begin
    result := S_OK;
  end;

  function TWMFNetWrite.AllocateForStream(wStreamNum: WORD; cbBuffer: DWORD; out ppBuffer: INSSBuffer;
             pvContext: pointer): HRESULT;
  begin
    result := E_NOTIMPL;
  end;

  function TWMFNetWrite.AllocateForOutput(dwOutputNum, cbBuffer: DWORD; out ppBuffer: INSSBuffer;
             pvContext: pointer): HRESULT;
  begin
    result := E_NOTIMPL;
  end;

  function TWMFNetWrite.WriteHeader (const pwszName: PWideChar): HRESULT;
  var
    nstreamNum : WORD;
    cbLength   : WORD;
    _type      : TWMTAttrDataType;
    hr         : HRESULT;
    pValue     : PBYTE;
  begin
    nstreamNum := 0;
    cbLength   := 0;
    result     := S_OK;
    pValue     := nil;

    // Get the no of bytes to be allocated for pValue
    result := m_pReaderHeaderInfo.GetAttributeByName(nstreamNum, pwszName, _type, nil, cbLength);
    if (FAILED(result) and (result <> longint(ASF_E_NOTFOUND))) then
    begin
      writeln(format('GetAttributeByName failed for Attribute name %s (hr=$%x)',[pwszName, result]));
      exit;
    end;

    if ((cbLength = 0) or (result = longint(ASF_E_NOTFOUND))) then
    begin
      result := S_OK;
      exit;
    end;

    getmem(pValue, cbLength);
    if (pValue = nil) then
    begin
      writeln(format('Unable to allocate memory for the Attribute name %s', [pwszName]));
      result := E_OUTOFMEMORY; 
      exit;
    end;

    //Dummy do-while loop
    repeat
      // Get the value
      hr := m_pReaderHeaderInfo.GetAttributeByName(nstreamNum, pwszName, _type, pValue, cbLength);
      if (FAILED(hr)) then
      begin
        writeln(format('GetAttributeByName failed for Attribute name %s (hr=$%x)', [pwszName, hr]));
        break;
      end; 

      // Set the attribute
      hr := m_pWriterHeaderInfo.SetAttribute(nstreamNum, pwszName, _type, pValue, cbLength);
      if (FAILED(hr)) then
      begin
        writeln(format('SetAttribute failed for Attribute name %s (hr=$%x)',[pwszName, hr]));
        break;
      end; 
    until (FALSE);

    freemem(pValue);
    pValue := nil;
    result := hr;
  end;

  function TWMFNetWrite.WriteScript: HRESULT;
  var
    hr            : HRESULT;
    pwszCommand   : PWideChar;
    pwszType      : PWideChar;
    cnsScriptTime : int64;
    cScript       : WORD;
    cchTypeLen    : WORD;
    cchCommandLen : WORD;
    i             : integer;
  begin
    hr            := S_OK;
    pwszCommand   := nil;
    pwszType      := nil;
    cnsScriptTime := 0;
    cScript       := 0;
    cchTypeLen    := 0;
    cchCommandLen := 0;


    result := m_pReaderHeaderInfo.GetScriptCount(cScript);
    if (FAILED(result)) then
    begin
      writeln(format('GetScriptCount failed (hr=$%x)',[result]));
      exit;
    end;

    for i := 0 to cScript - 1 do
    begin
      // Get the memory reqd for this script
      hr := m_pReaderHeaderInfo.GetScript(i, nil, cchTypeLen, nil, cchCommandLen, cnsScriptTime);
      if (FAILED(hr)) then
      begin
        writeln(format('GetScript failed for Script no %d (hr=$%x)',[i, hr]));
        break;
      end;

      getmem(pwszType, cchTypeLen * sizeof(WORD));
      getmem(pwszCommand, cchCommandLen * sizeof(WORD));

      if ((pwszType = nil) or (pwszCommand = nil)) then
      begin
        hr := E_OUTOFMEMORY;
        break;
      end;

      // Now, get the script
      hr := m_pReaderHeaderInfo.GetScript(i, pwszType, cchTypeLen, pwszCommand, cchCommandLen, cnsScriptTime);
      if (FAILED(hr)) then
      begin
        writeln(format('GetScript failed for Script no %d (hr=$%x)', [i, hr]));
        break;
      end;  // GetScript failed for Script no %d

      // Add the script to the writer
      hr := m_pWriterHeaderInfo.AddScript(pwszType, pwszCommand, cnsScriptTime);
      if (FAILED(hr)) then
      begin
        Writeln(format('AddScript failed for Script no %d (hr=$%x)', [i, hr]));
        break;
      end;

      if pwszType <> nil then freemem(pwszType);
      if pwszCommand <> nil then freemem(pwszCommand);
      pwszType    := nil;
      pwszCommand := nil;
      cchTypeLen    := 0 ;
      cchCommandLen := 0 ;
    end;

    if pwszType <> nil then freemem(pwszType);
    if pwszCommand <> nil then freemem(pwszCommand);
    pwszType    := nil;
    pwszCommand := nil;
    result := hr;
  end;

  function TWMFNetWrite.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  begin
    if IsEqualGUID(IID, IID_IWMReaderCallback) then
       IWMReaderCallback(Obj) := self
    else
      if IsEQualGUID(IID, IID_IWMReaderCallbackAdvanced) then
        IWMReaderCallbackAdvanced(obj) := self
      else
      begin
        result := E_NOINTERFACE;
        exit;
      end;
    result := S_OK;
  end;

  function TWMFNetWrite._AddRef: Integer; stdcall;
  begin
    result := 1;
  end;

  function TWMFNetWrite._Release: Integer; stdcall;
  begin
    result := 1;
  end;

end.
