//------------------------------------------------------------------------------
//
// Desc: DirectShow sample code - application using async filter.

// Converted to Delphi by
// Andriy Nevhasymyy (a.n@email.com), Milenko Mitrovich (dcoder@dsp-worx.de)
//
// Portions created by Microsoft are
// Copyright (c) 1992 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
program MemFile;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  BaseClass,
  DirectShow9,
  DSUtil,
  ActiveX,
  UAsyncIo in '..\UAsyncIo.pas',
  UAsyncRdr in '..\UAsyncRdr.pas',
  UMemFile in 'UMemFile.pas',
  UAsyncFlt in '..\UAsyncFlt.pas';

//  Select a filter into a graph and render its output pin,
//  returning the graph

function SelectAndRender(AReader: TBCMemFileReader;
  var AFG: IFilterGraph): HResult;
var
  _Builder: IGraphBuilder;
  _Pin: IPin;
begin
  if Not Assigned(AReader) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  //  Create filter graph
  Result := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC,
    IID_IFilterGraph, AFG);
  if Failed(Result) then
    Exit;

  //  Add our filter
  Result := AFG.AddFilter(AReader, nil);
  if Failed(Result) then
    Exit;

  //  Get a GraphBuilder interface from the filter graph
  Result := AFG.QueryInterface(IID_IGraphBuilder, _Builder);
  if Failed(Result) then
    Exit;

  //  Render our output pin
  _Pin := AReader.GetPin(0);
  Result := _Builder.Render(_Pin);

  // _Builder released on function exit
end;

function PlayFileWait(var AFG: IFilterGraph): HResult;
var
  _MC: IMediaControl;
  _ME: IMediaEvent;
  _Event: OAEVENT;
  _EvCode: Integer;
begin
  if Not Assigned(AFG) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := AFG.QueryInterface(IID_IMediaControl, _MC);
  if Failed(Result) then
    Exit;

  Result := AFG.QueryInterface(IID_IMediaEvent, _ME);
  if Failed(Result) then
    Exit;

  Result := _ME.GetEventHandle(_Event);
  if Succeeded(Result) then
  begin
    Result := _MC.Run;
    if Succeeded(Result) then
      Result := _ME.WaitForCompletion(Infinite, _EvCode);
  end;
end;

var
  KBPerSec: DWord;
  mt: TAMMediaType;
  pmt: PAMMediaType;
  Ext: String;
  hFile: THandle;
  Size: ULARGE_INTEGER;
  Mem: PByte;
  BytesRead: DWord;
  hr: HResult;
  Stream: TBCMemFileStream;
  Reader: TBCMemFileReader;
  FG: IFilterGraph;

  procedure OnFileDone;
  begin
    CloseHandle(hFile);
  end;

begin
  //  Read a file into memory, play it (or part of it), then exit

  if (ParamCount < 1) or (ParamCount > 2) then
  begin
    WriteLn('Usage : memfile FileName <Kbytes per sec>');
    ExitCode := 0;
    Exit;
  end;

  KBPerSec := StrToInt64Def(ParamStr(2), Infinite);
  pmt := @mt;
  TBCMediaType(pmt).InitMediaType;
  mt.majortype := MEDIATYPE_Stream;

  //  Find the extension

  Ext := UpperCase(ExtractFileExt(ParamStr(1)));

  // Set subtype based on file extension
  if (Ext = '.MPG') then
    mt.subtype := MEDIASUBTYPE_MPEG1System
  else
    if (Ext = '.MPA') then
      mt.subtype := MEDIASUBTYPE_MPEG1Audio
    else
      if (Ext = '.MPV') then
        mt.subtype := MEDIASUBTYPE_MPEG1Video
      else
        if (Ext = '.DAT') then
          mt.subtype := MEDIASUBTYPE_MPEG1VideoCD
        else
          if (Ext = '.AVI') then
            mt.subtype := MEDIASUBTYPE_Avi
          else
            if (Ext = '.MOV') then
              mt.subtype := MEDIASUBTYPE_QTMovie
            else
              if (Ext = '.WAV') then
                mt.subtype := MEDIASUBTYPE_WAVE
              else
                begin
                  WriteLn(Format('Unknown file type: %s', [Ext]));
                  ExitCode := 1;
                  Exit;
                end;

  //  Open the file
  hFile := CreateFile(PAnsiChar(ParamStr(1)), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, 0, 0);
  if (hFile = INVALID_HANDLE_VALUE) then
  begin
    WriteLn(Format('Could not open %s', [ParamStr(0)]));
    ExitCode := 1;
    Exit;
  end;

  // Determine the file size
  Size.LowPart := GetFileSize(hFile, @Size.HighPart);

  // Allocate a buffer to hold the file's data
  try
    GetMem(Mem, Size.LowPart);

    if (Not ReadFile(hFile, Mem^, Size.LowPart, BytesRead, nil)) or
      (BytesRead <> Size.LowPart) then
    begin
      WriteLn(Format('Could not read file %s', [ParamStr(1)]));
      ExitCode := 1;
      OnFileDone;
      Exit;
    end;

    OnFileDone;
  except
    WriteLn(Format('Could not allocate %d bytes', [Size.LowPart]));
    ExitCode := 1;
    OnFileDone;
    Exit;
  end;

  hr := S_OK;

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  Stream := TBCMemFileStream.Create(Mem, Size.QuadPart, KBPerSec);
  Reader := TBCMemFileReader.Create(Stream, @mt, hr);
  if (Failed(hr) or (Reader = nil)) then
  begin
    if Assigned(Reader) then
      FreeAndNil(Reader);
    WriteLn(Format('Could not create filter'#13#10 + 'HResult: %8.8x', [hr]));
    ExitCode := 1;
    CoUninitialize;
    Exit;
  end;

  //  Make sure we don't accidentally go away!
  Reader._AddRef;

  FG := nil;
  hr := SelectAndRender(Reader, FG);

  if Failed(hr) then
    begin
      WriteLn(Format('Failed to create graph and render file.'#13#10 +
        'HResult: %8.8x'#13#10+'Desc: %s', [hr, GetErrorString(hr)]));
    end
  else
    begin
      //  Play the file
      hr := PlayFileWait(FG);
      if Failed(hr) then
        WriteLn(Format('Failed to play graph.'#13#10 + 'HResult: %8.8x', [hr]));
    end;

  // don´t use Reader.Free !!!
  // The Reader will destroy itself if the Reference Count is 0
  Reader._Release;

  if Assigned(FG) then
    FG := nil;

  CoUninitialize;
  ExitCode := 0;
end.
