unit StreamOutPin;

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
@lastmod(JAn 01, 2005)
}

interface

uses AsyncReader, BaseClass, DirectShow9, ActiveX, SysUtils;

type
  TStreamOutPin = class(TBCBasePin, IAsyncReader)
  private
    FFilter: TBCBaseFilter;
    FIO: TAsyncIO;
    FURLMode: boolean;
    FQueriedForAsyncReader: boolean;
  protected
    (*** IAsyncReader methods used as a Property ***)
    property IO: TAsyncIO read FIO implements IAsyncReader;
  public
    constructor Create(ObjectName: string; Filter: TBCBaseFilter;
      Lock: TBCCritSec; out hr: HRESULT; Name: UnicodeString; AStream: IStream;
      FwdOnly: boolean = false; const StreamSize: Int64 = 0;
      Loadstream: boolean = false; URLMode: boolean = false);
    // calling the destructor causes crashes, may a bug in BaseClasses
    // or a iusse with release
    destructor Destroy; override;
    // the graph object for full control during buffering URL stream
    procedure setActiveGraph(var f_FilterGraph: IFilterGraph);
    // TBCBasePin Methods
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function CheckConnect(Pin: IPin): HRESULT; override;
    function CompleteConnect(ReceivePin: IPin): HRESULT; override;
    function GetMediaType(Position: Integer;
      out MediaType: PAMMediaType): HRESULT; override;
    function BeginFlush: HRESULT; override; stdcall;
    function EndFlush: HRESULT; override; stdcall;
    function NonDelegatingQueryInterface(const IID: TGUID;
      out Obj): HRESULT; override; stdcall;
    function BreakConnect: HRESULT; override;
    // URL
    procedure DoConnect(Adress: string; Port: string; Location: string;
      MetaData: boolean);
  end;

implementation

uses config;

procedure TStreamOutPin.setActiveGraph(var f_FilterGraph: IFilterGraph);
begin
  FIO.setActiveGraph(f_FilterGraph);
end;

procedure TStreamOutPin.DoConnect(Adress: string; Port: string;
  Location: string; MetaData: boolean);
begin
  FIO.Connect(Adress, Port, Location, MetaData);
  FURLMode := true;
end;

function TStreamOutPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  if FURLMode then
  begin
    if GFStringQueue = nil then
    begin
      Result := S_FALSE;
      exit;
    end;
  end;
  if IsEqualGUID(mt.majortype, MEDIATYPE_Stream) then
  begin
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

constructor TStreamOutPin.Create(ObjectName: string; Filter: TBCBaseFilter;
  Lock: TBCCritSec; out hr: HRESULT; Name: UnicodeString; AStream: IStream;
  FwdOnly: boolean = false; const StreamSize: Int64 = 0;
  Loadstream: boolean = false; URLMode: boolean = false);
begin
  FFilter := Filter;
  GFConnected := false;
  FURLMode := false;
  inherited Create(ObjectName, Filter, Lock, hr, Name, PINDIR_OUTPUT);
  if Loadstream then
  begin
    if URLMode then
      FIO := TAsyncIO.Create(AStream, FwdOnly, StreamSize, true)
    else
      FIO := TAsyncIO.Create(AStream, FwdOnly, StreamSize);
    FIO.AddRef;
  end;
end;

destructor TStreamOutPin.Destroy;
begin
  inherited Destroy;
  FIO.Release;
end;

function TStreamOutPin.BeginFlush: HRESULT;
begin
  Result := FIO.BeginFlush;
end;

function TStreamOutPin.EndFlush: HRESULT;
begin
  Result := FIO.EndFlush;
end;

function TStreamOutPin.GetMediaType(Position: Integer;
  out MediaType: PAMMediaType): HRESULT;
begin
  MediaType.majortype := MEDIATYPE_Stream;
  Result := S_OK;
  GFMayjorType := GUIDToString(MEDIATYPE_Stream);
  if (Position >= 0) and (Position <= High(ProposedTypes)) then
    MediaType.subtype := ProposedTypes[Position]^
  else
    Result := VFW_S_NO_MORE_ITEMS;

end;

function TStreamOutPin.CheckConnect(Pin: IPin): HRESULT;
begin
  FQueriedForAsyncReader := false;
  Result := inherited CheckConnect(Pin);
end;

function TStreamOutPin.CompleteConnect(ReceivePin: IPin): HRESULT;
begin
  GFConnected := true;
  if (FQueriedForAsyncReader) then
    Result := inherited CompleteConnect(ReceivePin)
  else
    Result := VFW_E_NO_TRANSPORT;
end;

function TStreamOutPin.NonDelegatingQueryInterface(const IID: TGUID;
  out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IID_IAsyncReader) then
    FQueriedForAsyncReader := true;
  Result := inherited NonDelegatingQueryInterface(IID, Obj);
end;

function TStreamOutPin.BreakConnect: HRESULT;
begin
  FQueriedForAsyncReader := false;
  Result := inherited BreakConnect;
end;

end.

