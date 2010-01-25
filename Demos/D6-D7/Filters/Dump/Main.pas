// *****************************************************************************
// Author: komarov andrey.
// Email: komar@elecard.net.ru
// This filter is a useful debugging tool. For example, you can verify,
// bit by bit, the results of a transform filter. You can build a graph
// manually by using GraphEdit, and connect the Dump filter to the output
// of a transform filter or any other output pin.
// *****************************************************************************

unit Main;

interface
uses BaseClass, DirectShow9, ActiveX, Windows, classes, Dialogs, Sysutils;

const CLSID_DumpNew : TGUID = '{62E78A56-7B3A-4CF0-B751-712B00C8E578}';

MyPinType: TRegPinTypes =
    (clsMajorType: @MEDIATYPE_NULL;
     clsMinorType: @MEDIASUBTYPE_NULL);

MyPins : array[0..0] of TRegFilterPins =
    ((strName: 'Input'; bRendered: FALSE; bOutput: FALSE; bZero: FALSE; bMany: FALSE;
      oFilter: nil; strConnectsToPin: nil; nMediaTypes: 1; lpMediaType: @MyPinType));

Type
  TDump = class;

  TMyPin = Class (TBCRenderedInputPin)  //TBCBasePin
  private
    FLast: TReferenceTime;
  {$IFDEF DEBUG}
    FDump: TDump;
  {$ENDIF}
    FFile: integer;
  public
    mF:string;
    constructor Create(ObjectName: string; pUnk: IUnKnown; Filter: TBCBaseFilter;
      Lock: TBCCritSec; out hr: HRESULT; Name: WideString; mpFileN:string);
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
    function Receive(pSample: IMediaSample): HRESULT; override;
    function EndOfStream: HRESULT; override;
    function BreakConnect: HRESULT; override;
    procedure OpenFile;
    Procedure CloseFile;
  end;

  /////////////////// Dump Class ////////////////////
  TDump = class (TBCBaseFilter,IFileSinkFilter)
  private
    xxx: integer;
    yyy: integer;
    FPin: TMyPin;
    FfileName: String;
  protected
    function SetFileName(pszFileName: PWideChar; pmt: PAMMediaType): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: PWideChar; pmt: PAMMediaType): HRESULT; stdcall;
  public
    function Stop: HRESULT; override;
    function Run(tStart: TReferenceTime): HRESULT; override;
    function GetPin(n: Integer): TBCBasePin; override;
    constructor Create(Name: string;           // Object description
                      Unk : IUnKnown;         // IUnknown of delegating object
                      Lock: TBCCritSec;       // Object who maintains lock
                      const clsid: TGUID      // The clsid to be used to serialize this filter
                      );
    function GetPinCount: integer; override;
  end;

implementation

procedure TMyPin.CloseFile;
begin
  FileClose(FFile);
end;

procedure TMyPin.OpenFile;
begin
  FFile := FileCreate(mF);
end;

constructor TMyPin.Create(ObjectName: string;pUnk: IUnKnown; Filter: TBCBaseFilter;
      Lock: TBCCritSec; out hr: HRESULT; Name: WideString; mpFileN:string);
begin
  inherited Create(ObjectName, Filter, Lock, hr, Name);
  mF := mpFileN;
  OpenFile;
  CloseFile;
  FLast := 0;
end;

function TMyPin.BreakConnect: HRESULT;
begin
  result := inherited BreakConnect;
end;

function TDump.Stop: HRESULT;
begin
  FPin.CloseFile;
  result := inherited Stop;
end;

function TMyPin.EndOfStream: HRESULT;
begin
 result := inherited EndOfStream;
end;

function TDump.Run(tStart: TReferenceTime): HRESULT;
begin
  FPin.OpenFile;
  result := inherited Run(tStart);
end;

function TMyPin.Receive(pSample: IMediaSample): HRESULT;
var
  pbData: PBYTE;
  tStart, tStop: TREFERENCETIME;
begin
  pSample.GetTime(tStart,tStop);
{$IFDEF DEBUG}
  DbgLog(FDump,'Komar');
{$ENDIF}
  pSample.GetPointer(pbData);
  FileWrite(FFile, pbData^, pSample.GetActualDataLength);
  result := S_OK;
end;

function TMyPin.CheckMediaType(mt: PAMMediaType): HRESULT;
begin
  result := S_OK;
end;

function TDump.GetPinCount: integer;
begin
  result := 1;
end;

constructor TDump.Create(Name: string;         // Object description
                       Unk : IUnKnown;         // IUnknown of delegating object
                       Lock: TBCCritSec;       // Object who maintains lock
                       const clsid: TGUID      // The clsid to be used to serialize this filter
                       );
begin
  inherited create(Name,Unk,Lock,CLSID_DumpNew);
end;

function TDump.GetPin(n: Integer): TBCBasePin;
var
  hr: HRESULT;
begin
  if (xxx = 0) then
  begin
    xxx := 1;
    FPin := TMyPin.Create('Null input pin', GetOwner, self, TBCCritSec.Create, hr, 'Input', FfileName);//,PINDIR_INPUT);
  end;
  result := FPin;
end;

function TDump.SetFileName(pszFileName: PWideChar; pmt: PAMMediaType): HRESULT;
begin
  if Length(pszFileName) > MAX_PATH then
  begin
    result := ERROR_FILENAME_EXCED_RANGE;
    exit;
  end;
  FFileName := copy(pszFileName, 1, Length(pszFileName));
  if (yyy > 0) then
  begin
    FPin.CloseFile;
    FPin.mF := FFileName;
    FPin.OpenFile;
    FPin.CloseFile;
  end;
  inc(YYY);
  if FfileName = '' then
    Result := E_OUTOFMEMORY
  else
    result:=S_OK;
end;

function TDump.GetCurFile(out ppszFileName: PWideChar; pmt: PAMMediaType): HRESULT;
begin
  ppszFileName := StringToOleStr(copy(FfileName, 1, Length(FfileName)));
  pmt.majortype := MEDIATYPE_NULL;
  pmt.subtype := MEDIASUBTYPE_NULL;
  result := S_OK;
end;

initialization
  TBCClassFactory.CreateFilter(TDump, 'Dump New', CLSID_DumpNew,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE, 1, @MyPins);
end.
