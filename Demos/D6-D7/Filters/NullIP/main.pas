unit main;

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface
uses BaseClass, ActiveX, DirectShow9, Windows, DSUTil, PropEdit;

const
  CLSID_NullInPlace        : TGUID = '{52b63860-dc93-11ce-a099-00aa00479a58}';
  IID_INullIPP             : TGUID = '{43D849C0-2FE8-11cf-BCB1-444553540000}';

type

  INullIPP = interface(IunKnown)
  ['{0952C77F-2EFF-427B-ACAD-F295ADE6F1E7}']
    function put_MediaType(mt: PAMMediaType): HRESULT; stdcall;      // the media type selected
    function get_MediaType(out mt: TAMMediaType): HRESULT; stdcall;  // the media type selected
    function get_IPin(out Pin: IPin): HRESULT; stdcall;                // the source pin
    function get_State(out State: TFilterState): HRESULT; stdcall;    // the filter state
  end;

const

  SudPinTypes : TRegPinTypes =
    (clsMajorType: @MEDIATYPE_NULL;
     clsMinorType: @MEDIASUBTYPE_NULL);

  SudPins : array[0..1] of TRegFilterPins =
    ((strName: 'Input'; bRendered: FALSE; bOutput: FALSE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: 'Output'; nMediaTypes: 1; lpMediaType: @SudPinTypes),
     (strName: 'Output'; bRendered: FALSE; bOutput: TRUE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: 'Input'; nMediaTypes: 1; lpMediaType: @SudPinTypes));

type

  TNullInPlaceInputPin = class(TBCTransInPlaceInputPin)
  public
    constructor Create(ObjectName: string; TransInPlaceFilter: TBCTransInPlaceFilter;
      out hr: HRESULT; Name: WideString);
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
  end;

  TNullInPlaceOutputPin = class(TBCTransInPlaceOutputPin)
  public
    constructor Create(ObjectName: string; TransInPlaceFilter: TBCTransInPlaceFilter;
      out hr: HRESULT; Name: WideString);
    function CheckMediaType(mt: PAMMediaType): HRESULT; override;
  end;

var
    // If there are multiple instances of this filter active, it's
    // useful for debug messages etc. to know which one this is.
  InstanceCount: integer = 0;

type

  TNullInPlace = class(TBCTransInPlaceFilter, INullIPP, ISpecifyPropertyPages)
    FThisInstance: integer;
    FPreferred: TAMMediaType; // Media type chosen from property sheet
    NullIPLock: TBCCritSec;     // To serialise access.
  public
     function GetPin(n: integer): TBCBasePin; override;
     function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function put_MediaType(mt: PAMMediaType): HRESULT; stdcall;
    function get_MediaType(out mt: TAMMediaType): HRESULT; stdcall;
    function get_IPin(out Pin: IPin): HRESULT; stdcall;
    function get_State(out State: TFilterState): HRESULT; stdcall;          //

    // --- ISpecifyPropertyPages ---
    function GetPages(out pages: TCAGUID): HResult; stdcall;

    constructor Create(ObjName: string; unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;

    // Overrides the PURE virtual Transform of CTransInPlaceFilter base class
    // This is where the "real work" is done.
    function Transform(Sample: IMediaSample): HRESULT; override;
  end;

implementation



{ TNullInPlaceInputPin }

// CheckMediaType
//
// Override CTransInPlaceInputPin method.
// If we have been given a preferred media type from the property sheet
// then only accept a type that is exactly that.
// else if there is nothing downstream, then accept anything
// else if there is a downstream connection then first check to see if
// the subtype (and implicitly the major type) are different from the downstream
// connection and if they are different, fail them
// else ask the downstream input pin if the type (i.e. all details of it)
// are acceptable and take that as our answer.

function TNullInPlaceInputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
var
  pmt: PAMMediaType;
begin
{$IFDEF DEBUG}
   DbgLog(self, 'Input type proposed');
{$ENDIF}
    pmt := @TNullInPlace(FTIPFilter).FPreferred;
    if not TBCMediaType(pmt).IsValid then
      begin
        if TNullInPlace(FTIPFilter).Output.IsConnected then
          begin

            //  We used to check here if the subtype of the proposed type
            //  matched the subtype of the type on the output pin
            //  but this broke as follows:
            //
            //  Renderering the output pin of a CODEC we picked up
            //  2 NULLIPs already in the graph:
            //
            //  Subtypes      Y41P       Y41P       RGB565
            //  Filters  CODEC---->NULLIP---->NULLIP------>RENDERER
            //
            //  Each NULLIP has scheduled a reconnect at this point
            //  and the reconnect on the first connection happens
            //  first:
            //
            //  Subtypes                 Y41P       RGB565
            //  Filters  CODEC     NULLIP---->NULLIP------>RENDERER
            //
            //  In trying to (re)connect the CODEC to the first NULLIP
            //  we first propose (say) Y41P and the first NULLIP
            //  checks that Y41P is the same as its output type
            //  so the call gets passed to the QueryAccept of
            //  the second NULLIP.  The second NULLIP rejected the
            //  call because the subtype on its output pin is not
            //  RGB565.  In a similar way the first NULLIP
            //  rejected Y41P.
            //
            //  By removing this optimization (checking the
            //  subtype before passing the call on) we avoided
            //  the problem.

            result :=  TNullInPlace(FTIPFilter).Output.GetConnected.QueryAccept(mt^);
            exit;
        end;
         result := S_OK;
         exit;
      end
    else
        if TBCMediaType(pmt).Equal(mt) then
          begin
            result := S_OK;
            exit;
          end
        else
          result := VFW_E_TYPE_NOT_ACCEPTED;
end;

constructor TNullInPlaceInputPin.Create(ObjectName: string;
  TransInPlaceFilter: TBCTransInPlaceFilter; out hr: HRESULT;
  Name: WideString);
begin
  inherited Create(ObjectName, TransInPlaceFilter, hr, Name);
end;

{ TNullInPlaceOutputPin }

function TNullInPlaceOutputPin.CheckMediaType(mt: PAMMediaType): HRESULT;
var pmt: PAMMediaType;
begin
  pmt := @TNullInPlace(FTIPFilter).FPreferred;
  if not TBCMediaType(pmt).IsValid then
    begin
      result := inherited CheckMediaType(mt);
      exit;
    end
  else
    if TBCMediaType(pmt).Equal(mt) then
      begin
        result := S_OK;
        exit;
      end
    else
      result := VFW_E_TYPE_NOT_ACCEPTED;
end;

constructor TNullInPlaceOutputPin.Create(ObjectName: string;
  TransInPlaceFilter: TBCTransInPlaceFilter; out hr: HRESULT;
  Name: WideString);
begin
  inherited Create(ObjectName, TransInPlaceFilter, hr, Name);
end;

{ TNullInPlace }

function TNullInPlace.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  result := S_OK;
end;

constructor TNullInPlace.Create(ObjName: string; unk: IInterface;
  out hr: HRESULT);
var pmt: PAMMediaType;
begin
  inherited Create(ObjName, unk, CLSID_NullInPlace, hr);
  FThisInstance := InterlockedIncrement(InstanceCount);
  pmt := @FPreferred;
  TBCMediaType(pmt).InitMediaType;
  NullIPLock := TBCCritSec.Create;
{$IFDEF DEBUG}
  DbgLog(self, 'TNullInPlace.Create');
{$ENDIF}
end;

constructor TNullInPlace.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IInterface);
var hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TNullInPlace.Destroy;
begin
  NullIPLock.Free;
  inherited;
end;

function TNullInPlace.get_IPin(out Pin: IPin): HRESULT;
begin
  result := S_OK;
  NullIPLock.Lock;
  try
    if (Input = nil) then
      begin
        Pin := nil;
        exit;
      end;
    if not Input.IsConnected then
         Pin := nil
    else Pin := Input.GetConnected;
  finally
    NullIPLock.UnLock;
  end;
end;

function TNullInPlace.get_MediaType(out mt: TAMMediaType): HRESULT;
begin
  NullIPLock.Lock;
  try
    mt := FPreferred;
    result := NOERROR ;
  finally
    NullIPLock.UnLock;
  end;
end;

function TNullInPlace.get_State(out State: TFilterState): HRESULT;
begin
  NullIPLock.Lock;
  try
    State := self.State;
    result := NOERROR ;
  finally
    NullIPLock.UnLock;
  end;
end;

function TNullInPlace.GetPages(out pages: TCAGUID): HResult;
begin
    Pages.cElems := 1;
    Pages.pElems := CoTaskMemAlloc(sizeof(TGUID));
    if (Pages.pElems = nil) then
      begin
        result := E_OUTOFMEMORY;
        exit;
      end;
   Pages.pElems^[0] := CLSID_NullIPPropertyPage;
   result := NOERROR;
end;

function TNullInPlace.GetPin(n: integer): TBCBasePin;
var hr: HRESULT;
begin
  // Create the single input pin and the single output pin
  // If anything fails, fail the whole lot and clean up.
  if (Input = nil) or (Output = nil) then
    begin
      hr := S_OK;
      Input := TNullInPlaceInputPin.Create('Null input pin', self, hr, 'Input');
      // a failed return code should delete the object

      if FAILED(hr) or (Input = nil) then
        begin
          if (Input <> nil) then input.Free;
          input := nil;
          result := nil;
          exit;
        end;

      Output := TNullInPlaceOutputPin.Create('Null output pin', self, hr, 'Output');

      // failed return codes cause both objects to be deleted

      if FAILED(hr) or (Output = nil) then
        begin
          if (Input  <> nil) then input.Free;
          if (Output <> nil) then Output.Free;
          Input  := nil;
          Output := nil;
          result := nil;
          exit;
        end;
    end;

  // Find which pin is required

  case n of
    0: result := Input;
    1: result := Output;
  else
    result := nil;
  end;
end;

function TNullInPlace.put_MediaType(mt: PAMMediaType): HRESULT;
var
  Pin: IPin;
  pmt: PAMMediaType;
begin
  NullIPLock.Lock;
  try
    // if the state of the graph is running, fail the call.
    if (State = State_Running) then
      begin
        result := E_UNEXPECTED;
        exit;
      end;

    // check the source and sink filters like this media type
    pmt := @FPreferred;
    if (mt = nil) then
        TBCMediaType(pmt).InitMediaType
    else
      begin
        Pin := Input.GetConnected;
        if (Pin <> nil) then
        begin
          if (Pin.QueryAccept(mt^) <> NOERROR) then
          begin
            MessageBox(0,PChar('Upstream filter cannot provide this type'),
                         PChar('Format Selection'),
                         MB_OK or MB_ICONEXCLAMATION);
            result := VFW_E_TYPE_NOT_ACCEPTED;
            exit;
          end;
        end;

        Pin := Output.GetConnected;
        if (Pin <> nil) then
        begin
          if (Pin.QueryAccept(mt^) <> NOERROR) then
          begin
            MessageBox(0, PChar('Downstream filter cannot accept this type'),
                          PChar('Format Selection'),
                          MB_OK or MB_ICONEXCLAMATION);
            result := VFW_E_TYPE_NOT_ACCEPTED;
            exit;
          end;
        end;
        FPreferred := mt^;
     end;

    // force reconnect of input if the media type of connection does not match.
    if (Input.IsConnected) then
    begin
      pmt := Input.CurrentMediaType.MediaType;
      if not TBCMediaType(pmt).Equal(@FPreferred) then
        Graph.Reconnect(Input);
    end;
    result := NOERROR ;
  finally
    NullIPLock.Unlock;
  end;
end;

function TNullInPlace.Transform(Sample: IMediaSample): HRESULT;
begin
  result := S_OK;
end;

initialization
  TBCClassFactory.CreateFilter(TNullInPlace, 'Null-In-Place', CLSID_NullInPlace,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE, 2, @SudPins);
end.
