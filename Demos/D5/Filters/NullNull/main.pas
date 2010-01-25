unit main;

interface
uses BaseClass, ActiveX, DirectShow9, Windows;

const
  CLSID_MyClass : TGUID = '{90A70CF4-A445-4211-B962-308054E93023}';

  MyPinTypes : TRegPinTypes =
    (clsMajorType: @MEDIATYPE_NULL;
     clsMinorType: @MEDIASUBTYPE_NULL);

  MyPins : array[0..1] of TRegFilterPins =
    ((strName: 'Input'; bRendered: FALSE; bOutput: FALSE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: nil; nMediaTypes: 1; lpMediaType: @MyPinTypes),
     (strName: 'Output'; bRendered: FALSE; bOutput: TRUE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: nil; nMediaTypes: 1; lpMediaType: @MyPinTypes));

type
  TMyClass = class(TBCTransInPlaceFilter)
    // Overrides the PURE virtual Transform of CTransInPlaceFilter base class
    // This is where the "real work" is done by altering *pSample.
    // We do the Null transform by leaving it alone.
    function Transform(Sample: IMediaSample): HRESULT; override;

    // We accept any input type.  We'd return S_FALSE for any we didn't like.
    function CheckInputType(mtin: PAMMediaType): HRESULT; override;

  end;

implementation

{ TMyClass }

function TMyClass.CheckInputType(mtin: PAMMediaType): HRESULT;
begin
  result := S_OK;
end;

function TMyClass.Transform(Sample: IMediaSample): HRESULT;
begin
  result := NOERROR;
end;

initialization
  TBCClassFactory.CreateFilter(TMyClass, 'Null-Null', CLSID_MyClass,
    CLSID_LegacyAmFilterCategory, MERIT_DO_NOT_USE, 2, @MyPins);
end.
