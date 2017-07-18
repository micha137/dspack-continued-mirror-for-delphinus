unit PauseFilter;

interface

uses
   BaseClass, DirectShow9, DSPack;

const
  CLSID_PauseFilter: TGUID = '{D7F33645-624D-46A5-8E6C-88E853344E55}';
type
  {$M+}
  TPauseFilter = class(TBCTransInPlaceFilter)
    function Transform(Sample: IMediaSample): HRESULT; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
  protected
    FLastForwardedSampleTimeEnd, FPauseTimes: REFERENCE_TIME;
    FPause, FSwitchingPauseState: Boolean;
    FSkipped: Cardinal;
    procedure SetPause(const APause: Boolean);
  public
    constructor Create(Name: PChar; Unk: IUnknown; hr: HResult);
  published
    property ForwardingPause: Boolean read FPause write SetPause stored False;
  end;

procedure ForwardingPause(f: TFilterGraph; const Pause: Boolean);

implementation

uses
  DXSUtil,
  SysUtils,
  Winapi.Windows;

function TPauseFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  Result := S_OK;
  FPauseTimes := 0;
end;

constructor TPauseFilter.Create(Name: PChar; Unk: IInterface; hr: HResult);
begin
  inherited Create(Name, Unk, CLSID_PauseFilter, hr);
end;

procedure TPauseFilter.SetPause(const APause: Boolean);
begin
  if FPause=APause then Exit;

  // We are entering or leaving a pause:
  // On entering, we will wait for the next keyframe.
  // On leaving, we will calculate the timestamp difference for the lenght of the last pause at the next key frame
  FSwitchingPauseState := True;

  FPause := APause;
end;

function TPauseFilter.Transform(Sample: IMediaSample): HRESULT;
var startTime, endTime, sampleDuration: REFERENCE_TIME;
label forwardit, skipit;
begin
  if FPause then begin
    if FSwitchingPauseState then begin
      if Sample.IsSyncPoint<>S_OK then goto forwardit;
      {$IFDEF DEBUG}
      DbgLog(self, Format('Entering pause at I-frame', [ ] ));
      {$ENDIF}
      FSkipped := 0;
      FSwitchingPauseState := False;
    end;

    skipit:
    Inc(FSkipped);
    Result := S_FALSE;// don't forward it
    Exit;
  end;

  forwardit:
  CheckDSError(Sample.GetTime(startTime, endTime));

  if FSwitchingPauseState and Not FPause then begin
    if Sample.IsSyncPoint<>S_OK then goto skipit;

    sampleDuration := endTime-startTime;
    FPauseTimes := FPauseTimes + FSkipped * sampleDuration;
    {$IFDEF DEBUG}
    DbgLog(self, Format('Pausetimes=%d ms (%f Samples, Dampleduration=%d ms skipped=%d samples, last pause duration=%d ms, skipped*SampleDuration=%d ms)',
      [ RefTimeToMiliSec(FPauseTimes), FPauseTimes/sampleDuration,
        RefTimeToMiliSec(sampleDuration), FSkipped,
        RefTimeToMiliSec(startTime - FLastForwardedSampleTimeEnd),
        FSkipped*RefTimeToMiliSec(sampleDuration) ] ));
    {$ENDIF}
    FSwitchingPauseState := False;
    Sample.SetDiscontinuity(True);
  end;
  FLastForwardedSampleTimeEnd := endTime;

  startTime := startTime-FPauseTimes;
  endTime := endTime-FPauseTimes;
  CheckDSError(Sample.SetTime(@startTime, @endTime));
  Result := S_OK;// forward the sample
end;

procedure ForwardingPause(f: TFilterGraph; const Pause: Boolean);
var fl: TFilterList;
  i: Integer;
  c: TGUID;
begin
  fl := TFilterList.Create(f as IFilterGraph);
  try
    for i := 0 to fl.Count-1 do begin
      CheckDSError(fl[i].GetClassID(c));
      if Not IsEqualGUID(c, CLSID_PauseFilter) then Continue;
      (fl[i] as TPauseFilter).ForwardingPause := Pause;
    end;
  finally
    FreeAndNil(fl);
  end;
end;

end.
