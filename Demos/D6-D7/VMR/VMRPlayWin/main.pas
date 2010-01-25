unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, DSPack, StdCtrls, ComCtrls, dsutil, Buttons, ToolWin,
  ImgList, SelectURL, ExtCtrls, DirectShow9;

  
type
  TFormPlayWin = class(TForm)
    FilterGraph: TFilterGraph;
    VideoWindow: TVideoWindow;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    FileMenu: TMenuItem;
    OpenMenu: TMenuItem;
    OpenURLMenu: TMenuItem;
    ExitMenu: TMenuItem;
    TrackBar: TDSTrackBar;
    ImageList: TImageList;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    btPlay: TToolButton;
    btPause: TToolButton;
    btStop: TToolButton;
    ToolButton1: TToolButton;
    SoundLevel: TTrackBar;
    btFullScreen: TToolButton;
    PopupMenu: TPopupMenu;
    Play1: TMenuItem;
    Pause1: TMenuItem;
    Stop1: TMenuItem;
    FullScreen1: TMenuItem;
    log: TMemo;
    ToolButton2: TToolButton;
    VideoControl: TPanel;
    Brightness: TTrackBar;
    Contrast: TTrackBar;
    Hue: TTrackBar;
    Saturation: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PositionX: TTrackBar;
    PositionY: TTrackBar;
    SizeX: TTrackBar;
    SizeY: TTrackBar;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    AspectRatio: TCheckBox;
    procedure OpenMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btPlayClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure TrackBarTimer(sender: TObject; CurrentPos,
      StopPos: Cardinal);
    procedure SoundLevelChange(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure OpenURLMenuClick(Sender: TObject);
    procedure btFullScreenClick(Sender: TObject);
    procedure FilterGraphDSEvent(sender: TComponent; Event, Param1,
      Param2: Integer);
    procedure AspectRatioClick(Sender: TObject);
    procedure BrightnessChange(Sender: TObject);
    procedure ContrastChange(Sender: TObject);
    procedure HueChange(Sender: TObject);
    procedure SaturationChange(Sender: TObject);
    procedure PositionChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ReadStreamProperties;
  end;

var
  FormPlayWin: TFormPlayWin;
  ProcAmpControl: TVMR9ProcAmpControl;
  NormalizedRect: TVMR9NormalizedRect;
implementation

{$R *.dfm}

procedure TFormPlayWin.OpenMenuClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if not FilterGraph.Active then FilterGraph.Active := true;
    FilterGraph.ClearGraph;
    FilterGraph.RenderFile(OpenDialog.FileName);
    VideoWindow.PopupMenu := PopupMenu;
    SoundLevel.Position := FilterGraph.Volume;
    FilterGraph.Play;
    ReadStreamProperties;
  end;
end;

procedure TFormPlayWin.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // Important
  FilterGraph.ClearGraph;
end;

procedure TFormPlayWin.btPlayClick(Sender: TObject);
begin
  if not FilterGraph.Active then OpenMenuClick(nil)
                            else FilterGraph.play;
end;

procedure TFormPlayWin.btPauseClick(Sender: TObject);
begin
  FilterGraph.Pause;
end;

procedure TFormPlayWin.btStopClick(Sender: TObject);
begin
  FilterGraph.Stop;
end;

procedure TFormPlayWin.TrackBarTimer(sender: TObject; CurrentPos,
  StopPos: Cardinal);
begin
  StatusBar.SimpleText := format('Position: %s Duration: %s',
    [TimeToStr(CurrentPos / MiliSecPerDay), TimeToStr(StopPos / MiliSecPerDay)])
end;

procedure TFormPlayWin.SoundLevelChange(Sender: TObject);
begin
  FilterGraph.Volume := SoundLevel.Position;
end;

procedure TFormPlayWin.ExitMenuClick(Sender: TObject);
begin
  FormPlayWin.Close;
end;

procedure TFormPlayWin.OpenURLMenuClick(Sender: TObject);
begin
  FormSelectURL:= TFormSelectURL.Create(nil);
  if FormSelectURL.ShowModal = mrOK then
  begin
    if not FilterGraph.Active then FilterGraph.Active := true;
    FilterGraph.ClearGraph;
    FilterGraph.RenderFile(FormSelectURL.URL.Text);
    VideoWindow.PopupMenu := PopupMenu;
    SoundLevel.Position := FilterGraph.Volume;
    FilterGraph.Play;
  end;
  FormSelectURL.Free;
end;

procedure TFormPlayWin.btFullScreenClick(Sender: TObject);
begin
  VideoWindow.FullScreen := not VideoWindow.FullScreen;
  btFullScreen.Down := VideoWindow.FullScreen;
end;

procedure TFormPlayWin.FilterGraphDSEvent(sender: TComponent; Event,
  Param1, Param2: Integer);
begin
  log.Lines.Add(GetEventCodeDef(event))
end;

procedure TFormPlayWin.AspectRatioClick(Sender: TObject);
begin
  VideoWindow.VMROptions.KeepAspectRatio := AspectRatio.Checked;
end;

procedure TFormPlayWin.ReadStreamProperties;
var ProcAmpControlRange: TVMR9ProcAmpControlRange;
  procedure Configure(TrackBar: TTrackBar; Prop: DWord);
  begin
    with (VideoWindow as IVMRMixerControl9) do
    begin
      ZeroMemory(@ProcAmpControlRange, SizeOf(ProcAmpControlRange));
      ProcAmpControlRange.dwSize := SizeOf(ProcAmpControlRange);
      ProcAmpControlRange.dwProperty := Prop;
      if Succeeded(GetProcAmpControlRange(0, @ProcAmpControlRange)) then
      begin
        TrackBar.Min := Trunc(ProcAmpControlRange.MinValue);
        TrackBar.Max := Trunc(ProcAmpControlRange.MaxValue);
        if TrackBar.Min = TrackBar.Max then
          TrackBar.Enabled := False else
          begin
           TrackBar.Position := Trunc(ProcAmpControlRange.DefaultValue);
           TrackBar.Frequency := Trunc(ProcAmpControlRange.StepSize);
           TrackBar.Enabled := True;
          end;
      end else
      begin
        TrackBar.Min := 0;
        TrackBar.Max := 0;
        TrackBar.Position := 0;
        TrackBar.Frequency := 0;
        TrackBar.Enabled := False;
      end;
    end;
  end;
begin
  Configure(Contrast, ProcAmpControl9_Contrast);
  Configure(Brightness, ProcAmpControl9_Brightness);
  Configure(Hue, ProcAmpControl9_Hue);
  Configure(Saturation, ProcAmpControl9_Saturation);


  with (VideoWindow as IVMRMixerControl9) do
  begin
    if succeeded(GetOutputRect(0, @NormalizedRect)) then
    begin
      PositionX.Enabled := True;
      PositionY.Enabled := True;
      SizeX.Enabled := True;
      SizeY.Enabled := True;
    end else
    begin
      PositionX.Enabled := False;
      PositionY.Enabled := False;
      SizeX.Enabled := False;
      SizeY.Enabled := False;
    end;
  end;
end;

procedure TFormPlayWin.BrightnessChange(Sender: TObject);
begin
  ProcAmpControl.dwSize := sizeof(ProcAmpControl);
  ProcAmpControl.Brightness := Brightness.Position;
  ProcAmpControl.dwFlags := ProcAmpControl9_Brightness;
  with (VideoWindow as IVMRMixerControl9) do
    SetProcAmpControl(0, @ProcAmpControl);
end;

procedure TFormPlayWin.ContrastChange(Sender: TObject);
begin
  ProcAmpControl.dwSize := sizeof(ProcAmpControl);
  ProcAmpControl.Contrast := Contrast.Position;
  ProcAmpControl.dwFlags := ProcAmpControl9_Contrast;
  with (VideoWindow as IVMRMixerControl9) do
    SetProcAmpControl(0, @ProcAmpControl);
end;

procedure TFormPlayWin.HueChange(Sender: TObject);
begin
  ProcAmpControl.dwSize := sizeof(ProcAmpControl);
  ProcAmpControl.Hue := Hue.Position;
  ProcAmpControl.dwFlags := ProcAmpControl9_Hue;
  with (VideoWindow as IVMRMixerControl9) do
    SetProcAmpControl(0, @ProcAmpControl);
end;

procedure TFormPlayWin.SaturationChange(Sender: TObject);
begin
  ProcAmpControl.dwSize := sizeof(ProcAmpControl);
  ProcAmpControl.Saturation := Saturation.Position;
  ProcAmpControl.dwFlags := ProcAmpControl9_Saturation;
  with (VideoWindow as IVMRMixerControl9) do
    SetProcAmpControl(0, @ProcAmpControl);
end;

procedure TFormPlayWin.PositionChange(Sender: TObject);
begin
  NormalizedRect.left   := PositionX.Position / 100;
  NormalizedRect.right  := (PositionX.Position + SizeX.Position) / 100;
  NormalizedRect.Top    := PositionY.Position / 100;
  NormalizedRect.Bottom := (PositionY.Position + SizeY.Position) / 100;
  with (VideoWindow as IVMRMixerControl9) do
    SetOutputRect(0, @NormalizedRect);
end;

end.
