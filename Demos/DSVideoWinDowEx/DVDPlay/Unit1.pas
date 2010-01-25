unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DSPack,
  Menus, ExtCtrls, ComCtrls, StdCtrls,directshow9, OleServer, DSUtil,
  ImgList, ToolWin, shellapi;

type
  TFormDVDPlayer = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    OpenMenu: TMenuItem;
    ExitMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    ControlMenu: TMenuItem;
    PlayMenu: TMenuItem;
    PauseMenu: TMenuItem;
    StopMenu: TMenuItem;
    ToolBar: TToolBar;
    btPreviousChapter: TToolButton;
    btRewind: TToolButton;
    btPause: TToolButton;
    btPlay: TToolButton;
    btStop: TToolButton;
    btFastForward: TToolButton;
    btNextChapter: TToolButton;
    ToolButton8: TToolButton;
    btRootMenu: TToolButton;
    btFullScreen: TToolButton;
    btFrameStep: TToolButton;
    StepForwardMenu: TMenuItem;
    OptionsMenu: TMenuItem;
    FullscreenMenu: TMenuItem;
    Popup: TPopupMenu;
    N1: TMenuItem;
    GoToMenu: TMenuItem;
    PlaySpeed1: TMenuItem;
    NextChapterMenu: TMenuItem;
    PreviousChapterMenu: TMenuItem;
    FastForward1: TMenuItem;
    Rewind1: TMenuItem;
    N4: TMenuItem;
    MenuRoot: TMenuItem;
    TitleMenu: TMenuItem;
    N5: TMenuItem;
    BookmarksMenu: TMenuItem;
    SavebookmarkMenu: TMenuItem;
    RestorebookmarkMenu: TMenuItem;
    Play2: TMenuItem;
    Pause2: TMenuItem;
    Stop2: TMenuItem;
    N2: TMenuItem;
    GoTo2: TMenuItem;
    NextChapter2: TMenuItem;
    PreviousChapter2: TMenuItem;
    PlaySpeed2: TMenuItem;
    FastForward2: TMenuItem;
    Rewind2: TMenuItem;
    N3: TMenuItem;
    MenuRoot2: TMenuItem;
    TitleMenu2: TMenuItem;
    Fullscreen2: TMenuItem;
    N6: TMenuItem;
    Bookmarks2: TMenuItem;
    Savebookmark2: TMenuItem;
    Restorebookmark2: TMenuItem;
    StatusBar: TStatusBar;
    N7: TMenuItem;
    progdigycom1: TMenuItem;
    FilterGraph: TFilterGraph;
    ImageList: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ColorControls1: TMenuItem;
    ColorControls2: TMenuItem;
    AspectRatio1: TMenuItem;
    Stretched1: TMenuItem;
    LetterBox1: TMenuItem;
    Crop1: TMenuItem;
    AspetcRatio1: TMenuItem;
    Stretched2: TMenuItem;
    LetterBox2: TMenuItem;
    Crop2: TMenuItem;
    N8: TMenuItem;
    ToolButton3: TToolButton;
    TrackBar1: TTrackBar;
    test1: TMenuItem;
    DSVideoWindowEx1: TDSVideoWindowEx2;
    Panel1: TPanel;
    procedure PlayMenuClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btFullScreenClick(Sender: TObject);
    procedure btFrameStepClick(Sender: TObject);
    procedure btRootMenuClick(Sender: TObject);
    procedure VideoWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VideoWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btNextChapterClick(Sender: TObject);
    procedure btPreviousChapterClick(Sender: TObject);
    procedure btFastForwardClick(Sender: TObject);
    procedure btRewindClick(Sender: TObject);
    procedure TitleMenuClick(Sender: TObject);
    procedure SavebookmarkMenuClick(Sender: TObject);
    procedure RestorebookmarkMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    procedure FilterGraphDVDTitleChange(sender: TObject; title: Integer);
    procedure FilterGraphDVDChapterStart(sender: TObject;
      chapter: Integer);
    procedure DSVideoWindowEx1ColorKeyChanged(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Stretched1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckColorControlSupport;
    procedure FilterGraphDVDCurrentHMSFTime(sender: TObject;
      HMSFTimeCode: tagDVD_HMSF_TIMECODE; TimeCode: tagDVD_TIMECODE);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure PlayDVD(FileName: WideString);
  end;

var
  FormDVDPlayer: TFormDVDPlayer;

implementation

uses ColorControl;

{$R *.DFM}

procedure TFormDVDPlayer.PlayDVD(FileName: WideString);
var
  Status : TAMDVDRenderStatus;
  DvdCmd: IDvdCmd;
  HR: HRESULT;
  DVDControl2 : IDVDControl2;
begin
  // Activate the filter Graph
  if not FilterGraph.Active then
  begin
    FilterGraph.Active := true;
    // Render DVD
    HR := FilterGraph.RenderDvd(Status, FileName);
    if HR <> S_OK then
    begin
      case HR of
        HRESULT(E_INVALIDARG): Application.MessageBox('Invalid Argument.','Error', mb_ok);
        HRESULT(S_FALSE)     : begin
                        if Status.hrVPEStatus <> 0 then Application.MessageBox(PChar(GetErrorString(Status.hrVPEStatus)), 'Error', mb_OK);
                        if Status.bDvdVolInvalid   then Application.MessageBox('The specified DVD volume to be played does not exist.', 'Error', mb_OK);
                        if Status.bDvdVolUnknown   then Application.MessageBox('No DVD volume is specified or isn''t found.', 'Error', mb_OK);
                        if Status.bNoLine21In      then Application.MessageBox('The video decoder doesn''t produce line 21 (closed captioning) data.', 'Error', mb_OK);
                        if Status.bNoLine21Out     then Application.MessageBox('The video decoder can''t be shown as closed captioning on video due to a problem with graph building.', 'Error', mb_OK);
                        if status.iNumStreamsFailed > 0 then Application.MessageBox('Can''t render one or more stream.', 'Error', mb_OK);
                      end;
        HRESULT(VFW_E_DVD_DECNOTENOUGH) : Application.MessageBox('There isn''t enough hardware or software decoders to decode all streams.','Error', mb_OK);
        HRESULT(VFW_E_DVD_RENDERFAIL)   : Application.MessageBox('Some basic error occurred in building the graph.'#13'Possibilities include the DVD Navigator filter or the video or audio renderer not instantiating,'#13'a trivial connection or pin enumeration failing, or none of the streams rendering.','Error', mb_OK);
      end;
      FilterGraph.ClearGraph;
      exit;
    end;
    FilterGraph.Play;

    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
    begin
      DVDControl2.SetOption(DVD_NotifyParentalLevelChange,false); //not notify us when parental level changes
      DVDControl2.SetOption(DVD_HMSF_TimeCodeEvents, true);       // use new HMSF timecode format
    end;
  end
  else
  begin
    FilterGraph.Play;
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DVDControl2.PlayForwards(1.0,DVD_CMD_FLAG_None, DvdCmd);
  end;
  CheckColorControlSupport;
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.PlayMenuClick(Sender: TObject);
begin
  PlayDVD('');
end;

procedure TFormDVDPlayer.btStopClick(Sender: TObject);
begin
  FilterGraph.Stop;
end;

procedure TFormDVDPlayer.btPauseClick(Sender: TObject);
begin
  FilterGraph.Pause;
end;

procedure TFormDVDPlayer.btFullScreenClick(Sender: TObject);
begin
  If DSVideoWindowEx1.FullScreen then
    DSVideoWindowEx1.NormalPlayback
  else
    DSVideoWindowEx1.StartFullScreen;
  btFullScreen.Down := DSVideoWindowEx1.FullScreen;
end;

procedure TFormDVDPlayer.btFrameStepClick(Sender: TObject);
var
  FrameStep: IVideoFrameStep;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IVideoFrameStep, FrameStep)) then
      FrameStep.Step(1,nil);
  FrameStep := Nil;
end;

procedure TFormDVDPlayer.btRootMenuClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.ShowMenu(DVD_MENU_Root, 0, DvdCmd);
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.VideoWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
    begin
      Point.x := x;
      Point.y := y;
      DVDControl2.SelectAtPosition(Point);
      DVDControl2 := Nil;
    end;
end;

procedure TFormDVDPlayer.VideoWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
    begin
      Point.x := x;
      Point.y := y;
      DVDControl2.ActivateAtPosition(Point);
      DVDControl2 := Nil;
    end;
end;

procedure TFormDVDPlayer.btNextChapterClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.PlayNextChapter(DVD_CMD_FLAG_None, DvdCmd);
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.btPreviousChapterClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.PlayPrevChapter(DVD_CMD_FLAG_None, DvdCmd);
  DVDControl2 := Nil;
end;


procedure TFormDVDPlayer.btFastForwardClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.PlayForwards(8.0,DVD_CMD_FLAG_None, DvdCmd);
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.btRewindClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.PlayBackwards(8.0,DVD_CMD_FLAG_None, DvdCmd);
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.TitleMenuClick(Sender: TObject);
var
  DvdCmd: IDvdCmd;
  DVDControl2 : IDVDControl2;
begin
  if FilterGraph.Active then
    If Succeeded(FilterGraph.QueryInterface(IID_IDVDControl2, DVDControl2)) then
      DvdControl2.ShowMenu(DVD_MENU_Title, DVD_CMD_FLAG_None, DvdCmd);
  DVDControl2 := Nil;
end;

procedure TFormDVDPlayer.SavebookmarkMenuClick(Sender: TObject);
begin
  FilterGraph.DVDSaveBookmark(ExtractFilePath(Application.EXEName)+'bookmark.bmk');
end;

procedure TFormDVDPlayer.RestorebookmarkMenuClick(Sender: TObject);
begin
  FilterGraph.DVDRestoreBookmark(ExtractFilePath(Application.EXEName)+'bookmark.bmk');
end;

procedure TFormDVDPlayer.ExitMenuClick(Sender: TObject);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
  Application.Terminate;
end;

procedure TFormDVDPlayer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  ExitMenuClick(nil)
end;

procedure TFormDVDPlayer.OpenMenuClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FilterGraph.ClearGraph;
    FilterGraph.Active := false;
    PlayDVD(OpenDialog.FileName);
  end;

end;

procedure TFormDVDPlayer.FilterGraphDVDTitleChange(sender: TObject;
  title: Integer);
begin
  statusbar.Panels.Items[1].Text := 'Title '+inttostr(title);
end;

procedure TFormDVDPlayer.FilterGraphDVDChapterStart(sender: TObject;
  chapter: Integer);
begin
  statusbar.Panels.Items[2].Text := 'Chapter '+ inttostr(chapter);
end;

procedure TFormDVDPlayer.DSVideoWindowEx1ColorKeyChanged(Sender: TObject);
begin
  Panel1.Color := DSVideoWindowEx1.ColorKey;
end;

procedure TFormDVDPlayer.ToolButton2Click(Sender: TObject);
begin
  ColorControlForm.Show;
end;

procedure TFormDVDPlayer.Stretched1Click(Sender: TObject);
begin
  DSVideoWindowEx1.AspectRatio := TRatioModes(TMenuItem(Sender).Tag);
  TMenuItem(Sender).Checked := True;  
end;

procedure TFormDVDPlayer.TrackBar1Change(Sender: TObject);
begin
  DSVideoWindowEx1.DigitalZoom := TrackBar1.Position;
end;

procedure TFormDVDPlayer.CheckColorControlSupport;
Begin
  ToolButton2.Enabled := True;
  ColorControls1.Enabled := True;
  ColorControls2.Enabled := True;

//  ToolButton2.Enabled :=DSVideoWindowEx1.GetColorControlCapabilities and DDCOLOR_BRIGHTNESS = DDCOLOR_BRIGHTNESS;
//  ColorControls1.Enabled :=DSVideoWindowEx1.GetColorControlCapabilities and DDCOLOR_BRIGHTNESS=DDCOLOR_BRIGHTNESS;
//  ColorControls2.Enabled :=DSVideoWindowEx1.GetColorControlCapabilities and DDCOLOR_BRIGHTNESS=DDCOLOR_BRIGHTNESS;
End;

procedure TFormDVDPlayer.FilterGraphDVDCurrentHMSFTime(sender: TObject;
  HMSFTimeCode: tagDVD_HMSF_TIMECODE; TimeCode: tagDVD_TIMECODE);
begin
  with HMSFTimeCode do
  Begin
    StatusBar.Panels.Items[0].Text :=
      format('%d:%2.2d:%2.2d',[bHours, bMinutes ,bSeconds]);
    Panel1.Caption :=
      format('%d:%2.2d:%2.2d',[bHours, bMinutes ,bSeconds]);
  End;
end;

end.
