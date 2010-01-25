unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, DSPack, StdCtrls, ComCtrls, dsutil, Buttons, ToolWin,
  ImgList, SelectURL;

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

  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormPlayWin: TFormPlayWin;

implementation
uses DirectShow9;

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

end.
