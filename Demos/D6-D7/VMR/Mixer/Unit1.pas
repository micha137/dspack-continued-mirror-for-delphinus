unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSPack, DSUtil, DirectShow9, ComCtrls;

type
  TFormVMRMixer = class(TForm)
    FilterGraph: TFilterGraph;
    VideoWindow: TVideoWindow;
    btRender: TButton;
    Alpha: TTrackBar;
    OpenDialog: TOpenDialog;
    CoordX: TTrackBar;
    CoordY: TTrackBar;
    Label1: TLabel;
    procedure btRenderClick(Sender: TObject);
    procedure AlphaChange(Sender: TObject);
    procedure CoordChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormVMRMixer: TFormVMRMixer;
  ARect: TVMR9NORMALIZEDRECT;

implementation

{$R *.dfm}

procedure TFormVMRMixer.btRenderClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FilterGraph.ClearGraph;
    ARect.left   := 0;
    ARect.top    := 0;
    ARect.right  := CoordX.Position / 100;
    ARect.bottom := CoordY.Position / 100;
    with (VideoWindow as IVMRMixerControl9) do
    begin
      SetOutputRect(1,@ARect);
      SetAlpha(1,Alpha.Position/100);
      SetZOrder(1,0)
    end;
    FilterGraph.RenderFile(OpenDialog.FileName);
    FilterGraph.RenderFile(ExtractFilePath(Application.ExeName) + '\penguin.gif');
    FilterGraph.Play;
  end;
end;

procedure TFormVMRMixer.AlphaChange(Sender: TObject);
begin
  (VideoWindow as IVMRMixerControl9).SetAlpha(1,Alpha.Position/100);
end;

procedure TFormVMRMixer.CoordChange(Sender: TObject);
begin
  ARect.left   := 0;
  ARect.top    := 0;
  ARect.right  := CoordX.Position / 100;
  ARect.bottom := CoordY.Position / 100;
  (VideoWindow as IVMRMixerControl9).SetOutputRect(1,@ARect)
end;

end.
