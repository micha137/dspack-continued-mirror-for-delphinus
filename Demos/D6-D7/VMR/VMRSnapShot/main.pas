unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSPack, ExtCtrls, DirectShow9, jpeg, ImgList, ComCtrls;

type
  TForm1 = class(TForm)
    VideoWindow: TVideoWindow;
    FilterGraph: TFilterGraph;
    Button2: TButton;
    SnapShot: TImage;
    Button1: TButton;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public

  end;

var
  Form1: TForm1;
implementation
uses activex;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FilterGraph.ClearGraph;
    FilterGraph.RenderFile(OpenDialog.FileName);
    FilterGraph.Play;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream:= TMemoryStream.Create;
  try
    if VideoWindow.VMRGetBitmap(Stream) then
      SnapShot.Picture.bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.



