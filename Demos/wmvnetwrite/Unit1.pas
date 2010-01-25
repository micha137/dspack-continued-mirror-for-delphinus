unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, activex, NetWrite,wmf9;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    OpenDialog: TOpenDialog;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  net: TWMFNetWrite;
implementation

{$R *.dfm}
procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if assigned(net) then net.free;
    net := TWMFNetWrite.Create;
    button2.Enabled := true;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var hr: HRESULT;
begin
  Button2.Enabled := false;
  hr := net.Init;
  memo1.Lines.Add(inttohex(hr,8));
  if Failed(hr) then exit;
  hr := net.Configure(UpDown1.Position, StringToOleStr(Opendialog.FileName), UpDown2.Position);
  memo1.Lines.Add(inttohex(hr,8));
  if Failed(hr) then exit;
  hr := net.WritetoNet;
  memo1.Lines.Add(inttohex(hr,8));
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(net) then net.free;
end;

end.
