program PlayWin;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ColorControl in 'ColorControl.pas' {ColorControlForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TColorControlForm, ColorControlForm);
  Application.Run;
end.
