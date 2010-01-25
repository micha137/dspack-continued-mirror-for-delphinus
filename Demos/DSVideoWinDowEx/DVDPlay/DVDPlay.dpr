program DVDPlay;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormDVDPlayer},
  ColorControl in 'ColorControl.pas' {ColorControlForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDVDPlayer, FormDVDPlayer);
  Application.CreateForm(TColorControlForm, ColorControlForm);
  Application.Run;
end.
