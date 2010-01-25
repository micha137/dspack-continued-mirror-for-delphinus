program DVDPlay;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormDVDPlayer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDVDPlayer, FormDVDPlayer);
  Application.Run;
end.
