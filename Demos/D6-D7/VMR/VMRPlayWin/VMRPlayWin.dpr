program VMRPlayWin;

uses
  Forms,
  main in 'main.pas' {FormPlayWin},
  SelectURL in 'SelectURL.pas' {FormSelectURL};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPlayWin, FormPlayWin);
  Application.Run;
end.
