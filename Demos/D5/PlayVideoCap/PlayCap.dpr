program PlayCap;

uses
  Forms,
  main in 'main.pas' {VideoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVideoForm, VideoForm);
  Application.Run;
end.
