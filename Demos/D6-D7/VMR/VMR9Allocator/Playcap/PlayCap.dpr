program PlayCap;

uses
  Forms,
  main in 'main.pas' {VideoForm},
  Allocator in '..\Allocator\Allocator.pas',
  PlaneScene in '..\Allocator\PlaneScene.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVideoForm, VideoForm);
  Application.Run;
end.
