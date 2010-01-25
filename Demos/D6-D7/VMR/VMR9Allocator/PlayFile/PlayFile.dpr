program PlayFile;

uses
  Forms,
  main in 'main.pas' {MainForm},
  PlaneScene in '..\Allocator\PlaneScene.pas',
  Allocator in '..\Allocator\Allocator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
