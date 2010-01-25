program VMRMixer;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormVMRMixer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormVMRMixer, FormVMRMixer);
  Application.Run;
end.
