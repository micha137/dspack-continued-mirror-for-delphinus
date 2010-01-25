program FilterEnum;

uses
  Forms,
  Editor in 'Editor.pas' {FormEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEditor, FormEditor);
  Application.Run;
end.
