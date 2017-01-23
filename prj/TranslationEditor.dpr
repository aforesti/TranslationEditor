program TranslationEditor;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  form.editor in '..\src\form.editor.pas' {FormEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormEditor, FormEditor);
  Application.Run;
end.
