program buki;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {MainForm},
  uConst in 'uConst.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
