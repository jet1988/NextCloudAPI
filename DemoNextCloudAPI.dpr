program DemoNextCloudAPI;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  NextCloudAPI in 'NextCloudAPI.pas',
  ConvertFileSize in 'ConvertFileSize.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
