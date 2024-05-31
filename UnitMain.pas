unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, System.IniFiles;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses NextCloudAPI, ConvertFileSize;

const config = './config.ini';
var NextCloud: TNextCloud;

function Max(const A, B: word): word;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var Dir: string;
    Status: TStatus;
begin
  //if SaveDialog1.Execute then
  if SelectDirectory('Выбор директории, в которой создадутся каталони.', ExtractFileDrive(Dir), Dir) then
  begin
    NextCloud.DownloadFile(Edit4.Text, Dir{SaveDialog1.FileName}, Status);
    Memo1.Lines.Add(Status.Code.ToString +': '+ Status.Text);
  end;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  NextCloud:= TNextCloud.Create;
  NextCloud.SetParametrs(Edit3.Text, Edit1.Text, Edit2.Text, CheckBox1.IsChecked);
end;

procedure TFormMain.Button3Click(Sender: TObject);
var Status: TStatus;
begin
  NextCloud.CreatFolder(Edit4.Text, Status);
  Memo1.Lines.Add(Status.Code.ToString +': '+ Status.Text);
end;

procedure TFormMain.Button4Click(Sender: TObject);
var Status: TStatus;
begin
  if OpenDialog1.Execute then
  begin
    NextCloud.UploadFile(Edit4.Text, OpenDialog1.FileName, Status);
    Memo1.Lines.Add(Status.Code.ToString +': '+ Status.Text);
  end;
end;

procedure TFormMain.Button5Click(Sender: TObject);
var Status: TStatus;
begin
  NextCloud.DeleteFile(Edit4.Text, Status);
  Memo1.Lines.Add(Status.Code.ToString +': '+ Status.Text);
end;

procedure TFormMain.Button6Click(Sender: TObject);
var s: String;
    FileList: TFileList;
    c1, c2: word;
    Status: TStatus;
begin
  try
    FileList:= NextCloud.GetListFolder(Edit4.Text, CheckBox2.IsChecked, Status);
  finally
    Memo1.Lines.Add(Status.Code.ToString +': '+ Status.Text);
  end;
  if Status.Code<>207 then Exit;


  //    !!!!!!!!!!     БЕЗ ВЫРАВНИВАНИЯ. оно и не нужно обычно.        !!!!!!!
   { for var key in FileList.Keys do
      begin
      if FileList[key].IsFolder then
        s:= FileList[key].FilePath + ' | DIR ' + ' | ' + FileList[key].FileDate
      else
        s:= FileList[key].FilePath + ' | ' + FormatByteString(FileList[key].FileSize.ToInt64) + ' | ' + FileList[key].FileDate;
      Memo1.Lines.Add(s);
      end; }

  //    !!!!!!!!!!     С ВЫРАВНИВАНИЕМ.          !!!!!!!!
  c1:=2; c2:=2;
  for var key in FileList.Keys do
    begin
      c1:= max(FileList[key].FilePath.Length, c1);
      if FileList[key].IsFolder then
        begin
          FileList[key].FileSize:= 'DIR';
          c2:= max(3, c2);
        end
      else
        begin
          FileList[key].FileSize:= FormatByteString(FileList[key].FileSize.ToInt64);
          c2:= max(FileList[key].FileSize.Length, c2);
        end;
    end;
    Memo1.Lines.Add('FileName'.PadRight(c1, ' ') + ' | ' + 'Size'.PadRight(c2, ' ') + ' | ' + 'Date');
    Memo1.Lines.Add('--'.PadRight(c1, '-') + '-|-' + '--'.PadRight(c2, '-') + '-|-' + '--'.PadRight(29, '-'));
    for var key in FileList.Keys do
      begin
      s:= FileList[key].FilePath.PadRight(c1, ' ') + ' | ' + FileList[key].FileSize.PadRight(c2, ' ') + ' | ' + FileList[key].FileDate;
      Memo1.Lines.Add(s);
      end;

end;

procedure TFormMain.Button7Click(Sender: TObject);
var ini: TIniFile;
begin
  ini:= TIniFile.Create(config);
  try
    Edit3.Text:= ini.ReadString('dav', 'server', '');
    Edit1.Text:= ini.ReadString('dav', 'login', '');
    Edit2.Text:= ini.ReadString('dav', 'password', '');
    CheckBox1.IsChecked:= ini.ReadBool('dav', 'ignorCert', false);

    Edit4.Text:= ini.ReadString('comand', 'path', '');
  finally
    ini.Free;
  end;
end;

procedure TFormMain.Button8Click(Sender: TObject);
var ini: TIniFile;
begin
  ini:= TIniFile.Create(config);
  try
    ini.WriteString('dav', 'server', Edit3.Text);
    ini.WriteString('dav', 'login', Edit1.Text);
    ini.WriteString('dav', 'password', Edit2.Text);
    ini.WriteBool('dav', 'ignorCert', CheckBox1.IsChecked);

    ini.WriteString('comand', 'path', Edit4.Text);
  finally
    ini.Free;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NextCloud.Free;

end;

end.
