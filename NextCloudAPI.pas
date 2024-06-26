﻿unit NextCloudAPI;

interface

uses System.Classes, System.SysUtils, System.Net.HttpClient, System.IOUtils,
     System.Net.HttpClientComponent, System.NetEncoding, System.Net.URLClient,
     Xml.XMLDoc, Xml.XMLIntf, System.Generics.Collections, FMX.Forms;
Type
  TFileInfo = class
    FilePath, FileName, FileSize, FileDate: String;
    IsFolder: Boolean;
  end;

  TFileList = TDictionary<String, TFileInfo>;

  TStatus = record
    Code: Integer;
    Text: string;
  end;

  TNextCloud = Class
  private
    FHTTPClient: TNetHTTPClient;
    FBaseURL: string;
    FLogin: string;
    FPassword: string;
    FIgnorInvalidCertificate: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property BaseURL: string read FBaseURL write FBaseURL;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property IgnorInvalidCertificate: boolean read FIgnorInvalidCertificate write FIgnorInvalidCertificate;
    procedure SetParametrs(const BaseURL, Login, Password: string; IgnorInvalidCertificate: boolean = False);
    procedure NetHTTPClientValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest;
      const Certificate: TCertificate; var Accepted: Boolean);
    function GetListFolder(Path: string; Depth: Boolean; var Status: TStatus): TFileList;
    function DownloadFileToStream(PathFrom: string; var Status: TStatus): TStream;
    function DownloadFile(PathFrom, PathTo: string; var Status: TStatus): boolean;
    function DownloadFileWithoutName(PathFrom, PathTo: string; var Status: TStatus): boolean;
    function CreatFolder(Path: string; var Status: TStatus): boolean;
    function UploadFile(PathTo, PathFrom: string; var Status: TStatus): boolean;
    function DeleteFile(Path: string; var Status: TStatus): boolean;
  End;

implementation


{ TNextCloud }

constructor TNextCloud.Create;
begin
  inherited;
  FHTTPClient:= TNetHTTPClient.Create(nil);
  //FHTTPClient.AcceptCharSet:='utf-8';
end;

function TNextCloud.CreatFolder(Path: string; var Status: TStatus): boolean;
var Response: IHTTPResponse;
begin
  try
    Response:=FHTTPClient.Execute('MKCOL', TURI.Create(FBaseURL + Path));
    Result := Response.StatusCode=201;
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;
  except
    Result := False;
  end;
end;

function TNextCloud.DeleteFile(Path: string; var Status: TStatus): boolean;
var Response: IHTTPResponse;
begin
  try
    Response:=FHTTPClient.Delete(FBaseURL + Path);
    Result := Response.StatusCode=204;
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;
  except
    Result := False;
  end;
end;

destructor TNextCloud.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TNextCloud.DownloadFileToStream(PathFrom: string; var Status: TStatus): TStream;
var Response: IHTTPResponse;
begin
  Result := nil;
  try
    Response:= FHTTPClient.Get(FBaseURL + PathFrom);
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;

    TCustomMemoryStream(Response.ContentStream).SaveToStream(Result);
  except
    Result := nil;
  end;
end;

function TNextCloud.DownloadFile(PathFrom, PathTo: string; var Status: TStatus): boolean;
var Response: IHTTPResponse;
    Caption: string;
begin
  try
    Response:= FHTTPClient.Get(FBaseURL + PathFrom);
    Result := Response.StatusCode=200;
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;
    Caption:= TNetEncoding.URL.Decode(Response.HeaderValue['Content-Disposition']);
    var p:= pos('filename="', Caption);
    if p>0 then
      begin
      delete(Caption,1,p+9);
      delete(Caption,Caption.Length,1);
      end
    else
      begin
        p:= LastDelimiter('/', PathFrom);
        Caption:=TNetEncoding.URL.Decode(Copy(PathFrom, p + 1, Length(PathFrom) - (p)));
      end;

    TCustomMemoryStream(Response.ContentStream).SaveToFile(TPath.Combine(PathTo, Caption));
  except
      Result := False;
  end;
end;

function TNextCloud.DownloadFileWithoutName(PathFrom, PathTo: string; var Status: TStatus): boolean;
var Response: IHTTPResponse;
    FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(PathTo, fmCreate);
  try
    try
      Response:= FHTTPClient.Get(FBaseURL + PathFrom, FileStream);
      Result := Response.StatusCode=200;
      Status.Code:= Response.StatusCode;
      Status.Text:= Response.StatusText;
    except
      Result := False;
    end;
  finally
    FileStream.Free;
  end;
end;

function TNextCloud.GetListFolder(Path: string; Depth: Boolean; var Status: TStatus): TFileList;
var Response: IHTTPResponse;
    XMLFile: TXMLDocument;
    MainNode, ChildNode: IXMLNode;
    FileGet: TFileInfo;
    FileDB: TFileList;
begin
  result:=nil;
  if Depth then
    FHTTPClient.CustomHeaders['Depth'] := '0';
  try
    Response:=FHTTPClient.Execute('PROPFIND', TURI.Create(FBaseURL + Path));
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;

  except
    on E : Exception do
      begin
        Status.Code:= -1;
        Status.Text:= 'ERROR: ' + E.Message;
      end;
  end;

  if Response.StatusCode=207 then
    begin
      FileDB:=TFileList.Create;
      XMLFile := TXMLDocument.Create(Application);
      XMLFile.LoadFromStream(Response.ContentStream);
      try
        XMLFile.Active := True;
        MainNode := XMLFile.DocumentElement;
        FileDB.Clear;

      for var I := 0 to MainNode.ChildNodes.Count - 1 do
        begin
        ChildNode := MainNode.ChildNodes.Nodes[I];

        if Trim(ChildNode.ChildNodes['d:href'].Text) = ''  then
          begin
          continue;
          end;

        FileGet := TFileInfo.Create;
        FileGet.FilePath := TNetEncoding.URL.Decode(ChildNode.ChildNodes['d:href'].Text);
        FileGet.FileDate := ChildNode.ChildNodes['d:propstat'].ChildNodes['d:prop'].ChildNodes['d:getlastmodified'].Text;
        FileGet.FileSize := ChildNode.ChildNodes['d:propstat'].ChildNodes['d:prop'].ChildNodes['d:getcontentlength'].Text;
        FileGet.IsFolder := Assigned(ChildNode.ChildNodes['d:propstat'].ChildNodes['d:prop'].ChildNodes['d:resourcetype'].ChildNodes.FindNode('d:collection'));

        if FileGet.IsFolder then
          begin
          FileGet.FileName := FileGet.FilePath;
          Delete(FileGet.FileName, Length(FileGet.FileName), 1);
          FileGet.FileName := StringReplace(FileGet.FileName, '/', '\', [rfReplaceAll, rfIgnoreCase]);
          FileGet.FileName := ExtractFileName(FileGet.FileName);
          end
        else
          begin
          FileGet.FileName := StringReplace(FileGet.FilePath, '/', '\', [rfReplaceAll, rfIgnoreCase]);
          FileGet.FileName := ExtractFileName(FileGet.FileName);
          end;

        FileDB.Add(FileGet.FilePath, FileGet);
        end;
      finally;
        FHTTPClient.CustHeaders.Delete('Depth');
        Result:= FileDB;
        XMLFile.Free;
      end;

    end;
end;

function TNextCloud.UploadFile(PathTo, PathFrom: string; var Status: TStatus): boolean;
var Response: IHTTPResponse;
    FileName: string;
begin
  try
    FileName:= TPath.GetFileName(PathFrom);
    Response:= FHTTPClient.Put(FBaseURL + PathTo + '/' + FileName, PathFrom);
    Result := Response.StatusCode=201;
    Status.Code:= Response.StatusCode;
    Status.Text:= Response.StatusText;
  except
    Result := False;
  end;

end;

procedure TNextCloud.SetParametrs(const BaseURL, Login, Password: string; IgnorInvalidCertificate: boolean = False);
begin
  FBaseURL:= BaseURL;
  FLogin:= Login;
  FPassword:= Password;
  FHTTPClient.CustomHeaders['Authorization'] := 'Basic ' + TNetEncoding.Base64.Encode(FLogin + ':' + FPassword);
  if IgnorInvalidCertificate Then
    FHTTPClient.OnValidateServerCertificate:=NetHTTPClientValidateServerCertificate;
end;

procedure TNextCloud.NetHTTPClientValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest;
   const Certificate: TCertificate; var Accepted: Boolean);
begin
Accepted:=True;
end;

end.
