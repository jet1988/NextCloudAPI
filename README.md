# NextCloudAPI
Use THTTPNetClient

## NextCloud.Create
```Pascal
uses NextCloudAPI;

var NextCloud:= TNextCloud.Create;
NextCloud.SetParametrs(BaseURL, Login, Password, IgnorInvalidCertificate);
```

## DownloadFile
```Pascal
var Description: string 
NextCloud.DownloadFile(PathFrom, PathTo, Description);

NextCloud.DownloadFileWithoutName(PathFrom, PathTo, Description); //если нужно указать имя файла вручную

var FileStream := NextCloud.DownloadFileToStream(PathFrom, Description);
```
