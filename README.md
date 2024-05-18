# NextCloudAPI
Use THTTPNetClient

### NextCloud.Create
```Pascal
uses NextCloudAPI;

var NextCloud:= TNextCloud.Create;
NextCloud.SetParametrs(BaseURL, Login, Password, IgnorInvalidCertificate);
```

### DownloadFile
```Pascal
var Description: string 
NextCloud.DownloadFile(PathFrom, PathTo, Description);

NextCloud.DownloadFileWithoutName(PathFrom, PathTo, Description); //если нужно указать имя файла вручную

var FileStream := NextCloud.DownloadFileToStream(PathFrom, Description);
```

### CreatFolder
```Pascal 
NextCloud.CreatFolder(Path, Description);
```

### UploadFile
```Pascal 
NextCloud.UploadFile(PathTo, PathFrom, Description);
```

### DeleteFile
```Pascal 
NextCloud.DeleteFile(Path, Description);
```

### FileList 
```Pascal
var FileList: TFileList;

FileList := NextCloud.FileList:= NextCloud.GetListFolder(Path, Depth, Description);  // Depth not release
```
