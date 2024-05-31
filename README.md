# NextCloudAPI
Use THTTPNetClient

### Create
```Pascal
uses NextCloudAPI;

var NextCloud:= TNextCloud.Create;
NextCloud.SetParametrs(BaseURL, Login, Password, IgnorInvalidCertificate);
```

### DownloadFile
```Pascal
var Status: TStatus; 
NextCloud.DownloadFile(PathFrom, PathTo, Status);

NextCloud.DownloadFileWithoutName(PathFrom, PathTo, Status); //если нужно указать имя файла вручную

var FileStream := NextCloud.DownloadFileToStream(PathFrom, Status);
```

### CreatFolder
```Pascal 
NextCloud.CreatFolder(Path, Status);
```

### UploadFile
```Pascal 
NextCloud.UploadFile(PathTo, PathFrom, Status);
```

### DeleteFile
```Pascal 
NextCloud.DeleteFile(Path, Status);
```

### FileList 
```Pascal
var FileList: TFileList;

FileList := NextCloud.FileList:= NextCloud.GetListFolder(Path, Depth, Status);  

for var key in FileList.Keys do
  begin
    if FileList[key].IsFolder then
      s:= FileList[key].FilePath + ' | DIR ' + ' | ' + FileList[key].FileDate
    else
      s:= FileList[key].FilePath + ' | ' + FormatByteString(FileList[key].FileSize.ToInt64) + ' | ' + FileList[key].FileDate;
    writeln(s);
  end;
```
