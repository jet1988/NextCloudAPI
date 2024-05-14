unit ConvertFileSize;

interface


Uses System.SysUtils;

const
  OneKB = 1024;
  OneMB = OneKB * OneKB;
  OneGB = OneKB * OneMB;
  OneTB = Int64(OneKB) * OneGB;

type
  TByteStringFormat = (bsfDefault, bsfBytes, bsfKB, bsfMB, bsfGB, bsfTB);

function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;

implementation

function FormatByteString(Bytes: UInt64; Format: TByteStringFormat = bsfDefault): string;
begin
  if Format = bsfDefault then begin
    if Bytes < OneKB then begin
      Format := bsfBytes;
    end
    else if Bytes < OneMB then begin
      Format := bsfKB;
    end
    else if Bytes < OneGB then begin
      Format := bsfMB;
    end
    else if Bytes < OneTB then begin
      Format := bsfGB;
    end
    else begin
      Format := bsfTB;
    end;
  end;

  case Format of
  bsfBytes:
    Result := System.SysUtils.Format('%d bytes', [Bytes]);
  bsfKB:
    Result := System.SysUtils.Format('%.1n KB', [Bytes / OneKB]);
  bsfMB:
    Result := System.SysUtils.Format('%.1n MB', [Bytes / OneMB]);
  bsfGB:
    Result := System.SysUtils.Format('%.1n GB', [Bytes / OneGB]);
  bsfTB:
    Result := System.SysUtils.Format('%.1n TB', [Bytes / OneTB]);
  end;
end;



end.
