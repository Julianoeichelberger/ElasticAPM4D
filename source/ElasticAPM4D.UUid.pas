unit ElasticAPM4D.UUid;

interface

type
  TElasticAPM4DUUid = class
  private
    class procedure RemoveChars(var AStr: string);
  public
    class function GetUUid64b: string;
    class function GetUUid128b: string;
  end;

implementation

Uses
  System.SysUtils,
  System.Types;

{ TElasticAPM4DUUid }

class function TElasticAPM4DUUid.GetUUid64b: string;
begin
  Result := Copy(GetUUid128b, 1, 16);
end;

class procedure TElasticAPM4DUUid.RemoveChars(var AStr: string);
begin
  AStr := AStr.Replace('-', '', [rfReplaceAll]);
  AStr := AStr.Replace('{', '');
  AStr := AStr.Replace('}', '');
end;

class function TElasticAPM4DUUid.GetUUid128b: string;
var
  Uid: TGuid;
  nResult: HResult;
begin
  Result := '';
  nResult := CreateGuid(Uid);
  if nResult = S_OK then
  begin
    Result := GuidToString(Uid).ToLower;
    RemoveChars(Result);
    Exit;
  end;
end;

end.
