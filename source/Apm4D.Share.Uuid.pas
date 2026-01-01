{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Uuid;

interface

type
  TUUid = class
  private
    class procedure RemoveChars(var AStr: string);
  public
    class function Get64b: string;
    class function Get128b: string;
  end;

implementation

uses 
  System.SysUtils;

{ TUUid }

class function TUUid.Get64b: string;
begin
  Result := Copy(Get128b, 1, 16);
end;

class procedure TUUid.RemoveChars(var AStr: string);
begin
  AStr := AStr.Replace('-', '', [rfReplaceAll]);
  AStr := AStr.Replace('{', '');
  AStr := AStr.Replace('}', '');
end;

class function TUUid.Get128b: string;
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
