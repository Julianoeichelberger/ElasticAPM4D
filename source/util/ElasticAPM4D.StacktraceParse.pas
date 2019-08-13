unit ElasticAPM4D.StacktraceParse;

interface

uses
  ElasticAPM4D.StacktraceFrame;

type
  TElasticAPM4DStacktraceParse = class
  strict private
    class function GetValueByRegEX(const AStr, ARegEX: string): string;
    class function GetLine(const AStr: string): integer;
    class function GetUnitName(const AStr: string): string;
    class function GetClassName(const AStr: string): string;
    class function GetContextLine(const AStr: string): string;
  public
    class function CanMakeParse(const AStr: string): Boolean;
    class function Parse(const AStr: string): TElasticAPM4DStacktrace;
  end;

implementation

Uses
  System.IOUtils,
  System.SysUtils,
  RegularExpressions,
  ElasticAPM4D.Resources;

{ TElasticAPM4DStacktraceParse }

class function TElasticAPM4DStacktraceParse.GetValueByRegEX(const AStr: string; const ARegEX: string): string;
var
  LMath: TMatch;
begin
  LMath := TRegEx.Match(AStr, ARegEX);
  if LMath.Success and (LMath.Groups.Count > 0) then
    Exit(LMath.Groups.Item[Pred(LMath.Groups.Count)].Value);

  Result := '';
end;

class function TElasticAPM4DStacktraceParse.GetLine(const AStr: string): integer;
begin
  Result := StrToIntDef(GetValueByRegEX(AStr, REGEx_LINE), 0);
end;

class function TElasticAPM4DStacktraceParse.GetClassName(const AStr: string): string;
begin
  Result := GetValueByRegEX(AStr, REGEx_CLASSNAME);
end;

class function TElasticAPM4DStacktraceParse.GetContextLine(const AStr: string): string;
begin
  Result := GetValueByRegEX(AStr, REGEx_CONTEXT_LINE);
  if Pos('(', Result) > 0 then
    Result := Result + ')';
end;

class function TElasticAPM4DStacktraceParse.GetUnitName(const AStr: string): string;

  procedure FormatExtension(var AStr: string);
  begin
    if TPath.GetFileNameWithoutExtension(AStr) <> AStr then
      AStr := TPath.ChangeExtension(AStr, '.pas');
  end;

var
  I: integer;
begin
  for I := 0 to Pred(Length(LIST_REGEx_UNIT_NAME)) do
  begin
    Result := GetValueByRegEX(AStr, LIST_REGEx_UNIT_NAME[I]);
    if not Result.IsEmpty then
      Break;
  end;
  if Result.IsEmpty then
    Result := 'unknown';
  FormatExtension(Result);
end;

class function TElasticAPM4DStacktraceParse.CanMakeParse(const AStr: string): Boolean;
begin
  Result := not GetValueByRegEX(AStr, REGEx_MAKE_PARSE).IsEmpty;
end;

class function TElasticAPM4DStacktraceParse.Parse(const AStr: string): TElasticAPM4DStacktrace;
begin
  Result := TElasticAPM4DStacktrace.Create;
  Result.lineno := GetLine(AStr);
  Result.module := GetClassName(AStr);
  Result.filename := GetUnitName(AStr);
  Result.context_line := GetContextLine(AStr);
end;

end.
