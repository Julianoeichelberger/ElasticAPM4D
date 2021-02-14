unit ElasticAPM4D.Jcl;

interface

Uses
  System.Classes,
  ElasticAPM4D.StacktraceFrame;

type
  TElasticAPM4DStacktraceJCL = class
  private
    function ExtractValue(const AStr, ARegEX: string): string;
    function GetLine(const AStr: string): integer;
    function GetUnitName(const AStr: string): string;
    function GetClassName(const AStr: string): string;
    function GetContextLine(const AStr: string): string;
    function GetStackList: TStringList;
    function IsValidStacktrace(const AStr: string): Boolean;
  public
    class function Get: TArray<TElasticAPM4DStacktrace>;
  end;

implementation

Uses
  JclDebug,
  System.IOUtils,
  System.SysUtils,
  System.RegularExpressions,
  ElasticAPM4D.Resources;

{ TElasticAPM4DStacktraceJCL }

function TElasticAPM4DStacktraceJCL.IsValidStacktrace(const AStr: string): Boolean;
begin
  Result := not ExtractValue(AStr, REGEx_MAKE_PARSE).IsEmpty;
end;

function TElasticAPM4DStacktraceJCL.ExtractValue(const AStr, ARegEX: string): string;
var
  LMath: TMatch;
begin
  LMath := TRegEx.Match(AStr, ARegEX);
  if LMath.Success and (LMath.Groups.Count > 0) then
    Exit(LMath.Groups.Item[Pred(LMath.Groups.Count)].Value);

  Result := '';
end;

function TElasticAPM4DStacktraceJCL.GetClassName(const AStr: string): string;
begin
  Result := ExtractValue(AStr, REGEx_CLASSNAME);
end;

function TElasticAPM4DStacktraceJCL.GetContextLine(const AStr: string): string;
begin
  Result := ExtractValue(AStr, REGEx_CONTEXT_LINE);
  if Pos('(', Result) > 0 then
    Result := Result + ')';
end;

function TElasticAPM4DStacktraceJCL.GetLine(const AStr: string): integer;
begin
  Result := StrToIntDef(ExtractValue(AStr, REGEx_LINE), 0);
end;

function TElasticAPM4DStacktraceJCL.GetStackList: TStringList;
begin
  Result := TStringList.Create;
  JclLastExceptStackListToStrings(Result, True);
end;

function TElasticAPM4DStacktraceJCL.GetUnitName(const AStr: string): string;

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
    Result := ExtractValue(AStr, LIST_REGEx_UNIT_NAME[I]);
    if not Result.IsEmpty then
      Break;
  end;
  if Result.IsEmpty then
    Result := 'unknown';
  FormatExtension(Result);
end;

class function TElasticAPM4DStacktraceJCL.Get: TArray<TElasticAPM4DStacktrace>;
var
  LStack: TStringList;
  LStacktrace: TElasticAPM4DStacktrace;
  LLine: integer;
  LClass: TElasticAPM4DStacktraceJCL;
begin
  LClass := TElasticAPM4DStacktraceJCL.Create;
  LStack := LClass.GetStackList;
  try
    for LLine := 0 to Pred(LStack.Count) do
    begin
      if not LClass.IsValidStacktrace(LStack.Strings[LLine]) then
        Continue;

      LStacktrace := TElasticAPM4DStacktrace.Create;
      LStacktrace.lineno := LClass.GetLine(LStack.Strings[LLine]);
      LStacktrace.module := LClass.GetClassName(LStack.Strings[LLine]);
      LStacktrace.filename := LClass.GetUnitName(LStack.Strings[LLine]);
      LStacktrace.context_line := LClass.GetContextLine(LStack.Strings[LLine]);
      Result := Result + [LStacktrace];
    end;
  finally
    LStack.Free;
    LClass.Free;
  end;
end;

end.
