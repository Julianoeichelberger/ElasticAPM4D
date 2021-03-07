unit ElasticAPM4D.StackTraceJCL;

interface

Uses
  System.Classes,
  ElasticAPM4D.Stacktrace;

type
  TStacktraceJCL = class
  private
    function ExtractValue(const AStr, ARegEX: string): string;
    function GetLine(const AStr: string): Integer;
    function GetUnitName(const AStr: string): string;
    function GetClassName(const AStr: string): string;
    function GetContextLine(const AStr: string): string;
    function GetStackList: TStringList;
    function IsValidStacktrace(const AStr: string): Boolean;
  public
    class function Get: TArray<TStacktrace>;
  end;

implementation

Uses
{$IFDEF MSWINDOWS}
{$IFDEF jcl}
  JclDebug,
{$ENDIF}
{$ENDIF} System.IOUtils, System.SysUtils, System.RegularExpressions, ElasticAPM4D.Resources;

{ TStacktraceJCL }

function TStacktraceJCL.IsValidStacktrace(const AStr: string): Boolean;
begin
  Result := not ExtractValue(AStr, REGEx_MAKE_PARSE).IsEmpty;
end;

function TStacktraceJCL.ExtractValue(const AStr, ARegEX: string): string;
var
  LMath: TMatch;
begin
  LMath := TRegEx.Match(AStr, ARegEX);
  if LMath.Success and (LMath.Groups.Count > 0) then
    Exit(LMath.Groups.Item[Pred(LMath.Groups.Count)].Value);

  Result := '';
end;

function TStacktraceJCL.GetClassName(const AStr: string): string;
begin
  Result := ExtractValue(AStr, REGEx_CLASSNAME);
end;

function TStacktraceJCL.GetContextLine(const AStr: string): string;
begin
  Result := ExtractValue(AStr, REGEx_CONTEXT_LINE);
  if Pos('(', Result) > 0 then
    Result := Result + ')';
end;

function TStacktraceJCL.GetLine(const AStr: string): Integer;
begin
  Result := StrToIntDef(ExtractValue(AStr, REGEx_LINE), 0);
end;

function TStacktraceJCL.GetStackList: TStringList;
begin
  Result := TStringList.Create;
{$IFDEF MSWINDOWS}
{$IFDEF jcl}
  JclLastExceptStackListToStrings(Result, True);
{$ENDIF}
{$ENDIF}
end;

function TStacktraceJCL.GetUnitName(const AStr: string): string;

  procedure FormatExtension(var AStr: string);
  begin
    if TPath.GetFileNameWithoutExtension(AStr) <> AStr then
      AStr := TPath.ChangeExtension(AStr, '.pas');
  end;

var
  I: Integer;
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

class function TStacktraceJCL.Get: TArray<TStacktrace>;
var
  StackList: TStringList;
  Stacktrace: TStacktrace;
  Line: Integer;
  JclStackTrace: TStacktraceJCL;
begin
  JclStackTrace := TStacktraceJCL.Create;
  StackList := JclStackTrace.GetStackList;
  try
    for Line := 0 to Pred(StackList.Count) do
    begin
      if not JclStackTrace.IsValidStacktrace(StackList.Strings[Line]) then
        Continue;

      Stacktrace := TStacktrace.Create;
      Stacktrace.lineno := JclStackTrace.GetLine(StackList.Strings[Line]);
      Stacktrace.module := JclStackTrace.GetClassName(StackList.Strings[Line]);
      Stacktrace.filename := JclStackTrace.GetUnitName(StackList.Strings[Line]);
      Stacktrace.context_line := JclStackTrace.GetContextLine(StackList.Strings[Line]);
      Result := Result + [Stacktrace];
    end;
  finally
    StackList.Free;
    JclStackTrace.Free;
  end;
end;

end.
