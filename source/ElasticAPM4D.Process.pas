unit ElasticAPM4D.Process;

interface

type
  TElasticAPM4DProcess = class
  private
    FArgv: TArray<String>;
    FPid: Cardinal;
    FPpid: Integer;
    Ftitle: string;
    function GetParentProcessId: longint;
    function GetProcessId: longint;
    function GetProcessName: string;
  public
    Constructor Create;

    property Argv: TArray<String> read FArgv write FArgv;
    property Pid: Cardinal read FPid;
    property Ppid: Integer read FPpid;
    property Title: string read Ftitle;
  end;

implementation

Uses
  TLHelp32,
  psAPI,
  Winapi.Windows,
  System.SysUtils,
  Vcl.Forms;

constructor TElasticAPM4DProcess.Create;
begin
  Ftitle := GetProcessName;
  FPid := GetProcessId;
  FPpid := GetParentProcessId;
end;

function TElasticAPM4DProcess.GetProcessId: longint;
var
  LHandle: THandle;
  LEntry: TProcessEntry32;
begin
  Result := 0;
  try
    LHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
    LEntry.dwSize := SizeOf(TProcessEntry32);
    if Process32First(LHandle, LEntry) and (LEntry.szExeFile = Application.ExeName) then
      Exit(LEntry.th32ProcessID);

    while Process32Next(LHandle, LEntry) do
      if LEntry.szExeFile = Application.ExeName then
        Exit(LEntry.th32ProcessID);
  except
    Exit;
  end;
end;

function TElasticAPM4DProcess.GetProcessName: string;
var
  LProcess: THandle;
  LModName: Array [0 .. MAX_PATH + 1] of Char;
begin
  Result := Application.Title;
  LProcess := OpenProcess(PROCESS_ALL_ACCESS, False, FPid);
  try
    if LProcess <> 0 then
      if GetModuleFileName(LProcess, LModName, SizeOf(LModName)) <> 0 then
        Result := LModName;
  finally
    CloseHandle(LProcess);
  end;
end;

function TElasticAPM4DProcess.GetParentProcessId: longint;
var
  LSnapshot: THandle;
  LEntry: TProcessEntry32;
  LNotFound: Boolean;
begin
  Result := 0;

  LSnapshot := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
  if LSnapshot <> 0 then
  begin
    FillChar(LEntry, SizeOf(LEntry), 0);
    LEntry.dwSize := SizeOf(LEntry);
    LNotFound := Process32First(LSnapshot, LEntry);
    while LNotFound do
    begin
      if LEntry.th32ProcessID = FPid then
      begin
        Result := LEntry.th32ParentProcessID;
        Break;
      end;
      LNotFound := Process32Next(LSnapshot, LEntry);
    end;
    CloseHandle(LSnapshot);
  end;
end;

end.
