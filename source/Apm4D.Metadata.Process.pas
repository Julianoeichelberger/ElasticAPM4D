{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Metadata.Process;

interface

type
  // Process Apm4D.Metadata about the monitored service
  TMetadataProcess = class
  private
    // <summary>
    // Argv holds the command line arguments used to start this process.
    // </summary>
    FArgv: TArray<String>;
    FPid: Cardinal;
    FPpid: Integer;
    // Title is the process title. It can be the same as process name
    Ftitle: string;
  public
    Constructor Create;
    property Argv: TArray<String> read FArgv;
    property Pid: Cardinal read FPid;
    property Ppid: Integer read FPpid;
    property Title: String read Ftitle;
  end;

implementation

uses
{$IFDEF MSWINDOWS} Winapi.TLHelp32, Winapi.psAPI, Winapi.Windows, Vcl.Forms, System.SysUtils; {$ENDIF}
{$IFDEF UNIX} Posix.Unistd, System.SysUtils; {$ENDIF}

{ TProcess }

constructor TMetadataProcess.Create;

{$IFDEF MSWINDOWS}
  function GetProcessName: string;
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

  function GetProcessId: longint;
  var
    LHandle: THandle;
    LEntry: TProcessEntry32;
    LExeName: string;
  begin
    Result := GetCurrentProcessId; // Use Windows API directly
    Exit;

    // Fallback method (commented out, using direct API above)
    LExeName := ExtractFileName(Application.ExeName);
    try
      LHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
      LEntry.dwSize := SizeOf(TProcessEntry32);
      if Process32First(LHandle, LEntry) then
      begin
        if LEntry.szExeFile = LExeName then
          Exit(LEntry.th32ProcessID);

        while Process32Next(LHandle, LEntry) do
          if LEntry.szExeFile = LExeName then
            Exit(LEntry.th32ProcessID);
      end;
      CloseHandle(LHandle);
    except
      // ignore
    end;
  end;

  function GetParentProcessId: longint;
  var
    Snapshot: THandle;
    Entry: TProcessEntry32;
    NotFound: Boolean;
  begin
    Result := 0;

    Snapshot := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
    if Snapshot <> 0 then
    begin
      FillChar(Entry, SizeOf(Entry), 0);
      Entry.dwSize := SizeOf(Entry);
      NotFound := Process32First(Snapshot, Entry);
      while NotFound do
      begin
        if Entry.th32ProcessID = FPid then
        begin
          Result := Entry.th32ParentProcessID;
          Break;
        end;
        NotFound := Process32Next(Snapshot, Entry);
      end;
      CloseHandle(Snapshot);
    end;
  end;

{$ENDIF}

{$IFDEF UNIX}
  function GetProcessName: string;
  begin
    Result := ExtractFileName(ParamStr(0));
  end;

  function GetProcessId: longint;
  begin
    Result := getpid;
  end;

  function GetParentProcessId: longint;
  begin
    Result := getppid;
  end;
{$ENDIF}


var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
  Ftitle := GetProcessName;
  FPid := GetProcessId;
  FPpid := GetParentProcessId;
{$ENDIF}
{$IFDEF UNIX}
  Ftitle := GetProcessName;
  FPid := GetProcessId;
  FPpid := GetParentProcessId;
{$ENDIF}
  for I := 1 to Pred(ParamCount) do
    FArgv := FArgv + [ParamStr(I)];
end;

end.
