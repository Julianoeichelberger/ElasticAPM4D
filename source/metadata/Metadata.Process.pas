unit Metadata.Process;

interface

type
  // Process metadata about the monitored service
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
  end;

implementation

Uses
{$IFDEF MSWINDOWS} TLHelp32, psAPI, Winapi.Windows, Vcl.Forms; {$ENDIF}

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


var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
  Ftitle := GetProcessName;
  FPid := GetProcessId;
  FPpid := GetParentProcessId;
{$ENDIF}
  for I := 1 to Pred(ParamCount) do
    FArgv := FArgv + [ParamStr(I)];
end;

end.
