unit ElasticAPM4D.Metadata;

interface

uses
  ElasticAPM4D.Service, ElasticAPM4D.User;

type
  // <summary>
  // Cloud metadata about where the monitored service is running
  // </summary>
  TCloud = class
  private type
    TAccount = class
    private
      Fname: string;
      Fid: string;
    public
      property id: string read Fid write Fid;
      property name: string read Fname write Fname;
    end;

    TInstance = class
    private
      Fid: string;
      Fname: string;
    public
      property id: string read Fid write Fid;
      property name: string read Fname write Fname;
    end;

    TMachine = class
    private
      Fid: string;
    public
      property id: string read Fid write Fid;
    end;

    // Project in which the monitored service is running
    TProject = class
    private
      Fid: string;
      Fname: string;
    public
      property id: string read Fid write Fid;
      property name: string read Fname write Fname;
    end;

    // Service that is monitored on cloud
    TService = class
    private
      Fname: string;
    public
      // Name of the cloud service, intended to distinguish services running on different platforms within a provider,
      // eg AWS EC2 vs Lambda, GCP GCE vs App Engine, Azure VM vs App Server
      property name: string read Fname write Fname;
    end;
  private
    Favailability_zone: string;
    FProvider: string;
    FRegion: string;
    FService: TService;
    FAccount: TAccount;
    FProject: TProject;
    FInstance: TInstance;
    FMachine: TMachine;
  public
    constructor Create(const AProvider: string);
    destructor Destroy; override;

    procedure AddService(const AName: string);
    procedure AddProject(const AId, AName: string);
    procedure AddMachine(const AId: string);
    procedure AddInstance(const AId, AName: string);
    procedure AddAccount(const AId, AName: string);

    // AvailabilityZone where the monitored service is running, e.g. us-east-1a
    property Availability_zone: string read Favailability_zone;
    // Provider that is used, e.g. aws, azure, gcp, digitalocean.
    property Provider: string read FProvider;
    // Region where the monitored service is running, e.g. us-east-1
    property Region: string read FRegion;
  end;

  TProcess = class
  private
    FArgv: TArray<String>;
    FPid: Cardinal;
    FPpid: Integer;
    Ftitle: string;
{$IFDEF MSWINDOWS}
    function GetParentProcessId: longint;
    function GetProcessId: longint;
    function GetProcessName: string;
{$ENDIF}
  public
    Constructor Create;

    // <summary>
    // Argv holds the command line arguments used to start this process.
    // </summary>
    property Argv: TArray<String> read FArgv;
    property Pid: Cardinal read FPid;
    property Ppid: Integer read FPpid;
    // <summary>
    // Title is the process title. It can be the same as process name
    // </summary>
    property Title: string read Ftitle;
  end;

  TSystem = class
  private
    FArchitecture: String;
    FHostname: String;
    FPlatform: String;
    function GetHostNameInOS: string;
  public
    constructor Create;

    property Architecture: String read FArchitecture;
    property Hostname: String read FHostname;
    property &Platform: String read FPlatform;
  end;

  TMetadata = class
  private
    FProcess: TProcess;
    FService: TService;
    FSystem: TSystem;
    FUser: TUser;
    FCloud: TCloud;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJsonString: string;

    procedure AddCloudProvider(const AProvider: string);

    property Cloud: TCloud read FCloud;
    property Process: TProcess read FProcess;
    property Service: TService read FService;
    property System: TSystem read FSystem;
    property User: TUser read FUser write FUser;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} TLHelp32, psAPI, Winapi.Windows, Vcl.Forms, {$ENDIF}
{$IFDEF UNIX} Unix, {$ENDIF}
  System.SysUtils, Rest.Json, System.TimeSpan, ElasticAPM4D.Resources;

constructor TProcess.Create;
var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
  Ftitle := GetProcessName;
  FPid := GetProcessId;
  FPpid := GetParentProcessId;
{$ENDIF}
  for I := 1 to Pred(ParamCount) do
    FArgv := FArgv + [
      ParamStr(I)];
end;

{$IFDEF MSWINDOWS}


function TProcess.GetProcessId: longint;
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

function TProcess.GetProcessName: string;
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

function TProcess.GetParentProcessId: longint;
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
{ TSystem }

function TSystem.GetHostNameInOS: string;
{$IFDEF MSWINDOWS}
var
  l: DWORD;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := Unix.GetHostName;
{$ENDIF}
{$IFDEF MSWINDOWS}
  l := 255;
  SetLength(Result, l);
  GetComputerName(PChar(Result), l);
  SetLength(Result, l);
{$ENDIF}
end;

constructor TSystem.Create;
const
  ARQHITECTURE: array [TOSVersion.TArchitecture] of string = ('IntelX86', 'IntelX64', 'ARM32', 'ARM64');
begin
  FArchitecture := ARQHITECTURE[TOSVersion.Architecture];
  FHostname := GetHostNameInOS;
  FPlatform := TOSVersion.ToString;
end;

{ TMetadata }

procedure TMetadata.AddCloudProvider(const AProvider: string);
begin
  FCloud := TCloud.Create(AProvider);
end;

constructor TMetadata.Create;
begin
  FService := TService.Create;
  FSystem := TSystem.Create;
  FUser := TUser.Create;
  FProcess := TProcess.Create;
end;

destructor TMetadata.Destroy;
begin
  if Assigned(FCloud) then
    FCloud.Free;
  FService.Free;
  FSystem.Free;
  FUser.Free;
  FProcess.Free;
  inherited;
end;

function TMetadata.ToJsonString: string;
begin
  Result := format(sMetadataJsonId, [TJson.ObjectToJsonString(self)]);
end;

{ TCloud }

procedure TCloud.AddAccount(const AId, AName: string);
begin
  FAccount := TAccount.Create;
  FAccount.id := AId;
  FAccount.name := AName;
end;

procedure TCloud.AddInstance(const AId, AName: string);
begin
  FInstance := TInstance.Create;
  FInstance.id := AId;
  FInstance.name := AName;
end;

procedure TCloud.AddMachine(const AId: string);
begin
  FMachine := TMachine.Create;
  FMachine.id := AId;
end;

procedure TCloud.AddProject(const AId, AName: string);
begin
  FProject := TProject.Create;
  FProject.id := AId;
  FProject.name := AName;
end;

procedure TCloud.AddService(const AName: string);
begin
  FService := TService.Create;
  FService.name := AName;
end;

constructor TCloud.Create(const AProvider: string);
begin
  FProvider := AProvider;

end;

destructor TCloud.Destroy;
begin
  if Assigned(FService) then
    FService.Free;
  if Assigned(FAccount) then
    FAccount.Free;
  if Assigned(FProject) then
    FProject.Free;
  if Assigned(FInstance) then
    FInstance.Free;
  if Assigned(FMachine) then
    FMachine.Free;
  inherited;
end;

end.
