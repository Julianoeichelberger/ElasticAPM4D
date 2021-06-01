unit ElasticAPM4D.Service;

interface

type
  // <summary>
  // Agent holds information about the APM agent capturing the event - ElasticAPM4D
  // </summary>
  TAgent = class
  private
    FName: String;
    FVersion: String;
    Fephemeral_id: string;
  public
    constructor Create;

    property Name: String read FName;
    property Version: String read FVersion;
    // <summary>
    // phemeralID is a free format ID used for metrics correlation by agents
    // </summary>
    property Ephemeral_id: string read Fephemeral_id write Fephemeral_id;
  end;

  // <summary>
  // Language holds information about the programming language of the monitored service.
  // </summary>
  TLanguage = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create;

    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  // <summary>
  // Framework holds information about the framework used in the monitored service.
  // </summary>
  TFramework = class
  private
    FName: String;
    FVersion: String;
  public
    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  // <summary>
  // Runtime holds information about the language runtime running the monitored service
  // </summary>
  TRuntime = class
  private
    FName: string;
    FVersion: String;
  public
    constructor Create;

    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  // <summary>
  // Node must be a unique meaningful name of the service node
  // </summary>
  TNode = class
  private
    FConfigured_name: string;
  public
    // <summary>
    // Name of the service node
    // </summary>
    property Configured_name: string read FConfigured_name write FConfigured_name;
  end;

  // <summary>
  // Service related information can be sent per event. Information provided here will override the more generic
  // information retrieved from metadata, missing service fields will be retrieved from the metadata information.
  // </summary>
  TService = class
  private
    FFramework: TFramework;
    FLanguage: TLanguage;
    FRuntime: TRuntime;
    FAgent: TAgent;
    FVersion: String;
    FEnvironment: string;
    FName: String;
    FNode: TNode;
  public
    constructor Create;
    destructor Destroy; override;

    // <summary>
    // name of the service node
    // </summary>
    procedure AddServiceNode(const AName: string);

    // <summary>
    // Environment in which the monitored service is running, e.g. `production` or `staging`.
    // </summary>
    procedure AddEnvironment(const AName: string);

    // <summary>
    // Information about the framework used
    // </summary>
    procedure AddFramework(const AName, AVersion: string);

    // <summary>
    // Name of the monitored service.
    // Version of the monitored service
    // </summary>
    procedure ChangeServiceInfo(const AName, AVersion: string);
  end;

implementation

uses
  System.SysUtils, ElasticAPM4D.Utils;

{ TAgent }

constructor TAgent.Create;
begin
  FVersion := '1.2.0';
  FName := 'ElasticAPM4D';
  Fephemeral_id := '';
end;

{ TLanguage }

constructor TLanguage.Create;
begin
  FName := 'Delphi/Object Pascal';
  FVersion :=
{$IFDEF VER80} 'Delphi 1'; {$ENDIF}
{$IFDEF VER90} 'Delphi 2'; {$ENDIF}
{$IFDEF VER100} 'Delphi 3'; {$ENDIF}
{$IFDEF VER120} 'Delphi 4'; {$ENDIF}
{$IFDEF VER130} 'Delphi 5'; {$ENDIF}
{$IFDEF VER140} 'Delphi 6'; {$ENDIF}
{$IFDEF VER150} 'Delphi 7'; {$ENDIF}
{$IFDEF VER160} 'Delphi 8'; {$ENDIF}
{$IFDEF VER170} 'Delphi 2005'; {$ENDIF}
{$IFDEF VER180} 'Delphi 2006'; {$ENDIF}
{$IFDEF VER185} 'Delphi 2007'; {$ENDIF}
{$IFDEF VER200} 'Delphi 2009'; {$ENDIF}
{$IFDEF VER210} 'Delphi 2010'; {$ENDIF}
{$IFDEF VER220} 'Delphi XE'; {$ENDIF}
{$IFDEF VER230} 'Delphi XE2'; {$ENDIF}
{$IFDEF VER240} 'Delphi XE3'; {$ENDIF}
{$IFDEF VER250} 'Delphi XE4'; {$ENDIF}
{$IFDEF VER260} 'Delphi XE5'; {$ENDIF}
{$IFDEF VER270} 'Delphi XE6'; {$ENDIF}
{$IFDEF VER280} 'Delphi XE7'; {$ENDIF}
{$IFDEF VER290} 'Delphi XE8'; {$ENDIF}
{$IFDEF VER300} 'Delphi 10.0 Seattle'; {$ENDIF}
{$IFDEF VER310} 'Delphi 10.1 Berlin'; {$ENDIF}
{$IFDEF VER320} 'Delphi 10.2 Tokyo'; {$ENDIF}
{$IFDEF VER330} 'Delphi 10.3 Rio'; {$ENDIF}
{$IFDEF VER331} 'Delphi 10.4 Sidney'; {$ENDIF}
end;

{ TRuntime }

constructor TRuntime.Create;
begin
  FName := TConfig.GetAppName;
  Version := TConfig.GetAppVersion;
end;

{ TService }

procedure TService.AddEnvironment(const AName: string);
begin
  FEnvironment := AName;
end;

procedure TService.AddFramework(const AName, AVersion: string);
begin
  FFramework := TFramework.Create;
  FFramework.Name := AName;
  FFramework.Version := AVersion;
end;

procedure TService.AddServiceNode(const AName: string);
begin
  FNode := TNode.Create;
  FNode.Configured_name := AName;
end;

procedure TService.ChangeServiceInfo(const AName, AVersion: string);
begin
  FName := AName;
  FVersion := AVersion;
end;

constructor TService.Create;
begin
  FAgent := TAgent.Create;
  FLanguage := TLanguage.Create;
  FRuntime := TRuntime.Create;
  FVersion := TConfig.GetAppVersion;
  FName := TConfig.GetAppName;
  FEnvironment := 'staging';
end;

destructor TService.Destroy;
begin
  FAgent.Free;
  FLanguage.Free;
  FRuntime.Free;
  if Assigned(FFramework) then
    FFramework.Free;
  if Assigned(FNode) then
    FNode.Free;
  inherited;
end;

end.
