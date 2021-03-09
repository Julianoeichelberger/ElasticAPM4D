unit ElasticAPM4D.Service;

interface

type
  TAgent = class
  private
    FName: String;
    FVersion: String;
    Fephemeral_id: string;
  public
    constructor Create;

    property Name: String read FName;
    property Version: String read FVersion;
    property Ephemeral_id: string read Fephemeral_id write Fephemeral_id;
  end;

  TLanguage = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create;

    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  TFramework = class
  private
    FName: String;
    FVersion: String;
  public
    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  TRuntime = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create;

    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  TService = class
  private
    FFramework: TFramework;
    FLanguage: TLanguage;
    FRuntime: TRuntime;
    FAgent: TAgent;
    FVersion: String;
    FEnvironment: string;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: String read FName;
    property Version: string read FVersion write FVersion;
    property Agent: TAgent read FAgent;
    property Environment: string read FEnvironment write FEnvironment;
    property Framework: TFramework read FFramework;
    property Language: TLanguage read FLanguage;
    property Runtime: TRuntime read FRuntime;
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

constructor TService.Create;
begin
  FAgent := TAgent.Create;
  FLanguage := TLanguage.Create;
  FRuntime := TRuntime.Create;
  FFramework := TFramework.Create;
  FVersion := TConfig.GetAppVersion;
  FName := TConfig.GetAppName;
end;

destructor TService.Destroy;
begin
  FAgent.Free;
  FLanguage.Free;
  FRuntime.Free;
  FFramework.Free;
  inherited;
end;

end.
