unit Share.Service;

interface

uses Share.Service.Agent, Share.Service.Framework, Share.Service.Language, Share.Service.Node, Share.Service.Runtime;

type
  // <summary>
  // Service related information can be sent per event. Information provided here will override the more generic
  // information retrieved from metadata, missing service fields will be retrieved from the metadata information.
  // </summary>
  TService = class
  private
    FFramework: TServiceFramework;
    FLanguage: TServiceLanguage;
    FRuntime: TServiceRuntime;
    FAgent: TServiceAgent;
    FVersion: string;
    FEnvironment: string;
    FName: string;
    FNode: TServiceNode;
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

Uses
  SysUtils, ElasticAPM4D.Config;

{ TService }

procedure TService.AddEnvironment(const AName: string);
begin
  FEnvironment := AName;
end;

procedure TService.AddFramework(const AName, AVersion: string);
begin
  FFramework := TServiceFramework.Create(AName, AVersion);
end;

procedure TService.AddServiceNode(const AName: string);
begin
  FNode := TServiceNode.Create(AName);
end;

procedure TService.ChangeServiceInfo(const AName, AVersion: string);
begin
  if not AName.isEmpty then
    FName := AName;
  FVersion := AVersion;
end;

constructor TService.Create;
begin
  FAgent := TServiceAgent.Create;
  FLanguage := TServiceLanguage.Create;
  FVersion := TConfig.GetAppVersion;
  FName := TConfig.GetAppName;
  FEnvironment := 'staging';
end;

destructor TService.Destroy;
begin
  FAgent.Free;
  FLanguage.Free;
  if Assigned(FRuntime) then
    FRuntime.Free;
  if Assigned(FFramework) then
    FFramework.Free;
  if Assigned(FNode) then
    FNode.Free;
  inherited;
end;

end.
