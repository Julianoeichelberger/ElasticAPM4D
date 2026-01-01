{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.Cloud;

interface

uses 
  Apm4D.Metadata.Cloud.Account, Apm4D.Metadata.Cloud.Instance, Apm4D.Metadata.Cloud.Machine, Apm4D.Metadata.Cloud.Project, Apm4D.Metadata.Cloud.Service;

type
  TMetadataCloud = class
  private
    Favailability_zone: string;
    // Provider that is used, e.g. aws, azure, gcp, digitalocean.
    FProvider: string;
    FRegion: string;
    FService: TMetadataCloudService;
    FAccount: TMetadataCloudAccount;
    FProject: TMetadataCloudProject;
    FInstance: TMetadataCloudInstance;
    FMachine: TMetadataCloudMachine;
  public
    constructor Create(const AProvider: string);
    destructor Destroy; override;

    procedure AddService(const AServiceName: string);
    procedure AddProject(const AId, AName: string);
    procedure AddMachine(const AId: string);
    procedure AddInstance(const AId, AName: string);
    procedure AddAccount(const AId, AName: string);

    // AvailabilityZone where the monitored service is running, e.g. us-east-1a
    property Availability_zone: string read Favailability_zone;
    // Region where the monitored service is running, e.g. us-east-1
    property Region: string read FRegion;
  end;

implementation

{ TCloud }

procedure TMetadataCloud.AddAccount(const AId, AName: string);
begin
  FAccount := TMetadataCloudAccount.Create;
  FAccount.id := AId;
  FAccount.name := AName;
end;

procedure TMetadataCloud.AddInstance(const AId, AName: string);
begin
  FInstance := TMetadataCloudInstance.Create;
  FInstance.id := AId;
  FInstance.name := AName;
end;

procedure TMetadataCloud.AddMachine(const AId: string);
begin
  FMachine := TMetadataCloudMachine.Create(AId);
end;

procedure TMetadataCloud.AddProject(const AId, AName: string);
begin
  FProject := TMetadataCloudProject.Create;
  FProject.id := AId;
  FProject.name := AName;
end;

procedure TMetadataCloud.AddService(const AServiceName: string);
begin
  FService := TMetadataCloudService.Create(AServiceName);
end;

constructor TMetadataCloud.Create(const AProvider: string);
begin
  FProvider := AProvider;
end;

destructor TMetadataCloud.Destroy;
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
