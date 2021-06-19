unit Metadata;

interface

uses
  Metadata.Cloud, Metadata.Process, Metadata.System, Share.Service, Share.User;

type
  TMetadataCloud = Metadata.Cloud.TMetadataCloud;

  TMetadata = class
  private
    FProcess: TMetadataProcess;
    FService: TService;
    FSystem: TSystem;
    FUser: TUser;
    FCloud: TMetadataCloud;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJsonString: string;

    function CreateCloud(const AProvider: string): TMetadataCloud;

    property Service: TService read FService;
    property User: TUser read FUser;
  end;

implementation

uses
  SysUtils, REST.Json;

{ TMetadata }

function TMetadata.CreateCloud(const AProvider: string): TMetadataCloud;
begin
  FCloud := TMetadataCloud.Create(AProvider);
  Result := FCloud;
end;

constructor TMetadata.Create;
begin
  FService := TService.Create;
  FSystem := TSystem.Create;
  FUser := TUser.Create;
  FProcess := TMetadataProcess.Create;
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
  Result := format('{"metadata": %s}', [TJson.ObjectToJsonString(self)]);
end;

end.
