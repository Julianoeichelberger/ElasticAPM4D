{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata;

interface

uses
  Apm4D.Metadata.Cloud,
  Apm4D.Metadata.Process,
  Apm4D.Metadata.System,
  Apm4D.Share.Service,
  Apm4D.Share.User,
  Apm4D.Serializer;

type
  TMetadataCloud = Apm4D.Metadata.Cloud.TMetadataCloud;

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

    property Process: TMetadataProcess read FProcess;
    property Service: TService read FService;
    property System: TSystem read FSystem;
    property User: TUser read FUser;
    property Cloud: TMetadataCloud read FCloud;
  end;

implementation

uses
  SysUtils;

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
  Result := TApm4DSerializer.ToJSON(Self, 'metadata');
end;

end.
