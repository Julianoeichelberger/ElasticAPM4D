unit ElasticAPM4D.Metadata;

interface

uses
  ElasticAPM4D.Process,
  ElasticAPM4D.Service,
  ElasticAPM4D.User,
  ElasticAPM4D.System;

type
  TElasticAPM4DMetadata = class
  private
    FProcess: TElasticAPM4DProcess;
    FService: TElasticAPM4DService;
    FSystem: TElasticAPM4DSystem;
    FUser: TElasticAPM4DUser;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJsonString: string;

    property Process: TElasticAPM4DProcess read FProcess;
    property Service: TElasticAPM4DService read FService;
    property System: TElasticAPM4DSystem read FSystem;
    property User: TElasticAPM4DUser read FUser write FUser;
  end;

implementation

Uses
  SysUtils,
  Rest.Json;

{ TElasticAPM4DMetadata }

constructor TElasticAPM4DMetadata.Create;
begin
  FService := TElasticAPM4DService.Create;
  FSystem := TElasticAPM4DSystem.Create;
  FUser := TElasticAPM4DUser.Create;
  FProcess := TElasticAPM4DProcess.Create;
end;

destructor TElasticAPM4DMetadata.Destroy;
begin
  FService.Free;
  FSystem.Free;
  FUser.Free;
  FProcess.Free;
  inherited;
end;

function TElasticAPM4DMetadata.ToJsonString: string;
begin
  result := format('{"metadata": %s}', [TJson.ObjectToJsonString(self)]);
end;

end.
