unit ElasticAPM4D.Metadata;

interface

uses
  ElasticAPM4D.Process, ElasticAPM4D.Service, ElasticAPM4D.User, ElasticAPM4D.System;

type
  TMetadata = class
  private
    FProcess: TProcess;
    FService: TService;
    FSystem: TSystem;
    FUser: TUser;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJsonString: string;

    property Process: TProcess read FProcess;
    property Service: TService read FService;
    property System: TSystem read FSystem;
    property User: TUser read FUser write FUser;
  end;

implementation

Uses
  System.SysUtils, Rest.Json, ElasticAPM4D.Resources;

{ TMetadata }

constructor TMetadata.Create;
begin
  FService := TService.Create;
  FSystem := TSystem.Create;
  FUser := TUser.Create;
  FProcess := TProcess.Create;
end;

destructor TMetadata.Destroy;
begin
  FService.Free;
  FSystem.Free;
  FUser.Free;
  FProcess.Free;
  inherited;
end;

function TMetadata.ToJsonString: string;
begin
  result := format(sMetadataJsonId, [TJson.ObjectToJsonString(self)]);
end;

end.
