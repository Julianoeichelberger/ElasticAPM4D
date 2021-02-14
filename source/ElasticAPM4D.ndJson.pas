unit ElasticAPM4D.ndJson;

interface

uses
  System.classes,
  System.SysUtils,
  System.Generics.Collections,
  ElasticAPM4D.Metadata,
  ElasticAPM4D.Transaction,
  ElasticAPM4D.Span,
  ElasticAPM4D.Error;

type
  TElasticAPM4DndJson = class
  strict private
  const
    sNDJsonSeparator = sLineBreak;
  strict private
    FJson: Widestring;
  public
    procedure Add(ASpans: TList<TElasticAPM4DSpan>); overload;
    procedure Add(AMetadata: TElasticAPM4DMetadata); overload;
    procedure Add(ATransaction: TElasticAPM4DTransaction); overload;
    procedure Add(AErro: TList<TElasticAPM4DError>); overload;

    function Get: Widestring;
  end;

implementation

{ TElasticAPM4DndJson }

procedure TElasticAPM4DndJson.Add(AMetadata: TElasticAPM4DMetadata);
begin
  FJson := AMetadata.ToJsonString;
end;

procedure TElasticAPM4DndJson.Add(ASpans: TList<TElasticAPM4DSpan>);
var
  LSpan: TElasticAPM4DSpan;
begin
  if not Assigned(ASpans) then
    exit;
  for LSpan in ASpans.List do
  begin
    if LSpan <> nil then                  
      FJson := FJson + sNDJsonSeparator + LSpan.ToJsonString;
  end;
end;

procedure TElasticAPM4DndJson.Add(AErro: TList<TElasticAPM4DError>);
var
  LError: TElasticAPM4DError;
begin
  if not Assigned(AErro) then
    exit;
  for LError in AErro.List do
    FJson := FJson + sNDJsonSeparator + LError.ToJsonString;
end;

procedure TElasticAPM4DndJson.Add(ATransaction: TElasticAPM4DTransaction);
begin
  FJson := FJson + sNDJsonSeparator + ATransaction.ToJsonString;
end;

function TElasticAPM4DndJson.Get: Widestring;
begin
  Result := FJson;
end;

end.
