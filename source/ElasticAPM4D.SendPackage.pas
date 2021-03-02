unit ElasticAPM4D.SendPackage;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ElasticAPM4D.Transaction,
  ElasticAPM4D.Metadata,
  ElasticAPM4D.User,
  ElasticAPM4D.Span,
  ElasticAPM4D.Error;

type
  EElasticAPM4DException = Exception;

  TElasticAPM4DSendPackage = class
  private
    FMetadata: TElasticAPM4DMetadata;
    FTransaction: TElasticAPM4DTransaction;
    FSpanList: TObjectList<TElasticAPM4DSpan>;
    FErrorList: TObjectList<TElasticAPM4DError>;
    FOpenSpanStack: TList;
    FUser: TElasticAPM4DUser;
    FHeader: string;
    procedure SetHeader(const Value: string);
    function ExtractTraceId: string;
    function ExtractParentID: string;
    function GetHeader: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Send;
    function SpanIsOpen: Boolean;
    function CurrentSpan: TElasticAPM4DSpan;

    property Metadata: TElasticAPM4DMetadata read FMetadata write FMetadata;
    property Transaction: TElasticAPM4DTransaction read FTransaction write FTransaction;
    property SpanList: TObjectList<TElasticAPM4DSpan> read FSpanList write FSpanList;
    property ErrorList: TObjectList<TElasticAPM4DError> read FErrorList write FErrorList;
    property OpenSpanStack: TList read FOpenSpanStack write FOpenSpanStack;
    property User: TElasticAPM4DUser read FUser write FUser;
    property Header: string read GetHeader write SetHeader;
  end;

implementation

Uses
  ElasticAPM4D.SendThread,
  ElasticAPM4D.ndJson,
  ElasticAPM4D.Config,
  ElasticAPM4D.Resources;

{ TElasticAPM4DSendPackage }

constructor TElasticAPM4DSendPackage.Create;
begin
  FTransaction := TElasticAPM4DTransaction.Create;
  FMetadata := TElasticAPM4DMetadata.Create;
  FSpanList := TObjectList<TElasticAPM4DSpan>.Create;
  FOpenSpanStack := TList.Create;
  FErrorList := TObjectList<TElasticAPM4DError>.Create;
  FUser := TElasticAPM4DUser.Create;
  FHeader := '';
end;

function TElasticAPM4DSendPackage.CurrentSpan: TElasticAPM4DSpan;
begin
  if not SpanIsOpen then
    raise EElasticAPM4DException.Create('Current span not found');

  Result := FOpenSpanStack.Items[Pred(FOpenSpanStack.Count)];
end;

destructor TElasticAPM4DSendPackage.Destroy;
begin
  FTransaction.Free;
  FMetadata.Free;
  FreeAndNil(FSpanList);
  FreeAndNil(FErrorList);
  FOpenSpanStack.Free;
  FUser.Free;
  inherited;
end;

function TElasticAPM4DSendPackage.SpanIsOpen: Boolean;
begin
  Result := FOpenSpanStack.Count > 0;
end;

function TElasticAPM4DSendPackage.GetHeader: string;
begin
  Result := FHeader;
  if Result.IsEmpty then
  begin
    if SpanIsOpen then
      Result := Format(sHEADER, [FTransaction.trace_id, CurrentSpan.id])
    else
      Result := Format(sHEADER, [FTransaction.trace_id, FTransaction.id]);
  end
end;

procedure TElasticAPM4DSendPackage.Send;
var
  LndJson: TElasticAPM4DndJson;
  LThread: TElasticAPM4DSendThread;
begin
  if not TElasticAPM4DConfig.Enabled then
    exit;
  LndJson := TElasticAPM4DndJson.Create;
  try
    LndJson.Add(FMetadata);
    LndJson.Add(FTransaction);
    LndJson.Add(FSpanList);
    LndJson.Add(FErrorList);

    LThread := TElasticAPM4DSendThread.Create(TElasticAPM4DConfig.URL);
    LThread.Send(GetHeader, LndJson.Get);
  finally
    FHeader := '';
    LndJson.Free;
  end;
end;

function TElasticAPM4DSendPackage.ExtractParentID: string;
begin
  Result := Copy(FHeader, 37, 16);
end;

function TElasticAPM4DSendPackage.ExtractTraceId: string;
begin
  Result := Copy(FHeader, 4, 32);
end;

procedure TElasticAPM4DSendPackage.SetHeader(const Value: string);
begin
  FHeader := Value;
  if not FHeader.IsEmpty then
  begin
    FTransaction.trace_id := ExtractTraceId;
    FTransaction.parent_id := ExtractParentID;
  end;
end;

end.
