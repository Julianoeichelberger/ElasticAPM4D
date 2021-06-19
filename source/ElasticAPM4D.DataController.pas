unit ElasticAPM4D.DataController;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Metadata, Span, Error, Transaction;

type
  TDataController = class
  private type
    TXndJson = class
    strict private
    const
      XndJsonSeparator = sLineBreak;
    strict private
      FValue: Widestring;
    public
      procedure Add(ASpans: TList<TSpan>); overload;
      procedure Add(AMetadata: TMetadata); overload;
      procedure Add(ATransaction: TTransaction); overload;
      procedure Add(AErrors: TList<TError>); overload;
      procedure Add(AMetricSet: string); overload;

      function Value: Widestring;
    end;
  private
    FMetadata: TMetadata;
    FTransaction: TTransaction;
    FSpanList: TObjectList<TSpan>;
    FErrorList: TObjectList<TError>;
    FOpenSpanStack: TList;
    FHeader: string;
    function ExtractTraceId: string;
    function ExtractParentID: string;
    function GetHeader: string;
    procedure SetHeader(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ToQueue;

    function StartSpan(const AName, AType: string): TSpan;
    function SpanIsOpened: Boolean;
    function CurrentSpan: TSpan;
    procedure EndSpan;

    property Metadata: TMetadata read FMetadata;
    property Transaction: TTransaction read FTransaction write FTransaction;
    property SpanList: TObjectList<TSpan> read FSpanList;
    property ErrorList: TObjectList<TError> read FErrorList;
    property OpenSpanStack: TList read FOpenSpanStack write FOpenSpanStack;
    property Header: string read GetHeader write SetHeader;
  end;

implementation


uses ElasticAPM4D.Config, ElasticAPM4D.QueueSingleton, Metricset, Metricset.Defaults;

{ TController.TXndJson }

procedure TDataController.TXndJson.Add(ASpans: TList<TSpan>);
var
  Span: TSpan;
begin
  if not Assigned(ASpans) then
    exit;
  for Span in ASpans.List do
    if Span <> nil then
      FValue := FValue + XndJsonSeparator + Span.ToJsonString;
end;

procedure TDataController.TXndJson.Add(AMetadata: TMetadata);
begin
  FValue := AMetadata.ToJsonString;
end;

procedure TDataController.TXndJson.Add(AErrors: TList<TError>);
var
  Error: TError;
begin
  if not Assigned(AErrors) then
    exit;
  for Error in AErrors.List do
    if Error <> nil then
      FValue := FValue + XndJsonSeparator + Error.ToJsonString;
end;

procedure TDataController.TXndJson.Add(ATransaction: TTransaction);
begin
  FValue := FValue + XndJsonSeparator + ATransaction.ToJsonString;
end;

procedure TDataController.TXndJson.Add(AMetricSet: string);
begin
  if AMetricSet.IsEmpty then
    exit;
  FValue := FValue + XndJsonSeparator + AMetricSet;
end;

function TDataController.TXndJson.Value: Widestring;
begin
  Result := FValue;
end;

constructor TDataController.Create;
begin
  FMetadata := TMetadata.Create;
  FTransaction := TTransaction.Create;
  FSpanList := TObjectList<TSpan>.Create;
  FOpenSpanStack := TList.Create;
  FErrorList := TObjectList<TError>.Create;
  FHeader := '';
end;

function TDataController.CurrentSpan: TSpan;
begin
  if not SpanIsOpened then
    exit(nil);

  Result := FOpenSpanStack.Items[Pred(FOpenSpanStack.Count)];
end;

destructor TDataController.Destroy;
begin
  FTransaction.Free;
  FMetadata.Free;
  FreeAndNil(FSpanList);
  FreeAndNil(FErrorList);
  FOpenSpanStack.Free;
  inherited;
end;

function TDataController.SpanIsOpened: Boolean;
begin
  Result := FOpenSpanStack.Count > 0;
end;

function TDataController.StartSpan(const AName, AType: string): TSpan;
begin
  if SpanIsOpened then
    Result := TSpan.Create(CurrentSpan.trace_id, CurrentSpan.transaction_id, CurrentSpan.id)
  else
    Result := TSpan.Create(Transaction.trace_id, Transaction.id, Transaction.id);
  Result.Start(AName, AType);

  SpanList.Add(Result);
  OpenSpanStack.Add(Result);
  Transaction.span_count.Inc;
end;

function TDataController.GetHeader: string;
begin
  Result := FHeader;
  if Result.IsEmpty then
  begin
    if SpanIsOpened then
      Result := Format('00-%s-%s-01', [FTransaction.trace_id, CurrentSpan.id])
    else
      Result := Format('00-%s-%s-01', [FTransaction.trace_id, FTransaction.id]);
  end
end;

procedure TDataController.ToQueue;
var
  Json: TXndJson;
begin
  if not TConfig.GetActive then
    exit;

  Json := TXndJson.Create;
  try
    Json.Add(FMetadata);
    Json.Add(FTransaction);
    Json.Add(FSpanList);
    Json.Add(FErrorList);
    Json.Add(TMetricSetDefaults.Get);

    TQueueSingleton.StackUp(Json.Value, Header);
  finally
    FHeader := '';
    Json.Free;
  end;
end;

procedure TDataController.EndSpan;
begin
  CurrentSpan.ToEnd;
  OpenSpanStack.Delete(Pred(OpenSpanStack.Count));
  Transaction.span_count.Dec;
end;

function TDataController.ExtractParentID: string;
begin
  Result := Copy(FHeader, 37, 16);
end;

function TDataController.ExtractTraceId: string;
begin
  Result := Copy(FHeader, 4, 32);
end;

procedure TDataController.SetHeader(const Value: string);
begin
  FHeader := Value;
  if not FHeader.IsEmpty then
  begin
    FTransaction.trace_id := ExtractTraceId;
    FTransaction.parent_id := ExtractParentID;
  end;
end;

end.
