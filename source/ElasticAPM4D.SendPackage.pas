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

  TSendPackage = class
  private
    FMetadata: TMetadata;
    FTransaction: TTransaction;
    FSpanList: TObjectList<TSpan>;
    FErrorList: TObjectList<TError>;
    FOpenSpanStack: TList;
    FUser: TUser;
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
    function CurrentSpan: TSpan;

    property Metadata: TMetadata read FMetadata write FMetadata;
    property Transaction: TTransaction read FTransaction write FTransaction;
    property SpanList: TObjectList<TSpan> read FSpanList write FSpanList;
    property ErrorList: TObjectList<TError> read FErrorList write FErrorList;
    property OpenSpanStack: TList read FOpenSpanStack write FOpenSpanStack;
    property User: TUser read FUser write FUser;
    property Header: string read GetHeader write SetHeader;
  end;

implementation

Uses
  ElasticAPM4D.SendThread,
  ElasticAPM4D.ndJson,
  ElasticAPM4D.Config,
  ElasticAPM4D.Resources;

{ TSendPackage }

constructor TSendPackage.Create;
begin
  FTransaction := TTransaction.Create;
  FMetadata := TMetadata.Create;
  FSpanList := TObjectList<TSpan>.Create;
  FOpenSpanStack := TList.Create;
  FErrorList := TObjectList<TError>.Create;
  FUser := TUser.Create;
  FHeader := '';
end;

function TSendPackage.CurrentSpan: TSpan;
begin
  if not SpanIsOpen then
    raise EElasticAPM4DException.Create('Current span not found');

  Result := FOpenSpanStack.Items[Pred(FOpenSpanStack.Count)];
end;

destructor TSendPackage.Destroy;
begin
  FTransaction.Free;
  FMetadata.Free;
  FreeAndNil(FSpanList);
  FreeAndNil(FErrorList);
  FOpenSpanStack.Free;
  FUser.Free;
  inherited;
end;

function TSendPackage.SpanIsOpen: Boolean;
begin
  Result := FOpenSpanStack.Count > 0;
end;

function TSendPackage.GetHeader: string;
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

procedure TSendPackage.Send;
var
  LndJson: TndJson;
  LThread: TSendThread;
begin
  if not TConfig.Enabled then
    exit;
  LndJson := TndJson.Create;
  try
    LndJson.Add(FMetadata);
    LndJson.Add(FTransaction);
    LndJson.Add(FSpanList);
    LndJson.Add(FErrorList);

    LThread := TSendThread.Create(TConfig.URL);
    LThread.Send(GetHeader, LndJson.Get);
  finally
    FHeader := '';
    LndJson.Free;
  end;
end;

function TSendPackage.ExtractParentID: string;
begin
  Result := Copy(FHeader, 37, 16);
end;

function TSendPackage.ExtractTraceId: string;
begin
  Result := Copy(FHeader, 4, 32);
end;

procedure TSendPackage.SetHeader(const Value: string);
begin
  FHeader := Value;
  if not FHeader.IsEmpty then
  begin
    FTransaction.trace_id := ExtractTraceId;
    FTransaction.parent_id := ExtractParentID;
  end;
end;

end.
