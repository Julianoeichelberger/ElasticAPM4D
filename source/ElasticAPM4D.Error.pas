unit ElasticAPM4D.Error;

interface

uses
  System.SysUtils,
  ElasticAPM4D.Context,
  ElasticAPM4D.StacktraceFrame,
  ElasticAPM4D.Transaction,
  ElasticAPM4D.Span;

type
  TElasticAPM4DErrorException = class
  private
    FCode: String;
    FHandled: Boolean;
    FMessage: String;
    FModule: String;
    FStacktrace: TArray<TElasticAPM4DStacktrace>;
    FType: String;
    Fattributes: TObject;
    Fparent: Integer;
    Fcause: TArray<TObject>;
  public
    destructor Destroy; override;

    property attributes: TObject read Fattributes write Fattributes;
    property cause: TArray<TObject> read Fcause write Fcause;
    property code: String read FCode write FCode;
    property handled: Boolean read FHandled write FHandled;
    property &message: String read FMessage write FMessage;
    property module: String read FModule write FModule;
    property parent: Integer read Fparent write Fparent;
    property Stacktrace: TArray<TElasticAPM4DStacktrace> read FStacktrace write FStacktrace;
    property &type: String read FType write FType;
  end;

  TElasticAPM4DError = class
  private
    FCulprit: String;
    FException: TElasticAPM4DErrorException;
    FId: String;
    FParent_id: String;
    FTrace_id: String;
    FTransaction_id: String;
    FTimestamp: Int64;
    Fcontext: TElasticAPM4DContext;
    procedure InternalCreate;
  public
    constructor Create(ASpan: TElasticAPM4DSpan); overload;
    constructor Create(ATransaction: TElasticAPM4DTransaction); overload;
    destructor Destroy; override;

    function ToJsonString: string;

    property id: String read FId;
    property Timestamp: Int64 read FTimestamp;
    property Parent_id: String read FParent_id;
    property Trace_id: String read FTrace_id;
    property Transaction_id: String read FTransaction_id;
    property Culprit: String read FCulprit write FCulprit;
    property Context: TElasticAPM4DContext read Fcontext write Fcontext;
    property Exception: TElasticAPM4DErrorException read FException write FException;
  end;

implementation

uses
  Rest.Json,
  ElasticAPM4D.TimestampEpoch,
  ElasticAPM4D.UUid;

{ TElasticAPM4DErrorException }

destructor TElasticAPM4DErrorException.Destroy;
var
  LCause: TObject;
begin
  if Assigned(Fattributes) then
    Fattributes.Free;
  for LCause in Fcause do
    LCause.Free;
  inherited;
end;

{ TElasticAPM4DError }

procedure TElasticAPM4DError.InternalCreate;
begin
  FId := TElasticAPM4DUUid.GetUUid128b;
  FException := TElasticAPM4DErrorException.Create;
  Fcontext := TElasticAPM4DContext.Create;
  FCulprit := '';
  FTimestamp := TElasticAPM4DTimestampEpoch.Get;
end;

constructor TElasticAPM4DError.Create(ATransaction: TElasticAPM4DTransaction);
begin
  InternalCreate;
  FTrace_id := ATransaction.Trace_id;
  FTransaction_id := ATransaction.id;
  FParent_id := ATransaction.id;
end;

constructor TElasticAPM4DError.Create(ASpan: TElasticAPM4DSpan);
begin
  InternalCreate;
  FTrace_id := ASpan.Trace_id;
  FTransaction_id := ASpan.Transaction_id;
  FParent_id := ASpan.id;
end;

destructor TElasticAPM4DError.Destroy;
begin
  FException.Free;
  Fcontext.Free;
  inherited;
end;

function TElasticAPM4DError.ToJsonString: string;
begin
  Result := format('{"error": %s}', [TJson.ObjectToJsonString(self, [joIgnoreEmptyStrings,
    joIgnoreEmptyArrays])]);
end;

end.
