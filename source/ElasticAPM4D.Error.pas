unit ElasticAPM4D.Error;

interface

uses
  IdHTTP, System.SysUtils, ElasticAPM4D.Context, ElasticAPM4D.Stacktrace, ElasticAPM4D.Transaction, ElasticAPM4D.Span;

type
  TErrorException = class
  private
    FCode: String;
    FHandled: Boolean;
    FMessage: String;
    FModule: String;
    FStacktrace: TArray<TStacktrace>;
    FType: String;
    Fattributes: TObject;
    Fparent: Integer;
    Fcause: TArray<TObject>;
  public
    destructor Destroy; override;

    property Attributes: TObject read Fattributes write Fattributes;
    property Cause: TArray<TObject> read Fcause write Fcause;
    property Code: String read FCode write FCode;
    property Handled: Boolean read FHandled write FHandled;
    property &Message: String read FMessage write FMessage;
    property Module: String read FModule write FModule;
    property Parent: Integer read Fparent write Fparent;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property &Type: String read FType write FType;
  end;

  TError = class
  private
    FCulprit: String;
    FException: TErrorException;
    FId: String;
    FParent_id: String;
    FTrace_id: String;
    FTransaction_id: String;
    FTimestamp: Int64;
    Fcontext: TContext;
    procedure InternalCreate;
  public
    constructor Create(ASpan: TSpan); overload;
    constructor Create(ATransaction: TTransaction); overload;
    destructor Destroy; override;

    procedure AutoConfigureError(const AIdHttp: TIdCustomHTTP);
    function ToJsonString: string;

    property id: String read FId;
    property Timestamp: Int64 read FTimestamp;
    property Parent_id: String read FParent_id;
    property Trace_id: String read FTrace_id;
    property Transaction_id: String read FTransaction_id;
    property Culprit: String read FCulprit write FCulprit;
    property Context: TContext read Fcontext write Fcontext;
    property Exception: TErrorException read FException write FException;
  end;

implementation

uses
  Rest.Json, ElasticAPM4D.Utils, ElasticAPM4D.StackTraceJCL, ElasticAPM4D.Resources;

{ TErrorException }

destructor TErrorException.Destroy;
var
  LCause: TObject;
begin
  if Assigned(Fattributes) then
    Fattributes.Free;
  for LCause in Fcause do
    LCause.Free;
  inherited;
end;

{ TError }

procedure TError.InternalCreate;
begin
  FId := TUUid.Get128b;
  FException := TErrorException.Create;
  FException.Stacktrace := TStacktraceJCL.Get;
  Fcontext := TContext.Create;
  FCulprit := '';
  FTimestamp := TTimestampEpoch.Get(now);
end;

procedure TError.AutoConfigureError(const AIdHttp: TIdCustomHTTP);
begin
  Fcontext.AutoCreatePage(AIdHttp);
  Fcontext.AutoCreateResponse(AIdHttp);
  Fcontext.AutoCreateRequest(AIdHttp);
end;

constructor TError.Create(ATransaction: TTransaction);
begin
  InternalCreate;
  FTrace_id := ATransaction.Trace_id;
  FTransaction_id := ATransaction.id;
  FParent_id := ATransaction.id;
end;

constructor TError.Create(ASpan: TSpan);
begin
  InternalCreate;
  FTrace_id := ASpan.Trace_id;
  FTransaction_id := ASpan.Transaction_id;
  FParent_id := ASpan.id;
end;

destructor TError.Destroy;
begin
  FException.Free;
  Fcontext.Free;
  inherited;
end;

function TError.ToJsonString: string;
begin
  Result := format(sErrorJsonId, [TJson.ObjectToJsonString(self, [joIgnoreEmptyStrings,
    joIgnoreEmptyArrays])]);
end;

end.
