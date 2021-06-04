unit ElasticAPM4D.Error;

interface

uses
  System.SysUtils, ElasticAPM4D.Stacktrace, ElasticAPM4D.Span;

type
  TException = class
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
    FException: TException;
    FId: String;
    FParent_id: String;
    FTrace_id: String;
    FTransaction_id: String;
    FTimestamp: Int64;
    Fcontext: TContext;
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    property id: String read FId;
    property Timestamp: Int64 read FTimestamp;
    property Parent_id: String read FParent_id;
    property Trace_id: String read FTrace_id;
    property Transaction_id: String read FTransaction_id;

    // <summary>
    // Culprit identifies the function call which was the primary perpetrator of this event
    // </summary>
    property Culprit: String read FCulprit write FCulprit;
    property Context: TContext read Fcontext write Fcontext;
    property Exception: TException read FException write FException;
  end;

implementation

uses
  Rest.Json, ElasticAPM4D.Utils, ElasticAPM4D.StackTraceJCL, ElasticAPM4D.Resources;

{ TException }

destructor TException.Destroy;
var
  Item: TObject;
begin
  if Assigned(Fattributes) then
    Fattributes.Free;
  for Item in Fcause do
    Item.Free;
  inherited;
end;

{ TError }

constructor TError.Create(const ATraceId, ATransactionId, AParentId: string);
begin
  FId := TUUid.Get128b;
  FException := TException.Create;
  FException.Stacktrace := TStacktraceJCL.Get;
  Fcontext := TContext.Create;
  FCulprit := '';
  FTimestamp := TTimestampEpoch.Get(now);
  FTrace_id := ATraceId;
  FTransaction_id := ATransactionId;
  FParent_id := AParentId;
end;

destructor TError.Destroy;
begin
  FException.Free;
  Fcontext.Free;
  inherited;
end;

function TError.ToJsonString: string;
begin
  Result := format(sErrorJsonId, [TJson.ObjectToJsonString(self, [joIgnoreEmptyStrings, joIgnoreEmptyArrays])]);
end;

end.
