unit Span;

interface

uses
  Share.Stacktrace,
  Span.Context;

type
  TSpan = class
  private
    FStartDate: TDateTime;
    FAction: string;
    FContext: TSpanContext;
    FDuration: Int64;
    FId: string;
    FName: string;
    FParent_id: string;
    FStacktrace: TArray<TStacktrace>;
    FSubtype: string;
    FSync: Boolean;
    FTrace_id: string;
    FTransaction_id: string;
    FType: string;
    Ftimestamp: Int64;
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    procedure Start(const AName: string; const AType: string = 'Undefined');
    procedure StartRequest(const AName, AHttpMethod: string);

    procedure ToEnd;
    procedure ToEndRequest(const AHttpCode: Integer);

    property Id: string read FId;
    property Transaction_id: string read FTransaction_id;
    property Trace_id: string read FTrace_id;
    property Subtype: string read FSubtype write FSubtype;
    property Action: string read FAction write FAction;
    property Context: TSpanContext read FContext;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property Sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
  end;

implementation

{ TSpan }

uses
  Share.Stacktrace.Jcl,
  System.SysUtils, System.DateUtils, REST.Json,
  Share.Uuid, Share.TimestampEpoch, ElasticAPM4D.Config;

constructor TSpan.Create(const ATraceId, ATransactionId, AParentId: string);
begin
  FId := TUUid.Get64b;
  FTrace_id := ATraceId;
  FTransaction_id := ATransactionId;
  FParent_id := AParentId;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FContext := TSpanContext.Create;
end;

destructor TSpan.Destroy;
var
  Stacktrace: TStacktrace;
begin
  for Stacktrace in FStacktrace do
    Stacktrace.Free;
  FreeAndNil(FContext);
  inherited;
end;

procedure TSpan.ToEnd;
begin
  FStacktrace :=  TStacktraceJCL.Get;
  FDuration := MilliSecondsBetween(now, FStartDate);
end;

procedure TSpan.ToEndRequest(const AHttpCode: Integer);
begin
  FContext.Http.AddStatusCode(AHttpCode);
  ToEnd;
end;

procedure TSpan.Start(const AName, AType: string);
begin
  FStartDate := now;
  Ftimestamp := TTimestampEpoch.Get(FStartDate);
  FName := AName;
  FType := AType;
end;

procedure TSpan.StartRequest(const AName, AHttpMethod: string);
begin
  Start(AName, 'Request');
  FContext.CreateHttp(AHttpMethod);
end;

function TSpan.ToJsonString: string;
begin
  Result := format('{"span": %s}', [TJson.ObjectToJsonString(self, [joDateIsUTC, joIgnoreEmptyStrings])]);
end;

end.
