unit Error;

interface

uses
  Share.Stacktrace,
  Share.Context,
  Error.Exception;

type
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
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    // <summary>
    // Culprit identifies the function call which was the primary perpetrator of this event
    // </summary>
    property Culprit: String read FCulprit write FCulprit;
    property Context: TContext read Fcontext;
    property Exception: TErrorException read FException;
  end;

implementation

uses
  System.SysUtils, REST.Json, Share.Uuid, Share.TimestampEpoch, Share.Stacktrace.Jcl;

{ TError }

constructor TError.Create(const ATraceId, ATransactionId, AParentId: string);
begin
  FId := TUUid.Get128b;
  FException := TErrorException.Create;
  FException.Stacktrace := TStacktraceJCL.Get;
  Fcontext := TContext.Create;
  FCulprit := TStacktraceJCL.GetLastMethod(FException.Stacktrace);
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
  Result := format('{"error": %s}', [TJson.ObjectToJsonString(self, [joIgnoreEmptyStrings, joIgnoreEmptyArrays])]);
end;

end.
