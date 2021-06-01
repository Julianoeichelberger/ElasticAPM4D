unit ElasticAPM4D.Message;

interface

type
  // <summary>
  // Age of the message. If the monitored messaging framework provides a timestamp for the message, agents may use it.
  // Otherwise, the sending agent can add a timestamp in milliseconds since the Unix epoch to the message's metadata to
  // be retrieved by the receiving agent. If a timestamp is not available, agents should omit this field.
  // </summary>
  TAge = class
  private
    Fms: Integer;
  public
    constructor Create(const AMilliseconds: Integer);
    property ms: Integer read Fms;
  end;

  // <summary>
  // Queue holds information about the message queue where the message is received.
  // </summary>
  TQueue = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  // <summary>
  // Message holds details related to message receiving and publishing if the captured event integrates with a messaging system
  // </summary>
  TMessage = class
  private
    FBody: string;
    FAge: TAge;
    FHeaders: TObject;
    FQueue: TQueue;
  public
    constructor Create(const AName: string = ''; ABody: string = '');
    destructor Destroy; override;

    property Age: TAge read FAge write FAge;
    property Headers: TObject read FHeaders write FHeaders;
    property Queue: TQueue read FQueue write FQueue;
    // <summary>
    // Body of the received message, similar to an HTTP request body
    // </summary>
    property Body: string read FBody write FBody;
  end;

implementation

uses
  SysUtils;

{ TAge }

constructor TAge.Create(const AMilliseconds: Integer);
begin
  Fms := AMilliseconds;
end;

{ TQueue }

constructor TQueue.Create(const AName: string);
begin
  FName := AName;
end;

{ TMessage }

constructor TMessage.Create(const AName: string; ABody: string);
begin
  if not AName.IsEmpty then
    FQueue := TQueue.Create(AName);

  FBody := ABody;
end;

destructor TMessage.Destroy;
begin
  if Assigned(FQueue) then
    FQueue.Free;
  if Assigned(FAge) then
    FAge.Free;
  if Assigned(FHeaders) then
    FHeaders.Free;
  inherited;
end;

end.
