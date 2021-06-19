unit Share.Context.Message.Queue;

interface

type
  // <summary>
  // Queue holds information about the message queue where the message is received.
  // </summary>
  TContextMessageQueue = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
  end;

implementation

{ TContextMessageQueue }

constructor TContextMessageQueue.Create(const AName: string);
begin
  FName := AName;
end;

end.
