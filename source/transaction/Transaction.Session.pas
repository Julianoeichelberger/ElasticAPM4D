unit Transaction.Session;

interface

type
  // <summary>
  // Session holds optional transaction session information for RUM
  // </summary>
  TTransactionSession = class
  private
    // <summary>
    // ID holds a session ID for grouping a set of related transactions.
    // </summary>
    Fid: string;
  public
    constructor Create(const AId: string);
  end;

implementation

{ TTransactionSession }

constructor TTransactionSession.Create(const AId: string);
begin
  Fid := AId;
end;

end.
