{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Transaction.Session;

interface

type
  // <summary>
  // Session holds optional Apm4D.Transaction.Session information for RUM
  // </summary>
  TTransactionSession = class
  private
    Fid: string;
  public
    constructor Create(const AId: string);

    // <summary>
    // ID holds a session ID for grouping a set of related transactions.
    // </summary>
    property Id: String read Fid;
  end;

implementation

{ TTransactionSession }

constructor TTransactionSession.Create(const AId: string);
begin
  Fid := AId;
end;

end.
