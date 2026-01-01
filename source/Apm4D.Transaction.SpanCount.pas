{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Transaction.SpanCount;

interface

type
  // <summary>
  // SpanCount counts correlated spans.
  // </summary>
  TTransactionSpanCount = class
  private
    FDropped: Integer;
    FStarted: Integer;
  public
    constructor Create;

    procedure Inc;
    procedure Dec;
    procedure Reset;

    property Dropped: Integer read FDropped;
    property Started: Integer read FStarted;
  end;

implementation

{ TTransactionSpanCount }

constructor TTransactionSpanCount.Create;
begin
  Reset;
end;

procedure TTransactionSpanCount.Dec;
begin
  FDropped := FDropped - 1;
end;

procedure TTransactionSpanCount.Inc;
begin
  FStarted := FStarted + 1;
  FDropped := FDropped + 1;
end;

procedure TTransactionSpanCount.Reset;
begin
  FDropped := 0;
  FStarted := 0;
end;

end.
