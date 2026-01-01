{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Transaction.Experience.Longtask;

interface

type
  // <summary>
  // Longtask holds longtask duration/count metrics.
  // </summary>
  TTransactionExperienceLongTask = class
  private
    FMax: Integer;
    FCount: Integer;
    FSum: Integer;
  public
    // Count is the total number of of longtasks.
    property Count: Integer read FCount write FCount;
    // Max longtask duration
    property Max: Integer read FMax write FMax;
    // Sum of longtask durations
    property Sum: Integer read FSum write FSum;
  end;

implementation

end.
