{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Transaction.Experience;

interface

uses 
  Apm4D.Transaction.Experience.Longtask;

type
  // <summary>
  // UserExperience holds metrics for measuring real user experience. This information is only sent by RUM agents.
  // </summary>
  TTransactionExperience = class
  private
    Fcls: Integer;
    Ffid: Integer;
    Ftbt: Integer;
    Flongtask: TTransactionExperienceLongTask;
  public
    destructor Destroy; override;
    // <summary>
    // CumulativeLayoutShift holds the Cumulative Layout Shift (CLS) metric value,
    // or a negative value if CLS is unknown. See https://web.dev/cls/
    // </summary>
    property cls: Integer read Fcls write Fcls;

    // <summary>
    // FirstInputDelay holds the First Input Delay (FID) metric value,
    // or a negative value if FID is unknown. See https://web.dev/fid/
    // </summary>
    property fid: Integer read Ffid write Ffid;

    // <summary>
    // TotalBlockingTime holds the Total Blocking Time (TBT) metric value,
    // or a negative value if TBT is unknown. See https://web.dev/tbt/
    // </summary>
    property tbt: Integer read Ftbt write Ftbt;

    property Longtask: TTransactionExperienceLongTask read Flongtask write Flongtask;
  end;

implementation

{ TTransactionExperience }

destructor TTransactionExperience.Destroy;
begin
  if Assigned(Flongtask) then
    Flongtask.Free;
  inherited;
end;

end.
