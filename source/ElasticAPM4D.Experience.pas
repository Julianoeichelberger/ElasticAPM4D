unit ElasticAPM4D.Experience;

interface

type
  // <summary>
  // Longtask holds longtask duration/count metrics.
  // </summary>
  TLongTask = class
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

  // <summary>
  // UserExperience holds metrics for measuring real user experience. This information is only sent by RUM agents.
  // </summary>
  TExperience = class
  private
    Fcls: Integer;
    Ffid: Integer;
    Ftbt: Integer;
    Flongtask: TLongTask;
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

    property LongTask: TLongTask read Flongtask write Flongtask;
  end;

implementation

{ TExperience }

destructor TExperience.Destroy;
begin
  if Assigned(Flongtask) then
    Flongtask.Free;
  inherited;
end;

end.
