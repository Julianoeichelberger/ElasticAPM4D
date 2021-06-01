unit ElasticAPM4D.Destination;

interface

type
  TService = class
  private
    Fname: string;
    Fresource: string;
    Ftype: string;
  public
    property Name: string read Fname write Fname;
    property Resource: string read Fresource write Fresource;
    property &Type: string read Ftype write Ftype;
  end;

  // <summary>
  // Destination contains contextual data about the destination of spans
  // </summary>
  TDestination = class
  private
    Faddress: string;
    FPort: Integer;
    FService: TService;
  public
    destructor Destroy; override;
    // <summary>
    // Address is the destination network address: hostname
    // (e.g. 'localhost'), FQDN (e.g. 'elastic.co'), IPv4 (e.g. '127.0.0.1') IPv6 (e.g. '::1')
    // </summary>
    property address: string read Faddress write Faddress;
    // <summary>
    // Port is the destination network port (e.g. 443)
    // </summary>
    property Port: Integer read FPort write FPort;

    property Service: TService read FService write FService;
  end;

implementation

{ TDestination }

destructor TDestination.Destroy;
begin
  if Assigned(FService) then
    FService.Free;
  inherited;
end;

end.
