unit Span.Context.Destination;

interface

uses
  Span.Context.Destination.Service;

type
  // <summary>
  // Destination contains contextual data about the destination of spans
  // </summary>
  TSpanContextDestination = class
  private
    Faddress: string;
    FPort: Integer;
    FService: TSpanContextDestinationService;
  public
    destructor Destroy; override;

    procedure AddService(const AName, AResource, AType: string);

    // <summary>
    // Address is the destination network address: hostname
    // (e.g. 'localhost'), FQDN (e.g. 'elastic.co'), IPv4 (e.g. '127.0.0.1') IPv6 (e.g. '::1')
    // </summary>
    property address: string read Faddress write Faddress;
    // <summary>
    // Port is the destination network port (e.g. 443)
    // </summary>
    property Port: Integer read FPort write FPort;
  end;

implementation


{ TSpanContextDestination }

procedure TSpanContextDestination.AddService(const AName, AResource, AType: string);
begin
  FService := TSpanContextDestinationService.Create(AName, AResource, AType);
end;

destructor TSpanContextDestination.Destroy;
begin
  if Assigned(FService) then
    FService.Free;
  inherited;
end;

end.
