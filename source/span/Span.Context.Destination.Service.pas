unit Span.Context.Destination.Service;

interface


type
  TSpanContextDestinationService = class
  private
    Fname: string;
    Fresource: string;
    Ftype: string;
  public
    constructor Create(const AName, AResource, AType: string);
  end;

implementation

{ TSpanContextDestinationService }

constructor TSpanContextDestinationService.Create(const AName, AResource, AType: string);
begin
  Fname := AName;
  Fresource := AResource;
  Ftype := AType;
end;

end.
