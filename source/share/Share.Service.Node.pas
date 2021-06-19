unit Share.Service.Node;

interface

type
  // <summary>
  // Node must be a unique meaningful name of the service node
  // </summary>
  TServiceNode = class
  private
    // <summary>
    // Name of the service node
    // </summary>
    FConfigured_name: string;
  public
    constructor Create(const AConfiguredName: string);
  end;

implementation

{ TServiceNode }

constructor TServiceNode.Create(const AConfiguredName: string);
begin
  FConfigured_name := AConfiguredName;
end;

end.
