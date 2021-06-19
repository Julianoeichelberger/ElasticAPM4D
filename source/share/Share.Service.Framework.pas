unit Share.Service.Framework;

interface

type
  // <summary>
  // Framework holds information about the framework used in the monitored service.
  // </summary>
  TServiceFramework = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create(const AName, AVersion: string);
  end;

implementation

{ TServiceFramework }

constructor TServiceFramework.Create(const AName, AVersion: string);
begin
  FName := AName;
  FVersion := AVersion;
end;

end.
