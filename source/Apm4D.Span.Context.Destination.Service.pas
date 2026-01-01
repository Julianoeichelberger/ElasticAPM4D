{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Span.Context.Destination.Service;

interface


type
  TSpanContextDestinationService = class
  private
    Fname: string;
    Fresource: string;
    Ftype: string;
  public
    constructor Create(const AName, AResource, AType: string);

    property Name: string read Fname;
    property Resource: string read Fresource;
    property &Type: string read Ftype;
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
