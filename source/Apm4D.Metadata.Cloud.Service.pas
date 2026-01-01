{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.Cloud.Service;

interface

type
  // Service that is monitored on cloud
  TMetadataCloudService = class
  private
    // Name of the cloud service, intended to distinguish services running on different platforms within a provider,
    // eg AWS EC2 vs Lambda, GCP GCE vs App Engine, Azure VM vs App Server
    Fname: string;
  public
    constructor Create(const AServiceName: string);
    property Name: String read Fname;
  end;

implementation

{ TMetadataCloudService }

constructor TMetadataCloudService.Create(const AServiceName: string);
begin
  Fname := AServiceName;
end;

end.
