{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.Cloud.Account;

interface

type
  // Account where the monitored service is running.
  TMetadataCloudAccount = class
  private
    Fname: string;
    Fid: string;
  public
    // ID of the cloud account
    property id: string read Fid write Fid;
    // Name of the cloud account.
    property name: string read Fname write Fname;
  end;

implementation

end.
