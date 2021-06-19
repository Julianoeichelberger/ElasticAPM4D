unit Metadata.Cloud.Instance;

interface

type
  // Instance on which the monitored service is running
  TMetadataCloudInstance = class
  private
    Fid: string;
    Fname: string;
  public
    // ID of the cloud instance
    property id: string read Fid write Fid;
    // Name of the cloud instance
    property name: string read Fname write Fname;
  end;

implementation

end.
