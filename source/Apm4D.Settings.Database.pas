unit Apm4D.Settings.Database;

interface

type
  /// <summary>
  /// Database connection information (thread-safe)
  /// </summary>
  TDatabaseSettings = class
  strict private 
    FType: string;
    FUser: string;
    FInstance: string;
    FServer: string; 
  public  
    property &Type: string read FType write FType;
    property User: string read FUser write FUser;
    property Instance: string read FInstance write FInstance;
    property Server: string read FServer write FServer;
  end;

implementation

end.    