unit Span.Context.Db;

interface

type
  // <summary>
  // Database contains contextual data for database spans
  // </summary>
  TSpanContextDB = class
  private
    // <summary>
    // Instance name of the database.
    // </summary>
    FInstance: string;
    // <summary>
    // Statement of the recorded database event, e.g. query.
    // </summary>
    FStatement: string;
    // <summary>
    // Type of the recorded database event., e.g. sql, cassandra, hbase, redis.
    // </summary>
    FType: string;
    // <summary>
    // User is the username with which the database is accessed.
    // </summary>
    FUser: string;
    Flink: string;
    // <summary>
    // RowsAffected shows the number of rows affected by the statement
    // </summary>
    Frows_affected: Integer;
  public
    constructor Create;

    procedure AddSQL(const ASQL: string; const ARowsAffected: Integer = 0);

    // <summary>
    // Link to the database server.
    // </summary>
    property Link: string read Flink write Flink;
  end;

implementation


uses
  ElasticAPM4D.Config;

{ TSpanContextDB }

procedure TSpanContextDB.AddSQL(const ASQL: string; const ARowsAffected: Integer);
begin
  FStatement := ASQL;
  Frows_affected := ARowsAffected;
end;

constructor TSpanContextDB.Create;
begin
  FInstance := TConfig.GetDatabaseInstance;
  FType := TConfig.GetDatabase;
  FUser := TConfig.GetDatabaseUser;
end;

end.
