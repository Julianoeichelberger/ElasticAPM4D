{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Span.Context.Db;

interface

uses
  System.SysUtils, System.RegularExpressions;

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
    property Instance: string read FInstance;
    property Statement: string read FStatement;
    property &Type: string read FType;
    property User: string read FUser;
    property Rows_affected: Integer read Frows_affected;
  end;

implementation


uses 
  Apm4D.Settings;

{ TSpanContextDB }

procedure TSpanContextDB.AddSQL(const ASQL: string; const ARowsAffected: Integer);
var
  LMatch: TMatch;
begin
  Frows_affected := ARowsAffected;
  FStatement := ASQL;
  // Extract SQL and RowsAffected from format: << Close [Command="SELECT..." ... RowsAffected=1 ...]
  // Regex captures both Command and optional RowsAffected in one pass
  LMatch := TRegEx.Match(FStatement, 'Command="([^"]+)"(?:.*?RowsAffected=(\d+))?', [roIgnoreCase, roSingleLine]);
  
  if LMatch.Success and (LMatch.Groups.Count > 1) then
  begin
    FStatement := Trim(LMatch.Groups[1].Value);    
    // Check if RowsAffected was captured (Group 2)
    if (LMatch.Groups.Count > 2) and (LMatch.Groups[2].Value <> '') then
      Frows_affected := StrToIntDef(LMatch.Groups[2].Value, ARowsAffected);
  end; 
end;

constructor TSpanContextDB.Create;
begin
  FInstance := TApm4DSettings.Database.Instance;
  FType := TApm4DSettings.Database.&Type;
  FUser := TApm4DSettings.Database.User;
  Flink := TApm4DSettings.Database.Server;
end;

end.
