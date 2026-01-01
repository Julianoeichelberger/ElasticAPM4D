{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Types;

interface

uses 
  SysUtils;

type
  TOutcome = (success, failure, unknown);

  EElasticAPM4DException = class(Exception);

  ETransactionNotFound = class(EElasticAPM4DException)

  end;

implementation

end.
