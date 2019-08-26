unit ElasticAPM4D.Auto;

interface

Uses
  System.SysUtils,
  System.RTTI;

type
  IElasticAPM4DReflection = Interface
    ['{73EC2F53-29E3-442A-B5D0-8E92F4DBF12F}']
    procedure Invoke(const AMethodName: string);
    function GetInvoke(const AMethodName: string): TValue;
  end;

type
  EElasticAPM4DReflection = Exception;

  TElasticAPM4DAuto = class(TInterfacedObject, IElasticAPM4DReflection)
  strict private
    FObject: TObject;
  private
    procedure ExecInSpanTransaction(const AName: string; AProc: TProc);
  public
    constructor Create(Sender: TObject);

    class function New(Sender: TObject): IElasticAPM4DReflection;

    procedure Invoke(const AMethodName: string);
    function GetInvoke(const AMethodName: string): TValue;
  end;

implementation

{ TElasticAPM4DAuto }

uses
  ElasticAPM4D;

constructor TElasticAPM4DAuto.Create(Sender: TObject);
begin
  FObject := Sender;
end;

class function TElasticAPM4DAuto.New(Sender: TObject): IElasticAPM4DReflection;
begin
  Result := TElasticAPM4DAuto.Create(Sender);
end;

function TElasticAPM4DAuto.GetInvoke(const AMethodName: string): TValue;
var
  LRes: TValue;
begin
  ExecInSpanTransaction(AMethodName,
    procedure
    var
      LMethod: TRttiMethod;
      LRttiContext: TRttiContext;
      LInfo: TRttiType;
    begin
      LInfo := LRttiContext.GetType(FObject.ClassType);
      try
        LMethod := LInfo.GetMethod(AMethodName);
        LRes := LMethod.Invoke(FObject, []);
      except
        raise EElasticAPM4DReflection.CreateFmt('Method not found', [AMethodName]);
      end;
    end);
  Result := LRes;
end;

procedure TElasticAPM4DAuto.Invoke(const AMethodName: string);
begin
  ExecInSpanTransaction(AMethodName,
    procedure
    var
      LMethod: TRttiMethod;
      LRttiContext: TRttiContext;
      LInfo: TRttiType;
    begin
      LInfo := LRttiContext.GetType(FObject.ClassType);
      try
        LMethod := LInfo.GetMethod(AMethodName);
        LMethod.Invoke(FObject, []);
      except
        raise EElasticAPM4DReflection.CreateFmt('Method not found', [AMethodName]);
      end;
    end);
end;

procedure TElasticAPM4DAuto.ExecInSpanTransaction(const AName: string; AProc: TProc);
var
  LExistsTransaction: Boolean;
begin
  LExistsTransaction := TElasticAPM4D.ExistsTransaction;
  if LExistsTransaction then
    TElasticAPM4D.StartCustomSpan(AName)
  else
    TElasticAPM4D.StartTransaction(AName);
  try
    Try
      AProc;
    except
      on E: Exception do
      begin
        TElasticAPM4D.AddError(E);
        raise;
      end;
    end;
  Finally
    if LExistsTransaction then
      TElasticAPM4D.EndSpan
    else
      TElasticAPM4D.EndTransaction;
  End;
end;

end.
