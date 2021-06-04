unit ElasticAPM4D.Package;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  ElasticAPM4D.Transaction, ElasticAPM4D.Metadata, ElasticAPM4D.User, ElasticAPM4D.Span, ElasticAPM4D.Error;

type
  TPackage = class
  private
    FMetadata: TMetadata;
    FTransaction: TTransaction;
    FSpanList: TObjectList<TSpan>;
    FErrorList: TObjectList<TError>;
    FOpenSpanStack: TList;
    FUser: TUser;
    FHeader: string;
    function ExtractTraceId: string;
    function ExtractParentID: string;
    function GetHeader: string;
    procedure SetHeader(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ToSend;
    function SpanIsOpened: Boolean;
    function CurrentSpan: TSpan;

    property Metadata: TMetadata read FMetadata write FMetadata;
    property Transaction: TTransaction read FTransaction write FTransaction;
    property SpanList: TObjectList<TSpan> read FSpanList write FSpanList;
    property ErrorList: TObjectList<TError> read FErrorList write FErrorList;
    property OpenSpanStack: TList read FOpenSpanStack write FOpenSpanStack;
    property User: TUser read FUser write FUser;
    property Header: string read GetHeader write SetHeader;
  end;

implementation

Uses
  IdHTTP, Forms, IOUtils, ElasticAPM4D.ndJson, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

function SendToElasticAPM(AURL, AHeader: string; const AJson: WideString): Boolean;
var
  CouldSend: Boolean;
begin
  CouldSend := True;
  TThread.CreateAnonymousThread(
    procedure
    var
      Http: TIdHTTP;
      DataSend, Result: TStringStream;
    begin
      Http := TIdHTTP.Create;
      DataSend := TStringStream.Create(AJson, TEncoding.UTF8);
      Result := TStringStream.Create('');
      try
        Http.Request.ContentType := 'application/x-ndjson';
        Http.Request.Charset := 'gzip';
        Http.Request.CustomHeaders.AddValue(sHEADER_KEY, AHeader);
        try
          Http.Post(AURL, DataSend, Result); 
        except
          CouldSend := False;
//          with TStringList.Create do
//          begin
//            Text := AJson;
//            SaveToFile(GetCurrentDir + '/' + TPath.GetFileNameWithoutExtension(Application.ExeName) + '_'
//              + Http.ResponseCode.ToString + '_' + FormatDateTime('hh-mm-sss', now) + '.txt');
//            Free;
//          end;
        end;
      finally
        DataSend.Free;
        Result.Free;
        Http.Free;
      end;
    end).Start;
  Result := CouldSend;
end;

{ TSendPackage }

constructor TPackage.Create;
begin
  FMetadata := TMetadata.Create;
  FTransaction := TTransaction.Create;
  FSpanList := TObjectList<TSpan>.Create;
  FOpenSpanStack := TList.Create;
  FErrorList := TObjectList<TError>.Create;
  FUser := TUser.Create;
  FHeader := '';
end;

function TPackage.CurrentSpan: TSpan;
begin
  if not SpanIsOpened then
    Exit(nil);

  Result := FOpenSpanStack.Items[Pred(FOpenSpanStack.Count)];
end;

destructor TPackage.Destroy;
begin
  FTransaction.Free;
  FMetadata.Free;
  FreeAndNil(FSpanList);
  FreeAndNil(FErrorList);
  FOpenSpanStack.Free;
  FUser.Free;
  inherited;
end;

function TPackage.SpanIsOpened: Boolean;
begin
  Result := FOpenSpanStack.Count > 0;
end;

function TPackage.GetHeader: string;
begin
  Result := FHeader;
  if Result.IsEmpty then
  begin
    if SpanIsOpened then
      Result := Format(sHEADER, [FTransaction.trace_id, CurrentSpan.id])
    else
      Result := Format(sHEADER, [FTransaction.trace_id, FTransaction.id]);
  end
end;

procedure TPackage.ToSend;
var
  ndJson: TndJson;
begin
  if not TConfig.GetIsActive then
    Exit;
  ndJson := TndJson.Create;
  try
    ndJson.Add(FMetadata);
    ndJson.Add(FTransaction);
    ndJson.Add(FSpanList);
    ndJson.Add(FErrorList);

    SendToElasticAPM(TConfig.GetUrlElasticAPM, GetHeader, ndJson.Get);
  finally
    FHeader := '';
    ndJson.Free;
  end;
end;

function TPackage.ExtractParentID: string;
begin
  Result := Copy(FHeader, 37, 16);
end;

function TPackage.ExtractTraceId: string;
begin
  Result := Copy(FHeader, 4, 32);
end;

procedure TPackage.SetHeader(const Value: string);
begin
  FHeader := Value;
  if not FHeader.IsEmpty then
  begin
    FTransaction.trace_id := ExtractTraceId;
    FTransaction.parent_id := ExtractParentID;
  end;
end;

end.
