{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor.RESTRequest;

interface

uses
  System.Classes, REST.Client, REST.HttpClient, REST.Types, Apm4D.Interceptor, System.Rtti;

type
  TApm4DInterceptRESTRequest = class(TApm4DInterceptor)
  private
    FRESTRequest: TCustomRESTRequest;
    FTransactionCreated: Boolean;
    FOnHTTPProtocolError: TCustomRESTRequestNotifyEvent;
    FOnAfterExecute: TCustomRESTRequestNotifyEvent;
    FOnBeforeExecute: TCustomRESTRequestNotifyEvent;
    function GetSpanName: string;
    function GetURL: string;
    function GetHost: string;
    function GetPort: Integer;
  protected
    procedure DoBeforeExecute(Sender: TCustomRESTRequest); virtual;
    procedure DoAfterExecute(Sender: TCustomRESTRequest); virtual;
    procedure DoHTTPProtocolError(Sender: TCustomRESTRequest); virtual;
  public
    constructor Create(AOwner, AInterceptControl: TComponent); override;
    class function IsCompatible(AComponent: TComponent): Boolean; override;
  end;

implementation

uses
  Apm4D, Apm4D.Span, System.SysUtils;

{ TApm4DInterceptRESTRequest }

class function TApm4DInterceptRESTRequest.IsCompatible(AComponent: TComponent): Boolean;
begin
  Result := AComponent is TCustomRESTRequest;
end;

constructor TApm4DInterceptRESTRequest.Create(AOwner, AInterceptControl: TComponent);
begin
  inherited;
  FTransactionCreated := False;
  FRESTRequest := FControl as TCustomRESTRequest;

  // Store original events
  FOnBeforeExecute := FRESTRequest.OnBeforeExecute;
  FOnAfterExecute := FRESTRequest.OnAfterExecute;
  FOnHTTPProtocolError := FRESTRequest.OnHTTPProtocolError;

  // Replace with our intercepted events
  FRESTRequest.OnBeforeExecute := DoBeforeExecute;
  FRESTRequest.OnAfterExecute := DoAfterExecute;
  FRESTRequest.OnHTTPProtocolError := DoHTTPProtocolError;
end;

function TApm4DInterceptRESTRequest.GetSpanName: string;
var
  Resource: string;
  Method: string;
begin
  Resource := FRESTRequest.Resource;
  if Resource.IsEmpty and Assigned(FRESTRequest.Client) then
    Resource := FRESTRequest.Client.BaseURL;

  Method := RESTRequestMethodToString(FRESTRequest.Method);

  Result := Format('%s %s', [Method, Resource]);
end;

function TApm4DInterceptRESTRequest.GetURL: string;
begin
  Result := '';
  if Assigned(FRESTRequest.Client) then
  begin
    Result := FRESTRequest.Client.BaseURL;
    if not FRESTRequest.Resource.IsEmpty then
    begin
      if not Result.EndsWith('/') and not FRESTRequest.Resource.StartsWith('/') then
        Result := Result + '/';
      Result := Result + FRESTRequest.Resource;
    end;
  end;
end;

function TApm4DInterceptRESTRequest.GetHost: string;
var
  URI: string;
  StartPos, EndPos: Integer;
begin
  Result := '';
  URI := GetURL;
  
  // Remove protocol
  StartPos := Pos('://', URI);
  if StartPos > 0 then
    URI := Copy(URI, StartPos + 3, Length(URI));
  
  // Get host before port or path
  EndPos := Pos(':', URI);
  if EndPos = 0 then
    EndPos := Pos('/', URI);
    
  if EndPos > 0 then
    Result := Copy(URI, 1, EndPos - 1)
  else
    Result := URI;
end;

function TApm4DInterceptRESTRequest.GetPort: Integer;
var
  URI: string;
  StartPos, EndPos: Integer;
  PortStr: string;
begin
  Result := 0;
  URI := GetURL;
  
  // Check for explicit port
  StartPos := Pos('://', URI);
  if StartPos > 0 then
  begin
    URI := Copy(URI, StartPos + 3, Length(URI));
    StartPos := Pos(':', URI);
    if StartPos > 0 then
    begin
      EndPos := Pos('/', URI);
      if EndPos = 0 then
        EndPos := Length(URI) + 1;
      PortStr := Copy(URI, StartPos + 1, EndPos - StartPos - 1);
      TryStrToInt(PortStr, Result);
    end;
  end;
  
  // Default ports
  if Result = 0 then
  begin
    if GetURL.StartsWith('https://', True) then
      Result := 443
    else if GetURL.StartsWith('http://', True) then
      Result := 80;
  end;
end;

procedure TApm4DInterceptRESTRequest.DoBeforeExecute(Sender: TCustomRESTRequest);
var
  SpanName: string;
  Method: string;
  Span: TSpan;
  Host: string;
  Port: Integer;
begin
  FTransactionCreated := False;
  
  // Create a transaction if none exists
  if not TApm4D.ExistsTransaction then
  begin
    SpanName := GetSpanName;
    Method := RESTRequestMethodToString(FRESTRequest.Method);
    TApm4D.StartTransactionRequest(SpanName, Method);
    FTransactionCreated := True;
  end;

  // Always create a span for HTTP request (even if we just created a transaction)
  if not TApm4D.IsPaused then
  begin
    SpanName := GetSpanName;
    Method := RESTRequestMethodToString(FRESTRequest.Method);
    
    // Create span with HTTP context
    Span := TApm4D.StartSpanRequest(SpanName, Method);
    
    // Add URL information
    Span.Context.Http.url := GetURL;
    
    // Add destination information
    Host := GetHost;
    Port := GetPort;
    if not Host.IsEmpty then
    begin
      Span.Context.CreateDestination.address := Host;
      if Port > 0 then
        Span.Context.Destination.Port := Port;

      // Add service destination info
      Span.Context.Destination.AddService(Host, FRESTRequest.Resource, 'external');
    end;
  end;

  // Call original event if exists
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Sender);
end;

procedure TApm4DInterceptRESTRequest.DoAfterExecute(Sender: TCustomRESTRequest);
begin
  // Call original event first
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Sender);

  if not TApm4D.ExistsTransaction then
    Exit;

  // Always end the span (we always create one in DoBeforeExecute)
  if Assigned(FRESTRequest.Response) then
    TApm4D.EndSpan(FRESTRequest.Response.StatusCode)
  else
    TApm4D.EndSpan;

  // End transaction only if we created it
  if FTransactionCreated then
  begin
    if Assigned(FRESTRequest.Response) then
      TApm4D.EndTransaction(FRESTRequest.Response)
    else
      TApm4D.EndTransaction;
  end;
  FTransactionCreated := false;
end;

procedure TApm4DInterceptRESTRequest.DoHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  // Log the error to APM
  if TApm4D.ExistsTransaction and Assigned(FRESTRequest.Response) then
    TApm4D.AddError(FRESTRequest.Response);

  // Call original event if exists
  if Assigned(FOnHTTPProtocolError) then
    FOnHTTPProtocolError(Sender);
end;

end.
