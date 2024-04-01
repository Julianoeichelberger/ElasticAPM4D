# ElasticAPM4D
It´s an Agent for Elastic-APM in Delphi to collect software metrics. 

Compatible with Elastic-APM 7.11.1 (not tested on others).

If you want to activate stacktrace (recommended) you need to install JEDI-Jcl
and add jcl in the conditional defines. 


Windows and Linux support.

To set the global settings, you can use the TConfig class. 
Example:

```delphi

Uses
  ElasticAPM4D.Config;

  TConfig.SetAppName('My App'); // If not defined, it'll use the application.name
  TConfig.SetAppVersion('V1.1.0') //If not defined, it'll get the application version (only windows)
  TConfig.SetActive(False); // Always active, but you can inactivate de APM agent. 
  TConfig.SetUrlElasticAPM('http://192.168.1.1:8200/intake/v2/events'); // default is local (http://127.0.0.1:8200/intake/v2/events)
  TConfig.SetSecret(''); // Set if you use secret token for authentication
  TConfig.SetEnvironment('production'); // If not defined, it'll use 'staging'

  TConfig.SetUserId('123'); 
  TConfig.SetUserMail('useremail@mycompany.com'); 
  TConfig.SetUserName('user name'); // if not defined, it'll get windows user
```

Transactions examples:

Simple transaction

```delphi
Uses 
  ElasticAPM4D;

 TApm.StartTransaction('MyTransactionName', 'MyType');
  try
    try
      TApm.StartSpan('Sleep1');
      Sleep(500);
      TApm.EndSpan;

      TApm.StartSpan('Sleep2');
      Sleep(750);
      TApm.EndSpan;

      Abort;
    except
      on E: exception do
        TApm.AddError(E);
    end;
  finally
    TApm.EndTransaction;
  end;

```

Rest request transaction (TRESTRequest)
```delphi

 RESTRequest.Resource := 'api/v1/customers';
  TApm.StartTransactionRequest(RESTRequest.Resource);
  try
    try 
      RESTRequest.Execute;
    except
      on E: EHTTPProtocolException do
        TApm.AddError(E);
      on E: exception do
        TApm.AddError(E);
    end;
  finally
    TApm.EndTransaction(RESTRequest.Response);
  end;

```
 
