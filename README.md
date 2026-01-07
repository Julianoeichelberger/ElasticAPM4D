# Apm4D - Elastic APM Agent para Delphi

[![Delphi](https://img.shields.io/badge/Delphi-12%20Yukon-red.svg)](https://www.embarcadero.com/products/delphi)
[![Elastic APM](https://img.shields.io/badge/Elastic%20APM-7.11.1+-005571.svg)](https://www.elastic.co/apm)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**[English](README.en.md)** | **[Español](README.es.md)** | **Português**

## 📋 Índice

- [Sobre](#-sobre)
- [Características](#-características)
- [Instalação](#-instalação)
- [Configuração](#-configuração)
- [Conceitos Fundamentais](#-conceitos-fundamentais)
  - [Transactions](#transactions)
  - [Spans](#spans)
  - [Errors](#errors)
  - [Metricsets](#metricsets)
- [Uso](#-uso)
  - [Transactions Básicas](#transactions-básicas)
  - [Spans](#usando-spans)
  - [Requisições HTTP](#requisições-http)
  - [Banco de Dados](#banco-de-dados)
  - [Tratamento de Erros](#tratamento-de-erros)
  - [Interceptors](#interceptors-automáticos)
  - [Métricas do Sistema](#métricas-do-sistema)
- [Exemplos Avançados](#-exemplos-avançados)
- [API Reference](#-api-reference)
- [Contribuindo](#-contribuindo)
- [Licença](#-licença)

---

## 🚀 Sobre

**Apm4D** é um agente de **Application Performance Monitoring** desenvolvido especificamente para **Delphi**, permitindo coletar métricas de desempenho, rastreamento distribuído e monitoramento de aplicações integrado ao **Elastic APM**.

Compatível com **Elastic APM 7.11.1+** e testado em **Windows** e **Linux**.

---

## ✨ Características

- ✅ **Rastreamento de Transactions** - Monitore requisições HTTP, operações batch, jobs
- ✅ **Spans Hierárquicos** - Rastreie sub-operações (queries SQL, chamadas API)
- ✅ **Tratamento de Erros** - Captura automática de exceções com stacktrace
- ✅ **Métricas do Sistema** - CPU, memória, processamento em tempo real
- ✅ **Interceptors Automáticos** - Rastreamento automático de UI, DataSets, Conexões DB
- ✅ **Thread-Safe** - Suporte completo a multi-threading
- ✅ **Rastreamento Distribuído** - Propagação de contexto entre serviços
- ✅ **Stacktrace com JCL** - Rastreamento detalhado de pilha de chamadas

---

## 📦 Instalação

### Pré-requisitos

- Delphi 10.3+ (testado no Delphi 12 Yukon)
- Elastic APM Server 7.11.1+
- **[Opcional]** JEDI-JCL para stacktrace detalhado

### Passos

1. **Clone o repositório**
   ```bash
   git clone https://github.com/seu-usuario/ElasticAPM4D.git
   ```
2. **Adicione ao projeto**
   - Adicione `Apm4D` na cláusula `uses` dos seus arquivos
   - Configure o caminho de busca para a pasta `source`

3. **[Opcional] Ative Stacktrace**
   - Instale JEDI-JCL: https://jedi-apilib.sourceforge.net/
   - Adicione `jcl` nas definições condicionais do projeto

---

## ⚙️ Configuração

Configure o agente APM usando `TApm4DSettings`:

```delphi
uses
  Apm4D, Apm4D.Settings;

procedure ConfigureAPM;
begin  
  // Configurações da aplicação
  TApm4DSettings.Application.Name := "Minha Aplicação"; // Default é o nome do executável
  TApm4DSettings.Application.Version := "1.0.0"; // Default é a versão do executável 
  TApm4DSettings.Application.Environment := "production"; // staging, development, production 
  
  // Configurações do Elastic APM
  TApm4DSettings.Elastic.Url := 'http://localhost:8200'; // URL do APM Server (Default = http://localhost:8200)
  TApm4DSettings.Elastic.SecretToken := 'seu-token-aqui'; // Opcional
  TApm4DSettings.Elastic.UpdateTime := 60000; // Intervalo de envio de métricas em milissegundos (Default = 60000)
  TApm4DSettings.Elastic.MaxJsonPerThread := 60; // Máximo de eventos JSON por thread na fila (Default = 60)
  
  // Configurações do usuário (opcional)
  TApm4DSettings.User.Id := '12345'; // Default ID único do usuário no SO
  TApm4DSettings.User.Username := 'joao.silva'; // Default Nome do usuário logado no SO 
  TApm4DSettings.User.Email := 'joao.silva@empresa.com'; // Email do usuário (Default '')

  // Configurações do banco de dados (opcional)
  TApm4DSettings.Database.Instance := 'MinhaBaseDados'; // Nome da base de dados (Default '')
  TApm4DSettings.Database.Server := 'localhost'; // Servidor de banco de dados (Default '')
  TApm4DSettings.Database.Type := 'mssql'; // Tipo do banco de dados (mssql, mysql, postgres, oracle) (Default '')
  TApm4DSettings.Database.User := 'dbuser'; // Usuário do banco de dados (Default '')

  // Ativar o agente
  TApm4DSettings.Activate;
end;
```

---

## 📚 Conceitos Fundamentais

### Transactions

Uma **Transaction** representa uma operação de alto nível, como:
- Uma requisição HTTP
- Um job batch
- Uma operação de processamento

```delphi
TApm4D.StartTransaction('ProcessarPedidos', 'batch');
try
  // Seu código aqui
finally
  TApm4D.EndTransaction;
end;
```

### Spans

Um **Span** representa uma sub-operação dentro de uma transaction:
- Query SQL
- Chamada HTTP externa
- Processamento de arquivo

```delphi
TApm4D.StartSpan('CarregarDados', 'db.query');
try
  // Consulta ao banco
finally
  TApm4D.EndSpan;
end;
```

### Errors

Erros capturados automaticamente e associados à transaction/span:

```delphi
try
  // Código que pode falhar
except
  on E: Exception do
    TApm4D.AddError(E);
end;
```

### Metricsets

Métricas coletadas automaticamente a cada 30 segundos:
- **CPU**: `system.cpu.total.norm.pct`, `system.process.cpu.total.norm.pct`
- **Memória**: `system.memory.total`, `system.memory.actual.free`

---

## 🔧 Uso

### Transactions Básicas

```delphi
uses
  Apm4D;

procedure ProcessarVendas;
begin
  TApm4D.StartTransaction('ProcessarVendas', 'business');
  try
    // Seu código de negócio
    ProcessarPedidos;
    AtualizarEstoque;
    GerarRelatorios;
  finally
    TApm4D.EndTransaction(success); // success, failure, unknown
  end;
end;
```

### Usando Spans

```delphi
procedure ProcessarPedidos;
begin
  TApm4D.StartTransaction('ProcessarPedidos', 'batch');
  try
    // Span 1: Carregar dados
    TApm4D.StartSpan('CarregarPedidos', 'db.query');
    try
      Query.Open('SELECT * FROM Pedidos WHERE Status = ''Pendente''');
    finally
      TApm4D.EndSpan;
    end;
    
    // Span 2: Processar
    TApm4D.StartSpan('ProcessarItens', 'business');
    try
      while not Query.Eof do
      begin
        ProcessarPedido(Query.FieldByName('Id').AsInteger);
        Query.Next;
      end;
    finally
      TApm4D.EndSpan;
    end;
    
    // Span 3: Enviar notificação
    TApm4D.StartSpan('EnviarEmail', 'external.http');
    try
      EnviarNotificacaoCliente;
    finally
      TApm4D.EndSpan;
    end;
  finally
    TApm4D.EndTransaction;
  end;
end;
```

### Requisições HTTP

#### Com TRESTClient (Delphi nativo)

```delphi
uses
  Apm4D, REST.Client;

procedure BuscarCliente(AId: Integer);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://api.exemplo.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'api/v1/clientes/{id}';
    RESTRequest.AddParameter('id', AId.ToString, TRESTRequestParameterKind.pkURLSEGMENT);
    
    // Inicia transaction HTTP
    TApm4D.StartTransactionRequest(RESTRequest);
    try
      RESTRequest.Execute;
      
      // Finaliza com resposta
      TApm4D.EndTransaction(RESTResponse);
    except
      on E: Exception do
      begin
        TApm4D.AddError(E);
        TApm4D.EndTransaction(RESTResponse);
        raise;
      end;
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

#### Com TRESTRequest (Nativo Delphi)

```delphi
uses
  Apm4D, REST.Client, REST.Types;

procedure ConsultarAPI;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://api.exemplo.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'api/v1/dados';
    RESTRequest.Method := rmGET;
    
    // Transaction/Span criado automaticamente pelo interceptor
    TApm4D.StartTransactionRequest(RESTRequest);
    try
      RESTRequest.Execute; // Monitorado automaticamente
      
      if RESTResponse.StatusCode = 200 then
        TApm4D.EndTransaction(RESTResponse)
      else
        TApm4D.EndTransaction(failure);
    except
      on E: Exception do
      begin
        TApm4D.AddError(RESTResponse); // Captura erro HTTP
        TApm4D.EndTransaction(failure);
        raise;
      end;
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

**Com Interceptor Automático:**
```delphi
// Registre o interceptor uma vez no FormCreate:
TApm4DSettings.RegisterInterceptor(TApm4DInterceptRESTRequest, [TRESTRequest]);

// Depois, apenas execute normalmente:
procedure TFormPrincipal.btnBuscarClick(Sender: TObject);
begin
  RESTRequest1.Execute; // Span HTTP criado automaticamente!
  // O interceptor:
  // - Cria uma transaction se não existir nenhuma
  // - SEMPRE cria um span com todas as informações HTTP (método, URL, host, porta)
  // - Adiciona informações de destino (destination) para rastreamento distribuído
  // - Finaliza o span automaticamente com o status code correto
  // - Finaliza a transaction somente se foi ele quem a criou
  // - Captura erros HTTP automaticamente
end;
```

**Exemplo de Span Gerado pelo Interceptor:**
```
Transaction: ProcessarPedido
  Span: GET https://api.exemplo.com/pedidos/123
    Type: external
    Subtype: http
    Context:
      - HTTP Method: GET
      - URL: https://api.exemplo.com/pedidos/123
      - Status Code: 200
      - Destination Address: api.exemplo.com
      - Destination Port: 443
    Duration: 350ms
```
  // - Captura erros HTTP automaticamente
end;
```

#### Com Indy (IdHTTP)

```delphi
uses
  Apm4D, IdHTTP;

procedure BuscarDados;
var
  HTTP: TIdHTTP;
  Response: string;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    TApm4D.StartTransactionRequest('/api/v1/dados', 'GET');
    try
      Response := HTTP.Get('https://api.exemplo.com/api/v1/dados');
      TApm4D.EndTransaction(success);
    except
      on E: EIdHTTPProtocolException do
      begin
        TApm4D.AddError(E);
        TApm4D.EndTransaction(failure);
        raise;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;
```

### Banco de Dados

#### Spans SQL Automáticos

```delphi
procedure CarregarClientes;
begin
  TApm4D.StartTransaction('CarregarClientes', 'db.operation');
  try
    // Span automático para query
    TApm4D.StartSpanDb('SELECT Clientes', 'mssql');
    try
      FDQuery.Open('SELECT * FROM Clientes WHERE Ativo = 1');
    finally
      TApm4D.EndSpan;
    end;
    
    // Processar dados
    while not FDQuery.Eof do
    begin
      ProcessarCliente(FDQuery);
      FDQuery.Next;
    end;
  finally
    TApm4D.EndTransaction;
  end;
end;
```

#### Definir SQL no Span Atual

```delphi
TApm4D.StartSpan('ExecutarUpdate', 'db.query');
try
  TApm4D.SetSQLToCurrentSpan('UPDATE Produtos SET Estoque = Estoque - 1 WHERE Id = 123');
  FDQuery.ExecSQL;
finally
  TApm4D.EndSpan;
end;
```

### Tratamento de Erros

#### Exceções Genéricas

```delphi
try
  ProcessarArquivo('dados.xml');
except
  on E: Exception do
  begin
    TApm4D.AddError(E); // Captura automática com stacktrace
    raise;
  end;
end;
```

#### Exceções HTTP

```delphi
try
  RESTRequest.Execute;
except
  on E: EIdHTTPProtocolException do
  begin
    TApm4D.AddError(E); // Captura código HTTP, mensagem
    raise;
  end;
end;
```

#### Erro Customizado

```delphi
uses
  Apm4D.Error;

var
  Error: TError;
begin
  Error := TError.Create;
  try
    Error.Exception.&Type := 'ValidationError';
    Error.Exception.Message := 'CPF inválido';
    Error.Exception.Code := 'VAL001';
    TApm4D.AddError(Error);
  finally
    Error.Free;
  end;
end;
```

### Interceptors Automáticos

Os interceptors monitoram automaticamente componentes VCL/FMX e criam spans.

#### Configuração

```delphi
uses
  Apm4D.Interceptor.Handler, Apm4D.Settings;

// No FormCreate ou DataModuleCreate:
procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  // Registrar interceptors globais (fazer uma vez na aplicação)
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptOnClick, [TButton, TSpeedButton, TBitBtn]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptDataSet, [TDataSet]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptDBConnection, [TFDConnection]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptRESTRequest, [TRESTRequest]);
  
  // Injetar interceptors no formulário
  FInterceptorHandler := TApm4DInterceptorBuilder.CreateDefault(Self);
end;
```

#### O que é monitorado automaticamente

- **Clicks em Botões**: Cria transaction para cada click
- **Operações de DataSet**: Monitora `Open`, `Post`, `Delete`, `Execute`
- **Conexões de Banco**: Monitora reconexão, rollback, disconnect
- **Requisições REST (TRESTRequest)**: 
  - SEMPRE cria um span com informações HTTP completas (método, URL, host, porta)
  - Cria uma transaction apenas se não existir nenhuma
  - Adiciona contexto de destino (destination) para rastreamento distribuído
  - Finaliza automaticamente com status code
  - Captura erros HTTP

#### Exemplo de Span Gerado Automaticamente

Quando você clica em um botão:
```
Transaction: FormPrincipal.btnSalvar.Click
  Type: UI.Click
  Duration: 250ms
```

Quando você executa um TRESTRequest dentro de uma transaction existente:
```
Transaction: ProcessarPedido
  Span: GET https://api.exemplo.com/pedidos
    Type: Request
    Duration: 350ms
    Status Code: 200
```

### Métricas do Sistema

Métricas são coletadas automaticamente a cada 30 segundos:

```delphi
// Não precisa fazer nada, métricas são enviadas automaticamente!
```

**Métricas disponíveis:**
- `system.memory.total`: Memória total do sistema (bytes)
- `system.memory.actual.free`: Memória livre disponível (bytes)
- `system.cpu.total.norm.pct`: CPU total do sistema (0-1)
- `system.process.cpu.total.norm.pct`: CPU do processo (0-1)

---

## 🎯 Exemplos Avançados

### Rastreamento Distribuído

```delphi
// Serviço A: Cria transaction e propaga contexto
procedure ChamarServicoB;
var
  HTTP: TIdHTTP;
begin
  TApm4D.StartTransaction('ChamarServicoB', 'http');
  try
    HTTP := TIdHTTP.Create(nil);
    try
      // Adiciona header de rastreamento
      HTTP.Request.CustomHeaders.AddValue(
        TApm4D.HeaderKey, 
        TApm4D.HeaderValue
      );
      
      HTTP.Get('http://servicoB/api/processar');
    finally
      HTTP.Free;
    end;
  finally
    TApm4D.EndTransaction;
  end;
end;

// Serviço B: Continua a transaction
procedure ProcessarRequisicao(ARequest: TWebRequest);
var
  TraceId: string;
begin
  // Extrai contexto do header
  TraceId := ARequest.GetFieldByName(TApm4D.HeaderKey);
  
  // Continua a transaction com o mesmo trace ID
  TApm4D.StartTransaction('ProcessarDados', 'business', TraceId);
  try
    ProcessarDados;
  finally
    TApm4D.EndTransaction;
  end;
end;
```

### Pausar/Despausar Monitoramento

```delphi
procedure OperacaoSensivel;
begin
  TApm4D.StartTransaction('OperacaoUsuario', 'business');
  try
    ProcessarDadosPublicos;
    
    // Pausar monitoramento para dados sensíveis
    TApm4D.Pause;
    try
      ProcessarDadosSensiveis; // Não será monitorado
    finally
      TApm4D.UnPause;
    end;
    
    GerarRelatorio;
  finally
    TApm4D.EndTransaction;
  end;
end;
```

### Transaction com Resultado Customizado

```delphi
TApm4D.StartTransaction('ProcessarPagamento', 'payment');
try
  if ProcessarCartao then
  begin
    TApm4D.Transaction.Result := 'APPROVED';
    TApm4D.EndTransaction(success);
  end
  else
  begin
    TApm4D.Transaction.Result := 'DECLINED';
    TApm4D.EndTransaction(failure);
  end;
end;
```

---

## 📖 API Reference

### TApm4D

#### Métodos de Transaction

| Método | Descrição |
|--------|-----------|
| `StartTransaction(Name, Type, TraceId)` | Inicia uma transaction |
| `StartTransactionRequest(Resource, Method, TraceId)` | Inicia transaction HTTP |
| `StartTransactionRequest(TRESTRequest)` | Inicia transaction com TRESTRequest |
| `EndTransaction(Outcome)` | Finaliza transaction com resultado |
| `EndTransaction(TRESTResponse)` | Finaliza transaction HTTP |
| `ExistsTransaction: Boolean` | Verifica se há transaction ativa |
| `Transaction: TTransaction` | Retorna transaction atual |

#### Métodos de Span

| Método | Descrição |
|--------|-----------|
| `StartSpan(Name, Type): TSpan` | Inicia um span |
| `StartSpanDb(Name, Database): TSpan` | Inicia span de banco de dados |
| `StartSpanRequest(Resource, Method): TSpan` | Inicia span HTTP |
| `SetSQLToCurrentSpan(SQL)` | Define SQL do span atual |
| `EndSpan` | Finaliza span atual |
| `EndSpan(StatusCode)` | Finaliza span HTTP com status |
| `Span: TSpan` | Retorna span atual |

#### Métodos de Erro

| Método | Descrição |
|--------|-----------|
| `AddError(Exception)` | Adiciona exceção genérica |
| `AddError(EIdHTTPProtocolException)` | Adiciona erro HTTP Indy |
| `AddError(TRESTResponse)` | Adiciona erro REST |
| `AddError(TError)` | Adiciona erro customizado |

#### Métodos de Controle

| Método | Descrição |
|--------|-----------|
| `Pause` | Pausa monitoramento |
| `UnPause` | Resume monitoramento |
| `IsPaused: Boolean` | Verifica se está pausado |
| `HeaderKey: string` | Retorna chave do header de rastreamento |
| `HeaderValue: string` | Retorna valor do header de rastreamento |

### TApm4DSettings

#### Application

```delphi
TApm4DSettings.Application
  .SetName(string)
  .SetVersion(string)
  .SetEnvironment(string); // staging, development, production
```

#### Elastic

```delphi
TApm4DSettings.Elastic
  .SetUrl(string)
  .SetSecretToken(string);
```

#### User

```delphi
TApm4DSettings.User
  .SetId(string)
  .SetUsername(string)
  .SetEmail(string);
```

#### Database

```delphi
TApm4DSettings.Database
  .SetName(string)
  .SetConnection(string);
```

#### Log

```delphi
TApm4DSettings.Log
  .SetLevel(TLogLevel)    // llNone, llError, llWarning, llInfo, llDebug
  .SetPath(string);
```

---

## 🤝 Contribuindo

Contribuições são bem-vindas! Para contribuir:

1. Fork o projeto
2. Crie uma branch para sua feature (`git checkout -b feature/MinhaFeature`)
3. Commit suas mudanças (`git commit -m 'Adiciona MinhaFeature'`)
4. Push para a branch (`git push origin feature/MinhaFeature`)
5. Abra um Pull Request

---

## 📄 Licença

Este projeto está licenciado sob a licença MIT - veja o arquivo [LICENSE](LICENSE) para detalhes.

---

## 🔗 Links Úteis

- [Elastic APM Documentation](https://www.elastic.co/guide/en/apm/get-started/current/index.html)
- [Elastic APM Specification](https://github.com/elastic/apm)
- [JEDI-JCL](https://jedi-apilib.sourceforge.net/)

---

**Desenvolvido com ❤️ para a comunidade Delphi**
 
