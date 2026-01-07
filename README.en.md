# Apm4D - Elastic APM Agent for Delphi

[![Delphi](https://img.shields.io/badge/Delphi-12%20Yukon-red.svg)](https://www.embarcadero.com/products/delphi)
[![Elastic APM](https://img.shields.io/badge/Elastic%20APM-7.11.1+-005571.svg)](https://www.elastic.co/apm)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**English** | **[Español](README.es.md)** | **[Português](README.md)**

## 📋 Table of Contents

- [About](#-about)
- [Features](#-features)
- [Installation](#-installation)
- [Configuration](#-configuration)
- [Core Concepts](#-core-concepts)
- [Usage](#-usage)
- [Advanced Examples](#-advanced-examples)
- [API Reference](#-api-reference)
- [Contributing](#-contributing)
- [License](#-license)

---

## 🚀 About

**Apm4D** is an **Application Performance Monitoring** agent developed specifically for **Delphi**, enabling collection of performance metrics, distributed tracing, and application monitoring integrated with **Elastic APM**.

Compatible with **Elastic APM 7.11.1+** and tested on **Windows** and **Linux**.

---

## ✨ Features

- ✅ **Transaction Tracking** - Monitor HTTP requests, batch operations, jobs
- ✅ **Hierarchical Spans** - Track sub-operations (SQL queries, API calls)
- ✅ **Error Handling** - Automatic exception capture with stacktrace
- ✅ **System Metrics** - Real-time CPU, memory monitoring
- ✅ **Automatic Interceptors** - Auto-tracking for UI, DataSets, DB Connections
- ✅ **Thread-Safe** - Full multi-threading support
- ✅ **Distributed Tracing** - Context propagation between services
- ✅ **Stacktrace with JCL** - Detailed call stack tracing

---

## 📦 Installation

### Prerequisites

- Delphi 10.3+ (tested on Delphi 12 Yukon)
- Elastic APM Server 7.11.1+
- **[Optional]** JEDI-JCL for detailed stacktrace

### Steps

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-user/Apm4D.git
   ```

2. **Open package in Delphi**
   - Open `Apm4D.dpk` in Delphi IDE

3. **Compile and Install**
   - Right-click → **Build**
   - Right-click → **Install**

4. **Add to your project**
   - Add `Apm4D` to the `uses` clause
   - Configure search path to `source` folder

5. **[Optional] Enable Stacktrace**
   - Install JEDI-JCL: https://jedi-apilib.sourceforge.net/
   - Add `jcl` to project conditional defines

---

## ⚙️ Configuration

Configure the APM agent using `TApm4DSettings`:

```delphi
uses
  Apm4D, Apm4D.Settings;

procedure ConfigureAPM;
begin
  // Activate agent
  TApm4DSettings.Activate;
  
  // Application settings
  TApm4DSettings.Application
    .SetName('MyApp')
    .SetVersion('1.0.0')
    .SetEnvironment('production'); // staging, development, production
  
  // Elastic APM settings
  TApm4DSettings.Elastic
    .SetUrl('http://localhost:8200')
    .SetSecretToken('your-token-here'); // Optional
  
  // User settings (optional)
  TApm4DSettings.User
    .SetId('12345')
    .SetUsername('john.doe')
    .SetEmail('john@company.com');
end;
```

---

## 📚 Core Concepts

### Transactions
A **Transaction** represents a high-level operation like an HTTP request or batch job.

### Spans
A **Span** represents a sub-operation within a transaction (SQL query, HTTP call).

### Errors
Errors are automatically captured and associated with transactions/spans.

### Metricsets
System metrics collected automatically every 30 seconds (CPU, memory).

---

## 🔧 Usage

### Basic Transaction

```delphi
uses
  Apm4D;

procedure ProcessSales;
begin
  TApm4D.StartTransaction('ProcessSales', 'business');
  try
    ProcessOrders;
    UpdateInventory;
  finally
    TApm4D.EndTransaction(success);
  end;
end;
```

### HTTP Requests

```delphi
uses
  Apm4D, REST.Client;

procedure FetchCustomer(AId: Integer);
var
  RESTRequest: TRESTRequest;
begin
  TApm4D.StartTransactionRequest('/api/customers');
  try
    RESTRequest.Execute;
    TApm4D.EndTransaction(RESTResponse);
  except
    on E: Exception do
    begin
      TApm4D.AddError(E);
      raise;
    end;
  end;
end;
```

### Automatic Interceptors

```delphi
// In FormCreate:
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptOnClick, [TButton]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptDataSet, [TDataSet]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptRESTRequest, [TRESTRequest]);
  
  FInterceptorHandler := TApm4DInterceptorBuilder.CreateDefault(Self);
end;
```

---

## 📖 API Reference

See [Portuguese README](README.md#-api-reference) for complete API documentation.

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

---

## 📄 License

This project is licensed under the MIT License - see [LICENSE](LICENSE) file for details.

---

**Built with ❤️ for the Delphi community**
