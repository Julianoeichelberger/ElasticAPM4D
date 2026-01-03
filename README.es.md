# Apm4D - Agente Elastic APM para Delphi

[![Delphi](https://img.shields.io/badge/Delphi-12%20Yukon-red.svg)](https://www.embarcadero.com/products/delphi)
[![Elastic APM](https://img.shields.io/badge/Elastic%20APM-7.11.1+-005571.svg)](https://www.elastic.co/apm)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**[English](README.en.md)** | **Español** | **[Português](README.md)**

## 📋 Índice

- [Acerca de](#-acerca-de)
- [Características](#-características)
- [Instalación](#-instalación)
- [Configuración](#-configuración)
- [Conceptos Fundamentales](#-conceptos-fundamentales)
- [Uso](#-uso)
- [Ejemplos Avanzados](#-ejemplos-avanzados)
- [Referencia API](#-referencia-api)
- [Contribuir](#-contribuir)
- [Licencia](#-licencia)

---

## 🚀 Acerca de

**Apm4D** es un agente de **Monitoreo de Rendimiento de Aplicaciones** desarrollado específicamente para **Delphi**, permitiendo la recopilación de métricas de rendimiento, rastreo distribuido y monitoreo de aplicaciones integrado con **Elastic APM**.

Compatible con **Elastic APM 7.11.1+** y probado en **Windows** y **Linux**.

---

## ✨ Características

- ✅ **Seguimiento de Transacciones** - Monitorea peticiones HTTP, operaciones batch, trabajos
- ✅ **Spans Jerárquicos** - Rastrea sub-operaciones (consultas SQL, llamadas API)
- ✅ **Manejo de Errores** - Captura automática de excepciones con stacktrace
- ✅ **Métricas del Sistema** - CPU, memoria en tiempo real
- ✅ **Interceptores Automáticos** - Rastreo automático de UI, DataSets, Conexiones DB
- ✅ **Thread-Safe** - Soporte completo para multi-threading
- ✅ **Rastreo Distribuido** - Propagación de contexto entre servicios
- ✅ **Stacktrace con JCL** - Rastreo detallado de pila de llamadas

---

## 📦 Instalación

### Prerequisitos

- Delphi 10.3+ (probado en Delphi 12 Yukon)
- Elastic APM Server 7.11.1+
- **[Opcional]** JEDI-JCL para stacktrace detallado

### Pasos

1. **Clonar el repositorio**
   ```bash
   git clone https://github.com/tu-usuario/Apm4D.git
   ```

2. **Abrir el paquete en Delphi**
   - Abrir `Apm4D.dpk` en Delphi IDE

3. **Compilar e Instalar**
   - Click derecho → **Build**
   - Click derecho → **Install**

4. **Añadir al proyecto**
   - Añadir `Apm4D` en la cláusula `uses`
   - Configurar ruta de búsqueda a la carpeta `source`

5. **[Opcional] Activar Stacktrace**
   - Instalar JEDI-JCL: https://jedi-apilib.sourceforge.net/
   - Añadir `jcl` en definiciones condicionales del proyecto

---

## ⚙️ Configuración

Configurar el agente APM usando `TApm4DSettings`:

```delphi
uses
  Apm4D, Apm4D.Settings;

procedure ConfigurarAPM;
begin
  // Activar el agente
  TApm4DSettings.Activate;
  
  // Configuraciones de la aplicación
  TApm4DSettings.Application
    .SetName('MiApp')
    .SetVersion('1.0.0')
    .SetEnvironment('production'); // staging, development, production
  
  // Configuraciones de Elastic APM
  TApm4DSettings.Elastic
    .SetUrl('http://localhost:8200')
    .SetSecretToken('tu-token-aqui'); // Opcional
  
  // Configuraciones del usuario (opcional)
  TApm4DSettings.User
    .SetId('12345')
    .SetUsername('juan.perez')
    .SetEmail('juan@empresa.com');
end;
```

---

## 📚 Conceptos Fundamentales

### Transacciones
Una **Transacción** representa una operación de alto nivel como una petición HTTP o trabajo batch.

### Spans
Un **Span** representa una sub-operación dentro de una transacción (consulta SQL, llamada HTTP).

### Errores
Los errores se capturan automáticamente y se asocian con transacciones/spans.

### Metricsets
Métricas del sistema recopiladas automáticamente cada 30 segundos (CPU, memoria).

---

## 🔧 Uso

### Transacción Básica

```delphi
uses
  Apm4D;

procedure ProcesarVentas;
begin
  TApm4D.StartTransaction('ProcesarVentas', 'business');
  try
    ProcesarPedidos;
    ActualizarInventario;
  finally
    TApm4D.EndTransaction(success);
  end;
end;
```

### Peticiones HTTP

```delphi
uses
  Apm4D, REST.Client;

procedure ObtenerCliente(AId: Integer);
var
  RESTRequest: TRESTRequest;
begin
  TApm4D.StartTransactionRequest('/api/clientes');
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

### Interceptores Automáticos

```delphi
// En FormCreate:
procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptOnClick, [TButton]);
  TApm4DSettings.RegisterInterceptor(TApm4DInterceptDataSet, [TDataSet]);
  
  FInterceptorHandler := TApm4DInterceptorBuilder.CreateDefault(Self);
end;
```

---

## 📖 Referencia API

Ver [README en Portugués](README.md#-api-reference) para documentación completa de la API.

---

## 🤝 Contribuir

¡Las contribuciones son bienvenidas! No dudes en enviar un Pull Request.

---

## 📄 Licencia

Este proyecto está licenciado bajo la Licencia MIT - ver archivo [LICENSE](LICENSE) para detalles.

---

**Desarrollado con ❤️ para la comunidad Delphi**
