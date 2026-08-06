# oil-volatility-forecasting-gas
Progetto corso universitario 2026. Analisi e previsione della volatilità dei futures del petrolio greggio (WTI) utilizzando modelli Generalized Autoregressive Score (GAS) in R.

# Analisi della Volatilità del Petrolio 🛢️

Questo progetto si occupa di esaminare la volatilità del prezzo dei contratti futures del petrolio greggio negli Stati Uniti, con l'obiettivo di costruire un modello statistico in grado di prevederne le fluttuazioni per il mese finanziario successivo.

## 📋 Panoramica del Progetto
* **Obiettivo**: Prevedere l'intensità delle fluttuazioni della volatilità per il mese successivo.
* **Dati**: Prezzi giornalieri in dollari/barile dei futures del petrolio greggio statunitense estratti da Yahoo Finance.
* **Periodo**: Dal 04-01-2010 al 28-01-2026 (4042 osservazioni).

## 🛠️ Metodologia e Modelli
* **Stimatore Garman-Klass**: Utilizzato per ottenere una misura robusta della volatilità sfruttando i prezzi Open, High, Low e Close.
* **Filtro di Kalman**: Applicato per imputare i valori mancanti causati dai prezzi negativi durante lo shock pandemico del 2020.
* **Modellistica GAS**: Sono stati testati diversi modelli Generalized Autoregressive Score con distribuzioni condizionate Gamma, Weibull, LogNormale, Burr e Fisk.
* **Modello Ottimale**: Il modello **GAS(2,2) con distribuzione LogNormale** (senza regressori esterni) è risultato il migliore per adattamento statistico e capacità previsiva.

## 📈 Risultati Principali
* **Persistenza**: La volatilità mostra un'elevata persistenza temporale.
* **Stagionalità**: È stata rilevata una componente ciclica ogni 22 giorni lavorativi, legata al rollover mensile dei contratti futures.
* **Previsioni Ex-ante**: Il modello proietta un progressivo riallineamento verso l'equilibrio teorico (media incondizionata $\approx 31.936$).
* **Rischio**: L'analisi evidenzia la possibilità di spike improvvisi e violenti verso l'alto, confermando la natura rischiosa dell'asset.
* **Update 06.03.2026**: Confrontati i risultati del modello previsivo ex-ante con i dati osservati reali adesso disponibili, caratterizzati dallo shock esogeno derivato dalla chiusura dello stretto di Hormuz.

## 📁 Struttura dei File
* Progetto_Volatilità_Petrolio.pdf`: Report finale dettagliato.
* Progetto_Volatilità_Petrolio.qmd`: Codice sorgente Quarto per la riproducibilità dell'analisi.
* Petrolio_greggio.csv: Dataset dei prezzi storici.

