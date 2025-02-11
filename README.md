# Municipal_Efficiency_Study

# Analisi dell'Impatto del Doppio Turno Elettorale nei Comuni Italiani
<img src="./fig/portfolio-4.png" width="800" class="center">


## 📌 Descrizione del Progetto
Questo progetto analizza l'effetto dell'introduzione del doppio turno elettorale nei comuni italiani con più di 15.000 abitanti. L'obiettivo è determinare se questa riforma ha portato a una riduzione o un aumento dell'efficienza amministrativa, confrontando i dati di un anno pre-policy (1994) e un anno post-policy (2003).

## 📊 Dataset
Il dataset utilizzato (\`myxz_runoff.xls\`) contiene:
- **17328 osservazioni**
- **22 variabili** rappresentanti vari aspetti dei comuni italiani tra il 1994 e il 2005.

Per la pulizia dei dati sono stati applicati i metodi **Tukey's Fences** con k=1.5 e k=3 per escludere le anomalie. Si sono considerati due dataset distinti:
1. Un dataset ripulito da tutte le variabili con outlier sospetti.
2. Un dataset ripulito solo dalla variabile dei quintali di rifiuti raccolti.

## 📈 Metodologia
L'analisi si basa su **tecniche di analisi non parametrica**, che non fanno ipotesi stocastiche sui dati:
- **FDH (Free Disposal Hull)**: metodo senza ipotesi di convessità.
- **DEA-V (Data Envelopment Analysis - Variabile)**: include convessità nei dati.
- **DEA-C (Costante)**: aggiunge proporzionalità e additivà.

Per misurare l'impatto della policy sono stati usati:
- **Difference-in-Differences (DiD)**: confronto tra unità trattate e non trattate.
- **Statistical Matching (SM)**: identificazione di unità simili per confronto.

## 📉 Risultati Principali
- **DEA-V (1994)**: Solo il **10.4%** dei comuni risultava efficiente.
- **DEA-V (2003)**: La percentuale di comuni efficienti scende all'**8.4%**, segnalando un generale aumento dell'inefficienza.
- **DiD Analysis**: L'efficienza aumenta nei comuni con doppio turno rispetto a quelli con turno unico.
- **Statistical Matching**: Conferma che i comuni trattati sono più efficienti rispetto ai non trattati.
- **Analisi di robustezza (plm)**: Alcuni risultati sono meno chiari, ma il doppio turno sembra avere un effetto positivo sull'efficienza amministrativa.

## 🛠️ Tecnologie e Strumenti
- **Linguaggi**: R, Python
- **Librerie principali**: dplyr, ggplot2, plm, lm
- **Software**: RStudio, Excel, QGIS (per eventuali analisi spaziali)

## 📜 Conclusioni
Nonostante alcune incertezze nei risultati di robustezza, i dati suggeriscono che **l'introduzione del doppio turno ha migliorato l'efficienza amministrativa nei comuni con più di 15.000 abitanti**, riducendo gli impegni di spesa corrente.

## 📂 Struttura del Progetto
```
├── data/                      # Dataset originale e dataset pulito
├── scripts/                   # Codici R per l'analisi
├── results/                   # Output delle analisi e grafici
├── README.md                  # Documentazione del progetto
└── report/                    # Relazione finale e appendici
```

## 🚀 Avvio del Progetto
1. Clonare la repository:
   ```sh
   git clone https://github.com/tuo-username/tuo-repo.git
   ```
2. Aprire lo script in **RStudio** e installare le dipendenze necessarie:
   ```r
   install.packages(c("dplyr", "ggplot2", "plm", "lm"))
   ```
3. Eseguire il codice per generare le analisi:
   ```r
   source("scripts/analysis.R")
   ```

## 📝 Autore
**Carmela Pia Senatore**

---
🔍 Per maggiori dettagli, consultare il report completo in **report/**.
