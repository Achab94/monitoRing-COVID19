# monitoRing-COVID19
Portale di monitoraggio dell'evoluzione dell'epidemia COVID-19 in Italia.

## Descrizione 

La presente directory fa riferimento al portale [https://achab94.shinyapps.io/covid-19/](https://achab94.shinyapps.io/covid-19/). 

Il suo scopo è quello di monitorare l'andamento nazionale dell'[epidemia COVID-19](http://www.salute.gov.it/nuovocoronavirus) sul territorio nazionale italiano, elaborando i dati pubblicamente disponibili e quotidianamente aggiornati dalla Protezione Civile. Si prega di consultare il [sito della Protezione Civile](http://www.protezionecivile.it/attivita-rischi/rischio-sanitario/emergenze/coronavirus) per qualunque informazione circa i rischi e le misure di protezione legate all'espanzione dell'epidemia, e la [repository della Protezione Civile](https://github.com/pcm-dpc/COVID-19) per avere maggiori dettagli sui dati a disposizione online.

La realizzazione è stata motivata dalle seguenti ragioni:
- Disporre di uno strumento **facilmente utilizzabile**, **gratuito** ed **usufruibile a tutti** che consentisse di elaborare la grande quantità di dati resa disponibile quotidianamente dalla Protezione Civile;
- Permettere all'utente di elaborare le proprie analisi in modo **interattivo**, a seconda delle proprie esigenze e del proprio interesse;
- Fornire grafici e statistiche sull'andamento temporale di molteplici variabili di interesse, non solo a livello nazionale ma anche sulle dimensioni territoriali **regionali** e **provinciali**. La mia percezione è che queste siano decisamente più interessanti e significative di quelle osservate a livello nazionale, quotidianamente commentate da giornali e fonti di informazioni, che risultano dalla mera aggregazione di evoluzioni locali, con caratteristiche spesso molto eterogenee l'una con l'altrao.

## Caratteristiche e funzionalità

Il portale, nella sua versione attuale pubblicamente rilasciata in data 14 Marzo 2020, è strutturato nelle seguenti schede:
- *Evoluzione nazionale*: è la schermata principale, che svolge la funzione di *cruscotto informativo* sull'evoluzione nazionale del fenomeno. La parte superiore riporta i dati grezzi ed espressi in forma percentuale e di variazione relativa rispetto al giorno precedente di alcune delle principali variabili d'interesse che vengono quotidianamente monitorate. La parte inferiore esibisce sei differenti grafici che aiutano ad avere un'idea immediata e visiva dell'entità del fenomeno e del suo andamento, sia sui dati cumulati (sommati) giorno per giorno, sia sui nuovi dati raccolti ogni anno. Il secondo grafico in particolare aiuta ad avere un'idea della variazione (in una finestra temporale comprendente gli ultimi 7 giorni) relativa quotidiana di quattro variabili d'interesse, per capire se la crescita giornaliera sta affrondanto una fase di *assestamento* o meno.
- *Evoluzione regionale*: questa scheramata permette di approfondire l'evoluzione del fenomeno nelle varie regioni d'Italia. Il pannello di sinistra consente l'interazione con l'utente, che può scegliere le regioni che intende studiare/confrontare (fino ad un massimo di 4, per ragioni di resa grafica), quale variabile d'interesse studiare, se visualizzarne i dati cumulati o quelli giornalieri, e su quale data esibire uno dei grafici mostrati a destra. La parte di destra esibisce un confronto delle variazioni relative quotidiane (da un giorno all'altro) su alcune variabili d'interesse, confrontate sulle regioni scelte (con un'interpretazione simile a quella del grafico fornito per l'Evoluzione Nazionale); il grafico con la suddivisione in un determinato giorno dei trattati positivi al COVID-19, confrontato sulle regioni scelte; il grafico con l'evoluzione temporale del fenomeno d'interesse, confrontato sulle regioni scelte.
- *Evoluzione provinciale*: siccome l'unico dato disponibile sulla dimensione provinciale è quello del numero totale dei casi registrati in una determinata provincia, questa è l'unica informazione consultabile. Viene inoltre fornito un confronto con tutte le altre province appartenenti alle regioni alle quali afferiscono le province scelte.
- *Info*: è la pagina che fornisce ulteriori dettagli su chi ha realizzato il sito, chi ha collaborato, come è organizzato e come è stato realizzato.

## Realizzazione

Il sito è stato completamente realizzato in linguaggio *R*, utilizzando la libreria [Shiny](https://shiny.rstudio.com), e attualmente risiede su uno spazio messo a disposizione da [ShinyApps](https://www.shinyapps.io). 

L'intero codice è consultabile all'interno del file *app.R* della presente repository. Il file è organizzato nella classica struttura di un file Shiny: una sezione sull'UI dell'applicativo (la veste grafica, dell'organizzazione delle sezioni e dei grafici prodotti) ed una seconda sezione relativa alle istruzioni da far compilare al server.

## Crediti

Il sito è stato realizzato da Emanuele Degani [(website)](https://achab94.github.io), in condivisione di idee con altri colleghi citati nella sezione Info del portale. Per qualunque critica, suggerimento, dubbio od informazione, siete pregati di contattarmi all'indirizzo mail *emanuele [dot] achab [at] gmail [dot] com* od aprendo una Issues nella repository.


