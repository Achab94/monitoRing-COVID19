# Informazioni su questa piattaforma

---

##### Autore

La presente piattaforma è stata realizzata da Emanuele Degani [(web)](https://achab94.github.io).
Suggerimenti, critiche e segnalazioni di eventuali bug sono caldamente suggerite all'indirizzo **degani [at] stat [dot] unipd [dot] it**.

E' doveroso un ringraziamento ad Emanuele Aliverti, Pietro Belloni, Laura D'Angelo, Nicola Pilia, Stefano, Francesco Mancone per aver contribuito con suggerimenti e critiche preziose.

**Update (16 Marzo 2020)** : nelle ultime ore ho registrato un elevato numero di accessi che ha compromesso l'usufruibilità del portale, viste le limitate risorse computazionali messe a disposizione dalla tariffa gratuita del server. Mi sono pertanto dovuto trovare costretto ad affittare maggiori risorse computazionali al provider (ShinyApps), per un costo mensile di 35 euro. Ho deciso di chiedere un'offerta libera a chiunque desiderasse contribuire alle spese, secondo le proprie disponibilità, tramite un [MoneyBox Paypal](https://paypal.me/pools/c/8nqxplRvVc) che si sarebbe automaticamente chiuso non appena la cifra necessaria sarebbe stata raggiunta (onde evitare di ricevere ulteriori offerte non necessarie). In meno di due ore la somma totale è stata raggiunta: questo è stato possibile grazie alle donazioni di Guido Milano, Gianpaolo Natale, Franco Bocci, Enrico Bonan, Fabio Comini, Stefano De Lazzari. Grazie di cuore.

---

##### Fonte dei dati

La piattaforma preleva i dati in automatico, ogniqualvolta un utente accede, dall'ultima versione aggiornata del database [(link GitHub)](https://github.com/pcm-dpc/COVID-19) reso disponibile dalla Protezione Civile quotidianamente intorno alle h18:30, subito dopo la conferenza stampa del Capo del Dipartimento della Protezione Civile Angelo Borrelli. Si consiglia di consultare [questo link](https://github.com/pcm-dpc/COVID-19/blob/master/README.md) per ogni informazione sui dati disponibili e sul data-entry (questo perchè spesso alcuni dati giornalieri sono mancanti, o arrivano in ritardo).

---

##### Funzionalità della piattaforma

La  presente piattaforma è organizzata secondo la seguente struttura
  - **Evoluzione nazionale**: la schermata riassume gli ultimi dati disponibili a livello nazionale, e mostra alcuni grafici relativi alle serie storiche di alcune variabili d'interesse. Le curve blu corrispondono a stime *loess* nonparametriche dell'evoluzione del fenomeno, le bande grigie misurano il grado di confidenza della stima realizzata: indicativamente, servono ad avere un'idea di massima di come il fenomeno si è evoluto nel tempo.
  - **Evoluzione regionale**: la schermata consente di analizzare l'andamento del fenomeno, regione per regione. E' possibile selezionare le regioni che si intendono studiare/confrontare, la dimensione d'interesse, e se osservarne il conteggio cumulato (sommato giorno per giorno) o il conteggio parziale di ciascun giorno. E' possibile inoltre osservare l'evoluzione temporale della suddivisione nel trattamento dei pazienti risultati positivi al COVID-19.
  - **Evoluzione provinciale**: la schermata consente di analizzare l'andamento del numero totale di contagiati (unico dato disponibile sulla dimensione territoriale provinciale), provincia per provincia. E' possibile selezionare le province che si intendono studiare/confrontare, ed osservare l'evoluzione temporale di tale dato, confrontando le province selezionate con tutte le altre province appartenenti alla Regione di riferimento.
  - **Info**: questa pagina.

---

##### Come è stata realizzata

La piattaforma è stata realizzata interamente in linguaggio **R**, utilizzando le funzionalità della libreria Shiny [(website)](https://shiny.rstudio.com). Altre librerie utilizzate sono `ggplot2` (per il data-visualization), `dplyr` (per il data-processing) e `shinyWidgets` (per alcuni dei widget inclusi).
La piattaforma giace online sullo spazio di calcolo messo a disposizione da ShinyApps [(website)](https://www.shinyapps.io).

A breve confido di poter rendere pubblicamente frubile l'intero codice utilizzato, sul [mio profilo GitHub](https://github.com/Achab94).

---

##### Licenza di utilizzo

![GitHub license](https://img.shields.io/badge/License-Creative%20Commons%20Attribution%204.0%20International-blue)
