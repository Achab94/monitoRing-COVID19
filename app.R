library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(shinyWidgets)
library(reshape2)
library(rmarkdown)

# instructions for updating
# library(rsconnect)
# rsconnect::deployApp("Dropbox/COVID-19/COVID-19/")

# Definition of the User Interface ----------------------------------------

ui <- navbarPage(title = "Monitoraggio evoluzione COVID-19 in Italia",

                 tabPanel("Evoluzione nazionale",
                          div(style="margin-top:-3.75em",
                              fluidRow(
                                  column(width=8,
                                     htmlOutput("headerCruscotto"),
                                     htmlOutput("summary"),
                                     style="padding:15px;")
                                  )
                              ),
                          
                          fluidPage(
                              tags$head(includeHTML(("google-analytics.html"))),
                              HTML('<meta name="viewport" content="width=1024">'),
                              fluidRow(column(width=4, plotOutput("scatterMonitoraggioITA")),
                                       column(width=4, plotOutput("barPlotDeltaPercentualiITA")),
                                       column(width=4, plotOutput("scatterRapportoDimessiGuaritiTotaleAttualmentePositiviITA"))),
                              fluidRow(column(width=4, plotOutput("scatterNuoviAttualmentePositiviITA")),
                                       column(width=4, plotOutput("scatterDecedutiITA")),
                                       column(width=4, plotOutput("scatterDimessiGuaritiITA")))
                              
                          )
                 ),
                 tabPanel("Evoluzione regionale",
                          sidebarLayout(
                              sidebarPanel(
                                  pickerInput(
                                      inputId = "selezionaRegioni", 
                                      label = "Seleziona la/e regione/i di interesse (max 4)", 
                                      choices = list("Nord Italia" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta", 
                                                                          "Emilia Romagna", "Friuli Venezia Giulia", "Veneto"), ### TRENTINO ALTO ADIGE NON C'E'
                                                     "Centro Italia" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                                     "Sud Italia" = list("Abruzzo", "Basilicata", "Calabria",
                                                                         "Campania", "Molise", "Puglia"),
                                                     "Isole" = list("Sardegna", "Sicilia")),
                                      selected = c("Lombardia", "Emilia Romagna"),
                                      options = list(
                                          `actions-box` = FALSE,
                                          header = "Regioni",
                                          `max-options` = 4,
                                          `max-options-text` = "Seleziona al massimo 4 regioni"
                                      ), 
                                      multiple = TRUE
                                  ),
                                  
                                  radioButtons("selezionaVariabileRegioni", 
                                               label = "Seleziona la variabile d'interesse",
                                               choices = list("Pazienti attualmente positivi", "Pazienti deceduti", "Pazienti dimessi/guariti", 
                                                              "Pazienti ricoverati con sintomi", "Pazienti in terapia intensiva", "Pazienti in isolamento domiciliare"), 
                                               selected = "Pazienti attualmente positivi"
                                  ),
                                  
                                  materialSwitch(inputId = "giornalieroSiNo", 
                                                 label = "Visualizza il dato giornaliero", 
                                                 status = "danger"),
                                  
                                  helpText("Il grafico di sinistra mostra di default il conteggio comulato dei pazienti attualmente infetti. Se si desidera visualizzare
                                           l'andamento giornaliero (quanti nuovi casi sono stati registrati ogni giorno), selezionare il pulsante."),
                                  
                                  uiOutput("selezionaGiornoRegio"),
                                  
                                  helpText("Il grafico di destra mostra di default la suddivisione odierna della tipologia di trattamento per i pazienti attualmente infetti. 
                                           Se si desidera visualizzare l'evoluzione della suddivisione giorno per giorno, trascinare il pulsante.")
                                  
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                                        ),
                                verticalLayout(
                                  fluidRow(
                                      splitLayout(cellWidths = c("49%", "49%"),  plotOutput("barPlotDeltaPercentualiRegionale"), plotOutput("barPlotRegionale"))
                                  ),
                                  fluidRow(plotOutput("scatterRegionale"))
                                )
                              )
                          )
                 ),
                 tabPanel("Evoluzione provinciale",
                          sidebarLayout(
                              sidebarPanel(
                                  pickerInput(
                                      inputId = "selezionaProvincie", 
                                      label = "Seleziona la/e provincia/e di interesse (max 4)", 
                                      choices = list("Abruzzo" = list("Chieti", "L'Aquila", "Pescara", "Teramo"),
                                                     "Basilicata" = list("Matera", "Potenza"),
                                                     "Calabria" = list("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia"),
                                                     "Campania" = list("Avellino", "Benevento", "Caserta", "Napoli", "Salerno"),
                                                     "Emilia Romagna" = list("Bologna", "Ferrara", "Forlì-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini"),
                                                     "Friuli Venezia Giulia" = list("Gorizia", "Pordenone", "Trieste", "Udine"),
                                                     "Lazio" = list("Frosinone", "Latina", "Rieti", "Roma", "Viterbo"),
                                                     "Liguria" = list("Genova", "Imperia", "La Spezia", "Savona"),
                                                     "Lombardia" = list("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese"),
                                                     "Marche" = list("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino"),
                                                     "Molise" = list("Campobasso", "Isernia"),
                                                     "Piemonte" = list("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli"),
                                                     "Puglia" = list("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto"),
                                                     "Sardegna" = list("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna"),
                                                     "Sicilia" = list("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani"),
                                                     "Toscana" = list( "Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena"),
                                                     "Umbria" = list("Perugia", "Terni"),
                                                     "Valle d'Aosta" = list("Aosta"),
                                                     "Veneto" = list("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")),
                                      selected = c("Bergamo", "Brescia"),
                                      options = list(
                                          `actions-box` = FALSE,
                                          header = "Province",
                                          `max-options` = 4,
                                          `max-options-text` = "Seleziona al massimo 4 province"
                                      ), 
                                      multiple = TRUE
                                  ),

                                  uiOutput("selezionaGiornoProv"),
                                  
                                  helpText("Il grafico di destra mostra di default la comparazione regionale odierna del numero di casi totali registrati. 
                                           Se si desidera visualizzare l'evoluzione giorno per giorno, trascinare il pulsante.")
                                  
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ),
                                  fluidRow(
                                      splitLayout(cellWidths = c("44%", "55%"),  plotOutput("scatterProvinciale"), plotOutput("barPlotProvinciale"))
                                  )
                              )
                          )
                 ),
                 # tabPanel("Previsione di evoluzione")
                 
                tabPanel("Info", includeMarkdown("Info.md"))
)


server <- function(input, output) {
    dataProvinceInput <- reactive({
        storicoProvince <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv", stringsAsFactors = FALSE)
        storicoProvince <- subset(storicoProvince, denominazione_provincia!="In fase di definizione/aggiornamento")
        
        myProvince <- data.frame(data=as.Date(storicoProvince$data),
                                 Provincia = storicoProvince$denominazione_provincia,
                                 Regione = storicoProvince$denominazione_regione,
                                 totale_casi = storicoProvince$totale_casi,
                                 x = (as.numeric(as.Date(storicoProvince$data)) - min(as.numeric(as.Date(storicoProvince$data))) + 1))
        myProvince <- myProvince %>%
            group_by(Provincia) %>%
            mutate(diff_totale_casi = diff(c(0, totale_casi)))
        myProvince
    })
    
    dataRegioniInput <- reactive({
        storicoRegioni <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", stringsAsFactors = FALSE)
        
        myRegioni <- data.frame(data=as.Date(storicoRegioni$data),
                                Regione = storicoRegioni$denominazione_regione,
                                totale_attualmente_positivi = storicoRegioni$totale_attualmente_positivi,
                                nuovi_attualmente_positivi = storicoRegioni$nuovi_attualmente_positivi,
                                deceduti = storicoRegioni$deceduti,
                                dimessi_guariti = storicoRegioni$dimessi_guariti,
                                totale_casi = storicoRegioni$totale_casi,
                                ricoverati_con_sintomi = storicoRegioni$ricoverati_con_sintomi,
                                terapia_intensiva = storicoRegioni$terapia_intensiva,
                                isolamento_domiciliare = storicoRegioni$isolamento_domiciliare,
                                x = (as.numeric(as.Date(storicoRegioni$data)) - min(as.numeric(as.Date(storicoRegioni$data))) + 1))
        myRegioni <- myRegioni %>%
            group_by(Regione) %>%
            mutate(diff_deceduti = diff(c(0, deceduti)),
                   diff_dimessi_guariti = diff(c(0, dimessi_guariti)),
                   diff_ricoverati_con_sintomi = diff(c(0, ricoverati_con_sintomi)),
                   diff_terapia_intensiva = diff(c(0, terapia_intensiva)),
                   diff_isolamento_domiciliare = diff(c(0, isolamento_domiciliare)))
        myRegioni
    })
    
    dataItaliaInput <- reactive({
        storicoItalia <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", stringsAsFactors = FALSE)
        
        myItalia <- data.frame(data=as.Date(storicoItalia$data),
                               totale_attualmente_positivi = storicoItalia$totale_attualmente_positivi,
                               nuovi_attualmente_positivi = storicoItalia$nuovi_attualmente_positivi,
                               deceduti = storicoItalia$deceduti,
                               diff_deceduti = diff(c(0, storicoItalia$deceduti)),
                               dimessi_guariti = storicoItalia$dimessi_guariti,
                               diff_dimessi_guariti = diff(c(0, storicoItalia$dimessi_guariti)),
                               totale_casi = storicoItalia$totale_casi,
                               ricoverati_con_sintomi = storicoItalia$ricoverati_con_sintomi,
                               diff_ricoverati_con_sintomi = diff(c(0, storicoItalia$ricoverati_con_sintomi)),
                               terapia_intensiva = storicoItalia$terapia_intensiva,
                               diff_terapia_intensiva = diff(c(0, storicoItalia$terapia_intensiva)),
                               isolamento_domiciliare = storicoItalia$isolamento_domiciliare,
                               diff_isolamento_domiciliare = diff(c(0, storicoItalia$isolamento_domiciliare)),
                               tamponi = storicoItalia$tamponi,
                               x = (as.numeric(as.Date(storicoItalia$data)) - min(as.numeric(as.Date(storicoItalia$data))) + 1))
        myItalia
    })
    
    output$selezionaGiornoRegio <- renderUI({
        sliderTextInput(
            inputId = "selezionaGiornoRegio", 
            label = "Seleziona il giorno di interesse", 
            grid = FALSE, 
            force_edges = TRUE,
            width = 500,
            selected = as.Date(dataRegioniInput()$data[length(dataRegioniInput()$data)]),
            choices = seq(as.Date("2020-02-24"), as.Date(dataRegioniInput()$data[length(dataRegioniInput()$data)]), 1)
        )
    })
    
    output$selezionaGiornoProv <- renderUI({
        sliderTextInput(
            inputId = "selezionaGiornoProv", 
            label = "Seleziona il giorno di interesse", 
            grid = FALSE, 
            force_edges = TRUE,
            width = 500,
            selected = as.Date(dataProvinceInput()$data[length(dataProvinceInput()$data)]),
            choices = seq(as.Date("2020-02-24"), as.Date(dataProvinceInput()$data[length(dataProvinceInput()$data)]), 1)
        )
    })
    
    output$headerCruscotto <- reactive({
        ultimaData <- dataItaliaInput()$data[length(dataItaliaInput()$data)]
        paste0(h1("La situazione attuale* in Italia:"), h5("* (aggiornata all'ultimo dato disponibile, ore 18:00 del ", day(ultimaData), "Marzo 2020)"))
    })
    
    output$summary <- reactive({
        attualmente_positivi_ultimo <- dataItaliaInput()$totale_attualmente_positivi[length(dataItaliaInput()$totale_attualmente_positivi)]
        attualmente_positivi_penultimo <- dataItaliaInput()$totale_attualmente_positivi[length(dataItaliaInput()$totale_attualmente_positivi) - 1]
        delta_attualmente_positivi <- diff(c(attualmente_positivi_penultimo, attualmente_positivi_ultimo))
        
        deceduti_ultimo <- dataItaliaInput()$deceduti[length(dataItaliaInput()$deceduti)]
        deceduti_penultimo <- dataItaliaInput()$deceduti[length(dataItaliaInput()$deceduti) - 1]
        delta_deceduti <- diff(c(deceduti_penultimo, deceduti_ultimo))
        
        dimessi_guariti_ultimo <- dataItaliaInput()$dimessi_guariti[length(dataItaliaInput()$dimessi_guariti)]
        dimessi_guariti_penultimo <- dataItaliaInput()$dimessi_guariti[length(dataItaliaInput()$dimessi_guariti) - 1] 
        delta_dimessi_guariti <- diff(c(dimessi_guariti_penultimo, dimessi_guariti_ultimo))
        
        totale_casi_ultimo <- dataItaliaInput()$totale_casi[length(dataItaliaInput()$totale_casi)]
        totale_casi_penultimo <- dataItaliaInput()$totale_casi[length(dataItaliaInput()$totale_casi) - 1]
        delta_totale_casi <- diff(c(totale_casi_penultimo, totale_casi_ultimo))
        
        delta_dimessiguariti_attualmentepositivi <- diff(c(dimessi_guariti_penultimo/attualmente_positivi_penultimo, dimessi_guariti_ultimo/attualmente_positivi_ultimo))
        paste0("<ul>",
              "<li><b>Pazienti attualmente positivi: </b>", attualmente_positivi_ultimo, " (", 
              if(delta_attualmente_positivi > 0) "+" else "-", round(abs(delta_attualmente_positivi/attualmente_positivi_penultimo) * 100, 2), "% rispetto al giorno precedente, ", round(attualmente_positivi_ultimo/totale_casi_ultimo * 100, 2) , "% sul totale dei casi positivi registrati)",
              "</li>",
              "<li><b>Pazienti deceduti: </b>", deceduti_ultimo, " (", 
              if(delta_deceduti > 0) "+" else "-", round(abs(delta_deceduti/deceduti_penultimo) * 100, 2), "% rispetto al giorno precedente, ", round(deceduti_ultimo/totale_casi_ultimo * 100, 2), "% sul totale dei casi positivi registrati)",
              "</li>",
              "<li><b>Pazienti dimessi o guariti: </b>", dimessi_guariti_ultimo, " (", 
              if(delta_dimessi_guariti > 0) "+" else "-", round(abs(delta_dimessi_guariti/dimessi_guariti_penultimo) * 100, 2), "% rispetto al giorno precedente, ", round(dimessi_guariti_ultimo/totale_casi_ultimo * 100, 2), "% sul totale dei casi positivi registrati)",
              "</li>",
              "<li><b>Casi positivi totali: </b>", totale_casi_ultimo, " (", 
              if(delta_totale_casi > 0) "+" else "-", round(abs(delta_totale_casi/totale_casi_penultimo) * 100, 2), "% rispetto al giorno precedente)",
              "</li>",
              "</ul>
              <!-- AddToAny BEGIN -->
                <div class='a2a_kit a2a_kit_size_32 a2a_default_style' data-a2a-url='https://achab94.shinyapps.io/covid-19/'>
                <a class='a2a_dd' href='https://www.addtoany.com/share'></a>
                <a class='a2a_button_facebook'></a>
                <a class='a2a_button_whatsapp'></a>
                <a class='a2a_button_twitter'></a>
                <a class='a2a_button_email'></a>
                </div>
                <script>
                    var a2a_config = a2a_config || {};
                        a2a_config.locale = 'it';
                </script>
                <script async src='https://static.addtoany.com/menu/page.js'></script>
            <!-- AddToAny END -->"
        )
    })
    
    output$scatterMonitoraggioITA <- renderPlot({
        ggplot(dataItaliaInput(), aes(data, totale_attualmente_positivi)) +
            geom_line(aes(color="Attualmente positivi"), size=1) +
            geom_point(aes(color="Attualmente positivi"), size=2) +
            geom_line(aes(data, dimessi_guariti, color="Dimessi/guariti"), size=1) +
            geom_point(aes(data, dimessi_guariti, color="Dimessi/guariti"), size=2) +
            geom_line(aes(data, deceduti, color="Deceduti"), size=1) +
            geom_point(aes(data, deceduti, color="Deceduti"), size=2) +
            geom_line(aes(data, totale_casi, color="Totale casi positivi"), size=1) +
            geom_point(aes(data, totale_casi, color="Totale casi positivi"), size=2) +
            scale_color_manual(values = c("Attualmente positivi" = "red3", 
                                          "Dimessi/guariti" = "dodgerblue3",
                                          "Deceduti" = "green4",
                                          "Totale casi positivi" = "black")) +
            theme_minimal() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Monitoraggio complessivo COVID-19 (dato cumulato)") +
            theme(axis.text.x = element_text(angle=40, hjust=1),
                  legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=11),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm")) 
    })
    
    
    output$scatterRapportoDimessiGuaritiTotaleAttualmentePositiviITA <- renderPlot({
        ggplot(dataItaliaInput(), aes(data, dimessi_guariti/totale_attualmente_positivi)) +
            geom_point(size=3) +
            geom_line() +
            geom_smooth(method = 'loess', se=T) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Rapporto tra il numero di pazienti attualmente dimessi o guariti \ned il numero di attualmente positivi (dato cumulato)") +
            theme(axis.text.x = element_text(angle=40, hjust=1))
    })
    
    output$barPlotDeltaPercentualiITA <- renderPlot({
        myItalia <- dataItaliaInput()
        delayDays <- 7
        myItaliaRelDiffs <- data.frame(data=as.Date(tail(myItalia$data, delayDays)),
                                       AttualmentePositivi = tail(diff(myItalia$totale_attualmente_positivi)/myItalia$totale_attualmente_positivi[-length(myItalia$totale_attualmente_positivi)], delayDays),
                                       Deceduti = tail(diff(myItalia$deceduti)/myItalia$deceduti[-length(myItalia$deceduti)], delayDays),
                                       DimessiGuariti = tail(diff(myItalia$dimessi_guariti)/myItalia$dimessi_guariti[-length(myItalia$dimessi_guariti)], delayDays),
                                       TerapiaIntensiva = tail(diff(myItalia$terapia_intensiva)/myItalia$terapia_intensiva[-length(myItalia$terapia_intensiva)], delayDays))

        myItaliaRelDiffsReshaped <- melt(myItaliaRelDiffs, id.vars = "data", measure.vars = c("AttualmentePositivi",
                                                                                              "Deceduti",
                                                                                              "DimessiGuariti",
                                                                                              "TerapiaIntensiva"), variable.name = "Dimensione")
        ggplot(myItaliaRelDiffsReshaped, aes(x=data, y=value)) + 
            geom_bar(stat="identity", position="dodge", width=0.5) +
            #geom_point(size=2) + geom_line() + 
            #scale_y_continuous(labels = scales::percent_format(accuracy = 1),
            scale_y_continuous(labels = function(x) paste0("+", x*100, "%"),
                               breaks= pretty_breaks()) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            theme(legend.position="none") +
            theme_bw() +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `AttualmentePositivi` = "Attualmente positivi",
                           `Deceduti` = "Deceduti",
                           `DimessiGuariti` = "Dimessi o guariti",
                           `TerapiaIntensiva` = "Trattati in terapia intensiva"
                       ))) +
            labs(x="", y="", title=paste("Variazione % rispetto al giorno precedente (ultimi", delayDays, "giorni)"))
    })
    
    output$scatterDimessiGuaritiITA <- renderPlot({
        ggplot(dataItaliaInput(), aes(data, diff_dimessi_guariti)) +
            geom_point(size=3) +
            geom_line() +
            geom_smooth(method = 'loess', se=T) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Pazienti attualmente dimessi o guariti (dato giornaliero)") +
            theme(axis.text.x = element_text(angle=40, hjust=1))
    })
    
    output$scatterNuoviAttualmentePositiviITA <- renderPlot({
        ggplot(dataItaliaInput(), aes(data, nuovi_attualmente_positivi)) +
            geom_point(size=3) +
            geom_line() +
            geom_smooth(method = 'loess', se=T) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Pazienti positivi al COVID-19 (dato giornaliero)") +
            theme(axis.text.x = element_text(angle=40, hjust=1))
    })
    
    output$scatterDecedutiITA <- renderPlot({
        ggplot(dataItaliaInput(), aes(x=data, y=diff_deceduti)) +
            geom_point(size=3) +
            geom_line() +
            geom_smooth(method = 'loess', se=T) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Pazienti in cura deceduti (dato giornaliero)") +
            theme(axis.text.x = element_text(angle=40, hjust=1))
    })
    
    output$scatterRegionale <- renderPlot({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)
        
        if(input$giornalieroSiNo){
            ### daily visualization
            if(input$selezionaVariabileRegioni == "Pazienti attualmente positivi") plotVar <- "nuovi_attualmente_positivi"
            if(input$selezionaVariabileRegioni == "Pazienti deceduti") plotVar <- "diff_deceduti"
            if(input$selezionaVariabileRegioni == "Pazienti dimessi/guariti") plotVar <- "diff_dimessi_guariti"
            if(input$selezionaVariabileRegioni == "Pazienti ricoverati con sintomi") plotVar <- "diff_ricoverati_con_sintomi"
            if(input$selezionaVariabileRegioni == "Pazienti in terapia intensiva") plotVar <- "diff_terapia_intensiva"
            if(input$selezionaVariabileRegioni == "Pazienti in isolamento domiciliare") plotVar <- "diff_isolamento_domiciliare"
        } else {
            ### global visualization
            if(input$selezionaVariabileRegioni == "Pazienti attualmente positivi") plotVar <- "totale_attualmente_positivi"
            if(input$selezionaVariabileRegioni == "Pazienti deceduti") plotVar <- "deceduti"
            if(input$selezionaVariabileRegioni == "Pazienti dimessi/guariti") plotVar <- "dimessi_guariti"
            if(input$selezionaVariabileRegioni == "Pazienti ricoverati con sintomi") plotVar <- "ricoverati_con_sintomi"
            if(input$selezionaVariabileRegioni == "Pazienti in terapia intensiva") plotVar <- "terapia_intensiva"
            if(input$selezionaVariabileRegioni == "Pazienti in isolamento domiciliare") plotVar <- "isolamento_domiciliare"
        }
        
        myDataRegionVariable <- myDataRegion[, c("data", "Regione", plotVar)]
        colnames(myDataRegionVariable) <- c("data", "Regione", "plotVar")

        ggplot(myDataRegionVariable, aes(x = data, y = plotVar, color = Regione)) +
            geom_point(size=3) +
            geom_line(size=1) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title=paste0(input$selezionaVariabileRegioni, " (dato ",
                                          if(input$giornalieroSiNo) "giornaliero)" else "cumulato)")) +
            theme(axis.text.x = element_text(angle=40, hjust=1),
                  legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")) +
            scale_color_brewer(palette="Set1")
    })
    
    output$barPlotRegionale <- renderPlot({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)
        myDataRegionMelted <- melt(myDataRegion, id.vars=c("data", "Regione"), 
                                   measure.vars=c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare"),
                                   variable.name="Trattamento")
        ggplot(subset(myDataRegionMelted, data==as.character(input$selezionaGiornoRegio)), aes(fill=Trattamento, x=value, y=Regione)) +
            geom_bar(position="stack", stat="identity") +
            theme_bw() +
            labs(x="Totale pazienti attualmente trattati", y="", title=paste0("Tipologia di trattamento sanitario dei contagiati (", format(as.Date(input$selezionaGiornoRegio), "%d/%m/%Y"), ")")) +
            scale_fill_brewer(breaks=c("ricoverati_con_sintomi", "terapia_intensiva", "isolamento_domiciliare"),
                              labels=c("Ricoverati con sintomi", "Terapia intensiva", "Isolamento domiciliare"), 
                              palette="Set2") +
            scale_x_continuous(breaks= pretty_breaks()) + 
            theme(legend.position = "bottom",
                  legend.text=element_text(size=11),
                  legend.title=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm")) 
    })
    
    output$barPlotDeltaPercentualiRegionale <- renderPlot({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)
        
        delayDays <- 7
        myRegioniRelDiffs <- myDataRegion %>%
            group_by(Regione) %>%
            mutate(AttualmentePositivi = diff(c(0, totale_attualmente_positivi))/c(1, totale_attualmente_positivi[-n()]),
                   Deceduti = diff(c(0, deceduti))/c(1, deceduti[-n()]),
                   DimessiGuariti = diff(c(0, dimessi_guariti))/c(1, dimessi_guariti[-n()]),
                   TerapiaIntensiva = diff(c(0, terapia_intensiva))/c(1, terapia_intensiva[-n()]))
        
        myRegioniRelDiffs <- myRegioniRelDiffs %>%
            group_by(Regione) %>%
            slice(tail(row_number(), delayDays))
        
        myRegioniRelDiffsReshaped <- melt(myRegioniRelDiffs, id.vars = c("data", "Regione"), measure.vars = c("AttualmentePositivi",
                                                                                                              "Deceduti",
                                                                                                              "DimessiGuariti",
                                                                                                              "TerapiaIntensiva"), variable.name = "Dimensione")
        ggplot(myRegioniRelDiffsReshaped, aes(x=data, y=value, fill=Regione)) + 
            geom_bar(stat="identity", position="dodge", width=0.5) +
            #geom_point(size=2) + geom_line() + 
            scale_y_continuous(labels = function(x) paste0("+", x*100, "%"),
                               breaks= pretty_breaks()) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_fill_brewer(palette="Set1") +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")) +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `AttualmentePositivi` = "Attualmente positivi",
                           `Deceduti` = "Deceduti",
                           `DimessiGuariti` = "Dimessi o guariti",
                           `TerapiaIntensiva` = "Trattati in terapia intensiva"
                       ))) +
            labs(x="", y="", title=paste("Variazione % rispetto al giorno precedente (ultimi", delayDays, "giorni)"))
        
    })
    
    output$scatterProvinciale <- renderPlot({
        myDataProvince <- subset(dataProvinceInput(), Provincia %in% input$selezionaProvincie)
        
        ggplot(myDataProvince, aes(x = data, y = totale_casi, color = Provincia)) +
            geom_point(size=3) +
            geom_line(size=1) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) + 
            labs(x="", y="", title="Totale casi di COVID-19 registrati (dato cumulato)") +
            theme(axis.text.x = element_text(angle=40, hjust=1),
                  legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")) +
            scale_color_brewer(palette="Set1")
    })
    
    output$barPlotProvinciale <- renderPlot({
        myDataProvinceRegioni <- subset(dataProvinceInput(), Regione %in% as.character(unique(dataProvinceInput()$Regione[dataProvinceInput()$Provincia %in% input$selezionaProvincie])))
        
        ggplot(subset(myDataProvinceRegioni, data==as.character(input$selezionaGiornoProv)), aes(x=totale_casi, y=Provincia)) +
            geom_bar(position="dodge", stat="identity", width = 0.75) +
            theme_bw() +
            labs(x=paste0("Totale casi di COVID-19 registrati (", format(as.Date(input$selezionaGiornoProv), "%d/%m/%Y"), ")"), y="", title=paste0("Comparazione regionale delle province selezionate")) +
            scale_x_continuous(breaks= pretty_breaks()) +
            scale_fill_brewer(palette="Set2") +
            theme(legend.position = "none") +
            facet_grid(rows=vars(Regione), scales="free_y")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
