library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(scales)
library(lubridate)
library(shinyWidgets)
library(reshape2)
library(rmarkdown)
library(ggiraph)
library(DT)

# instructions for updating
# library(rsconnect)
# rsconnect::deployApp("Dropbox/COVID-19/COVID-19/")

mydati_comune <- read.csv("comuni_settimana.csv", header = T)
lista_comuni <- as.character(unique(mydati_comune$NOME_COMUNE))

# Definition of the User Interface ----------------------------------------

ui <- navbarPage(theme = shinytheme("flatly"),
                 title = "Monitoring COVID-19 evolution in Italy",

                 tabPanel("National evolution",
                          div(style="margin-top:-3.5em",
                              fluidRow(
                                  column(width=10,
                                     htmlOutput("headerCruscotto"),
                                     htmlOutput("summary"),
                                     style="padding:15px;")
                                  )
                              ),
                          
                          fluidPage(
                              tags$head(includeHTML(("google-analytics.html")),
                                        tags$style(type="text/css", "text {font-family: arial, helvetica}")),
                              fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                                                   ggiraphOutput("scatterMonitoraggioITA"),
                                                   ggiraphOutput("datiGiornalieriITA"),
                                                   ggiraphOutput("barPlotDeltaPercentualiITA"))),
                              fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                                                   ggiraphOutput("scatterRapportoTotalePositiviTamponi"),
                                                   ggiraphOutput("evoluzioneTassi"),
                                                   ggiraphOutput("barPlotTrattamento")))
                          )
                 ),
                 tabPanel("Regional evolution",
                          sidebarLayout(
                              sidebarPanel(
                                  pickerInput(
                                      inputId = "selezionaRegioni",
                                      label = "Select the region(s) of interest (max 4)",
                                      choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                                                          "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto"),
                                                     "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                                     "Sourhern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                                                         "Campania", "Molise", "Puglia"),
                                                     "Insular" = list("Sardegna", "Sicilia")),
                                      selected = c("Lombardia", "Emilia-Romagna", "Veneto"),
                                      options = list(
                                          `actions-box` = FALSE,
                                          header = "Regions",
                                          `max-options` = 4,
                                          `max-options-text` = "Select 4 regions at maximum"
                                      ),
                                      multiple = TRUE
                                  ),

                                  helpText("Trentino Alto Adige does not communicate the regional data, this is why it does not appear among the list."),

                                  radioButtons("selezionaVariabileRegioni",
                                               label = "Select the variable of interest",
                                               choices = list("Total positive cases",
                                                              "Currently positive patients",
                                                              "Deceased patients",
                                                              "Discharged/healed patients",
                                                              "Hospitalized patients with symptoms",
                                                              "Intensive care patients",
                                                              "Home isolation patients",
                                                              "Administered swabs",
                                                              "Rate between positives and swabs made"),
                                               selected = "Total positive cases"
                                  ),

                                  materialSwitch(inputId = "giornalieroSiNoRegione",
                                                 label = "View the daily data",
                                                 status = "danger"),

                                  helpText("The graph on the left shows by default the combined count of currently infected patients. If you want to view
                                           the daily trend (how many new cases have been recorded each day), select the button."),

                                  uiOutput("selezionaGiornoRegio"),

                                  helpText("The chart on the right shows by default the current subdivision of the type of treatment for currently infected patients.
                                           If you want to view the evolution of the day-to-day breakdown, drag the button.")

                              ),

                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                                        ),
                                verticalLayout(
                                  fluidRow(
                                      splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("scatterRegionale"), ggiraphOutput("barPlotDeltaPercentualiRegionale"))
                                  ),
                                  fluidRow(
                                      splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("evoluzioneTassiRegionale"), ggiraphOutput("barPlotRegionale"))
                                  )
                                )
                              )
                          )
                 ),
                 tabPanel("Provincial evolution",
                          sidebarLayout(
                              sidebarPanel(
                                  pickerInput(
                                      inputId = "selezionaProvincie",
                                      label = "Select the province(s) of interest (max 4)",
                                      choices = list("Abruzzo" = list("Chieti", "L'Aquila", "Pescara", "Teramo"),
                                                     "Basilicata" = list("Matera", "Potenza"),
                                                     "Calabria" = list("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia"),
                                                     "Campania" = list("Avellino", "Benevento", "Caserta", "Napoli", "Salerno"),
                                                     "Emilia-Romagna" = list("Bologna", "Ferrara", "Forlì-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini"),
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
                                                     "Trentino-Alto Adige" = list("Bolzano", "Trento"),
                                                     "Umbria" = list("Perugia", "Terni"),
                                                     "Valle d'Aosta" = list("Aosta"),
                                                     "Veneto" = list("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")),
                                      selected = c("Bergamo", "Brescia", "Milano"),
                                      options = list(
                                          `actions-box` = FALSE,
                                          header = "Province",
                                          `max-options` = 5,
                                          `max-options-text` = "Select 5 regions at maximum"
                                      ),
                                      multiple = TRUE
                                  ),

                                  materialSwitch(inputId = "giornalieroSiNoProvincia",
                                                 label = "View the daily data",
                                                 status = "danger"),

                                  helpText("The graph on the left shows by default the combined count of the total of positives. If you want to view
                                           the daily trend (how many new positive cases have been recorded each day), select the button."),

                                  uiOutput("selezionaGiornoProv"),

                                  helpText("The chart on the right shows by default today's regional comparison of the number of total cases recorded.
                                           If you want to view the evolution day by day, drag the button.")

                              ),

                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              verticalLayout(
                                  fluidRow(
                                      splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("scatterProvinciale"), ggiraphOutput("barPlotProvinciale"))
                                  ),
                                  h3("Relative increase (%) in the number of positive cases between yesterday and today"),
                                  fluidRow(
                                      DTOutput("dfProvinceIncrementoMaggiore")
                                  )
                              )
                              )
                          )
                 ),
                 tabPanel("Deaths in municipalities",
                          sidebarLayout(
                              sidebarPanel(
                                  pickerInput(
                                      inputId = "selezionaComune",
                                      label = "Select the municipality of interest",
                                      choices = lista_comuni,
                                      selected = c("Bergamo"),
                                      options = list(
                                          `actions-box` = TRUE,
                                          `live-search` = TRUE,
                                          header = "Municipalities",
                                          `max-options` = 1,
                                          `max-options-text` = "Select the municipality"
                                      ),
                                      multiple = TRUE
                                  ),
                                  helpText("Notice not all the municipalities are available. This issue regards the structure of the original dataset provided by ISTAT. See https://www.istat.it/it/archivio/240401 for further informations.")
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              ggiraphOutput("graficoDecessiComunali")
                              )
                          )
                 ),
                 tabPanel("Donations", div(style="margin-top:-2.5em", includeMarkdown("Donazioni.md"))),
                 tabPanel("Credits and informations", div(style="margin-top:-2.5em", includeMarkdown("Info.md")))
)


server <- function(input, output) {
    dataProvinceInput <- reactive({
        storicoProvince <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv", stringsAsFactors = FALSE)
        storicoProvince <- subset(storicoProvince, denominazione_provincia!="In fase di definizione/aggiornamento")
        storicoProvince$denominazione_regione[storicoProvince$denominazione_regione %in% list("P.A. Trento", "P.A. Bolzano")] <- "Trentino-Alto Adige"

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
                                totale_attualmente_positivi = storicoRegioni$totale_positivi,
                                nuovi_attualmente_positivi = storicoRegioni$variazione_totale_positivi,
                                deceduti = storicoRegioni$deceduti,
                                dimessi_guariti = storicoRegioni$dimessi_guariti,
                                totale_casi = storicoRegioni$totale_casi,
                                ricoverati_con_sintomi = storicoRegioni$ricoverati_con_sintomi,
                                terapia_intensiva = storicoRegioni$terapia_intensiva,
                                isolamento_domiciliare = storicoRegioni$isolamento_domiciliare,
                                tamponi = storicoRegioni$tamponi,
                                x = (as.numeric(as.Date(storicoRegioni$data)) - min(as.numeric(as.Date(storicoRegioni$data))) + 1))
        myRegioni <- myRegioni %>%
            group_by(Regione) %>%
            mutate(diff_deceduti = diff(c(0, deceduti)),
                   diff_dimessi_guariti = diff(c(0, dimessi_guariti)),
                   diff_ricoverati_con_sintomi = diff(c(0, ricoverati_con_sintomi)),
                   diff_terapia_intensiva = diff(c(0, terapia_intensiva)),
                   diff_isolamento_domiciliare = diff(c(0, isolamento_domiciliare)),
                   diff_totale_casi = diff(c(0, totale_casi)),
                   diff_tamponi = diff(c(0, tamponi)),
                   totalratetamponi = round(totale_casi/tamponi, 4),
                   dailyratetamponi = round(diff_totale_casi/diff_tamponi, 4))
        myRegioni
    })

    dataItaliaInput <- reactive({
        storicoItalia <- read.csv(file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", stringsAsFactors = FALSE)

        myItalia <- data.frame(data=as.Date(storicoItalia$data),
                               totale_attualmente_positivi = storicoItalia$totale_positivi,
                               nuovi_attualmente_positivi = storicoItalia$variazione_totale_positivi,
                               deceduti = storicoItalia$deceduti,
                               diff_deceduti = diff(c(0, storicoItalia$deceduti)),
                               dimessi_guariti = storicoItalia$dimessi_guariti,
                               diff_dimessi_guariti = diff(c(0, storicoItalia$dimessi_guariti)),
                               totale_casi = storicoItalia$totale_casi,
                               diff_totale_casi = diff(c(0, storicoItalia$totale_casi)),
                               ricoverati_con_sintomi = storicoItalia$ricoverati_con_sintomi,
                               diff_ricoverati_con_sintomi = diff(c(0, storicoItalia$ricoverati_con_sintomi)),
                               terapia_intensiva = storicoItalia$terapia_intensiva,
                               diff_terapia_intensiva = diff(c(0, storicoItalia$terapia_intensiva)),
                               isolamento_domiciliare = storicoItalia$isolamento_domiciliare,
                               diff_isolamento_domiciliare = diff(c(0, storicoItalia$isolamento_domiciliare)),
                               tamponi = storicoItalia$tamponi,
                               diff_tamponi = diff(c(0, storicoItalia$tamponi)),
                               x = (as.numeric(as.Date(storicoItalia$data)) - min(as.numeric(as.Date(storicoItalia$data))) + 1))
        myItalia
    })

    output$selezionaGiornoRegio <- renderUI({
        sliderTextInput(
            inputId = "selezionaGiornoRegio",
            label = "Select the day of interest",
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
            label = "Select the day of interest",
            grid = FALSE,
            force_edges = TRUE,
            width = 500,
            selected = as.Date(dataProvinceInput()$data[length(dataProvinceInput()$data)]),
            choices = seq(as.Date("2020-02-24"), as.Date(dataProvinceInput()$data[length(dataProvinceInput()$data)]), 1)
        )
    })

    output$headerCruscotto <- reactive({
        ultimaData <- dataItaliaInput()$data[length(dataItaliaInput()$data)]
        paste0(h1("The current* situation of the CoVid-19 epidemic in Italy:"), h5("* (updated to the latest available data, dating back to 18pm on", day(ultimaData), "April 2020)"))
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
              "<li><b>Currently positive patients: </b>", attualmente_positivi_ultimo, " (",
              if(delta_attualmente_positivi > 0) "+" else "-", round(abs(delta_attualmente_positivi/attualmente_positivi_penultimo) * 100, 2), "% over the previous day, ", round(attualmente_positivi_ultimo/totale_casi_ultimo * 100, 2) , "% of the total positive cases)",
              "</li>",
              "<li><b>Deceased patients: </b>", deceduti_ultimo, " (",
              if(delta_deceduti > 0) "+" else "-", round(abs(delta_deceduti/deceduti_penultimo) * 100, 2), "% over the previous day, ", round(deceduti_ultimo/totale_casi_ultimo * 100, 2), "% of the total positive cases)",
              "</li>",
              "<li><b>Discharged/healed patients: </b>", dimessi_guariti_ultimo, " (",
              if(delta_dimessi_guariti > 0) "+" else "-", round(abs(delta_dimessi_guariti/dimessi_guariti_penultimo) * 100, 2), "% over the previous day, ", round(dimessi_guariti_ultimo/totale_casi_ultimo * 100, 2), "% of the total positive cases)",
              "</li>",
              "<li><b>Total positive cases: </b>", totale_casi_ultimo, " (",
              if(delta_totale_casi > 0) "+" else "-", round(abs(delta_totale_casi/totale_casi_penultimo) * 100, 2), "% over the previous day)",
              "</li>",
              "</ul>
              <i>For an optimal consultation of this portal, which allows <b> interaction with the graphics </b>, it is advisable to access from desktop instead of mobile or tablet.</i>
              "
        )
    })

    output$scatterMonitoraggioITA <- renderggiraph({
        myItaliaReshaped <- melt(dataItaliaInput(), id.vars = "data",
                                 measure.vars = c("totale_attualmente_positivi",
                                                  "deceduti",
                                                  "dimessi_guariti",
                                                  "totale_casi"))

        gg_point <- ggplot(myItaliaReshaped) +
            geom_point_interactive(aes(x = data, y = log(value), color = variable, tooltip = value), size=2.25) +
            geom_line(aes(x=data, y=log(value), color=variable), size=1) +
            scale_color_manual(values = c("totale_attualmente_positivi" = "red3",
                                          "dimessi_guariti" = "dodgerblue3",
                                          "deceduti" = "green4",
                                          "totale_casi" = "black"),
                               labels = c("totale_attualmente_positivi" = "Currently positive",
                                          "dimessi_guariti" = "Discharged or healed",
                                          "deceduti" = "Deceased",
                                          "totale_casi" = "Total positive cases")) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) +
            labs(x="", y="", title="Overall monitoring of COVID-19", caption = "total data (logarithmic scale)") +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1.25, unit="cm"))
        ggiraph(ggobj = gg_point)
    })

    output$barPlotDeltaPercentualiITA <- renderggiraph({
        myItalia <- dataItaliaInput()
        delayDays <- 14
        myItaliaRelDiffs <- data.frame(data=as.Date(tail(myItalia$data, delayDays)),
                                       TotalePositivi = tail(diff(myItalia$totale_casi)/myItalia$totale_casi[-length(myItalia$totale_casi)], delayDays),
                                       Deceduti = tail(diff(myItalia$deceduti)/myItalia$deceduti[-length(myItalia$deceduti)], delayDays),
                                       DimessiGuariti = tail(diff(myItalia$dimessi_guariti)/myItalia$dimessi_guariti[-length(myItalia$dimessi_guariti)], delayDays),
                                       Tamponi = tail(diff(myItalia$tamponi)/myItalia$tamponi[-length(myItalia$tamponi)], delayDays))

        myItaliaRelDiffsReshaped <- melt(myItaliaRelDiffs, id.vars = "data", measure.vars = c("TotalePositivi",
                                                                                              "Deceduti",
                                                                                              "DimessiGuariti",
                                                                                              "Tamponi"), variable.name = "Dimensione")
        gg_bar <- ggplot(myItaliaRelDiffsReshaped, aes(x=data, y=value)) +
            geom_bar_interactive(aes(tooltip=percent(value, 0.01)), stat="identity", position="dodge", width=0.5) +
            scale_y_continuous(labels = function(x) paste0("+", x*100, "%"),
                               breaks= pretty_breaks()) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            theme_bw() +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `TotalePositivi` = "Total positive cases",
                           `Deceduti` = "Deceased",
                           `DimessiGuariti` = "Discharged or healed",
                           `Tamponi` = "Swabs"
                       ))) +
            labs(x="", y="", title=paste("% Variation from the previous day"), caption=paste("daily data (last", delayDays, "days)")) +
            theme(legend.position="none",
                  axis.title.x=element_blank())
        ggiraph(ggobj = gg_bar)
    })

    output$datiGiornalieriITA <- renderggiraph({
        myItalia <- dataItaliaInput()

        myItaliaDiffs <- data.frame(data = myItalia$data,
                                    diff_totale_casi = myItalia$diff_totale_casi,
                                    diff_deceduti = myItalia$diff_deceduti,
                                    diff_dimessi_guariti = myItalia$diff_dimessi_guariti,
                                    diff_tamponi = myItalia$diff_tamponi)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_point <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value)) +
            geom_line() + geom_smooth(size=0.75) + geom_point_interactive(aes(tooltip=round(value, 2)), size=2.25) +
            scale_y_continuous(breaks = pretty_breaks()) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            facet_wrap(~Variabile, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `diff_totale_casi` = "Total positive cases",
                           `diff_deceduti` = "Deceased",
                           `diff_dimessi_guariti` = "Discharged or healed",
                           `diff_tamponi` = "Swabs"
                       ))) +
            theme_bw() +
            theme(axis.title.x=element_blank()) +
            labs(x="", y="", title=paste("Overall monitoring of COVID-19"), caption="daily data")

        girafe(ggobj = gg_point)
    })

    output$evoluzioneTassi <- renderggiraph({
        myItalia <- dataItaliaInput()

        myItaliaDiffs <- data.frame(data = myItalia$data,
                                    tasso_deceduti = myItalia$deceduti/myItalia$totale_casi,
                                    tasso_dimessi_guariti = myItalia$dimessi_guariti/myItalia$totale_casi)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_point <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value, color=Variabile)) +
            geom_line() + geom_smooth(size=0.75, show.legend = F) +
            geom_point_interactive(aes(tooltip=percent(value, 0.01)), size=3) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks(), labels=percent, limits = c(0, 0.2)) +
            labs(x="", y="", title="Monitoring of healing and mortality rates", caption="total data") +
            theme_bw() +
            scale_color_brewer(breaks=c("tasso_dimessi_guariti", "tasso_deceduti"),
                              labels=c("% discharged/healed out of all positives", "% deceased out of all positives"),
                              palette="Set1") +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe(ggobj = gg_point)

    })

    output$scatterRapportoTotalePositiviTamponi <- renderggiraph({
        gg_point <- ggplot(dataItaliaInput(), aes(data, y=diff_totale_casi/diff_tamponi)) +
            geom_smooth(method = 'loess', se=T) +
            geom_point_interactive(aes(tooltip=percent(diff_totale_casi/diff_tamponi, 0.01)), size=3) +
            geom_line() +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks(), labels=percent) +
            labs(x="", y="", title="Daily rate between new positives and swabs made", caption="daily data") +
            theme(axis.title.x=element_blank())

        girafe(ggobj = gg_point)
    })

    output$barPlotTrattamento <- renderggiraph({
        myItaliaDiffs <- data.frame(data = dataItaliaInput()$data,
                                    ricoverati_con_sintomi = dataItaliaInput()$ricoverati_con_sintomi,
                                    terapia_intensiva = dataItaliaInput()$terapia_intensiva,
                                    isolamento_domiciliare = dataItaliaInput()$isolamento_domiciliare)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_bar <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value, fill=Variabile)) +
            geom_bar_interactive(aes(tooltip=value), stat="identity", position = "stack") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) +
            labs(x="", y="Total currently positive patients", title="Subdivision of the health treatment for the infected", caption="total data") +
            theme_bw() +
            scale_fill_brewer(palette="Set2", labels=c("Hospitalized with symptoms", "Intensive care", "Home isolation")) +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe(ggobj = gg_bar)
    })


    output$scatterRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)

        if(input$giornalieroSiNoRegione){
            ### daily visualization
            if(input$selezionaVariabileRegioni == "Total positive cases") plotVar <- "diff_totale_casi"
            if(input$selezionaVariabileRegioni == "Currently positive patients") plotVar <- "nuovi_attualmente_positivi"
            if(input$selezionaVariabileRegioni == "Deceased patients") plotVar <- "diff_deceduti"
            if(input$selezionaVariabileRegioni == "Discharged/healed patients") plotVar <- "diff_dimessi_guariti"
            if(input$selezionaVariabileRegioni == "Hospitalized patients with symptoms") plotVar <- "diff_ricoverati_con_sintomi"
            if(input$selezionaVariabileRegioni == "Intensive care patients") plotVar <- "diff_terapia_intensiva"
            if(input$selezionaVariabileRegioni == "Home isolation patients") plotVar <- "diff_isolamento_domiciliare"
            if(input$selezionaVariabileRegioni == "Administered swabs") plotVar <- "diff_tamponi"
            if(input$selezionaVariabileRegioni == "Rate between positives and swabs made") plotVar <- "dailyratetamponi"
        } else {
            ### global visualization
            if(input$selezionaVariabileRegioni == "Total positive cases") plotVar <- "totale_casi"
            if(input$selezionaVariabileRegioni == "Currently positive patients") plotVar <- "totale_attualmente_positivi"
            if(input$selezionaVariabileRegioni == "Deceased patients") plotVar <- "deceduti"
            if(input$selezionaVariabileRegioni == "Discharged/healed patients") plotVar <- "dimessi_guariti"
            if(input$selezionaVariabileRegioni == "Hospitalized patients with symptoms") plotVar <- "ricoverati_con_sintomi"
            if(input$selezionaVariabileRegioni == "Intensive care patients") plotVar <- "terapia_intensiva"
            if(input$selezionaVariabileRegioni == "Home isolation patients") plotVar <- "isolamento_domiciliare"
            if(input$selezionaVariabileRegioni == "Administered swabs") plotVar <- "tamponi"
            if(input$selezionaVariabileRegioni == "Rate between positives and swabs made") plotVar <- "totalratetamponi"
        }

        myDataRegionVariable <- myDataRegion[, c("data", "Regione", plotVar)]
        colnames(myDataRegionVariable) <- c("data", "Region", "plotVar")

        gg_point <- ggplot(myDataRegionVariable, aes(x = data, y = plotVar, color = Region)) +
            geom_point_interactive(aes(tooltip=plotVar), size=3.25) +
            geom_line(size=1) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) +
            labs(x="", y="", title=input$selezionaVariabileRegioni, caption = paste0(if(input$giornalieroSiNoRegione) "daily data" else "total data")) +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm"))

        girafe(ggobj = gg_point)
    })

    output$barPlotRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)
        myDataRegionMelted <- melt(myDataRegion, id.vars=c("data", "Regione"),
                                   measure.vars=c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare"),
                                   variable.name="Trattamento")
        gg_bar <- ggplot(subset(myDataRegionMelted, data==as.character(input$selezionaGiornoRegio)), aes(fill=Trattamento, x=value, y=Regione)) +
            geom_bar_interactive(aes(tooltip=value), position="stack", stat="identity") +
            theme_bw() +
            labs(x="Total currently positive patients", y="", title="Subdivision of the health treatment for the infected", caption=paste0("daily data (", format(as.Date(input$selezionaGiornoRegio), "%d/%m/%Y"), ")")) +
            scale_fill_brewer(breaks=c("ricoverati_con_sintomi", "terapia_intensiva", "isolamento_domiciliare"),
                              labels=c("Hospitalized with symptoms", "Intensive care", "Home isolation"),
                              palette="Set2") +
            scale_x_continuous(breaks = pretty_breaks()) +
            theme(legend.position = "top",
                  legend.text=element_text(size=11),
                  legend.title=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1.25, unit="cm"))
        girafe(ggobj = gg_bar)
    })

    output$barPlotDeltaPercentualiRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)

        delayDays <- 7
        myRegioniRelDiffs <- myDataRegion %>%
            group_by(Regione) %>%
            mutate(CasiPositiviTotali = diff(c(0, totale_casi))/c(1, totale_casi[-n()]),
                   Deceduti = diff(c(0, deceduti))/c(1, deceduti[-n()]),
                   DimessiGuariti = diff(c(0, dimessi_guariti))/c(1, dimessi_guariti[-n()]),
                   TerapiaIntensiva = diff(c(0, terapia_intensiva))/c(1, terapia_intensiva[-n()]))

        myRegioniRelDiffs <- myRegioniRelDiffs %>%
            group_by(Regione) %>%
            slice(tail(row_number(), delayDays))

        myRegioniRelDiffsReshaped <- melt(myRegioniRelDiffs, id.vars = c("data", "Regione"), measure.vars = c("CasiPositiviTotali",
                                                                                                              "Deceduti",
                                                                                                              "DimessiGuariti",
                                                                                                              "TerapiaIntensiva"), variable.name = "Dimensione")
        gg_bar <- ggplot(myRegioniRelDiffsReshaped, aes(x=data, y=value, fill=Regione)) +
            geom_bar_interactive(aes(tooltip=percent(value, 0.01)), stat="identity", position="dodge", width=0.5) +
            scale_y_continuous(labels = function(x) paste0("+", x*100, "%"),
                               breaks= pretty_breaks()) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=11),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")) +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `CasiPositiviTotali` = "Total positive cases",
                           `Deceduti` = "Deceased",
                           `DimessiGuariti` = "Discharged or healed",
                           `TerapiaIntensiva` = "Intensive care treated"
                       ))) +
            labs(x="", y="", title=paste("% Variation from the previous day"), caption=paste("daily data (last", delayDays, "days)"))

        girafe(ggobj = gg_bar)
    })

    output$evoluzioneTassiRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)

        myRegioniRates <- myDataRegion %>%
            group_by(Regione) %>%
            mutate(tasso_deceduti = deceduti/totale_casi,
                   tasso_dimessi_guariti = dimessi_guariti/totale_casi)

        myRegioniRatesReshaped <- melt(myRegioniRates, id.vars = list("data", "Regione"), measure.vars = list("tasso_deceduti", "tasso_dimessi_guariti"), variable.name = "Tasso")

        gg_point <- ggplot(myRegioniRatesReshaped, aes(x=data, y=value, color=Tasso)) +
            geom_line() + geom_smooth(size=0.75, show.legend = F) +
            geom_point_interactive(aes(tooltip=percent(value, 0.01)), size=3) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks(), labels=percent) +
            labs(x="", y="", title="Monitoring of healing and mortality rates", caption="total data") +
            theme_bw() +
            scale_color_brewer(breaks=c("tasso_dimessi_guariti", "tasso_deceduti"),
                              labels=c("% discharged/healed out of all positives", "% deceased out of all positives"),
                              palette="Set1") +
            facet_wrap(~Regione, ncol=1, scales="free_y") +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe(ggobj = gg_point)
    })

    output$scatterProvinciale <- renderggiraph({
        myDataProvince <- subset(dataProvinceInput(), Provincia %in% input$selezionaProvincie)

        if(input$giornalieroSiNoProvincia){
            ### daily visualization
            plotVar <- "diff_totale_casi"
        } else {
            ### global visualization
            plotVar <- "totale_casi"
        }

        myDataProvinceVariable <- myDataProvince[, c("data", "Provincia", plotVar)]
        colnames(myDataProvinceVariable) <- c("data", "Provincia", "plotVar")

        gg_point <- ggplot(myDataProvinceVariable, aes(x = data, y = plotVar, color = Provincia)) +
            geom_point_interactive(aes(tooltip=plotVar), size=3) +
            geom_line(size=1) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day") +
            scale_y_continuous(breaks= pretty_breaks()) +
            labs(x="", y="", title="Total positive cases", caption=paste0(if(input$giornalieroSiNoProvincia) "daily data" else "total data")) +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  axis.title.x=element_blank(),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm"))
        girafe(ggobj = gg_point)
    })

    output$barPlotProvinciale <- renderggiraph({
        myDataProvinceRegioni <- subset(dataProvinceInput(), Regione %in% as.character(unique(dataProvinceInput()$Regione[dataProvinceInput()$Provincia %in% input$selezionaProvincie])))

        gg_bar <- ggplot(subset(myDataProvinceRegioni, data==as.character(input$selezionaGiornoProv)), aes(x=totale_casi, y=Provincia)) +
            geom_bar_interactive(aes(tooltip=totale_casi), position="dodge", stat="identity", width = 0.75) +
            theme_bw() +
            labs(x="Total positive cases", y="", title=paste0("Regional comparison of the selected provinces"),
                 caption=paste0("total data (till ", format(as.Date(input$selezionaGiornoProv), "%d/%m/%Y"), ")")) +
            scale_x_continuous(breaks= pretty_breaks()) +
            scale_fill_brewer(palette="Set2") +
            theme(legend.position = "none",
                  axis.title.x=element_blank()) +
            facet_grid(rows=vars(Regione), scales="free_y")

        girafe(ggobj = gg_bar)
    })

    output$dfProvinceIncrementoMaggiore <- renderDT({
        myProvince <- dataProvinceInput() %>%
            group_by(Provincia) %>%
            mutate(relincr_totale_casi = diff(c(0, totale_casi))/c(1, totale_casi[-n()]))

        lastDataProvince <- tail(myProvince, 107)

        df_output <- data.frame(Provincia=as.character(lastDataProvince$Provincia),
                                Regione=as.character(lastDataProvince$Regione),
                                IncrementoGiornaliero=lastDataProvince$relincr_totale_casi,
                                CasiIeri=(lastDataProvince$totale_casi - lastDataProvince$diff_totale_casi),
                                CasiOggi=lastDataProvince$totale_casi)
        df_output <- df_output[order(df_output$IncrementoGiornaliero, decreasing = T),]
        colnames(df_output) <- c("Province", "Region", "Daily increase", "Total positives (yesterday)", "Total positives (today)")
        datatable(df_output, filter = 'bottom', rownames = FALSE,
                  options = list(
                      searching = T,
                      pageLength = 10)) %>% formatPercentage(columns = "Daily increase", digits = 2)
    })
    
    output$graficoDecessiComunali <- renderggiraph({
        dati_comune <- subset(mydati_comune, NOME_COMUNE == input$selezionaComune, 
                              select = c("SETTIMANA", "CLASSE_DI_ETA",
                                         "TOTALE_2015", "TOTALE_2016", "TOTALE_2017",
                                         "TOTALE_2018", "TOTALE_2019", "TOTALE_2020"))
        colnames(dati_comune) <- c("Settimana", "Fascia", "2015", "2016", "2017", "2018", "2019", "2020")
        
        decessi_comune <- melt(dati_comune, id.vars = c("Settimana", "Fascia"), 
                               measure.vars = c("2015", "2016", "2017", "2018", "2019", "2020"), 
                               variable.name = "Year")
        
        morti_comune <- decessi_comune %>% 
            group_by(Settimana, Year) %>% 
            summarise(Morti=sum(value))
        
        morti_comune$NumeroSettimana <- 0
        settimane <- c("01/01-11/01", "12/01-18/01", "19/01-25/01", "26/01-01/02", "02/02-08/02",
                       "09/02-15/02", "16/02-22/02", "23/02-29/02", "01/03-07/03", "08/03-14/03", "15/03-21/03")
        for(i in 1:length(settimane)) morti_comune$NumeroSettimana[morti_comune$Settimana == settimane[i]] <- i
        
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        gg_decessi <- ggplot(morti_comune, aes(x=NumeroSettimana, y=Morti, col=Year)) +
            geom_point_interactive(aes(tooltip=Morti), size=3) + geom_line(size=1) +
            scale_x_continuous(breaks = 1:length(settimane), labels=settimane) +
            scale_colour_manual(values=cbPalette) +
            scale_y_continuous(breaks= pretty_breaks()) +
            labs(title=paste("Deaths in the first 11 weeks of the calendar year in", input$selezionaComune), caption="weekly data") +
            theme_bw() + 
            theme(legend.position = "bottom",
                  axis.text.x=element_text(size=6),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.grid.minor.x = element_blank()) +
            guides(colour = guide_legend(nrow = 1))
        girafe(ggobj = gg_decessi)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
