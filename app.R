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
                              # fluidRow(splitLayout(cellWidths = c("49%", "49%"),
                              #                      ggiraphOutput("scatterMonitoraggioITA"),
                              #                      ggiraphOutput("datiGiornalieriITA"))),
                              # fluidRow(splitLayout(cellWidths = c("49%", "49%"),
                              #                      ggiraphOutput("barPlotDeltaPercentualiITA"),
                              #                      ggiraphOutput("scatterRapportoTotalePositiviTamponi"))),
                              # fluidRow(splitLayout(cellWidths = c("49%", "49%"),
                              #                      ggiraphOutput("evoluzioneTassi"),
                              #                      ggiraphOutput("barPlotTrattamento")))
                              fluidRow(ggiraphOutput("scatterMonitoraggioITA")),
                              fluidRow(ggiraphOutput("datiGiornalieriITA")),
                              fluidRow(ggiraphOutput("barPlotDeltaPercentualiITA")),
                              fluidRow(ggiraphOutput("scatterRapportoTotalePositiviTamponi")),
                              fluidRow(ggiraphOutput("evoluzioneTassi")),
                              fluidRow(ggiraphOutput("barPlotTrattamento"))
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
                                      selected = c("Lombardia", "Piemonte"),
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
                                            # fluidRow(
                                            #     splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("scatterRegionale"), ggiraphOutput("barPlotDeltaPercentualiRegionale"))
                                            # ),
                                            # fluidRow(
                                            #     splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("evoluzioneTassiRegionale"), ggiraphOutput("barPlotRegionale"))
                                            # )
                                            fluidRow(ggiraphOutput("scatterRegionale")),
                                            fluidRow(ggiraphOutput("barPlotDeltaPercentualiRegionale")),
                                            fluidRow(ggiraphOutput("evoluzioneTassiRegionale")),
                                            fluidRow(ggiraphOutput("barPlotRegionale"))
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
                               casi_testati = storicoItalia$casi_testati,
                               diff_casi_testati = diff(c(0,storicoItalia$casi_testati)),
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
        paste0(h1("The current* situation of the CoVid-19 epidemic in Italy:"), h5("* (updated to the latest available data, dating back to 18pm on", day(ultimaData), "August 2020)"))
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

        casi_testati_ultimo <- dataItaliaInput()$casi_testati[length(dataItaliaInput()$casi_testati)]
        casi_testati_penultimo <- dataItaliaInput()$casi_testati[length(dataItaliaInput()$casi_testati) - 1]
        delta_casi_testati <- diff(c(casi_testati_penultimo, casi_testati_ultimo))

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
              if(delta_totale_casi > 0) "+" else "-", round(abs(delta_totale_casi/totale_casi_penultimo) * 100, 2), "% over the previous day, ", round(totale_casi_ultimo/casi_testati_ultimo * 100, 2), "% of the total tested cases)",
              "</li>",
              "<li><b>Total tested cases: </b>", casi_testati_ultimo, " (",
              if(delta_casi_testati > 0) "+" else "-", round(abs(delta_casi_testati/casi_testati_penultimo) * 100, 2), "% over the previous day. Notice this number is different from the total number of administered swabs)",
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
                                                  "totale_casi",
                                                  "terapia_intensiva"))

        gg_point <- ggplot(myItaliaReshaped) +
            geom_point_interactive(aes(x = data, y = value, color = variable, tooltip = value), size=1.5) +
            geom_line(aes(x=data, y=value, color=variable), size=0.5) +
            scale_color_manual(values = c("totale_attualmente_positivi" = "red3",
                                          "dimessi_guariti" = "dodgerblue3",
                                          "deceduti" = "green4",
                                          "totale_casi" = "black",
                                          "terapia_intensiva" = "pink4"),
                               labels = c("totale_attualmente_positivi" = "Currently positive",
                                          "dimessi_guariti" = "Discharged or healed",
                                          "deceduti" = "Deceased",
                                          "totale_casi" = "Total positive cases",
                                          "terapia_intensiva" = "Intensive care treated")) +
            theme_bw() + scale_y_continuous(trans = log10_trans(),
                                            breaks = trans_breaks("log10", function(x) 10^x),
                                            labels = trans_format("log10", math_format(10^.x))) +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            labs(x="", y="", title="Overall monitoring of COVID-19", caption = "total data (Log10 scale)") +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"))
        girafe_options(girafe(ggobj = gg_point, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
    })

    output$barPlotDeltaPercentualiITA <- renderggiraph({
        myItalia <- dataItaliaInput()
        delayDays <- 14
        myItaliaRelDiffs <- data.frame(data=as.Date(tail(myItalia$data, delayDays)),
                                       TotalePositivi = tail(diff(myItalia$totale_casi)/myItalia$totale_casi[-length(myItalia$totale_casi)], delayDays),
                                       Deceduti = tail(diff(myItalia$deceduti)/myItalia$deceduti[-length(myItalia$deceduti)], delayDays),
                                       DimessiGuariti = tail(diff(myItalia$dimessi_guariti)/myItalia$dimessi_guariti[-length(myItalia$dimessi_guariti)], delayDays),
                                       Tamponi = tail(diff(myItalia$tamponi)/myItalia$tamponi[-length(myItalia$tamponi)], delayDays),
                                       TerapiaIntensiva = tail(diff(myItalia$terapia_intensiva)/myItalia$terapia_intensiva[-length(myItalia$terapia_intensiva)], delayDays))

        myItaliaRelDiffsReshaped <- melt(myItaliaRelDiffs, id.vars = "data", measure.vars = c("TotalePositivi",
                                                                                              "Deceduti",
                                                                                              "DimessiGuariti",
                                                                                              "Tamponi",
                                                                                              "TerapiaIntensiva"), variable.name = "Dimensione")
        gg_bar <- ggplot(myItaliaRelDiffsReshaped, aes(x=data, y=value)) +
            geom_bar_interactive(aes(tooltip=percent(value, 0.01)), stat="identity", position="dodge", width=0.5) +
            scale_y_continuous(labels = scales::percent) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            theme_bw() +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `TotalePositivi` = "Total positive cases",
                           `Deceduti` = "Deceased",
                           `DimessiGuariti` = "Discharged or healed",
                           `Tamponi` = "Swabs",
                           `TerapiaIntensiva` = "Intensive care treated"
                       ))) +
            labs(x="", y="", title=paste("Relative variation from the previous day"), caption=paste("daily data (last", delayDays, "days)")) +
            theme(legend.position="none",
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank())
        girafe_options(girafe(ggobj = gg_bar, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
    })

    output$datiGiornalieriITA <- renderggiraph({
        myItalia <- dataItaliaInput()

        myItaliaDiffs <- data.frame(data = myItalia$data,
                                    diff_totale_casi = myItalia$diff_totale_casi,
                                    diff_deceduti = myItalia$diff_deceduti,
                                    diff_dimessi_guariti = myItalia$diff_dimessi_guariti,
                                    diff_tamponi = myItalia$diff_tamponi,
                                    diff_terapia_intensiva = myItalia$diff_terapia_intensiva)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_point <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value)) +
            geom_line(size=0.75) + geom_smooth(size=0.5) + geom_point_interactive(aes(tooltip=round(value, 2)), size=1.5) +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            facet_wrap(~Variabile, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `diff_totale_casi` = "Total positive cases",
                           `diff_deceduti` = "Deceased",
                           `diff_dimessi_guariti` = "Discharged or healed",
                           `diff_tamponi` = "Swabs",
                           `diff_terapia_intensiva` = "Intensive care treated"
                       ))) +
            theme_bw() +
            theme(axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank()) +
            labs(x="", y="", title=paste("Overall monitoring of COVID-19"), caption="daily data")

        girafe_options(girafe(ggobj = gg_point, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
    })

    output$evoluzioneTassi <- renderggiraph({
        myItalia <- dataItaliaInput()

        myItaliaDiffs <- data.frame(data = myItalia$data,
                                    tasso_deceduti = myItalia$deceduti/myItalia$totale_casi,
                                    tasso_dimessi_guariti = myItalia$dimessi_guariti/myItalia$totale_casi)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_point <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value, color=Variabile)) +
            geom_line(size=0.75) + geom_smooth(size=0.5, show.legend = F) +
            geom_point_interactive(aes(tooltip=percent(value, 0.01)), size=1.5) +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            scale_y_continuous(labels=percent) +
            labs(x="", y="", title="Monitoring of healing and lethality rates", caption="total data") +
            theme_bw() +
            scale_color_brewer(breaks=c("tasso_dimessi_guariti", "tasso_deceduti"),
                              labels=c("% discharged/healed out of positives", "% deceased out of positives"),
                              palette="Set1") +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe_options(girafe(ggobj = gg_point, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
    })

    output$scatterRapportoTotalePositiviTamponi <- renderggiraph({
        gg_point <- ggplot(dataItaliaInput(), aes(data, y=diff_totale_casi/diff_casi_testati)) +
            geom_smooth(method = 'loess', se=T) +
            geom_point_interactive(aes(tooltip=percent(diff_totale_casi/diff_casi_testati, 0.01)), size=2) +
            geom_line(size=0.5) +
            theme_bw() +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", minor_breaks = "1 day",
            #              limits = c(as.Date("2020-04-20"), as.Date(today()))) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days", limits = c(as.Date("2020-04-21"), as.Date(today()))) +
            scale_y_continuous(labels=percent, limits = c(0, 0.15)) +
            labs(x="", y="", title="Daily rate between new positives and person tested", caption="daily data") +
            theme(axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank())

        girafe_options(girafe(ggobj = gg_point, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
    })

    output$barPlotTrattamento <- renderggiraph({
        myItaliaDiffs <- data.frame(data = dataItaliaInput()$data,
                                    ricoverati_con_sintomi = dataItaliaInput()$ricoverati_con_sintomi,
                                    terapia_intensiva = dataItaliaInput()$terapia_intensiva,
                                    isolamento_domiciliare = dataItaliaInput()$isolamento_domiciliare)

        myItaliaDiffsReshaped <- melt(myItaliaDiffs, id.vars = "data", variable.name = "Variabile")

        gg_bar <- ggplot(myItaliaDiffsReshaped, aes(x=data, y=value, fill=Variabile)) +
            geom_bar_interactive(aes(tooltip=value), stat="identity", position = "stack") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            labs(x="", y="", title="Subdivision of the health treatment for the currently positive patients", caption="total data") +
            theme_bw() +
            scale_fill_brewer(palette="Set2", labels=c("Hospitalized with symptoms", "Intensive care", "Home isolation")) +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe_options(girafe(ggobj = gg_bar, width_svg = 14, height_svg = 13), opts_toolbar(saveaspng=FALSE))
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
            geom_point_interactive(aes(tooltip=plotVar), size=1.25) +
            geom_line(size=0.75) +
            theme_bw() +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            labs(x="", y="", title=input$selezionaVariabileRegioni, caption = paste0(if(input$giornalieroSiNoRegione) "daily data" else "total data")) +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm"))

        girafe_options(girafe(ggobj = gg_point, width_svg = 12, height_svg = 11), opts_toolbar(saveaspng=FALSE))
    })

    output$barPlotRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)
        myDataRegionMelted <- melt(myDataRegion, id.vars=c("data", "Regione"),
                                   measure.vars=c("ricoverati_con_sintomi","terapia_intensiva","isolamento_domiciliare"),
                                   variable.name="Trattamento")
        gg_bar <- ggplot(subset(myDataRegionMelted, data==as.character(input$selezionaGiornoRegio)), aes(fill=Trattamento, x=value, y=Regione)) +
            geom_bar_interactive(aes(tooltip=value), position="stack", stat="identity") +
            theme_bw() +
            labs(x="", y="", title="Subdivision of the health treatment for the infected", caption=paste0("daily data (", format(as.Date(input$selezionaGiornoRegio), "%d/%m/%Y"), ")")) +
            scale_fill_brewer(breaks=c("ricoverati_con_sintomi", "terapia_intensiva", "isolamento_domiciliare"),
                              labels=c("Hospitalized with symptoms", "Intensive care", "Home isolation"),
                              palette="Set2") +
            theme(legend.position = "top",
                  legend.text=element_text(size=11),
                  legend.title=element_blank(),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1.25, unit="cm"))
        girafe_options(girafe(ggobj = gg_bar, width_svg = 12, height_svg = 11), opts_toolbar(saveaspng=FALSE))
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
            scale_y_continuous(labels = scales::percent) +
            scale_x_date(date_labels = "%d/%m", date_breaks = "1 day", minor_breaks = "1 day") +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=11),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")) +
            facet_wrap(~Dimensione, ncol=1, scales="free_y",
                       labeller = as_labeller(c(
                           `CasiPositiviTotali` = "Total positive cases",
                           `Deceduti` = "Deceased",
                           `DimessiGuariti` = "Discharged or healed",
                           `TerapiaIntensiva` = "Intensive care treated"
                       ))) +
            labs(x="", y="", title=paste("Relative variation from the previous day"), caption=paste("daily data (last", delayDays, "days)"))

        girafe_options(girafe(ggobj = gg_bar, width_svg = 12, height_svg = 11), opts_toolbar(saveaspng=FALSE))
    })

    output$evoluzioneTassiRegionale <- renderggiraph({
        myDataRegion <- subset(dataRegioniInput(), Regione %in% input$selezionaRegioni)

        myRegioniRates <- myDataRegion %>%
            group_by(Regione) %>%
            mutate(tasso_deceduti = deceduti/totale_casi,
                   tasso_dimessi_guariti = dimessi_guariti/totale_casi)

        myRegioniRatesReshaped <- melt(myRegioniRates, id.vars = list("data", "Regione"), measure.vars = list("tasso_deceduti", "tasso_dimessi_guariti"), variable.name = "Tasso")

        gg_point <- ggplot(myRegioniRatesReshaped, aes(x=data, y=value, color=Tasso)) +
            geom_line(size=0.75) + geom_smooth(size=0.75, show.legend = F) +
            geom_point_interactive(aes(tooltip=percent(value, 0.01)), size=1.5) +
            # scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            scale_x_date(date_labels = "%d/%m", date_breaks = "15 days") +
            scale_y_continuous(labels=percent) +
            labs(x="", y="", title="Monitoring of healing and lethality rates", caption="total data") +
            theme_bw() +
            scale_color_brewer(breaks=c("tasso_dimessi_guariti", "tasso_deceduti"),
                              labels=c("% discharged/healed out of positives", "% deceased out of positives"),
                              palette="Set1") +
            facet_wrap(~Regione, ncol=1, scales="free_y") +
            theme(legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.margin=margin(t=0, r=0, b=0, l=-1, unit="cm"))

        girafe_options(girafe(ggobj = gg_point, width_svg = 12, height_svg = 11), opts_toolbar(saveaspng=FALSE))
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
            geom_point_interactive(aes(tooltip=plotVar), size=1.5) +
            geom_line(size=0.75) +
            theme_bw() +
            scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", minor_breaks = "1 day") +
            labs(x="", y="", title="Total positive cases", caption=paste0(if(input$giornalieroSiNoProvincia) "daily data" else "total data")) +
            theme(legend.position = "bottom",
                  legend.title=element_blank(),
                  legend.text = element_text(size=13),
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank(),
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
            scale_fill_brewer(palette="Set2") +
            theme(legend.position = "none",
                  axis.title.x=element_blank(),
                  panel.grid.minor.x = element_blank()) +
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
}

# Run the application
shinyApp(ui = ui, server = server)
