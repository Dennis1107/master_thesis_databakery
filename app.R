library(shiny)
library(prophet)
library(ggplot2)
library(shinydashboard)
library(shinyjs)
library(xlsx)
library(SCperf)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(scales)
library(htmltools)
library(flexdashboard)
library(bsplus)
library(Metrics)
library(MLmetrics)
library(shinyalert)


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = tags$a(href='https://www.google.de/',
                                 tags$img(src='logoneu6.png',height='50px'))),
  

  
  ## Sidebar ------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard2", icon = icon("dashboard")),
      menuItem("Umsatzanalyse", tabName = "Umsatzanalyse", icon = icon("euro")),
      menuItem("Retourenanalyse", tabName = "Retourenanalyse", icon = icon("undo")),
      menuItem("Prognose", tabName = "Prognose", icon = icon("bar-chart-o")),
      menuItem("Finanzen", tabName = "Finanzen", icon = icon("calculator")),
      menuItem("Downloads", tabName = "Downloads", icon = icon("download")),
      
      #helpText("Filtern"),
      fileInput(inputId = "file2", 
                label = "Upload CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput(inputId = "FilialeFilter", label = "Filiale", choices = c("Gesamt"), selectize=TRUE),
      selectInput(inputId = "ProduktFilter", label = "Produkt", choices = c("Gesamt"), selectize=TRUE),
      dateRangeInput(inputId = "DateFilter", label = "Zeitraum")
      
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "Finanzen",
    fluidRow(
      box(width = 12,
          title = "Wachstum vs. Umsatzanteil Matrix", status = "primary", solidHeader = TRUE, 
          actionButton("BCG_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          sliderInput(inputId = "quantile", label = "Filter - Umsatzanteil", min = 0, max = 1, value = 0.8),
          sliderInput(inputId = "BCG_days", label = "Anzahl an Vergleichstage", min = 7, max = 100, value = 30),
          plotlyOutput(outputId = "BCG")
          
      ),
      box(width = 12,
          title = "Umsatzüberleitung", status = "primary", solidHeader = TRUE, 
                 #"Entwicklung des Return on Sales der letzten 4 Wochen",

                  actionButton("Umsatzuberleitung_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  selectInput(inputId = "ErsterMonat", label = "Zeitraum alt", choices = c(""), selectize=TRUE),        
                  selectInput(inputId = "ZweiterMonat", label = "Zeitraum neu", choices = c(""), selectize=TRUE),        
                  plotlyOutput(outputId = "Umsatzuberleitung" )
          
      )
      
    #------  
    )),

    #-------
    tabItem(tabName = "Prognose",
            fluidRow(
              useShinyalert(),
                  column(6, offset = 0, style='padding:3px;',
                    
                    actionButton("ForecastUpperLower_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    plotlyOutput(outputId = "ForecastUpperLower")
                    
                  ),
                  column(6, offset = 0, style='padding:3px;',
                    
                    actionButton("ForecastLMVM_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    plotlyOutput(outputId = "ForecastLMVM")
                  ),
                  column(6, offset = 0, style='padding:3px;',
                         
                         actionButton("ActualsVSPrognose_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         plotlyOutput(outputId = "ActualsVSPrognose")
                  ),
                  column(6, offset = 0, style='padding:3px;',
                       
                         actionButton("PrognoseFehler_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         plotlyOutput(outputId = "PrognoseFehler")
                  ),
              box(width = 12,
                  title = "Prognose", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("OptimaleLiefermenge2"),  
                  downloadButton("downloadData2", "Download")
              )
              
            )),
    
    
    #-------
    
    tabItem(tabName = "Dashboard2",
      fluidRow(
        

        
        tags$head(tags$style(HTML(".small-box {height: 100px}"))),
        #Dadurch sind die Boxen maximal 100px groß
        
       
        tags$head(tags$style(HTML(".fa { font-size: 20px; }"))),
        column(8,offset = 0, style='padding:3px;',
        actionButton("Box_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        column(4,offset = 0, style='padding:3px;',
        actionButton("Gauge_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        
        
       column(8, offset = 0, style='padding:0px;',
        shinydashboard::valueBoxOutput("UmsatzBox"),
        shinydashboard::valueBoxOutput("UmsatzWachstumBox"),
        shinydashboard::valueBoxOutput("UmsatzPrognoseBox") 
        
            ),
       
      column(4, offset = 0, style='padding:0px;',
        gaugeOutput("gaugeROS", height = "100px")      
            ),
      column(8, offset = 0, style='padding:0px;',
        shinydashboard::valueBoxOutput("RetourenBox"),
        shinydashboard::valueBoxOutput("RetourenWachstumBox"),
        shinydashboard::valueBoxOutput("StockoutBox")
      ),
      
      column(4, offset = 0, style='padding:0px;',
        gaugeOutput("gauge", height = "100px")           
      ),
      
        column(7, offset = 0, style='padding:3px;',
               #"Die Verkaufszahlen im aktuellen Monat (blau) im Vergleich zu den durchschnittlichen Verkaufszaheln (rot)",
               actionButton("Tracker_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               plotlyOutput(outputId = "UmsatzGewinn")
        )
        ,
        column(5, offset = 0, style='padding:3px;',
               actionButton("Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               plotlyOutput(outputId = "RetourenWoche")
        )
        
      )
    ),
    tabItem(tabName = "Umsatzanalyse",
            fluidRow(
              column(6, offset = 0, style='padding:3px;',
                     actionButton("MonatlicherUmsatz_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     plotlyOutput(outputId = "MonatlicherUmsatz")
                     
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("SalesLetzteWoche_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "SalesLetzteWoche")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("TopFilialen_ly_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "TopFilialen_ly")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("Top10Produkte_ly_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "Top10Produkte_ly")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("SalesWochentage_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "SalesWochentage")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("ROS_Monatlich_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "ROS_Monatlich")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("mymap_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       leafletOutput("mymap")
              )
              #)
              
              
              #---Ende Box2
              
            )
    ),
    tabItem(tabName = "Retourenanalyse",
            fluidRow(
              
              column(6, offset = 0, style='padding:3px;',
                     actionButton("Test2_Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     plotlyOutput(outputId = "MonatlicheRetouren")
                     
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("SalesLetzteWoche_Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "SalesLetzteWoche_Retouren")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("TopFilialen_ly_Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "TopFilialen_ly_Retouren")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("Top10Produkte_ly_Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "Top10Produkte_ly_Retouren")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("Top10Stockout_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "Top10Stockout")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("Stockout_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       plotlyOutput(outputId = "Stockout")
              ),column(6, offset = 0, style='padding:3px;',
                       actionButton("mymap_Retouren_info", "info", icon("info-circle"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                       leafletOutput("mymap_Retouren")
              )
              #)
              
              
              #---Ende Box2
              
            )
    ),
    tabItem(tabName = "Downloads",
            fluidRow(
              box(width = 6,
                  title = "Liefermengen", status = "primary", solidHeader = TRUE, 
              DT::dataTableOutput("OptimaleLiefermenge3"),  
              downloadButton("downloadData3", "Optimale Liefermenge")
           
              ),
              box(width = 6,
                  title = "Produktionsmengen", status = "primary", solidHeader = TRUE, 
                  DT::dataTableOutput("OptimaleProduktionsmenge4"),  
                  downloadButton("downloadData4", "Produktionsmenge")  
              )
            )
    )        
    #-----
    
    
    
    
    #-----        
  )
  ),
  
  tags$head(tags$style(HTML("

                            /* logo */
                            .skin-black .main-header .logo {
                            background-color: white;
                            }
                           

                            /* navbar (rest of the header) */
                            .skin-black .main-header .navbar {
                            background-color: white;
                            }

                            /* active selected tab in the sidebarmenu */
                            .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #666666;
                            }

                            /* other links in the sidebarmenu */
                            .skin-black .main-sidebar .sidebar .sidebar-menu a{
                            background-color: ##08306A;
                            color: #FFFFFF;
                            }
                            
                            .skin-black .main-sidebar {
                            background-color:  ##08306A;
                            }")))

  
)



options(shiny.maxRequestSize = 80*1024^3)
#Server Funktion reagiert auf Veränderungen der Parameter
server <- function(input,output, session) {

  
  df<-reactive({
    if (is.null(input$file2))
      return(NULL)                
    df <- as.data.frame(read.csv(input$file2$datapath))
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    return(df)
  })
  
 
  Filiale<-reactive({
    req(df())
    df_filiale <- df()
    df_filiale <- select(df_filiale, KundenNr)
    df_filiale$KundenNr <- as.character(df_filiale$KundenNr)
    Gesamt <- as.data.frame("Gesamt")
    colnames(Gesamt) <- c("KundenNr")
    df_filiale <- rbind(Gesamt, df_filiale)
    FilialName <- as.character(unique(df_filiale$KundenNr))
    return(FilialName)
  })
  
  Produkt<-reactive({
    req(df())
    df_produkt <- df()
    df_produkt <- select(df_produkt, ArtNr)
    df_produkt$ArtNr <- as.character(df_produkt$ArtNr)
    Gesamt <- as.data.frame("Gesamt")
    colnames(Gesamt) <- c("ArtNr")
    df_produkt <- rbind(Gesamt, df_produkt)
    ProduktName <- as.character(unique(df_produkt$ArtNr))
    return(ProduktName)
  })
  
  DatumMax<-reactive({
    req(df())
    df_datum <- df()
    df_datum <- select(df_datum, Datum)
    DatumMax <- as.Date(max(df_datum$Datum))
    return(DatumMax)
  })
  
  DatumMax<-reactive({
    req(df())
    df_datum <- df()
    df_datum <- select(df_datum, Datum)
    DatumMax <- as.Date(max(df_datum$Datum))
    return(DatumMax)
  })
  
  DatumMin<-reactive({
    req(df())
    df_datum <- df()
    df_datum <- select(df_datum, Datum)
    DatumMin <- as.Date(min(df_datum$Datum))
    return(DatumMin)
  })
  
  Monate<-reactive({
    req(df())
    df_datum <- df()
    df_datum <- select(df_datum, Datum)
    df_datum$Month_Yr <- format(as.Date(df_datum$Datum), "%Y-%m")
    my <- as.data.frame(unique(df_datum$Month_Yr))
    colnames(my) <- c("Monat")
    my <- as.data.frame(my[order(my$Monat, decreasing = TRUE),])
    colnames(my) <- c("Monat")
    return(my)
  })
  
  observe({
    req(DatumMin(), DatumMax())
    s <- DatumMin()
    e <- DatumMax()
    updateDateRangeInput(session,
                      "DateFilter",
                      start = s,
                      min = s,
                      end = e,
                      max = e)
    
  })
  
  
  observe({
  req(Filiale())
  x <- Filiale()
  updateSelectInput(session,
                    "FilialeFilter",
                    choices = x)
  
  })
  
  observe({
    req(Produkt())
    x <- Produkt()
    updateSelectInput(session,
                      "ProduktFilter",
                      choices = x)
    
  })
  
  observe({
    req(Monate())
    x <- Monate()
    updateSelectInput(session,
                      "ErsterMonat",
                      choices = x)
    
  })
  
  observe({
    req(Monate())
    x <- Monate()
    updateSelectInput(session,
                      "ZweiterMonat",
                      choices = x)
    
  })
  
  
  
  
  output$UmsatzNachMonaten <- renderPlot({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
    df <- df %>%
      filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
      
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    eda <- aggregate(df$VerkaufteMenge, by=list(ds=df$Date), FUN=sum)
    eda$ds <- paste(eda$ds, "01", sep = "-")
    
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    ggplot(eda, aes(x=ds, y=x)) +
      geom_point(colour="#FFFFFF") +
      geom_line(colour="#FFFFFF") +
      labs(title = "Monatliche Verkaufszahlen") +
      scale_x_date(date_breaks="1 month", date_labels="%Y %m") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, colour="#FFFFFF"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#232323",colour = NA),
            plot.background = element_rect(fill = "#232323",colour = NA),
            axis.text.y=element_text(colour="#FFFFFF"),
            axis.title.x = element_text(color="#FFFFFF"),
            axis.title.y = element_text(color="#FFFFFF"),
            plot.title = element_text(colour = "#FFFFFF")
            )

  })
  
  output$Top10Produkte <- renderPlot({
    req(df())
    df <- df()
    
    
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    pg <- aggregate(df$VerkaufteMenge, by=list(Artikel=df$ArtNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.factor(pg$Artikel)
    pg <- pg[(1:10),]
    
    ggplot(pg, aes(Artikel)) + 
      geom_bar(aes(weight = x), colour="#FFFFFF") + coord_flip() +
      labs(title = "Top10 Verkaufsprodukte") +
      ylab("Verkaufte Einheiten") + theme_classic() +
      theme(axis.text.x = element_text(colour="#FFFFFF"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#232323",colour = NA),
            plot.background = element_rect(fill = "#232323",colour = NA),
            axis.text.y=element_text(colour="#FFFFFF"),
            axis.title.x = element_text(color="#FFFFFF"),
            axis.title.y = element_text(color="#FFFFFF"),
            plot.title = element_text(colour = "#FFFFFF")
            )
    
  })
  
  
  output$UmsatzBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 7
    
    df <- df%>%
      filter(Datum >= md)
    
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$Preis <- as.numeric(df$Preis)
    
    summe <- round((sum(df$VerkaufteMenge * df$Preis))/ 100, 0)
    
    shinydashboard::valueBox(
      summe, "Umsatz VW in Hundert",
      color = "yellow"
    )
  })
  
  
  output$UmsatzWachstumBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$Preis <- as.numeric(df$Preis)
    
    md <- max(df$Datum) - 21
    
    df <- df%>%
      filter(Datum >= md)
    
    vwd <- max(df$Datum) - 14
    vw <- df%>%
      filter(Datum <= vwd & Datum >= md)
    
    awd <- max(df$Datum) - 7
    aw <- df%>%
      filter(Datum <= awd & Datum >= vwd)
    
    pwd <- max(df$Datum)
    pw <- df%>%
      filter(Datum <= pwd & Datum >= awd)
    
    Summevwd <- round((sum(vw$VerkaufteMenge * vw$Preis)), 0)
    Summeawd <- round((sum(aw$VerkaufteMenge * aw$Preis)), 0)
    Summepwd <- round((sum(pw$VerkaufteMenge * pw$Preis)), 0)
    
    wachstum = round((Summeawd-Summevwd)/Summevwd,4) * 100
    wachstum <- paste(wachstum, " %", sep="")
    
    shinydashboard::valueBox(
      wachstum, "Umsatzwachstum VW",
      color = "yellow"
    )
  })

  
  output$UmsatzPrognoseBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$Preis <- as.numeric(df$Preis)
    
    md <- max(df$Datum) - 21
    
    df <- df%>%
      filter(Datum >= md)
    
    vwd <- max(df$Datum) - 14
    vw <- df%>%
      filter(Datum <= vwd & Datum >= md)
    
    awd <- max(df$Datum) - 7
    aw <- df%>%
      filter(Datum <= awd & Datum >= vwd)
    
    pwd <- max(df$Datum)
    pw <- df%>%
      filter(Datum <= pwd & Datum >= awd)
    
    Summevwd <- round((sum(vw$VerkaufteMenge * vw$Preis)), 0)
    Summeawd <- round((sum(aw$VerkaufteMenge * aw$Preis)), 0)
    Summepwd <- round((sum(pw$VerkaufteMenge * pw$Preis)), 0)
    
    wachstum = round((Summepwd-Summeawd)/Summeawd,4) * 100
    wachstum <- paste(wachstum, " %", sep="")
    
    shinydashboard::valueBox(
      wachstum, "Umsatzprognose NW",
      color = "yellow"
    )
  })
  
  
  
  output$AbsatzBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    summe <- round(sum(df$VerkaufteMenge) / 1000, 0)
    
    shinydashboard::valueBox(
      summe, "Absatz in Tsd",
      color = "teal"
    )
  })
  
  
  output$AbsatzWachstumBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$Preis <- as.numeric(df$Preis)
    
    md <- max(df$Datum) - 21
    
    df <- df%>%
      filter(Datum >= md)
    
    vwd <- max(df$Datum) - 14
    vw <- df%>%
      filter(Datum <= vwd & Datum >= md)
    
    awd <- max(df$Datum) - 7
    aw <- df%>%
      filter(Datum <= awd & Datum >= vwd)
    
    pwd <- max(df$Datum)
    pw <- df%>%
      filter(Datum <= pwd & Datum >= awd)
    
    
    Summevwd <- round((sum(vw$VerkaufteMenge)), 0)
    Summeawd <- round((sum(aw$VerkaufteMenge)), 0)
    Summepwd <- round((sum(pw$VerkaufteMenge)), 0)
    
    wachstum = round((Summeawd-Summevwd)/Summevwd,4) * 100
    wachstum <- paste(wachstum, " %", sep="")
    
    shinydashboard::valueBox(
      wachstum, "Absatzwachstum VW",
      color = "teal"
    )
  })
  
  
  output$AbsatzPrognoseBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$Preis <- as.numeric(df$Preis)
    
    md <- max(df$Datum) - 21
    
    df <- df%>%
      filter(Datum >= md)
    
    vwd <- max(df$Datum) - 14
    vw <- df%>%
      filter(Datum <= vwd & Datum >= md)
    
    awd <- max(df$Datum) - 7
    aw <- df%>%
      filter(Datum <= awd & Datum >= vwd)
    
    pwd <- max(df$Datum)
    pw <- df%>%
      filter(Datum <= pwd & Datum >= awd)
    
    Summevwd <- round((sum(vw$VerkaufteMenge)), 0)
    Summeawd <- round((sum(aw$VerkaufteMenge)), 0)
    Summepwd <- round((sum(pw$VerkaufteMenge)), 0)
    
    wachstum = round((Summepwd-Summeawd)/Summeawd,4) * 100
    wachstum <- paste(wachstum, " %", sep="")
    
    shinydashboard::valueBox(
      wachstum, "Absatzprognose NW",
      color = "teal"
    )
  })
  
  output$RetourenBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 7
    
    df <- df%>%
      filter(Datum >= md)
    
    
    df$Retoure <- as.numeric(df$Retoure)
    
    summe <- round(sum(df$Retoure) / 100, 0)
    
    shinydashboard::valueBox(
      summe, "Retouren VW in Hundert",
      color = "teal"
    )
  })
  
  output$RetourenWachstumBox <- shinydashboard::renderValueBox({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$Retoure <- as.numeric(df$Retoure)
    
    md <- max(df$Datum) - 21
    
    df <- df%>%
      filter(Datum >= md)
    
    vwd <- max(df$Datum) - 14
    vw <- df%>%
      filter(Datum <= vwd & Datum >= md)
    
    awd <- max(df$Datum) - 7
    aw <- df%>%
      filter(Datum <= awd & Datum >= vwd)
    
    pwd <- max(df$Datum)
    pw <- df%>%
      filter(Datum <= pwd & Datum >= awd)
    
    Summevwd <- sum(vw$Retoure)
    Summeawd <- sum(aw$Retoure)

    
    wachstum = round((Summeawd-Summevwd)/Summevwd,4) * 100
    wachstum <- paste(wachstum, " %", sep="")
    
    shinydashboard::valueBox(
      wachstum, "Retourenwachstum VW",
      color = "teal"
    )
  })
  
  output$StockoutBox = shinydashboard::renderValueBox({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$GelieferteMenge <- as.numeric(df$GelieferteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 7
    
    df <- df%>%
      filter(Datum >= md)
    
    df$stockout <- if_else(df$VerkaufteMenge >= df$GelieferteMenge, 1,0)
    so <- round(100 * sum(df$stockout) / nrow(df),1)
    
    so <- paste(so, " %", sep="")
    
    shinydashboard::valueBox(
      so, "Stockout VW",
      color = "teal")
  })
  


  

  output$downloadData <- downloadHandler(
    
    filename = function(file) {
      paste0("Optimale_Liefermenge_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(con) {
      req(input$LM, LM())
      
        LM <- unlist(LM, use.names=FALSE)
        LM <- as.data.frame(LM)
        
        aa <- as.data.frame(LM())
        aa <- tail(aa, input$PrognoseZeitraum)
        aa <- as.data.frame(aa)
        
        #print(head(aa))
      write.xlsx2(as.data.frame(aa), con, sheetName = "Liefermenge", row.names = FALSE)
    }
    
  )
  
  #---
  

  
  #---
  output$Test <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df <- df%>%
      filter(KundenNr == input$FilialeFilter) %>%
      filter(ArtNr == input$ProduktFilter) %>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
 
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    eda <- aggregate(df$VerkaufteMenge, by=list(ds=df$Date), FUN=sum)
    eda$ds <- paste(eda$ds, "01", sep = "-")
    
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    plot_ly(eda, x = ~ds, y = ~x, type = 'scatter', mode = 'lines')
    
    
  })
  
  output$Test2 <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    

    if(day(max(df$Datum)) > 27) {
    #dann kann der Monat drin gelassen werden. Auch wenn der Monat noch nicht ganz fertig ist
      
    } else {
      df <- df %>%
        filter(Datum < as.Date(paste(year(max(df$Datum)),month(max(df$Datum)),01,sep = "-")))
      #Alles bis zum 27. wird noch nicht aktuell angezeigt. Deswegen wird dieser Monat rausgefiltert
    }
    

    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Umsatz <- as.numeric(df$Umsatz)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    eda <- aggregate(df$Umsatz, by=list(ds=df$Date), FUN=sum)
    eda$ds <- paste(eda$ds, "01", sep = "-")
    
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
  
    plot_ly(eda, x = ~ds, y = ~x, type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
      layout(title = "Monatliche Umsatzzahlen in €",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  output$MonatlicherUmsatz <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    if(day(max(df$Datum)) > 27) {
      #dann kann der Monat drin gelassen werden. Auch wenn der Monat noch nicht ganz fertig ist
      
    } else {
      df <- df %>%
        filter(Datum < as.Date(paste(year(max(df$Datum)),month(max(df$Datum)),01,sep = "-")))
      #Alles bis zum 27. wird noch nicht aktuell angezeigt. Deswegen wird dieser Monat rausgefiltert
    }
    
    
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Umsatz <- as.numeric(df$Umsatz)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    
    Years = as.data.frame(unique(year(df$Datum)))
    colnames(Years) <- c("Jahre")
    
    df$Jahr <- year(df$Datum)
    
    p <- plot_ly(type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
      layout(title = "Monatliche Umsatzzahlen in €",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    color_vec <- c('rgb(8,48,107)','rgb(205, 12, 24)','rgb(0, 0, 0)','rgb(128, 128, 128)','rgb(0, 255, 255)', 'rgb(218, 165, 32)','rgb(34, 139, 34)')
    
    i = 1
    
    while (i <= nrow(Years)) {
      
      df_neu <- df %>%
        filter(Jahr == Years$Jahr[i])
      
      eda <- aggregate(df_neu$Umsatz, by=list(ds=df_neu$Date), FUN=sum)
      eda$ds <- paste(eda$ds, "01", sep = "-")
      
      eda$x <- as.numeric(eda$x)
      eda$ds <- as.Date(eda$ds)
      eda$month <- month(eda$ds)
      name <- unique(year(eda$ds))
      
      p <- p %>%
        add_trace(data = eda, x = ~month, y = ~x , name = paste("Umsatz ",name), line = list(color = color_vec[i]))  
      
      i = i+1
    }
    
    p
    
    
    
    
    
    
  })
  
  
  output$SalesLetzteWoche <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    df$GelieferterUmsatz <- as.numeric(df$Preis * df$GelieferteMenge)
    
    df$Umsatz <- as.numeric(df$Umsatz)

    eda <- aggregate(df$Umsatz, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    

    t2 <- aggregate(df$GelieferterUmsatz, by=list(ds=df$Datum), FUN=sum)
    t2$x <- as.numeric(t2$x)
    t2$ds <- as.Date(t2$ds)
    
    
    eda$gm <- t2$x
    
    
    plot_ly(eda, x = ~ds, y = ~x, name = 'Umsatz', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      add_trace(y = ~gm, name = 'Gelieferter Umsatz', line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
      layout(title = "Umsatz der letzten 2 Wochen in €",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'))
    
    
  })
  
  
  
  output$Top10Produkte_ly <- renderPlotly({
    req(df())
    df <- df()
    
    
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Datum <- as.Date(df$Datum)
    df$Umsatz <- as.numeric(df$Umsatz)
    
    pg <- aggregate(df$Umsatz, by=list(Artikel=df$ArtNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    pg <- pg[(1:10),]
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    
    plot_ly(pg, x = ~x, y = ~Artikel, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Top Umsatzstärkste Produkte",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  
  output$TopFilialen_ly <- renderPlotly({
    req(df())
    df <- df()
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    df$Datum <- as.Date(df$Datum)
    df$Umsatz <- as.numeric(df$Umsatz)
    
    pg <- aggregate(df$Umsatz, by=list(Artikel=df$KundenNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    #pg <- pg[(1:10),]
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    
    plot_ly(pg, x = ~x, y = ~Artikel, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Umsatzstärkste Filialen",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  output$ROS_Monatlich <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    if(day(max(df$Datum)) > 27) {
      #dann kann der Monat drin gelassen werden. Auch wenn der Monat noch nicht ganz fertig ist
      
    } else {
      df <- df %>%
        filter(Datum < as.Date(paste(year(max(df$Datum)),month(max(df$Datum)),01,sep = "-")))
      #Alles bis zum 27. wird noch nicht aktuell angezeigt. Deswegen wird dieser Monat rausgefiltert
    }
    
    
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Umsatz <- as.numeric(df$Umsatz)
    df$Kosten <- as.numeric(df$VerkaufteMenge*df$VarKost)
    df$Gewinn <- df$Umsatz - df$Kosten
    
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    eda <- aggregate(df$Umsatz, by=list(ds=df$Date), FUN=sum)
    eda$ds <- paste(eda$ds, "01", sep = "-")
    
    gew <- aggregate(df$Gewinn, by=list(ds=df$Date), FUN=sum)
    eda$g <- gew$x
    
    eda$x <- as.numeric(eda$x)
    eda$g <- as.numeric(eda$g)
    eda$ds <- as.Date(eda$ds)
    eda$ros <- as.numeric(round(eda$g/eda$x,2))
    
    plot_ly(eda, x = ~ds, y = ~ros, type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
      layout(title = "Monatlicher Return on Sales in €",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  
  output$ForecastLMVM <- renderPlotly({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    df <- df%>%
      filter(Datum >= md)
    
    
    df$Datum <- as.Date(df$Datum)
    df$PrognoseMenge <- as.numeric(df$PrognoseMenge)
    df$PrognoseUpper <- as.numeric(df$PrognoseUpper)
    df$PrognoseLower <- as.numeric(df$PrognoseLower)
    
    eda <- aggregate(df$PrognoseMenge, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    t2 <- aggregate(df$PrognoseUpper, by=list(ds=df$Datum), FUN=sum)
    t2$x <- as.numeric(t2$x)
    t2$ds <- as.Date(t2$ds)
    
    t3 <- aggregate(df$PrognoseLower, by=list(ds=df$Datum), FUN=sum)
    t3$x <- as.numeric(t3$x)
    t3$ds <- as.Date(t3$ds)
    
    eda$upper <- t2$x
    eda$lower <- t3$x
    
    plot_ly(eda, x = ~ds, y = ~upper, name = 'Upper', type = 'scatter', mode = 'lines', line = list(color = 'transparent'),showlegend = FALSE) %>%
      add_trace(y = ~lower, name = 'Lower', type = 'scatter', mode = 'lines', line = list(color = 'transparent'),showlegend = FALSE, fill = 'tonexty', fillcolor='rgba(31, 119, 180,0.2)') %>%
      add_trace(y = ~x, name = 'Prognose', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      layout(title = "Verkaufsprognose 2 Wochen",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = "",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    
    
  })
  
  
  output$ForecastUpperLower <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    
    df$Datum <- as.Date(df$Datum)
    df$PrognoseMenge <- as.numeric(df$PrognoseMenge)
    df$LieferMenge <- as.numeric(df$LieferMenge)
    
    
    
    
    eda <- aggregate(df$PrognoseMenge, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    t2 <- aggregate(df$LieferMenge, by=list(ds=df$Datum), FUN=sum)
    t2$x <- as.numeric(t2$x)
    t2$ds <- as.Date(t2$ds)
    
    eda$gm <- t2$x
    
    plot_ly(eda, x = ~ds, y = ~x, name = 'Prognose Verkaufsmenge', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      add_trace(y = ~gm, name = 'Prognose Liefermenge', line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
      layout(title = "Verkaufsprognose 2 Wochen",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  
  output$OptimaleLiefermenge2 <- renderDataTable({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    df <- df%>%
      select(Datum, KundenNr, ArtNr, PrognoseMenge, LieferMenge)
    
    DT::datatable(df, 
                  options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, colnames = c("Datum","Filiale","Produkt", "Verkaufsmenge", "Liefermenge"), caption = 'Optimale Liefermenge', filter = 'top') %>% 
      formatRound(columns=2,digits=0)
  })
  
  output$OptimaleLiefermenge3 <- renderDataTable({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    df <- df%>%
      select(Datum, KundenNr, ArtNr, PrognoseMenge, LieferMenge)
    
    DT::datatable(df, 
                  options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, colnames = c("Datum","Filiale","Produkt", "Verkaufsmenge", "Liefermenge"), caption = 'Optimale Liefermenge', filter = 'top') %>% 
      formatRound(columns=2,digits=0)
  })
  
  output$OptimaleProduktionsmenge4 <- renderDataTable({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    df <- df%>%
      filter(Datum >= md)
    df <- df %>% 
      group_by(Datum, ArtNr) %>% 
      summarise(ProduktionsMenge = sum(LieferMenge))
    
    
    DT::datatable(df, 
                  options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, colnames = c("Datum","Artikel","Produktionsmenge"), caption = 'Optimale Liefermenge', filter = 'top') %>% 
      formatRound(columns=2,digits=0)
  })
  
  
 
  output$downloadData2 <- downloadHandler(
    
    filename = function() {
      paste0("Optimale_Liefermenge_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(df())
      df <- df()
      df$Datum <- as.Date(df$Datum)
      
      if(input$FilialeFilter == "Gesamt") {
        
      } else {
        df <- df %>%
          filter(KundenNr == input$FilialeFilter)
      }
      
      if(input$ProduktFilter == "Gesamt") {
        
      } else {
        df <- df %>%
          filter(ArtNr == input$ProduktFilter)
      }
      
      md <- max(df$Datum) - 14
      
      
      df <- df%>%
        filter(Datum >= md)
      df <- df%>%
        select(Datum, KundenNr, ArtNr, PrognoseMenge, LieferMenge)
      
      write.xlsx2(as.data.frame(df), file, sheetName = "Liefermenge", row.names = FALSE)
    }
    
  )
  
  
  output$downloadData3 <- downloadHandler(
    
    filename = function() {
      paste0("Optimale_Liefermenge_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(df())
      df <- df()
      df$Datum <- as.Date(df$Datum)
      
      md <- max(df$Datum) - 14
      
      df <- df%>%
        filter(Datum >= md)
      df <- df%>%
        select(Datum, KundenNr, ArtNr, PrognoseMenge, LieferMenge)
      
      write.xlsx2(as.data.frame(df), file, sheetName = "Liefermenge", row.names = FALSE)
    }
    
  )
  
  output$downloadData4 <- downloadHandler(
    
    filename = function() {
      paste0("Produktproduktion_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(df())
      df <- df()
      df$Datum <- as.Date(df$Datum)
      
      md <- max(df$Datum) - 14
      
      df <- df%>%
        filter(Datum >= md)
      
      df <- df %>% 
        group_by(Datum, ArtNr) %>% 
        summarise(ProduktionsMenge = sum(LieferMenge))
      
      write.xlsx2(as.data.frame(df), file, sheetName = "Produktionsmenge", row.names = FALSE)
    }
    
  )
  
  output$BCG <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    
    md <- max(df$Datum) - as.numeric(input$BCG_days)
    cd <- max(df$Datum) - (2 * as.numeric(input$BCG_days))
    

    df_am <- df%>%
      filter(Datum >= md)
    
    df_vm <- df%>%
      filter(Datum < md & Datum >= cd)
    
    df_am_gr <- aggregate(df_am$VerkaufteMenge, by=list(ArtNr=df_am$ArtNr), FUN=sum)
    df_am_gr$x <- as.numeric(df_am_gr$x)
    colnames(df_am_gr) <- c("ArtNr", "VMAM")
    
    df_vm_gr <- aggregate(df_vm$VerkaufteMenge, by=list(ArtNr=df_vm$ArtNr), FUN=sum)
    df_vm_gr$x <- as.numeric(df_vm_gr$x)
    colnames(df_vm_gr) <- c("ArtNr", "VMVM")
    
    
    df_new <- left_join(df_am_gr, df_vm_gr, by = "ArtNr" )
    df_new <- na.omit(df_new)

    qAM <- as.numeric(quantile(df_new$VMAM, input$quantile))
    qVM <- as.numeric(quantile(df_new$VMVM, input$quantile))
    
    df_new <- df_new %>%
      filter(VMAM >= qAM) %>%
      filter(VMVM >= qVM) 
    
    df_new$Wachstum <- ((df_new$VMAM-df_new$VMVM)/df_new$VMVM) * 100
    df_new[is.na(df_new)] <- 0
    
    SP <- max(df_new$VMAM)
    
    df_new$relMarktanteil <- (df_new$VMAM/max(df_new$VMAM)) * 100
    df_new[is.na(df_new)] <- 0
    
    maxwachstum <- df_new[order(-df_new$Wachstum),] 
    maxwachstum <- maxwachstum[(1:10),]
    
    maxMarktanteil <- df_new[order(-df_new$relMarktanteil),]
    maxMarktanteil <- maxMarktanteil[(1:10),]
    
    df_new <- rbind(maxwachstum, maxMarktanteil)
    
    plot_ly(df_new, x = ~relMarktanteil, y = ~Wachstum, text = ~ArtNr, type = 'scatter', mode = 'markers',
            marker = list(size = ~relMarktanteil, opacity = 0.5)) %>%
      layout(title = 'Marktübersicht',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
    
    
  })
  
  
  output$ROS <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    

    
    df_aw <- df%>%
      filter(Datum >= (max(df$Datum) - 7))
    
    df_vw <- df%>%
      filter(Datum < (max(df$Datum) - 7) & Datum >= (max(df$Datum) - 14))
    
    df_vw2 <- df%>%
      filter(Datum < (max(df$Datum) - 14) & Datum >= (max(df$Datum) - 21))
    
    df_vw3 <- df%>%
      filter(Datum < (max(df$Datum) - 21) & Datum >= (max(df$Datum) - 28))
    
   
    ros_aw <- sum(df_aw$VerkaufteMenge*(df_aw$Preis-df_aw$VarKost)) / sum(df_aw$VerkaufteMenge*(df_aw$Preis))
    ros_vw <- sum(df_vw$VerkaufteMenge*(df_vw$Preis-df_vw$VarKost)) / sum(df_vw$VerkaufteMenge*(df_vw$Preis))
    ros_vw2 <- sum(df_vw2$VerkaufteMenge*(df_vw2$Preis-df_vw2$VarKost)) / sum(df_vw2$VerkaufteMenge*(df_vw2$Preis))
    ros_vw3 <- sum(df_vw3$VerkaufteMenge*(df_vw3$Preis-df_vw3$VarKost)) / sum(df_vw3$VerkaufteMenge*(df_vw3$Preis))
    
    ros <- as.data.frame(c(ros_vw3, ros_vw2, ros_vw, ros_aw))
    ros$Woche <- c("AW-3","AW-2","AW-1","AW")
    colnames(ros) <- c("ROS", "Woche")
    
    ros$Woche <- factor(ros$Woche, levels = c(as.character(ros$Woche)))
    ros$ROS <- as.numeric(ros$ROS)
    
    plot_ly(ros, x = ~Woche, y = ~ROS, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Return On Sales der letzten 4 Wochen",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  output$UmsatzGewinn <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    df$Monat <- month(df$Datum)
    df$Tag <- day(df$Datum)
    
    avg_day <- df %>% 
      group_by(Tag) %>% 
      summarise(VerkaufteMenge = mean(VerkaufteMenge))
    
    df$Month_Yr <- format(as.Date(df$Datum), "%Y-%m")
    
    df_am <- df %>%
      filter(Month_Yr >= max(df$Month_Yr))
    df_am <- df_am %>% 
      group_by(Tag) %>% 
      summarise(VerkaufteMenge = mean(VerkaufteMenge))
    
    
    df_new <- left_join(avg_day, df_am, by = "Tag" )
    colnames(df_new) <- c("Tag","AVG","Actuals")
    df_new[is.na(df_new)] <- 0
    
    df_new$AVGcum <- cumsum(df_new$AVG)
    df_new$Actualscum <- cumsum(df_new$Actuals)
    
    
    mon <- months(max(df$Datum))
    
    plot_ly(df_new, x = ~Tag, y = ~Actualscum, name = 'Aktueller Monat', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      add_trace(y = ~AVGcum, name = 'Durchschnitt', line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
      layout(title = paste("Täglicher Verkaufstracker ",mon,sep = ""),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'))
    
  })
  
  
  output$SalesWochentage <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    df$Wochentag <- weekdays(df$Datum)

    avg_day <- df %>% 
      group_by(Wochentag) %>% 
      summarise(VerkaufteMenge = mean(VerkaufteMenge))
    colnames(avg_day) <- c("Wochentag","Durchschnitt")
    
    df_aw <- df%>%
      filter(Datum >= (max(df$Datum) - 7))
    
    vec <- unique(df_aw$Wochentag)
    
    df_fc <- df_aw %>% 
      group_by(Wochentag) %>% 
      summarise(VerkaufteMenge = mean(VerkaufteMenge))
    colnames(df_fc) <- c("Wochentag","Forecast")
    
    df_new <- left_join(avg_day, df_fc, by = "Wochentag" )
    df_new <- df_new[order(match(df_new[[1]], vec)), ]

    df_new$Wochentag <- factor(df_new$Wochentag, levels = c(as.character(df_new$Wochentag)))
    
    
    
       p <- plot_ly(df_new) %>%
      add_trace(x = ~Wochentag, y = ~Forecast, type = 'bar', name = 'Forecast',
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      add_trace(x = ~Wochentag, y = ~Durchschnitt, type = 'scatter', mode = 'lines', name = 'Durchschnitt',
                line = list(color = 'rgb(205, 12, 24)',width = 0.7),
                hoverinfo = "text") %>%
      layout(title = "Umsatz Forecast nach Wochentagen",
         bargap = 0.4,
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         legend = list(orientation = 'h'))
         
    
  })
  
  
  
  output$mymap <- renderLeaflet({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])

    pg <- aggregate(df$VerkaufteMenge, by=list(Artikel=df$KundenNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    pg$scaled_x <- rescale(pg$x, to=c(0.1,1.1))
    pg$latitude <- c(48.1779237,48.2680232,48.2315361,48.22158476,48.23404954,48.22798906,48.25759853,48.2476544,48.20579931,48.21666663,48.19332766,48.18062405,48.18897903,48.28901839,48.29518584)
    pg$longitude <- c(8.4196042,8.4839444,8.4066168,8.41690063,8.37827682,8.33890676,8.44342232,8.49105835,8.51560593,8.50049973,8.47612381,8.44762802,8.35158691,8.56461525,8.4914875)

    leaflet(pg) %>% addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude, weight=1, radius = ~sqrt(scaled_x) * 30) %>%
      addMarkers(lat = ~latitude, lng = ~longitude, popup = ~htmlEscape(as.character(x)))
    
  })
  
  output$gauge = renderGauge({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    retoure = 100 * round(sum(df$VerkaufteMenge)/ sum(df$GelieferteMenge),2)
    
    
    gauge(retoure, 
          min = 0, 
          max = 100,
          symbol = '%',
          label = "Retourenquote",
          sectors = gaugeSectors(success = c(80, 100), 
                                 warning = c(40, 80),
                                 danger = c(0, 40)))
  })
  
  output$gaugeROS = renderGauge({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    ros = 100 * round(sum(df$VerkaufteMenge*(df$Preis-df$VarKost)) / sum(df$VerkaufteMenge*(df$Preis)),2)
    
    gauge(ros, 
          min = 0, 
          max = 100,
          symbol = '%',
          label = "Return on Sales",
          sectors = gaugeSectors(success = c(50, 100), 
                                 warning = c(35, 50),
                                 danger = c(0, 35)))
  })
  
  output$gaugeStockout = renderGauge({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$GelieferteMenge <- as.numeric(df$GelieferteMenge)

    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df$stockout <- if_else(df$VerkaufteMenge >= df$GelieferteMenge, 1,0)
    so <- round(100 * sum(df$stockout) / nrow(df),1)
    
    gauge(so, 
          min = 0, 
          max = 100,
          symbol = '%',
          label = "Stockoutquote",
          sectors = gaugeSectors(danger = c(80, 100), 
                                 warning = c(40, 80),
                                 success = c(0, 40)))
  })
  
  output$RetourenWoche <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    

    eda <- aggregate(df$Retoure, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      automargin = TRUE
    )

    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 25,
      pad = 1
    )

    plot_ly(eda, x = ~ds, y = ~x, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title="Retouren der letzten 2 Wochen",
             bargap = 0.4,
             xaxis = ax,
             yaxis = ax,
             margin = m,
             autosize = TRUE

             )
    
  })
  
  
  output$Top10Produkte_ly_Retouren <- renderPlotly({
    req(df())
    df <- df()
    
    
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Datum <- as.Date(df$Datum)
    df$Retoure <- as.numeric(df$Retoure)

    pg <- aggregate(df$Retoure, by=list(Artikel=df$ArtNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    pg <- pg[(1:10),]
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    
    plot_ly(pg, x = ~x, y = ~Artikel, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Top 10 Retouren Produkte",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  
  output$TopFilialen_ly_Retouren <- renderPlotly({
    req(df())
    df <- df()
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    df$Datum <- as.Date(df$Datum)
    df$Retoure <- as.numeric(df$Retoure)
    
    pg <- aggregate(df$Retoure, by=list(Artikel=df$KundenNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    
    plot_ly(pg, x = ~x, y = ~Artikel, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Retouren Filialen",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  output$Test2_Retouren <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Retoure <- as.numeric(df$Retoure)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    eda <- aggregate(df$Retoure, by=list(ds=df$Date), FUN=sum)
    eda$ds <- paste(eda$ds, "01", sep = "-")
    
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    plot_ly(eda, x = ~ds, y = ~x, type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
      layout(title = "Monatliche Retouren",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'))
    
    
  })
  
  output$MonatlicheRetouren <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    if(day(max(df$Datum)) > 27) {
      #dann kann der Monat drin gelassen werden. Auch wenn der Monat noch nicht ganz fertig ist
      
    } else {
      df <- df %>%
        filter(Datum < as.Date(paste(year(max(df$Datum)),month(max(df$Datum)),01,sep = "-")))
      #Alles bis zum 27. wird noch nicht aktuell angezeigt. Deswegen wird dieser Monat rausgefiltert
    }
    
    
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    
    df$Retoure <- as.numeric(df$Retoure)
    
    df <- df %>%
      mutate(Date = format(as.Date(df$Datum), "%Y-%m"))
    
    Years = as.data.frame(unique(year(df$Datum)))
    colnames(Years) <- c("Jahre")
    
    df$Jahr <- year(df$Datum)
    
    p <- plot_ly(type = 'scatter', mode = 'lines', line = list(shape = "spline")) %>%
      layout(title = "Monatliche Retouren in Stück",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    color_vec <- c('rgb(8,48,107)','rgb(205, 12, 24)','rgb(0, 0, 0)','rgb(128, 128, 128)','rgb(0, 255, 255)', 'rgb(218, 165, 32)','rgb(34, 139, 34)')
    
    i = 1
    
    while (i <= nrow(Years)) {
      
      df_neu <- df %>%
        filter(Jahr == Years$Jahr[i])
      
      eda <- aggregate(df_neu$Retoure, by=list(ds=df_neu$Date), FUN=sum)
      eda$ds <- paste(eda$ds, "01", sep = "-")
      
      eda$x <- as.numeric(eda$x)
      eda$ds <- as.Date(eda$ds)
      eda$month <- month(eda$ds)
      name <- unique(year(eda$ds))
      
      p <- p %>%
        add_trace(data = eda, x = ~month, y = ~x , name = paste("Retouren ",name), line = list(color = color_vec[i]))  
      
      i = i+1
    }
    
    p
    
    
    
    
    
    
  })
  
  
  output$SalesLetzteWoche_Retouren <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    df$Retoure <- as.numeric(df$Retoure)
    
    eda <- aggregate(df$Retoure, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    

    
    plot_ly(eda, x = ~ds, y = ~x, name = 'Verkaufte Einheiten', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      layout(title = "Retouren letzte 2 Wochen",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'))
    
    
  })
  
  output$mymap_Retouren <- renderLeaflet({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    df$Retoure <- as.numeric(df$Retoure)
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    df <- df%>%
      filter(Datum >= input$DateFilter[1]) %>%
      filter(Datum <= input$DateFilter[2])
    
    pg <- aggregate(df$Retoure, by=list(Artikel=df$KundenNr), FUN=sum)
    pg$x <- as.numeric(pg$x)
    pg <- pg[order(-pg$x),] 
    pg$Artikel <- as.character(pg$Artikel)
    pg$Artikel <- factor(pg$Artikel, levels = c(as.character(pg$Artikel)))
    pg$scaled_x <- rescale(pg$x, to=c(0.1,1.1))
    pg$latitude <- c(48.1779237,48.2680232,48.2315361,48.22158476,48.23404954,48.22798906,48.25759853,48.2476544,48.20579931,48.21666663,48.19332766,48.18062405,48.18897903,48.28901839,48.29518584)
    pg$longitude <- c(8.4196042,8.4839444,8.4066168,8.41690063,8.37827682,8.33890676,8.44342232,8.49105835,8.51560593,8.50049973,8.47612381,8.44762802,8.35158691,8.56461525,8.4914875)

    leaflet(pg) %>% addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude, weight=1, radius = ~sqrt(scaled_x) * 30) %>%
      addMarkers(lat = ~latitude, lng = ~longitude, popup = ~htmlEscape(as.character(x)))
    
  })
  
  
  output$Umsatzuberleitung <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    
    df$Umsatz = df$VerkaufteMenge * df$Preis
    
    df$Month_Yr <- format(as.Date(df$Datum), "%Y-%m")
    
    df_neu <- df%>%
      filter(Month_Yr == input$ZweiterMonat)
    
    df_alt <- df%>%
      filter(Month_Yr == input$ErsterMonat)
    
    
    
    Produkt_list = as.data.frame(unique(df_neu$ArtNr))
    colnames(Produkt_list) <- c("Artikel")
    
    Umsatz_alt = sum(df_alt$Umsatz)
    Umsatz_neu = sum(df_neu$Umsatz)
    
    diff = Umsatz_neu - Umsatz_alt
    
    Gesamtmenge_alt = sum(df_alt$VerkaufteMenge)
    Gesamtmenge_neu = sum(df_neu$VerkaufteMenge)
    Mengenabweichung = 0
    Strukturabweichung = 0
    Strukturabweichung2 = 0
    Preisabweichung = 0
    PreisMenge = 0
    
    i = 1
    
    while (i <= nrow(Produkt_list)) {
      
      df_schleife_neu <- df_neu %>%
        filter(ArtNr == Produkt_list$Artikel[i])
      
      df_schleife_alt <- df_alt %>%
        filter(ArtNr == Produkt_list$Artikel[i])
      
      Mengenabweichung = Mengenabweichung + ((Gesamtmenge_neu - Gesamtmenge_alt) * if_else(is.nan((sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt)) == TRUE, 0, (sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt)) * if_else(is.nan((sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))))
      Strukturabweichung = Strukturabweichung + (( if_else(is.nan((sum(df_schleife_neu$VerkaufteMenge) / Gesamtmenge_neu)) == TRUE, 0, (sum(df_schleife_neu$VerkaufteMenge) / Gesamtmenge_neu)) - if_else(is.nan((sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt)) == TRUE, 0, (sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt))) * Gesamtmenge_alt * if_else(is.nan((sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))))
      Strukturabweichung2 = Strukturabweichung2 + ((Gesamtmenge_neu - Gesamtmenge_alt) * (if_else(is.nan((sum(df_schleife_neu$VerkaufteMenge) / Gesamtmenge_neu)) == TRUE, 0, (sum(df_schleife_neu$VerkaufteMenge) / Gesamtmenge_neu)) - if_else(is.nan((sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt)) == TRUE, 0, (sum(df_schleife_alt$VerkaufteMenge) / Gesamtmenge_alt))) * if_else(is.nan((sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))))
      Preisabweichung = Preisabweichung + (sum(df_schleife_alt$VerkaufteMenge) * (if_else(is.nan((sum(df_schleife_neu$Umsatz) / sum(df_schleife_neu$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_neu$Umsatz) / sum(df_schleife_neu$VerkaufteMenge))) - if_else(is.nan((sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge)))))
      PreisMenge = PreisMenge + ((sum(df_schleife_neu$VerkaufteMenge) - sum(df_schleife_alt$VerkaufteMenge)) * (if_else(is.nan((sum(df_schleife_neu$Umsatz) / sum(df_schleife_neu$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_neu$Umsatz) / sum(df_schleife_neu$VerkaufteMenge))) - if_else(is.nan((sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge))) == TRUE, 0, (sum(df_schleife_alt$Umsatz) / sum(df_schleife_alt$VerkaufteMenge)))))
      
      i = i +1
    }
    
  
    Strukturabweichung = Strukturabweichung + Strukturabweichung2
    zw = Mengenabweichung + Strukturabweichung + Preisabweichung + PreisMenge
    
    PreisMenge = PreisMenge + (diff- zw)
    
    
    if(Mengenabweichung < 0) {
      b2 = Umsatz_alt + Mengenabweichung
    } else {
      b2 = Umsatz_alt
    }
    
    m2 = Umsatz_alt + Mengenabweichung
    
    if(Strukturabweichung < 0) {
      b3 = m2 + Strukturabweichung
    } else {
      b3 = m2
    }
    
    m3 = m2 + Strukturabweichung
    
    if(Preisabweichung < 0) {
      b4 = m3 + Preisabweichung
    } else {
      b4 = m3
    }
    
    m4 = m3 + Preisabweichung
    
    if(PreisMenge < 0) {
      b5 = m4 + PreisMenge
    } else {
      b5 = m4
    }
    
    base <- c(0, b2,b3,b4,b5,0)
    Umsatz <- c(Umsatz_alt, 0,0,0,0, Umsatz_neu)
    Effkte <- c(0,abs(Mengenabweichung),abs(Strukturabweichung),abs(Preisabweichung),abs(PreisMenge),0)
    x <- c('Umsatz alt', 'Mengeneffekt', 'Struktureffekt', 'Preiseffekt', 'PreisMenge', 'Umsatz neu')
    
    ef <- as.data.frame(c(Mengenabweichung, Strukturabweichung, Preisabweichung, PreisMenge))
    colnames(ef) <- c("ef")
    
    Pos <- list()
    Neg <- list()
    i = 1
    
    Pos[[1]] <- 0
    Neg[[1]] <- 0
    
    while (i <= 4) {
      
      if(ef$ef[i] >= 0) {
        Pos[[i+1]] <- ef$ef[i]
        Neg[[i+1]] <- 0
      } else {
        Pos[[i+1]] <- 0
        Neg[[i+1]] <- ef$ef[i]
      }
      
      
      i=i+1
    }
    
    Pos[[i+1]] <- 0
    Neg[[i+1]] <- 0
    
    Pos <- unlist(Pos, use.names=FALSE)
    Neg <- unlist(Neg, use.names=FALSE)
    Neg <- abs(Neg)
    
    
    
    data <- data.frame(x, base, Umsatz, Effkte)
    data$x <- factor(data$x, levels = data[["x"]])
    
    min_r = max(c(Umsatz_alt,Umsatz_neu)) - abs(Mengenabweichung)- abs(Strukturabweichung)- abs(Preisabweichung)- abs(PreisMenge)
    
    
    p <- plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)')) %>%
      add_trace(y = ~Umsatz, marker = list(color = 'rgb(158,202,225)',
                                           line = list(color = 'rgb(8,48,107)',
                                                       width = 1.5))) %>%
      add_trace(y = ~Pos, marker = list(color = 'rgb(34,139,34)',
                                           line = list(color = 'rgb(0,0,0)',
                                                       width = 1.5))) %>%
      add_trace(y = ~Neg, marker = list(color = 'rgb(205, 12, 24)',
                                        line = list(color = 'rgb(0, 0, 0)',
                                                    width = 1.5))) %>%
      
      layout(title = 'Umsatzüberleitung',
             xaxis = list(title = ""),
             yaxis = list(title = "",range = c(min_r,max(c(Umsatz_alt,Umsatz_neu)))),
             barmode = 'stack',
             showlegend = FALSE)
    
    
    
    
    
  })
  
  
  output$ActualsVSPrognose <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$PrognoseMenge <- as.numeric(df$PrognoseMenge)
    
    eda <- aggregate(df$VerkaufteMenge, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    t2 <- aggregate(df$PrognoseMenge, by=list(ds=df$Datum), FUN=sum)
    t2$x <- as.numeric(t2$x)
    t2$ds <- as.Date(t2$ds)
    
    eda$gm <- t2$x
    
    plot_ly(eda, x = ~ds, y = ~x, name = 'Verkaufte Einheiten', type = 'scatter', mode = 'lines', line = list(color = 'rgb(8,48,107)')) %>%
      add_trace(y = ~gm, name = 'Prognose', line = list(color = 'rgb(205, 12, 24)', dash = 'dash')) %>%
      layout(title = "Verkaufszahlen vs. Prognose",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  output$PrognoseFehler <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$PrognoseMenge <- as.numeric(df$PrognoseMenge)
    
    eda <- aggregate(df$VerkaufteMenge, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    t2 <- aggregate(df$PrognoseMenge, by=list(ds=df$Datum), FUN=sum)
    t2$x <- as.numeric(t2$x)
    t2$ds <- as.Date(t2$ds)
    
    eda$gm <- t2$x
    
    eda$fehler <- eda$gm - eda$x
    
    #RMSE
    rmse_metric <- rmse(eda$x, eda$gm)
    
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      automargin = TRUE
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 25,
      pad = 1
    )
    
    plot_ly(eda, x = ~ds, y = ~fehler, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title= paste("Prognosefehler RMSE: ", round(rmse_metric,1)),
             bargap = 0.4,
             xaxis = ax,
             yaxis = ax,
             margin = m,
             autosize = TRUE
      )
    
    
  })
  
  output$Stockout <- renderPlotly({
    req(df())
    df <- df()
    df$Datum <- as.Date(df$Datum)
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    if(input$ProduktFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(ArtNr == input$ProduktFilter)
    }
    
    md <- max(df$Datum) - 14
    
    
    df <- df%>%
      filter(Datum >= md)
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$GelieferteMenge <- as.numeric(df$GelieferteMenge)
    
    df$stockout <- if_else(df$VerkaufteMenge >= df$GelieferteMenge, 1,0)
    
    
    eda <- aggregate(df$stockout, by=list(ds=df$Datum), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda$ds <- as.Date(eda$ds)
    
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = TRUE,
      showticklabels = TRUE,
      showgrid = FALSE,
      automargin = TRUE
    )
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 25,
      pad = 1
    )
    
    plot_ly(eda, x = ~ds, y = ~x, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title= "Anzahl an Stockouts",
             bargap = 0.4,
             xaxis = ax,
             yaxis = ax,
             margin = m,
             autosize = TRUE
      )
    
    
  })
  
  
  output$Top10Stockout <- renderPlotly({
    req(df())
    df <- df()
    
    if(input$FilialeFilter == "Gesamt") {
      
    } else {
      df <- df %>%
        filter(KundenNr == input$FilialeFilter)
    }
    
    md <- max(df$Datum) - 14
    
    df <- df%>%
      filter(Datum >= md)
    
    df$VerkaufteMenge <- as.numeric(df$VerkaufteMenge)
    df$GelieferteMenge <- as.numeric(df$GelieferteMenge)
    
    df$stockout <- if_else(df$VerkaufteMenge >= df$GelieferteMenge, 1,0)
    
    
    eda <- aggregate(df$stockout, by=list(Artikel=df$ArtNr), FUN=sum)
    eda$x <- as.numeric(eda$x)
    eda <- eda[order(-eda$x),] 
    eda$Artikel <- as.character(eda$Artikel)
    eda <- eda[(1:10),]
    eda$Artikel <- factor(eda$Artikel, levels = c(as.character(eda$Artikel)))
    
    
    plot_ly(eda, x = ~x, y = ~Artikel, type = 'bar', 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Stockouts nach Produkten - letzte 2 Wochen",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    
  })
  
  
  observeEvent(input$ForecastUpperLower_info, {
    # Show a modal when the button is pressed
    shinyalert("Verkaufsprognose der nächsten 2 Wochen", "Diese Grafik zeigt die Prognose der verkauften Einheiten für die nächsten 2 Wochen (blaue Linie). Die rote Linie zeigt die resultierende optimale Liefermenge. Grundsätzlich ist es wünschenswert, dass die Linien nah beieinander sind, um möglichst wenig Retouren zu haben. Allerdings sollte genügend Sicherheitsbestand in den Filialen vorhanden sein damit die Produkte nicht zu schnell ausverkauft sind.")
  })

  observeEvent(input$ForecastLMVM_info, {
    # Show a modal when the button is pressed
    shinyalert("Verkaufsprognose der nächsten 2 Wochen mit Konfidenzintervallen", "Diese Grafik zeigt die Prognose der verkauften Einheiten für die nächsten 2 Wochen. Die hellblau markierte Fläche um die Linie zeigt das Konfidenzintervall an jedem Tag. Ein Konfidenzintervall gibt an, dass die tatsächliche Verkaufsprognose mit sehr hoher Wahrscheinlichkeit in diesem Bereich liegt. Demnach beschreibt die Fläche die Unsicherheit des Prognosemodells.")
  })
  
  observeEvent(input$ActualsVSPrognose_info, {
    # Show a modal when the button is pressed
    shinyalert("Prognosegüte der letzten 2 Wochen", "Diese Grafik zeigt die Verkaufszahlen der letzten 2 Wochen. Dabei zeigt die blaue Linie die tatsächlich verkauften Einheiten, während die rote Linie die Verkaufsprognose an den einzelnen Tagen zeigt. Hier wird deutlich wie gut die Vorhersage in den letzten 2 Wochen war.")
  })
  
  observeEvent(input$PrognoseFehler_info, {
    # Show a modal when the button is pressed
    shinyalert("Prognosefehler der letzten 2 Wochen", "Diese Grafik zeigt wie hoch die Abweichung der Verkaufszahlen der tatsächlich verkauften Einheiten zur Verkaufsprognose war. Grundsätzlich ist es erstrebenswert, dass dieser Fehler minimiert wird. Der RMSE zeigt die durchschnittliche Abweichung der letzten 2 Wochen. ")
  })
  #-----
  
  observeEvent(input$BCG_info, {
    # Show a modal when the button is pressed
    shinyalert("BCG Matrix", "Diese Grafik zeigt auf der X-Achse den Umsatzanteil der Produkte. Auf der Y-Achse ist das Wachstum der Produkte für die gewählte Anzahl an Tagen, die über den Filter eingestellt werden können. Mit dieser Grafik können vielversprechende Produkte identifiziert werden, die in letzter Zeit ein hohes Wachstum verzeichnen. Über den Filter-Umsatzanteil kann eingestellt werden, wie viele Produkte dargestellt werden. Setzt man den Filter hoch, so werden nur Produkte angezeigt, die einen entsprechenden Umsatzanteil haben.")
  })
  
  observeEvent(input$Umsatzuberleitung_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatzüberleitung", "Diese Grafik zeigt die Erklärung für die Umsatzveränderung zwischen zwei Perioden. Der Mengeneffekt beschreibt, wie viel Umsatz alleine auf eine höhere/niedrigere Menge zurückzuführen ist. Der Struktureffekt beschreibt wie viel Umsatz nur auf eine Veränderung der verkauften Produkte zurückzuführen ist. Ist beispielsweise in Periode 1 der Umsatzanteil der Brezel bei 15% und in Periode 2 bei 20% wird dies im Struktureffekt sichtbar. Der Preiseffekt beschreibt den Umsatzanteil der auf eine reine Preisveränderung der Produkte zurückzuführen ist. Der letzte Effekt beschreibt den Umsatzanteil der nicht auf die anderen Effekte zurückzuführen ist")
  })
  #----
  observeEvent(input$Test2_Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Monatliche Retouren", "Diese Grafik zeigt die monatlich aggregierten Retourenmengen im Zeitverlauf.")
  })
  
  observeEvent(input$SalesLetzteWoche_Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Retouren der letzten 2 Wochen", "In dieser Grafik sind die Retouren der letzten 2 Wochen dargestellt.")
  })
  
  observeEvent(input$TopFilialen_ly_Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Retourenmenge nach Filialen", "In dieser Grafik sind die Retourenmengen nach Filialen dargestellt. Über den Produktfilter wird dadurch ersichtlich wie hoch die Retouren in den jeweiligen Filialen für ein bestimmtes Produkt sind.")
  })
  
  observeEvent(input$Top10Produkte_ly_Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Retourenmenge nach Produkten", "In dieser Grafik sind die Retourenmengen nach den Produkten dargestellt. Dabei werden immer nur die 10 Produkte mit den höchsten Retouren angezeigt. Über den Produktfilter wird dadurch ersichtlich wie hoch die Retouren der jeweiligen Produkte für eine bestimmte Filiale sind.")
  })
  
  observeEvent(input$Top10Stockout_info, {
    # Show a modal when the button is pressed
    shinyalert("Stockouts nach Produkten", "In dieser Grafik wird die Anzahl an Stockouts nach Produkten dargestellt. Ein Stockout bedeutet, dass an einem bestimmten Tag dieses Produkt ausverkauft ist. Dann beträgt der Stockout in diesem Fall 1 und falls das Produkt nicht ausverkauft ist beträgt der Stockout 0. Mit dieser Grafik können Produkte identifiziert werden, die sehr häufig ausverkauft sind. Dabei werden nur die 10 Produkte angezeigt mit der höchsten Stockout Anzahl.")
  })
  
  observeEvent(input$Stockout_info, {
    # Show a modal when the button is pressed
    shinyalert("Stockouts in den letzten 2 Wochen", "Diese Grafik zeigt die Anzahl an Stockouts in den letzten 2 Wochen. Ein Stockout bedeutet, dass an einem bestimmten Tag dieses Produkt ausverkauft ist. Dann beträgt der Stockout in diesem Fall 1 und falls das Produkt nicht ausverkauft ist beträgt der Stockout 0.")
  })
  
  observeEvent(input$mymap_Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Retourenmengen nach Filialen", "Auf dieser Karte sind die Retourenmengen nach Filialen dargestellt. Diese soll abschließend einen interaktiven Überblick über die Retourenmengen Ihrer Filialen geben.")
  })
  #---
  
  observeEvent(input$MonatlicherUmsatz_info, {
    # Show a modal when the button is pressed
    shinyalert("Monatlicher Umsatz in €", "Diese Grafik zeigt den monatlich aggregierten Umsatz im Zeitverlauf.")
  })
  
  observeEvent(input$SalesLetzteWoche_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatz der letzten 2 Wochen in €", "Diese Grafik zeigt den Umsatz der letzten 2 Wochen. Zusätzlich beschreibt die rote Linie den gelieferten Umsatz. Die logische Differenz aus den beiden Linien entspricht der Retoure.")
  })
  
  observeEvent(input$TopFilialen_ly_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatz nach Filialen", "In dieser Grafik sind die Umsätze nach Filialen dargestellt. Über den Produktfilter wird dadurch ersichtlich wie hoch die Umsätze in den jeweiligen Filialen für ein bestimmtes Produkt sind.")
  })
  
  observeEvent(input$Top10Produkte_ly_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatz nach Produkten", "In dieser Grafik sind die Umsätze nach den Produkten dargestellt. Dabei werden immer nur die 10 Produkte mit den höchsten Umsätzen angezeigt. Über den Produktfilter wird dadurch ersichtlich wie hoch die Umsätze der jeweiligen Produkte für eine bestimmte Filiale sind.")
  })
  
  observeEvent(input$SalesWochentage_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatzprognose für die nächste Woche", "Diese Grafik zeigt die Umsatzprognose für die kommende Woche aufgeteilt nach Wochentagen. Die rote Linie zeigt den durchschnittlichen Umsatz an den jeweiligen Tagen. Dadurch wird sichtbar, ob die kommende Woche über- oder unterdurchschnittlich hohe Umsätze vorhergesagt werden.")
  })
  
  observeEvent(input$ROS_Monatlich_info, {
    # Show a modal when the button is pressed
    shinyalert("Return on Sales nach Monaten in %", "Der Return on Sales zeigt wie viel Prozent des Umsatzes als Gewinn übrig bleibt. Folgendes Beispiel: Ein Brot erzielt 1€ Umsatz bei 40 Cent Kosten. Der resultierende Return on Sales entspricht dann 40%.")
  })
  
  observeEvent(input$mymap_info, {
    # Show a modal when the button is pressed
    shinyalert("Umsatz nach Filialen", "Auf dieser Karte sind die Umsätze nach Filialen dargestellt. Diese soll abschließend einen interaktiven Überblick über die Umsätze Ihrer Filialen geben.")
  })
  
  observeEvent(input$Box_info, {
    # Show a modal when the button is pressed
    shinyalert("Kennzahlen", "Diese Boxen zeigen die wichtigsten Kennzahlen Umsatz und Retouren. Dabei beziehen sich die erste Spalte immer auf Gesamtwerte im ausgewählten Zeitraum. Die zweite Spalte zeigt die Veränderung der aktuellen Woche zur Vorwoche. Die letzte Spalte stellt die Prognose für die nächste Woche dar. ")
  })
  
  observeEvent(input$Gauge_info, {
    # Show a modal when the button is pressed
    shinyalert("Tachometer", "Die erste Grafik Return on Sales zeigt das Verhältnis von Umsatz und Gewinn. Demnach gibt diese Kennzahl einen Idikator für die aktuelle Profitabilität. Die zweite Grafik zeigt die Retourenquote in %. Also wie viele % der gelieferten Produkte werden tatsächlich verkauft.")
  })
  
  observeEvent(input$Tracker_info, {
    # Show a modal when the button is pressed
    shinyalert("Verkaufstracker", "Diese Grafik zeigt die aggregierten Verkaufszahlen im aktuellen Monat an. Die rote Linie bildet den Durchschnitt der vergangenen Monate und gilt als Indikator für die Performance im aktuellen Monat. Liegen die Verkaufszahlen sehr weit unter dem Durchschnitt, sollte dies genauer analysiert werden.")
  })
  
  observeEvent(input$Retouren_info, {
    # Show a modal when the button is pressed
    shinyalert("Retouren der letzten 2 Wochen", "In dieser Grafik sind die Retouren der letzten 2 Wochen dargestellt.")
  })
#--
  
}
shinyApp(ui = ui, server = server)  