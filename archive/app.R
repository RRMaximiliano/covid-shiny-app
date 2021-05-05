# 1. Libraries ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(scales)
library(DT)
library(tidyverse)
library(extrafont)
library(COVID19)
library(highcharter)

# Load data
df         <- read_csv("data/observatorio_nicaragua_dep.csv")
df_country <- read_csv("data/observatorio_nicaragua.csv")
df_minsa   <- read_csv("data/minsa.csv")
mapdata    <- read_rds("data/mapdata.rds")

# COVID19::covid19("Nicaragua") %>%
#   select(id, date, confirmed, deaths) %>%
#   rename(cases = confirmed) %>%
#   filter(!is.na(cases)) %>%
#   write_csv("data/minsa.csv")

# Set Graph Theme
theme_plex <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "Roboto Condensed",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(
    hjust = 0, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = "Roboto Condensed"
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = "Roboto Condensed"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = "Roboto Condensed"
  )
  ret
}

# 2. UI -------------------------------------------------------------------

ui <- navbarPage("Coronavirus: Nicaragua", 
                 theme = shinytheme("paper"),
                 tabPanel("Observatorio Ciudadano COVID-19",
                          sidebarLayout(
                            sidebarPanel(position = "left", width = 3,
                                         selectInput("dataset", 
                                                     "Seleccione nivel:",
                                                     c("Nacional" = "country",
                                                       "Departamental" = "state")),
                                         conditionalPanel(
                                           condition = "input.dataset == 'state'",
                                           selectInput("dep", 
                                                       label = "Elige departamento:", 
                                                       selected = "Managua",
                                                       choices = df$departamento %>% unique %>% sort,
                                                       multiple = TRUE
                                           )),
                                         selectInput("selectedvariable",
                                                     label = "Variable: ", 
                                                     choices = c("Casos sospechosos"   = "cases",
                                                                 "Muertes sospechosas" = "deaths")),
                                         checkboxInput("logarithmicY", "Escala log", FALSE),
                                         downloadButton("data.csv", "Descargar datos")
                            ),
                            mainPanel(
                              fluidRow(
                                valueBoxOutput("vbox", width = 3),
                                valueBoxOutput("vbox1", width = 3),
                                valueBoxOutput("vbox2", width = 3)
                              ),
                              tabsetPanel(
                                id = "Tabs",
                                tabPanel("Acumulados",
                                         plotlyOutput("plot")
                                ),
                                tabPanel("Conteo diario",
                                         plotlyOutput("plot2")
                                ),
                                tabPanel("Tabla", 
                                         dataTableOutput("table"))
                              )
                            )
                          )
                 ),
                 tabPanel("MINSA",
                          sidebarLayout(
                            sidebarPanel(position = "left", width = 3, 
                                         selectInput("selectedvariable_minsa",
                                                     label = "Variable: ", 
                                                     choices = c("Casos confirmados"   = "cases",
                                                                 "Muertes confirmadas" = "deaths")),
                                         checkboxInput("logarithmicY_minsa", "Escala log", FALSE),
                                         downloadButton("data_minsa.csv", "Descargar datos")
                            ),
                            mainPanel(
                              fluidRow(
                                valueBoxOutput("vbox_minsa", width = 3),
                                valueBoxOutput("vbox1_minsa", width = 3),
                                valueBoxOutput("vbox2_minsa", width = 3)
                              ),
                              tabsetPanel(
                                id = "Tabs",
                                tabPanel("Acumulados",
                                         plotlyOutput("plot_minsa")
                                ),
                                tabPanel("Conteo diario",
                                         plotlyOutput("plot2_minsa")
                                ),
                                tabPanel("Tabla", 
                                         dataTableOutput("table_minsa"))
                              )
                            )
                          )
                 ),
                 tabPanel("Mapa departamental",
                          sidebarLayout(
                            sidebarPanel(position = "left", width = 3, 
                                         selectInput("selectedvariable_map",
                                                     label = "Variable: ", 
                                                     choices = c("Casos sospechosos"   = "cases",
                                                                 "Muertes sospechosas" = "deaths"))
                            ),
                            mainPanel(
                              highchartOutput("hchart", height = "600px")
                            )
                          )
                 ), 
                 tabPanel("Info",
                          includeMarkdown("info.Rmd"))
)

# 3. Server ---------------------------------------------------------------

server <- function(input, output, session) {
  
  #-------------#
  #### Labels  ####
  
  y_label <- reactive({
    req(input$selectedvariable)
    if(input$selectedvariable == "cases"){
      y_label <- "Casos sospechosos"
    } else if (input$selectedvariable == "deaths"){
      y_label <- "Muertes sospechosas"
    }})
  
  # MINSA
  y_label_minsa <- reactive({
    req(input$selectedvariable_minsa)
    if(input$selectedvariable_minsa == "cases"){
      y_label <- "Casos confirmados"
    } else if (input$selectedvariable_minsa == "deaths"){
      y_label <- "Muertes confirmadas"
    }})  
  
  #-------------#
  #### BOXES  ####
  
  max_date <- df_country %>% 
    filter(date == max(date))
  
  max_casos   <- max_date$cases
  max_muertes <- max_date$deaths
  max_fecha   <- max_date$date
  
  output$vbox <- renderValueBox({
    valueBox("Datos actualizados al:", format(max_fecha, "%b %d %Y"))
  })
  
  output$vbox1 <- renderValueBox({
    valueBox("Total de casos sospechosos:", format(max_casos, big.mark = ","))
  })
  
  output$vbox2 <- renderValueBox({ 
    valueBox("Total de muertes sospechosas:", format(max_muertes, big.mark = ","))
  })
  
  # MINSA
  max_date_minsa <- df_minsa %>% 
    filter(date == max(date))
  
  max_casos_minsa   <- max_date_minsa$cases
  max_muertes_minsa <- max_date_minsa$deaths
  max_fecha_minsa   <- max_date_minsa$date
  
  output$vbox_minsa <- renderValueBox({
    valueBox( "Datos actualizados al:", format(max_fecha_minsa, "%b %d %Y"))
  })
  
  output$vbox1_minsa <- renderValueBox({
    valueBox( "Total de casos confirmados:", format(max_casos_minsa, big.mark = ","))
  })
  
  output$vbox2_minsa <- renderValueBox({ 
    valueBox("Total de muertes confirmadas:", format(max_muertes_minsa, big.mark = ","))
  })  
  
  #-------------#
  #### PLOT  ####
  output$plot <- renderPlotly({
    
    if (input$dataset == "state"){
      
      plot <- df %>% 
        filter(!is.na(cases)) %>% 
        filter(departamento %in% input$dep) %>%
        ggplot(aes(x = date, group = 1, color = departamento)) + 
        geom_line(aes_string(y = input$selectedvariable), size = 1.125) +
        scale_y_continuous(labels = comma) +
        scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%y")) + 
        labs(color = "",
             x = "Fecha",
             y = y_label()) + 
        theme_plex() + 
        theme(text = element_text(size = 16)) 
      
      if(input$logarithmicY)
        plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
      
      ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
      
    }
    
    else if (input$dataset == "country") {
      plot <- df_country %>% 
        ggplot(aes(x = date)) + 
        geom_line(aes_string(y = input$selectedvariable), size = 1.125) +
        scale_y_continuous(labels = comma) + 
        scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%y")) + 
        labs(x = "Fecha",
             y = y_label()) + 
        theme_plex() + 
        theme(text = element_text(size = 16)) 
      
      if(input$logarithmicY)
        plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
      
      ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
    }
    
  })
  
  # MINSA 
  output$plot_minsa <- renderPlotly({
    
    plot <- df_minsa %>% 
      ggplot(aes(x = date)) + 
      geom_line(aes_string(y = input$selectedvariable_minsa), size = 1.125) +
      scale_y_continuous(labels = comma) + 
      scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%y")) + 
      labs(x = "Fecha",
           y = y_label_minsa()) + 
      theme_plex() + 
      theme(text = element_text(size = 16)) 
    
    if(input$logarithmicY_minsa)
      plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
    
    ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
    
    
  })  
  
  #-------------#
  #### PLOT 2 ####
  output$plot2 <- renderPlotly({
    
    if (input$dataset == "state"){
      
      plot <- df %>% 
        group_by(departamento) %>% 
        mutate(
          cases2 = cases - lag(cases), 
          deaths2 = deaths - lag(deaths)
        ) %>% 
        mutate(
          cases2 = ifelse(date == "2020-03-18", cases, cases2),
          deaths2 = ifelse(date == "2020-03-18", deaths, deaths2)
        ) %>% 
        select(date, departamento, cases2, deaths2) %>% 
        rename(cases = cases2, deaths = deaths2) %>% 
        ungroup() %>% 
        filter(departamento %in% input$dep) %>%
        ggplot(aes(x = date, group = departamento, fill = departamento)) + 
        geom_col(aes_string(y = input$selectedvariable), width = 1, alpha = 0.9) +
        scale_y_continuous(labels = comma) +
        scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%y")) + 
        labs(fill = "",
             x = "Fecha",
             y = y_label()) + 
        theme_plex() + 
        theme(text = element_text(size = 16)) 
      
      if(input$logarithmicY)
        plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
      
      ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
      
    }
    
    else if (input$dataset == "country") {
      plot <- df_country %>% 
        mutate(
          cases2 = cases - lag(cases), 
          deaths2 = deaths - lag(deaths)
        ) %>% 
        mutate(
          cases2 = ifelse(date == "2020-03-18", cases, cases2),
          deaths2 = ifelse(date == "2020-03-18", deaths, deaths2)
        ) %>% 
        select(date, cases2, deaths2) %>% 
        rename(cases = cases2, deaths = deaths2) %>% 
        ggplot(aes(x = date)) + 
        geom_col(aes_string(y = input$selectedvariable), width = 1, fill = "gray", alpha = 0.9) +
        scale_y_continuous(labels = comma) + 
        scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%y")) + 
        labs(x = "Fecha",
             y = y_label()) + 
        theme_plex() + 
        theme(text = element_text(size = 16)) 
      
      if(input$logarithmicY)
        plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
      
      ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
    }
    
  })  
  
  #MINSA
  output$plot2_minsa <- renderPlotly({
    
    plot <- df_minsa %>% 
      mutate(
        cases2 = cases - lag(cases), 
        deaths2 = deaths - lag(deaths)
      ) %>% 
      mutate(
        cases2 = ifelse(date == "2020-03-19", cases, cases2),
        deaths2 = ifelse(date == "2020-03-19", deaths, deaths2)
      ) %>% 
      select(date, cases2, deaths2) %>% 
      rename(cases = cases2, deaths = deaths2) %>% 
      ggplot(aes(x = date)) + 
      geom_col(aes_string(y = input$selectedvariable_minsa), width = 1, fill = "gray", alpha = 0.9) +
      scale_y_continuous(labels = comma) + 
      scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%b/%y")) + 
      labs(x = "Fecha",
           y = y_label_minsa()) + 
      theme_plex() + 
      theme(text = element_text(size = 16)) 
    
    if(input$logarithmicY_minsa)
      plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
    
    ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
    
    
  })    
  
  #-------------#
  #### TABLE ####
  tableData <- reactive({
    if (input$dataset == "state"){
      
      df %>% 
        select(departamento, date, input$selectedvariable) %>% 
        mutate(source = "Observatorio Ciudadano COVID-19") %>% 
        select(source, date, departamento, input$selectedvariable) %>% 
        filter(., departamento %in% input$dep) 
      
    }
    
    else if (input$dataset == "country") {
      df_country %>% 
        mutate(source = "Observatorio Ciudadano COVID-19") %>% 
        select(source, date, input$selectedvariable)
      
    }
  })
  
  # MINSA
  tableData_minsa <- reactive({
    
    df_minsa %>% 
      ungroup() %>% 
      mutate(source = "MINSA") %>% 
      select(source, date, input$selectedvariable_minsa, -id)
    
  })  
  
  output$table <- renderDT( #renderDataTable(
    tableData(),
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(
      "pageLength" = 20)
  )  
  
  # MINSA
  output$table_minsa <- renderDT( #renderDataTable(
    tableData_minsa(),
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(
      "pageLength" = 20)
  )
  
  #----------------#
  #### DOWNLOAD ####
  
  output$data.csv <- downloadHandler(
    filename = function() {
      paste("covd-nic-observatorio-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    }
  )
  
  #MINSA
  output$data_minsa.csv <- downloadHandler(
    filename = function() {
      paste("covd-nic-minsa-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tableData_minsa(), file, row.names = FALSE)
    }
  )
  
  #----------------#
  #### MAPAS ####
  
  # Map data
  # mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/ni/ni-all.js"))  %>% 
  #   write_rds("mapdata.rds")
  
  # MINSA
  y_label_map <- reactive({
    req(input$selectedvariable_map)
    if(input$selectedvariable_map == "cases"){
      y_label <- "Casos sospechosos"
    } else if (input$selectedvariable_map == "deaths"){
      y_label <- "Muertes sospechosas"
    }})  
  
  # Prepare data
  last_day <- df %>% 
    group_by(departamento) %>% 
    filter(date == max(date)) %>% 
    mutate(name = departamento) 
  
  # Codes to merge
  codes <- mapdata %>% 
    select(code = `hc-a2`, `name`) %>% 
    mutate(
      name = ifelse(code == "AS", "RACCS", name),
      name = ifelse(code == "AN", "RACCN", name),
      name = ifelse(code == "RS", "Río San Juan", name),
      departamento = name, 
    ) %>% 
    left_join(last_day) 
  
  # Chart
  output$hchart <- renderHighchart(
    hcmap("countries/ni/ni-all", 
          data = codes,
          value = input$selectedvariable_map,
          joinBy = c("hc-a2", "code"),
          dataLabels = list(enabled = TRUE,  format = "{point.departamento}"),
          borderColor = "#AFAFAF",
          borderWidth = 1
    ) %>% 
      hc_tooltip(headerFormat = "") %>% 
      hc_title(text = paste0(y_label_map())) %>% 
      hc_subtitle(text = paste0("Actualizado al ", format(max_fecha, "%d %b %Y"))) %>% 
      hc_caption(text = "Dato: Obsevatorio Ciudadano COVID-19.") %>% 
      hc_mapNavigation(enabled = T) %>%   
      hc_exporting(enabled = TRUE, filename = "custom")
    
  )
}


# 4. App ------------------------------------------------------------------

shinyApp(ui, server)
