shinyServer(function(input, output, session) {
    


# Labels ------------------------------------------------------------------

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
    
# Boxes -------------------------------------------------------------------

    ## Observatorio ----
    max_date <- df_country %>% 
        filter(date == max(date))
    
    max_casos   <- max_date$cases
    max_muertes <- max_date$deaths
    max_fecha   <- max_date$date
    
    output$vbox1 <- renderInfoBox({
        infoBox(
            title = tags$p(format(max_casos, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Casos sospechosos", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })
    
    output$vbox2 <- renderInfoBox({ 
        infoBox(
            title = tags$p(format(max_muertes, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Muertes sospechosas", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })
    
    ## MINSA ----
    max_date_minsa <- df_minsa %>% 
        filter(date == max(date))
    
    max_casos_minsa   <- max_date_minsa$cases
    max_muertes_minsa <- max_date_minsa$deaths
    max_fecha_minsa   <- max_date_minsa$date
    
    output$vbox1_minsa <- renderInfoBox({
        infoBox(
            title = tags$p(format(max_casos_minsa, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Casos confirmados", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })
    
    output$vbox2_minsa <- renderInfoBox({ 
        infoBox(
            title = tags$p(format(max_muertes_minsa, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Muertes confirmadas", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })
    


# Acumulado ---------------------------------------------------------------

    output$plot <- renderPlotly({
        
        ## Departmental ----
        
        if (input$dataset == "state"){
            
            plot <- df %>% 
                filter(!is.na(cases)) %>% 
                filter(departamento %in% input$dep) %>%
                ggplot(aes(x = date, group = 1, color = departamento)) + 
                geom_line(aes_string(y = input$selectedvariable), size = 1.125) +
                scale_y_continuous(labels = comma) +
                scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
                labs(color = "",
                     x = "Fecha",
                     y = y_label(),
                     title = y_label()) 
            
            if(input$logarithmicY) {
                plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
            }
            
            ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
            
        }
        
        ## Country level ----
        
        else if (input$dataset == "country") {
            plot <- df_country %>% 
                ggplot(aes(x = date)) + 
                geom_line(aes_string(y = input$selectedvariable), size = 1.125) +
                scale_y_continuous(labels = comma) + 
                scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
                labs(x = "Fecha",
                     y = y_label(),
                     title = y_label()) 
            
            if(input$logarithmicY)
                plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
            
            ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
        }
        
    })
    
    ## MINSA ---- 
    output$plot_minsa <- renderPlotly({
        
        plot <- df_minsa %>% 
            ggplot(aes(x = date)) + 
            geom_line(aes_string(y = input$selectedvariable_minsa), size = 1.125) +
            scale_y_continuous(labels = comma) + 
            scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
            labs(x = "Fecha",
                 y = y_label_minsa(),
                 title = y_label_minsa()) 
        
        if(input$logarithmicY_minsa)
            plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
        
        ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
        
        
    })  
    

# Diario ------------------------------------------------------------------

    output$plot2 <- renderPlotly({
        
        ## Departmental ----
        
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
                geom_bar(aes_string(y = input$selectedvariable), 
                         stat = "identity",
                         width = 1, 
                         alpha = 0.9) +
                scale_y_continuous(labels = comma) +
                scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
                labs(fill = "",
                     x = "Fecha",
                     y = y_label(),
                     title = y_label()) 
            
            if(input$logarithmicY)
                plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
            
            ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
            
        }
        
        ## Country level ----
        
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
                geom_col(aes_string(y = input$selectedvariable), 
                         width = 1, 
                         fill = "grey",
                         alpha = 0.9) +
                scale_y_continuous(labels = comma) + 
                scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
                labs(x = "Fecha",
                     y = y_label(),
                     title = y_label()) 
            
            if(input$logarithmicY)
                plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
            
            ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
        }
        
    })  
    
    ## Minsa ----
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
            scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b/%Y")) + 
            labs(x = "Fecha",
                 y = y_label_minsa(),
                 title = y_label_minsa()) + 
            theme_ipsum_rc()
        
        if(input$logarithmicY_minsa)
            plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
        
        ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
        
        
    })    
    

# Table -------------------------------------------------------------------
    
    ## Observatorio ----
    
    tableData <- reactive({
        if (input$dataset == "state"){
            
            df %>% 
                select(departamento, date, input$selectedvariable) %>% 
                mutate(source = "Observatorio Ciudadano COVID-19") %>% 
                select(source, date, departamento, input$selectedvariable) %>% 
                filter(., departamento %in% input$dep) %>% 
                arrange(desc(date))
            
        }
        
        else if (input$dataset == "country") {
            df_country %>% 
                mutate(source = "Observatorio Ciudadano COVID-19") %>% 
                select(source, date, input$selectedvariable) %>% 
                arrange(desc(date))
            
        }
    })
    
    ## MINSA ----
    
    tableData_minsa <- reactive({
        
        df_minsa %>% 
            ungroup() %>% 
            mutate(source = "MINSA") %>% 
            select(source, date, input$selectedvariable_minsa, -id) %>% 
            arrange(desc(date))
        
    })  
    
    ## Observatorio ----
    output$table <- renderDT(
        tableData(),
        class = "display nowrap compact",
        filter = "top", 
        options = list(
            "pageLength" = 20)
    )  
    
    ## MINSA ----
    output$table_minsa <- renderDT( 
        tableData_minsa(),
        class = "display nowrap compact",
        filter = "top", 
        options = list(
            "pageLength" = 20)
    )
    

# Vaccines ----------------------------------------------------------------

    y_label_vacunas <- reactive({
        req(input$selectedvariable_vaccines)
        
        if(input$selectedvariable_vaccines == "total_vaccinations"){
            
            y_label <- "# de vacunas administradas"
        
        } else if (input$selectedvariable_vaccines == "people_vaccinated"){
            
            y_label <- "Personas vacunadas"
            
        } else if (input$selectedvariable_vaccines == "people_fully_vaccinated"){
         
            y_label <- "Personas completamente vacunadas"
               
        }
        
    })  
    
    output$plot_vacunas <- renderPlotly({
    
        plot <- vaccines %>%
        	filter(!is.na(!!rlang::sym(input$selectedvariable_vaccines))) %>%
            filter(location %in% c(input$countries, "Nicaragua")) %>% 
        	ggplot(aes(x = date, group = location, color = location)) +
        	geom_line(size = 1.2, alpha = 0.8, aes_string(y = input$selectedvariable_vaccines)) +
        	geom_point(alpha = 0.6, aes_string(y = input$selectedvariable_vaccines)) +
        	scale_y_continuous(labels = comma) +
        	scale_color_viridis_d(option = "turbo") +
        	scale_x_date(breaks = date_breaks("1 months"), labels = date_format("%m/%Y")) +
        	labs(
        		y = y_label_vacunas(),
        		x = "Fecha",
        		color = "",
        		title = y_label_vacunas()
        	)
        
        ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50)) 
    })    

    ## Descargar datos de vacunas ----
    tableData_vacunas <- reactive({
        
        vaccines %>% 
            ungroup() %>% 
            mutate(source = "Our World in Data") %>% 
            select(source, everything()) %>%
            arrange(desc(date))

    })  
    
    output$data_vacunas.csv <- downloadHandler(
        filename = function() {
            paste("vacunas-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableData_vacunas(), file, row.names = FALSE)
        }
    )
    

# Health Workers ----------------------------------------------------------
    ## BOXES ----
    max_date_workers <- workers %>% 
        filter(date == max(date))
    
    max_casos_workers   <- max_date_workers$cases
    max_muertes_workers <- max_date_workers$deaths

    output$vbox1_workers <- renderInfoBox({
        infoBox(
            title = tags$p(format(max_casos_workers, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Casos sospechosos", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })
    
    output$vbox2_workers <- renderInfoBox({ 
        infoBox(
            title = tags$p(format(max_muertes_workers, big.mark = ","), style = "font-size: 150%;"),
            subtitle = tags$p("Muertes sospechosas", style = "font-size: 120%;"),
            icon  = icon("users"),
            color = "black",
            fill = TRUE
        )
    })    
    
    ## Y LABEL WORKERS ----
    y_label_workers <- reactive({
        req(input$selectedvariable_workers)
        
        if(input$selectedvariable_workers == "cases"){
            
            y_label <- "Casos sospechosos"
            
        } else if (input$selectedvariable_workers == "deaths"){
            
            y_label <- "Muertes sospechosas"
            
        } 
    }) 
    
    ## PLOTS ----
    output$plot_workers <- renderPlotly({
        
        ## Country level ----
        plot <- workers %>% 
            ggplot(aes(x = date)) + 
            geom_line(aes_string(y = input$selectedvariable_workers), size = 1.125) +
            scale_y_continuous(labels = comma) + 
            scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m/%Y")) + 
            labs(
                x = "Fecha",
                y = y_label_workers(), 
                title = y_label_workers()
            ) 
        
        if(input$logarithmicY_workers)
            plot <- plot + scale_y_log10(labels = comma_format(accuracy = 1))
        
        ggplotly(plot, height = 600, width = 900, margin = list(b = 50, l = 50))
        
    })
    
    
    ## Descargar datos de vacunas ----
    tableData_workers <- reactive({
        
        workers %>% 
            ungroup() %>% 
            mutate(source = "Observatorio Ciudadano COVID-1") %>% 
            select(source, everything()) %>%
            arrange(desc(date))
        
    })  
    
    output$data_workers.csv <- downloadHandler(
        filename = function() {
            paste("health-workers-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(tableData_workers(), file, row.names = FALSE)
        }
    )    
    
# Download bottom ---------------------------------------------------------

    ## Observatorio ----
    output$data.csv <- downloadHandler(
        filename = function() {
            paste("covd-nic-observatorio-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(tableData(), file, row.names = FALSE)
        }
    )
    
    ## MINSA ----
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
    #   write_rds("data/mapdata.rds")
    
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
            departamento = name
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
            hc_caption(text = "Datos: Obsevatorio Ciudadano COVID-19.") %>% 
            hc_mapNavigation(enabled = T) %>%   
            hc_exporting(enabled = TRUE, filename = "custom")
        
    ) %>% 
        bindCache(input$selectedvariable_map)
})
