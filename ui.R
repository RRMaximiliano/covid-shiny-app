shinyUI(
	navbarPage(
		"COVID19 en Nicaragua",
		theme = shinytheme("paper"),
		header = tagList(
			useShinydashboard()
		),
    tabPanel("Observatorio Ciudadano COVID-19",
    				 sidebarLayout(
    				 	sidebarPanel(
    				 		position = "top", 
    				 		width = 3,
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
    				 									multiple = TRUE)),
    				 		selectInput("selectedvariable",
    				 								label = "Seleccione variable: ",
    				 								choices = c("Casos sospechosos"   = "cases",
    				 														"Muertes sospechosas" = "deaths")),
    				 		checkboxInput("logarithmicY", "Escala log", FALSE),
    				 		downloadButton("data.csv", "Descargar datos")
    				 		),
    				 	mainPanel(
    				 		fluidRow(
    				 			tags$style(".info-box.bg-black { background-color: #D3D3D3 !important; color: #000000 !important; }"),
    				 			infoBoxOutput(
    				 				"vbox1",
    				 				width = 4),
    				 			infoBoxOutput(
    				 				"vbox2",
    				 				width = 4)
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
    				 							 dataTableOutput("table")
    				 							 )
    				 			)
    				 		)
    				 	)
    				 ),
		tabPanel("MINSA",
						 sidebarLayout(
						 	sidebarPanel(position = "left", width = 3, 
						 							 selectInput("selectedvariable_minsa",
						 							 						label = "Seleccione variable: ", 
						 							 						choices = c("Casos confirmados"   = "cases",
						 							 												"Muertes confirmadas" = "deaths")),
						 							 checkboxInput("logarithmicY_minsa", "Escala log", FALSE),
						 							 downloadButton("data_minsa.csv", "Descargar datos")
						 							 ),
						 	mainPanel(
						 		fluidRow(
						 			infoBoxOutput(
						 				"vbox1_minsa",
						 				width = 4),
						 			infoBoxOutput(
						 				"vbox2_minsa",
						 				width = 4)
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
						 							 dataTableOutput("table_minsa")
						 							 )
						 			)
						 		)
						 	)
						 ),
		tabPanel("Mapa departamental COVID",
						 sidebarLayout(
						 	sidebarPanel(position = "left", width = 3, 
						 							 selectInput("selectedvariable_map",
						 							 						label = "Seleccione variable: ", 
						 							 						choices = c("Casos sospechosos"   = "cases",
						 							 												"Muertes sospechosas" = "deaths"))
                              ),
						 	mainPanel(
						 		highchartOutput(
						 			"hchart", 
						 			height = "600px"
						 			)
						 		)
						 	)
						 ), 
		tabPanel("Vacunas",
						 sidebarLayout(
						 	sidebarPanel(
						 		position = "left", width = 3, 
						 		selectInput("countries", 
						 								label = "Elige país a comparar con Nicaragua:", 
						 								selected = "Costa Rica",
						 								choices = vaccines$location %>% unique %>% sort %>% .[. != "Nicaragua"],
						 								multiple = TRUE),
						 		selectInput("selectedvariable_vaccines",
						 								label = "Seleccione variable:",
						 								choices = c("Total vacunas" = "total_vaccinations",
						 														"Personas vacunadas" = "people_vaccinated",
						 														"Personas completamente vacunadas" = "people_fully_vaccinated")),
						 		downloadButton("data_vacunas.csv", "Descargar datos"),
						 		tags$br(),
						 		tags$br(),
						 		tags$p("Notas: Los datos provienen de la compilación de datos de Our World in Data. Nicaragua todavía no presenta personas completamente vacunadas.")
						 ),
						 mainPanel(
						 	plotlyOutput("plot_vacunas")
						 )
					 )
					),
		tabPanel("Trabajadores de la Salud",
						 sidebarLayout(
						 	sidebarPanel(
						 		position = "left", width = 3 ,
						 		selectInput("selectedvariable_workers",
						 								label = "Selecciones variable:",
						 								choices = c("Casos sospechosos" = "cases",
						 														"Muertes sospechosas" = "deaths")),
						 		checkboxInput("logarithmicY_workers", "Escala log", FALSE),
						 		downloadButton("data_workers.csv", "Descargar datos"),
						 		tags$br(),
						 		tags$br(),
						 		tags$p("Notas: Las figuras presentan los casos y muertes sospechosas por COVID-19 en trabajadores de la salud en Nicaragua que fueron reportados al Observatorio Ciudadano COVID-19.")
						 	),
						 	mainPanel(
						 		fluidRow(
						 			infoBoxOutput(
						 				"vbox1_workers",
						 				width = 4),
						 			infoBoxOutput(
						 				"vbox2_workers",
						 				width = 4)
						 		),
						 		plotlyOutput("plot_workers")
						 	)
						 )
						),
		tabPanel("Info",
						 tags$head(
						 	tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
						 ),
						 includeMarkdown("info.Rmd")
						 )
		)
	)
