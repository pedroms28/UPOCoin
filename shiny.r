#Lista de paquetes necesarios
packages = c("tidyverse", "shiny", "data.table", "shinydashboard")

#Función que instala los paquetes si no están instalados y los carga
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
search()

#Unimos todos los CSVs en un único dataframe
filenames = list.files(path = "data/", full.names = TRUE)
coins <- rbindlist(lapply(filenames, fread))

#Eliminamos de columna SNo
coins <- coins[,-1]

#Almacenamos los nombres de las criptomonedas y sus símbolos
names_coins <- as.vector(coins$Name[!duplicated(coins$Name)])
symbols_coins <- as.vector(coins$Symbol[!duplicated(coins$Symbol)])

#Juntamos los nombres con sus símbolos (Ejemplo: Aave (AAVE))
symbols_and_names <- str_c(names_coins, " (", symbols_coins, ")")

#Interfaz gráfica
ui <- dashboardPage(
  dashboardHeader(title = "UPOCoin"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Visualización",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Predicciones", tabName = "Predicciones", icon = icon("th"))
  )),
  
  dashboardBody(tabItems(
    #Primera pestaña
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                width = 9,
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "column",
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                ),
                #Gráfica
                plotOutput("plot1")
              ),
              box(
                width = 3,
                title = "Criptomonedas",
                #Checkbox múltiple para seleccionar la/s criptomoneda/s
                checkboxGroupInput(
                  inputId = "coins",
                  label = "Selecciona la/s criptomoneda/s:",
                  choiceValues = names_coins,
                  choiceNames = symbols_and_names,
                  #Por defecto seleccionada Bitcoin
                  selected = names_coins[3]
                )
              )
            )),
    
    #Segunda pestaña
    tabItem(tabName = "predicts",
            h2("Widgets tab content"))
  ))
)

server <- function(input, output) {
    output$plot1 <-
      renderPlot({
        #Filtramos el dataset por nombre de criptomoneda (atributo Name) seleccionado/s en el checkbox
        df <-
          coins[grepl(paste(paste(paste0(
            "^", input$coins
          ), "$", sep = ""), collapse = "|"), coins$Name), ]
        #Eje x -> fecha, eje y -> seleccionado con el input select
        ggplot(data = df, aes(
          x = Date,
          y = df[[input$column]],
          group = Name,
          color = Name
        )) +
          geom_line() + labs(y = input$column)
      })
  }

shinyApp(ui, server)