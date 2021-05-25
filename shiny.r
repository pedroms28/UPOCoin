#Lista de paquetes necesarios
packages = c("tidyverse",
             "shiny",
             "data.table",
             "shinydashboard",
             "zoo",
             "corrplot")

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
coins <- coins[, -1]

#Almacenamos los nombres de las criptomonedas y sus símbolos
names_coins <- as.vector(coins$Name[!duplicated(coins$Name)])
symbols_coins <- as.vector(coins$Symbol[!duplicated(coins$Symbol)])

#Juntamos los nombres con sus símbolos (Ejemplo: Aave (AAVE))
symbols_and_names <- str_c(names_coins, " (", symbols_coins, ")")

#Almacenamos las fechas inicio y final de todas las criptos
coins_dd <- split(coins, coins$Name)
min_date_in_common <- as.Date("1970-01-01")
data_with_date_in_common <- coins_dd
#Convierto el campo Date a tipo Date y calculo fecha en la que todas las criptomonedas están monitorizándose
for (i in 1:length(coins_dd)) {
  coins_dd[[i]] <-
    transform(coins_dd[[i]], Date = as.Date(coins_dd[[i]]$Date))
  min_date_in_common <-
    max(c(min(coins_dd[[i]]$Date), min(min_date_in_common)))
}

for (i in 1:length(data_with_date_in_common)) {
  data_with_date_in_common[[i]] <-
    subset(data_with_date_in_common[[i]], Date >= min_date_in_common)
}
max_date_in_common <- tail(data_with_date_in_common[[1]]$Date, 1)

#Interfaz gráfica
ui <- dashboardPage(
  dashboardHeader(title = "UPOCoin"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Visualización",
        tabName = "dashboard",
        icon = icon("chart-line")
      ),
      menuItem(
        "Predicciones",
        tabName = "predicts",
        icon = icon("search-dollar")
      ),
      menuItem(
        "Agrupamientos",
        tabName = "groups",
        icon = icon("object-group")
      ),
      menuItem(
        "Agrupamiento por diferencia",
        tabName = "groupsVariance",
        icon = icon("object-group")
      ),
      menuItem(
        "Correlación",
        tabName = "correlation",
        icon = icon("people-arrows")
      )
    )
  ),
  
  dashboardBody(tabItems(
    #Primera pestaña
    tabItem(tabName = "dashboard",
            fluidRow(
              h2("Visualización de datos"),
              box(width = 9,
                  #Gráfica
                  plotOutput("plot1")),
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
                ),
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "column",
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                )
              )
            )),
    
    #Segunda pestaña
    tabItem(tabName = "predicts",
            fluidRow(
              h2("Pestaña de predicciones"),
              box(
                width = 9,
                #Gráfica
                plotOutput("plotPredict"),
                htmlOutput("prediccion"),
                verbatimTextOutput("summary", placeholder = FALSE)
              ),
              
              box(
                width = 3,
                title = "Criptomonedas",
                #Checkbox múltiple para seleccionar la/s criptomoneda/s
                radioButtons(
                  inputId = "coinsPredict",
                  label = "Selecciona la/s criptomoneda/s:",
                  choiceValues = names_coins,
                  choiceNames = symbols_and_names,
                  #Por defecto seleccionada Bitcoin
                  selected = names_coins[3]
                ),
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "columnPredict",
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                ),
                #Slider con el numero de días para el aprendizaje:
                sliderInput(
                  "sliderLearner",
                  "1.Introduce el número de días para el aprendizaje",
                  min = 5,
                  max = 90,
                  value = 14
                ),
                #Slider con el numero de dias para la prediccion:
                sliderInput(
                  "sliderPredictor",
                  "1.introduce el número de días de predicción",
                  min = 1,
                  max = 5,
                  value = 1
                ),
                #Botón para lanzar la predicción:
                actionButton("executePredict", "Predecir")
              )
            )),
    #Tercera pestana
    tabItem(tabName = "groups",
            fluidRow(
              h2("Agrupamientos"),
              box(
                width = 9,
                #Gráfica
                plotOutput("plotCluster"),
                #plotPredict
                htmlOutput("cluster"),
                #prediccion
                verbatimTextOutput("summaryCluster", placeholder = FALSE)
              ),
              
              box(
                width = 3,
                title = "Criptomonedas",
                #Slider con el numero de grupos que se quiera crear:
                sliderInput(
                  "sliderCluster",
                  "1.introduce el numero de grupos que quieras crear",
                  min = 2,
                  max = 15,
                  value = 4
                ),
                #Slider con el numero de dias para la agrupacion:
                sliderInput(
                  "sliderClusterDays",
                  "2.introduce el numero de dias para la predicción",
                  min = 5,
                  max = 90,
                  value = 14
                ),
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "columnCluster",
                  #columnPredict
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                ),
                #Botón para lanzar la busqueda:
                actionButton("executeCluster", "Buscar"),
                textInput("texto", "3. Visualiza un cluster", placeholder = "Cluster ID"),
                actionButton("visual", "Visualizar")
              )
            )),
    #Cuarta pestana
    tabItem(tabName = "groupsVariance",
            fluidRow(
              h2("Agrupamientos por diferencia"),
              box(
                width = 9,
                #Gráfica
                plotOutput("plotCluster2"),
                #plotPredict
                htmlOutput("cluster2"),
                #prediccion
                verbatimTextOutput("summaryCluster2", placeholder = FALSE)
              ),
              
              box(
                width = 3,
                title = "Criptomonedas",
                #Slider con el numero de grupos que se quiera crear:
                sliderInput(
                  "sliderCluster2",
                  "1.introduce el numero de grupos que quieras crear",
                  min = 2,
                  max = 15,
                  value = 4
                ),
                #Slider con el numero de dias para la agrupacion:
                sliderInput(
                  "sliderClusterDays2",
                  "2.introduce el numero de dias para la predicción",
                  min = 5,
                  max = 90,
                  value = 14
                ),
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "columnCluster2",
                  #columnPredict
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                ),
                #Botón para lanzar la busqueda:
                actionButton("executeCluster2", "Buscar"),
                textInput("texto2", "3. Visualiza un cluster", placeholder = "Cluster ID"),
                actionButton("visual2", "Visualizar")
              )
            )),
    #Quinta pestana
    tabItem(tabName = "correlation",
            fluidRow(
              h2("Correlación"),
              box(
                width = 9,
                #Gráfica
                plotOutput("plotCorrelation", width = "100%", height = "100%"),
                HTML(text = "<h3>Interpretacion de los datos</h3><p>El signo nos indica la dirección de la relación:<p><ul><li>un valor <b>positivo</b> indica una <b>relación directa o positiva</b></li><li>un valor <b>negativo</b> indica relación <b>indirecta, inversa o negativa</b></li><li>un valor <b>nulo</b> indica que <b>no existe una tendencia</b> entre ambas variables (puede ocurrir que no exista relación o que la relación sea más compleja que una tendencia).</li></ul><p>Para interpretar qué tan fuerte es la correlación podemos utilizar el criterio de Cohen, quien para valores absolutos indica que valores entre:<p><ul><li> <b>0.1-0.3</b> -> representa un efecto <b>pequeño</b></li><li> <b>0.3-0.5</b> -> representa un efecto <b>medio</b></li><li> <b>≥ 0.5</b> -> representa un efecto <b>grande</b></li></ul>")
              ),
              box(
                width = 3,
                title = "Opciones",
                dateRangeInput(
                  "daterange",
                  "Fechas para la correlación:",
                  start  = min_date_in_common,
                  end    = max_date_in_common,
                  min    = min_date_in_common,
                  max    = max_date_in_common,
                  format = "yyyy-mm-dd",
                  separator = " - "
                ),
                #Selector de columnas/atributos:
                selectInput(
                  inputId = "columnCorrelation",
                  #columnPredict
                  label = "Selecciona un atributo:",
                  #Solo seleccionamos las columnas High, Low, Open, Close, Volume y Marketcap
                  choices = names(coins)[4:9],
                  #Por defecto seleccionada la columna Close
                  selected = names(coins)[7]
                ),
                #Botón para lanzar la busqueda:
                actionButton("executeCorrelation", "Ejecutar")
              )
            ))
  ))
)

server <- function(input, output) {
  clusters <- reactiveValues(data = NULL)
  clusters2 <- reactiveValues(data = NULL)
  coins_list <- reactiveValues(data = NULL)
  coins_list2 <- reactiveValues(data = NULL)
  sliderLearner <- reactiveValues(data = NULL)
  sliderPredictor <- reactiveValues(data = NULL)
  columnPredict <- reactiveValues(data = NULL)
  dates <- reactiveValues(data = NULL)
  output$plot1 <-
    renderPlot({
      #Filtramos el dataset por nombre de criptomoneda (atributo Name) seleccionado/s en el checkbox
      df <-
        coins[grepl(paste(paste(paste0(
          "^", input$coins
        ), "$", sep = ""), collapse = "|"), coins$Name),]
      #Eje x -> fecha, eje y -> seleccionado con el input select
      ggplot(data = df, aes(
        x = Date,
        y = df[[input$column]],
        group = Name,
        color = Name
      )) +
        geom_line() + labs(y = input$column)
    })
  
  observeEvent(input$executePredict, {
    coin_selected <-
      coins[grepl(paste(paste(
        paste0("^", input$coinsPredict), "$", sep = ""
      ), collapse = "|"), coins$Name),]
    
    #Guardamos los valores de los inputs para que no cambie automáticamente, sólo cuando se esté observando el evento
    sliderLearner$data <- input$sliderLearner
    sliderPredictor$data <- input$sliderPredictor
    columnPredict$data <- input$columnPredict
    
    coin_selected$Date <-
      as.numeric(as.Date(coin_selected$Date))
    
    rule <- str_c(input$columnPredict, " ~ Date")
    
    modeloPredictivo <-
      lm(rule,
         data = tail(coin_selected, input$sliderLearner))
    
    next_day <- tail(coin_selected$Date, 1) + 1
    
    nuevo <-
      data.frame(Date = c(next_day:(next_day + sliderPredictor$data - 1)))
    
    prediccion <-
      predict(object = modeloPredictivo,
              newdata = nuevo,
              interval = "prediction")
    
    output$prediccion <- renderText({
      string <-
        "<style>
      table,
      th,
      td {
        padding: 10px;
        border: 1px solid black;
        border-collapse: collapse;
      }
    </style>
        <h3>La prediccion para el/los día/s</h3><table><tr><th>Días</th>
    <th>Prediccion</th>"
      for (i in 1:sliderPredictor$data) {
        date <- as.Date(nuevo[[1]][i])
        pred <- round(prediccion[[i]], 2)
        string <-
          str_c(string,
                "<tr><td>",
                date,
                "</td><td> ",
                pred,
                "</td></tr>",
                collapse = NULL)
      }
      string <- str_c(string, "</table><br><br>")
    })
    
    output$summary <- renderPrint({
      print("Resumen del modelo predictivo:")
      summary(modeloPredictivo)
    })
    
    output$plotPredict <-
      renderPlot({
        # 2. Regression line + confidence intervals
        nuevo$Date <- as.Date(nuevo$Date)
        coin_selected$Date <- as.Date(coin_selected$Date)
        coin_selected <- tail(coin_selected, sliderLearner$data)
        prediccion <- as.data.frame(prediccion)
        p <-
          ggplot(coin_selected, aes(x = Date, y = coin_selected[[columnPredict$data]])) +
          geom_point() +
          labs(y = columnPredict$data) +
          stat_smooth(method = lm) +
          geom_line(aes(y = min(prediccion$lwr)), color = "red", linetype = "dashed") +
          geom_line(aes(y = max(prediccion$upr)), color = "red", linetype = "dashed")
        
        
        for (i in 1:sliderPredictor$data) {
          p <-
            p + annotate(
              geom = "point",
              x = nuevo$Date[i],
              y = prediccion$fit[i],
              color = "blue"
            )
        }
        
        p
      })
    
    if (columnPredict$data %in% c("Close", "Open", "Low", "High")) {
      if (prediccion[[1]] > (tail(coin_selected[[columnPredict$data]], 1) * 1.05)) {
        showNotification("Se recomienda comprar", type = "warning")
      } else if (prediccion[[1]] < (tail(coin_selected[[columnPredict$data]], 1) *
                                    0.95)) {
        showNotification("Se recomienda vender", type = "warning")
      }
    }
    
  })
  
  #Tercera
  observeEvent(input$executeCluster, {
    coin_cluster <- coins
    
    coin_cluster =  split(coin_cluster, coin_cluster$Name)
    
    coins_list$data <- data.frame()
    
    for (i in 1:length(coin_cluster)) {
      coin_cluster[[i]] <-
        tail(coin_cluster[[i]], input$sliderClusterDays)
      coin_cluster[[i]] <-
        subset(coin_cluster[[i]], select = c("Date", input$columnCluster))
      coin_cluster[[i]]$Date <- as.Date(coin_cluster[[i]]$Date)
      coin_cluster[[i]] <-
        transpose(make.names = "Date", coin_cluster[[i]])
      coins_list$data <- rbind(coins_list$data, coin_cluster[[i]])
    }
    
    clusters$data <-
      kmeans(coins_list$data, input$sliderCluster, nstart = 25)
    if (!is.null(clusters$data)) {
      showNotification("Algoritmo ejecutado.", type = "message")
    }
    
  })
  observeEvent(input$visual, {
    output$summaryCluster = renderPrint({
      print("Resumen del Cluster: ")
      summary(coins_list$data[clusters$data$cluster == input$texto,])
    })
    
    output$plotCluster <-
      
      renderPlot({
        c <- coins_list$data
        c$Name <- names_coins
        c$Group <- clusters$data$cluster
        c <- c[c$Group == input$texto,]
        c$Group <- NULL
        d <- melt(data = c, id.vars = "Name")
        ggplot(data = d, aes(
          x = variable,
          y = value,
          group = Name,
          color = Name
        )) +
          geom_line() + labs(y = input$columnCluster, x = "Date")
      })
    
  })
  
  #Cuarta
  observeEvent(input$executeCluster2, {
    coin_cluster <- coins
    
    coin_cluster =  split(coin_cluster, coin_cluster$Name)
    
    coins_list2$data <- data.frame()
    
    for (i in 1:length(coin_cluster)) {
      coin_cluster[[i]] <-
        subset(coin_cluster[[i]], select = c("Date", input$columnCluster2))
      
      coin_cluster[[i]]$copia <-
        lag(coin_cluster[[i]][[input$columnCluster2]])
      
      coin_cluster[[i]] <-
        tail(coin_cluster[[i]], input$sliderClusterDays2)
      
      coin_cluster[[i]][[input$columnCluster2]] <-
        coin_cluster[[i]][[input$columnCluster2]] - coin_cluster[[i]]$copia
      
      coin_cluster[[i]]$copia <- NULL
      
      coin_cluster[[i]]$Date <- as.Date(coin_cluster[[i]]$Date)
      
      coin_cluster[[i]] <-
        transpose(make.names = "Date", coin_cluster[[i]])
      
      coins_list2$data <- rbind(coins_list2$data, coin_cluster[[i]])
    }
    
    coins_list2$data <- scale(coins_list2$data)
    clusters2$data <-
      kmeans(coins_list2$data, input$sliderCluster2, nstart = 25)
    if (!is.null(clusters2$data)) {
      showNotification("Algoritmo ejecutado.", type = "message")
    }
    
  })
  
  observeEvent(input$visual2, {
    output$summaryCluster2 = renderPrint({
      print("Resumen del Cluster: ")
      summary(coins_list2$data[clusters2$data$cluster == input$texto2,])
    })
    
    output$plotCluster2 <-
      
      renderPlot({
        c2 <- as.data.table(coins_list2$data)
        c2$Name <- names_coins
        c2$Group <- clusters2$data$cluster
        c2 <- c2[c2$Group == input$texto2,]
        c2$Group <- NULL
        d <- melt(data = c2, id.vars = "Name")
        ggplot(data = d, aes(
          x = variable,
          y = value,
          group = Name,
          color = Name
        )) +
          geom_line() + labs(y = input$columnCluster, x = "Date")
      })
    
  })
  
  #Quinto
  observeEvent(input$executeCorrelation, {
    dates$data <- input$daterange
    temp <- coins
    temp$Date <- as.Date(temp$Date, format = "%Y-%m-%d")
    temp <-
      subset(temp, Date >= dates$data[1] & Date <= dates$data[2])
    temp <-
      subset(temp, select = c("Name", input$columnCorrelation))
    temp <- split(temp, temp$Name)
    for (i in 1:length(temp)) {
      temp[i] <- subset(temp[[i]], select = c(input$columnCorrelation))
    }
    correlaciones <- cor(as.data.table(temp))
    
    output$plotCorrelation <- renderPlot({
      corrplot.mixed(
        correlaciones,
        lower.col = "black",
        number.cex = .7,
        tl.pos = 'lt',
        tl.cex = .6,
        tl.col = 'blue'
      )
    }, height = 600, width = 600)
  })
}

shinyApp(ui, server)