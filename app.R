library(shiny)
library(shinyjs)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
   # Application title
   titlePanel("INVENTARIO"),
   sidebarLayout(
     sidebarPanel(
       radioButtons("dist", "Totales:",
                    c("Format" = "forma",
                      "Edicion" = "edition",
                      "Year" = "years")),
       DT::dataTableOutput("mytable")
     ),
     mainPanel(
       tabsetPanel(
         tabPanel("Peliculas",
                  DT::dataTableOutput("tablePelis")),
         tabPanel("Carga",
                  fileInput("file1", "Choose CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  verbatimTextOutput("carga")),
         tabPanel("Estadistica", "Graficos")
       )
     )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtro <- reactive({
    pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    s = input$mytable_rows_selected
    
    if (length(s)) {
      switch (input$dist,
              "forma" = {
                formatos.df <- as.data.frame(table(pelis$Formato))
                colnames(formatos.df) = c("Formato", "Cantidad")
                formatos <- formatos.df
                
                formatoOK <- as.character(formatos[s, "Formato"])
                
                pelisFormato <- pelis[which(pelis$Formato == formatoOK), ]
                pelis <- pelisFormato
              },
              "edition" = {
                ediciones.df <- as.data.frame(table(pelis$Edicion))
                colnames(ediciones.df) = c("Edicion", "Cantidad")
                ediciones <- ediciones.df
                
                edicionOK <- as.character(ediciones[s, "Edicion"])
                
                pelisEdicion <- pelis[which(pelis$Edicion == edicionOK), ]
                pelis <- pelisEdicion
              },
              "years" = {
                years.df <- as.data.frame(table(pelis$Year))
                colnames(years.df) = c("Year", "Cantidad")
                years <- years.df
                
                yearOK <- as.character(years[s, "Year"])
                
                pelisYear <- pelis[which(pelis$Year == yearOK), ]
                pelis <- pelisYear
              }
              )
      
      return(pelis) 
      
      #El siguiente paso seria poder filtrar aplicando los filtros a la vez
      # hacer mas bonito el sitio con imagenes y tal
      #controlar la carga de fichero (permitir add nuevas pelis, controlar duplicados)
      #add graficos bonitos
      #intentar hacer login y control de usuarios
      
    }
    else {
      pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    }
  })
  
  cargafichero <- reactive({
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    csvLectura <- read.csv(file$datapath, header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    if(ncol(csvLectura) < 5 || ncol(csvLectura) > 9) {
      cat("Debe haber minimo 5 columnas y maximo 9")
    }else{
      cat("Todo ok")
    }
    
    
    for(i in names(csvLectura)){ 
      cat(i)
      switch (i,
              Titulo = {print("ok")},
              Formato = {print("ok")},
              Edicion = {print("ok")},
              discos = {print("ok")},
              Director = {print("ok")},
              Year = {print("ok")},
              Genero = {print("ok")},
              Observaciones = {print("ok")},
              Localizacion = {print("ok")},
              {flagColumnOK <- FALSE
              
              }
      )
      #add the operation to be applied here
    }
  })
  
  output$tablePelis = DT::renderDataTable({
    filtro()
  })

#Uso de la libreria DT para formatear tabla de salida (Paginacion, busqueda, ordenacion)
    
   output$mytable = DT::renderDataTable(selection = 'single', {
     pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
     
     dist <- switch(input$dist,
                  forma = {formatos.df <- as.data.frame(table(pelis$Formato))
                  colnames(formatos.df) = c("Formato", "Cantidad")
                  pelis <- formatos.df},
                  
                  edition = {edicion.df <- as.data.frame(table(pelis$Edicion))
                  colnames(edicion.df) = c("Edicion", "Cantidad")
                  pelis <- edicion.df},
                  
                  years = {year.df <- as.data.frame(table(pelis$Year))
                  colnames(year.df) = c("Year", "Cantidad")
                  pelis <- year.df})

   })
   
   output$carga = renderPrint({
     cargafichero()

   })
}

# Run the application 
shinyApp(ui = ui, server = server)
