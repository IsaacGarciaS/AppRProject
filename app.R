library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = bs_theme(version = 4, bootswatch = "darkly"),
  shinyjs::useShinyjs(),
  
   # Application title
   titlePanel("INVENTARIO"),
   sidebarLayout(
     sidebarPanel(
       DT::dataTableOutput("formato"),
       DT::dataTableOutput("edicion"),
       DT::dataTableOutput("year")
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
         tabPanel("Estadistica", "Graficos",
                  radioButtons("dist", "Totales:",
                               c("Format" = "forma",
                                 "Edicion" = "edition",
                                 "Year" = "years")),
                  DT::dataTableOutput("mytable"),
                  plotOutput("estadistica"))
       )
     )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  leerpelis <- reactive({
    pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    return(pelis)
  })
  
  filtropelis <- reactive({
    
    pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    #guardamos la seleccion de cada tabla
    seleccionformato = input$formato_rows_selected
    seleccionedicion = input$edicion_rows_selected
    seleccionyear = input$year_rows_selected
    
    #si no hay ninguno no se filtra nada 
    if (length(seleccionformato) | length(seleccionedicion) | length(seleccionyear)) {
      #si hay alguno marcado, hay que ir aplicando los filtros al total de pelis
      
      #Obtenemos seleccion
      formatos.df <- as.data.frame(table(pelis$Formato))
      colnames(formatos.df) = c("Formato", "Cantidad")
      formatos <- formatos.df
      
      formatoOK <- as.character(formatos[seleccionformato, "Formato"])
      
      if(!length(formatoOK)) formatoOK <- ""
      
      ediciones.df <- as.data.frame(table(leerpelis()$Edicion))
      colnames(ediciones.df) = c("Edicion", "Cantidad")
      ediciones <- ediciones.df

      edicionOK <- as.character(ediciones[seleccionedicion, "Edicion"])
      
      if(!length(edicionOK)) edicionOK <- ""
      
      years.df <- as.data.frame(table(leerpelis()$Year))
      colnames(years.df) = c("Year", "Cantidad")
      years <- years.df

      yearOK <- as.character(years[seleccionyear, "Year"])
      
      if(!length(yearOK)) yearOK <- ""
      #Fin obtener seleccion
      
      #guardamos la seleccion
      seleccion <- c(formatoOK, edicionOK, yearOK)
      
      if (seleccion[1:0] != "") {
        pelis <- pelis[which(pelis$Formato == formatoOK), ]
      }
      
      if (seleccion[2:2] != "") {
        pelis <- pelis[which(pelis$Edicion == edicionOK), ]
      }
      
      if (seleccion[3:3] != "") {
        pelis <- pelis[which(pelis$Year == yearOK), ]
      }
      
    }else{
      leerpelis()
    }
    
    return(pelis)
      
      # hacer mas bonito el sitio con imagenes y tal
      #controlar la carga de fichero (permitir add nuevas pelis, controlar duplicados)
      #add graficos bonitos
      #intentar hacer login y control de usuarios
      
    #}
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
    filtropelis()
  })

#Uso de la libreria DT para formatear tabla de salida (Paginacion, busqueda, ordenacion)
    
   output$mytable = DT::renderDataTable({
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
   
   output$formato = DT::renderDataTable(selection = 'single', {

     formatos.df <- as.data.frame(table(leerpelis()$Formato))
     colnames(formatos.df) = c("Formato", "Cantidad")
     pelis <- formatos.df
     
   })
   
   output$edicion = DT::renderDataTable(selection = 'single', {

     edicion.df <- as.data.frame(table(leerpelis()$Edicion))
     colnames(edicion.df) = c("Edicion", "Cantidad")
     pelis <- edicion.df
   })
   
   output$year = DT::renderDataTable(selection = 'single', {

     year.df <- as.data.frame(table(leerpelis()$Year))
     colnames(year.df) = c("Year", "Cantidad")
     pelis <- year.df
   })
   
   output$estadistica <- renderPlot({
     s = input$mytable_rows_selected

     switch (input$dist,
             "forma" = {
               if (length(s)){
                 formatos.df <- as.data.frame(table(leerpelis()$Formato))
                 colnames(formatos.df) = c("Formato", "Cantidad")
                 formatos <- formatos.df
                 
                 data <- data.frame(
                   formato=formatos$Formato ,  
                   cantidad=formatos$Cantidad)
                 
                 ggplot(data[s, , drop = FALSE], aes(x=formato, y=cantidad, fill = formato)) + 
                   geom_bar(stat='identity') +
                   scale_fill_hue(c = 40) +
                   theme(legend.position="none")
               } 
             },
             "edition" = {
               if (length(s)){
                 ediciones.df <- as.data.frame(table(leerpelis()$Edicion))
                 colnames(ediciones.df) = c("Edicion", "Cantidad")
                 ediciones <- ediciones.df
                 
                 data <- data.frame(
                   edicion=ediciones$Edicion ,  
                   cantidad=ediciones$Cantidad)
                 
                 ggplot(data[s, , drop = FALSE], aes(x=edicion, y=cantidad, fill = edicion)) + 
                   geom_bar(stat='identity') +
                   scale_fill_hue(c = 40) +
                   theme(legend.position="none")

               } 
             },
             "years" = {
               if (length(s)){
                 years.df <- as.data.frame(table(leerpelis()$Year))
                 colnames(years.df) = c("Year", "Cantidad")
                 years <- years.df
                 
                 data <- data.frame(
                   year=years$Year ,  
                   cantidad=years$Cantidad)
                 
                 ggplot(data[s, , drop = FALSE], aes(x=year, y=cantidad, fill = year)) + 
                   geom_bar(stat='identity') +
                   scale_fill_hue(c = 80) +
                   theme(legend.position="none")
                 
               } 
             }
        )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
