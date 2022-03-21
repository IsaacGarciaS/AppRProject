library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(ggplot2)
library(slickR)


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "superhero"),
  shinyjs::useShinyjs(),
  title = "INVENTARIO",
  
  #añadimos div, en lugar del mainPanel para que se ajuste a todo lo ancho
     div(
       slickROutput("slick_output",width='100%',height='50%'),
       tabsetPanel(id = "generalPanel",
         # tabPanel("Home", titlePanel("INVENTARIO"),
         #          
         #          ),
         tabPanel("Peliculas", "Filtros:",
                  sidebarLayout(
                    sidebarPanel (
                      DT::dataTableOutput("formato"),
                      DT::dataTableOutput("edicion"),
                      DT::dataTableOutput("year")
                    )
                    ,
                    mainPanel(
                      fluidRow(
                        column(6, 
                               img(src = "Blu-Ray.png", height = 100, width = 200)
                        ),
                        column(6,
                               div(
                                 img(src = "DVD.png", height = 100, width = 200), style="text-align: right;"
                               )
                               
                        )
                      ),
                      DT::dataTableOutput("tablePelis"),
                      fluidRow(
                        column(6, 
                                img(src = "00-Twenty-Century-Fox.jpg", height = 180, width = 200)
                        ),
                        column(6,
                               div(
                                 img(src = "Logo_new_web.png", height = 160, width = 450), style="text-align: right;"
                               )
                               
                        )
                      )
                    )
                  )
         ),
         tabPanel("Carga",
                  fileInput("file1", "Choose CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  verbatimTextOutput("carga"),
                  DT::dataTableOutput("tablepeliscargadas"),
                  actionButton("btn", "Cargar", icon("fa-solid fa-upload"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                  
         tabPanel("Estadística", "Select items to see in graphic:",
                  radioButtons("dist", "",
                               c("Formato" = "forma",
                                 "Edición" = "edition",
                                 "Año" = "years")),
                  DT::dataTableOutput("mytable"),
                  plotOutput("estadistica"))
       ), class = "span7"
     ),
  div(print("IsaacGarcíaProductions© 2022"), style="text-align: right;")
  
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  updateTabsetPanel(session, "generalPanel", selected = "Peliculas")
  
  shinyjs::hide("btn")
  
  #se crea function para no duplicar codigo
  file_upload <- function(){
    #shinyjs::show("tablepeliscargadas")
    #shinyjs::show("carga")
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    csvLectura <- read.csv(file$datapath, header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    return(csvLectura)
  }
  
  datosCargados <- function(){
    #obtenemos los datos que se muestran en la tabla
    csvLectura <- file_upload()
  }
  
  
  #Una vez está el fichero cargado, y se muestra en pantalla la tabla con los datos. El boton cargar debe cargar esos datos
  #Para ello debe hacer write en el fichero csv
  
  observeEvent(input$btn, {
    
    pelis <- leerpelis()
    
    file_name <- 'InventarioPeliculas.csv'
    
    #llegados hasta aqui ya se han hecho las validaciones previas de los datos cargados
    datosTable <- datosCargados();
    
    #Add al fichero inicial las nuevas peliculas cargadas
    if(file.exists(file_name)){
      
      write.table(datosTable, file = file_name, sep = ";", row.names = FALSE, col.names = FALSE, 
                  fileEncoding = "latin1", append = TRUE, na = "", quote = FALSE, eol = "\r")
      shinyjs::hide("btn")
      
      # tras finalizar la carga volvemos al tap de las peliculas (recargar pagina)
      refresh()
    }
  })
  
# Reactives ---------------------------------------------------------------

  leerpelis <- reactive({
    pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    return(pelis)
  })
  
  filtropelis <- reactive({
    
    pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")
    
    #guardamos la selección de cada tabla
    seleccionformato = input$formato_rows_selected
    seleccionedicion = input$edicion_rows_selected
    seleccionyear = input$year_rows_selected
    
    #si no hay ninguno no se filtra nada 
    if (length(seleccionformato) | length(seleccionedicion) | length(seleccionyear)) {
      #si hay alguno marcado, hay que ir aplicando los filtros al total de pelis
      
      #Obtenemos selección
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
  })
  
  validacioncargafichero <- reactive({
    #se crea un reactivo muy similar a cargarPelis pero jugando con lo que se hace en cada uno y sobretodo con el return
    #en este caso se hace validacion y se muestran los mensajes al usuario

    csvLectura <- file_upload()

    if(ncol(csvLectura) < 5 || ncol(csvLectura) > 9) {
      cat("Debe haber minimo 5 columnas y maximo 9. Columnas actuales: \n")

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
    }else{
      #comprobamos duplicados en el titulo
      
      duplicados <- FALSE
      
      #creamos datafreame vacio para guardar los titulos repetidos
      repes <- data.frame(Titulo=character()) 
      
      for (row in 1:nrow(leerpelis())) {
        titulosExistentes <- leerpelis()[row, "Titulo"]
        
        for (newrow in 1:nrow(csvLectura)) {
          nuevoTitulo <- csvLectura[newrow, "Titulo"]
          
          if(titulosExistentes == nuevoTitulo) {
            duplicados <- TRUE
            repes[nrow(repes) + 1, ] = nuevoTitulo
          }
        }
      }
      
      if(!duplicados){
        cat("Todo ok")
      }else{
        #si hay duplicados no permitimos hacer la carga y los mostramos
        shinyjs::hide("btn")
        cat("Titulos repetidos: ")
        names(repes) <- NULL #quitamos header
        repes
      } 
    }
  })
  
  cargarPelis <- reactive({
    #en este caso si hay errores vaciamos dataframe, el otro reactivo se encarga de mostras los mensajes
    csvLectura <- file_upload()
    
      if(ncol(csvLectura) < 5 || ncol(csvLectura) > 9) {
         #si hay error de validacion se vacia el dataframe
        shinyjs::hide("btn")
         csvLectura <- csvLectura[, FALSE]
      }else{
        shinyjs::show("btn")
        
        csvLectura
      }
  })
  

# Fin Reactives -----------------------------------------------------------


# Outputs -----------------------------------------------------------------

  output$tablepeliscargadas = DT::renderDataTable({
      cargarPelis()
  })
  
  output$tablePelis = DT::renderDataTable({
    filtropelis()
  })

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
     validacioncargafichero()

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
  
  imgs_links <- list(
    "colectionBlu-ray.png",
    "colectionDVD.png")
  
  output$slick_output <- renderSlickR({
    
    photo_list <- lapply(imgs_links, function(x){
      tags$div(
        tags$img(src = x, width = "10%", height = "10%")
      )
    })
    
    imgs <- do.call(tagList, photo_list)
    slickR(imgs)
  })

# Fin Outputs -------------------------------------------------------------
}

# Run the application 
shinyApp(ui = ui, server = server)
