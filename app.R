library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("INVENTARIO"),
   # Show a plot of the generated distribution
   mainPanel(
     tabsetPanel(
       tabPanel("Summary",
          fileInput("file1", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          )),
       tabPanel("Table",
             radioButtons("dist", "Totales:",
                          c("All" = "none",
                            "Format" = "forma",
                            "Edicion" = "edition",
                            "Year" = "years")),
                DT::dataTableOutput("mytable")
                )
     )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
       # generate bins based on input$bins from ui.R
       x    <- faithful[, 2]
       bins <- seq(min(x), max(x), length.out = input$bins + 1)
       # draw the histogram with the specified number of bins
       hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
    output$value <- renderText({ input$caption })

#Uso de la libreria DT para formatear tabla de salida (Paginacion, busqueda, ordenacion)
    
   output$mytable = DT::renderDataTable({

     pelis <- read.csv("InventarioPeliculas.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")

     # si no hay datos en el fichero se habilita el control para cargarlo
     if (nrow(pelis) == 0 || is.null(pelis)) {
       file <- input$file1
       ext <- tools::file_ext(file$datapath)
       req(file)
       validate(need(ext == "csv", "Please upload a csv file"))

       pelis <- read.csv(file$datapath, header = TRUE, sep = ";", dec = ".", stringsAsFactors=FALSE, fileEncoding="latin1")

     }else{
       #deshabilitar inputfile

       observe({
         shinyjs::disable("file1", length(input$selector) %in% 0:4)
       })
     }

   dist <- switch(input$dist,
                  forma = {formatos.df <- as.data.frame(table(pelis$Formato))
                  colnames(formatos.df) = c("Formato", "Cantidad")
                  pelis <- formatos.df},
                  edition = {edicion.df <- as.data.frame(table(pelis$Edicion))
                  colnames(edicion.df) = c("Edicion", "Cantidad")
                  pelis <- edicion.df},
                  years = {year.df <- as.data.frame(table(pelis$Year))
                  colnames(year.df) = c("Year", "Cantidad")
                  pelis <- year.df},
                  none = pelis)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
