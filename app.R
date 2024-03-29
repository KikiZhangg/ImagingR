#
# This is a Shiny web application. You can run tinstall.packages("plotly")he application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:

#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(EBImage)
library(ijtiff)
library(dygraphs)
library(magrittr)
library(readr)
library(shinyWidgets)
library(bslib)
source("helpers.R")



# Define UI for application
ui <-fluidPage(
  tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  # add the name of the tab you want to use as title in data-value),
  
  navbarPage(
  title = "Time Series Dashboard",
  theme = bs_theme(bootswatch = "cyborg"),
  # title
  dashboardSidebar(),

  # Sidebar with a slider input for number of bins]
 
    dashboardBody(
          fluidRow(
            column(width = 3,
                   fileInput("imgFile", label = " Choose Image (.tif)", multiple = TRUE), # upload image file 
                   
                   radioButtons("channel_sel", label = "Select Channel to Display",
                                choices = list("1st Channel" = 1, "2nd Channel" = 2, "3rd Channel" = 3), 
                                selected = 1)
            ),
            
            column(width = 4,
             # selectInput("dataset",
             #              label = "Choose a dataset",
             #              choices = list("rst1","rst2"),
             #              selected = rst1
             #              ),
              airDatepickerInput("time", "Starting Time:",
                                timepicker = TRUE,
                                todayButton = TRUE,
                                clearButton = TRUE,
                                timepickerOpts = timepickerOptions(timeFormat = 'hh:ii')),
          
             
              sliderInput("minimum",
                          label = "Choose Minimum",
                          min = 5000,
                          max = 30000,
                          value = 10000,
                          step = 100
                          ),

              sliderInput("maximum",
                          label = "Choose Maximum",
                          min = 50000,
                          max = 400000,
                          value = 200000,
                          step = 100
                          )
            )
      
    ),

    # Show a plot/image
          fluidRow(
                box(width = 4, title = h6("Image Stack"), status = 'primary',
                   displayOutput("actualImage")
                   ),
               
                box(width = 8, 
                   dygraphOutput("dygraph")
               )

      ),
    
          fluidRow(
                box(width = 4, title = h6("Select ROI"), status = 'warning',
                  plotlyOutput("meanStack")
                ),

                box(width = 4, title = h6("ROI Info"),
                   downloadButton("downloadData", "Download Data"),
                   verbatimTextOutput("info")
               )
              )
            )
  )
)
  


  
  


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 600*1024^2)
  # show image file output
  output$files <- renderTable(input$imgFile)
  # reactive values
  glob <- reactiveValues(img = NULL, imgpath = "", nFrame = 1, nChannel = 1,
                         time = NULL, mean1 = list(), mean2 = list(), df = NULL,
                         df1 = NULL, df2 = NULL, don = NULL, don1 = NULL, don2 = NULL, projImg = NULL) 
  
  # update minimum
  observeEvent(input$minimum,{
    if(input$minimum > input$maximum){
      updateSliderInput(session, "minimum", value = input$maximum)
   }
 }
 )
  
  # update maximum
  observeEvent(input$maximum, {
    if(input$maximum < input$minimum){
      updateSliderInput(session, "maximum", value = input$minimum)
   }
 })
  
  
  # read image files
  newImg <- eventReactive({
    input$imgFile
    input$minimum
    input$maximum
    input$channel_sel},{
    glob$imgPath <- input$imgFile$datapath   # set path
    glob$nFrame <- dim(read_tif(glob$imgPath))[4]  # number of frames
    glob$nChannel <- dim(read_tif(glob$imgPath))[3] # number of channels 
    glob$img <- read_tif(glob$imgPath)      # read tiff image
    
    # 1 channel
    if(glob$nChannel == 1){
      glob$img <- scaling(glob$img, input$minimum, input$maximum)  # scale the image
      glob$projImg <- EBImage::rotate(apply(glob$img, c(2,1), mean), angle = 90)
    return(glob$img)
    }
    
    # multi-channel
    else{
      glob$img <- glob$img[, , as.numeric(input$channel_sel), ,drop = F]
      glob$projImg <- EBImage::rotate(apply(glob$img, c(2,1), mean), angle = 90)
    return(glob$img)
    }
    
    })
  

  observeEvent(input$imgFile,{
      output$actualImage <-renderDisplay({
      ijtiff::display(newImg(), method = "browser")   #display image
      })
    
    
    output$meanStack <- renderPlotly({
      p <- plot_ly(z= glob$projImg, type = "heatmap", source = "A", height = 490) %>%
        config(modeBarButtonsToAdd = c("drawcircle", "drawrect", "eraseshape")) %>%
        event_register("plotly_relayout")
    })
  })
  
  
  # reactive value containing ROI coordinates
  crop1 <- reactiveVal()
  crop2 <- reactiveVal()
  
  # get relayout info from the plot
  observeEvent(event_data("plotly_relayout", source = "A"),{
      d <- event_data("plotly_relayout", source = "A")
      val <- NULL
      if(!is.null(d$"shapes[0].x0")){
        val <- list(
          xmin = d$"shapes[0].x0",
          xmax = d$"shapes[0].x1",
          ymax = d$"shapes[0].y0",
          ymin = d$"shapes[0].y1"
        )
        crop1(val)
      }
      
      
      if(!is.null(d$"shapes[1].x0")){
        val <- list(
          xmin = d$"shapes[1].x0",
          xmax = d$"shapes[1].x1",
          ymax = d$"shapes[1].y0",
          ymin = d$"shapes[1].y1"
        )
        crop2(val)
      }
      
      
  })
  
  
# compute time stamps and mean value of ROI    
  observeEvent({
    input$time
    crop1()},{
      glob$mean1 <- seg(glob$img, crop1()$xmin, crop1()$xmax, crop1()$ymin, crop1()$ymax, glob$nFrame)
      glob$time <- format(seq(as.POSIXct(input$time, tz = 'GMT')
                        , length.out = dim(newImg())[4], by = '1 min'),'%Y-%m-%d %H:%M') # generate time stamps
      glob$time <- as.POSIXct(glob$time)

      glob$df1 <- data.frame(time = glob$time, mean = unlist(glob$mean1))
      glob$don1 <- xts(x = glob$df1$mean, order.by = glob$df1$time)

  })
  
  
  observeEvent(crop2(),{
      glob$mean2 <- seg(glob$img, crop2()$xmin, crop2()$xmax, crop2()$ymin, crop2()$ymax, glob$nFrame)
      glob$df2 <- data.frame(time = glob$time, mean = unlist(glob$mean2))
      glob$don2 <- xts(x = glob$df2$mean, order.by = glob$df2$time)
    })

  
  observeEvent({
    crop1()
    crop2()}, {
      mydata <- cbind(glob$df1, glob$df2)
      df <- mydata[-3]
      colnames(df) <- c('time','ROI1 mean','ROI2 mean')
      output$info <- renderPrint({
        head(df, 25)        # display dataframe
      })
      

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("myData-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(df, file, row.names = FALSE)      # write df to csv file
        })
    }
  )
  

  
  
  # time series plot with ROI
  output$dygraph <- renderDygraph({
    req(glob$don1)
    dygraph(cbind(glob$don1,glob$don2),
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dySeries("glob.don1", label = "ROI1") %>%
      dySeries("glob.don2", label = "ROI2") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
    }
  )
  

  
}

# Run the application
shinyApp(ui = ui, server = server)



  
  
  
  







