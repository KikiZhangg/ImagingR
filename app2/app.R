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
source("fun.R")



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
        box(width = 4, title = h6("1st Channel Image Stack"), status = 'primary',
            displayOutput("actualImage1")
        ),
        
        box(width = 4, title = h6("2nd Channel Image Stack"), status = 'primary',
            displayOutput("actualImage2")
        ),
        
        box(width = 4, title = h6("3rd Channel Image Stack"), status = 'primary',
            displayOutput("actualImage3")
        )
        
        ),
        
      
      fluidRow(
        box(width = 4, title = h6("Select ROI for 1st Channel"), status = 'warning',
            plotlyOutput("meanStack1")
        ),
        
        box(width = 8, 
            dygraphOutput("dygraph1")
        )
        ),
      
      
      fluidRow(
        box(width = 4, title = h6("Select ROI for 2nd Channel"), status = 'warning',
            plotlyOutput("meanStack2")
        ),
        
        box(width = 8, 
            dygraphOutput("dygraph2")
        )
        ),
      
      fluidRow(
        box(width = 4, title = h6("Select ROI for 3rd Channel"), status = 'warning',
            plotlyOutput("meanStack3")
        ),
        
        box(width = 8, 
            dygraphOutput("dygraph3")
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
  glob <- reactiveValues(img = NULL, img1 = NULL, img2 = NULL, img3 = NULL, imgpath = "", nFrame = 1, nChannel = 1,
                         time = NULL, mean1 = list(), mean2 = list(), mean3 = list(),
                         mean4 = list(), mean5 = list(), mean6 = list(),
                         df1 = NULL, df2 = NULL, df3 = NULL, df4 = NULL, df5 = NULL, df6 = NULL,
                         don1 = NULL, don2 = NULL, don3 = NULL, don4 = NULL, don5 = NULL, don6 = NULL,
                         projImg1 = NULL, projImg2 = NULL, projImg3 = NULL) 
  
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
    input$imgFile},{
      glob$imgPath <- input$imgFile$datapath   # set path
      glob$nFrame <- dim(read_tif(glob$imgPath))[4]  # number of frames
      glob$nChannel <- dim(read_tif(glob$imgPath))[3] # number of channels 
      glob$img <- read_tif(glob$imgPath)      # read tiff image
      glob$img1 <- glob$img[, , 1, ,drop = F]
      glob$img2 <- glob$img[, , 2, ,drop = F]
      glob$img3 <- glob$img[, , 3, ,drop = F]
      glob$projImg1 <- EBImage::rotate(apply(glob$img1, c(2,1), mean), angle = 90)
      glob$projImg2 <- EBImage::rotate(apply(glob$img2, c(2,1), mean), angle = 90)
      glob$projImg3 <- EBImage::rotate(apply(glob$img3, c(2,1), mean), angle = 90)
      return(glob$img)
      }
    )
  
  
  observeEvent(input$imgFile,{
    output$actualImage1 <- renderDisplay({
      ijtiff::display(newImg()[, , 1, ,drop = F], method = "browser")   #display image 1st channel
    })
    
    output$actualImage2 <- renderDisplay({
      ijtiff::display(newImg()[, , 2, ,drop = F], method = "browser")   #display image 2nd channel
    })
    
    output$actualImage3 <- renderDisplay({
      ijtiff::display(newImg()[, , 3, ,drop = F], method = "browser")   #display image 3rd channel
    })
    
    
    output$meanStack1 <- renderPlotly({
      p <- plot_ly(z= glob$projImg1, type = "heatmap", source = "A", height = 490) %>%
        config(modeBarButtonsToAdd = c("drawcircle", "drawrect", "eraseshape")) %>%
        event_register("plotly_relayout")
    })
    
    output$meanStack2 <- renderPlotly({
      p <- plot_ly(z= glob$projImg2, type = "heatmap", source = "B", height = 490) %>%
        config(modeBarButtonsToAdd = c("drawcircle", "drawrect", "eraseshape")) %>%
        event_register("plotly_relayout")
    })
    
    
    output$meanStack3 <- renderPlotly({
      p <- plot_ly(z= glob$projImg3, type = "heatmap", source = "C", height = 490) %>%
        config(modeBarButtonsToAdd = c("drawcircle", "drawrect", "eraseshape")) %>%
        event_register("plotly_relayout")
    })
    
  })
  
  
  # reactive value containing ROI coordinates
  crop1 <- reactiveVal()
  crop2 <- reactiveVal()
  crop3 <- reactiveVal()
  crop4 <- reactiveVal()
  crop5 <- reactiveVal()
  crop6 <- reactiveVal()
  
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
  
  
  observeEvent(event_data("plotly_relayout", source = "B"),{
    d <- event_data("plotly_relayout", source = "B")
    val <- NULL
    if(!is.null(d$"shapes[0].x0")){
      val <- list(
        xmin = d$"shapes[0].x0",
        xmax = d$"shapes[0].x1",
        ymax = d$"shapes[0].y0",
        ymin = d$"shapes[0].y1"
      )
      crop3(val)
    }
    
    
    
    if(!is.null(d$"shapes[1].x0")){
      val <- list(
        xmin = d$"shapes[1].x0",
        xmax = d$"shapes[1].x1",
        ymax = d$"shapes[1].y0",
        ymin = d$"shapes[1].y1"
      )
      crop4(val)
    }
    
  })
  
  
  observeEvent(event_data("plotly_relayout", source = "C"),{
    d <- event_data("plotly_relayout", source = "C")
    val <- NULL
    if(!is.null(d$"shapes[0].x0")){
      val <- list(
        xmin = d$"shapes[0].x0",
        xmax = d$"shapes[0].x1",
        ymax = d$"shapes[0].y0",
        ymin = d$"shapes[0].y1"
      )
      crop5(val)
    }
    
    
    if(!is.null(d$"shapes[1].x0")){
      val <- list(
        xmin = d$"shapes[1].x0",
        xmax = d$"shapes[1].x1",
        ymax = d$"shapes[1].y0",
        ymin = d$"shapes[1].y1"
      )
      crop6(val)
    }
    
  })
  
  
  # compute time stamps and mean value of ROI    
  observeEvent({
    input$time
    crop1()},{
      glob$mean1 <- seg(glob$img1, crop1()$xmin, crop1()$xmax, crop1()$ymin, crop1()$ymax, glob$nFrame)
      glob$time <- format(seq(as.POSIXct(input$time, tz = 'GMT')
                              , length.out = dim(newImg())[4], by = '1 min'),'%Y-%m-%d %H:%M') # generate time stamps
      glob$time <- as.POSIXct(glob$time)
      
      glob$df1 <- data.frame(time = glob$time, mean = unlist(glob$mean1))
      glob$don1 <- xts(x = glob$df1$mean, order.by = glob$df1$time)
      
    })
  
  
  observeEvent(crop2(),{
    glob$mean2 <- seg(glob$img1, crop2()$xmin, crop2()$xmax, crop2()$ymin, crop2()$ymax, glob$nFrame)
    glob$df2 <- data.frame(time = glob$time, mean = unlist(glob$mean2))
    glob$don2 <- xts(x = glob$df2$mean, order.by = glob$df2$time)
  })
  

  
  
  # time series plot with ROI
  output$dygraph1 <- renderDygraph({
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
  
 
  observeEvent({
    crop3()},{
      glob$mean3 <- seg(glob$img2, crop3()$xmin, crop3()$xmax, crop3()$ymin, crop3()$ymax, glob$nFrame)
      glob$df3 <- data.frame(time = glob$time, mean = unlist(glob$mean3))
      glob$don3 <- xts(x = glob$df3$mean, order.by = glob$df3$time)
    })
  
  observeEvent({
    crop4()},{
      glob$mean4 <- seg(glob$img2, crop4()$xmin, crop4()$xmax, crop4()$ymin, crop4()$ymax, glob$nFrame)
      glob$df4 <- data.frame(time = glob$time, mean = unlist(glob$mean4))
      glob$don4 <- xts(x = glob$df4$mean, order.by = glob$df4$time)
    })
  
  
  output$dygraph2 <- renderDygraph({
    req(glob$don3)
    dygraph(cbind(glob$don3,glob$don4),
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dySeries("glob.don3", label = "ROI1") %>%
      dySeries("glob.don4", label = "ROI2") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  
  observeEvent({
    crop5()},{
      glob$mean5 <- seg(glob$img3, crop5()$xmin, crop5()$xmax, crop5()$ymin, crop5()$ymax, glob$nFrame)
      glob$df5 <- data.frame(time = glob$time, mean = unlist(glob$mean5))
      glob$don5 <- xts(x = glob$df5$mean, order.by = glob$df5$time)
    })
  
  observeEvent({
    crop6()},{
      glob$mean6 <- seg(glob$img3, crop6()$xmin, crop6()$xmax, crop6()$ymin, crop6()$ymax, glob$nFrame)
      glob$df6 <- data.frame(time = glob$time, mean = unlist(glob$mean6))
      glob$don6 <- xts(x = glob$df6$mean, order.by = glob$df6$time)
    })

  
  output$dygraph3 <- renderDygraph({
    req(glob$don5)
    dygraph(cbind(glob$don5,glob$don6),
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dySeries("glob.don5", label = "ROI1") %>%
      dySeries("glob.don6", label = "ROI2") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
