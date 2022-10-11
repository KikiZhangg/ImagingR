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
    theme = bs_theme(bootswatch = "lux"),
    # title
    dashboardSidebar(),
    
    # Sidebar with a slider input for number of bins]
    
    dashboardBody(
      fluidRow(
        column(width = 4,
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
                                  timepickerOpts = timepickerOptions(timeFormat = 'hh:mm')
                                  ),
        ),
               
        #        
        #        sliderInput("minimum",
        #                    label = "Choose Minimum",
        #                    min = 500,
        #                    max = 10000,
        #                    value = 1000,
        #                    step = 50
        #        ),
        #        
        #        sliderInput("maximum",
        #                    label = "Choose Maximum",
        #                    min = 500,
        #                    max = 20000,
        #                    value = 2000,
        #                    step = 50
        #        )
        # ),
        
        column(width = 4,
               selectizeInput(inputId = 'select',
                              label = 'Select Channel to Compare',
                              choices = list('Neuronal'= 'Neuronal', 'CBF'= 'CBF', 'CBV' = 'CBV'),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(maxItems = 2))

               )
        ),
               
      
      
      # Show a plot/image
      fluidRow(
        box(width = 4, title = h6("1st Channel Image Stack (Neuronal)"), status = 'primary',
            displayOutput("actualImage1")
        ),
        
        box(width = 4, title = h6("2nd Channel Image Stack (CBV)"), status = 'primary',
            displayOutput("actualImage2")
        ),
        
        box(width = 4, title = h6("3rd Channel Image Stack (CBF)"), status = 'primary',
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
        ),
    
      fluidRow(
        box(width = 6, title = h6("Cross Correlation Between Selected Series "), status = 'warning',
            plotlyOutput("ccf")
 
      ),
      
        box(width = 6,
            dygraphOutput("dygraph4")
            )
      
    ),
    
    fluidRow(
      box(width=6,
          plotlyOutput("power")
          )
    )
)
)
)








# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10000*1024^2)
  # reactive values
  glob <- reactiveValues(img = NULL, img1 = NULL, img2 = NULL, img3 = NULL, imgpath = "", nFrame = 1, nChannel = 1,
                         time = NULL, mean1 = list(), mean2 = list(), mean3 = list(),
                         df1 = NULL, df2 = NULL, df3 = NULL,
                         don1 = NULL, don2 = NULL, don3 = NULL,
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
      glob$img <- read_tif(glob$imgPath)      # read tiff image
      glob$nFrame <- dim(glob$img)[4]  # number of frames
      glob$nChannel <- dim(glob$img)[3] # number of channels 
      glob$img1 <- glob$img[, , 1, ,drop = F]
      glob$img2 <- glob$img[, , 2, ,drop = F]
      glob$img3 <- glob$img[, , 3, ,drop = F]
      glob$projImg1 <- EBImage::rotate(apply(glob$img1, c(2,1), mean), angle = 90) %>%
        EBImage::normalize(separate=TRUE, ft=c(0,1))
      glob$projImg2 <- EBImage::rotate(apply(glob$img2, c(2,1), mean), angle = 90) %>%
        EBImage::normalize(separate=TRUE, ft=c(0,1))
      glob$projImg3 <- EBImage::rotate(apply(glob$img3, c(2,1), mean), angle = 90) %>%
        EBImage::normalize(separate=TRUE, ft=c(0,1))
      return(glob$img)
      }
    )
  

  
  observeEvent(input$imgFile,{
    
    output$actualImage1 <- renderDisplay({
      ijtiff::display(newImg()[, , 1, ,drop = F], method = "browser", normalize = TRUE)   #display image 1st channel
    })
    
    output$actualImage2 <- renderDisplay({
      ijtiff::display(newImg()[, , 2, ,drop = F], method = "browser", normalize = TRUE)   #display image 2nd channel
    })
    
    output$actualImage3 <- renderDisplay({
      ijtiff::display(newImg()[, , 3, ,drop = F], method = "browser", normalize = TRUE)   #display image 3rd channel
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
  
  # mimic brush
  observeEvent(crop1(),{
    brush_rect <- list(
      type = "rect",
      x0 = crop1()$xmin,
      x1 = crop1()$xmax,
      y0 = crop1()$ymin,
      y1 = crop1()$ymax,
      fillcolor = "LightSalmon",
      opacity=0.5,
      line = list(
        color = "RoyalBlue",
        width = 2
      )
    )
    
    # update second and third graph
    plotlyProxy("meanStack2", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list(brush_rect)))
    
    
    plotlyProxy("meanStack3", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list(brush_rect)))
    })
  
  
  observeEvent(crop2(),{
    brush_rect_2 <- list(
      type = "rect",
      x0 = crop2()$xmin,
      x1 = crop2()$xmax,
      y0 = crop2()$ymin,
      y1 = crop2()$ymax,
      fillcolor = "darkSalmon",
      opacity=0.5,
      line = list(
        color = "beige",
        width = 2
      )
    )
    
    
    plotlyProxy("meanStack2", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list(brush_rect_2)))
    
    
    plotlyProxy("meanStack3", session) %>%
      plotlyProxyInvoke("relayout", list(shapes = list(brush_rect_2)))
  })
    
  

    

  
  
  # compute time stamps and mean value of ROI    
  observeEvent({
    input$time
    crop1()},{
      glob$mean1 <- seg(EBImage::normalize(glob$img1), crop1()$xmin, crop1()$xmax, crop1()$ymin, crop1()$ymax, glob$nFrame)
      glob$time <- format(seq(as.POSIXct(input$time, tz = 'GMT')
                              , length.out = dim(newImg())[4], by = '1 min'),'%Y-%m-%d %H:%M') # generate time stamps
      glob$time <- as.POSIXct(glob$time)
      
      glob$df1 <- data.frame(time = glob$time, mean = unlist(glob$mean1))
      glob$don1 <- xts(x = glob$df1$mean, order.by = glob$df1$time)
      
      
      glob$mean2 <- seg(EBImage::normalize(glob$img2), crop1()$xmin, crop1()$xmax, crop1()$ymin, crop1()$ymax, glob$nFrame)
      glob$df2 <- data.frame(time = glob$time, mean = unlist(glob$mean2))
      glob$don2 <- xts(x = glob$df2$mean, order.by = glob$df2$time)
      
      
      glob$mean3 <- seg(EBImage::normalize(glob$img3), crop1()$xmin, crop1()$xmax, crop1()$ymin, crop1()$ymax, glob$nFrame)
      glob$df3 <- data.frame(time = glob$time, mean = unlist(glob$mean3))
      glob$don3 <- xts(x = glob$df3$mean, order.by = glob$df3$time)
      
      
    })

  

  
  
  # time series plot with ROI
  output$dygraph1 <- renderDygraph({
    req(glob$don1)
    dygraph(glob$don1,
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "PiYG")) %>%
      dyOptions(strokeWidth = 3) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  

  
  
  output$dygraph2 <- renderDygraph({
    req(glob$don2)
    dygraph(glob$don2,
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = 'skyblue') %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  
  output$dygraph3 <- renderDygraph({
    req(glob$don3)
    dygraph(glob$don3,
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set3")) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  
  output$dygraph4 <- renderDygraph({
    req(glob$don1)
    req(glob$don2)
    req(glob$don3)
    threeChannel <- cbind(glob$don1, glob$don2, glob$don3)
    
    dygraph(threeChannel,
            main = "Mean Intensity of ROI Over Time",
            ylab = "Mean",
            xlab = "Time") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyRangeSelector() %>%
      dyOptions(digitsAfterDecimal = 7)
  }
  )
  
  
  # ccf plot based on selection of series
  observeEvent(input$select,{
    req(glob$img)
    req(input$select[2])
    if(input$select[1] == 'Neuronal' && input$select[2] == 'CBV'){
      output$ccf <- renderPlotly({
        ccf_ly(series1 = unlist(glob$mean1), series2 = unlist(glob$mean2))
      })
    }
    
    if(input$select[1] == 'Neuronal' && input$select[2] == 'CBF'){
      output$ccf <- renderPlotly({
        ccf_ly(series1 = unlist(glob$mean1), series2 = unlist(glob$mean3))
      })
    }
    
    if(input$select[1] == 'CBV' && input$select[2] == 'CBF'){
      output$ccf <- renderPlotly({
        ccf_ly(series1 = unlist(glob$mean2), series2 = unlist(glob$mean3))
      })
    }
    
  })
  
  
  
  
  output$power <- renderPlotly({
    req(c(glob$don1, glob$don2, glob$don3))
    power_spectrum(series1 = unlist(glob$mean1), series2 = unlist(glob$mean2), series3 = unlist(glob$mean3))
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
