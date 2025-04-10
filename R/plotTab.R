library(shiny)

  

plotTabUI <- function(id=NULL, title=NULL) {
  #namespace ns
  #wrap input and output ids with ns
  ns <- NS(id)
  
  tabPanel(title,
           useShinyjs(),
           fluidRow(
             div(id = ns("wait_msg"), "Generating plot, please wait...", div(class = "spinner"), style = "display: none;"),  # Initially hidden
           ),
           fluidRow(
             style = "margin: 0 5px; margin-bottom:60px",
             plotlyOutput(ns("plot"),),
             
             # div(class="visual-placeholder-xl")
           ), 
          )
}


plotTabServer <- function(id, plotting_function, rv, show_wait=TRUE, plotly=TRUE) {
    moduleServer(id, function(input, output, session) {
    ns <- session$ns
    plotLoading <- reactiveVal(show_wait)
    observe({
      if (plotLoading()){
        shinyjs::show(id = ns("wait_msg"), asis = TRUE)
      }
      else{
        shinyjs::hide(id = ns("wait_msg"), asis = TRUE)
      }
    })
    
    if(plotly){
      output$plot <- renderPlotly({
        if (!is.null(rv$results) ) {
          plot <- do.call(plotting_function, list(rv))
          plotLoading(FALSE)
          plot %>% ggplotly(height = 500)
        }else{
          print("no results")
          NULL
        }
      })
    } else{
      output$plot <- renderPlot({
        if (!is.null(rv$results) ) {
          plot <- do.call(plotting_function, c(list(rv), FALSE))
          plotLoading(FALSE)
          plot
        }else{
          print("no results")
          NULL
        }
      })
    }
    
  })
}


# Define the simple plot Module without tabpanel
simpleplotUI <- function(id,title) {
  ns <- NS(id)
  div(class="plots",  plotOutput(ns("plot")))
}

simpleplotServer <- function(id, plotFunction, rv) {
  moduleServer(id, function(input, output, session) {
    print("In module:")
    print(head(rv$pop_input))
    output$plot <- renderPlot({
      do.call(plotFunction, list(rv))
      # plotFunction(rv)  # Call the function to get the ggplot object
    })
  })
}
