library(shinythemes)
library(shiny)

# load global variables if they don't already exist
if (!exists("desc_content")) {
  source("global.R")
}

headerUI <- function(id) {

  # wrap input and output ids with ns
  # namespace ns
  ns <- NS(id)

  navbarPage(
    id = ns("header_options"),
    #------------------------
    # load project title from global.R
    {
      project_title
    },
    #------------------------
    theme = shinytheme("flatly"),
    # theme = shinytheme("united"),
    tabPanel(
      "Home",
      # homeUI("home-page")
      fluidPage(
        useShinyjs(),
        tagList(
          tags$br(),
          actionButton(ns("run_simulation"), "Run new simulation", class='menuButton'),
          tags$br(),
          tags$br(),
          actionButton(ns("run_previous_config"), "Run from previous configuration", class='menuButton'),
          tags$br(),
          tags$br(),
          actionButton(ns("view_runs"), "View previous runs", class='menuButton'),
          tags$br(),
          tags$br(),
          hidden(actionButton(ns("cancel_home"), "Cancel" , class="menuButton"))
        ))
    ),
    navbarMenu("Options",
               tabPanel("Run Simulation", runSimulationUI("sim1")),
               tabPanel("View Previous Results", viewRunsUI("view_runs1")),
               selected = NULL,
               ),
    
    tabPanel(
      "About",
      aboutUI("about-page")
    ),
    selected = NULL,
    position = "fixed-top",
    fluid = TRUE,
    collapsible = TRUE
  )
}

headerServer <- function(id, store=NULL) {

  aboutServer("about-page")
  
  # Reactive value to trigger a when return to result
  switchToResultEvent <- reactiveVal(FALSE)
  
  ### initialize variables used in simulation steps
  rv <- reactiveValues(page = 1, 
                       show_region = TRUE,
                       input_file = global_config_file,
                       scenarios_sheet = "Scenarios",
                       folder_df = NULL,
                       selected_config_path = NULL,
                      
                        sim_refresh =TRUE
  )
 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # set uid to reactiveValues
    shinyjs::runjs(sprintf("set_shiny_uid('%s')", ns("uid")))
    #shinyjs::hide('cancel_home')
    observeEvent(input$uid, {
      # create a inputfile for the current user based on uid
      rv$input_file <-  reload_config(input$uid) 
      rv$uid <- input$uid
      ### Simplfy to assume only one scenario case
      rv$scenarios_input <- first(read_excel(rv$input_file, sheet = rv$scenarios_sheet))
    })
    
    # Observe the event and switch to result
    observeEvent(switchToResultEvent(), {
      if(switchToResultEvent()) {
        updateNavbarPage(session, inputId = "header_options", selected = "View Previous Results")
        switchToResultEvent(FALSE)  # Reset the event
      }
    })
    
    # navbarPage functions
    observeEvent(input$cancel_home, {
      updateNavbarPage(session, inputId = "header_options", selected = "Run Simulation" )
      print("cancel")
    })
    
    observeEvent(input$run_simulation, {
     
      rv$input_file <-  reload_config(input$uid)
      rv$scenarios_input <- first(read_excel(rv$input_file, sheet = rv$scenarios_sheet))
      rv$show_region <- TRUE
      rv$sim_refresh <- TRUE
      updateNavbarPage(session, inputId = "header_options", selected = "Run Simulation" )
      #rv$page <- "restart"
      # set up user config file 
      shinyjs::show('cancel_home')
    })
    
    
    observeEvent(input$run_name_checked, {
     
      show_no_result <- FALSE
      if (!is.null(input$prev_run_names)){
        # update the folder_df when requested
        rv$folder_df <- get_result_folders_dt(input$prev_run_names)
        # Render the folder table using DT
        
        output$dt_select_prev_config <- renderDT({
          datatable(rv$folder_df, selection = 'single')
        })
        
        if (!is.null(rv$folder_df)){
          # populate previous run config, let user select it as config
          showModal(
            modalDialog(
              title = "Select previous run to reload assumptions from.",
              "When you reload from a previous run, the option to change the default region values is not available. If you would like to run a new region, please cancel and instead choose to run a new simulation.",
              tags$br(),
              tags$br(),
              DTOutput(ns("dt_select_prev_config")),
              footer = tagList(
                actionButton(ns("proceedPrevConfigBtn"), "Proceed"),
                modalButton("Cancel")
              )
            )
          )
        }
        else{
          show_no_result <- TRUE
        }
      }
      else{
        show_no_result <- TRUE
      }
      if(show_no_result){
        showModal(
          modalDialog(
            title = "Unable to use previous config",
            "You did not have any previous results available yet, please start a new simulation.",
          )
        )
      }
     
    })
    
    observeEvent(input$run_previous_config, {
      # check test names from local storage
      shinyjs::runjs(sprintf("get_test_names('%s', '%s', '%s')", ns("prev_run_names"), ns("prev_run_details"), ns("run_name_checked")))
      shinyjs::show('cancel_home')
      })
    
    # Store the selected config file path
    observeEvent(input$dt_select_prev_config_rows_selected, {
      rv$selected_config_path <- renderText({
        req(input$dt_select_prev_config_rows_selected)
        selected_row <- input$dt_select_prev_config_rows_selected
        file.path(result_root, rv$folder_df[selected_row, "run_name"], "config.xlsx")
      })
    })

    # Handle the previously used config scenario 
    observeEvent(input$proceedPrevConfigBtn, {
      removeModal()
      shinyjs::show('cancel_home')
      if (!is.null(rv$selected_config_path())){
        rv$input_file <- reload_config(input$uid, rv$selected_config_path())
        
        rv$scenarios_input <- first(read_excel(rv$input_file, sheet = rv$scenarios_sheet))
        rv$show_region <- FALSE
        updateNavbarPage(session, inputId = "header_options", selected = "Run Simulation" )
        
        rv$task_input <- read_excel(rv$input_file, sheet = rv$task_sheet)
        rv$seasonality_input <- read_excel(rv$input_file, sheet = rv$seasonality_sheet)
        rv$pop_input <- read_excel(rv$input_file, sheet = "TotalPop")
        
        
      }
    })
    
    
    observeEvent(input$view_runs, {
      rv$sim_refresh <- TRUE
      updateNavbarPage(session, inputId = "header_options", selected = "View Previous Results")
    })
  })
  runSimulationServer("sim1", return_event = switchToResultEvent, rv =rv, store = store)
  viewRunsServer("view_runs1", rv = rv, store = store)
}

