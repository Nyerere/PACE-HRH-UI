library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(DT)

#-------------------Global Variables-------------------#
sc_colours <- c(
  "MNH" = "#1f77b4", "FPAYH" = "#ff7f0e", "Child health" = "#2ca02c",
  "EPI" = "#9467bd", "Nutrition" = "#8c564b", "TB & leprosy" = "#e377c2",
  "Malaria" = "#7f7f7f", "First aid" = "#bcbd22", "NCD" = "#17becf",
  "NCDs" = "#17becf", "Mental health" = "#aec7e8", "NTD" = "#ffbb78",
  "HIV/AIDS & STI" = "#98df8a", "-" = "#ff9896", "Other outpatient services" = "#9edae5",
  "Healthy housing" = "#a64d79", "Personal hygiene" = "#ffd966", "Waste management" = "#85200c",
  "Institutional hygiene" = "#45818e", "Health Education" = "#ff9896", "HPC" = "#c6b0d6"
)

sc_fillScale <- scale_fill_manual(name = "ServiceCat", values = sc_colours)
sc_colorScale <- scale_color_manual(name = "ServiceCat", values = sc_colours)

#-------------------Module UI-------------------#
newModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
    
    uiOutput(ns("year_selector")),
    uiOutput(ns("ref_year_selector")),
    uiOutput(ns("service_selector")),
    
    checkboxInput(ns("facet_view"), "View Facets", value = FALSE),
    checkboxInput(ns("show_diff"), "Show Differences", value = FALSE),
    
    downloadButton(ns("download_weekly_diff"), "Download Weekly Differences"),
    
    tabsetPanel(
      tabPanel("Visualization", 
               plotOutput(ns("plot"), height = "600px"),
               plotOutput(ns("yearly_diff_plot"), height = "600px")
      ),
      tabPanel("Weekly Differences Table", 
               DTOutput(ns("weekly_diff_table")),
               plotOutput(ns("weekly_diff_plot"), height = "600px")
      )
    )
  )
}

#-------------------Module Server-------------------#
newModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespacing for UI components
    
    dataset <- reactive({
      req(input$file)
      df <- fread(input$file$datapath, stringsAsFactors = FALSE) %>%
        mutate(Year = as.numeric(Year), ServiceCat = as.factor(ServiceCat)) %>%
        distinct()
      return(df)
    })
    
    # Dynamic UI elements
    output$year_selector <- renderUI({
      req(dataset())
      years_available <- sort(unique(dataset()$Year))
      checkboxGroupInput(ns("years"), "Select Years:", choices = years_available, selected = years_available)
    })
    
    output$ref_year_selector <- renderUI({
      req(dataset())
      years_available <- sort(unique(dataset()$Year))
      radioButtons(ns("ref_year"), "Select Reference Year:", choices = years_available, selected = min(years_available))
    })
    
    output$service_selector <- renderUI({
      req(dataset())
      service_categories <- unique(dataset()$ServiceCat)
      checkboxGroupInput(ns("service_cats"), "Select Service Categories:", choices = service_categories, selected = service_categories)
    })
    
    # Compute Mean Hours
    mean_hours <- reactive({
      req(input$years, input$service_cats)
      dataset() %>%
        filter(Year %in% input$years, ServiceCat %in% input$service_cats) %>%
        group_by(Year, ServiceCat) %>%
        summarise(MeanHrs = round(mean(MeanHrs, na.rm = TRUE), 1), .groups = 'drop') %>%
        arrange(desc(MeanHrs))
    })
    
    # Compute Yearly Differences
    yearly_diff <- reactive({
      req(input$show_diff, input$ref_year, input$years, input$service_cats)
      df <- mean_hours()
      
      ref_year <- as.numeric(input$ref_year)
      df_base <- df %>% filter(Year == ref_year) %>% select(ServiceCat, MeanHrs)
      
      df %>%
        pivot_wider(names_from = Year, values_from = MeanHrs, names_prefix = "MeanHrs_") %>%
        left_join(df_base, by = "ServiceCat", suffix = c("", "_base")) %>%
        mutate(across(starts_with("MeanHrs_"), ~ round(. - get(paste0("MeanHrs_", ref_year)), 1), .names = "YearlyDiff_{.col}")) %>%
        pivot_longer(cols = starts_with("YearlyDiff_"), names_to = "Year", values_to = "YearlyDiff") %>%
        mutate(Year = gsub("YearlyDiff_MeanHrs_", "", Year)) %>%
        arrange(desc(YearlyDiff))
    })
    
    # Compute Weekly Differences
    weekly_diff <- reactive({
      req(input$show_diff, input$ref_year, input$years, input$service_cats)
      df_diff <- yearly_diff()
      
      dataset() %>%
        filter(Year %in% input$years, ServiceCat %in% input$service_cats) %>%
        select(Year, ServiceCat, WeeksPerYr) %>%
        mutate(Year = as.character(Year)) %>%
        left_join(df_diff, by = c("Year", "ServiceCat")) %>%
        mutate(WeeklyDiff = round(YearlyDiff / WeeksPerYr, 1)) %>%
        arrange(desc(WeeklyDiff))
    })
    
    # Render Weekly Differences Table
    output$weekly_diff_table <- renderDT({
      req(input$show_diff)
      datatable(weekly_diff(), options = list(pageLength = 10), rownames = FALSE)
    })
    
    # Render Plots
    output$plot <- renderPlot({
      df <- mean_hours()
      ggplot(df, aes(x = reorder(ServiceCat, -MeanHrs), y = MeanHrs, fill = ServiceCat)) +
        geom_bar(stat = "identity", position = "dodge") +
        sc_fillScale +
        theme_minimal() +
        labs(title = "Mean Hours by Service Category", x = "Service Category", y = "Mean Hours", fill = "Service Category")
    })
    
    output$yearly_diff_plot <- renderPlot({
      req(input$show_diff)
      df_diff <- yearly_diff()
      ggplot(df_diff, aes(x = reorder(ServiceCat, -YearlyDiff), y = YearlyDiff, fill = ServiceCat)) +
        geom_bar(stat = "identity", position = "dodge") +
        sc_fillScale +
        theme_minimal() +
        labs(title = "Yearly Differences in Mean Hours by Service Category", subtitle = paste("Reference Year:", input$ref_year))
    })
    
    output$weekly_diff_plot <- renderPlot({
      req(input$show_diff)
      df_weekly_diff <- weekly_diff()
      ggplot(df_weekly_diff, aes(x = reorder(ServiceCat, -WeeklyDiff), y = WeeklyDiff, fill = ServiceCat)) +
        geom_bar(stat = "identity", position = "dodge") +
        sc_fillScale +
        theme_minimal() +
        labs(title = "Weekly Differences in Mean Hours by Service Category", subtitle = paste("Reference Year:", input$ref_year))
    })
    
    # Download handler for weekly differences
    output$download_weekly_diff <- downloadHandler(
      filename = function() { paste("weekly_differences-", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write.csv(weekly_diff(), file, row.names = FALSE) }
    )
  })
}
