library(shiny)
library(shinythemes)
library(DT)
library(shinyalert)
library(shinybusy)

# UI
ui <- fluidPage(theme = shinytheme("paper"),
                includeCSS("styles.css"),
                useShinyalert(),
                shinyjs::useShinyjs(),
                sidebarPanel(
                textInput("accession_code", "GEO Accession Code:", "E-GEOD-28750",
                          placeholder = "E-GEOD-XXXXX"),
                actionButton("geo","Submit GEO Accession Code")
                ),
                add_busy_bar(color = "#48D1CC"),
                uiOutput("sdrf_header"),
                uiOutput("dropdown_sample"),
                uiOutput("dropdown_condition"),
                uiOutput("text_prefix"),
                uiOutput("text_suffix"),
                uiOutput("update_sdrf_table"),
                DT::dataTableOutput("raw_SDRF"),
                uiOutput("select_condition_level"),
                uiOutput("select_control_level"),
                uiOutput("add_filter"),
                uiOutput("remove_filter"),
                uiOutput("newInputs"),
                uiOutput("submit_levels"),
                uiOutput("raw_sdrf_back_button"),
                uiOutput("positive_DEG_Tag"),
                DT::dataTableOutput("positive_DEG"),
                uiOutput("negative_DEG_Tag"),
                DT::dataTableOutput("negative_DEG"),
                uiOutput("confirm_DEG"),
                uiOutput("level_selection_back_button"),
                DT::dataTableOutput("top_drugs")
                )

# Server
server <- function(input, output) {
  
  # Triggered when the GEO Accession Code is submitted
  observeEvent(input$geo, {
    
    tryCatch({
      # download data from ArrayExpress, load the SDRF file, and render it
      source("SDRF_loader.R")
      # download_AE_data(input$accession_code)
      SDRF <<- load_SDRF(AE_data, input$accession_code)
      source("raw_SDRF_page.R")
      output <- render_raw_SDRF_page(input, output)
    },
    
    error=function(cond) {
      shinyalert("Invalid Accession Code", cond$message, type = "error")
      })
    
    })
  
  filters <- reactiveValues(
    input_filters = list()
  )
  
  observeEvent(input$add_filter, {
    isolate({
        id <- base::paste(
          "filter", isolate(length(filters$input_filters)) + 1, sep = "")
        filters$input_filters <- c(
          filters$input_filters, list(c(id, input$test, "")))
      })
  
    filters
    
    source("filters.R")
    output <- render_filters(output, filters)
    
  })
  
  observeEvent(input$remove_filter, {
    isolate({
      filters$input_filters <- 
        filters$input_filters[-length(filters$input_filters)]
    })
    
    filters
    
    source("filters.R")
    output <- render_filters(output, filters)
  })
  
  # Triggered when the user submits manually inputted SDRF details
  observeEvent(input$sdrf, {
    
    # load input variables
    sample_col_name <- input$select_sample
    condition_col_name <- input$select_condition
    prefix <- input$text_prefix
    suffix <- input$text_suffix
    
    tryCatch({
      # Update SDRF based on user-provided SDRF details and render the next page
      source("SDRF_loader.R")
      updated_SDRF <<- update_sdrf(SDRF, c(sample_col_name, condition_col_name),
                                   prefix, suffix)

      source("level_selection_page.R")
      output <- render_level_selection_page(output)
    },
    
    error=function(cond) {
      shinyalert("Selection Error", cond$message, type = "error")
    })
    
  })
  
  # Triggered when user wants to go from the level selection page back to the 
  # raw SDRF page
  observeEvent(input$raw_sdrf_back_button, {
    # hide level selection page
    shinyjs::hide(id = "select_control_level")
    shinyjs::hide(id = "select_condition_level")
    shinyjs::hide(id = "submit_levels")
    shinyjs::hide(id = "raw_sdrf_back_button")
    
    # show raw sdrf page
    shinyjs::show(id = "dropdown_sample")
    shinyjs::show(id = "dropdown_condition")
    shinyjs::show(id = "sdrf")
    shinyjs::show(id = "raw_SDRF")
    shinyjs::show(id = "text_prefix")
    shinyjs::show(id = "text_suffix")
  })
  
  # Triggered when the user submits their level selection
  observeEvent(input$submit_levels, {
    # Load variables
    conditions = input$condition_level
    controls = input$control_level
      
    tryCatch({
      
      source("SDRF_loader.R")
      filtered_SDRF <<- filter_sdrf(updated_SDRF, conditions, controls)
      
      source("tt_generator.R")
      file_paths <<- get_file_paths(AE_data, filtered_SDRF)
      
      res <- build_expression_matrix(file_paths, filtered_SDRF)
      expression_matrix <- res[[1]]
      grouped_df <- res[[2]]
      
      design_matrix <- build_design_matrix(nrow(grouped_df$control),
                                           nrow(grouped_df$condition))
      
      top_table <- fit_data(expression_matrix, design_matrix)
      top_table <- affy_to_entrez(top_table)
      
      positive_DEG <- generate_DEG_table(greater_than_zero, top_table)
      negative_DEG <- generate_DEG_table(less_than_zero, top_table)
      
      write_gmt_files(positive_DEG, negative_DEG)
      
      source("top_table_page.R")
      render_top_table_page(output, positive_DEG, negative_DEG)
      },
      
      error = function(cond) {
        test <<- cond
        shinyalert("Processing Error", cond$message, type = "error")
      })
  })
  
  # Triggered when user wants to go from the top table page back to the level
  # selection page
  observeEvent(input$level_selection_back_button, {
    # hide top table page
    shinyjs::hide(id = "positive_DEG_Tag")
    shinyjs::hide(id = "positive_DEG")
    shinyjs::hide(id = "negative_DEG_Tag")
    shinyjs::hide(id = "negative_DEG")
    shinyjs::hide(id = "confirm_DEG")
    shinyjs::hide(id = "level_selection_back_button")
    
    # show level selection page
    shinyjs::show(id = "select_control_level")
    shinyjs::show(id = "select_condition_level")
    shinyjs::show(id = "submit_levels")
    shinyjs::show(id = "raw_sdrf_back_button")
  })
  
  # Triggered when user confirms the top tables and submits the CMap query.
  observeEvent(input$confirm_DEG, {
    
    source("cmap_analyzer.R")
    
    # query and retrieve CMap data
    poll <- query_cmap(base::paste(input$accession_code, "_up150_dn150", sep = ""))
    download_cmap_data(poll)
    
    # load and process CMap results
    ds <- load_cmap_data(poll$job_id)
    top_drugs <<- process_cmap_data(ds)
    
    source("cmap_results_page.R")
    output <- render_cmap_results_page(output)
    
  })
}

shinyApp(ui = ui, server = server)
