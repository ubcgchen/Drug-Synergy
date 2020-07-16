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
                actionButton("reset", "Switch Datasets"),
                add_busy_bar(color = "#008CBA"),
                uiOutput("sdrf_header"),
                uiOutput("dropdown_sample"),
                uiOutput("dropdown_condition"),
                uiOutput("text_prefix"),
                uiOutput("text_suffix"),
                uiOutput("update_sdrf_table"),
                DT::dataTableOutput("raw_SDRF"),
                uiOutput("select_condition_level"),
                uiOutput("select_control_level"),
                uiOutput("num_upgenes_select"),
                uiOutput("num_upgenes_custom"),
                uiOutput("num_downgenes_select"),
                uiOutput("num_downgenes_custom"),
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
  
  observeEvent(input$reset, {
    # hide all UI elements
    source("raw_SDRF_page.R")
    manage_raw_SDRF_page(shinyjs::hide)
    
    source("level_selection_page.R")
    manage_level_selection_page(shinyjs::hide)
    
    source("top_table_page.R")
    manage_top_table_page(shinyjs::hide)
    
    source("cmap_results_page.R")
    manage_cmap_results_page(shinyjs::hide)
    
    # re-enable button to submit dataset
    shinyjs::enable("geo")
  })
  
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
  
  # Triggered when user clicks button to add a filter
  observeEvent(input$add_filter, {
    isolate({
      # add a new filter id to filters
      id <- base::paste("filter", 
                        isolate(length(filters$input_filters)) + 1, sep = "")
      filters$input_filters <- c(filters$input_filters, id)
      })
    
    # invoke filters to update it
    filters
    
    # render all filters
    source("filters.R")
    output <- render_filters(input, output, filters)
    
  })
  
  # Triggered when user clicks button to remove a filter
  observeEvent(input$remove_filter, {
    isolate({
      # remove the last filter id from filters
      filters$input_filters <- 
        filters$input_filters[-length(filters$input_filters)]
    })
    
    # invoke filters to update it
    filters
    
    # render all filters
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
    source("level_selection_page.R")
    manage_level_selection_page(shinyjs::hide)
    
    # show raw sdrf page
    source("raw_SDRF_page.R")
    manage_raw_SDRF_page(shinyjs::show)
  })
  
  observeEvent (input$num_downgenes_select, {
    source("gene_number_choices.R")
    output <- render_gene_number_choices(input$num_downgenes_select, 
                                         "num_downgenes_custom",
                                         output)
  })
  
  observeEvent (input$num_upgenes_select, {
    source("gene_number_choices.R")
    output <- render_gene_number_choices(input$num_upgenes_select, 
                                         "num_upgenes_custom", 
                                         output)
  })
  
  # Triggered when the user submits their level selection
  observeEvent(input$submit_levels, {
    # Load variables
    conditions = input$condition_level
    controls = input$control_level
    
    num_downgenes = if (input$num_downgenes_select == "custom") 
      input$num_downgenes_custom else 
        input$num_downgenes_select
    num_upgenes = if (input$num_upgenes_select == "custom") 
      input$num_upgenes_custom else 
        input$num_upgenes_select
      
    tryCatch({
      
      user_filters <- list()
      
      for (filter in filters$input_filters) {
        user_filters[[filter]] <-
          c(input[[base::paste(filter, "column", sep = "")]],
            input[[base::paste(filter, "comparator", sep = "")]],
            input[[base::paste(filter, "value", sep = "")]])
      }
      
      # filter SDRF based on user-specified condition/control levels
      source("SDRF_loader.R")
      filtered_SDRF <<- filter_sdrf(updated_SDRF, conditions, controls)
      
      # generate file paths for all data files
      source("tt_generator.R")
      file_paths <<- get_file_paths(AE_data, filtered_SDRF)
      
      res <- build_expression_matrix(file_paths, filtered_SDRF)
      expression_matrix <- res[[1]]
      grouped_df <- res[[2]]
      
      design_matrix <- build_design_matrix(nrow(grouped_df$control),
                                           nrow(grouped_df$condition))
      
      # Do limma analysis + convert affy to entrez
      top_table <- fit_data(expression_matrix, design_matrix)
      top_table <- affy_to_entrez(top_table)
      
      positive_DEG <- generate_DEG_table(greater_than_zero, top_table, 
                                         user_filters, num_upgenes)
      negative_DEG <- generate_DEG_table(less_than_zero, top_table,
                                         user_filters, num_downgenes)
     
      write_gmt_files(positive_DEG, negative_DEG)
      
      upreg_length <<- nrow(positive_DEG)
      downreg_length <<- nrow(negative_DEG)
      
      source("top_table_page.R")
      render_top_table_page(output, positive_DEG, negative_DEG, 
                            upreg_length, downreg_length)
      },

      error = function(cond) {
        shinyalert("Processing Error", cond$message, type = "error")
      })
  })
  
  # Triggered when user wants to go from the top table page back to the level
  # selection page
  observeEvent(input$level_selection_back_button, {
    # hide top table page
    source("top_table_page.R")
    manage_top_table_page(shinyjs::hide)
    
    # show level selection page
    source("level_selection_page.R")
    manage_level_selection_page(shinyjs::show)
  })
  
  # Triggered when user confirms the top tables and submits the CMap query.
  observeEvent(input$confirm_DEG, {
    
    tryCatch({
      source("cmap_analyzer.R")
      
      # query and retrieve CMap data
      poll <- query_cmap(base::paste(input$accession_code, "_up", upreg_length,
                                     "_dn", downreg_length, sep = ""))
      download_cmap_data(poll)
      
      # load and process CMap results
      ds <- load_cmap_data(poll$job_id)
      top_drugs <<- process_cmap_data(ds)
      
      # render CMap results
      source("cmap_results_page.R")
      output <- render_cmap_results_page(output)
    },
    error = function(cond) {
      shinyalert("Processing Error", cond$message, type = "error")
    })
    
  })
}

shinyApp(ui = ui, server = server)
