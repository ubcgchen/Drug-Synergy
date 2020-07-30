library(shiny)
library(shinythemes)
library(DT)
library(shinyalert)
library(shinybusy)
library(shinyBS)

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
                DT::dataTableOutput("top_drugs"),
                uiOutput("synergy_number"),
                uiOutput("reference_drug"),
                uiOutput("find_synergy"),
                uiOutput("top_table_back_button"),
                uiOutput("synergized_drug_tag"),
                uiOutput("synergized_drug_panel"),
                uiOutput("cmap_results_back_button"),
                )

# Server
server <- function(input, output) {
  
  # source("synergized_drugs_page.R")
  # output <- render_cmap_results_page(output)
  
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
    
    source("synergized_drugs_page.R")
    manage_synthesized_drugs_page(shinyjs::hide)
    
    # re-enable button to submit dataset
    shinyjs::enable("geo")
  })
  
  # Triggered when the GEO Accession Code is submitted
  observeEvent(input$geo, {
    
    tryCatch({
      # download data from ArrayExpress, load the SDRF file, and render it
      source("AE_downloader.R")
      # AE_data <<- download_AE_data(input$accession_code)
      
      source("SDRF_loader.R")
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
      filtered_SDRF <- filter_sdrf(updated_SDRF, conditions, controls)
      
      # generate file paths for all data files
      source("file_path_loader.R")
      file_paths <- get_file_paths(AE_data, filtered_SDRF)
      
      # generate matrices
      source("matrices_generator.R")
      res <- build_matrices(file_paths)
      expression_matrix <- res$expression_matrix
      design_matrix <- res$design_matrix
      
      # Do analysis
      source("tt_generator.R")
      res <- do_analysis(expression_matrix, design_matrix, user_filters,
                         num_upgenes, num_downgenes)
      
      upreg_length <<- res$positive_DEG_len
      downreg_length <<- res$negative_DEG_len
      positive_DEG <<- res$positive_DEG
      negative_DEG <<- res$negative_DEG
      
      source("top_table_page.R")
      output <- render_top_table_page(output, res$positive_DEG, res$negative_DEG, 
                            res$positive_DEG_len, res$negative_DEG_len)
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
      source("cmap_querier.R")
      source("cmap_loader.R")
      source("cmap_processor.R")
      source("cmap_results_page.R")
      
      # poll <- query_cmap(base::paste(input$accession_code, "_up", upreg_length,
      #                               "_dn", downreg_length, sep = ""))
      # ds <- load_cmap_data(poll)
      # top_drugs <<- process_cmap_data(ds)
      output <- render_cmap_results_page(output)
    },
    error = function(cond) {
      shinyalert("Processing Error", cond$message, type = "error")
    })

  })
  
  observeEvent(input$top_table_back_button, {
    # hide cmap results page
    source("cmap_results_page.R")
    manage_cmap_results_page(shinyjs::hide)
    
    # show top table page
    source("top_table_page.R")
    manage_top_table_page(shinyjs::show)
  })
  
  observeEvent(input$find_synergy, {
    source("drug_synergizer.R")
    drugs <- synergize_drugs(top_drugs, input$reference_drug, positive_DEG,
                             negative_DEG, input$synergy_number)
    
    source("synergized_drugs_page.R")
    output <- render_synergized_drugs_page(output, drugs)
  })
  
  observeEvent(input$cmap_results_back_button, {
    # hide drug synergy page
    source("synergized_drugs_page.R")
    manage_synthesized_drugs_page(shinyjs::hide)
    
    # show top table page
    source("cmap_results_page.R")
    manage_cmap_results_page(shinyjs::show)
  })
}

shinyApp(ui = ui, server = server)
