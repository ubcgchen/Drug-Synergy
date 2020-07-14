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
                uiOutput("submit_levels"),
                uiOutput("raw_sdrf_back_button"),
                uiOutput("positive_DEG_Tag"),
                DT::dataTableOutput("positive_DEG"),
                uiOutput("negative_DEG_Tag"),
                DT::dataTableOutput("negative_DEG"),
                uiOutput("confirm_DEG"),
                DT::dataTableOutput("top_drugs")
                )

# Server
server <- function(input, output) {
  observeEvent(input$geo, {
    
    if(input$accession_code == "") {
      shinyalert("Error", "You must provide an Accession Code!", type = "error")
    } else{
      source("SDRF_loader.R")
      
      # AE_data <<- tryCatch(
      #   {download_AE_data(input$accession_code)},
      #   warning=function(cond) {
      #     shinyalert("Invalid Accession Code",
      #                "Check your spelling and follow the suggested format",
      #                type = "error")
      #     return(NULL)
      #   }
      # )
      
      if (!is.null(AE_data)) {
        SDRF <<- load_SDRF(AE_data, input$accession_code)
        source("raw_SDRF_page.R")
        output <- render_raw_SDRF_page(input, output)
      }
    }
  })
  
  observeEvent(input$raw_sdrf_back_button, {
    shinyjs::hide(id = "select_control_level")
    shinyjs::hide(id = "select_condition_level")
    shinyjs::hide(id = "submit_levels")
    shinyjs::hide(id = "raw_sdrf_back_button")
    
    shinyjs::show(id = "dropdown_sample")
    shinyjs::show(id = "dropdown_condition")
    shinyjs::show(id = "sdrf")
    shinyjs::show(id = "raw_SDRF")
    shinyjs::show(id = "text_prefix")
    shinyjs::show(id = "text_suffix")
  })
  
  observeEvent(input$sdrf, {
    source("SDRF_loader.R")
    sample_col_name <- input$select_sample
    condition_col_name <- input$select_condition
    
    prefix <- input$text_prefix
    suffix <- input$text_suffix
    
    if (sample_col_name == condition_col_name) {
      shinyalert("Selection Error", 
                 "The sample column must be different from the condition column",
                 type = "error")
    } else {
      updated_SDRF <<- tryCatch(
          {update_sdrf(SDRF, c(sample_col_name, condition_col_name),
                       prefix, suffix)},
          error=function(cond) {
            shinyalert("Invalid Prefix or Suffix",
                       "Check to make sure your prefix or suffix are correctly spelled",
                       type = "error")
            return(NULL)
          }
        )
      if (!is.null(updated_SDRF)) {
        source("level_selection_page.R")
        output <- render_level_selection_page(output)
      }
    }
  })
  
  observeEvent(input$submit_levels, {
    conditions = input$condition_level
    controls = input$control_level
    
    if (length(intersect(conditions, controls)) != 0) {
      shinyalert("Invalid Selection",
                 "Condition and control levels cannot intersect",
                 type = "error")
    } else {
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
    }
  })
  
  observeEvent(input$confirm_DEG, {
    removeUI("#positive_DEG_Tag")
    removeUI("#negative_DEG_Tag")
    removeUI("#positive_DEG")
    removeUI("#negative_DEG")
    removeUI("#confirm_DEG")
    
    source("cmap_analyzer.R")
    
    # poll <- query_cmap(base::paste(input$accession_code, "_up150_dn150", sep = ""))
    # download_cmap_data(poll)
    # ds <- load_cmap_data(poll$job_id)
    # top_drugs <<- process_cmap_data(ds)
    
    output$top_drugs <- DT::renderDataTable({
      top_drugs
    })
  })
}

shinyApp(ui = ui, server = server)
