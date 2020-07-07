library(shiny)
library(shinythemes)
library(DT)
library(shinycssloaders)

# UI
ui <- fluidPage(theme = shinytheme("paper"),
                sidebarPanel(
                textInput("accession_code", "GEO Accession Code:", ""), # default value
                actionButton("geo","Submit GEO Accession Code")
                ),
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
                uiOutput("positive_DEG_Tag"),
                DT::dataTableOutput("positive_DEG"),
                uiOutput("negative_DEG_Tag"),
                DT::dataTableOutput("negative_DEG"),
                uiOutput("confirm_DEG"),
                DT::dataTableOutput("top_drugs"),
                )

# Server
server <- function(input, output) {
  observeEvent(input$geo, {
    
    if (input$accession_code != ""){
      source("SDRF_loader.R")
      # AE_data <- download_AE_data(input$accession_code)
      SDRF <<- load_SDRF(AE_data, input$accession_code)

      output$dropdown_sample <- renderUI({
        selectInput(inputId = "select_sample", 
                    label = "Select the column containing sample names", 
                    choices = names(SDRF),
                    multiple = F)
      })
      
      output$dropdown_condition <- renderUI({
        selectInput(inputId = "select_condition", 
                    label = "Select the column containing the condition", 
                    choices = names(SDRF),
                    multiple = F)
      })
      
      output$text_prefix <- renderUI({
        textInput(inputId = "text_prefix",
                  label = "The SDRF file names may include unnecessary prefixes or suffixes. Below, enter the prefixes and suffixes you would like ignored during analysis",
                  placeholder = "prefix")
      })
      
      output$text_suffix <- renderUI({
        textInput(inputId = "text_suffix",
                  label = "",
                  placeholder = "suffix")
      })
      
      output$update_sdrf_table <- renderUI({
        actionButton("sdrf","Update SDRF", icon("refresh"))
      })
      
      output$raw_SDRF <- DT::renderDataTable({
        SDRF
      })
    }
  })
  
  observeEvent(input$sdrf, {
    source("SDRF_loader.R")
    SDRF <<- update_sdrf(SDRF, c(input$select_sample, input$select_condition),
                         input$text_prefix, input$text_suffix)
    
    removeUI("#dropdown_sample")
    removeUI("#dropdown_condition")
    removeUI("#sdrf")
    removeUI("#raw_SDRF")
    removeUI("#text_prefix")
    removeUI("#text_suffix")
    
    output$select_condition_level <- renderUI({
      selectInput(inputId = "condition_level", 
                         label = "Select the label for the condition", 
                         choices = levels(SDRF$Condition)
                         )
    })
    
    output$select_control_level <- renderUI({
      selectInput(inputId = "control_level", 
                         label = "Select the label for the control", 
                         choices = levels(SDRF$Condition)
      )
    })
    
    output$submit_levels <- renderUI({
      actionButton("submit_levels","Submit Levels")
    })
  })
  
  observeEvent(input$submit_levels, {
    source("SDRF_loader.R")
    SDRF <<- filter_sdrf(SDRF, input$condition_level, input$control_level)
    removeUI("#select_control_level")
    removeUI("#select_condition_level")
    removeUI("#submit_levels")
    
    source("tt_generator.R")
    
    file_paths <<- get_file_paths(AE_data)
    
    res <- build_expression_matrix(file_paths, SDRF, input$control_level)
    expression_matrix <- res[[1]]
    grouped_df <- res[[2]]
    design_matrix <- build_design_matrix(nrow(grouped_df[[input$control_level]]),
                                         nrow(grouped_df[[input$condition_level]]))

    top_table <- fit_data(expression_matrix, design_matrix)
    top_table <- affy_to_entrez(top_table)

    positive_DEG <- generate_DEG_table(greater_than_zero, top_table)
    negative_DEG <- generate_DEG_table(less_than_zero, top_table)

    write_gmt_files(positive_DEG, negative_DEG)
    
    output$positive_DEG_Tag <- renderUI({
      HTML(base::paste('<br/>', '<br/>', '<br/>','<br/>', '<br/>', '<br/>', '<br/>','<br/>',
                       h4("Top 150 up-regulated genes")))
    })
    
    output$positive_DEG <- DT::renderDataTable({
      positive_DEG
    })
    
    output$negative_DEG_Tag <- renderUI({
      HTML(base::paste('<br/>', '<br/>',
                       h4("Top 150 down-regulated genes")))
    })
    
    output$negative_DEG <- DT::renderDataTable({
      negative_DEG
    })
    
    output$confirm_DEG <- renderUI({
      actionButton("confirm_DEG","Submit Query to CMap")
    })
  })
  
  observeEvent(input$confirm_DEG, {
    removeUI("#positive_DEG_Tag")
    removeUI("#negative_DEG_Tag")
    removeUI("#positive_DEG")
    removeUI("#negative_DEG")
    removeUI("#confirm_DEG")
    
    source("cmap_analyzer.R")
    
    poll <- query_cmap(base::paste(input$accession_code, "_up150_dn150", sep = ""))
    download_cmap_data(poll)
    ds <- load_cmap_data(poll$job_id)
    top_drugs <<- process_cmap_data(ds)
    
    output$top_drugs <- DT::renderDataTable({
      top_drugs
    })
  })
}

shinyApp(ui = ui, server = server)