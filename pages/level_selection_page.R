render_level_selection_page <- function(output, updated_SDRF) {
  
  source("pages/raw_SDRF_page.R")
  manage_raw_SDRF_page(shinyjs::hide)
  
  output$select_condition_level <- renderUI({
    checkboxGroupInput(inputId = "condition_level", 
                       label = "Select the label(s) for the condition", 
                       choices = levels(updated_SDRF$Condition)
    )
  })
  
  output$select_control_level <- renderUI({
    checkboxGroupInput(inputId = "control_level", 
                       label = "Select the label(s) for the control", 
                       choices = levels(updated_SDRF$Condition)
    )
  })
  
  output$num_upgenes_select <- renderUI({
    radioButtons(inputId = "num_upgenes_select",
                 label = "Select the maximum 
                 number of up-regulated genes you wish to 
                 include in your final result",
                 choices = c("10" = "10",
                             "50" = "50",
                             "100" = "100",
                             "150 (default)" = "150",
                             "custom" = "custom"),
                 selected = "150",
                 inline = TRUE)
  })
  
  output$num_downgenes_select <- renderUI({
    radioButtons(inputId = "num_downgenes_select",
                 label = "Select the maximum 
                 number of down-regulated genes you wish to 
                 include in your final result",
                 choices = c("10" = "10",
                             "50" = "50",
                             "100" = "100",
                             "150 (default)" = "150",
                             "custom" = "custom"),
                 selected = "150",
                 inline = TRUE)
  })
  
  output$add_filter <- renderUI({
    actionButton("add_filter", "Add Filter", icon("plus", lib = "glyphicon"))
  })
  
  output$remove_filter <- renderUI({
    actionButton("remove_filter", "Remove Filter", icon("minus", lib = "glyphicon"))
  })
  
  output$submit_levels <- renderUI({
    actionButton("submit_levels","Submit Levels", icon("ok", lib = "glyphicon"))
  })
  
  output$raw_sdrf_back_button <- renderUI({
    actionButton("raw_sdrf_back_button","Back", icon("arrow-left", lib = "glyphicon"))
  })
  
  manage_level_selection_page(shinyjs::show)
  
  return(output)
}

manage_level_selection_page <- function (func) {
    func(id = "select_control_level")
    func(id = "select_condition_level")
    func(id = "submit_levels")
    func(id = "raw_sdrf_back_button")
    func(id = "num_upgenes_select")
    func(id = "num_upgenes_custom")
    func(id = "num_downgenes_select")
    func(id = "num_downgenes_custom")
    func(id = "add_filter")
    func(id = "remove_filter")
    func(id = "newInputs")
}

