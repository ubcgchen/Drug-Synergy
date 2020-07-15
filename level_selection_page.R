render_level_selection_page <- function(output) {
  
  shinyjs::hide(id = "dropdown_sample")
  shinyjs::hide(id = "dropdown_condition")
  shinyjs::hide(id = "sdrf")
  shinyjs::hide(id = "raw_SDRF")
  shinyjs::hide(id = "text_prefix")
  shinyjs::hide(id = "text_suffix")
  
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
  
  shinyjs::show(id = "select_control_level")
  shinyjs::show(id = "select_condition_level")
  shinyjs::show(id = "submit_levels")
  shinyjs::show(id = "raw_sdrf_back_button")
  
  return(output)
}
