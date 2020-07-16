render_gene_number_choices <- function(num_genes_select, select_id, 
                                       output) {
  if (num_genes_select == "custom") {
    output[[select_id]] <- renderUI({
      textInput(inputId = select_id,
                label = "<= 150")
    })
    shinyjs::show(id = select_id)
  } else {
    shinyjs::hide(id = select_id)
  }
  
  return(output)
}
