render_filters <- function(output, filters) {
  
  output$newInputs <- renderUI({
    lapply(filters$input_filters, function(a) {
      tagList(
        selectInput(inputId = a[1], 
                    label = "",
                    choices = c("logFC", "AveExpr", "t", "P.Value",
                                "adj.P.Val", "B"),
                    multiple = F),
        selectInput(inputId = a[1], 
                    label = "",
                    choices = c(">", ">=", "<", "<=", "="),
                    multiple = F),
        textInput(a[1], a[2], value = a[3]),
        HTML("<br>")
      )
    })
  })
  
  return(output)
}