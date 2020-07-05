library(shiny)
library(shinythemes)

# UI
ui <- fluidPage(theme = shinytheme("slate"),
                sidebarPanel(
                  textInput("txt1", "GEO Accession Code:", ""), # default value
                  ),
                mainPanel(
                  h1("Header 1"),
                  h3("Output 1"),
                  verbatimTextOutput("txtout")
                  )
                )

# txt1 and txt2 will be sent to the server (input)
# txtout is generated from the server (output)

# Server
server <- function(input, output) {
  output$txtout <- renderText({
    # Server function: combine first name and surname
    base::paste(input$txt1, input$txt2)
  })
}

shinyApp(ui = ui, server = server)