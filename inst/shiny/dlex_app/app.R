library(shiny)
library(dlexDB)
library(DT)
library(dplyr)

# UI Definition
ui <- fluidPage(
  titlePanel("dlexDB Interface"),

  sidebarLayout(
    sidebarPanel(
      # Switch between Full and Demo data
      checkboxInput("use_demo", "Use Demo Data (Fast/Test)", value = TRUE),
      hr(),

      # Conditional UI: Show different inputs based on the selected Tab
      conditionalPanel(
        condition = "input.tabs == 'Lookup'",
        textAreaInput("word_list", "Paste Words (one per line or space-separated):",
                      value = "Haus\nMaus\nund\noder", height = "200px"),
        actionButton("btn_lookup", "Get Statistics", class = "btn-primary")
      ),

      conditionalPanel(
        condition = "input.tabs == 'Regex'",
        textInput("regex_pattern", "Regex Pattern:", value = "^Ver.*ungen$"),
        numericInput("min_freq", "Min. Norm. Frequency:", value = 0, step = 1),
        actionButton("btn_regex", "Search Database", class = "btn-primary")
      ),

      hr(),
      helpText("Data source: dlexDB / DWDS Kernkorpus")
    ),

    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Lookup",
                           br(),
                           DTOutput("table_lookup")
                  ),
                  tabPanel("Regex",
                           br(),
                           DTOutput("table_regex")
                  )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Connect to DB based on the checkbox (Reactive Connection)
  get_con <- reactive({
    dlex_connect(demo = input$use_demo)
  })

  # --- Tab 1: Lookup Logic ---
  lookup_data <- eventReactive(input$btn_lookup, {
    req(input$word_list)
    con <- get_con()

    # Split input text into a clean vector
    words <- unlist(strsplit(input$word_list, "[[:space:],;]+"))
    words <- words[words != ""]

    dlex_lookup(words, db_con = con)
  })

  output$table_lookup <- renderDT({
    datatable(lookup_data(),
              options = list(pageLength = 15, scrollX = TRUE),
              filter = "top")
  })

  # --- Tab 2: Regex Logic ---
  regex_data <- eventReactive(input$btn_regex, {
    req(input$regex_pattern)
    con <- get_con()

    dlex_regex(input$regex_pattern, min_freq = input$min_freq, db_con = con)
  })

  output$table_regex <- renderDT({
    datatable(regex_data(),
              options = list(pageLength = 15, scrollX = TRUE),
              filter = "top")
  })
}

shinyApp(ui, server)
