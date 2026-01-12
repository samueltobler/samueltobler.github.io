# app.R
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card {
        max-width: 700px;
        margin: 30px auto;
        padding: 55px 35px;
        border: 2px solid #222;
        border-radius: 16px;
        background: #fff;
        text-align: center;
        font-size: 44px;
        font-weight: 700;
        line-height: 1.2;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
      }
      .hint { text-align:center; color:#444; margin-top:10px; }
      .controls { text-align:center; margin-top: 15px; }
    "))
  ),
  
  titlePanel("Montagsmaler"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "CSV hochladen", accept = c(".csv", "text/csv")),
      tags$hr(),
      verbatimTextOutput("stats")
    ),
    mainPanel(
      div(class = "card", textOutput("word")),
      div(class = "controls",
          actionButton("prev_btn", "Zurück"),
          actionButton("next_btn", "Weiter")
      ),
      div(class = "hint", textOutput("status"))
    )
  )
)

server <- function(input, output, session) {
  
  words <- reactive({
    req(input$file)
    
    df <- tryCatch(
      read.csv(
        input$file$datapath,
        header = TRUE,
        stringsAsFactors = FALSE,
        sep = ",",
        check.names = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(df) || nrow(df) == 0) {
      raw_lines <- readLines(input$file$datapath, warn = FALSE, encoding = "UTF-8")
      df <- data.frame(V1 = raw_lines, stringsAsFactors = FALSE)
    }
    
    x <- paste(unlist(df, use.names = FALSE), collapse = ",")
    parts <- unlist(strsplit(x, "[,;]"))
    
    if (isTRUE(input$trim)) parts <- trimws(parts)
    
    parts <- parts[!is.na(parts)]
    parts <- parts[parts != ""]
    
    if (isTRUE(input$dedup)) parts <- unique(parts)
    
    parts
  })
  
  state <- reactiveValues(
    history = character(0),
    pos = 0
  )
  
  observeEvent(input$file, {
    w <- words()
    if (length(w) == 0) {
      state$history <- character(0)
      state$pos <- 0
      return()
    }
    first <- sample(w, 1)
    state$history <- first
    state$pos <- 1
  })
  
  observeEvent(input$next_btn, {
    req(input$file)
    w <- words()
    if (length(w) == 0) return()
    
    new_word <- sample(w, 1)
    
    if (state$pos < length(state$history)) {
      state$history <- state$history[1:state$pos]
    }
    
    state$history <- c(state$history, new_word)
    state$pos <- state$pos + 1
  })
  
  observeEvent(input$prev_btn, {
    if (state$pos > 1) state$pos <- state$pos - 1
  })
  
  output$word <- renderText({
    if (state$pos == 0) return("CSV hochladen")
    state$history[state$pos]
  })
  
  output$status <- renderText({
    if (state$pos == 0) return("")
    paste0("Karte ", state$pos, " von ", length(state$history))
  })
  
  output$stats <- renderText({
    if (is.null(input$file)) return("Noch kein File geladen.")
    w <- words()
    paste0(
      "Wörter gefunden: ", length(w), "\n",
      "Duplikate entfernen: ", if (isTRUE(input$dedup)) "ja" else "nein", "\n",
      "Trimmen: ", if (isTRUE(input$trim)) "ja" else "nein"
    )
  })
}

shinyApp(ui, server)
