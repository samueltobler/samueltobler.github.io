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
      radioButtons(
        "source",
        "Quelle wählen",
        choices = c("Gespeicherte Liste" = "saved", "Upload (CSV)" = "upload"),
        selected = "saved"
      ),
      
      conditionalPanel(
        condition = "input.source == 'saved'",
        selectInput(
          "saved_choice",
          "Liste auswählen",
          choices = c(
            "Fortpflanzung" = "data/montagsmaler_fortpflanzung.csv",
            "Zellbio" = "data/montagsmaler_zellbiologie.csv"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.source == 'upload'",
        fileInput("file", "CSV hochladen", accept = c(".csv", "text/csv"))
      ),
      
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
  
  read_words_from_csv <- function(path) {
    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) return(character(0))
    
    x <- paste(unlist(df, use.names = FALSE), collapse = ",")
    parts <- unlist(strsplit(x, "[,;]"))
    
    parts <- trimws(parts)
    parts <- parts[!is.na(parts)]
    parts <- parts[parts != ""]
    unique(parts)
  }
  
  words <- reactive({
    if (input$source == "saved") {
      req(input$saved_choice)
      return(read_words_from_csv(input$saved_choice))
    } else {
      req(input$file)
      return(read_words_from_csv(input$file$datapath))
    }
  })
  
  state <- reactiveValues(history = character(0), pos = 0)
  
  reset_deck <- function() {
    w <- words()
    if (length(w) == 0) {
      state$history <- character(0)
      state$pos <- 0
      return()
    }
    first <- sample(w, 1)
    state$history <- first
    state$pos <- 1
  }
  
  observeEvent(list(input$source, input$saved_choice, input$file), {
    reset_deck()
  }, ignoreInit = TRUE)
  
  observeEvent(input$next_btn, {
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
    if (state$pos == 0) return("Keine Wörter gefunden")
    state$history[state$pos]
  })
  
  output$status <- renderText({
    if (state$pos == 0) return("")
    paste0("Karte ", state$pos, " von ", length(state$history))
  })
  
  output$stats <- renderText({
    w <- words()
    
    src_txt <- if (input$source == "saved") {
      if (input$saved_choice == "data/montagsmaler_fortpflanzung.csv") "Fortpflanzung"
      else if (input$saved_choice == "data/montagsmaler_zellbiologie.csv") "Zellbio"
      else input$saved_choice
    } else {
      "Upload"
    }
    
    paste0(
      "Quelle: ", src_txt, "\n",
      "Wörter gefunden: ", length(w)
    )
  })
}

shinyApp(ui, server)
