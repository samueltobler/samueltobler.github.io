# app.R
library(shiny)
library(jsonlite)

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

      /* Sidebar Toggle */
      #sidebar { transition: all 0.25s ease-in-out; }
      body.sidebar-hidden #sidebar { display: none; }
      body.sidebar-hidden #main { width: 100% !important; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleSidebar', function(message) {
        document.body.classList.toggle('sidebar-hidden');
      });
    "))
  ),
  
  titlePanel(
    div(
      style = "display:flex; align-items:center; justify-content:space-between;",
      span("Montagsmaler"),
      actionButton("toggle_sidebar_btn", "ℹ️")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      
      radioButtons(
        "source",
        "Quelle wählen",
        choices = c("GitHub-Liste" = "saved", "Upload (CSV)" = "upload"),
        selected = "saved"
      ),
      
      conditionalPanel(
        condition = "input.source == 'saved'",
        uiOutput("saved_ui"),
        actionButton("refresh_btn", "Liste aktualisieren")
      ),
      
      conditionalPanel(
        condition = "input.source == 'upload'",
        fileInput("file", "CSV hochladen", accept = c(".csv", "text/csv"))
      ),
      
      tags$hr(),
      verbatimTextOutput("stats")
    ),
    
    mainPanel(
      id = "main",
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
  
  owner  <- "samueltobler"
  repo   <- "samueltobler.github.io"
  branch <- "main"
  folder <- "biologie/montagsmaler/data"
  
  get_github_csv_choices <- function() {
    api <- sprintf(
      "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
      owner, repo, folder, branch
    )
    
    x <- tryCatch(jsonlite::fromJSON(api), error = function(e) NULL)
    if (is.null(x) || !is.data.frame(x) || !"name" %in% names(x)) return(list())
    
    x <- x[grepl("\\.csv$", x$name, ignore.case = TRUE), , drop = FALSE]
    if (nrow(x) == 0) return(list())
    
    raw_urls <- sprintf(
      "https://raw.githubusercontent.com/%s/%s/%s/%s/%s",
      owner, repo, branch, folder, x$name
    )
    
    labels <- sub("\\.csv$", "", x$name, ignore.case = TRUE)
    
    stats::setNames(as.list(raw_urls), labels)
  }
  
  github_choices <- reactiveVal(list())
  
  load_github_choices <- function() {
    choices <- get_github_csv_choices()
    github_choices(choices)
    
    if (length(choices) > 0) {
      current <- isolate(input$saved_choice)
      if (is.null(current) || !(current %in% unlist(choices, use.names = FALSE))) {
        updateSelectInput(session, "saved_choice",
                          choices = choices,
                          selected = unlist(choices, use.names = FALSE)[1])
      } else {
        updateSelectInput(session, "saved_choice",
                          choices = choices,
                          selected = current)
      }
    } else {
      updateSelectInput(session, "saved_choice", choices = list("Keine CSV gefunden" = ""))
    }
  }
  
  observe({
    load_github_choices()
  })
  
  observeEvent(input$refresh_btn, {
    load_github_choices()
  })
  
  output$saved_ui <- renderUI({
    selectInput("saved_choice", "Liste auswählen", choices = github_choices())
  })
  
  read_words_from_csv <- function(source) {
    df <- tryCatch(
      read.csv(source, stringsAsFactors = FALSE, check.names = FALSE),
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
      if (identical(input$saved_choice, "")) return(character(0))
      read_words_from_csv(input$saved_choice)
    } else {
      req(input$file)
      read_words_from_csv(input$file$datapath)
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
    if (state$pos == 0) {
      if (input$source == "upload") return("CSV hochladen")
      return("Liste wählen")
    }
    state$history[state$pos]
  })
  
  output$status <- renderText({
    if (state$pos == 0) return("")
    paste0("Karte ", state$pos, " von ", length(state$history))
  })
  
  output$stats <- renderText({
    w <- words()
    
    src_txt <- if (input$source == "saved") {
      choices <- github_choices()
      if (length(choices) == 0) {
        "GitHub (keine Daten)"
      } else {
        nm <- names(choices)[match(input$saved_choice, unlist(choices, use.names = FALSE))]
        if (length(nm) == 0 || is.na(nm)) "GitHub" else nm
      }
    } else {
      "Upload"
    }
    
    paste0(
      "Quelle: ", src_txt, "\n",
      "Wörter gefunden: ", length(w)
    )
  })
  
  observeEvent(input$toggle_sidebar_btn, {
    session$sendCustomMessage("toggleSidebar", list())
  })
}

shinyApp(ui, server)
