library(shiny)
library(shinyjs)
library(HH)
library(grid)

make_counts_df <- function(input) {
  data.frame(
    Category = c(
      "Die Prüfungszeit war angemessen.",
      "Die Prüfungsfragen waren klar und fair formuliert.",
      "Die Prüfungsfragen deckten die Lernziele ab."
    ),
    `1` = c(input$a1_1, input$a2_1, input$a3_1),
    `2` = c(input$a1_2, input$a2_2, input$a3_2),
    `3` = c(input$a1_3, input$a2_3, input$a3_3),
    `4` = c(input$a1_4, input$a2_4, input$a3_4),
    `5` = c(input$a1_5, input$a2_5, input$a3_5),
    check.names = FALSE
  )
}

ui <- fluidPage(
  title = "Prüfungsevaluation",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body {
        background: #f3f5f7;
      }

      .container-fluid {
        max-width: 1600px;
      }

      .app-title {
        font-weight: 700;
        margin-bottom: 18px;
      }

      .layout-row {
        display: flex;
        gap: 20px;
        align-items: flex-start;
      }

      .sidebar-panel {
        width: 380px;
        min-width: 380px;
        background: #ffffff;
        border: 1px solid #dde3ea;
        border-radius: 12px;
        padding: 18px;
      }

      .main-panel-full {
        flex: 1;
        min-width: 0;
      }

      .card-panel {
        background: #ffffff;
        border: 1px solid #dde3ea;
        border-radius: 12px;
        padding: 18px;
      }

      .section-title {
        font-weight: 700;
        color: #1f2933;
        margin-bottom: 12px;
      }

      .field-label {
        font-weight: 600;
        margin-bottom: 6px;
        color: #334e68;
      }

      .question-block {
        margin-bottom: 16px;
        padding: 12px;
        border: 1px solid #e3e8ee;
        border-radius: 10px;
        background: #fafbfc;
      }

      .question-title {
        font-weight: 600;
        margin-bottom: 10px;
        line-height: 1.35;
        color: #243b53;
      }

      .scale-row {
        display: flex;
        gap: 8px;
        align-items: flex-end;
        flex-wrap: wrap;
      }

      .scale-box {
        width: 60px;
      }

      .scale-label {
        text-align: center;
        font-size: 12px;
        font-weight: 600;
        color: #52667a;
        margin-bottom: 3px;
      }

      .control-label {
        display: none;
      }

      .form-group {
        margin-bottom: 0;
      }

      .topbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 12px;
        margin-bottom: 10px;
        flex-wrap: wrap;
      }

      .topbar-left,
      .topbar-right {
        display: flex;
        gap: 8px;
        align-items: center;
        flex-wrap: wrap;
      }

      .hint {
        color: #6b7c8f;
        margin-top: 8px;
        font-size: 13px;
      }

      .btn-primary {
        background: #2f6fb0;
        border-color: #2f6fb0;
      }

      .btn-primary:hover {
        background: #285f97;
        border-color: #285f97;
      }
    "))
  ),
  
  titlePanel(div(class = "app-title", "Prüfungsevaluation")),
  
  div(
    class = "layout-row",
    
    div(
      id = "sidebar_wrap",
      class = "sidebar-panel",
      
      div(class = "section-title", "Angaben"),
      
      div(class = "field-label", "Bezeichnung der Prüfung"),
      textInput("pruefung", NULL, value = "", placeholder = "z.B. Zellbiologie-Test"),
      
      div(class = "field-label", "Klasse"),
      textInput("klasse", NULL, value = "", placeholder = "z.B. 4a"),
      
      tags$hr(),
      
      div(class = "section-title", "Antwortzahlen"),
      
      div(
        class = "question-block",
        div(class = "question-title", "1. Die Prüfungszeit war angemessen."),
        div(
          class = "scale-row",
          div(class = "scale-box", div(class = "scale-label", "1"), numericInput("a1_1", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "2"), numericInput("a1_2", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "3"), numericInput("a1_3", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "4"), numericInput("a1_4", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "5"), numericInput("a1_5", NULL, 0, min = 0, width = "60px"))
        )
      ),
      
      div(
        class = "question-block",
        div(class = "question-title", "2. Die Prüfungsfragen waren klar und fair formuliert."),
        div(
          class = "scale-row",
          div(class = "scale-box", div(class = "scale-label", "1"), numericInput("a2_1", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "2"), numericInput("a2_2", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "3"), numericInput("a2_3", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "4"), numericInput("a2_4", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "5"), numericInput("a2_5", NULL, 0, min = 0, width = "60px"))
        )
      ),
      
      div(
        class = "question-block",
        div(class = "question-title", "3. Die Prüfungsfragen deckten die Lernziele ab."),
        div(
          class = "scale-row",
          div(class = "scale-box", div(class = "scale-label", "1"), numericInput("a3_1", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "2"), numericInput("a3_2", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "3"), numericInput("a3_3", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "4"), numericInput("a3_4", NULL, 0, min = 0, width = "60px")),
          div(class = "scale-box", div(class = "scale-label", "5"), numericInput("a3_5", NULL, 0, min = 0, width = "60px"))
        )
      ),
      
      actionButton("generate_plot", "Grafik generieren", class = "btn-primary"),
      div(class = "hint", "Nach dem Generieren kann die Eingabespalte wieder eingeblendet werden.")
    ),
    
    div(
      id = "main_wrap",
      class = "main-panel-full",
      
      conditionalPanel(
        condition = "output.plot_ready == false",
        div(
          class = "card-panel",
          h4("Noch keine Grafik erzeugt"),
          p("Links Prüfung, Klasse und Antwortzahlen eingeben; danach auf «Grafik generieren» klicken.")
        )
      ),
      
      conditionalPanel(
        condition = "output.plot_ready == true",
        div(
          class = "card-panel",
          div(
            class = "topbar",
            div(
              class = "topbar-left",
              actionButton("show_sidebar", "Eingaben anzeigen")
            ),
            div(
              class = "topbar-right",
              downloadButton("download_png", "PNG herunterladen")
            )
          ),
          plotOutput("likert_plot", height = "650px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  
  subtitle_text <- reactive({
    parts <- character(0)
    if (nzchar(trimws(input$pruefung))) {
      parts <- c(parts, paste("Prüfung:", trimws(input$pruefung)))
    }
    if (nzchar(trimws(input$klasse))) {
      parts <- c(parts, paste("Klasse:", trimws(input$klasse)))
    }
    paste(parts, collapse = " | ")
  })
  
  draw_likert <- function(dfx, subtitle = "", cex = 1.75) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(
      cex = cex,
      cex.main = cex * 1.2,
      cex.lab = cex,
      cex.axis = cex,
      mar = c(4, 12, 4, 2)
    )
    
    p <- HH::likert(
      Category ~ .,
      data = dfx,
      ylab = "",
      ReferenceZero = 3,
      as.percent = TRUE,
      positive.order = FALSE,
      main = list("Prüfungsevaluation", x = unit(0.55, "npc")),
      sub = list(subtitle, x = unit(0.55, "npc")),
      xlab = "Anzahl Antworten [%]",
      ylab.right = ""
    )
    
    print(p)
  }
  
  observeEvent(input$generate_plot, {
    current_data(make_counts_df(input))
    shinyjs::hide("sidebar_wrap")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_sidebar, {
    shinyjs::show("sidebar_wrap")
  }, ignoreInit = TRUE)
  
  output$plot_ready <- reactive({
    !is.null(current_data())
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  output$likert_plot <- renderPlot({
    req(current_data())
    draw_likert(current_data(), subtitle = subtitle_text(), cex = 1.75)
  }, res = 150)
  
  output$download_png <- downloadHandler(
    filename = function() {
      parts <- c("pruefungsevaluation")
      if (nzchar(trimws(input$pruefung))) {
        parts <- c(parts, gsub("[^A-Za-z0-9_-]", "_", trimws(input$pruefung)))
      }
      if (nzchar(trimws(input$klasse))) {
        parts <- c(parts, gsub("[^A-Za-z0-9_-]", "_", trimws(input$klasse)))
      }
      paste0(paste(parts, collapse = "_"), ".png")
    },
    content = function(file) {
      req(current_data())
      
      grDevices::png(
        filename = file,
        width = 4400,
        height = 1400,
        res = 400,
        bg = "white"
      )
      
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_likert(current_data(), subtitle = subtitle_text(), cex = 1.7)
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)