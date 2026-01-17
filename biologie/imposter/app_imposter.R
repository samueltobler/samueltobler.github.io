# app.R
# Modernes, mobiles UI (Bootstrap 5 via bslib):
# - Responsive Layout; grosse Touch-Buttons; klare Typografie
# - Setup als einklappbares Panel; klappt nach Start automatisch zu
# - Toggle: Imposter-Hinweiswort (word_b) AN/AUS
# - Wörter nur im Modal; nie im Haupt-UI
# - Startspieler bias-randomisiert; Auflösung nur Imposter

library(shiny)
library(bslib)

default_pairs <- data.frame(
  word_a = c(
    "Zelle","DNA","Gen","Chromosom","Vererbung","Mutation","Evolution","Anpassung","Art","Population",
    "Ökosystem","Lebensraum","Umwelt","Nahrung","Energie","Wasser","Luft","Licht","Wärme","Wachstum",
    "Fortpflanzung","Entwicklung","Bewegung","Reiz","Sinne","Gesundheit","Krankheit","Abwehr","Immunsystem","Mikroben",
    "Bakterien","Viren","Pilze","Körper","Organ","Gewebe","Blut","Herz","Atmung","Verdauung",
    "Stoffwechsel","Gehirn","Nerven","Muskeln","Skelett","Haut","Pflanze","Tier","Mensch","Nahrungskette"
  ),
  word_b = c(
    "Innenwelt","Ursprung","Regel","Ordnung","Tradition","Zufall","Wandel","Tendenz","Gruppe","Menge",
    "Zusammenspiel","Ort","Einfluss","Routine","Antrieb","Fluss","Umgebung","Hinweis","Faktor","Prozess",
    "Neustart","Abschnitt","Impuls","Kontakt","Eindruck","Zustand","Störung","Schutz","Alarm","Begleitung",
    "Spuren","Gerücht","Schatten","Hülle","Bereich","Schicht","Transport","Takt","Rhythmus","Umweg",
    "Umsetzung","Zentrale","Leitung","Kraft","Gerüst","Mantel","Wachstum","Instinkt","Alltag","Kreislauf"
  ),
  stringsAsFactors = FALSE
)

pick_pair <- function(df) df[sample.int(nrow(df), 1), , drop = FALSE]

assign_roles <- function(n_players, n_impostors = 1) {
  n_impostors <- max(1, min(n_players - 1, n_impostors))
  roles <- rep("Spieler", n_players)
  roles[sample.int(n_players, n_impostors)] <- "Imposter"
  roles
}

pick_start_index_biased <- function(roles, imposter_start_prob = 0.10) {
  n <- length(roles)
  imp_idx <- which(roles == "Imposter")
  non_idx <- which(roles != "Imposter")
  
  if (length(imp_idx) == 0) return(sample.int(n, 1))
  if (length(non_idx) == 0) return(sample(imp_idx, 1))
  
  p_imp_each <- imposter_start_prob / length(imp_idx)
  p_non_each <- (1 - imposter_start_prob) / length(non_idx)
  
  probs <- rep(0, n)
  probs[imp_idx] <- p_imp_each
  probs[non_idx] <- p_non_each
  
  sample.int(n, 1, prob = probs)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

theme_modern <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter")
)

ui <- fluidPage(
  theme = theme_modern,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
    tags$style(HTML("
      :root { --radius: 16px; }
      body { background: #f6f7fb; }
      .app-wrap { max-width: 740px; margin: 0 auto; padding: 14px 12px 28px; }
      .app-title { font-weight: 800; letter-spacing: -0.2px; }
      .pill { display:inline-flex; align-items:center; gap:8px; border: 1px solid rgba(0,0,0,.08); padding: 6px 10px; border-radius: 999px; font-size: 12px; background: rgba(255,255,255,.7); }
      .cardish { border-radius: var(--radius) !important; box-shadow: 0 10px 30px rgba(0,0,0,.06); border: 1px solid rgba(0,0,0,.06) !important; }
      .btn { border-radius: 14px !important; padding: 12px 14px; font-weight: 650; }
      .btn-wide { width: 100%; }
      .muted { color: rgba(0,0,0,.6); }
      .tight p { margin-bottom: .5rem; }
      textarea.form-control { border-radius: 14px !important; }
      input.form-control { border-radius: 14px !important; }
      .modal-content { border-radius: 18px !important; }
      .word-big { font-size: 30px; font-weight: 850; letter-spacing: -0.3px; }
      .touch-gap { height: 10px; }
    "))
  ),
  
  div(class = "app-wrap",
      div(class = "d-flex align-items-center justify-content-between mb-2",
          div(
            tags$div(class = "app-title h3 mb-0", "🤓 Impostor: Bio-Version"),
          ),
          uiOutput("step_badge")
      ),
      
      card(
        class = "cardish",
        card_body(uiOutput("setup_ui"))
      ),
      
      div(class = "touch-gap"),
      
      card(
        class = "cardish",
        card_body(uiOutput("main_ui"))
      )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    started = FALSE,
    idx = 1,
    players = character(0),
    roles = character(0),
    word_player = "",
    word_imposter = "",
    setup_open = TRUE,
    start_player = "",
    show_imposter_word = TRUE
  )
  
  get_players <- reactive({
    p <- trimws(unlist(strsplit(input$players, "\n", fixed = TRUE)))
    p <- p[nzchar(p)]
    unique(p)
  })
  
  output$step_badge <- renderUI({
    if (!state$started) return(span(class = "pill", "Setup"))
    n <- length(state$players)
    if (state$idx >= 1 && state$idx <= n) return(span(class = "pill", paste0("Reveal ", state$idx, "/", n)))
    if (state$idx == n + 1) return(span(class = "pill", "Runde läuft"))
    if (state$idx == n + 2) return(span(class = "pill", "Auflösung"))
    span(class = "pill", "Status")
  })
  
  output$setup_ui <- renderUI({
    header <- div(class = "d-flex align-items-center justify-content-between",
                  tags$h4(class = "mb-0", "Setup"),
                  actionButton(
                    "toggle_setup",
                    if (state$setup_open) "Schliessen" else "Öffnen",
                    class = "btn btn-outline-secondary"
                  )
    )
    
    body <- NULL
    if (state$setup_open) {
      body <- div(
        class = "mt-3 tight",
        textAreaInput(
          "players",
          "Spielernamen; ein Name pro Zeile",
          value = isolate(input$players) %||% paste0("Spieler ", 1:6, collapse = "\n"),
          rows = 7
        ),
        numericInput("n_impostors", "Anzahl Imposters", value = 1, min = 1, step = 1),
        
        actionButton(
          "toggle_imposter_word",
          if (isTRUE(state$show_imposter_word)) "Imposter-Hinweiswort: AN" else "Imposter-Hinweiswort: AUS",
          class = if (isTRUE(state$show_imposter_word)) "btn btn-success btn-wide" else "btn btn-outline-secondary btn-wide"
        ),
        tags$p(class = "muted", "AN: Imposter sieht Hinweis; AUS: Imposter sieht kein Wort."),
        
        actionButton("start_round", "Runde starten", class = "btn btn-primary btn-wide"),
        div(class = "touch-gap"),
        actionButton("new_round", "Neue Runde", class = "btn btn-outline-secondary btn-wide"),
      )
    }
    
    tagList(header, body)
  })
  
  observeEvent(input$toggle_setup, {
    state$setup_open <- !isTRUE(state$setup_open)
  })
  
  observeEvent(input$toggle_imposter_word, {
    state$show_imposter_word <- !isTRUE(state$show_imposter_word)
  })
  
  observeEvent(input$start_round, {
    players <- get_players()
    n <- length(players)
    validate(
      need(n >= 3, "Mindestens 3 Spieler nötig."),
      need(input$n_impostors <= n - 1, "Anzahl Imposters muss kleiner als Spielerzahl sein.")
    )
    
    pair <- pick_pair(default_pairs)
    roles <- assign_roles(n, input$n_impostors)
    
    state$word_player <- pair$word_a[1]
    state$word_imposter <- pair$word_b[1]
    
    si <- pick_start_index_biased(roles, imposter_start_prob = 0.10)
    
    state$started <- TRUE
    state$idx <- 1
    state$players <- players
    state$roles <- roles
    state$start_player <- players[si]
    
    state$setup_open <- FALSE
    removeModal()
  })
  
  observeEvent(input$new_round, {
    state$started <- FALSE
    state$idx <- 1
    state$players <- character(0)
    state$roles <- character(0)
    state$word_player <- ""
    state$word_imposter <- ""
    state$start_player <- ""
    state$setup_open <- TRUE
    removeModal()
  })
  
  show_word_modal <- function(player_name, role) {
    title <- paste0("Nur für: ", player_name)
    
    if (role == "Imposter") {
      if (isTRUE(state$show_imposter_word)) {
        body <- tagList(
          tags$h4("Du bist IMPOSTER"),
          div(class = "word-big my-2", state$word_imposter),
          tags$p(class = "muted mb-0", "Merke dir das Wort; schliesse danach das Fenster.")
        )
      } else {
        body <- tagList(
          tags$h4("Du bist IMPOSTER"),
          tags$p(class = "muted mb-0", "Du bekommst kein Wort; schliesse danach das Fenster.")
        )
      }
    } else {
      body <- tagList(
        tags$h4("Dein Wort"),
        div(class = "word-big my-2", state$word_player),
        tags$p(class = "muted mb-0", "Merke dir das Wort; schliesse danach das Fenster.")
      )
    }
    
    showModal(modalDialog(
      title = title,
      body,
      footer = actionButton("close_word", "Ich habe es mir gemerkt", class = "btn btn-primary btn-wide"),
      size = "m",
      easyClose = FALSE
    ))
  }
  
  observeEvent(input$reveal_modal, {
    req(state$started)
    n <- length(state$players)
    req(state$idx >= 1 && state$idx <= n)
    
    player <- state$players[state$idx]
    role <- state$roles[state$idx]
    show_word_modal(player, role)
  })
  
  observeEvent(input$close_word, {
    removeModal()
  })
  
  observeEvent(input$next_player, {
    req(state$started)
    n <- length(state$players)
    state$idx <- if (state$idx < n) state$idx + 1 else n + 1
    removeModal()
  })
  
  observeEvent(input$show_summary, {
    req(state$started)
    state$idx <- length(state$players) + 2
    removeModal()
  })
  
  output$main_ui <- renderUI({
    if (!state$started) {
      return(tagList(
        tags$h4("So läufts"),
        tags$p(class = "muted",
               "Gerät herumgeben; jede Person klickt „Wort anzeigen“; Popup schliessen; dann „Nächster Spieler“."),
        tags$ul(
          tags$li("Reihum je ein Hinweiswort sagen; keine direkten Wortteile."),
          tags$li("Dann Diskussion und Abstimmung; Imposter gewinnt; wenn er nicht entdeckt wird.")
        )
      ))
    }
    
    n <- length(state$players)
    
    if (state$idx >= 1 && state$idx <= n) {
      player <- state$players[state$idx]
      return(tagList(
        tags$h4(paste0("Jetzt: ", player)),
        tags$p(class = "muted", "Nur dieser Person zeigen; danach sofort weitergeben."),
        actionButton("reveal_modal", "Wort anzeigen", class = "btn btn-success btn-wide"),
        div(class = "touch-gap"),
        actionButton(
          "next_player",
          if (state$idx < n) "Nächster Spieler" else "Runde starten",
          class = "btn btn-primary btn-wide"
        ),
        tags$hr(),
        tags$p(class = "muted mb-0", "Das Wort erscheint nur im Popup und ist danach nicht mehr sichtbar.")
      ))
    }
    
    if (state$idx == n + 1) {
      return(tagList(
        tags$h4("Runde läuft"),
        tags$p(paste0("Die Runde beginnt bei: ", state$start_player)),
        actionButton("show_summary", "Auflösung anzeigen", class = "btn btn-warning btn-wide")
      ))
    }
    
    if (state$idx == n + 2) {
      imposters <- state$players[state$roles == "Imposter"]
      return(tagList(
        tags$h4("Auflösung"),
        tags$p("Imposter:"),
        tags$ul(lapply(imposters, tags$li)),
        tags$hr(),
        actionButton("new_round", "Neue Runde", class = "btn btn-primary btn-wide")
      ))
    }
    
    tagList(tags$h4("Unbekannter Zustand"))
  })
}

shinyApp(ui, server)
