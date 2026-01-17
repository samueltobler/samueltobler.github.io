# app.R
# Änderungen:
# - word_a ist das Hauptwort (für alle Spieler); word_b ist das Hinweiswort (nur für Imposter)
# - Toggle-Button, ob das Hinweiswort (word_b) dem Imposter angezeigt wird (AN/AUS)
# - Startspieler bleibt bias-randomisiert; Setup klappt nach Start zu; Spielernamen bleiben bei "Neue Runde"

library(shiny)

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

ui <- fluidPage(
  tags$style(HTML("
    .wrap { max-width: 760px; margin: 0 auto; }
    .card { border: 1px solid #e5e7eb; border-radius: 12px; padding: 18px; }
    .muted { color: #6b7280; }
    .btn-wide { width: 100%; }
    .title { font-weight: 700; font-size: 22px; margin-bottom: 8px; }
    .step { display:inline-block; padding:4px 10px; border:1px solid #e5e7eb; border-radius:999px; font-size: 12px; }
    textarea, input { border-radius: 10px !important; }
    .btn { border-radius: 10px !important; }
    .setup-head { display:flex; align-items:center; justify-content:space-between; gap:10px; }
    .setup-body { margin-top: 10px; }
  ")),
  div(class = "wrap",
      div(class = "card",
          div(class = "title", "🧠 Impostor | Bio-Version"),
          uiOutput("step_badge"),
          tags$hr(),
          uiOutput("setup_ui"),
          tags$hr(),
          uiOutput("main_ui")
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
    if (!state$started) return(span(class = "step", "Setup"))
    n <- length(state$players)
    if (state$idx >= 1 && state$idx <= n) return(span(class = "step", paste0("Reveal ", state$idx, "/", n)))
    if (state$idx == n + 1) return(span(class = "step", "Runde läuft"))
    if (state$idx == n + 2) return(span(class = "step", "Auflösung"))
    span(class = "step", "Status")
  })
  
  output$setup_ui <- renderUI({
    tagList(
      div(class = "setup-head",
          tags$h3(style = "margin:0;", "Setup"),
          actionButton("toggle_setup", if (state$setup_open) "Schliessen" else "Öffnen",
                       class = "btn btn-default")
      ),
      if (state$setup_open) {
        div(class = "setup-body",
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
              class = if (isTRUE(state$show_imposter_word)) "btn btn-success btn-wide" else "btn btn-default btn-wide"
            ),
            tags$p(class = "muted",
                   "AN: Imposter sieht Hinweis; AUS kein Hinweis."),
            
            actionButton("start_round", "Runde starten", class = "btn-primary btn-wide"),
            tags$div(style = "height:8px;"),
            actionButton("new_round", "Neue Runde", class = "btn-default btn-wide"),
            tags$p(class = "muted", style = "margin-top:12px;",
                   "word_a für Spieler; word_b als Hinweiswort für Imposter; Startspieler randomisiert; Imposter beginnt selten.")
        )
      }
    )
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
          div(style = "font-size:28px; font-weight:800; padding:10px 0;", state$word_imposter),
          tags$p(class = "muted", "Merke dir das Wort; schliesse danach das Fenster.")
        )
      } else {
        body <- tagList(
          tags$h4("Du bist IMPOSTER"),
          tags$p(class = "muted", "Du bekommst kein Wort; schliesse danach das Fenster.")
        )
      }
    } else {
      body <- tagList(
        tags$h4("Dein Wort"),
        div(style = "font-size:28px; font-weight:800; padding:10px 0;", state$word_player),
        tags$p(class = "muted", "Merke dir das Wort; schliesse danach das Fenster.")
      )
    }
    
    showModal(modalDialog(
      title = title,
      body,
      footer = actionButton("close_word", "Ich habe es mir gemerkt", class = "btn-primary"),
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
    if (state$idx < n) {
      state$idx <- state$idx + 1
    } else {
      state$idx <- n + 1
    }
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
        tags$p(class = "muted",
               "Ablauf: Gerät herumgeben; jede Person klickt „Wort anzeigen“; Popup schliessen; dann „Nächster Spieler“."),
        tags$ul(
          tags$li("Reihum je ein Hinweiswort sagen; keine direkten Wortteile."),
          tags$li("Danach Diskussion und Abstimmung; Imposter gewinnt, wenn er nicht entdeckt wird.")
        )
      ))
    }
    
    n <- length(state$players)
    
    if (state$idx >= 1 && state$idx <= n) {
      player <- state$players[state$idx]
      return(tagList(
        tags$h3(paste0("Jetzt: ", player)),
        tags$p(class = "muted", "Nur dieser Person geben; danach sofort weitergeben."),
        actionButton("reveal_modal", "Wort anzeigen", class = "btn-success btn-wide"),
        tags$div(style = "height:8px;"),
        actionButton("next_player", if (state$idx < n) "Nächster Spieler" else "Runde starten",
                     class = "btn-primary btn-wide"),
        tags$hr(),
        tags$p(class = "muted", "Das Wort erscheint nur im Popup und ist danach nicht mehr sichtbar.")
      ))
    }
    
    if (state$idx == n + 1) {
      return(tagList(
        tags$h3("Runde läuft"),
        tags$p(paste0("Die Runde beginnt bei: ", state$start_player)),
        actionButton("show_summary", "Auflösung anzeigen", class = "btn-warning btn-wide")
      ))
    }
    
    if (state$idx == n + 2) {
      imposters <- state$players[state$roles == "Imposter"]
      return(tagList(
        tags$h3("Auflösung"),
        tags$p("Imposter:"),
        tags$ul(lapply(imposters, tags$li)),
        tags$hr(),
        actionButton("new_round", "Neue Runde", class = "btn-primary btn-wide")
      ))
    }
    
    tagList(tags$h3("Unbekannter Zustand"))
  })
}

shinyApp(ui, server)
