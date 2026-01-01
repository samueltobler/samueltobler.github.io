# app.R
# Installieren falls nötig:
# install.packages(c("shiny", "qrcode", "png", "grid"))

library(shiny)
library(qrcode)
library(png)
library(grid)

ui <- fluidPage(
  titlePanel("QR-Code Generator"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "text",
        "Text / URL",
        value = "https://example.com",
        rows = 4
      ),
      numericInput("size", "Grösse (Pixel)", value = 600, min = 100, max = 2000, step = 50),
      checkboxInput("quiet", "Rand (Quiet Zone) hinzufügen", value = TRUE),
      hr(),
      downloadButton("download_png", "Als PNG herunterladen")
    ),
    mainPanel(
      tags$div(
        style = "max-width: 650px;",
        h4("Vorschau"),
        plotOutput("qr_plot", height = "650px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  qr_matrix <- reactive({
    req(input$text)
    txt <- trimws(input$text)
    validate(need(nchar(txt) > 0, "Bitte Text oder URL eingeben."))
    
    # qrcode::qr_code gibt eine Matrix mit 0/1 zurück
    qrcode::qr_code(txt)
    
  })
  
  output$qr_plot <- renderPlot({
    m <- qr_matrix()
    
    # Optional: Quiet Zone (Rand) hinzufügen
    if (isTRUE(input$quiet)) {
      pad <- 4
      m <- rbind(
        matrix(0, nrow = pad, ncol = ncol(m)),
        m,
        matrix(0, nrow = pad, ncol = ncol(m))
      )
      m <- cbind(
        matrix(0, nrow = nrow(m), ncol = pad),
        m,
        matrix(0, nrow = nrow(m), ncol = pad)
      )
    }
    
    # Darstellung: 1 = schwarz; 0 = weiss
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op), add = TRUE)
    
    image(
      t(apply(m, 2, rev)),
      col = c("white", "black"),
      axes = FALSE,
      asp = 1
    )
  })
  
  output$download_png <- downloadHandler(
    filename = function() {
      # Dateiname aus Text ableiten, aber sicher halten
      base <- gsub("[^A-Za-z0-9_-]+", "_", substr(trimws(input$text), 1, 40))
      if (nchar(base) == 0) base <- "qr"
      paste0(base, ".png")
    },
    content = function(file) {
      m <- qr_matrix()
      
      if (isTRUE(input$quiet)) {
        pad <- 4
        m <- rbind(
          matrix(0, nrow = pad, ncol = ncol(m)),
          m,
          matrix(0, nrow = pad, ncol = ncol(m))
        )
        m <- cbind(
          matrix(0, nrow = nrow(m), ncol = pad),
          m,
          matrix(0, nrow = nrow(m), ncol = pad)
        )
      }
      
      # Matrix in Raster umwandeln (TRUE=schwarz)
      # qrcode liefert i. d. R. 0/1; wir mappen 1->black, 0->white
      # Für PNG erzeugen wir ein RasterGrob
      g <- rasterGrob(
        # Bildmatrix: 1 -> schwarz, 0 -> weiss
        # Wir bauen ein Array mit RGB-Werten
        as.raster(ifelse(t(apply(m, 2, rev)) == 1, "black", "white")),
        interpolate = FALSE
      )
      
      # PNG schreiben
      png(filename = file, width = input$size, height = input$size, bg = "white", res = 96)
      grid.newpage()
      grid.draw(g)
      dev.off()
    }
  )
}

shinyApp(ui, server)
