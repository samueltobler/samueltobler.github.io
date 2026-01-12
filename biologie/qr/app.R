# app.R
# install.packages(c("shiny", "qrcode", "png", "grid", "colourpicker"))

library(shiny)
library(qrcode)
library(png)
library(grid)
library(colourpicker)

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
      numericInput("size", "Grösse (Pixel)", value = 1200, min = 100, max = 2000, step = 50),
      
      hr(),
      colourInput(
        "fg", "Vordergrund (QR)",
        value = "#000000FF",
        allowTransparent = TRUE
      ),
      colourInput(
        "bg", "Hintergrund",
        value = "#FFFFFFFF",
        allowTransparent = TRUE
      ),
      
      hr(),
      downloadButton("download_png", "Als PNG herunterladen")
    ),
    mainPanel(
      plotOutput("qr_plot", height = "650px")
    )
  )
)

server <- function(input, output, session) {
  
  qr_matrix <- reactive({
    req(input$text)
    txt <- trimws(input$text)
    validate(need(nchar(txt) > 0, "Bitte Text oder URL eingeben."))
    qrcode::qr_code(txt)
  })
  
  output$qr_plot <- renderPlot({
    m <- qr_matrix()
    
    par(mar = c(0, 0, 0, 0))
    image(
      t(apply(m, 2, rev)),
      col = c(input$bg, input$fg),
      axes = FALSE,
      asp = 1
    )
  })
  
  output$download_png <- downloadHandler(
    filename = function() {
      base <- gsub("[^A-Za-z0-9_-]+", "_", substr(trimws(input$text), 1, 40))
      if (nchar(base) == 0) base <- "qr"
      paste0(base, ".png")
    },
    content = function(file) {
      m <- qr_matrix()
      
      img <- as.raster(
        ifelse(t(apply(m, 2, rev)) == 1, input$fg, input$bg)
      )
      
      g <- rasterGrob(img, interpolate = FALSE)
      
      png(
        filename = file,
        width = input$size,
        height = input$size,
        bg = "transparent",
        res = 96
      )
      grid.newpage()
      grid.draw(g)
      dev.off()
    }
  )
}

shinyApp(ui, server)
