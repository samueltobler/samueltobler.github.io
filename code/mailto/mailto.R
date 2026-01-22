# Pakete (einmalig installieren, falls nötig):
# install.packages(c("readxl"))

library(readxl)

pfad <- "priv/Noten_2ePS_Photosynthese.xlsx"

# Erwartung: Spalte 1 = Betreff; Spalte 2 = Text; Spalte 3 = Mail
df <- read_excel(pfad, col_names = FALSE)
names(df) <- c("betreff", "text", "mail")

# Falls in der Excel-Datei eine Kopfzeile drin ist, diese Zeile entfernen:
# df <- df[-1, ]

# Hilfsfunktionen
esc_as <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)   # Backslashes
  x <- gsub('"', '\\"', x)           # Anführungszeichen
  x
}

run_applescript <- function(script_text) {
  f <- tempfile(fileext = ".applescript")
  writeLines(script_text, f, useBytes = TRUE)
  
  out <- tryCatch(
    system2("osascript", f, stdout = TRUE, stderr = TRUE),
    error = function(e) paste("R-Fehler:", conditionMessage(e))
  )
  
  list(file = f, output = out)
}

# Mails als Entwurf öffnen (Apple Mail)
for (i in seq_len(nrow(df))) {
  subject <- esc_as(df$betreff[i])
  body    <- esc_as(paste0(df$text[i], "\n"))
  to      <- esc_as(df$mail[i])
  
  script <- sprintf('
tell application "Mail"
  activate
  set newMessage to make new outgoing message with properties {subject:"%s", content:"%s", visible:true}
  tell newMessage
    make new to recipient at end of to recipients with properties {address:"%s"}
  end tell
end tell
', subject, body, to)
  
  r <- run_applescript(script)
  
  cat("\n--- Eintrag", i, "---\n")
  cat(paste(r$output, collapse = "\n"), "\n")
}
