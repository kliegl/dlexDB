library(dlexDB)
library(DBI)

library(dlexDB)
library(DBI)

files_folder <- "/Users/reinholdkliegl/Library/CloudStorage/GoogleDrive-reinhold.kliegl@gmail.com/My Drive/dlexDB/v1"
dir(files_folder)

# Erweiterte Peek-Funktion: Zeigt Inhalt!
peek_content <- function(filename) {
  path <- file.path(files_folder, paste0(filename, ".parquet"))

  if (!file.exists(path)) {
    message("❌ Datei nicht gefunden: ", path)
    return(NULL)
  }

  con <- dlex_connect(demo=TRUE) # Schnelle Verbindung

  tryCatch({
    # Wir holen die ersten 5 Zeilen
    preview <- dbGetQuery(con, paste0("SELECT * FROM '", path, "' LIMIT 5"))

    message(paste("\n✅ --- Inhalt von:", filename, "---"))
    print(preview) # Gibt die Tabelle aus

  }, error = function(e) {
    message("Fehler: ", e$message)
  }, finally = {
    dbDisconnect(con)
  })
}

# check file content
peek_content("dlex_de_char_1gram_c0_v1")    # char_1gram
peek_content("dlex_de_char_1gram_cu_v1")    # char_1gram

peek_content("dlex_de_char_2gram_cc0_v1")   # char_2gram
peek_content("dlex_de_char_2gram_cb_v1")    # char_2gram

peek_content("dlex_de_char_3gram_ccc0_v1")   # char_3gram
peek_content("dlex_de_char_3gram_ct_v1")     # char_3gram
