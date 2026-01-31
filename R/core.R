#' @import dplyr
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @importFrom duckdb duckdb duckdb_register
#' @importFrom utils download.file
#' @importFrom rappdirs user_cache_dir
NULL

# Globale Variable für den Cache-Pfad
get_cache_dir <- function() {
  rappdirs::user_cache_dir("dlexDB")
}

#' Connect to dlexDB
#'
#' @param version Character. The version of the database to use (default: "v1").
#' @param demo Logical. If TRUE, downloads a small demo dataset (1000 words) for testing.
#' @export
dlex_connect <- function(version = "v1", demo = FALSE) {
  cache_dir <- get_cache_dir()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # Define filenames based on version and mode
  # Naming convention: dlex_de_[mode]_[version].parquet
  mode_str <- if (demo) "demo" else "full"
  filename <- paste0("dlex_de_", mode_str, "_", version, ".parquet")

  # Construct URL (pointing to the specific version folder on HF)
  # Note: Ensure the file exists in the 'v1' folder on Hugging Face!
  db_url <- paste0(
    "https://huggingface.co/datasets/rkliegl/dlexdb/resolve/main/",
    version, "/", filename
  )

  db_file <- file.path(cache_dir, filename)

  # Download logic (same as before, just dynamic now)
  if (!file.exists(db_file)) {
    message(paste("Initializing dlexDB", version, "(", mode_str, ")..."))

    old_timeout <- getOption("timeout")
    options(timeout = 3600)
    on.exit(options(timeout = old_timeout))

    tryCatch({
      utils::download.file(db_url, destfile = db_file, mode = "wb")
      message("Download successful.")
    }, error = function(e) {
      if (file.exists(db_file)) unlink(db_file)
      stop("Download failed: ", e$message)
    })
  }

  con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)
  # We use the View name 'dlex' regardless of version, so downstream code works for both
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW dlex AS SELECT * FROM '", db_file, "'"))

  return(con)
}

#' Abruf lexikalischer Statistiken für eine Wortliste
#' @param word_list Ein Character-Vektor mit den Wörtern.
#' @param db_con Optional: Eine bestehende Verbindung.
#' @return Ein Dataframe mit Statistiken.
#' @export
dlex_lookup <- function(word_list, db_con = NULL) {
  # Auto-Connect
  created_con <- FALSE
  if (is.null(db_con)) {
    db_con <- dlex_connect()
    created_con <- TRUE
  }

  # Verbindung am Ende schließen, falls wir sie hier geöffnet haben
  on.exit({
    if (created_con) DBI::dbDisconnect(db_con)
  })

  # Input Tabelle registrieren
  input_df <- data.frame(search_term = word_list, stringsAsFactors = FALSE)

  # FIX: Falls die Tabelle vom letzten Klick noch da ist -> erst löschen!
  try(duckdb::duckdb_unregister(db_con, "user_input"), silent = TRUE)

  duckdb::duckdb_register(db_con, "user_input", input_df)

  # Die eigentliche Abfrage
  result <- dplyr::tbl(db_con, "user_input") %>%
    dplyr::left_join(dplyr::tbl(db_con, "dlex"), by = c("search_term" = "typ_cit")) %>%
    dplyr::select(
      Wort = search_term,
      PoS = pos_tag,
      Lemma = lem_cit,
      Freq_Norm = typposlem_freq_nor,
      Freq_Abs = typposlem_freq_abs,
      Silben = typ_syls_cnt,
      Orth_Nachbarn = typ_nei_lev_all_cnt_abs
    ) %>%
    dplyr::collect()

  return(result)
}#' Abruf lexikalischer Statistiken für eine Wortliste
#' @param word_list Ein Character-Vektor mit den Wörtern.
#' @param db_con Optional: Eine bestehende Verbindung.
#' @return Ein Dataframe mit Statistiken.
#' @export
dlex_lookup <- function(word_list, db_con = NULL) {
  # Auto-Connect
  created_con <- FALSE
  if (is.null(db_con)) {
    db_con <- dlex_connect()
    created_con <- TRUE
  }

  # Verbindung am Ende schließen, falls wir sie hier geöffnet haben
  on.exit({
    if (created_con) DBI::dbDisconnect(db_con)
  })

  # Input Tabelle registrieren
  input_df <- data.frame(search_term = word_list, stringsAsFactors = FALSE)

  # FIX: Falls die Tabelle vom letzten Klick noch da ist -> erst löschen!
  try(duckdb::duckdb_unregister(db_con, "user_input"), silent = TRUE)

  duckdb::duckdb_register(db_con, "user_input", input_df)

  # Die eigentliche Abfrage
  result <- dplyr::tbl(db_con, "user_input") %>%
    dplyr::left_join(dplyr::tbl(db_con, "dlex"), by = c("search_term" = "typ_cit")) %>%
    dplyr::select(
      Wort = search_term,
      PoS = pos_tag,
      Lemma = lem_cit,
      Freq_Norm = typposlem_freq_nor,
      Freq_Abs = typposlem_freq_abs,
      Silben = typ_syls_cnt,
      Orth_Nachbarn = typ_nei_lev_all_cnt_abs
    ) %>%
    dplyr::collect()

  return(result)
}

#' Suche nach Wörtern mit Regulären Ausdrücken (Regex)
#' @param pattern Ein Regulärer Ausdruck (z.B. "^Ver.*ung$").
#' @param min_freq Minimale normalisierte Frequenz (Standard: 0).
#' @param db_con Optional: Eine bestehende Verbindung.
#' @return Ein Dataframe mit den Treffern.
#' @export
dlex_regex <- function(pattern, min_freq = 0, db_con = NULL) {
  created_con <- FALSE
  if (is.null(db_con)) {
    db_con <- dlex_connect()
    created_con <- TRUE
  }
  on.exit({
    if (created_con) DBI::dbDisconnect(db_con)
  })

  # Hier nutzen wir jetzt explizit dbplyr::sql, um den Konflikt zu vermeiden
  result <- tbl(db_con, "dlex") %>%
    filter(
      dbplyr::sql(paste0("regexp_matches(typ_cit, '", pattern, "')")),
      typposlem_freq_nor >= min_freq
    ) %>%
    select(
      Wort = typ_cit,
      PoS = pos_tag,
      Lemma = lem_cit,
      Freq_Norm = typposlem_freq_nor,
      Freq_Abs = typposlem_freq_abs,
      Silben = typ_syls_cnt
    ) %>%
    collect() %>%
    arrange(desc(Freq_Norm))

  return(result)
}
