library(pins)

dbPath <- "local/cfme.db"

pinDB <- function(
  password,
  dbPath,
  exportPin = "cfme_db_export",
  importPin = "cfme_db_import",
  nBackups = 3
) {
  if (Sys.getenv("adminPass") == "" || Sys.getenv("adminPass") != password) {
    return(list(success = F, msg = "Password incorrect or not activated"))
  }

  board <- board_connect()

  # Backup the existing DB (export it)
  pin_upload(board, dbPath, exportPin)
  pin_versions_prune(board, paste0(board$account, "/", exportPin), n = nBackups)
  # Import the latest upload and replace it locally
  new <- pin_download(board, paste0(board$account, "/", importPin))
  file.copy(new, dbPath, overwrite = T)

  # pin_versions(board, "testDB")
  # recentEdit <- file.info(dbPath)$mtime >
  #   pin_meta(board, paste0(board$account, "/cfme_db_import"))$created
}
