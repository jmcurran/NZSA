getDB = function(dbName = "NZSA"){
  return(dbConnect(RSQLite::SQLite(), dbName))
}
