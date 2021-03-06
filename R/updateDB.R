updateDB  = function(ss, progOnly = TRUE){

  Worksheets = c("Monday", "Tuesday", "Wednesday", "Thursday")

  db = dbConnect(RSQLite::SQLite(), "NZSA")

  ## read the days first
  d = 1
  for(worksheet in Worksheets[1:4]){
    rng = switch(d, "A1:G23", "A1:G19", "A1:G22", "A1:G11")
    rngChairs = switch(d, "A1:G14", "A1:G11", "A1:G13", "A1:G6")

    chairsheet = paste0(worksheet, "_Chairs")

    dayWs = ss %>% gs_read(ws = worksheet, range = rng, col_names = LETTERS[1:7])
    chairsWs = ss %>% gs_read(ws = chairsheet, range = rngChairs, col_names = LETTERS[1:7])
    dbWriteTable(db, worksheet, dayWs, overwrite = TRUE)
    dbWriteTable(db, chairsheet, chairsWs, overwrite = TRUE)

    d = d + 1
  }

  if(!progOnly){
    allSubs = ss %>% gs_read(ws = "All Submissions")
    allocs = ss %>% gs_read(ws = "Allocations")
    all_authors = ss %>% gs_read(ws = "All_Authors")

    dbWriteTable(db, "all_submissions", allSubs, overwrite = TRUE)
    dbWriteTable(db, "allocations", allocs, overwrite = TRUE)
    dbWriteTable(db, "all_authors", all_authors, overwrite = TRUE)
  }



  #dbListTables(db)
  #dbDisconnect(db)
}
