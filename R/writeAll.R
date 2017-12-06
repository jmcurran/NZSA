writeAll = function(printVersion = FALSE){
  writeTT(printVersion = printVersion)
  cat("Wrote timetable\n")
  writeProg(printVersion = printVersion)
  cat("Wrote programme\n")
  writeIndexPages()
  cat("Wrote indices\n")
  writeSessionChairs()
  cat("Wrote session chairs\n")
}
