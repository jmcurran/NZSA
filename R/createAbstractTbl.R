createAbstractTbl = function(db, overwrite = TRUE,
                             path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/Submissions/Rmd/"){
  Files = list.files(path = path, pattern = "Rmd")

  abstractTbl = tibble(abstractID = integer(), subID = integer(), fileName = character(),
                       abstract = character())
  abstractID = 1

  for(f1 in Files){
    abstractTbl = abstractTbl %>%
      add_row(abstractID = abstractID,
              subID = as.integer(gsub("^.*paper_([0-9]{1,3})\\.Rmd$", "\\1", f1)),
              fileName = f1,
              abstract = paste0(readLines(paste0(path, f1), warn = FALSE), collapse = "\n"))
    abstractID = abstractID + 1
  }

  dbWriteTable(db, "abstractTbl", abstractTbl, overwrite = overwrite)

  invisible(db)
}
