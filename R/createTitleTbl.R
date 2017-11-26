createTitleTbl = function(db, overwrite = TRUE){
  allSubmissions = dbReadTable(db, "all_submissions")

  titleTbl = allSubmissions %>%
    rename(subID = X1, title = Title) %>%
    select(subID,title) %>%
    mutate(title = str_trim(title))

  dbWriteTable(db, "titleTbl", titleTbl, overwrite = overwrite)

  invisible(db)
}
