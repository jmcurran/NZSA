createAuthorSubTbl = function(db, overwrite = TRUE){
  all_authors =db %>% dbReadTable("all_authors")
  authorTbl =db %>% dbReadTable("authorTbl")

  all_authors = all_authors %>%
    rename(subID = "X.") %>%
    add_column(allID = 1:nrow(all_authors), .before = 1)

  authorSubTbl = authorTbl %>%
    left_join(all_authors, by = c("author" = "Author")) %>%
    select(allID, authorID, subID) %>%
    rename(authorSubID = allID) %>%
    filter(!is.na(subID)) %>%
    arrange(authorSubID)

  dbWriteTable(db, "authorSubTbl", authorSubTbl)

  return(db)
}
