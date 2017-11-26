createAuthorTbl = function(db, overwrite = TRUE){
  all_authors = db %>% dbReadTable("all_authors")

  authorTbl = all_authors %>%
    distinct(Author) %>%
    rename(author = Author) %>%
    mutate(author = gsub("(Prof(\\.|essor)?\\.?|Dr\\.?)", "", author)) %>%
    mutate(author = str_trim(author)) %>%
    arrange(author)

  authorTbl = authorTbl %>%
    add_column(authorID = 1:nrow(authorTbl), .before = 1)

  dbWriteTable(db, "authorTbl", authorTbl, overwrite = overwrite)

  invisible(db)
}
