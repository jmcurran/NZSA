createAuthorTbl = function(db, overwrite = TRUE){
  all_authors = db %>% dbReadTable("all_authors")

  authorTbl = all_authors %>%
    distinct(Author, .keep_all = TRUE) %>%
    select(-`X.`) %>%
    rename(author = Author) %>%
    mutate(author = gsub("(Prof(\\.|essor)?\\.?|Dr\\.?)", "", author)) %>%
    mutate(author = str_trim(author)) %>%
    arrange(author)

  authorTbl = authorTbl %>%
    add_column(authorID = 1:nrow(authorTbl), .before = 1)

  affilMapTbl = db %>% dbReadTable("affilMapTbl")

  authorTbl = authorTbl %>%
    left_join(affilMapTbl, by = "Affiliation") %>%
    rename(affilID1 = affilID)

  ## Fix the people who have listed two insitutions
  affilTbl = db %>% dbReadTable("affilTbl")

  misfits = authorTbl %>%
    filter(grepl("/", affil)) %>%
    mutate(affil1 = gsub("(^[^/]+)/.*$", "\\1", affil)) %>%
    mutate(affil2 = gsub("^[^/]+/(.*)$", "\\1", affil))

  misfits = misfits %>%
    mutate(affilID1 = misfits %>%
             left_join(affilTbl, by = c("affil1" = "affil")) %>%
             select(affilID) %>%
             pull(affilID)) %>%
    mutate(affilID2 = misfits %>%
             left_join(affilTbl, by = c("affil2" = "affil")) %>%
             select(affilID) %>%
             pull(affilID)) %>%
    select(authorID, affilID1, affilID2)


  authorTbl = authorTbl %>%
    left_join(misfits, by = "authorID") %>%
    mutate(affilID1.x = case_when(
      is.na(affilID1.y) ~ affilID1.x,
      TRUE ~ affilID1.y)) %>%
    select(-affilID1.y) %>%
    rename(affilID1 = affilID1.x)

  dbWriteTable(db, "authorTbl", authorTbl, overwrite = overwrite)

  invisible(db)
}
