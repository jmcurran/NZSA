findAuthorTalk = function(name){
  db = getDB()
  authorTbl = db %>%
    dbReadTable("authorTbl") %>%
    filter(grepl(name, author)) %>%
    select(author, authorID)

  if(nrow(authorTbl) == 0){
    stop(paste0("Cannot find ", name, "\n"))
  }

  authorSubTbl = db %>%
    dbReadTable("authorSubTbl") %>%
    filter(authorID %in% (authorTbl %>% pull(authorID))) %>%
    select(subID)

  cat(paste0(authorSubTbl %>% pull(subID), "\n"))

  progTbl = db %>%
    dbReadTable("progTbl") %>%
    filter(subID %in% (authorSubTbl %>% pull(subID)))

  return(progTbl)


}
