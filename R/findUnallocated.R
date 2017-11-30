findUnallocated = function(db){
  titleTbl = db %>%
    dbReadTable("titleTbl") %>%
    filter(!grepl("withdrawn", status, ignore.case = TRUE))

  progTbl = db %>% dbReadTable("progTbl")

  titleTbl %>%
    anti_join(progTbl, by = "subID") %>%
    arrange(subID)

}
