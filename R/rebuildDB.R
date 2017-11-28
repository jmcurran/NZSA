rebuildDB = function(refreshWebData = FALSE){
  if(refreshWebData){
    programme = getProgrammeSheet()
    programme %>% updateDB(FALSE)
  }

  db = getDB()
  db = db %>%
    createRoomsTbl() %>%
    createAffilTbl() %>%
    createTitleTbl() %>%
    createAbstractTbl() %>%
    createAuthorTbl() %>%
    createAuthorSubTbl() %>%
    createProgTbl()

}
