refreshAbstracts = function(){
  db = getDB()
  db = db %>% createAbstractTbl()
}
