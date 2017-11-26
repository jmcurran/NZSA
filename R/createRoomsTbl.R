createRoomsTbl = function(db, overwrite = TRUE){
  Monday = dbReadTable(db, "Monday")
  i = which(is.na(Monday$A))[1]
  Rooms = Monday[i,LETTERS[2:7]]
  roomTbl = tibble(
    rname = str_trim(gsub("(^[^(]+).*$", "\\1", Rooms)),
    rnumber = gsub("^.*\\(([^)]+)\\)$", "\\1", Rooms))

  roomTbl = roomTbl %>%
    arrange(rnumber) %>%
    add_column(roomID = 1:nrow(roomTbl), .before = 1)

  dbWriteTable(db, "roomTbl", roomTbl, overwrite = overwrite)

  return(db)
}
