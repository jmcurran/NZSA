createProgTbl = function(db, overwrite = TRUE){
  progDays = list(Monday = dbReadTable(db, "Monday"),
                  Tuesday = dbReadTable(db, "Tuesday"),
                  Wednesday = dbReadTable(db, "Wednesday"),
                  Thursday = dbReadTable(db, "Thursday"))

  getType = function(col){
    keynotes = "(Luke|Simon|Ross|Alan|Alastair|Michael|Jenny)"
    meals =  "(Morning|Lunch|Afternoon)"
    posters = "Lightning"
    welcome = "Opening"
    confClose = "Closing"
    housekeeping = "Housekeeping"
    conferenceDay = "(Mon|Tues|Wednes|Thurs)day"

    type = rep(NA, length(col))

    type[grep(housekeeping, col)] = "housekeeping"
    type[grep(conferenceDay, col)] = "day"
    type[grep(meals, col)] = "mealbreak"
    type[grep(posters, col)] = "poster"
    type[grep(welcome, col)] = "open"
    type[grepl(keynotes, col)] = "keynote"
    type[grepl(confClose, col)] = "close"
    type[grepl("^$", type)] = "contributed"

    return(type)
  }


  for(day in progDays){
    colnames(day)[1] = "Time"
    contribSessionRows = which(is.na(day$Time))

  }
}
