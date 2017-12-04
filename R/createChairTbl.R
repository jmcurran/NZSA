createChairTbl = function(db, overwrite = TRUE){
  progDays = list(Monday = dbReadTable(db, "Monday_Chairs"),
                  Tuesday = dbReadTable(db, "Tuesday_Chairs"),
                  Wednesday = dbReadTable(db, "Wednesday_Chairs"),
                  Thursday = dbReadTable(db, "Thursday_Chairs"))

  getType = function(col){
    keynotes = "(Luke|Simon|Ross|Alan|Alastair|Michael|Jenny)"
    meals =  "(Morning|Lunch|Afternoon|Dinner)"
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
    type[grepl(confClose, col)] = "confclose"
    type[grepl("^$", type)] = "contributed"

    return(type)
  }

  buildTbl = function(){
    chairTbl = data_frame(sessionID = integer(),
                         day = integer(),
                         stream = integer(),
                         roomID = integer(),
                         time = integer(),
                         chair = character(),
                         type = character())

    # every day has:
    # 1. Housekeeping
    # 2. Keynote at 9:10am

    d = 1
    for(day in progDays){
      colnames(day)[1] = "Time"
      day = day %>%
        mutate(Time = as.integer(Time))

      contribSessionRows = which(is.na(day$Time))

      day = day[-1,] ## first row just has the day of the week in it

      types = getType(day$B)

      keynotes = day %>%
        filter(types == "keynote") %>%
        mutate(chair = gsub("^.*CHAIR:(.*$)", "\\1", B))

      contrib = day %>%
        filter(is.na(types))

      # add keynotes - sessionID is day * 1000 + 20 + keynoteNum

      numKeyNotes = nrow(keynotes)
      sessionID = d * 1000 + 20 + 1:numKeyNotes

      chairTbl = chairTbl %>%
        add_row(sessionID = sessionID,
                day = rep(d, numKeyNotes),
                stream = 1,
                roomID = 7,
                time = keynotes$Time,
                chair = keynotes$chair,
                type = "keynote")

      ## Process the contributed sessions

      roomTbl = db %>% dbReadTable("roomTbl")

      contribRooms = contrib %>%
        filter(is.na(Time)) %>%
        select(-Time) %>%
        mutate_all(function(x)gsub("^.*\\(([^)]+)\\)$", "\\1", x)) %>%
        gather() %>%
        left_join(roomTbl, by = c("value" = "rnumber")) %>%
        select(key, roomID) %>%
        group_by(key) %>%
        mutate(id=1:n()) %>%
        spread(key, roomID) %>%
        select(-id)

      ## This is the number of contributor blocks across a day
      numContribBocks = nrow(contribRooms)
      contrib = contrib %>%
        mutate(Time = as.integer(Time))

      sessionTime = contrib$Time[which(is.na(contrib$Time)) + 1]

      for(block in 1:numContribBocks){
        sessionID = d * 1000 + block * 100
        talks = contrib %>%
          filter(Time == sessionTime[block])

        numTalks = nrow(talks)

        ## add a sessionheader
        chairTbl = chairTbl %>%
          add_row(sessionID = sessionID + 10 * 1:6,
                  day = d,
                  stream = 1:6,
                  roomID = contribRooms[block, ] %>% unlist(use.names = FALSE),
                  time = talks$Time[1],
                  chair = NA,
                  type = "sessionheader")


        chairEntries = talks %>% select(LETTERS[2:7]) %>% unlist(use.names = FALSE)
        chairTbl = chairTbl %>%
          add_row(sessionID = sessionID + 10,
                  day = d,
                  stream = 1:6,
                  roomID = contribRooms[block, 1:6] %>% unlist(use.names = FALSE),
                  time = talks$Time,
                  chair = chairEntries,
                  type = "contributed")

      }
      d = d + 1
    }

    return(chairTbl)
  }

  chairTbl = buildTbl()


  dbWriteTable(db, "chairTbl", chairTbl, overwrite = overwrite)

  invisible(db)
}
