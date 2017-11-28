createProgTbl = function(db, overwrite = TRUE){
  progDays = list(Monday = dbReadTable(db, "Monday"),
                  Tuesday = dbReadTable(db, "Tuesday"),
                  Wednesday = dbReadTable(db, "Wednesday"),
                  Thursday = dbReadTable(db, "Thursday"))

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
    programmeID = 1
    progTbl = data_frame(programmeID = integer(),
                         sessionID = integer(),
                         day = integer(),
                         block = integer(),
                         stream = integer(),
                         roomID = integer(),
                         time = integer(),
                         rawEntry = character(),
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

      housekeeping = day %>%
        filter(types == "housekeeping")

      meals = day %>%
        filter(types == "mealbreak")

      ceremonies = day %>%
        filter(types == "open" | types == "confclose")

      keynotes = day %>%
        filter(types == "keynote")

      contrib = day %>%
        filter(is.na(types))

      # add Housekeeping

      sessionID = d * 1000

      progTbl = progTbl %>%
        add_row(programmeID = programmeID,
                sessionID = sessionID,
                day = d,
                stream = NA,
                roomID = 7,
                time = housekeeping$Time,
                rawEntry = housekeeping$B,
                type = "housekeeping")

      programmeID = programmeID + 1

      # add meals - sessionID is day * 1000 + 10 + mealNum

      numMeals = nrow(meals)
      sessionID = d * 1000 + 10 + 1:numMeals

      progTbl = progTbl %>%
        add_row(programmeID = programmeID + (0:(numMeals - 1)),
                sessionID = sessionID,
                day = rep(d, numMeals),
                stream = NA,
                roomID = NA,
                time = meals$Time,
                rawEntry = meals$B,
                type = "mealbreak")

      programmeID = programmeID + numMeals

      # add keynotes - sessionID is day * 1000 + 20 + keynoteNum

      numKeyNotes = nrow(keynotes)
      sessionID = d * 1000 + 20 + 1:numKeyNotes

      progTbl = progTbl %>%
        add_row(programmeID = programmeID + (0:(numKeyNotes - 1)),
                sessionID = sessionID,
                day = rep(d, numKeyNotes),
                stream = 1,
                roomID = 7,
                time = keynotes$Time,
                rawEntry = keynotes$B,
                type = "keynote")

      programmeID = programmeID + numKeyNotes

      if(nrow(ceremonies) > 0){
        ## add ceremonies - sessionID is day * 1000 + 30 + ceremony
        ## There will only ever be 1 per day

        sessionID = d * 1000 + 30

        progTbl = progTbl %>%
          add_row(programmeID = programmeID,
                  sessionID = sessionID,
                  day = d,
                  stream = 1,
                  roomID = 7,
                  time = ceremonies$Time,
                  rawEntry = ceremonies$B,
                  type = getType(ceremonies$B))

        programmeID = programmeID + 1

      }

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

      blockStart = contrib$Time[which(is.na(contrib$Time)) + 1]
      blockEnd = contrib$Time[which(is.na(c(contrib$Time, NA))) - 1]


      for(block in 1:numContribBocks){
        sessionID = d * 1000 + block * 100
        talks = contrib %>%
          filter(Time >= blockStart[block] & Time <= blockEnd[block])

        numTalks = nrow(talks)

        ## add a sessionheader
        progTbl = progTbl %>%
          add_row(programmeID = programmeID + (0:5),
                  sessionID = sessionID + 10 * 1:6,
                  day = d,
                  stream = 1:6,
                  roomID = contribRooms[block, ] %>% unlist(use.names = FALSE),
                  time = talks$Time[1],
                  rawEntry = NA,
                  type = "sessionheader")

        programmeID = programmeID + 6


        for(stream in 1:6){
          talkEntries = talks %>% select((LETTERS[2:7])[stream]) %>% unlist(use.names = FALSE)
          progTbl = progTbl %>%
            add_row(programmeID = programmeID + (0:(numTalks - 1)),
                    sessionID = sessionID + 10 * stream,
                    day = d,
                    stream = stream,
                    roomID = contribRooms[block, stream] %>% unlist(use.names = FALSE),
                    time = talks$Time,
                    rawEntry = talkEntries,
                    type = "contributed")

          programmeID = programmeID + numTalks
        }
      }
      d = d + 1
    }

    return(progTbl)
  }

  progTbl = buildTbl()

  ## try and match the talks with the talk table

  titleTbl = dbReadTable(db, "titleTbl")

  progTbl = progTbl %>%
    mutate(title = tolower(gsub("(^[^\n]+)\n.*$", "\\1", progTbl$rawEntry))) %>%
    left_join(titleTbl %>% mutate(title = tolower(title)), by = "title")

  #browser()

  dbWriteTable(db, "progTbl", progTbl, overwrite = overwrite)
}
