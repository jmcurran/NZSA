writeSessionChairs = function(path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/"){

  if(grepl("windows", Sys.info()[[1]], ignore.case = TRUE)){
    path = gsub("~", "C:/Users/jcur002/", path)
  }

  db = getDB()
  chairTbl = db %>% dbReadTable("chairTbl")
  roomTbl = db %>% dbReadTable("roomTbl")

  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  fileName = paste0(path, "01-Session-Chairs.Rmd")
  f1 = file(fileName, "w")

  buildHTMLTable = function(dayChairsTbl, d){
    numStreams = 6

    tbl = "<div style = \"overflow-x:auto;\">"
    tbl = c(tbl, "<table style = \"width: 1500px;\">")
    tbl = c(tbl, "<thead>")

    tbl = c(tbl, "<tr>")
    tbl = c(tbl, "<th class = \"time\">Time</th>")
    tbl = c(tbl, sprintf("<th class = \"day\" colspan = \"%d\">%s</th>",
                         numStreams,
                         paste0(daysOfWeek[d]," ", 10 + d, "<sup>th</sup>")
                         ))
    tbl = c(tbl, "</tr>")
    tbl = c(tbl, "</thead>")

    tbl = c(tbl, "<tbody>")

    times = dayChairsTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData){
      nRows = nrow(rowData)
      # browser()

      if(nRows == 1){ ## usually housekeeping, keynote, mealbreak, poster, close
        tblRow = "<tr>"

        if(rowData$type == "keynote"){
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)
          rowData = rowData %>%
            mutate(tableEntry = ifelse(is.na(chair), "", chair))


          rowStr = sprintf("<td class = \"%s\" colspan = \"%d\">%s</td>",
                           rowData$type,
                           numStreams,
                           rowData$tableEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        }else{
          browser()
        }
      }else if(nRows >=2){ ##  2-3 talks or 2-3 session headers and 2-3 talks
        #browser()
        tblRow = NULL

        if("sessionheader" %in% rowData$type){

          rooms = rowData %>%
            filter(type == "sessionheader") %>%
            select(roomID) %>%
            left_join(roomTbl, by = "roomID") %>%
            mutate(rname = paste0(rname, "(", rnumber, ")")) %>%
            select(rname) %>%
            unlist(use.names = FALSE)

            tblRow = "<tr>"
            tblRow = c(tblRow, "<td class = \"time\"></td>")
            sessionStr = sprintf("<td class = \"sessionheader\">%s</td>", rooms)
            tblRow = c(tblRow, sessionStr)
            tblRow = c(tblRow, "</tr>")

          rowData = rowData %>%
            filter(type == "contributed")
        }

        tblRow = c(tblRow, "<tr>")
        timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time[1])
        tblRow = c(tblRow, timeStr)

        tidyEntry = function(x){
          splitData = x %>%
            str_split(pattern = "\n") %>%
            sapply(function(y){
              y[1] = tidyTitle(y[1])
              paste0(y, collapse = "<br/>")
              })
        }

        #browser()

        talks = rowData %>%
          filter(type == "contributed") %>%
          arrange(stream) %>%
          mutate(tableEntry = ifelse(is.na(chair), "", chair))

        chairStr = sprintf("<td class = \"contributed\">%s</td>", talks$tableEntry)
        tblRow = c(tblRow, chairStr)
        tblRow = c(tblRow, "</tr>")
      }

      return(tblRow)
    }

    for(tm in times$time){
      rowData = dayChairsTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")
    tbl = c(tbl, "</div>")

    return(tbl)
  }

  writeLines("# Am I A Session Chair? Please Read!!! {-}", f1)

  for(d in 1:4){
    dayChairsTbl = chairTbl %>%
      filter(day == d) %>%
      arrange(time)

    writeLines(sprintf("## %s {#%s-chairtbl .unnumbered}", daysOfWeek[d], daysOfWeek[d]), f1)

    htmlTbl = buildHTMLTable(dayChairsTbl, d)
    writeLines(htmlTbl, f1)
  }

  close(f1)
}
