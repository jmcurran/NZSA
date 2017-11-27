writeTT = function(path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/"){
  db = getDB()
  progTbl = db %>% dbReadTable("progTbl")
  roomTbl = db %>% dbReadTable("roomTbl")

  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  fileName = paste0(path, "00-Programme.Rmd")
  f1 = file(fileName, "w")

  preamble = "---
title: \"None\"
  output:
  html_document:
  css: style.css
  ---"

  writeLines(preamble, f1)

  buildHTMLTable = function(dayProgTbl, d){
    numStreams = 6

    tbl = "<table>"
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

    times = dayProgTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData){
      nRows = nrow(rowData)
      # browser()

      if(nRows == 1){ ## usually housekeeping, keynote, mealbreak, poster, close
        tblRow = "<tr>"

        if(rowData$type %in% c("close","housekeeping", "keynote", "mealbreak", "poster", "open")){
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          rowData = rowData %>%
            mutate(rawEntry = gsub("\n", "<br/>", rawEntry))

          rowStr = sprintf("<td class = \"%s\" colspan = \"%d\">%s</td>",
                           rowData$type,
                           numStreams,
                           rowData$rawEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        }else{
          ## This only happens in one place day 1, 12 pm stream 1 is emptpy
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          rowData = rowData %>%
            mutate(rawEntry = gsub("\n", "<br/>", rawEntry))

          rowStr = sprintf("<td class = \"contributed\"></td>\n<td class = \"%s\">%s</td>",
                           rowData$type,
                           rowData$rawEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
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
        talks = rowData %>%
          filter(type == "contributed") %>%
          arrange(stream) %>%
          mutate(rawEntry = gsub("\n", "<br/>", rawEntry)) %>%
          mutate(rawEntry = ifelse(is.na(rawEntry), "", rawEntry))

        talkStr = sprintf("<td class = \"contributed\">%s</td>", talks$rawEntry)
        tblRow = c(tblRow, talkStr)
        tblRow = c(tblRow, "</tr>")

        #browser()



      #   tblRow = NULL
      #
      #   sessionHeaders = rowData %>%
      #     filter(type == "sessionheader") %>%
      #     arrange(stream)
      #
      #   if(nrow(sessionHeaders) > 0){
      #     tblRow = "<tr>"
      #     tblRow = c(tblRow, "<td class = \"time\"></td>")
      #
      #     sessionStr = sprintf("<td class = \"sessionheader\">%s</td>", sessionHeaders$title)
      #     tblRow = c(tblRow, sessionStr)
      #     tblRow = c(tblRow, "</tr>")
      #   }
      #
      #   if(is.null(tblRow)){
      #     tblRow = "<tr>"
      #   }else{
      #     tblRow = c(tblRow, "<tr>")
      #   }
      #
      #   timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time[1])
      #   tblRow = c(tblRow, timeStr)
      #
      #   talks = rowData %>%
      #     filter(type == "contributed") %>%
      #     arrange(stream) %>%
      #     mutate(rawEntry = gsub("\n", "<br/>", rawEntry))
      #   talkStr = sprintf("<td class = \"contributed\">%s</td>", talks$rawEntry)
      #   tblRow = c(tblRow, talkStr)
      #   tblRow = c(tblRow, "</tr>")
      # }else{
      #   browser()
      }

      return(tblRow)
    }

    for(tm in times$time){
      rowData = dayProgTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")

  }

  writeLines("# Programme At A Glance {-}", f1)

  for(d in 1:4){
    dayProgTbl = progTbl %>%
      filter(day == d) %>%
      arrange(time)

    writeLines(sprintf("## %s {-}", daysOfWeek[d]), f1)

    htmlTbl = buildHTMLTable(dayProgTbl, d)
    writeLines(htmlTbl, f1)
  }

  close(f1)
}
