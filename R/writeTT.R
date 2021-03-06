writeTT = function(path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/",
                   printVersion = FALSE) {
  if (grepl("windows", Sys.info()[[1]], ignore.case = TRUE)) {
    path = gsub("~", "C:/Users/jcur002/", path)
  }

  db = getDB()
  progTbl = db %>% dbReadTable("progTbl")
  roomTbl = db %>% dbReadTable("roomTbl")

  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  fileName = paste0(path, "00-Programme.Rmd")
  f1 = file(fileName, "w")

  preamble = "---
  output:
    bookdown::gitbook:
      includes:
        in_header: tblsawinclude.html
  ---"

  writeLines(preamble, f1)

  buildHTMLTable = function(dayProgTbl, d) {
    numStreams = 6

    tbl = "<div style = \"overflow-x:auto;\">"
    tbl = c(tbl, "<table class=\"tablesaw\" data-tablesaw-mode=\"swipe\" data-tablesaw-minimap>")

    tbl = c(tbl, "<thead>")

    tbl = c(tbl, "<tr>")
    tbl = c(tbl, "<th scope=\"col\" data-tablesaw-priority=\"persist\" class=\"time\">Time</th>")
    tbl = c(tbl, sprintf(
      "<th scope=\"col\" data-tablesaw-priority=\"persist\" class=\"day\">%s</th>",
      paste0(daysOfWeek[d], " ", 10 + d, "<sup>th</sup>")
    ))

    for(i in 1:5){
      tbl = c(tbl, "<th scope=\"col\" class=\"day\"></th>")
    }

    tbl = c(tbl, "</tr>")
    tbl = c(tbl, "</thead>")

    tbl = c(tbl, "<tbody>")

    times = dayProgTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData) {
      nRows = nrow(rowData)
      # browser()

      if (nRows == 1) {
        ## usually housekeeping, keynote, mealbreak, poster, close
        tblRow = "<tr>"

        if (rowData$type %in% c("confclose",
                                "housekeeping",
                                "keynote",
                                "mealbreak",
                                "poster",
                                "open")) {
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          if (rowData$type == "keynote") {
            splitData = unlist(rowData$rawEntry %>% str_split(pattern = "\n"))
            splitData[1] = tidyTitle(splitData[1])
            rowData = rowData %>%
              mutate(rawEntry = paste0(splitData, collapse = "<br/>")) %>%
              mutate(hyperlinkTag = sprintf("#talk_%s",
                                            str_pad(
                                              as.character(subID), 3, "left", "0"
                                            ))) %>%
              mutate(
                tableEntry = sprintf(
                  "<a href=\"%s%s\" style = \"color: white;\">%s</a>",
                  paste0(daysOfWeek[d], ".html"),
                  hyperlinkTag,
                  rawEntry
                )
              )
          } else{
            rowData = rowData %>%
              mutate(tableEntry = gsub("\n", "<br/>", rawEntry))
          }

          rowStr = sprintf(
            "<td class = \"%s\" colspan = \"%d\">%s</td>",
            rowData$type,
            numStreams,
            rowData$tableEntry
          )
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        } else{
          browser()
          ## This only happens in one place day 1, 12 pm stream 1 is emptpy
          # timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          # tblRow = c(tblRow, timeStr)
          #
          # splitData = unlist(rowData$rawEntry %>% str_split(pattern = "\n"))
          # splitData[1] = tidyTitle(splitData[1])
          # rowData = rowData %>%
          #   mutate(rawEntry = paste0(splitData, collapse = "<br/>"))
          #
          # rowStr = sprintf("<td class = \"contributed\"></td>\n<td class = \"%s\">%s</td>",
          #                  rowData$type,
          #                  rowData$rawEntry)
          # tblRow = c(tblRow, rowStr)
          # tblRow = c(tblRow, "</tr>")
        }
      } else if (nRows >= 2) {
        ##  2-3 talks or 2-3 session headers and 2-3 talks
        #browser()
        tblRow = NULL

        if ("sessionheader" %in% rowData$type) {
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

        tidyEntry = function(x) {
          splitData = x %>%
            str_split(pattern = "\n") %>%
            sapply(function(y) {
              y[1] = tidyTitle(y[1])
              paste0(y, collapse = "<br/>")
            })
        }

        #browser()

        talks = rowData %>%
          filter(type == "contributed") %>%
          arrange(stream) %>%
          mutate(rawEntry = tidyEntry(rawEntry)) %>%
          mutate(rawEntry = ifelse(is.na(rawEntry), "", rawEntry)) %>%
          mutate(hyperlinkTag = sprintf("#talk_%s",
                                        str_pad(
                                          as.character(subID), 3, "left", "0"
                                        ))) %>%
          mutate(
            tableEntry = sprintf(
              "<a href=\"%s%s\" style = \"color: black;\">%s</a>",
              paste0(daysOfWeek[d], ".html"),
              hyperlinkTag,
              rawEntry
            )
          )


        talkStr = sprintf("<td class = \"contributed\">%s</td>",
                          talks$tableEntry)
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

    for (tm in times$time) {
      rowData = dayProgTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")
    tbl = c(tbl, "</div>")

    return(tbl)
  }

  buildOverviewTable = function(dayProgTbl, d) {
    tbl = "<div style = \"overflow-x:auto;\">"
    tbl = c(tbl, "<table style = \"width: 80%;\">")
    tbl = c(tbl, "<thead>")

    tbl = c(tbl, "<tr>")
    tbl = c(tbl, "<th class = \"time\">Time</th>")
    tbl = c(tbl, sprintf(
      "<th class = \"day\">%s</th>",
      paste0(daysOfWeek[d], " ", 10 + d, "<sup>th</sup>")
    ))
    tbl = c(tbl, "</tr>")
    tbl = c(tbl, "</thead>")

    tbl = c(tbl, "<tbody>")

    times = dayProgTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData) {
      nRows = nrow(rowData)
      # browser()

      if (nRows == 1) {
        ## usually housekeeping, keynote, mealbreak, poster, close
        tblRow = "<tr>"

        if (rowData$type %in% c("confclose",
                                "housekeeping",
                                "keynote",
                                "mealbreak",
                                "poster",
                                "open")) {
          timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time)
          tblRow = c(tblRow, timeStr)

          if (rowData$type == "keynote") {
            splitData = unlist(rowData$rawEntry %>% str_split(pattern = "\n"))
            splitData[1] = tidyTitle(splitData[1])
            rowData = rowData %>%
              mutate(rawEntry = paste0(splitData, collapse = "<br/>")) %>%
              mutate(hyperlinkTag = sprintf("#talk_%s",
                                            str_pad(
                                              as.character(subID), 3, "left", "0"
                                            ))) %>%
              mutate(
                tableEntry = sprintf(
                  "<a href=\"%s%s\" style = \"color: white;\">%s</a>",
                  paste0(daysOfWeek[d], ".html"),
                  hyperlinkTag,
                  rawEntry
                )
              )
          } else{
            rowData = rowData %>%
              mutate(tableEntry = gsub("\n", "<br/>", rawEntry))
          }

          rowStr = sprintf("<td class = \"%s\">%s</td>",
                           rowData$type,
                           rowData$tableEntry)
          tblRow = c(tblRow, rowStr)
          tblRow = c(tblRow, "</tr>")
        } else{
          browser()
        }
      } else if (nRows >= 2) {
        ##  2-3 talks or 2-3 session headers and 2-3 talks
        #browser()
        tblRow = NULL

        if ("sessionheader" %in% rowData$type) {
          tblRow = "<tr>"
          tblRow = c(tblRow, "<td class = \"time\"></td>")
          sessionStr = "<td class = \"sessionheader\">Contributed And Invited Talks</td>"
          tblRow = c(tblRow, sessionStr)
          tblRow = c(tblRow, "</tr>")

          rowData = rowData %>%
            filter(type == "contributed")
        }

        tblRow = c(tblRow, "<tr>")
        timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time[1])
        tblRow = c(tblRow, timeStr)

        talkStr = "<td class = \"contributed\"></td>"
        tblRow = c(tblRow, talkStr)
        tblRow = c(tblRow, "</tr>")
      }

      return(tblRow)
    }

    for (tm in times$time) {
      rowData = dayProgTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")
    tbl = c(tbl, "</div>")

    return(tbl)
  }

  buildRoomTable = function(dayRoomTbl, d) {
    tbl = "<div style = \"overflow-x:auto;\">"
    tbl = c(tbl, "<table style = \"width: 80%;\">")
    tbl = c(tbl, "<thead>")

    tbl = c(tbl, "<tr>")
    tbl = c(tbl, "<th class = \"time\">Time</th>")
    tbl = c(tbl, sprintf(
      "<th class = \"day\">%s</th>",
      paste0(daysOfWeek[d], " ", 10 + d, "<sup>th</sup>")
    ))
    tbl = c(tbl, "</tr>")
    tbl = c(tbl, "</thead>")

    tbl = c(tbl, "<tbody>")

    times = dayRoomTbl %>%
      distinct(time) %>%
      select(time) %>%
      arrange(time)

    createHTMlrow = function(rowData) {
      nRows = nrow(rowData)

      tblRow =  "<tr>"
      timeStr = sprintf("<td class = \"time\">%d</td>", rowData$time[1])
      tblRow = c(tblRow, timeStr)

      tidyEntry = function(x) {
        splitData = x %>%
          str_split(pattern = "\n") %>%
          sapply(function(y) {
            y[1] = tidyTitle(y[1])
            paste0(y, collapse = "<br/>")
          })
      }

      #browser()

      talks = rowData %>%
        mutate(rawEntry = tidyEntry(rawEntry)) %>%
        mutate(rawEntry = ifelse(is.na(rawEntry), "", rawEntry)) %>%
        mutate(hyperlinkTag = sprintf("#talk_%s",
                                      str_pad(as.character(subID), 3, "left", "0"))) %>%
        mutate(
          tableEntry = sprintf(
            "<a href=\"%s%s\" style = \"color: black;\">%s</a>",
            paste0(daysOfWeek[d], ".html"),
            hyperlinkTag,
            rawEntry
          )
        )


      talkStr = sprintf("<td class = \"contributed\">%s</td>", talks$tableEntry)
      tblRow = c(tblRow, talkStr)
      tblRow = c(tblRow, "</tr>")

      return(tblRow)
    }

    for (tm in times$time) {
      rowData = dayRoomTbl %>% filter(time == tm)
      tbl = c(tbl, createHTMlrow(rowData))
    }

    # browser()

    tbl = c(tbl, "</tbody>")
    tbl = c(tbl, "</table>")
    tbl = c(tbl, "</div>")

    return(tbl)
  }


  writeLines("# Programme At A Glance {-}", f1)

  for (d in 1:4) {
    dayProgTbl = progTbl %>%
      filter(day == d) %>%
      arrange(time)

    writeLines(sprintf("## %s {#%s-tbl .unnumbered}", daysOfWeek[d], daysOfWeek[d]),
               f1)

    if(printVersion){
      writeLines(sprintf("### Daily Overview {#%s_overview .unnumbered}", daysOfWeek[d]), f1)
      htmlTbl = buildOverviewTable(dayProgTbl, d) #buildHTMLTable(dayProgTbl, d)
      writeLines(htmlTbl, f1)

      writeLines("### Contributed And Invited Talks By Room {.unnumbered}", f1)


      for(rmID in 1:7){
        dayRoomTbl = dayProgTbl %>%
          filter(roomID == rmID, type == "contributed")

        #browser()

        if(nrow(dayRoomTbl) > 0){
          room = roomTbl %>%
            filter(roomID == rmID) %>%
            mutate(room = paste0(rname, " (", rnumber, ")")) %>%
            pull(room)

            writeLines(sprintf("#### %s {#%s_%d .unnumbered}", room, daysOfWeek[d], rmID), f1)
            htmlTbl = buildRoomTable(dayRoomTbl, d)
            writeLines(htmlTbl, f1)
        }
      }
    }else{
      htmlTbl = buildHTMLTable(dayProgTbl, d)
      writeLines(htmlTbl, f1)
    }
  }

  close(f1)
}
