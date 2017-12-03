createEntry = function(fileCon,
                       progTbl, affilTbl, authorTbl, authorSubTbl, titleTbl, abstractTbl, roomTbl,
                       row,
                       isTalk = TRUE){
  if(isTalk){
    thisSubID = progTbl$subID[row]

    # if(thisSubID == 197){
    #   browser()
    # }


    day = progTbl$day[row]
    stream = progTbl$stream[row]
    programmeID = progTbl$programmeID[row]
    time = progTbl$time[row]
    room = roomTbl %>% filter(roomID == progTbl$roomID[row])

    days = paste0(c("Monday", "Tuesday", "Wednesday", "Thursday"), " ", 11:14, "<sup>th</sup>")
    daysOfWeek = c("Monday", "Tuesday", "Wednesday", "Thursday")

    fmtTime = function(tm){
      hr = floor(tm / 100)
      mins = tm %% 100

      sprintf("%d:%s",hr, str_pad(as.character(mins), 2, "left", "0"))
    }

    keynotes = progTbl %>% filter(type == "keynote") %>% pull(subID)
    morningKeynotes =progTbl %>% filter(type == "keynote",
                                        time <= 1100) %>% pull(subID)
    keynote = FALSE
    talkTag = sprintf("talk_%s", str_pad(as.character(thisSubID),3,"left","0"))

    if(thisSubID %in% keynotes){
      keynote = TRUE
      writeLines(sprintf("<div id = \"%s\"><p class=\"keynoteBanner\">Keynote: %s %s %s (%s)</p></div>",
                         talkTag, days[day], fmtTime(time), room$rname, room$rnumber), fileCon)
    }else{
      writeLines(sprintf("<div id = \"%s\"><p class=\"contribBanner\">%s %s %s (%s)</p></div>",
                           talkTag, days[day], fmtTime(time), room$rname, room$rnumber), fileCon)
    }

    theTitle = titleTbl %>% filter(subID == thisSubID)


    writeLines(sprintf("## %s {.unnumbered}",
                       tidyTitle(theTitle$title)),
                       fileCon)

    # browser()
    # speaker = authorTbl %>% slice(authorID = progTbl$authorID[row])
    #
    # if(nrow(speaker) == 0){
    #   browser()
    # }
    #
    # speakerAffiliation1 = affilTbl %>% slice(affilID = speaker$affilID1)
    # speakerAffiliation2 = if(!is.na(speaker$affilID2)){
    #   affilTbl %>% slice(affilID = speaker$affilID2)
    # }else{
    #   NA
    # }
    #
    # affiliation = if(is.na(speakerAffiliation2)){
    #   speakerAffiliation1$affiliation
    # }else{
    #   paste(speakerAffiliation1$affiliation, "and", speakerAffiliation2$affiliation)
    # }

    # tocEntry = if(keynote){
    #   if(tex){
    #     sprintf("\\addcontentsline{toc}{subsection}{%s \\textbf{Keynote}: %s, %s \\emph{%s}}",
    #             fmtTime(time),
    #             speaker$author,
    #             affiliation,
    #             theTitle$title)
    #   }
    # }else{
    #   if(tex){
    #     sprintf("\\addcontentsline{toc}{subsection}{%s %s: %s, %s \\emph{%s}}",
    #             fmtTime(time),
    #             rooms[stream],
    #             speaker$author,
    #             affiliation,
    #             theTitle$title)
    #   }
    # }
    #
    # if(tex){
    #   writeLines(tocEntry, fileCon)
    # }

    # browser()

    writeLines("<p style=\"text-align:center\">", fileCon)

    authors = authorSubTbl %>%
      filter(subID == thisSubID) %>%
      select(authorSubID, authorID) %>%
      arrange(authorSubID)

    authorDetails = authorTbl %>% right_join(authors, by = "authorID")

    affil1 = authorDetails %>%
      left_join(affilTbl, by = c("affilID1" = "affilID")) %>%
      select(affilID1, affil.x) %>%
      rename(affilID = affilID1, affil = affil.x)

    affil2 = if(authorDetails %>% summarise(any(!is.na(affilID2))) %>% unlist()){
      authorDetails %>%
      left_join(affilTbl, by = c("affilID2" = "affilID")) %>%
      select(affilID2, affil.x) %>%
      rename(affilID = affilID2, affil = affil.x)
    }else{
      NULL
    }


    affiliationNumbers = if(is.null(affil2)){
      affil1 %>% filter(!is.na(affilID)) %>% distinct(affilID) %>% pull(affilID)
    }else{
      unique(c(affil1 %>% filter(!is.na(affilID)) %>% distinct(affilID) %>% pull(affilID),
               affil2 %>% filter(!is.na(affilID)) %>% distinct(affilID) %>% pull(affilID)))
    }
    affiliations = affilTbl %>%
      filter(affilID %in% affiliationNumbers) %>%
      slice(match(affiliationNumbers, affilID)) ## Hopefully this preserves order
    numAffiliations = length(affiliationNumbers)

    numAuthors = nrow(authorDetails)

    s = if(numAffiliations == 1){
      authorDetails$author
    }else{
      m = cbind(match(affil1$affilID, affiliationNumbers),
                match(affil2$affilID, affiliationNumbers))

      afString = function(x){
        if(!is.na(x[1]) && is.na(x[2])){
          sprintf("%d",x[1])
        }else{
          x = sort(x) ## make sure the affiliations are displayed in a sensible order
          sprintf("%d,%d",x[1],x[2])
        }
      }
      ms = apply(m, 1, afString)

      sprintf("%s^%s^", authorDetails$author, ms)
    }


    authorLine  = if(numAuthors == 1){
      paste0(s, "<br />")
    }else if(numAuthors == 2){
      paste0(paste(s, collapse = " and "), "<br />")
    }else{
      paste0(paste0(s[-numAuthors], collapse = ", "), ", and ", s[numAuthors], "<br />", sep="")
    }

    writeLines(authorLine, fileCon)

    affilLine = if(numAffiliations == 1){
      paste0(affiliations$affil, "<br />")
    }else{
      paste0(sprintf("^%d^%s<br />", 1:numAffiliations, affiliations$affil))
    }
    writeLines(affilLine, fileCon)
    writeLines("</p>", fileCon)

    abstract = abstractTbl %>% filter(subID == thisSubID)

    # if(tex){
    #   abstractLine = gsub("&","\\\\&", abstract$abstract)
    #   abstractLine = gsub("%","\\\\%", abstractLine)
    #   abstractLine = gsub("~","\\\\~", abstractLine)
    #   writeLines(abstractLine, fileCon)
    # }else{
    writeLines(abstract$abstract, fileCon)

    writeLines("<p style = \"text-align: right\">", fileCon)
    writeLines(sprintf("<a href = \"programme-at-a-glance.html#%s\">Return to Programme</a><br/><br/></p>\n\n",
                       sprintf("%s-tbl", daysOfWeek[day])), fileCon)
    writeLines("<p class=\"pagebreak\"></p>", con = fileCon)

  }else{ #it's a poster
    thisSubID = progTbl$subID[row]

    #if(thisSubID == 90) browser()

    theTitle = titleTbl %>% filter(subID == thisSubID)

    if(grepl("Alzheimer'S", theTitle$title)){
      theTitle$title = gsub("'S", "'s", theTitle$title)
    }

    writeLines(sprintf("## %s {-}", tidyTitle(theTitle$title)), fileCon)

    speaker = authorTbl %>% slice(authorID = progTbl$authorID[row])
    speakerAffiliation1 = affilTbl %>% slice(affilID = speaker$affilID1)
    speakerAffiliation2 = if(!is.na(speaker$affilID2)){
      affilTbl %>% slice(affilID = speaker$affilID2)
    }else{
      NA
    }

    affiliation = if(is.na(speakerAffiliation2)){
      speakerAffiliation1$affiliation
    }else{
      paste(speakerAffiliation1$affiliation, "and", speakerAffiliation2$affiliation)
    }

    writeLines("<p style=\"text-align:center\">", fileCon)

    authors = authorSubTbl %>%
      filter(subID == thisSubID) %>%
      select(otherID, authorID) %>%
      arrange(otherID)

    authorDetails = authorTbl %>% right_join(authors, by = "authorID")

    affil1 = authorDetails %>%
      left_join(affilTbl, by = c("affilID1" = "affilID")) %>%
      select(affilID1, affiliation) %>%
      rename(affilID = affilID1)

    affil2 = authorDetails %>%
      left_join(affilTbl, by = c("affilID2" = "affilID")) %>%
      select(affilID2, affiliation) %>%
      rename(affilID = affilID2)


    affiliationNumbers = unique(c(affil1$affilID[!is.na(affil1$affilID)],
                                  affil2$affilID[!is.na(affil2$affilID)]))
    affiliations = affilTbl %>% slice(affilID = affiliationNumbers)
    numAffiliations = length(affiliationNumbers)

    numAuthors = nrow(authorDetails)

    s = if(numAffiliations == 1){
      authorDetails$author
    }else{
      m = cbind(match(affil1$affilID, affiliationNumbers),
                match(affil2$affilID, affiliationNumbers))
      afString = function(x){
        if(!is.na(x[1]) && is.na(x[2])){
          sprintf("%d",x[1])
        }else{
          sprintf("%d,%d",x[1],x[2])
        }
      }
      ms = apply(m, 1, afString)

      sprintf("%s^%s^", authorDetails$author, ms)
    }

    authorLine  = if(numAuthors == 1){
      paste0(s, "<br />")
    }else if(numAuthors == 2){
      paste0(paste(s, collapse = " and "), "<br />")
    }else{
      paste0(paste0(s[-numAuthors], collapse = ", "), ", and ", s[numAuthors], "<br />", sep="")
    }

    writeLines(authorLine, fileCon)

    affilLine = if(numAffiliations == 1){
      paste0(sprintf("%s%s", affiliations$affiliation, "<br />"))
    }else{
        paste0(sprintf("^%d^%s%s", 1:numAffiliations, affiliations$affiliation, "<br />"))
    }
    writeLines(affilLine, fileCon)
    writeLines("</p>", fileCon)

    abstract = abstractTbl %>% filter(subID == thisSubID)
    writeLines(abstract$abstract, con = fileCon)
    writeLines("<p class=\"pagebreak\"></p>", con = fileCon)
  }
}
