writeIndexPages = function(path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/"){
  fileName = paste0(path, "05-Index-by-Author.Rmd")
  f1 = file(fileName, "w")

  db = getDB()

  authorSubTbl = db %>%
    dbReadTable("authorSubTbl") %>%
    arrange(authorID, subID)

  authorTbl = db %>%
    dbReadTable("authorTbl")

  authorSubTbl = authorSubTbl %>%
    left_join(authorTbl, by = "authorID") %>%
    select(authorID, subID, author) %>%
    arrange(author)

  subIds = db %>%
    dbReadTable("titleTbl") %>%
    filter(!grepl("Withdrawn", status)) %>%
    pull(subID)

  writeLines("# Index By First Name of Author {#indexbyauthor .unnumbered}", f1)

  authorIDs = authorSubTbl %>%
    distinct(authorID) %>%
    pull(authorID)

  progTbl = db %>% dbReadTable("progTbl")
  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  for(autID in authorIDs){

    author = authorTbl %>%
      filter(authorID == autID) %>%
      pull(author)

    subs = authorSubTbl %>%
      filter(authorID == autID) %>%
      arrange(subID) %>%
      filter(subID %in% subIds) %>%
      mutate(subIDstr = str_pad(as.character(subID), 3, "left", "0"))

    if(nrow(subs) >= 1){

      days = progTbl %>%
        filter(subID %in% subs$subID) %>%
        pull(day)

      subs = subs %>%
        mutate(hyperLink = sprintf("<a href=\"%s.html#talk_%s\" style =\"color: blue;\">%s</a>",
                                   daysOfWeek[days],
                                   subs %>% pull(subIDstr),
                                   subs %>% pull(subIDstr)))

      entry = sprintf("%s: %s<br />",
                      author,
                      paste0(subs %>% pull(hyperLink), collapse = ", "))

      writeLines(entry, f1)
    }
  }
  close(f1)

  fileName = paste0(path, "06-Index-by-Submission-Number.Rmd")
  f1 = file(fileName, "w")
  writeLines("# Index by Submission Number {#indexbysubmission .unnumbered}", f1)
  writeLines("<table>", f1)
  writeLines("<thead>", f1)
  writeLines("<tr>", f1)
  writeLines("<th style = \"width: 10%;\">Sub.</th>", f1)
  writeLines("<th style = \"width: 90%;\">Title</th>", f1)
  writeLines("</tr>", f1)
  writeLines("</thead>", f1)
  writeLines("<tbody>", f1)

  titleTbl = db %>%
    dbReadTable("titleTbl") %>%
    filter(subID %in% subIds) %>%
    arrange(subID)

  index = titleTbl %>%
    left_join(progTbl, by = "subID") %>%
    select(subID, title.x, day) %>%
    rename(title = title.x) %>%
    mutate(title = tidyTitle(title)) %>%
    mutate(DoW = daysOfWeek[day]) %>%
    mutate(subIDstr = str_pad(as.character(subID), 3, "left", "0"))

  tags = sprintf("<tr><td><a href = \"%s.html#talk_%s\">%s</a></td><td><a href = \"%s.html#talk_%s\">%s</a></td></tr>",
                 index %>% pull(DoW),
                 index %>% pull(subIDstr),
                 index %>% pull(subIDstr),
                 index %>% pull(DoW),
                 index %>% pull(subIDstr),
                 index %>% pull(title)
                 )


  writeLines(tags, f1)
  writeLines("</tbody>", f1)
  writeLines("</table>", f1)
  close(f1)


}
