writeProg = function(path = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/"){
  db = getDB()
  progTbl = db %>% dbReadTable("progTbl") %>% arrange(day, time)
  affilTbl = db %>% dbReadTable("affilTbl")

  authorTbl = db %>% dbReadTable("authorTbl")
  authorSubTbl = db %>% dbReadTable("authorSubTbl")
  titleTbl = db %>% dbReadTable("titleTbl")

  abstractTbl = db %>% dbReadTable("abstractTbl")
  roomTbl = db %>% dbReadTable("roomTbl")


  daysOfWeek = paste0(c("Mon", "Tues", "Wednes", "Thurs"), "day")

  for(d in 1:4){
    dayProgTbl = progTbl %>% filter(day == d)

    fileName = paste0(path, str_pad(as.character(d), 2, "left", "0"), "-", daysOfWeek[d], ".Rmd")
    f1 = file(fileName, "w")

    writeLines(paste0("# Programme And Abstracts For ",
                      daysOfWeek[d]," ",
                      (11:14)[d], "^th^ Of December {#",daysOfWeek[d], " .unnumbered}"),
               f1)

    for(row in 1:nrow(dayProgTbl)){
      if(!is.na(dayProgTbl$subID[row])){
        createEntry(f1, dayProgTbl, affilTbl, authorTbl, authorSubTbl, titleTbl,
                  abstractTbl, roomTbl,  row)
      }
    }
    close(f1)
  }


  # fileName = paste0(path, "05-Posters.Rmd")
  # f1 = file(fileName, "w")
  # writeLines("# Poster Abstracts {-}", f1)
  #
  #
  # posterTbl = db %>% dbReadTable("posterTbl")
  #
  # for(row in 1:nrow(posterTbl)){
  #   createEntry(f1, posterTbl, affilTbl, authorTbl, otherTbl, titleTbl,
  #               abstractTbl, row, isTalk = FALSE)
  # }
  #
  # close(f1)
}
