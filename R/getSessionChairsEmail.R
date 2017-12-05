getSessionChairsEmail = function(){
  db = getDB()
  chairTbl = db %>% dbReadTable("chairTbl") %>%
    filter(type != "sessionheader", !is.na(chair))
  chairEmailTbl = db %>%
    dbReadTable("chairEmailTbl")  %>%
    filter(!is.na(chair))

  missing = chairEmailTbl %>%
    filter(is.na(Email)) %>%
    distinct(chair) %>%
    select(chair)

  missing = missing %>%
    mutate(Email = c("hmwu@gm.ntpu.edu.tw",
                     "d.scott",
                     "r.turner",
                     "m.fitch",
                     "aj.lee",
                     "cm.triggs",
                     "a.balemi",
                     "i.zeng",
                     "marco.reale@canterbury.ac.nz",
                     "dkim@skku.edu")) %>%
    mutate(Email = gsub("(^[^@]+$)", "\\1@auckland.ac.nz", Email))

  nMissing = nrow(missing)
  for(i in 1:nMissing){
    pattern = paste0("^", missing$chair[i], "$")
    email = missing$Email[i]

    chairEmailTbl = chairEmailTbl %>%
      mutate(Email = case_when(
        !grepl(pattern, chair) ~ Email,
        TRUE ~ email
      ))
  }

  chairEmailTbl %>% filter(is.na(Email))

  chairEmailTbl %>% pull(Email) %>% paste0(collapse = ";")
}
