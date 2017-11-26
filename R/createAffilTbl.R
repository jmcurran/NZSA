createAffilTbl = function(db, overwrite = TRUE){

  all_authors = db %>% dbReadTable("all_authors")
  affilTbl = all_authors %>%
    select(Affiliation)

  affilTbl = affilTbl %>%
    mutate(affil = Affiliation) %>%
    mutate(affil = gsub("(^.*)The University(.*$)", "\\1Unversity\\2", affil)) %>%
    mutate(affil = gsub("^.*(University of Technology Sydney|UTS).*$", "University of Technology Sydney", affil)) %>%
    mutate(affil = gsub("^.*(Auckland University (of )?Technology|AUT).*$", "Auckland University of Technology", affil)) %>%
    mutate(affil = gsub("^.*(Osaka City University|Kwansei Gakuin University|Duke-NUS Medical School|Griffith University|Flinders University|University of Auckland|Institute of Statistical Mathematics|Academia Sinica|Doshisha University|Queensland University of Technology).*$", "\\1", affil)) %>%
    mutate(affil = gsub("^.*(La Trobe University|Nara Institute of Science and Technology|Feng Chia University|University of Pau et Pays de L'Adour|Centro de Investigacionen Matematicas).*$", "\\1", affil)) %>%
    mutate(affil = gsub("^.*(National Changhua University of Education|National Chiao Tung University|National University of Kaohsiung|Kyoto University|Osaka University).*$", "\\1", affil)) %>%
    mutate(affil = gsub("^.*(Pusan National|Xi'an Jiaotong|Xiamen|Beijing( Normal)?|Renmin) University.*$", "\\1 University", affil)) %>%
    mutate(affil = gsub("^.*UNSW.*$", "\\1 University of New South Wales", affil)) %>%
    mutate(affil = gsub("^.*Philippines ?(Diliman|School of Statistics)*$", "University of the Philippines Diliman", affil)) %>%
    mutate(affil = gsub("^.*Uni?versity +[Oo]f +Auckland.*$", "University of Auckland", affil)) %>%
    mutate(affil = gsub("^.*IIM Bangalore.*$", "Indian Institute of Management Bangalore", affil)) %>%
    mutate(affil = gsub("^.Indian Institute of Technology, Patna.*", "Indian Institute of Technology Patna", affil)) %>%
    mutate(affil = gsub("^.*IBADAN.*$", "University of Ibadan", affil)) %>%
    mutate(affil = gsub("^.*Hong Kong University of Science and Technology.*$", "Hong Kong University of Science and Technology", affil)) %>%
    mutate(affil = gsub("^.*Nagoya university graduate school of medicine.*$", "Nagoya University Graduate School of Medicine", affil)) %>%
    mutate(affil = gsub("^.*LSHTM.*$", "London School of Hygiene \\& Tropical Medicine", affil)) %>%
    mutate(affil = gsub("^.*Kakao corporation.*$", "Kakao Corporation", affil)) %>%
    mutate(affil = gsub("^.*National Chung[-]?Hsing University.*$",  "National Chung Hsing University", affil)) %>%
    mutate(affil = gsub("^.*National Tsing[-]?Hua University.*$",  "National Tsing Hua University", affil)) %>%
    mutate(affil = str_trim(affil))

  affilTbl2 = affilTbl %>%
    distinct(affil) %>%
    arrange(affil)

  affilTbl2 = affilTbl2 %>%
    add_column(affilID = 1:nrow(affilTbl2), .before = 1)

  dbWriteTable(db, "affilTbl", affilTbl2, overwrite = TRUE)

  affilTbl = affilTbl %>% left_join(affilTbl2, by = "affil")

  invisible(db)

}