programme = getProgrammeSheet()
cSheet = programme %>%
  gs_read("Ciprian Allocations") %>%
  distinct(ID, .keep_all = TRUE) %>%
  mutate(Scheduled = case_when(
  Scheduled == "X" ~ "X",
  is.na(Scheduled) ~ ""
  )) %>%
  rename(Class = X12, `Programme Entry` = X14) %>%
  arrange(Class1,Class2,Class3,ID,Scheduled)


# programme = programme %>%
#   gs_ws_new(ws_title = "Allocations", input = cSheet, trim = TRUE, row_extent = 14)

programme = programme %>%
  gs_edit_cells(ws = "Allocations", input = cSheet, anchor = "A1", trim = TRUE)

programme = getProgrammeSheet()
allSheet = programme %>% gs_read("All_Authors")

allSheet %>% filter(grepl("Institute of Statistical Mathematics", Affiliation)) %>% select(Affiliation) %>% View()
allSheet = allSheet %>%
  mutate(affil = Affiliation) %>%
  mutate(affil = gsub("(^.*)The University(.*$)", "\\1Unversity\\2", affil)) %>%
  mutate(affil = gsub("^.*(University of Technology Sydney|UTS).*$", "University of Technology Sydney", affil)) %>%
  mutate(affil = gsub("^.*(Auckland University (of )?Technology|AUT).*$", "Auckland University of Technology", affil)) %>%
  mutate(affil = gsub("^.*(Osaka City University|Kwansei Gakuin University|Duke-NUS Medical School|Griffith University|Fliders University|University of Auckland|Institute of Statitical Mathematics|Academia Sinica|Doshisha University|Queensland University of Technology).*$", "\\1", affil)) %>%
  mutate(affil = gsub("^.*(La Trobe University|Nara Institute of Science and Technology|Feng Chia University|University of Pau et Pays de L'Adour|Centro de Investigacionen Matematicas).*$", "\\1", affil)) %>%
  mutate(affil = gsub("^.*(National Chiao Tung University|National University of Kaohsiung|Kyoto University|Osaka University).*$", "\\1", affil)) %>%
  mutate(affil = gsub("^.*(Pusan National|Xi'an Jiaotong|Xiamen|Beijing( Normal)?|Renmin) University.*$", "\\1 University", affil)) %>%
  mutate(affil = gsub("^.*UNSW.*$", "\\1 University of New South Wales", affil)) %>%
  mutate(affil = gsub("^.*Philipines (Diliman|School of Statistics)$", "University of The Philipines Diliman", affil)) %>%
  mutate(affil = gsub("^.*Uni?versity +[Oo]f +Auckland.*$", "University of Auckland", affil)) %>%
  mutate(affil = gsub("^.IIM Bangalore.*$", "Indian Institute of Management Bangalore", affil)) %>%
  mutate(affil = gsub("^.Indian Institute of Technology, Patna.*", "Indian Institute of Technology Patna", affil))


table(allSheet$affil) %>% View()





                                 case_when(
  grepl("The University", Affiliation) ~ gsub("(^.*)The University(.*$)", "\\1Unversity\\2", Affiliation))
  )
allSheet %>% filter(grepl("The University", Affiliation)) %>% select(Affiliation,affil) %>% View()

