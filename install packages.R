install.packages("baseballr")
install.packages("devtools")
devtools::install_github("rstudio/gridlayout", force = TRUE)


kharrison <- playerid_lookup("Harrison") %>%
  filter(first_name == "Kyle") %>%
  select(mlbam_id, first_name, last_name)

kharrison_24 <- statcast_search("2023-03-30", "2023-11-1", 
                                playerid = kharrison[1,1], player_type = "pitcher")

#getting rid of deprecated fields and description field
kharrison_24 <- kharrison_24[-c(11:14, 16, 40:41)]

