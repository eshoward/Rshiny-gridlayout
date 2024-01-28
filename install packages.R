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

#replacing pitch names
unique(kharrison_24$pitch_type)
kharrison_24 <- kharrison_24 %>%
  mutate(pitch_type = case_when(
    pitch_type == "FF" ~ "Fastball",
    pitch_type == "SV" ~ "Slurve",
    pitch_type == "SL" ~ "Slider",
    pitch_type == "CH" ~ "Changeup"
  ))

#finding median sz top and bottom
median(kharrison_24$sz_top)
median(kharrison_24$sz_bot)

#creating strike zone and 9 zones. might be a tad verbose
x <- c(-.95, .95, .95, -.95, -.95, .95, .95, -.95, -.95, -.3167, -.3167, .3167, .3167, .95, .95)
z <- c(1.6, 1.6, 2.2, 2.2, 2.8, 2.8, 3.4, 3.4, 1.6, 1.6, 3.4, 3.4, 1.6, 1.6, 2.8)

sz <- data.frame(x, z)

ggplot()+
  geom_path(data = sz , aes(x, z), linewidth = 1)+
  coord_equal()+
  
  #plotting pitches. plate_x is multiplied by -1 so the view is from P perspective
  geom_point(data = kharrison_24, aes(x = plate_x * -1, y=plate_z, color = pitch_type))+
  geom_segment(x = (-.708 -(2/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2.5)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2.5), xend = (.708-(4/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(4/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(2/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0))+
  theme(panel.background = element_blank())
  




