install.packages("baseballr")
install.packages("devtools")
install.packages("scales")
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

#creating strike zone and 9 zones. might be a tad verbose. NOT USING IN FINAL SHINY
x <- c(-.95, .95, .95, -.95, -.95, .95, .95, -.95, -.95, -.3167, -.3167, .3167, .3167, .95, .95)
z <- c(1.6, 1.6, 2.2, 2.2, 2.8, 2.8, 3.4, 3.4, 1.6, 1.6, 3.4, 3.4, 1.6, 1.6, 2.8)

sz <- data.frame(x, z)

#creating strike zone plot using sz dataframe. NOT USING IN FINAL SHINY
ggplot()+
  geom_path(data = sz , aes(x, z), linewidth = 1)+
  coord_equal()+
  
  #plotting pitches. plate_x is multiplied by -1 so the view is from P perspective. NOT USING IN FINAL SHINY
  geom_point(data = kharrison_24, aes(x = plate_x * -1, y=plate_z, color = pitch_type))+
  theme(panel.background = element_blank())
  


#plotting Harrison's FB sz. xlim(-2, 2), ylim(0, 5), lost 7 pitches with limits.
kharrison_24 %>%
  filter(pitch_type == "Fastball") %>%
  ggplot(kharrison_24, mapping = aes(x = plate_x * -1, y = plate_z))+
  geom_point(aes(color = pitch_type), size = 2)+
  scale_color_brewer(palette = "Dark2")+
  
  #creating the outer limits of strike zone
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12))+
  
  #creating the inner quadrants of the strike zone
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), linewidth = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), linewidth = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), linewidth = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), linewidth = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), linewidth = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), linewidth = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), linewidth = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), linewidth = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), linewidth = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), linewidth = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), linewidth = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), linewidth = .3)+
  
  #creating home plate
  geom_segment(x = (-.708 -(2/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2.5)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2.5), xend = (.708-(4/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(4/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(2/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0))+
  
  #adjusting details
  ggtitle("Pitch Strike Zone Location")+
  theme(legend.position = "none",
        plot.title = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color = 'black', size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(color = 'black', size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(3, "lines"))+
  xlim(-2, 2) + ylim(0, 5)


#velo freq graph.
  kharrison_24 %>%
    ggplot(aes(x = release_speed), fill = pitch_type)+
    stat_density(alpha = .75)+
    scale_fill_brewer(palette = "Dark2")+
    ggtitle("Velocity by Pitch Frequency")+
    xlab("Velocity in MPH") + ylab("Frequency")+
    theme(legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          plot.title = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
          axis.title.x = element_text(color = 'black', size = 14, face = "bold", hjust = 0.5),
          axis.title.y = element_text(color = 'black', size = 14, face = "bold", hjust = 0.5),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 9, face = "bold"))+
    facet_grid(pitch_type ~ .)

















