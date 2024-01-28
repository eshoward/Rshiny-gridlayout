library(shiny)
library(gridlayout)
library(ggplot2)
library(gt)
library(dplyr)
library(bslib)
library(baseballr)
library(scales)
library(gridExtra)
library(grid)

my_layout <- new_gridlayout("
                            |    |1fr    |1fr     |1fr   |1fr    |
                            |80px|header  |header  |header  |NULL   |
                            |1fr |velo    |velo    |velo  |table  |
                            |1fr |fb_sz   |sv_sz   |sl_sz |ch_sz  |")

# Define UI for application that draws a histogram
shinyApp(
ui = grid_page(
  layout = my_layout,
  
  #header
  grid_card_text("header", "Kyle Harrison 2023 Strike Zones"),
  
  #plot of the veloFreq for the velo location
  grid_card(
      "velo",
        card_body(
          grid_card_plot("veloFreq")
        )
    ),
  
  #plot of the veloTable for the table location
  grid_card(
      "table",
        card_body(
          gt_output("veloTable")
        )
    ),
  
  #plot of fb sz for the fb_sz location
  grid_card(
    "fb_sz",
    card_body(
      grid_card_plot("fbStrikezone")
    )
  ), 
  
  #plot of sv sz for the sv_sz location
  grid_card(
    "sv_sz",
    card_body(
      grid_card_plot("svStrikezone")
    )
  ),
  
  #plot of sl sz for the sl_sz location
  grid_card(
    "sl_sz",
    card_body(
      grid_card_plot("slStrikezone")
    )
  ),
  
  #plot of ch sz for the ch_sz location
  grid_card(
    "ch_sz", 
    card_body(
      grid_card_plot("chStrikezone")
    )
  ),
  
  flag_mismatches = FALSE
  ),


# Define server logic required to draw a histogram
server = function(input, output) {
    
    #graphing frequency of velo
  output$veloFreq <- renderPlot({
    kharrison_24 %>%
      ggplot(aes(x = release_speed), fill = pitch_type)+
      stat_density(alpha = .75)+
      #scale_fill_brewer(palette = "Dark2")+
      ggtitle("Velocity by Pitch Frequency")+
      xlab("Velocity in MPH") + ylab("Frequency")+
      theme(legend.position = "none",
            panel.spacing = unit(.75, "lines"),
            plot.title = element_text(color = 'black', size = 20, face = "bold", hjust = 0.5),
            axis.title.x = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 14, face = "bold"))+
      facet_grid(pitch_type ~ .)
    })
  
  #creating simple velo table
  output$veloTable <- render_gt({
      kharrison_24 %>%
        group_by("Pitch Type" = pitch_type) %>%
          summarise(
            "Max Velo" = max(release_speed),
            "Avg Velo" = mean(release_speed),
            "Avg Spin Rate" = mean(release_spin_rate)
          )%>%
      gt()%>%
      tab_header(
        title = "Pitches Max/Avg Velo and Avg Spin Rate"
        ) %>%
      fmt_number(
        columns = c("Avg Velo"),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c("Avg Spin Rate"),
        decimals = 0
      )
    },
    height = "100%",
    width = "100%")
  
  #creating fb sz plot
  output$fbStrikezone <- renderPlot({
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
      ggtitle("Fastball Strike Zone Location")+ 
      xlab("Distance in Feet") + ylab("Height in Feet") + 
      labs(subtitle = "7 pitches missing due to X and Y limits")+
      theme(legend.position = "none",
            plot.title = element_text(color = 'black', size = 20, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = 'black', size = 13, hjust = .5),
            axis.title.x = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.spacing = unit(3, "lines"))+
      xlim(-2, 2) + ylim(0, 5)
  })
  
  #creating sv sz plot
  output$svStrikezone <- renderPlot({
    kharrison_24 %>%
      filter(pitch_type == "Slurve") %>%
      ggplot(kharrison_24, mapping = aes(x = plate_x * -1, y = plate_z))+
      geom_point(aes(color = pitch_type), size = 2)+
      #scale_color_brewer(palette = "Dark2")+
      
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
      ggtitle("Slurve Strike Zone Location")+ 
      xlab("Distance in Feet") + ylab("Height in Feet") + 
      labs(subtitle = "3 pitches missing due to X and Y limits")+
      theme(legend.position = "none",
            plot.title = element_text(color = 'black', size = 20, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = 'black', size = 13, hjust = .5),
            axis.title.x = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.spacing = unit(3, "lines"))+
      xlim(-2, 2) + ylim(0, 5)
  })
  
  #creating sl sz plot
  output$slStrikezone <- renderPlot({
    kharrison_24 %>%
      filter(pitch_type == "Slider") %>%
      ggplot(kharrison_24, mapping = aes(x = plate_x * -1, y = plate_z))+
      geom_point(aes(color = "#981643"), size = 2)+
      #scale_color_brewer(palette = "Dark2")+
      
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
      ggtitle("Slider Strike Zone Location")+ 
      xlab("Distance in Feet") + ylab("Height in Feet") + 
      labs(subtitle = "No pitches missing due to X and Y limits")+
      theme(legend.position = "none",
            plot.title = element_text(color = 'black', size = 20, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = 'black', size = 13, hjust = .5),
            axis.title.x = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.title.y = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.spacing = unit(3, "lines"))+
      xlim(-2, 2) + ylim(0, 5)
  })
  
  #creating ch sz plot
  output$chStrikezone <- renderPlot({
      kharrison_24 %>%
        filter(pitch_type == "Changeup") %>%
        ggplot(kharrison_24, mapping = aes(x = plate_x * -1, y = plate_z))+
        geom_point(aes(color = "#5e4fa2"), size = 2)+
        #scale_color_brewer(palette = "Dark2")+
        
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
        ggtitle("Changeup Strike Zone Location")+ 
        xlab("Distance in Feet") + ylab("Height in Feet") + 
        labs(subtitle = "1 pitch missing due to X and Y limits")+
        theme(legend.position = "none",
              plot.title = element_text(color = 'black', size = 20, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(color = 'black', size = 13, hjust = .5),
              axis.title.x = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
              axis.title.y = element_text(color = 'black', size = 16, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 14),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.spacing = unit(3, "lines"))+
        xlim(-2, 2) + ylim(0, 5)
  })
  }
)
