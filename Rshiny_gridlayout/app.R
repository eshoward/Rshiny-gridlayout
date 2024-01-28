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
                            |    |.5fr    |1fr     |1fr   |1fr    |
                            |80px|header  |header  |header  |NULL   |
                            |1fr |velo    |velo    |velo  |table  |
                            |1fr |fb_sz   |sv_sz   |sl_sz |ch_sz  |")

# Define UI for application that draws a histogram
shinyApp(
ui = grid_page(
  layout = my_layout,
  
  #header
  grid_card_text("header", "Kyle Harrison 2024 Strike Zones"),
  
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
  
  flag_mismatches = FALSE
  ),


# Define server logic required to draw a histogram
server = function(input, output) {
    
    #graphing frequency of velo
  output$veloFreq <- renderPlot({
    kharrison_24 %>%
      ggplot(aes(x = release_speed), fill = pitch_type)+
      stat_density(alpha = .75)+
      scale_fill_brewer(palette = "Dark2")+
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
  }
)
