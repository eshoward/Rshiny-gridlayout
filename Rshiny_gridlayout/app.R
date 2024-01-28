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
  grid_card_text("header", "Kyle Harrison 2024 Strike Zones"),
  grid_card(
    "velo",
    card_body(
      grid_card_plot("veloFreq")
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
  }
)
# Run the application 
#shinyApp(ui = ui, server = server)
