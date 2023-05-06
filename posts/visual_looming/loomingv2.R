library(shiny)
library(tidyverse) 
# library(readr)
library(patchwork)
library(arrow)
library(ggrepel)
library(shinyWidgets)
library(shinydashboard)
library(grid)
library(png)


source("geom_car.R")


df <- read_parquet("driver_data.parquet")







ui <- fluidPage(
  useShinydashboard(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tags$style(".recalculating { opacity: inherit !important; }"),
  
  fluidRow(
    column(width=4,
  selectInput("driver", label = h3("Select scenario"), 
              choices = unique(df$scenario), 
              selected = "Moving LV"),
   
  uiOutput("timez"),
  infoBoxOutput("vbox1")
  
  ),
  column(width=2,
         plotOutput("plotr")
  ) ,
  
  column(width=6,
         plotOutput("plota")
  ) 
  
  
),


  plotOutput("plotf", height = "100"),
  
)

server <- function(input, output, session) {
  
  df_selected <- reactive({df %>% 
      filter(scenario==input$driver) %>% 
      drop_na(LV_speed_mps)})
  
  output$timez <- renderUI({
    tagList(
      sliderInput("timezz", "Time:", min = min(df_selected()$Time), max = max(df_selected()$Time), 
                  step = 2, value =min(df_selected()$Time), animate = list(interval = 1000))
      
    )
  })
  
  
  df_filt <- reactive({df_selected() %>% filter(Time == input$timezz)})
  df_filt2 <- reactive({df_selected() %>% filter(Time <= input$timezz)})
  
  
  output$plotf <- renderPlot({
    
    p1 <- ggplot(df_filt() ) +
      geom_car(aes(x=ED_position_m_new, y=300, 
                   length=ED_length_m, width=ED_width_m, 
                   fill="ed")) +
      geom_text(aes(x=ED_position_m_new, y=300+10), 
                label = "Following Car") +
      
      geom_car(aes(x=LV_position_m_new, y=300, 
                   length=LV_length_m, width=LV_width_m, 
                   fill="lv")) +
      geom_text(aes(x=LV_position_m_new, y=300+10), 
                label = "Lead Car") +
      geom_hline(yintercept = 300 + 1.83, linetype = "longdash") +
      geom_hline(yintercept = 300 - 1.83) +
      geom_hline(yintercept = 300 + 1.83 + 3.66) +
      labs(title = "Top View") +
      theme_void() +
      coord_equal(ratio = 2.5,
                  xlim = c(median(df_selected()$ED_position_m_new),
                           max(df_selected()$LV_position_m_new))
                  ) +
      # expand_limits(y = c(298, 306),
      #               x = c(0, 343)) +
      # expand_limits(y = c(298, 306),
      #               x = c(df_filt()$ED_position_m_new-5, df_filt()$LV_position_m_new+1)) +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
    
    
    
    
    

    p1
  }
  
    )
  
  
  
  output$plotr <- renderPlot({
    
    p2 <- ggplot(df_filt()) +
      geom_car_rear(aes(x=0, y=0, length=visual_angle_W*5,
                        width=visual_angle_H*5), fill="skyblue") +
      labs(title = "View from the Following Car") +
      theme_void() +
      coord_fixed(ratio = 0.7) +
      expand_limits(x = c(0.0016, 0.05),
                    y = c(0.0016, 0.05)) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
    
  
    
    
     p2 
    
    
  })
  
  
  
  
  
  
  output$plota <- renderPlot({
    
    speed_plot <- ggplot(data = df_filt2()) +
      geom_line(aes(x = Time, y = ED_speed_mps)) +
      geom_line(aes(x = Time, y = LV_speed_mps), linetype = "dashed") +
      geom_text_repel(data = df_filt(), aes(x = Time-8, y = ED_speed_mps,
                                      label = paste("Following car speed =", round(ED_speed_mps, 2), "m/s")), color = "black",  size = 5) +
      geom_text_repel(data = df_filt(), aes(x = Time-8, y = LV_speed_mps,
                                            label = paste("Lead car speed =", round(LV_speed_mps, 2), "m/s")), color = "black",  size = 5) +
      theme_classic() +
      labs(y = NULL) +
      theme_classic() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
    
    
    
   pedals_pos_b <- ggplot(data = df_filt2(),
                           mapping = aes(x = Time)) +
      geom_line(aes(y = angular_vel_W*1000), color = "black") +
      geom_text(data = df_filt(), aes(x = Time-8, y = angular_vel_W*1000,
                                      label = paste("Angular vel. =", round(angular_vel_W, 5), "rad/s")), color = "black",
                size = 5) +
      geom_area(aes(y = angular_vel_W*1000), fill = "gray",
                position = "identity", alpha=0.6)+
      
      geom_line(aes(y = ED_gas_pedal_pos), color = "darkgreen") +
      geom_text(data = df_filt(),aes(y = ED_gas_pedal_pos, x = Time-8,
                label = paste("Gas pedal pos. =", round(ED_gas_pedal_pos, 2))), color = "darkgreen", size = 5) +
     labs(y = NULL) +
      theme_classic() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
    
    speed_plot / pedals_pos_b
    
    
  })
  
  output$vbox1 <- shinydashboard::renderValueBox({ 
    
    shinydashboard::infoBox("State",  df_filt()$cf_state, icon=icon("car"), color = "red")
  })
  
  
}

shinyApp(ui, server)