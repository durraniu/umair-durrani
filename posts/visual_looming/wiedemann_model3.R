
# Libraries ---------------------------------------------------------------

library(shiny)
library(bslib)
library(plotly)
# library(carfollowingmodels)
library(tidyverse)
library(arrow)
library(Rcpp)
library(shinyWidgets)
library(shinydashboard)
library(thematic)

options(scipen = 999)

# Functions ---------------------------------------------------------------


sse <- function(y_pred, y_data, type = "mix"){
  
  if (type == "mix"){
    
    sse <- sum(((y_pred - y_data)^2) / abs(y_data), na.rm = TRUE)  / sum(abs(y_data), na.rm = TRUE)
    
  } else if (type == "abs") {
    
    sse <- sum(((y_pred - y_data)^2), na.rm = TRUE) / sum(((y_data)^2), na.rm = TRUE)
    
  } else if (type == "rel") {
    
    sse <- sum((((y_pred - y_data) / y_data)^2), na.rm = TRUE) / length(na.omit(y_data))
    
  } else {
    
    sse <- sum(((y_pred - y_data)^2) / abs(y_data), na.rm = TRUE)  / sum(abs(y_data), na.rm = TRUE)
    
  }
  
  return(sse)
  
}

# Global ------------------------------------------------------------------
## Data
df <- read_parquet("driver_data.parquet")

sourceCpp("wiedemann74_driver_for_loop_vanilla.cpp")
source("simulate_wiedemann74_driver_vanilla.R")










# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = bs_theme(
    bg = "#f0eeeb", 
    fg = "black", 
    primary = "#939bc9", 
    base_font = font_google("Oswald"),
    code_font = font_google("JetBrains Mono")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "height: 600px; overflow-y: auto;",
      
      selectInput("driver", label = h3("Select scenario"), 
                  choices = unique(df$scenario), 
                  selected = "Moving LV"),
      
      tabsetPanel(
        tabPanel("Speed parameters",
                 sliderInput("desv", "Desired Speed:", 
                             min = 25, max = 45, value = 28),
                 sliderInput("maxv", "Max. Speed:", 
                             min = 40, max = 50, value = 44)
        ),
        tabPanel("Acceleration parameters",
                 
                 sliderInput("bmax", "BMAXmult:", 
                             min = 0.01, max = 0.1, value = 0.080),
                 sliderInput("fv", "FAKTORVmult:", 
                             min = 0.0005, max = 0.005, value = 0.001),
                 sliderInput("bnull", "BNULLmult:", 
                             min = 0.1, max = 0.7, value = 0.25),
        ),
        tabPanel("Angular velocity parameter",
                
                 sliderInput("cx", "CX:", 
                             min = 15, max = 200, value = 100)
        )
      )
    ),
    
    mainPanel(
      fluidRow( 
        column(width = 6, infoBoxOutput("vbox1")),
        column(width = 6, infoBoxOutput("vbox4"))
      ),
      fluidRow(
        column(width = 6, infoBoxOutput("vbox2")),
        column(width = 6, infoBoxOutput("vbox3"))
      ),
      br(),
      plotlyOutput("splot")
    )
  )
)





















# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  driver1 <- reactive({
    df %>% 
      filter(scenario == input$driver)
  })
  
  df_pred_wiedemann <- reactive({
    simulate_wiedemann74_driver(
      resolution    = 0.1,
      N             = 1,
      dfn1          = driver1(),
      xn1           ="LV_position_m_new",
      vn1           ="LV_speed_mps",
      bn1           ="LV_acc_mps2",
      xn_first      = list(driver1()$ED_position_m_new[1]),
      vn_first      = list(driver1()$ED_speed_mps[1]),
      ln            = list(unique(na.omit(driver1()$LV_length_m))),
      D_MAX         = 1000,
      V_MAX         = input$maxv,
      V_DESIRED     = input$desv,
      FAKTORVmult   = input$fv,
      BMAXmult      = input$bmax,
      BNULLmult     = input$bnull,
      BMIN          =(-10),
      CX            = input$cx,
      AXadd         = 4, #input$axadd,
      BXadd         = 2, #input$bxadd,
      EXadd         = 1, #input$exadd,
      # EXmult        = input$exmult,
      OPDVadd       = 1 #input$opdvadd#,
      # OPDVmult      = input$opdvmult
    ) %>% 
      filter(cf_state_sim %in% c("free_driving", "approaching", "following", "braking")) %>% 
      mutate(angular_vel_W = (unique(na.omit(driver1()$LV_width_m)) * (vn - vn1))/((sn - ln1)^2))
  })
  
  output$splot <- renderPlotly({
    
    
    
    ggplotly(
      ggplot(data = df_pred_wiedemann()) +
        geom_line(aes(x=Time, y=vn, color = cf_state_sim), linetype = "longdash") +
        geom_line(data = driver1(),
                  aes(x=Time, y=ED_speed_mps, color = cf_state))+
        geom_line(data = driver1(),
                  aes(x=Time, y=LV_speed_mps))+
        
        annotate(geom = "text", x = 40, y = 10, 
                 label = "Dashed line is the predicted speed\nof the following car by the Wiedemann model") +
        annotate(geom = "text", label = "Following car speed\n(observed)", x = 40, y = 32) +
        annotate(geom = "text", label = "Lead car speed\n(observed)", x = 40, y = median(driver1()$LV_speed_mps, 
                                                                                         na.rm=TRUE) +.5) +
        theme_classic() +
        labs(x="Time (s)", y = "Speed (m/s)", color = "State") +
        expand_limits(y = c(0, 35))
    )
    
  })
  
  
  nSSE <- reactive({
    -sse((df_pred_wiedemann()$sn - df_pred_wiedemann()$ln1),
         driver1()$LV_frspacing_m %>% 
           head(-1))
  })
  
  output$vbox1 <- shinydashboard::renderValueBox({ 
    
    
    shinydashboard::infoBox("Sum of Squared Error: ",  
                            round(nSSE(), 4),
                            
                            icon=icon("circle-exclamation"), color = "red")
  })
  
  
  
  
  
  
  
  output$vbox2 <- shinydashboard::renderValueBox({ 
    
    if (length(unique(df_pred_wiedemann()$cf_state_sim))==1) {
      
      vector_model_rt <- "Unable to estimate reaction time"
      
    } else {
      LV_time_at_appearance <- unique(driver1()$LV_time_at_appearance)
      
      vector_model_rt <- df_pred_wiedemann()$Time[head(which(df_pred_wiedemann()$cf_state_sim == "approaching"), 1)] - LV_time_at_appearance
      
      vector_model_rt <- paste(round(vector_model_rt,1), "s")
      
    }
    
    shinydashboard::infoBox("Predicted reaction time: ",  vector_model_rt,  
                            icon=icon("stopwatch"), color = "red")
  })
  
  
  
  output$vbox3 <- shinydashboard::renderValueBox({ 
    
    
    rt <- unique(driver1()$RT)
    
    
    shinydashboard::infoBox("Observed reaction time: ",  paste(round(rt,1), "s"), 
                            icon=icon("clock"), color = "red")
  })
  
  
  
  output$vbox4 <- shinydashboard::renderValueBox({ 
    
    rt <- unique(driver1()$RT)
    avo <-  driver1()$angular_vel_W[head(which(driver1()$cf_state == "approaching"), 1)]     
    
    
    avp <-  df_pred_wiedemann()$angular_vel_W[head(which(df_pred_wiedemann()$cf_state == "approaching"), 1)]
    
    
    shinydashboard::infoBox("AV threshold: ",  paste("Observed:", round(avo,4), "rad/s;\n", "Wiedemann:", round(avp,4), "rad/s"), 
                            icon=icon("eye"), color = "red")
  })
  
  
}



# App ---------------------------------------------------------------------
thematic_shiny(inherit = TRUE)

shinyApp(ui, server)