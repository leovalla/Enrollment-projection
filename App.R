#
# This Shiny App presents the enrollment aggregated as the user decides to filter 
# the data. Ones aggregated its project enrollment for future periods. 
#
# Author: Leo Valladares
# Last modification: 2022-05-17


library(shiny);library(shinyjs); library(shinythemes)
library(shinyWidgets);library(ggplot2);library(plotly)
library(readxl);library(dplyr)

#############################################################################
####  Projection functions              #####################################
#############################################################################

# weighted moving average
wma <- function(df){
  counter <- 0
  
  for (i in 1:nrow(df)) { 
    
    if (counter > 2 ){
      df[i,6] <- df[i-3,2]*1/6+ df[i-2,2]*2/6 + df[i-1,2]*3/6
      
    } else {
      df[i,6] <- NA
    }
    counter <- counter + 1
  }
  return(df)
}

# Exponential smoothing no trend

exp_smoot_nt <- function(df){ 
  alpha <- 0.50   # weight assigned to previous observation 
  counter<-0 
  
  for (i in 1:nrow(df)){
    
    if(counter < 2){
      df[i,7] <- df[i,2]
    } 
    else {
      df[i,7] <- alpha*df[i-1,2]+(1-alpha)*df[i-2,7]
    }
    counter <- counter + 1 
  }
  return(df)
}

# Linear regression

lin_reg <- function(df){
  counter <- 0
  
  
  for (i in 1:nrow(df)) { 
    
    if (counter > 2 ){
      term.val <- c(as.numeric(df[i-3,1]),as.numeric(df[i-2,1]),
                    as.numeric(df[i-1,1]))
      enroll.val <- c(as.numeric(df[i-3,2]),as.numeric(df[i-2,2]),
                      as.numeric(df[i-1,2]))
      slr <- lm(enroll.val~term.val,data=df)
      df[i,8] <- as.numeric(slr$coeff[1]+slr$coeff[2]*df[i,1])
    } else {
      df[i,8] <- NA
    }
    counter <- counter + 1
  }
  return(df)
}


shinyApp(
  
  #############################################################################
  ####         UI            ##################################################
  #############################################################################
  
  ui <- fluidPage(
    
    theme = shinytheme("cerulean"),
    
    headerPanel("MSU Enrollment Forecast"),
    h4("Select one or more options from each list"),
    hr(),
    sidebarLayout(             # Sidebar whit multiple selection options ----
             sidebarPanel(
               useShinyjs(),
               
               # Exclude 500 and 600 courses
               checkboxGroupInput("include", 
                                  h4("Course level to be included?"), 
                                  choices = list("000" = 0,"100" = 1,"200" = 2,
                                                 "300"= 3,"400" = 4,"500"= 5,
                                                 "600" = 6), inline = TRUE,
                                  selected = c(0,1,2,3,4,5,6)),
               tags$h6("Warning: Do not uncheck all boxes. It 
                       will crash the program"),
               hr(),
               
               #  Selection options
               selectizeGroupUI(
                 id = "my-filters",
                 inline = FALSE,
                 params = list(
                   CAMPUS = list(inputId = "CAMPUS", title = "Select Campus",
                                 placeholder = 'select'),
                   CORE = list(inputId = "CORE", title = "Core Area",
                               placeholder = 'select'),
                   SUBJ_CODE = list(inputId = "SUBJ_CODE", title = "Subj Code", 
                                    placeholder = 'select'),
                   COURSE_NUMB = list(inputId = "COURSE_NUMB", title = "Course Number", 
                                      placeholder = 'select'),
                   SECT_TYPE = list(inputId = "SECT_TYPE", title = "Section Type", 
                                    placeholder = 'select')
                 )
               ),
               hr(),
           
               # helping text 
               tags$h5("Weighted Moving Average: Uses the past three periods to 
               project enrollment for the current period. It Assigns more weight 
               to the closest period and less to the farthest ones"),
               tags$h5("Exponential Smoothing: Assigns exponentially decreasing 
                       weights over time"),
               tags$ h5("It creates a simple linear regression (SLR) model with 
                        the last three periods and projects enrollment for the 
                        current one"),
               hr(),
               
               tags$ h4("202270 Data gathered from Banner"),
               tags$ h5("Last update: 05/23/22, 11:00 am")
               ),
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        plotlyOutput(outputId = "linechart" ),
        tableOutput("table.out")
      )
    ))
    ,
      
  
  
  #############################################################################
  #########   SERVER    #######################################################
  #############################################################################
  
  server = function(input, output, session) {
    
    # reads enrollment data. This data was compiled in a separated R script
    data <- read_excel("output enroll.xlsx")
    data2 <- data[,1:14]
    
    # reactive module to filter 500 and 600 courses
    data3 <- reactive({

      data2 %>% filter(
        substring(COURSE_NUMB, first=1, last=1) %in% input$include)
      
    })
    
    # reactive selection module. It allows to filter just relevant data 
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = data3,
      vars = c("CAMPUS", "CORE", "COURSE_NUMB", "SUBJ_CODE", "SECT_TYPE")
    )
    
    # Plot code
    output$linechart <- renderPlotly({
      
      # Calculates the sum of the columns used in the projection by term
      term_sum_data <- res_mod() %>% group_by(TERM) %>%
       summarise(ENROLL = sum(ENROLL), PROJECTED_CAP = sum(PROJECTED_CAP),
              MAX_CAP = sum(MAX_CAP), SECTIONS = sum(SECTIONS)) %>% 
        mutate( WMA = 0, exp_smo = 0, linear_reg = 0)
      
      # Calculates projections
      wma_data <- wma(term_sum_data)
      exp_data <- exp_smoot_nt(wma_data)
      complet_data <- lin_reg(exp_data)
      
      # Includes terms that are missing in Data to be able to do the graph 
      if (!(201770 %in% complet_data$TERM)){
        temp <- c(TERM = 201770, ENROLL = NA,  
                  PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                  WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      if (!(201870 %in% complet_data$TERM)){
        temp <- c(TERM = 201870, ENROLL = NA,  
                  PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                  WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      if (!(201970 %in% complet_data$TERM)){
        temp <- c(TERM = 201970, ENROLL = NA,  
                  PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                  WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      if (!(202070 %in% complet_data$TERM)){
        temp <- c(TERM = 202070, ENROLL = NA,  
                  PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                  WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      if (!(202170 %in% complet_data$TERM)){
        temp <- c(TERM = 202170, ENROLL = NA,  
                   PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                   WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      if (!(202270 %in% complet_data$TERM)){
        temp <- c(TERM = 202270, ENROLL = NA,  
                  PROJECTED_CAP = NA,MAX_CAP = NA,SECTIONS = NA, 
                  WMA = NA, exp_smo = NA, linear_reg = NA)
        complet_data[nrow(complet_data) + 1, ] <- as.list(temp)
      }
      
      complet_data$TERM <- as.factor(complet_data$TERM)
      complet_data$ENROLL <- as.integer(complet_data$ENROLL)
      complet_data$PROJECTED_CAP  <- as.integer(complet_data$PROJECTED_CAP )
      complet_data$MAX_CAP <- as.integer(complet_data$MAX_CAP)
      complet_data$SECTIONS <- as.integer(complet_data$SECTIONS)
      
      complet_data <- complet_data %>% arrange(TERM)
      
      #complet_data$TERM <- as.numeric(complet_data$TERM)
      
      # table code
      output$table.out <- renderTable({
        newTable <- complet_data %>% select(-c(WMA, exp_smo,linear_reg)) %>%
          mutate(REMAINING_SEATS = MAX_CAP - ENROLL)
        
        print(newTable)
      })
      
      # Graph code
      graph <- plot_ly(complet_data, x= ~as.factor(TERM), y= ~ENROLL, 
                       name = 'Enrollment', type="scatter", mode="lines", 
                       width=800, height=400) %>% 
        layout( 
          title = list(title="Projection", titlefont = list(size=30)), 
          xaxis = list(title = "Term", showgrid = FALSE, titlefont = list(size=20), 
                       autorange = FALSE),
          yaxis = list(title = "Enrollment", showgrid = FALSE, titlefont = list(size=20), 
                       rangemode = "tozero"),
          plot_bgcolor = '#f0f0f0'
        )

      graph <- graph %>% add_trace(y = ~WMA, name = 'Weighted Moving Average', 
                                   mode = 'lines+markers', linetype = I("dash") )
      graph <- graph %>% add_trace(y = ~exp_smo, name = 'Exponential smoothing',
                                   mode = 'lines+markers', linetype = I("dash") )
      graph <- graph %>% add_trace(y = ~linear_reg, name = 'Linear Regression', 
                                   mode = 'lines+markers', linetype = I("dash") )
      graph <- graph %>% add_trace(y = ~MAX_CAP, name = 'Maximum Capacity', 
                                   mode = 'lines+markers', linetype = I("dot") )
      graph <- graph %>% add_trace(y = ~PROJECTED_CAP, name = 'Projected cap', 
                                   mode = 'lines+markers', linetype = I("dot") )

      graph
      
     })
    
  },

)