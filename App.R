#
# This Shiny App presents the enrollment aggregated as the user decides to filter 
# the data. Ones aggregated its project enrollment for future periods. 
#
# Author: Leo Valladares
# Last modification: 2022-05-31


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
               
               tags$ h4("202270 Data gathered from warehouse"),
               tags$ h5("Last update: 05/27/22, 1:30 pm")
               ), # sidebar Panel end
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        tabsetPanel(type = "pills",
                    tabPanel('Enrollment projection',
                             list(plotlyOutput(outputId = "linechart" ),
                                  tableOutput("table.out"))),
                    tabPanel("Section by capacity",
                             
                             sliderInput("capacity", label = "Available capacity (%)",
                      min = 1, max = 100, value = c(50,100)),
                      tableOutput("tablecap.out")),
                    tabPanel('New Students Distribution',
                             numericInput('newStudents',
                                          label = "Number of new Students", 
                                          value = 1000),
                             numericInput('oldStudents',
                                          label = "Number of continuing Students", 
                                          value = 1000),
                             div(htmlOutput('totalStu'),style = "font-size:150%"),
                             hr(),
                             tableOutput("tableNewStu.out"))
                    )
        ) # main panel end
      
    ) # Side bar Layout end
    ) # Fluid panel
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
    

    #######################################
    # First tab Enrollment projection code
    ######################################
    
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
      
      # Includes terms that are missing in Data to be able to graph 
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
      
      # Order data by Term
      complet_data <- complet_data %>% arrange(TERM)
      
      # Transform data classes for a cleaner look
      complet_data$TERM <- as.factor(complet_data$TERM)
      complet_data$ENROLL <- as.integer(complet_data$ENROLL)
      complet_data$PROJECTED_CAP  <- as.integer(complet_data$PROJECTED_CAP )
      complet_data$MAX_CAP <- as.integer(complet_data$MAX_CAP)
      complet_data$SECTIONS <- as.integer(complet_data$SECTIONS)
      
      # table Enrollment projection code
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
      
      if(!(is.na(complet_data$exp_smo[4]))){
        graph <- graph %>% add_trace(y = ~exp_smo, name = 'Exponential smoothing',
                                     mode = 'lines', 
                                     line = list(dash = "dash", color = "Orange"))
      }
      if(!(is.na(complet_data$WMA[6]))){
        graph <- graph %>% add_trace(y = ~WMA, name = 'Weighted Moving Average', 
                                     mode = 'lines', 
                                     line = list(dash = "dash", color = "green") )
      }
      if(!(is.na(complet_data$linear_reg[6]))){
        graph <- graph %>% add_trace(y = ~linear_reg, name = 'Linear Regression', 
                                     mode = 'lines', 
                                     line = list(dash = "dash", color = "red"))
      }
      
      graph <- graph %>% add_trace(y = ~MAX_CAP, name = 'Maximum Capacity', 
                                   mode = 'lines', 
                                   line = list(dash = "dot", color = "mediumpurple"))
      graph <- graph %>% add_trace(y = ~PROJECTED_CAP, name = 'Projected cap', 
                                   mode = 'lines', 
                                   line = list(dash = "dot", color = "brown"))

      graph
      
      
     })
    
    
    #######################################
    # Second tab Section by capacity code
    #######################################
    
    # Capacity percentage available
    perc <- reactive(input$capacity) 
    
    # table Enrollment projection code
    output$tablecap.out <- renderTable({
      
      # Calculates the sum of the columns used in the projection by term
      complet_data <- res_mod() %>% filter(TERM == 202270) %>%
        mutate(COURSE = paste(SUBJ_CODE, COURSE_NUMB, sep = "_")) %>%
        select(TERM,COURSE,ENROLL,PROJECTED_CAP,MAX_CAP,SECTIONS)
    
      # Transform data classes for a cleaner look
      complet_data$ENROLL <- as.integer(complet_data$ENROLL)
      complet_data$PROJECTED_CAP  <- as.integer(complet_data$PROJECTED_CAP )
      complet_data$MAX_CAP <- as.integer(complet_data$MAX_CAP)
      complet_data$SECTIONS <- as.integer(complet_data$SECTIONS)
      
      # includes remaining seats and availability columns
      newTable <- complet_data %>% mutate(REMAINING_SEATS = MAX_CAP - ENROLL) %>%
        mutate(Availability = REMAINING_SEATS/MAX_CAP*100)
      
      newTable <- newTable %>% filter(Availability >= input$capacity[1])%>%
        filter(Availability <= input$capacity[2])
      
      print(newTable)
    })
    
    #######################################
    # Third tab New Students Distribution code
    #######################################
    
    output$tableNewStu.out <- renderTable({
      
      # Identifies courses offered in 202270 
      classes2022 <- data2 %>% filter(TERM == 202270) %>% 
        select(subj_CRSE) %>% unique()
      
      # Campus selected for the analysis
      campusList <- res_mod()%>% select(CAMPUS)%>%unique()
      
      # Data filtered for the selected campus 
      headCount<- data2 %>% filter(CAMPUS %in% campusList$CAMPUS) 
      
      #Head count of previous terms for classes offered in 202270
      headCount2 <- headCount %>% filter(TERM != 202270 & 
                                           subj_CRSE %in% classes2022$subj_CRSE )
      
      headCountNew <- sum(headCount2$NEWENR)
      headCountCont<- sum(headCount2$CONTENR)
      
      # Total for each course in the previous terms (classes offered in 202270)
      courseTotal <- headCount2 %>% filter(TERM != 202270) %>%
        select(NEWENR,CONTENR,subj_CRSE) %>% group_by(subj_CRSE) %>%
        summarise(NEW_TOTAL = sum(NEWENR), CONT_TOTAL = sum(CONTENR))
      
      # Introduce the ratio of total student for each class 
      dist_ratio <- courseTotal %>% mutate(new_ratio = NEW_TOTAL/headCountNew, 
                                           cont_ratio = CONT_TOTAL/headCountCont) %>%
        select(subj_CRSE,new_ratio,cont_ratio)
      
      # Replace NaN values
      is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))
      
      dist_ratio[is.nan(dist_ratio)] <- 0
      

      # New students input
      newStu <- input$newStudents
      oldStu <- input$oldStudents
      
      # 2022 data 
      data2022 <- data2 %>% filter (TERM == 202270) %>% 
        select(TERM,PROJECTED_CAP,	MAX_CAP,	ENROLL,	SECTIONS,
               subj_CRSE)
      
      # Includes Distribution ratio in 2022 Data
      data2022.2 <- left_join(data2022,dist_ratio, by = 'subj_CRSE')
      
      # replace NA to 0
      data2022.2[is.na(data2022.2)] <- 0
      
      # calculate number of students per class base on the number of students
      # indicated by the user
      data2022.3 <- data2022.2 %>% mutate(NEW_ENR = new_ratio*newStu,
                                          CONT_ENR = cont_ratio*oldStu)%>%
        mutate(REMAINING_SEATS = MAX_CAP - ENROLL) %>% 
        select(subj_CRSE,NEW_ENR,CONT_ENR,REMAINING_SEATS)
      
      # Data selected by user
      data_input <- res_mod() %>% filter(TERM == 202270) %>%
        select(TERM, PROJECTED_CAP,	MAX_CAP, SUBJ_CODE, SECT_TYPE,
                                         COURSE_NUMB, ENROLL,	SECTIONS, subj_CRSE)
      
      # Includes calculated field to the data selected by the user
      data2022.4 <- left_join(data_input ,data2022.3, by = 'subj_CRSE')
      
      data2022.4 <- data2022.4 %>% 
        mutate(COURSE = paste(SUBJ_CODE,COURSE_NUMB,SECT_TYPE, sep = "_")) %>%
        select(TERM, COURSE, PROJ_CAP = PROJECTED_CAP,	MAX_CAP, ENROLL,	SECTIONS,
               NEW_ENR,CONT_ENR, REM_SEATS = REMAINING_SEATS)
      
      # replace NA to 0
      #data2022.3[is.na(data2022.3)] <- 0
      
      data2022.4$TERM <- as.integer(data2022.4$TERM)
      
      # table Enrollment projection code
      table2 <- data2022.4 %>% group_by(TERM) %>% 
        summarise(`TOTAL_NEW` = sum(NEW_ENR), `TOTAL CONTINUE` = sum(CONT_ENR),
                  REMAINING_SEATS = sum(REM_SEATS))
      
      output$totalStu <- renderText({
        
        str <- paste("<B>Total New Students:</B>",as.integer(table2[2]), sep=" ")
        str2 <- paste("<B>Total Cont. Students:</B>",as.integer(table2[3]), sep=" ")
        str3 <- paste("<B>Remaining Seats:</B>",as.integer(table2[4]), sep=" ")
        
        str4 <- paste(str, str2, str3, sep = "\t")
        print(str4)
      })
      
      print (data2022.4)
      
    })
    
  },

)