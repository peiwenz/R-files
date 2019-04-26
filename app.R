library(shiny)
library(lpSolve)

ui <- fluidPage(
  
  # Application title
  titlePanel("Exam Capacity Optimization"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  
  sidebarPanel(width = 6,
               "High Risk Firm",
               fluidRow(
                 column(6,
                        numericInput("stf1", "Avg. Staff:",9)),
                 column(6,
                        numericInput("day1", "Avg. days:",261))
                 
               ),
               "Med-High Risk Firm",
               fluidRow(
                 column(6,
                        numericInput("stf2", "Avg. Staff:",6)), 
                 column(6,
                        numericInput("day2", "Avg. days:",186))
                 
               ),
               "Med-Low Risk Firm",
               fluidRow(
                 column(6,
                        numericInput("stf3", "Avg. Staff:",4)),
                 column(6,
                        numericInput("day3", "Avg. days:",143))),
               "Low Risk Firm",
               fluidRow(
                 column(6,
                        numericInput("stf4", "Avg. Staff:",3)),
                 column(6,
                        numericInput("day4", "Avg. days:",109)))
  ),
  
  sidebarPanel(width = 2,
               "High Risk Firm",
               fluidRow(
                 column(12,
                        numericInput("max1", "Total Firms in the group:",27))
                 
               ),
               "Med-High Risk Firm",
               fluidRow(
                 column(12,
                        numericInput("max2", "Total Firms in the group:",334))
                 
               ),
               "Med-Low Risk Firm",
               fluidRow(
                 column(12,
                        numericInput("max3", "Total Firms in the group:",1289))
               ),
               "Low Risk Firm",
               fluidRow(
                 column(12,
                        numericInput("max4", "Total Firms in the group:",1580))
               )),
  
  sidebarPanel(width = 4,
               "High Risk Firm",
               fluidRow(
                 column(4,
                        numericInput("min1", "Commitment Exams:",7)),
                 column(4,
                        numericInput("risk1", "Risk Exams:",20))
                 
               ),
               "Med-High Risk Firm",
               fluidRow(
                 column(4,
                        numericInput("min2", "Commitment Exams:",35)),
                 column(4,
                        numericInput("risk2", "Risk Exams:",299))
                 
               ),
               "Med-Low Risk Firm",
               fluidRow(
                 column(4,
                        numericInput("min3", "Commitment Exams:",159)),
                 column(4,
                        numericInput("risk3", "Risk Exams:",0))
               ),
               "Low Risk Firm",
               fluidRow(
                 column(4,
                        numericInput("min4", "Commitment Exams:",222)),
                 column(4,
                        numericInput("risk4", "Risk Exams:",0))
               )
  ),
  
  sidebarPanel(width=12,
               fluidRow(
                 column(12,    
                        h3("Other Assumptions")),
                 column(2,
                        numericInput("ttstf", "Total Staff Available",600)),
                 
                 column(2,
                        numericInput("examsimu", "Average Exams doing Simultaneously by Staff",4)),
                 #column(2,
                 #       numericInput("same", "Exams to be Executed Simultaneouly",4)),
                 column(2,
                        numericInput("utl", "Utilization Rate",0.75)),
                 column(2,
                        tags$strong("Avg Exams by Staff per Year"),
                        verbatimTextOutput("avgexam")
                 )
               ),
               
               mainPanel(
                 fluidRow(
                   column(6,
                          h2("Input Summary", style = "color:skyblue"),
                          DT::dataTableOutput("in1")),
                   column(6,
                          h2("Optimization Summary", style = "color:skyblue"),
                          DT::dataTableOutput("e1")))
                 
               )
  )
)


server <-function(input, output) {
  
  output$in1 <- DT::renderDataTable({
    cmm_in_s <- sum(input$min1,input$min2,input$min3,input$min4)
    cmm_in <- c(input$min1,input$min2,input$min3,input$min4,cmm_in_s)
    rsk_in_s <- sum(input$risk1,input$risk2,input$risk3,input$risk4)
    rsk_in <- c(input$risk1,input$risk2,input$risk3,input$risk4,rsk_in_s)
    in_t <- cbind(cmm_in,rsk_in)
    in_t <- as.data.frame(in_t)
    in_t$tt <- in_t[,1]+in_t[,2]
    n <- c("High Risk Firms","Med-High Risk Firms","Med-Low Risk Firms","Low Risk Firms","Total Exams")
    row.names(in_t) <- n
    colnames(in_t) <- c("Commitment","Risk ","Total Input")
    print(in_t)
  })
  
  # Generate a summary of the dataset
  
  
  output$e1 <- DT::renderDataTable({
    
    ## Set the coefficients of the decision variables
    objective.in  <- c(1, 1, 1,1)
    
    ## Create constraint martix
    const.mat <- matrix(c(input$stf1, input$stf2,input$stf3,input$stf4, 
                          input$stf1*input$day1,input$stf2*input$day2,input$stf3*input$day3,input$stf4*input$day4,
                          #1,0,0,0,
                          #0,-1,1,0,
                          #0,-1,1,0,
                          #-1,-1,-1,1,
                          1,0,0,0,
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1,
                          1,0,0,0,
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1),  
                        nrow=10, byrow=TRUE)
    
    ## define constraints
    avg.length=(input$day1*input$min1+input$day2*input$min2+input$day3*input$min3+input$day4*input$min4)/(input$min1+input$min2+input$min3+input$min4)
    avg.peryear=input$examsimu*365/avg.length
    
    resource_constraint <- input$ttstf*avg.peryear
    
    output$avgexam<-renderText({floor(avg.peryear)})
    
    
    time_constraint <- (input$ttstf*365*input$utl*input$examsimu)
    # cm <- 0
    # cc <- 0
    # dv <- 0
    # rt <- 0
    # te <- 0
    type1_low <- input$min1+input$risk1
    type2_low <- input$min2+input$risk2
    type3_low <- input$min3+input$risk3
    type4_low <- input$min4+input$risk4
    #type5_low <- input$min5
    type1_up <- input$max1
    type2_up <- input$max2
    type3_up <- input$max3
    type4_up <- input$max4
    # type5_up <- input$max5
    
    ## RHS for the constraints
    const.rhs <- c(resource_constraint,
                   time_constraint,
                   #cm,
                   #cc,dv,
                   #rt,
                   type1_low ,type2_low ,type3_low ,type4_low,
                   type1_up,type2_up,type3_up,type4_up)
    
    ## Constraints direction
    const.dir  <- c("<=",
                    "<=", 
                    # ">", ">", ">", 
                    # "<", 
                    ">", ">", ">", ">", 
                    "<=", "<=", "<=", "<=")
    
    ## Find the optimal solution
    optimum <-  lp(direction="max",  objective.in, const.mat, const.dir,  const.rhs)
    op<- round(optimum$solution)
    s<- sum(optimum$solution)
    op <- as.data.frame(op)
    t <- rbind(op, s)
    cmm_in_s <- sum(input$min1,input$min2,input$min3,input$min4)
    cmm_in <- c(input$min1,input$min2,input$min3,input$min4,cmm_in_s)
    rsk_in_s <- sum(input$risk1,input$risk2,input$risk3,input$risk4)
    rsk_in <- c(input$risk1,input$risk2,input$risk3,input$risk4,rsk_in_s)
    t2 <-t-cmm_in-rsk_in
    t3 <- cmm_in + rsk_in
    t2 <- cbind(t2,t3)
    t2 <- round(t2)
    t2$Total <- t2[,1]+t2[,2]
    t2 <- as.data.frame(t2)
    #t <- cbind(t,cmm_in,rsk_in)
    #t<- as.data.frame(t)
    #t$Optimization <- t[,1] - t[,2]-t[,3]
    #t <- round(t)
    n <- c("High Risk Firms","Med-High Risk Firms","Med-Low Risk Firms","Low Risk Firms","Total Exams")
    row.names(t2) <- n
    #  t <- t[,4]
    colnames(t2) <- c("Optimization", "Input", "Total Exams")
    #summary(optimum)
    print(t2)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

