
SSOI<-read.csv("~/shiny//SSOI.csv")
ui<-fluidPage(
  titlePanel("SSOI Data"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
           selectInput("tre",
                       "Tresuries:",
                       c("All",
                         'Missing'))),
    column(3,
           selectInput("corp",
                       "Corporate:",
                       c("All",
                         'Missing'))),
    column(3,
           selectInput("muni",
                       "Muni:",
                       c("All",
                         'Missing'))),
    column(3,
           selectInput("sec",
                       "Securitized:",
                       c("All",
                         'Missing')))
  ),
  sidebarPanel(
    fluidRow(
      column(6,
             selectInput("finopd",
                         "Finop District:",
                         c("All",
                           unique(as.character(SSOI$Finop.District))))),
      column(6,
             selectInput("spd",
                         "SP District:",
                         c("All",
                           unique(as.character(SSOI$SP.District))))),
      column(6,
             selectInput("finops",
                         "Finop Supervisor:",
                         c("All",
                           unique(as.character(SSOI$FINOP.Supervisor))))),
      column(6,
             selectInput("sps",
                         "SP Supervisor:",
                         c("All",
                           unique(as.character(SSOI$SP.Supervisor))))),
      column(6,
             selectInput("finoprc",
                         "Finop RC:",
                         c("All",
                           unique(as.character(SSOI$FINOP.RC))))),
      column(6,
             selectInput("sprc",
                         "SP RC:",
                         c("All",
                           unique(as.character(SSOI$SP.RC)))))
    ),
    # Button
    downloadButton("downloadData", "Download as csv")
    
  ),
  verbatimTextOutput("summary"),
  # Create a new row for the table.
  DT::dataTableOutput("tb")
)

server<-function(input, output,session) {
  data<-SSOI
  filteredTB<-reactive({
    if (input$finopd != "All") {
      data <- data[data$Finop.District == input$finopd,]
    }
    if (input$finoprc != "All") {
      data <- data[data$FINOP.RC == input$finoprc,]
    }
    if (input$finops != "All") {
      data <- data[data$FINOP.Supervisor == input$finops,]
    }
    if (input$spd != "All") {
      data <- data[data$SP.District == input$spd,]
    }
    if (input$sprc != "All") {
      data <- data[data$SP.RC == input$sprc,]
    }
    if (input$sps != "All") {
      data <- data[data$SP.Supervisor == input$sps,]
    }
    if(input$tre == "Missing"){
      data=data[data$Tresuries.Revenue==0 & data$Tresuries.Volume!=0,]
      df=data()
    }
    if(input$corp == "Missing"){
      data=data[data$Corp.Revenue==0 & data$Corp.Volume!=0,]
      df=data()
    }
    if(input$muni == "Missing"){
      data=data[data$Muni.Revenue==0 & data$Muni.Volume!=0,]
      df=data()
    }
    if(input$sec == "Missing"){
      data=data[data$Securitized.Revenue==0 & data$Securitized.Volume!=0,]
      df=data()
    }
    data
  })
  
  # Filter data based on selections
  output$tb <- DT::renderDataTable(DT::datatable({
    
    filteredTB()
    
    #df=data()
  }))
  output$summary <- renderPrint({
    renderText('this is for testing ==0')
    summary(data[2:9]==0)
    
  })
  help(summary)
  #thedata<-reactive(SSOI)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('SSOI-filtered data', ".csv", sep = "")
    },
    content = function(file) {
      s=input$tb_rows_all
      write.csv(filteredTB()[s, , drop=F], file, row.names = T)
    }
  )
}

shinyApp(ui,server)
