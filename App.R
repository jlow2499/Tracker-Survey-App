library(shiny)
library(shinyjs)
library(DT)
library(dplyr)



setwd("C:/Users/193344/Desktop/tracker gui")
ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv")

ar <- select(ARMASTER,A.R,desk,manager)
ar <- rename(ar,Desk=desk)
ar <- rename(ar,Manager=manager)

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


fields <- c("Desk","Program","File #", "Set Up Date","Tier","Due Date","Payment Method","Payment Amount")


shinyApp(
  ui = fluidPage(shinyjs::useShinyjs(), 
    fluidRow(column(6,downloadButton("downloadData", 'Download Data')),
                 column(6,actionButton("refresh", "Refresh"))),
    
    tabsetPanel( 
    tabPanel("User Input",
            
    numericInput("Desk", "Desk", ""),
    numericInput("File #","File #",""),
    dateInput("Set Up Date","SetUpDate",value=Sys.Date()),
    selectInput("Program","Program",choices=c("15%","FIS","SIF","BIF")),
    selectInput("Tier","Tier",choices=c("Extreme","High","Medium","Low")),
    dateInput("Due Date","Due Date",value=Sys.Date()),
    selectInput("Payment Method","Payment Method",choices=c("Credit Card","Check","Money Order","WU","Mail In")),
    numericInput("Payment Amount","Payment Amount",""),
    actionButton("submit", "Submit")
    ),
    tabPanel("Daily Tracker",
             fluidRow (
                       
    
    dataTableOutput("table1"
    ))),
    tabPanel("Daily Duplicates",
             
             dataTableOutput("dupes")
             ),
    tabPanel("MasterTracker", fluidRow(column(4),
                                       column(4,
                                                        selectInput("Manager",
                                                                    "Manager Select",
                                                                    choices=levels(MasterTracker$Manager),
                                                                    multiple=T,
                                                                    selected=levels(MasterTracker$Manager),
                                                                    selectize=F,
                                                                    size=2)),
                                       column(4)),
             DT::dataTableOutput("MasterTracker")),
    tabPanel("Office Tracker", fluidRow(column(4),
                                        column(4,
                                               selectInput("MGR",
                                                           "Manager Select",
                                                           choices=levels(Budgets$Manager),
                                                           multiple=T,
                                                           selected=levels(Budgets$Manager),
                                                           selectize=F,
                                                           size=2)),
                                        column(4)),
             DT::dataTableOutput("OfficeTracker")
             
             
             ),
    tabPanel("Monthly Duplicates",
             DT::dataTableOutput("MTH")
             )
    
    
    
  )),
  server = function(input, output, session) {
    
    observe({
      if (input$submit > 0) {
        shinyjs::info("Thank you!")
      }
    })   
    observe({
      toggleState("submit", !is.null(input$Desk) && input$Desk != "")
    })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- reactive({

      input$submit
      loadData()
      
    })
    
    output$table1 <- renderDataTable({
      
      input$submit
      loadData()
      
      responses$Desk <- unlist(as.integer(as.character(responses$Desk)))
     responses <- left_join(responses,ar,by="Desk")
     
     
     responses$"Set Up Date" <- unlist(as.numeric(as.character(responses$"Set Up Date")))
     responses$"Set Up Date" <- unlist(as.Date(responses$"Set Up Date", origin = "1970-01-01"))
     responses$"Due Date" <- unlist(as.numeric(as.character(responses$"Due Date")))
     responses$"Due Date" <- unlist(as.Date(responses$"Due Date", origin = "1970-01-01"))
     
      
      responses

      
    },options = list(lengthMenu = c(10, 50, 100, 3000), pageLength = 3000))
    

    output$dupes <- renderDataTable({
      
      input$submit
      loadData()
      
      responses$Desk <- unlist(as.integer(as.character(responses$Desk)))
      responses <- left_join(responses,ar,by="Desk")
      responses$"Set Up Date" <- as.numeric(as.character(responses$"Set Up Date"))
      responses$"Set Up Date" <- as.Date(responses$"Set Up Date", origin = "1970-01-01")
      responses$"Due Date" <- as.numeric(as.character(responses$"Due Date"))
      responses$"Due Date" <- as.Date(responses$"Due Date", origin = "1970-01-01")

      responses2 <- responses[duplicated(responses[,3]),]

      responses2 <- left_join(responses2,ar,by="Desk")
      #responses2$"Set Up Date" <- as.numeric(as.character(responses2$"Set Up Date"))
      #responses2$"Set Up Date" <- as.Date(responses2$"Set Up Date", origin = "1970-01-01")
      #responses2$"Due Date" <- as.numeric(as.character(responses2$"Due Date"))
      #responses2$"Due Date" <- as.Date(responses2$"Due Date", origin = "1970-01-01")
      
      
      responses2
      
      
    })
    
    
    output$MasterTracker <- DT::renderDataTable({
      track <- subset(MasterTracker,Manager %in% c(input$Manager))
      
      datatable(track,extensions = 'TableTools', options = list(
        "sDom" = 'T<"clear">lfrtip',
        "oTableTools" = list(
          "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
          "aButtons" = list(
            "copy",
            "print",
            list("sExtends" = "collection",
                 "sButtonText" = "Save",
                 "aButtons" = c("csv","xls"))))))%>%
        formatCurrency(c("CurrBal","Payment.Amount","EffDt"),"$")
      
    
      
      
      })
    
    
    
    output$OfficeTracker <- DT::renderDataTable({
      
      TR <- subset(TR,Manager%in%c(input$MGR))
      
      datatable(TR,extensions = 'TableTools', options = list(
        "sDom" = 'T<"clear">lfrtip',
        "oTableTools" = list(
          "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
          "aButtons" = list(
            "copy",
            "print",
            list("sExtends" = "collection",
                 "sButtonText" = "Save",
                 "aButtons" = c("csv","xls")))))
                  
                      ) %>%
        formatCurrency(c("Dollar.Budget","Dollar_Initiated","Dollar_Posted"),"$") %>%
        formatPercentage(c("Dollar_BVA","RHB_BVA","RHB_Posted_BVA"))
  
   
    
    })
    
    
    output$MTH <- DT::renderDataTable({
    Duplicates <- MasterTracker[duplicated(MasterTracker[,8]),]
    row.names(Duplicates)<-NULL
    Duplicates <- Duplicates[,-c(1,2,3,13:26)]
    datatable(Duplicates,extensions = 'TableTools', options = list(
      "sDom" = 'T<"clear">lfrtip',
      "oTableTools" = list(
        "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
        "aButtons" = list(
          "copy",
          "print",
          list("sExtends" = "collection",
               "sButtonText" = "Save",
               "aButtons" = c("csv","xls"))))))%>%
      formatCurrency(c("CurrBal"),"$")
    
    })
    
    observeEvent(input$refresh, {
      shinyjs::reset("form")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(responses, file)
      }
    )
    outputOptions(output, "table1", suspendWhenHidden = FALSE)
    outputOptions(output, "dupes", suspendWhenHidden = FALSE)
    
    
    
  }
)
