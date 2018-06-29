library(rCharts)
shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("ForWarD - Online Water Demand Forecasting Tool - Clustering Module"),
  sidebarLayout(
    sidebarPanel(
      h4("Data Selection"),
      selectInput("NoClusters", label="No. of Clusters", choices =1:10),
      selectInput("TSStartYear", label="Year", choices = 2007:format(Sys.Date(), "%Y"), selected="2014"),
      selectizeInput("area",label = "Postal Code(s)","",multiple = TRUE),
      selectizeInput("steps",label = "Consumption Interval(s)","",multiple = TRUE),
      br(), 
      submitButton(text = "Run", icon("cog")),
      br(),
      br())
    ,   
    mainPanel(
      tabsetPanel(
        tabPanel("Datatable",  h4("Clusters"), tableOutput("text1"), downloadButton('downloadData1', 'Download'), h4("Mean Value of Annual Water Consumption per Cluster"), tableOutput("text2"),  downloadButton('downloadData2', 'Download')),
        tabPanel("Clusters Barcharts", uiOutput("plots"))
     )
  ))
))