##########Libraries##########
library(shiny)
library(data.table)
library(rCharts)
library(forecast)
library(reshape2)
library(xtable)
`%then%` <- shiny:::`%OR%`
##########Data Parsing##########
#TK<-read.csv("Areas.csv", header = TRUE, stringsAsFactors = FALSE,sep="#")
#TK<-TK[TK$Μunicipal !="ΕΙΚΟΝΙΚΟΣ ΔΗΜΟΣ", ]
#Steps<-read.csv("Steps.csv", header = TRUE, stringsAsFactors = FALSE,sep="#")

TK<-read.csv("Areas.csv", header = TRUE, stringsAsFactors = FALSE,sep=",")
Steps<-read.csv("Steps.csv", header = TRUE, stringsAsFactors = FALSE,sep="#")
data1<-read.table("WorkingFileFormats/data1.txt", header = TRUE, stringsAsFactors = FALSE,sep=";")
data1<-read.table("data1.txt", header = TRUE, stringsAsFactors = FALSE,sep=";")
data1<- data1[ data1$ZipCode != 0, ]
##########Shiny server.R##########
shinyServer(function(input, output, session) { 
  withProgress(message = 'ForWarD Initialisation',
               detail = 'Please Wait...', value = 0, {
                 for (i in 1:10) {
                   incProgress(1/10)
                   Sys.sleep(0.25)
                 }
               })
  
  observe({ #Area and Step Input   
    areasel <-rbind("All", TK$Selection)
    updateSelectInput(session, "area", choices =areasel)
    stepssel <- rbind("All", Steps$Step)
    updateSelectInput(session, "steps", choices =stepssel)
  }) 
 
  x<-reactive({ #Get data for selected areas and steps
    validate(
      need(input$area != '', 'Please select at least 1 Postal Code') %then%
        need(input$steps != '', 'Please select at least 1 Consumption Interval')
    )
    inputarea<-input$area
    inputsteps<-input$steps
    TSPERIOD1<-paste(input$TSStartYear,sprintf("%02d", 01), sep="")
    TSPERIOD2<-paste(input$TSStartYear,sprintf("%02d", 12), sep="")
    data1 <- data1[data1$Date >= TSPERIOD1 & data1$Date <= TSPERIOD2,]
   
    if (any("All" %in% inputarea)) inputarea<-TK$Selection
    if (any("All" %in% inputsteps)) inputsteps<-Steps$Step
    

                  
                   subsettk<-TK[TK$Selection %in% inputarea,]      
                   subsetdata<-data1[data1$ZipCode %in% subsettk$ZipCode,]      
                   subsetdata<-subsetdata[subsetdata$Steps %in% inputsteps,] 
                   selected<-subsetdata[,c("Date","CNT","QTY", "Steps", "ZipCode")] #select date cnt & qty columns
                   selected[is.na(selected)] <- 0
                   TempDate <- selected$Date
                   TempDate <- paste(TempDate,"01",sep="")
                   FullDate <- as.Date(TempDate, "%Y%m%d")
                   selected$Hmeromhnia <- as.character(FullDate)
                   selected <- selected[order(selected$Date),]
                   selected
                 

  })

  ##############Cluster Analysis#########
  
  fileCNT<-reactive({
    df<-x()
    inputarea<-input$area
    inputsteps<-input$steps
    if (any("All" %in% inputarea)) inputarea<-TK$Selection
    if (any("All" %in% inputsteps)) inputsteps<-Steps$Step
    fileCNTa<- data.frame(matrix( ncol=length(inputsteps), nrow=length(inputarea)))

    subsettk<-TK[TK$Selection %in% inputarea,]  
    inputarea2<-subsettk$ZipCode
    inputarea3<-as.character(as.numeric(subsettk$ZipCode))
    inputarea2[is.na(inputarea2)] <- 0

    withProgress(message = 'Clusters Creation',
                 detail = 'Please Wait...', value = 0, {
                   pbar <- function() {
                     incProgress(1/length(df[,1]))
                     Sys.sleep(0.05)     
                   }      
    
    fileCNTa[is.na(fileCNTa)] <- 0
    subsetdata<-df[df$Steps %in% inputsteps,]
    for (i in 1:length(df[,1])){ 
      for (k in 1:length(inputarea)){
        for (j in 1:length(inputsteps)){         
            if (df$Steps[i]==inputsteps[j]){ 
            if (df$ZipCode[i]==inputarea2[k]){
            rownames(fileCNTa)[k] <- inputarea3[k]
            colnames(fileCNTa)[j] <- inputsteps[j]
            fileCNTa[k,j]=fileCNTa[k,j]+df$QTY[i]
          }}
        }
      }
    pbar()
    }
})
    fileCNTa
  })
 
clustersum<-reactive({
  d <- dist(fileCNT(), method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward.D2") 
  groups <- cutree(fit, k=input$NoClusters) # cut tree into 5 clusters
  groups
})
output$text1 <- renderTable({ 
  d<-as.data.frame(as.matrix(clustersum()))
  colnames(d)[colnames(d)=="x"] <- "Cluster"
  tkk <- rownames(d)
  d <- cbind(ZipCode=tkk, d)
  total <- merge(d, TK, by="ZipCode")
  colnames(total)[colnames(total)=="V1"] <- "Cluster"
  total[,c("Selection", "Cluster")]
})
downloaddata1<- reactive({
  d<-as.data.frame(as.matrix(clustersum()))
  colnames(d)[colnames(d)=="x"] <- "Cluster"
  tkk <- rownames(d)
  d <- cbind(ZipCode=tkk, d)
  total <- merge(d, TK, by="ZipCode")
  colnames(total)[colnames(total)=="V1"] <- "Cluster"
  total[,c("Selection", "Cluster")]
})
plotdata <-reactive({
  inputarea<-input$area
  inputsteps<-input$steps
  if (any("All" %in% inputarea)) inputarea<-TK$Selection
  if (any("All" %in% inputsteps)) inputsteps<-Steps$Step
  clsum<-as.data.frame(as.matrix(clustersum()))
  clsum<-cbind(ZipCode = rownames(clsum), clsum) 

  total <- merge(x(),clsum,by="ZipCode")
  colnames(total)[colnames(total)=="V1"] <- "Cluster"
  mdata<- data.frame(matrix( ncol=length(inputsteps), nrow=length(input$NoClusters)))
  mdata[is.na(mdata)] <- 0
  for (i in 1:input$NoClusters) {

    newdata<- total[total$Cluster == i, ]
    for (j in 1:length(inputsteps)) {
      stepdata<- newdata[newdata$Steps == inputsteps[j], ] 
      stepdatab<-stepdata[,c("QTY")]
      mdata[i,j] <- mean(stepdatab, na.rm = TRUE)
      colnames(mdata)[j] <- inputsteps[j]
    }
    rownames(mdata)[i]<-i
  }
  mdata
})
output$text2 <- renderTable({ 
plotdata()
})
output$text3 <- renderTable({ 
  tdata<-t(data.matrix(plotdata()[1,], rownames.force = NA))
  tdata<-t(data.matrix(plotdata()[1,], rownames.force = NA))
  datatable<-cbind(tdata, colnames(plotdata()))
  colnames(datatable)[1]<-"mesos"
  colnames(datatable)[2]<-"steps"
  datatable<-as.data.frame(datatable)
  datatable$steps<-as.character(datatable$steps)
  datatable$mesos<-as.numeric(as.character(datatable$mesos))
  datatable
})
  ##############Final Table Output############## 
  OutputTable<-reactive({
    datatable<-x()
    colnames(datatable)[colnames(datatable)=="Date"] <- "ΗΜΕΡΟΜΗΝΙΑ"
    colnames(datatable)[colnames(datatable)=="QTY"] <- "ΣΥΝΟΛΙΚΗ ΚΑΤΑΝΑΛΩΣΗ"
    colnames(datatable)[colnames(datatable)=="CNT"] <- "ΑΡΙΘΜΟΣ ΜΕΤΡΗΤΩΝ"
    head(datatable, n=6)
  })  
  #############################################

  ##############Plot Output############## 
  output$plots <- renderUI({
    plot_output_list <- lapply(1:input$NoClusters, function(i) {
      plotname <- paste("plot", i, sep="")
      chartOutput(plotname, "highcharts")
    })
    # Convert the list to a tagList - this is necessary for the list of item to display properly.
    do.call(tagList, plot_output_list)
  })
  
  for (i in 1:10) {
    
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      output[[plotname]] <- renderChart2({

        tdata<-t(data.matrix(plotdata()[my_i,], rownames.force = NA))
        tdata<-t(data.matrix(plotdata()[my_i,], rownames.force = NA))
        datatable<-cbind(tdata, colnames(plotdata()))
        colnames(datatable)[1]<-"mesos"

        colnames(datatable)[2]<-"steps"
        datatable<-as.data.frame(datatable)
        datatable$steps<-as.character(datatable$steps)
        datatable$mesos<-as.numeric(as.character(datatable$mesos))
        datatable$mesos<-round(datatable$mesos, digits = 2)
        colnames(datatable)[1]<-"ΕΤΗΣΙΑ ΚΑΤΑΝΑΛΩΣΗ"
        colnames(datatable)[2]<-"ΚΛΙΜΑΚΙΑ ΜΗΝΙΑΙΑΣ ΚΑΤΑΝΑΛΩΣΗΣ"
        labs<-as.list(datatable[,c("ΚΛΙΜΑΚΙΑ ΜΗΝΙΑΙΑΣ ΚΑΤΑΝΑΛΩΣΗΣ")])
        
        h1 <- Highcharts$new()
        h1$series(data = datatable[,c("ΕΤΗΣΙΑ ΚΑΤΑΝΑΛΩΣΗ")], type = "column", name=(paste0("CLUSTER ", my_i)))
        h1$xAxis(labels=list(rotation = -90), categories = labs )
        h1$title(text = paste0("CLUSTER ", my_i) ) 
        h1$exporting(enabled = T)
        h1$chart(zoomType = "xy")
        return(h1)  
      })
    })
  }
  ############################################

  ##############Download Buttons##############

output$downloadData1 <- downloadHandler(
  filename = function() {
    paste('data-1-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(downloaddata1(), file, fileEncoding="UTF-8")
  }
)
output$downloadData2 <- downloadHandler(
  filename = function() {
    paste('data-2-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(plotdata(), file)
  }
)



})

