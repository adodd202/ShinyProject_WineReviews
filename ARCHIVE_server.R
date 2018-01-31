library(shiny) 
library(shinyGlobe)
library(dplyr)
library(ggplot2)
library(ggthemes)

shinyServer(function(input, output, session) {
  
  #Observers for filters and inputs
  values1 = reactiveValues(default = 0)
  observeEvent(input$action, {values1$default = input$action})  
  dataInput1 = eventReactive(input$action, {input$varietySelected})
  dataInput2 = eventReactive(input$action, {input$maxPrice})
  
  #Building reactive dataframe, Wine Data
  datadata <- wine
  makeReactiveBinding("datadata")
  newData <- reactive({
    input$action
    isolate({
      datadata <- wine
      input1 = dataInput1()
      input2 = dataInput2() #access with [1], [2]
      if (input1 != "Any"){
         datadata = datadata %>% filter(variety == input1)
      }
      datadata = datadata %>% filter(price  <= input2)
    })
  })
  
  #Building reactive globe dataframe
  wine_lat_long <- wine
  makeReactiveBinding("wine_lat_long")
  newData2 <- reactive({
    input$action
    isolate({
      datadata2 <- wine
      #Doing filters
      input1 = dataInput1()
      input2 = dataInput2() #access with [1], [2]
      if (input1 != "Any"){
          wine_lat_long = wine_lat_long %>% filter(variety == input1)
      }
      wine_lat_long = wine_lat_long %>% filter(price  <= input2)
      
      #Building globe data
      wine_lat_long = wine_lat_long %>% group_by(lat, long) %>%
        summarise(n_wines = n()/nrow(wine_lat_long))
      wine_max = wine_lat_long %>% arrange(desc(n_wines))
      wine_max = wine_max[1,3]$n_wines[1]
      wine_lat_long = wine_lat_long %<>% mutate(n_wines = (n_wines/wine_max))
    })
  })
  
  #Plot generation
  output$winePlot1 = renderPlot({
    datadata = newData()
    length = nrow(datadata)
    ggplot(datadata, aes(x = points , y= price, color = country)) + geom_point() + 
      geom_jitter() + 
      ggtitle("Wines: Cost versus Points") +
      xlab("Points (out of 100)") + ylab("Price ($)")
  }, height = 400, width = 600)
  
  #observe event around 
  
  #Hover for plot
  output$hover_info <- renderPrint({
    if(!is.null(input$plot1_hover)){
      hover=input$plot1_hover
      newData = newData()
      dist=sqrt((hover$x-newData$points)^2+(hover$y-newData$price)^2)
      cat("Wine title:")
      if(min(dist) < 3)
        newData$title[which.min(dist)]
    }
  })
  
  output$globe <- renderGlobe({
    newData2()
  })
  
  ################################################################
     # CUSTOM RESULT GENERATOR
  ################################################################
  # dataInput3 = eventReactive(input$search, {input$custom})
  wineOutput = wine3000
  makeReactiveBinding("wineOutput")
  wineOutputDF <- reactive({
      input$search
      isolate({
      nrow1 = 1000
      UserInput = input$inputText
      maxPrice2 = input$maxPrice2
      #print(UserInput)
      tdm_user = TermDocumentMatrix(Corpus(VectorSource(UserInput)), control = ctrl)
      myTdm_user <- as.matrix(tdm_user)
      freqDF_user <- data.frame(ST = rownames(myTdm_user), 
                                Freq = rowSums(myTdm_user), 
                                row.names = NULL)
      freqOneDesc_user = freqDF_user %>% filter(!grepl("[0-9]{5,}",ST))
      vec = seq(1,nrow1)
      descripOne_user = paste(freqOneDesc_user$ST, sep = " ", collapse = " ")
      #print(descripOne_user)
      userV1 = sapply(vec,function(x) {x = 1*grepl(listLex[x],descripOne_user)})
      vecResults = seq(1:3000)
      vecResults = sapply(vecResults,function(x) {x = cosine(userV1,mm[,x])})
      #print(vecResults[1:20])
      whichpart <- function(x, n) {
          nx <- length(x)
          p <- nx-n
          xp <- sort(x, partial=p)[p]
          which(x > xp) 
      }
      #print ("hi")
      #print(vecResults)
      wineIndices = whichpart(vecResults,n = 100)
      #print(wineIndices)
      #print("hello")
      wineOutput = data.frame(wine3000[wineIndices,])
      wineOutput = wineOutput[c("title", "description", "variety","country","price","points")]
      wineOutput = wineOutput %>%
        filter(price  <= maxPrice2) %>% 
        arrange(desc(points))
      wineOutput = setnames(wineOutput, old=c("title","description","variety","country","price","points"),
                            new=c("Title", "Description","Variety","Country","Price","Points"))
      # drops <- c("taster_name","region_2")#"taster_twitter_handle",
      #            #"address","lat","long","description2","V1")
      # wineOutput <- subset(wineOutput, select = -drops)
      #print(wineOutput)
      # maxPrice = 
      })
  })
  
  #Table Generation
  output$wineTable = renderTable({
    wineOutputDF()
  }, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '400px', targets = 3,scrollX = TRUE))
  ))
  
  
})
