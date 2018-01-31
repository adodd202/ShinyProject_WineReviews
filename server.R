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
      input2 = dataInput2()
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
      input1 = dataInput1()
      input2 = dataInput2()
      if (input1 != "Any"){
        wine_lat_long = wine_lat_long %>% filter(variety == input1)
      }
      wine_lat_long = wine_lat_long %>% filter(price  <= input2)
      wine_lat_long = wine_lat_long %>% group_by(lat, long) %>%
        summarise(n_wines = n()/nrow(wine_lat_long))
      wine_max = wine_lat_long %>% arrange(desc(n_wines))
      wine_max = wine_max[1,3]$n_wines[1]
      wine_lat_long = wine_lat_long %<>% mutate(n_wines = (n_wines/wine_max))
    })
  })
  
  # Wine table 2 generation
  wineOutput2 = wine
  makeReactiveBinding("wineOutput2")
  wineOutputDF2 <- reactive({
    input$action
    isolate({
      wineOutput2  <- wine
      input1 = dataInput1()
      input2 = dataInput2()
      if (input1 != "Any"){
        wineOutput2  = wineOutput2  %>% filter(variety == input1)
      }
      wineOutput2  = wineOutput2  %>% filter(price  <= input2)
      wineOutput2 = wineOutput2[c("title", "description", "variety","country","price","points")]
      wineOutput2 = wineOutput2 %>% arrange(desc(points))
      wineOutput2 = setnames(wineOutput2, old=c("title","description","variety","country","price","points"),
                            new=c("Title", "Description","Variety","Country","Price","Points"))
    })
  })
  
  # Wine table 2 plotting
  output$wineTable2 = renderTable({
    wineOutputDF2()
  }, options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '400px', targets = 3,scrollX = TRUE))
  ))
  
  #Plot generation
  output$winePlot1 = renderPlot({
    datadata = newData()
    length = nrow(datadata)
    ggplot(datadata, aes(x = points , y= price, color = country)) + geom_point() + 
      geom_jitter() + 
      ggtitle("Wines: Cost versus Rating") +
      xlab("Rating (out of 100)") + ylab("Price ($)") + 
      theme(legend.position="bottom")
  }, height = 400, width = 600)
  
  
  #Plot generation
  output$winePlot2 = renderPlot({
    ggplot(wine2, aes(x = points , y= price)) + geom_point(alpha=0.1) + 
      ggtitle("All Wines: Cost versus Rating") +
      xlab("Rating (out of 100)") + ylab("Price ($)") + 
      theme(legend.position="bottom")
  }, height = 400, width = 600)
  
  #Globe generation
  output$globe <- renderGlobe({
    newData2()
  })
  
  ################################################################
  # CUSTOM RESULT GENERATOR
  ################################################################
  
  wineOutput = wine3000
  makeReactiveBinding("wineOutput")
  wineOutputDF <- reactive({
    input$search
    isolate({
      nrow1 = 1000
      UserInput = input$inputText
      maxPrice2 = input$maxPrice2
      tdm_user = TermDocumentMatrix(Corpus(VectorSource(UserInput)), control = ctrl)
      myTdm_user <- as.matrix(tdm_user)
      freqDF_user <- data.frame(ST = rownames(myTdm_user), 
                                Freq = rowSums(myTdm_user), 
                                row.names = NULL)
      freqOneDesc_user = freqDF_user %>% filter(!grepl("[0-9]{5,}",ST))
      vec = seq(1,nrow1)
      descripOne_user = paste(freqOneDesc_user$ST, sep = " ", collapse = " ")
      userV1 = sapply(vec,function(x) {x = 1*grepl(listLex[x],descripOne_user)})
      vecResults = seq(1:3000)
      vecResults = sapply(vecResults,function(x) {x = cosine(userV1,mm[,x])})
      whichpart <- function(x, n) {
        nx <- length(x)
        p <- nx-n
        xp <- sort(x, partial=p)[p]
        which(x > xp) 
      }
      wineIndices = whichpart(vecResults,n = 100)
      wineOutput = data.frame(wine3000[wineIndices,])
      wineOutput = wineOutput[c("title", "description", "variety","country","price","points")]
      wineOutput = wineOutput %>%
        filter(price  <= maxPrice2) %>% 
        arrange(desc(points))
      wineOutput = setnames(wineOutput, old=c("title","description","variety","country","price","points"),
                            new=c("Title", "Description","Variety","Country","Price","Points"))
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
