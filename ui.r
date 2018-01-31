library(shiny)
library(shinyGlobe)
library(shinydashboard)
library(gridExtra)

shinyUI(dashboardPage(
  dashboardHeader(title  = "The Wine List"),
  dashboardSidebar(
    sidebarUserPanel(tags$div(class = "header2", tags$p("Andrew Dodd")),
                     tags$head(tags$style("p{color: black; font-size: 16px;} .header2 p{font-size: 15px; color: white}")),
                     image = "https://media-exp2.licdn.com/mpr/mpr/shrinknp_200_200/AAEAAQAAAAAAAAdzAAAAJDA3OTU4ODNjLTRiZDctNGEzNi1iNGFlLTU0MzBjYjdjYmJiZA.jpg"),
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("glass")),
      menuItem("Wine Search Results", tabName = "wine", icon = icon("signal")),
      menuItem("Global View", tabName = "globe", icon = icon("globe")),
      menuItem("Custom Search", tabName = "custom", icon = icon("code"))
    ),
    absolutePanel(title = 'Filters (Click "Go!" to Search)', status = "primary",
    actionButton("action", label = "Go!"), 
    selectizeInput("varietySelected", "Variety", varietyStr),
    numericInput("maxPrice", "Max Price:", 30, min = 0),
    top = 350, left = 0, right = 0, 
    bottom = 0, style = 'background: #000000, opacity: 1')
  ),
  
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            box(
              textOutput('text1'),
              tags$head(tags$style("p{color: black;
                                   font-size: 16px;}
                                   .header p{font-size: 22px;}")),
              tags$div(class = "header",
                       tags$p("The Wine List: Introduction")
              ),
              tags$p("This is a wine selection app that is built on a database of 
                approximately 120,000 wines from Kaggle.com [1]. To use the app, you may select various 
                filters from the left column, and when ready, click 'Go'. This will create some displays
                in 'Wine Search Results' and 'Global View'. In the 'Custom Input' tab, you can type in
                a search like 'sweet dessert wines from southern france, taste like raspberries' and the app 
                will search for the closest fit to your request. Enjoy!"),
              tags$p(" "),
              tags$p("Source data: https://www.kaggle.com/zynicide/wine-reviews")
              )),
    tabItem(tabName = "wine",
            fluidRow(
              tabBox(
                id = "tabset1", height = "250px", width = '100%',
                tabPanel("Wine Plots",
                         fluidRow(
                         column(width = 6,
                         plotOutput("winePlot1")
                         ),
                         column(width = 6,
                         plotOutput("winePlot2")))
                        ),
                tabPanel("Wines Results List",
                         tableOutput("wineTable2"),
                         tags$head(tags$style("#wineTable2 table {color: black; background: white; }", media="screen", type="text/css"))
            ))
    )),
    tabItem(
      includeCSS("www/custom.css"),
      tabName = "globe",
      tags$head(tags$style(
        HTML(
          '
          /* logo */
          .skin-blue .main-header .logo {
          background-color: #8B0000;
          }
          
          /* logo when hovered */
          .skin-blue .main-header .logo:hover {
          background-color: #CB0470;
          }
          
          /* navbar (rest of the header) */
          .skin-blue .main-header .navbar {
          background-color: #8B0000;
          }
          
          /* main sidebar */
          .skin-blue .main-sidebar {
          background-color: #993333;
          }
          
          /* active selected tab in the sidebarmenu */
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #ffccff;
          }
          
          /* other links in the sidebarmenu */
          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          background-color: #ff99ff;
          color: #000000;
          }
          
          /* other links in the sidebarmenu when hovered */
          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #ff69b4;
          }
          /* toggle button when hovered  */
          .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #EF4AA9;
          }
          '
        )
        )),
      
        tagList(globeOutput("globe"))
    ),
    tabItem(tabName = "custom",
            fluidPage(
              box(fluidRow(
                column(6,
                       tags$div(class = "header",
                                tags$p("Woogle Search Engine")
                       ),
                       textInput("inputText"," ", "Type search (e.g.: 'sweet dessert wines from southern france'):"),
                       actionButton("search", label = "Search")),
                column(6,
                       sliderInput("maxPrice2","Max Price",min = 0,max = 2000, 50),
                       tags$head(tags$style("#maxPrice2 {color:black;}")))
              ), width = 12)),
              title = "Wine Suggestions",
              box(tableOutput('wineTable'), width = 12),
              tags$head(tags$style("#wineTable table {color: black; background: white; }", media="screen", type="text/css")),
              hr()
    ))

  ))
)