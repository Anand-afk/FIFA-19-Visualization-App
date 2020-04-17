#importing all the required libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(d3heatmap)
library(corrgram)
library(countrycode)

#reading the csv file
data_f <- read.csv('data.csv',stringsAsFactors = F,header=T)
fifa_data <- tbl_df(data_f)

#updating the dataframe with only the required columns
fifa_data <- select(fifa_data, ID, Name, Age,Photo, Nationality, Overall,Potential, Club, Value, Wage, Position, Preferred.Foot,Height,Weight,GKDiving,GKHandling,GKReflexes,GKKicking,GKPositioning,
                    Crossing,Finishing,ShortPassing,Volleys,Dribbling,Curve,FKAccuracy,LongPassing,BallControl,Acceleration,SprintSpeed,Agility,ShotPower,Jumping,Stamina,Strength,LongShots,Aggression,Interceptions,Positioning,Vision,Penalties,Composure,Marking,StandingTackle,
                    SlidingTackle)

country_names<-levels(factor(fifa_data$Nationality))
club_names<-levels(factor(fifa_data$Club))

#converting the market value and wage into numbers, removing the euro sign and substrings K and M.
toNumberCurrency <- function(vector) {
  vector <- as.character(vector)
  vector <- iconv(vector, 'utf-8', 'ascii', sub='')
  
  result <- as.numeric(vector)
  
  k_positions <- grep("K", vector)
  result[k_positions] <- as.numeric(gsub("K","",vector[k_positions])) * 1000
  
  m_positions <- grep("M", vector)
  result[m_positions] <- as.numeric(gsub("M","",vector[m_positions])) * 1000000
  
  return(result)
  
}
#calling the above function to convert the value and wage
fifa_data$Value<- toNumberCurrency(fifa_data$Value)
fifa_data$Wage<- toNumberCurrency(fifa_data$Wage)


#converting all sorts of unncessary postions to the more general positions 
x <- as.factor(fifa_data$Position)
levels(x) <- list(GK  = c("GK"), 
                  left_def = c("LWB","LB","LCB"), 
                  right_def=c("RB","RWB","RCB"),
                  center_def=c("CB"),
                  center_mid=c("CDM","CM","CAM",""),
                  left_mid=c("LM","LAM","LCM","LDM"),
                  right_mid=c("RM","RCM","RDM","RAM"),
                  striker = c( "ST"),
                  left_fwd=c("LF","LS","LW"),
                  right_fwd=c("RF","RS","RW"),
                  center_fwd=c("CF"))
#after converting saving the result in the dataframe, as a new column called position general.
fifa_data <- mutate(fifa_data, Position_general = x)


########################################################################################################

# Application title
header <- dashboardHeader(title = "Fifa 19 Visualisation")

# Sidebar with a slider input for number of bins 
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Player Stats", tabName = "stats", icon = icon("dashboard")),
              menuItem("Attributes Correlation", tabName = "corr", icon=icon("dashboard")),
              menuItem("Left Footed VS Right Footed", tabName = "lr", icon = icon("dashboard")),
              menuItem("Density Globe",tabName = "map",icon = icon("dashboard")),
              menuItem("Best Players", tabName = "best", icon = icon("dashboard"))
  ))

#contains the dashboard body
body <- dashboardBody(
  tabItems(
    tabItem(
      #tab number 1
      tabName = "stats",
            h3("Player performance"), 
            sidebarMenu(
              radioButtons("rad", "Choose:", c("Country-Wise" = "count", "Club-Wise" = "club"),selected = "count"),
              conditionalPanel('input.rad === "count"', selectInput(
                inputId = "country",
                label = "Country:",
                choices = country_names,
                selected = "Portugal",
                selectize = FALSE
              )),
              
              conditionalPanel('input.rad === "club"', selectInput(
                inputId = "clubid",
                label = "Club:",
                choices = club_names,
                selected = "Real Madrid",
                selectize = FALSE
              ))),tags$br(),
            fluidRow(
              
              actionBttn(inputId = "Pot",label = "Potential", color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
              actionBttn(inputId = "Over",label = "Overall",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
              actionBttn(inputId = "age",label = "Age",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
              actionBttn(inputId = "height",label = "Height",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
              actionBttn(inputId = "weight",label = "Weight",color = "success",style = "material-flat",icon = icon("sliders"),size = "md")
            ),
            tags$br(),
            
            fluidRow(
              column(
                width = 8,
                plotlyOutput("plot1")
              ),
              column(
                width = 4,
                uiOutput("img"),
                tags$br(),
                verbatimTextOutput("click")
              )
            )),
    #tab number 2
    tabItem(tabName = "corr",
            h3("Attribute Correlation HeatMap"),
            tags$br(),
            h4("Select a Position and get the correlation matrix of the attributes: "),
            sidebarMenu(fluidRow(
              actionBttn(
                inputId = "dirty",
                label = "Dirty",
                color = "success",
                style = "material-flat",
                icon = icon("sliders"),
                size = "md"
              ),
              actionBttn(
                inputId = "keeper",
                label = "Keeper Attr",
                color = "success",
                style = "material-flat",
                icon = icon("sliders"),
                size = "md"
              ),
              actionBttn(
                inputId = "striker",
                label = "Striker Attr",
                color = "success",
                style = "material-flat",
                icon = icon("sliders"),
                size = "md"
              ),
              actionBttn(
                inputId = "mid",
                label = "Mid-fielder Attr",
                color = "success",
                style = "material-flat",
                icon = icon("sliders"),
                size = "md"
              ),
              actionBttn(
                inputId = "def",
                label = "Defense Attr",
                color = "success",
                style = "material-flat",
                icon = icon("sliders"),
                size = "md"
              ))
            ),
            selectInput("palette", "Palette", c("Blues","YlOrRd", "RdYlBu", "Greens")),
            checkboxInput("cluster", "Apply Clustering"),
            checkboxInput("scale", "Apply Scale"),
            tags$br(),
            wellPanel(h4("Heatmap"),d3heatmapOutput("heatmap", width = "60%", height = "500px"))),
    
    #tab number 3
    tabItem(tabName = "lr",
            tabsetPanel(
              id = "tabs",
              #subtab no. 1
              tabPanel(
                title = "Proportion",
                value = "page1",
                h3("Left vs Right foot players"),
                sidebarMenu(
                  radioButtons("rad1", "Choose:", c("Country-Wise" = "count1", "Club-Wise" = "club1"),selected = "club1"),
                  conditionalPanel('input.rad1 === "count1"', selectInput(
                    inputId = "country1",
                    label = "Country:",
                    choices = country_names,
                    selected = "Portugal",
                    selectize = FALSE
                  )),
                  
                  conditionalPanel('input.rad1 === "club1"', selectInput(
                    inputId = "clubid1",
                    label = "Club:",
                    choices = club_names,
                    selected = "Real Madrid",
                    selectize = FALSE
                  ))
                ),
                
                fluidRow(
                  column(
                    width = 5,
                    tags$br(),
                    plotlyOutput("plot2")
                  ),
                  column(
                    width = 6,
                    dataTableOutput("click1")
                  )
                )
              ),
              #subtab no. 2
              tabPanel(
                title = "At various positions",
                value = "page2",
                h3("Left and right at various positions"),
                sidebarMenu(
                  radioButtons("rad2", "Choose:", c("Country-Wise" = "count2", "Club-Wise" = "club2"),selected = "club2"),
                  conditionalPanel('input.rad2 === "count2"', selectInput(
                    inputId = "country2",
                    label = "Country:",
                    choices = country_names,
                    selected = "Portugal",
                    selectize = FALSE
                  )),
                  
                  conditionalPanel('input.rad2 === "club2"', selectInput(
                    inputId = "clubid2",
                    label = "Club:",
                    choices = club_names,
                    selected = "Real Madrid",
                    selectize = FALSE
                  ))
                ),
                
                fluidRow(
                  column(
                    width = 5,
                    plotlyOutput("plot3")
                  ),
                  column(
                    width = 6,
                    dataTableOutput('table')
                  ))
              )
              
            )),
    #tab number 4
    tabItem(tabName = "map",
            h3("Density of players all over the world."),
            fluidRow(column( 
              width = 12,
              plotlyOutput('map')
            ),
            tags$br(),
            column(width = 6),
            dataTableOutput('click2')
            )),
    tabItem(tabName = "best",
            h3("Best Players at different postions"),
            h4("Now, let's have a look at the top 10 players for the different player positions based on the overall rating differentiated by their respective positions. We will first subset the entire data set to the players having their playing position in the column 'Preferred.Positions'. Then we select the Top 10 players for each playing position and sort them in the descending order of their overall ranking and their particular player position attribute value."),
            plotlyOutput('violin'),
            #verbatimTextOutput('clickviolin')
            dataTableOutput('clickviolin')
            
    )))

ui <- dashboardPage(skin = "purple",
                    header,
                    sidebar,
                    body
)
###############################################################################################################
#The server starts here

server <- function(input, output) {
  #declaring reactive functions for first tab
  tab1 <- reactive({
    if (input$rad == "club"){
      x <- fifa_data %>%
        filter(Club == input$clubid)
    }
    else if (input$rad == "count"){
      x <- fifa_data %>%
        filter(Nationality == input$country)}
  })
  #declaring reactive functions for third tab, first subtab
  tab3 <- reactive({
    if (input$rad1 == "club1"){
      x <- fifa_data %>%
        filter(Club == input$clubid1)
    }
    else if (input$rad1 == "count1"){
      x <- fifa_data %>%
        filter(Nationality == input$country1)}
  })
  #declaring reactive functions for third tab, second subtab
  tab3_1 <- reactive({
    if (input$rad2 == "club2"){
      x <- fifa_data %>%
        filter(Club == input$clubid2)
    }
    else if (input$rad2 == "count2"){
      x <- fifa_data %>%
        filter(Nationality == input$country2)}
  })
  #rendering UI to print the photo of the player in the first tab
  output$img <- renderUI({
    d <- event_data("plotly_click")
    if (is.null(d)) paste("Click on a Player bar to view the Photo" )
    else {
      res<-tab1()
      vars <- c(d[["x"]], d[["y"]])
      rowi<-res%>%filter(Name==vars[1])
      tags$img(src = rowi$Photo, height = "75", width = "75")
      
    }
  })
  #print the basic details of the player below the photo from above
  output$click <- renderPrint({
    res<-tab1()
    d <- event_data("plotly_click")
    if (is.null(d)) paste("Click on a Player bar to view the name" )
    else {var <- c(d[["x"]], d[["y"]])
    res<-res%>%filter(var[1]==Name)
    cat(" Name:",paste(res$Name),"\n","Nationality:",paste(res$Nationality),
        "\n","Club:",paste(res$Club),"\n","Age:",paste(res$Age),
        "\n","Height:",paste(res$Height),"\n","Weight:",paste(res$Weight),
        "\n","Overall:",paste(res$Overall),"\n","Potential:",paste(res$Potential)
    )}
    
  })
  # plotting the bar graph in the first tab
  plotHist = function(x,y){
    output$plot1 <- renderPlotly({plot_ly(x=x, y=y, type = "bar") %>%
        layout(title = "Stats: ",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
      
    })
  }
  #observing if the event happens, then calling the plothist method
  observeEvent(input$Pot, {res<-tab1()
  res<-res%>%arrange(desc(Overall))%>%
    top_n(30,wt = Overall)
  plotHist(res$Name,res$Potential)})
  #if overall clicked
  observeEvent(input$Over, {res<-tab1()
  res<-res%>%arrange(desc(Overall))%>%
    top_n(30,wt = Overall)
  plotHist(res$Name, res$Overall)})
  #if age clicked
  observeEvent(input$age, {res<-tab1()
  res<-res%>%arrange(desc(Overall))%>%
    top_n(30,wt = Overall)
  plotHist(res$Name, res$Age)})
  #if weight clicked
  observeEvent(input$weight, {res<-tab1()
  res<-res%>%arrange(desc(Overall))%>%
    top_n(30,wt = Overall)
  plotHist(res$Name, res$Weight)})
  #if height clicked
  observeEvent(input$height, {res<-tab1()
  res<-res%>%arrange(desc(Overall))%>%
    top_n(30,wt = Overall)
  plotHist(res$Name, res$Height)})
  
  -----------------------------------------------------------------------------------------------------------
  # to plot the density globe on the 4th tab
  dfg<-as.data.frame(table(fifa_data['Nationality']))
  dfg['code']=countrycode(dfg$Var1, 'country.name', 'iso3c')
  dfg['id']=c(0,1:161)
  l <- list(color = toRGB("black"), width = 0.5)
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic')
  )
  output$map<- renderPlotly({plot_geo(dfg) %>%
      add_trace(
        z = ~Freq, color = ~Freq, colors = 'Blues',
        text = ~Var1, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Number of players', tickprefix = '') %>%
      layout(
        geo = g
      )})
  
  # for displaying data table when clicked on the country on the density globe.
  output$click2 <- renderDataTable({
    e <- event_data("plotly_click")
    if (is.null(e)) paste("Click on a Player bar to view the name" )
    else {vars<-c(e)
    rom<-dfg%>% filter(vars[2]==id)
    rex<-fifa_data%>% filter(Nationality == rom$Var1)
    rex %>% mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
      select(Photo,Name,Age,Overall,Club,Nationality,Potential,Position,Value,Wage,Height,Weight)%>%
      datatable(class = "nowrap hover row-border", escape = FALSE, 
                options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
  })    
  
  #for rendering the violin plot on the fifth tab  
  output$violin<-renderPlotly({
    p <- fifa_data %>%
      plot_ly(
        x = ~Position_general,
        y = ~Potential,
        split = ~Position_general,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) %>% 
      layout(
        xaxis = list(
          title = "Different Positions"
        ),
        yaxis = list(
          title = "Potential",
          zeroline = F
        )
      )})
  
  #to display the top 10 best players at the particular position when clicked
  output$clickviolin<- renderDataTable({
    ez <- event_data("plotly_click")
    if (is.null(ez)) paste("Click on either of the points" )
    else {vars<-c(ez)
    if (vars$x[1]=="GK"){
      rom<-fifa_data%>%filter(vars$x[1]==Position_general)%>%
        arrange(desc(Overall),desc(GKHandling))%>%
        select(Photo,Name,Overall,GKHandling,Club,Nationality,
               Value,Wage,GKDiving,GKKicking,GKPositioning,GKReflexes)%>%
        top_n(10,wt = Overall) %>%
        mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        datatable(class = "nowrap hover row-border", escape = FALSE,
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    else{
      rom<-fifa_data%>%filter(vars$x[1]==Position_general)%>%
        arrange(desc(Overall))%>%
        select(Photo,Name,Overall,Club,Nationality,
               Value,Wage,Crossing,Finishing,Aggression,Penalties,Vision,Positioning,Marking)%>%
        top_n(10,wt = Overall) %>%
        mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        datatable(class = "nowrap hover row-border", escape = FALSE,
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    
    }
  })
  
  #for the dumbbell plot in the third tab, 2nd subtab.
  plotbone=function(dataset){
    
    a<-table(dataset$Position_general,dataset$Preferred.Foot)
    a<-as.data.frame.matrix(a)
    position<-c('GK','left_def','right_def','center_def','center_mid','left_mid','right_mid','striker','left_fwd','right_fwd','center_fwd')
    data <- data.frame(position, a['Left'], a['Right'])
    data['Gap']=a['Right']-a['Left']
    
    output$plot3<-renderPlotly({plot_ly(data, color = I("gray80")) %>%
        add_segments(x = ~Left, xend = ~Right, y = ~position, yend = ~position, showlegend = FALSE) %>%
        add_markers(x = ~Left, y = ~position, name = "Left", color = I("pink")) %>%
        add_markers(x = ~Right, y = ~position, name = "Right", color = I("blue")) %>%
        layout(
          title = "Left VS Right",
          xaxis = list(title = "Number of players"),
          margin = list(l = 65)
        )})}
  
  #for rendering data table when clicked on the violin plot of the respective position
  output$table <- renderDataTable({
    ez <- event_data("plotly_click")
    if (is.null(ez)) paste("Click on either of the points" )
    else {vars<-c(ez)
    if (vars[1]==1){
      re<-tab3_1()
      rowix<-re%>%filter(Position_general==vars[4],Preferred.Foot=="Left")
      rowix%>%mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        select(Photo,Name,Age,Overall,Club,Nationality,Potential,Position,Value,Wage,Height,Weight)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    else if (vars[1]==2){
      re<-tab3_1()
      rowix<-re%>%filter(Position_general==vars[4],Preferred.Foot=="Right")
      rowix%>%mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        select(Photo,Name,Age,Overall,Club,Nationality,Potential,Position,Value,Wage,Height,Weight)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    }
  })
  #observing event, if the country name or club name is inputted by the user, and then upon calling the 
  #plotbone to render the dumbbell chart
  observeEvent(input$clubid2, {r<-tab3_1()
  plotbone(r)})
  observeEvent(input$country2, {re<-tab3_1()
  plotbone(re)})
  
  
  
  #for plotting pie chart in the third tab
  plotpie = function(dataset){
    f=table(dataset$Preferred.Foot)
    perc = (f/sum(f))*100
    dataset=as.data.frame(perc)
    output$plot2 <- renderPlotly({plot_ly(data=dataset, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Proportion of Left and Right footed players ',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  }
  
  # observing event, to see if the user enters the country or the club using the drop box, and there upon calling
  # the plotpie function to plot the pie chart.
  observeEvent(input$clubid1, {resi<-tab3()
  plotpie(resi)})
  observeEvent(input$country1, {resi<-tab3()
  plotpie(resi)})
  
  #rendering the datatable, when the user clicks on the pie chart in the third tab
  output$click1 <- renderDataTable({
    e <- event_data("plotly_click")
    resik<-tab3()
    if (is.null(e)) paste("Click on a Player bar to view the name" )
    else {vars<-c(e)
    if (vars[2]==0){
      tabss<-resik%>%filter(Preferred.Foot=="Left")
      tabss%>%mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        select(Photo,Name,Age,Overall,Potential,Preferred.Foot,Value,Wage,Height,Weight)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    else{
      tabss<-resik%>%filter(Preferred.Foot=="Right")
      tabss%>%mutate(Photo = paste0('<img src="', `Photo`, '"></img>'))%>%
        select(Photo,Name,Age,Overall,Potential,Preferred.Foot,Value,Wage,Height,Weight)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    }
  })
  # function defined to plot the correlation matrix
  plot_corr <- function(dat_frame){
    cr_soccer <- abs(cor(dat_frame, method = c("pearson"), use="complete.obs")) # Calculate Pearson's Correlation Coefficient
    output$heatmap <- renderD3heatmap({ d3heatmap(if (input$scale) scale(cr_soccer) else cr_soccer , revC=FALSE, scale="column",
                                                  theme="", k_row = 3, k_col = 3, colors = input$palette,
                                                  dendrogram = if (input$cluster) "both" else "none", show_grid = TRUE)
    })}
  
  #defining the attributes, to use for plotting the matrix
  def <- fifa_data[c(38,37,43:45)] #5
  keeper <-fifa_data[c(15,16,17,18,19)]#5
  mid<-fifa_data[c(24,39,28,40,26,25,22,20,36,23)]#10
  strik<-fifa_data[c(31,24,39,28,30,29,20)]#7
  final<-fifa_data[c(38,37,43,44,45,15,16,17,18,19,24,39,28,40,26,25,22,20,36,23,29,30,31)]
  
  #observing event, that if the user clicks on the respective action buttons. 
  observeEvent(input$dirty, {plot_corr(final)})
  observeEvent(input$striker, {plot_corr(strik)})
  observeEvent(input$keeper, {plot_corr(keeper)})
  observeEvent(input$mid, {plot_corr(mid)})
  observeEvent(input$def, {plot_corr(def)})
}

shinyApp(ui = ui, server = server)