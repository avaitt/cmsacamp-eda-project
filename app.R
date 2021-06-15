library(shiny)

keys <- c("OAK", "LAD", "KC",  "MIA", "STL", "COL", "ARI", "SEA", "NYY", "MIN", "TB",  
          "SD",  "TEX", "BOS" ,"CWS" ,"SF"  ,"ATL", "TOR", "MIL" ,"PHI", "PIT", "DET", 
          "CHC", "NYM", "CIN" ,"BAL", "WSH", "CLE", "HOU","LAA")
values <- c("athletics","dodgers", "royals","marlins","cardinals","rockies","diamondbacks","mariners","yankees","twins","rays","padres","rangers","red sox",
            "white sox","giants","braves","blue jays","brewers","phillies","pirates","tigers","cubs","mets","reds","orioles","nationals","indians","astros",
            "angels")
names(values) <- keys
my_list <- as.list(values)
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
valid_data <- read.csv('valid_data.csv')

library(shiny)
shhh(library(dplyr))
shhh(library(GeomMLBStadiums))
shhh(library(ggplot2))
shhh(library(data.table))

y_list = list()
for (i in 1:30) {
  val <- valid_data %>% filter(row_team == keys[i], !(away_team == keys[i])) %>% select(away_team) %>% distinct(away_team)
  y_list[[i]] <- val$away_team
}

x_list = list()
for (i in 1:30) {
  x_list[[i]] <- c(rep(keys[i], length(y_list[[i]])))
}
  
x <- unlist(x_list)
y <- unlist(y_list)
#bind x and y into a dataframe with two columns, using cbind() (column bind)
l <- NULL
l <- as.data.frame(cbind(x,y))
colnames(l) <- c("home","away")


server <- shinyServer(function(input,output, session){
  
  data1 <- reactive({
    if(input$Box1 == "All"){
      l
    }else{
      l[which(l$home == input$Box1),]
    }
  })
  
  data2 <- reactive({
    if (input$Box2 == "All"){
      l
    }else{
      l[which(l$away == input$Box2),]
    }
  })
  
  observe({
    
    if(input$Box1 != "All"){
      updateSelectInput(session,"Box2","Away Team", choices = c(unique(data1()$away)))
    }
    
    else if(input$Box2 != 'All'){
      updateSelectInput(session,"Box1","Home Team", choices = c(unique(data2()$home)))
    }
    
    else if (input$Box1 == "All" & input$Box2 == "All"){
      updateSelectInput(session,"Box2","Away Team", choices = c(unique(l$away)))
      updateSelectInput(session,"Box1","Home Team", choices = c(unique(l$home)))
    }
  })
  
  

  data3 <- reactive({
    if(input$Box2 == "All"){
      data1()
    }else if (input$Box1 == "All"){
      data2()
    }else if (input$Box2 == "All" & input$Box1 == "All"){
      l
    }
    else{
      test_data <- valid_data %>% filter(row_team %in% c(input$Box1, input$Box2), home_team == input$Box1, away_team == input$Box2)
      batted_ball_data <- copy(test_data)
      dates <- length(unique(batted_ball_data$game_date))
      if (dates > 2){
        col_val = ceiling(dates / 2)
        row_val = 2
      } else {
        col_val = 2
        row_val = 1
      }
      batted_ball_data$team <- rep(c(my_list[[batted_ball_data$home_team[1]]]), each=dim(batted_ball_data)[1])
      batted_ball_data$row_team <- factor(batted_ball_data$row_team, levels = c(input$Box1, input$Box2))

      batted_ball_data %>% mlbam_xy_transformation() %>%  
        ggplot(aes(x=hc_x_, y=hc_y_, color=row_team)) + 
        geom_spraychart(stadium_ids = unique(batted_ball_data$team),
                        stadium_transform_coords = TRUE, 
                        stadium_segments = "all", na.rm = TRUE, size = 3) + 
        facet_wrap(~ game_date, ncol = col_val, nrow = row_val) + 
        theme_void() + 
        theme(strip.text.x = element_text(size = 16),   
              legend.title = element_text(size=15), 
              legend.text = element_text(size=15)) +  
        coord_fixed() + 
        labs(color = "Team")
      
    }
  })
  
  
  output$table1 <- renderPlot({
    input$do
    isolate(data3())

  }, height = 700, width = 1100 )
  
})



ui <-shinyUI(fluidPage(
  titlePanel(h4('Test Title', align = "center")),
  
  sidebarPanel(
    selectInput("Box1","Home Team", choices = c(unique(l$home))),
    selectInput("Box2","Away Team", choices = c(unique(l$away))),
    actionButton("do", "Submit"), width=2),
  
  mainPanel(
    br(),
    plotOutput("table1")
  )))

shinyApp(ui,server)
