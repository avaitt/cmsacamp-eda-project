library(shiny)

keys <- c("OAK", "LAD", "KC",  "MIA", "STL", "COL", "ARI", "SEA", "NYY", "MIN", "TB",  
          "SD",  "TEX", "BOS" ,"CWS" ,"SF"  ,"ATL", "TOR", "MIL" ,"PHI", "PIT", "DET", 
          "CHC", "NYM", "CIN" ,"BAL", "WSH", "CLE", "HOU","LAA")
values <- c("athletics","dodgers", "royals","marlins","cardinals","rockies","diamondbacks","mariners","yankees",
            "twins","rays","padres","rangers","red_sox", "white_sox","giants","braves","blue jays","brewers",
            "phillies","pirates","tigers", "cubs", "mets","reds","orioles","nationals","indians","astros",
            "angels")
names(values) <- keys
my_list <- as.list(values)


values2 <- c("Philadelphia Phillies", "Oakland Athletics", "Tampa Bay Rays", "Miami Marlins",
             "Washington Nationals", "San Francisco Giants", "New York Yankees", "San Diego Padres",
             "New York Mets","Seattle Mariners","Boston Red Sox","Houston Astros","Baltimore Orioles",
             "Los Angeles Angels", "Toronto Blue Jays", "St. Louis Cardinals","Cincinnati Reds","Los Angeles Dodgers",
             "Texas Rangers","Detroit Tigers","Chicago Cubs","Chicago White Sox","Milwaukee Brewers","Cleveland Indians",
             "Pittsburgh Pirates","Minnesota Twins","Kansas City Royals","Atlanta Braves","Arizona Diamondbacks","Colorado Rockies")


keys2 <- c("PHI", "OAK", "TB", "MIA", "WSH", "SF", "NYY", "SD", "NYM", "SEA", "BOS",
           "HOU", "BAL", "LAA", "TOR", "STL", "CIN", "LAD", "TEX", "DET", "CHC",
           "CWS", "MIL", "CLE", "PIT", "MIN", "KC", "ATL", "ARI", "COL")

stadiums <- c("Citizens Bank Park", "Oakland Coliseum", "Tropicana Field", "Marlins Park", "Nationals Park", "Oracle Park", 
             "Yankee Stadium", "Petco Park", "Citi Field", "T-Mobile Park", "Fenway Park", "Minute Maid Park", "Oriole Park at Camden Yards",
             "Angel Stadium", "Rogers Centre", "Busch Stadium", "Great American Ball Park", "Dodger Stadium", 
             "Globe Life Field", "Comerica Park", "Wrigley Field", "Guaranteed Rate Field", "Miller Park", "Progressive Field", "PNC Park", 
             "Target Field", "Kauffman Stadium", "Truist Park", "Chase Field", "Coors Field")

names(keys2) <- stadiums
my_list2 <- as.list(keys2)


names(values2) <- stadiums
my_list3 <- as.list(values2)





shhh <- suppressPackageStartupMessages 
mlb_data <- readRDS("mlb_data.rds")
percentiles_df <- readRDS("percentiles_df.rds")


# metric <- c("Launch Angle", 'Hit Distance', "Effective Speed", "Launch Speed")
# vals <- c("launch_angle", 'hit_distance_sc', "effective_speed", "launch_speed")
# names(vals) <- metric
# my_list2 <- as.list(vals)

library(shiny)
shhh(library(dplyr))
shhh(library(GeomMLBStadiums))
shhh(library(ggplot2))
shhh(library(data.table))
shhh(library(toOrdinal))
set.seed(100)

server <- shinyServer(function(input,output, session){
  
  data3 <- reactive({
    
      test_data <- mlb_data %>% filter(home_team == my_list2[[input$Box1]])
      
      batted_ball_data <- copy(test_data)
      # unique(batted_ball_data$game_date)
      dates <- length(unique(batted_ball_data$game_date))
      
      batted_ball_data$team <- rep(c(my_list[[batted_ball_data$home_team[1]]]), each=dim(batted_ball_data)[1])
      
      # batted_ball_data$row_team <- factor(batted_ball_data$row_team, levels = c("COL", "ARI"))
    
      if (input$Box2 == 'Hit Distance'){
        batted_ball_data %>% mlbam_xy_transformation() %>% 
          ggplot(aes(x = hc_x_, y = hc_y_, z = hit_distance_sc, group = -1)) +
          stat_summary_hex(color="gray", na.rm =TRUE, fun=mean, alpha = 0.70) + #<
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = "Hit Distance (ft.)") +
          geom_mlb_stadium(stadium_ids = unique(batted_ball_data$team), 
                           stadium_transform_coords = TRUE, stadium_segments = "all", na.rm=TRUE) +
          theme_void() + 
          theme(legend.title = element_text(size=16), 
                legend.text = element_text(size=15)) + 
          coord_fixed()
      }
      
      else if (input$Box2 == 'Effective Speed'){
        batted_ball_data %>% mlbam_xy_transformation() %>% 
          ggplot(aes(x = hc_x_, y = hc_y_, z = effective_speed, group = -1)) + 
          stat_summary_hex(color="gray", na.rm =TRUE, fun=mean, alpha = 0.70) + #<
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = "Effective Speed (mph)") +
          geom_mlb_stadium(stadium_ids = unique(batted_ball_data$team), 
                           stadium_transform_coords = TRUE, stadium_segments = "all", na.rm=TRUE) +
          theme_void() + 
          theme(legend.title = element_text(size=16), 
                legend.text = element_text(size=15)) + 
          coord_fixed()
        
      }
      
      else if (input$Box2 == 'Bases Covered') {
        batted_ball_data %>% mlbam_xy_transformation() %>% 
          ggplot(aes(x = hc_x_, y = hc_y_, z = bases_covered, group = -1)) + 
          
          stat_summary_hex(color="gray", na.rm =TRUE, fun=mean, alpha = 0.70) + #<
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = input$Box2, 
                               labels = c("", "single", "double", "triple", "home run")) +
          geom_mlb_stadium(stadium_ids = unique(batted_ball_data$team), 
                           stadium_transform_coords = TRUE, stadium_segments = "all", na.rm=TRUE) +
          theme_void() + 
          theme(legend.title = element_text(size=16), 
                legend.text = element_text(size=15)) + 
          coord_fixed()
      }
    
      
    })
  
  output$header <- renderText({
    input$do
    isolate(paste("<B>Home Field of the ", my_list3[[input$Box1]], "</B>"))
  })
  
  
  output$table1 <- renderPlot({
    input$do
    isolate(data3())
    
  }, height = 600)
  
  # c("team", "altitude_rankings", "hit_distance", "effective_speed", "bases_covered", 
  #   "home_runs", "triples", "doubles", "singles")
  
  output$text <- renderText({
    input$do
    isolate(paste("<B>",input$Box1, " Falls in the:</B>"))
  })

  output$text1 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$altitude_ranking[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$altitude_ranking)(team_data) * 100)
    isolate(paste(toOrdinal(floor(percentile)), " Percentile for Altitude"))
  })
  
  output$text_new <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$outfield_walls[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$outfield_walls)(team_data) * 100)
    isolate(paste(toOrdinal(floor(percentile)), " Percentile for Outfield Wall Height"))
  })
  
  output$text2 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$hit_distance[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$hit_distance)(team_data) * 100)
    if (isolate(input$Box2 == 'Hit Distance')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Hit Distance</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Hit Distance"))
    }
  })
  
  output$text3 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$effective_speed[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$effective_speed)(team_data) * 100)
    if (isolate(input$Box2 == 'Effective Speed')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Effective Speed</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Effective Speed"))
    }
  })
  
  output$text4 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$bases_covered[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$bases_covered)(team_data) * 100)
    if (isolate(input$Box2 == 'Bases Covered')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Bases Covered</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Bases Covered"))
    }
  
  })
  
  output$text5 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$home_runs[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$home_runs)(team_data) * 100)
    if (isolate(input$Box2 == 'Bases Covered')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Home Runs</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Home Runs"))
    }
    
  })
  
  output$text6 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$triples[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$triples)(team_data) * 100)
    if (isolate(input$Box2 == 'Bases Covered')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Triples</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Triples"))
    }
    
  })
  
  output$text7 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$doubles[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$doubles)(team_data) * 100)
    if (isolate(input$Box2 == 'Bases Covered')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Doubles</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Doubles"))
    }
    
  })
  
  output$text8 <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$singles[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$singles)(team_data) * 100)
    if (isolate(input$Box2 == 'Bases Covered')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Singles</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Singles"))
    }
    
  })
  
})

ui <-shinyUI(fluidPage(
  titlePanel(h2('How Does Variability In Baseball Stadiums Impact Batting Statistics?', align = "center")),
  
  sidebarPanel(
    selectInput("Box1","Select Stadium", choices = stadiums, selected = "Coors Field"),
    selectInput("Box2","Select Metric To Visualize", choices = c("Hit Distance", "Effective Speed", "Bases Covered")),
    actionButton("do", "Submit"), width=2),
  
  mainPanel(
    br(),
    fluidRow(
      column(8,
             plotOutput("table1"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             strong(h4(em(htmlOutput("header"))), align = "right")),
      column(4, style=list("padding-left: 40px;"),
             br(),
             br(),
             br(),
             h3(strong(htmlOutput("text"))),
             br(),
             h4(htmlOutput("text1")),
             br(),
             h4(htmlOutput("text_new")),
             br(),
             h4(htmlOutput("text2")),
             br(),
             h4(htmlOutput("text3")),
             br(),
             h4(htmlOutput("text4")),
             tags$style(HTML("
                    li {
                    font-size: 15px;

                    }
                    ul {
                    list-style-type: square;
                    }

                    ")),
             tags$div(tags$ul(
               tags$li(h4(em(htmlOutput("text5")))),
               tags$li(h4(em(htmlOutput("text6")))),
               tags$li(h4(em(htmlOutput("text7")))), 
               tags$li(h4(em(htmlOutput("text8")))), style = "font-size: 30px")),
             br())
  ))))

shinyApp(ui,server)
