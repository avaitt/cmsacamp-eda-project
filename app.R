library(shiny)

keys <- c("OAK", "LAD", "KC",  "MIA", "STL", "COL", "ARI", "SEA", "NYY", "MIN", "TB",  
          "SD",  "TEX", "BOS" ,"CWS" ,"SF"  ,"ATL", "MIL" ,"PHI", "PIT", "DET", 
          "CHC", "NYM", "CIN" ,"BAL", "WSH", "CLE", "HOU","LAA")
values <- c("athletics","dodgers", "royals","marlins","cardinals","rockies","diamondbacks","mariners","yankees",
            "twins","rays","padres","rangers","red_sox", "white_sox","giants","braves","brewers",
            "phillies","pirates","tigers", "cubs", "mets","reds","orioles","nationals","indians","astros",
            "angels")
names(values) <- keys
my_list <- as.list(values)


values2 <- c("Philadelphia Phillies", "Oakland Athletics", "Tampa Bay Rays", "Miami Marlins",
             "Washington Nationals", "San Francisco Giants", "New York Yankees", "San Diego Padres",
             "New York Mets","Seattle Mariners","Boston Red Sox","Houston Astros","Baltimore Orioles",
             "Los Angeles Angels", "St. Louis Cardinals","Cincinnati Reds","Los Angeles Dodgers",
             "Texas Rangers","Detroit Tigers","Chicago Cubs","Chicago White Sox","Milwaukee Brewers","Cleveland Indians",
             "Pittsburgh Pirates","Minnesota Twins","Kansas City Royals","Atlanta Braves","Arizona Diamondbacks","Colorado Rockies")


keys2 <- c("PHI", "OAK", "TB", "MIA", "WSH", "SF", "NYY", "SD", "NYM", "SEA", "BOS",
           "HOU", "BAL", "LAA", "STL", "CIN", "LAD", "TEX", "DET", "CHC",
           "CWS", "MIL", "CLE", "PIT", "MIN", "KC", "ATL", "ARI", "COL")

stadiums <- c("Citizens Bank Park", "Oakland Coliseum", "Tropicana Field", "Marlins Park", "Nationals Park", "Oracle Park", 
             "Yankee Stadium", "Petco Park", "Citi Field", "T-Mobile Park", "Fenway Park", "Minute Maid Park", "Oriole Park at Camden Yards",
             "Angel Stadium", "Busch Stadium", "Great American Ball Park", "Dodger Stadium", 
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
    
      test_data <- mlb_data %>% filter(home_team == my_list2[[input$Box1]], effective_speed != 0)
      
      batted_ball_data <- copy(test_data)
      # unique(batted_ball_data$game_date)
      dates <- length(unique(batted_ball_data$game_date))
      
      batted_ball_data$team <- rep(c(my_list[[batted_ball_data$home_team[1]]]), each=dim(batted_ball_data)[1])
      
      # batted_ball_data$row_team <- factor(batted_ball_data$row_team, levels = c("COL", "ARI"))
    
      if (input$Box2 == 'Hit Distance'){
        batted_ball_data %>% mlbam_xy_transformation() %>% 
          ggplot(aes(x = hc_x_, y = hc_y_, z = hit_distance_sc, group = -1)) +
          stat_summary_hex(color="gray", na.rm =TRUE, fun=mean, alpha = 0.70) + #<
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = input$Box2) +
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
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = input$Box2) +
          geom_mlb_stadium(stadium_ids = unique(batted_ball_data$team), 
                           stadium_transform_coords = TRUE, stadium_segments = "all", na.rm=TRUE) +
          theme_void() + 
          theme(legend.title = element_text(size=16), 
                legend.text = element_text(size=15)) + 
          coord_fixed()
        
      }
      
      else if (input$Box2 == 'Launch Speed'){
        batted_ball_data %>% mlbam_xy_transformation() %>% 
          ggplot(aes(x = hc_x_, y = hc_y_, z = launch_speed, group = -1)) + 
          stat_summary_hex(color="gray", na.rm =TRUE, fun=mean, alpha = 0.70) + #<
          scale_fill_distiller(palette = "YlGnBu", direction=1, name = input$Box2) +
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
  
  
  output$graph <- renderPlot({
    input$do
    isolate(data3())
    
  }, height = 600)
  
  # c("team", "altitude_rankings", "hit_distance", "effective_speed", "bases_covered", 
  #   "home_runs", "triples", "doubles", "singles")
  
  output$park_description <- renderText({
    input$do
    isolate(paste("<B>",input$Box1, " Falls in The:</B>"))
  })

  output$altitude <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$altitude[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$altitude)(team_data) * 100)
    isolate(paste(toOrdinal(floor(percentile)), " Percentile for Altitude"))
  })
  
  output$outfield <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$outfield_height[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$outfield_height)(team_data) * 100)
    isolate(paste(toOrdinal(floor(percentile)), " Percentile for Outfield Wall Height"))
  })
  
  output$hit_distance <- renderText({ 
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
  
  output$effective_speed <- renderText({ 
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
  
  output$bases_covered <- renderText({ 
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
  
  output$home_runs <- renderText({ 
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
  
  output$triples <- renderText({ 
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
  
  output$doubles <- renderText({ 
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
  
  output$singles <- renderText({ 
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
  
  output$launch_speed <- renderText({ 
    input$do
    team_data <- isolate(percentiles_df$launch_speed[percentiles_df$team == my_list2[[input$Box1]]])
    percentile <- isolate(ecdf(percentiles_df$launch_speed)(team_data) * 100)
    if (isolate(input$Box2 == 'Launch Speed')){
      isolate(paste("<B>", toOrdinal(floor(percentile)), " Percentile for Launch Speed</B>"))
    }
    else {
      isolate(paste(toOrdinal(floor(percentile)), " Percentile for Launch Speed"))
    }
    
  })
  
})



ui <-shinyUI(fluidPage(
  headerPanel(title = '', windowTitle = "Variability in Baseball Stadiums"), 
  titlePanel(h2('How Does Variability In Baseball Stadiums Impact Batting Statistics?', align = "center")),

  sidebarPanel(
    # tags$head(
    #   tags$style(type="text/css", "select { max-width: 140px; }"),
    #   tags$style(type="text/css", ".span4 { max-width: 190px; }"),
    #   tags$style(type="text/css", ".well { max-width: 295px; }")
    # ),
    selectInput("Box1","Select Stadium", choices = stadiums, selected = "Fenway Park"),
    selectInput("Box2","Select Metric To Visualize", choices = c("Hit Distance", "Effective Speed", "Launch Speed", "Bases Covered")),
    #actionButton("do", "Submit"),
    actionButton("do", "Submit", icon("paper-plane"), 
                 style="color: #fff; background-color: green; border-color: #2e6da4"),
    br(),
    br(),
    HTML(paste0("<b>", "Hit Distance:","</b>", " distance of the batted ball (ft)")),
    br(),
    br(),
    HTML(paste0("<b>", "Effective Speed:","</b>", " velocity of pitch adjusted for closeness to home plate (mph)")),
    br(),
    br(),
    HTML(paste0("<b>", "Launch Speed:","</b>", " exit velocity of the ball off the bat (mph)")), 
    br(),
    br(),
    em(HTML(paste0("<b>", "NOTE:","</b>", " 2021 data on Rogers Centre (home to the Toronto Blue Jays) 
                does not exist due to COVID-19"))), 
    width=2),
  
  mainPanel(
    fluidRow(
      column(8,
             plotOutput("graph"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             strong(h4(em(htmlOutput("header"))), align = "right")),
      column(4, style=list("padding-left: 7px;"),
             br(),
             h4(strong(htmlOutput("park_description")), style = "font-size:22px;"),
             br(),
             h4(htmlOutput("altitude")),
             br(),
             h4(htmlOutput("outfield")),
             br(),
             h4(htmlOutput("hit_distance")),
             br(),
             h4(htmlOutput("effective_speed")),
             br(),
             h4(htmlOutput("launch_speed")),
             br(),
             h4(htmlOutput("bases_covered")),
             tags$style(HTML("
                    li {
                    font-size: 15px;

                    }
                    ul {
                    list-style-type: square;
                    }

                    ")),
             tags$div(tags$ul(
               tags$li(h4(em(htmlOutput("home_runs")))),
               tags$li(h4(em(htmlOutput("triples")))),
               tags$li(h4(em(htmlOutput("doubles")))), 
               tags$li(h4(em(htmlOutput("singles")))), style = "font-size: 30px")),
             br())
  ))))

shinyApp(ui,server)
