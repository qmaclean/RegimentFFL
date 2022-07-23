#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(gt)
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(shinythemes)
library(rsconnect)
#library(caret)
#library(lme4)
#library(merTools)
library(ggpmisc)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggsci)
#library(googledrive)
library(curl)
#library(arrow)
library(ggrepel)
library(RCurl)
library(ggimage)




##### Data load ####
draft<-read.csv(url("https://raw.githubusercontent.com/qmaclean/RegimentFFL/main/Data/History%20of%20Regiment%20-%20Draft_History.csv"))
lineups<-read.csv(url("https://raw.githubusercontent.com/qmaclean/RegimentFFL/main/Data/History%20of%20Regiment%20-%20Weekly_Historical_Lineups.csv"))


roster<-load_rosters(seasons = c(2014:2021))



draft<-draft %>%
  dplyr::filter(Drafted.By. %in% c("Vandy","Brolson","Miles","Chet","Huy","Riley",
                                     "Zach","Ian","Quinn","Halladay",
                                     "Charles","Coley","Colin","Eric",
                                     "Jesse","Justin","Patrick","Taylor")) %>%
  mutate(img = case_when(
    Drafted.By. == "Vandy" ~ "https://media.socastsrm.com/wordpress/wp-content/blogs.dir/1411/files/2019/03/vandy-logo.jpg",
    Drafted.By. == "Brolson" ~ "https://1000logos.net/wp-content/uploads/2021/07/Wisconsin-Badgers-logo.png",
    Drafted.By. == "Miles" ~ "https://media-blog.zutobi.com/wp-content/uploads/sites/3/2021/09/28173051/three-point-turn-1-scaled.jpg",
    Drafted.By. == "Chet" ~ "https://bringmethenews.com/.image/ar_1:1%2Cc_fill%2Ccs_srgb%2Cq_auto:good%2Cw_1200/MTgwNDMwMTMzNTMyODI4ODIy/screen-shot-2021-04-19-at-83649-am.png",
    Drafted.By. == "Huy" ~ "https://static.wikia.nocookie.net/sportsmascots/images/e/eb/Blaze.jpg",
    Drafted.By. == "Riley" ~ "https://pbs.twimg.com/profile_images/864178054373023745/ItQQsPEa_400x400.jpg",
    Drafted.By. == "Zach" ~ "https://www.gannett-cdn.com/presto/2021/10/15/NREG/54c0cf53-da79-4b87-a30f-94c7e014b608-uomarchingband1.jpg",
    Drafted.By. == "Ian" ~ "https://www.hollywoodreporter.com/wp-content/uploads/2017/04/socal_-h_2017.jpg",
    Drafted.By. == "Quinn" ~ "https://waydev.co/wp-content/uploads/2020/01/moneyball-git-analytics.jpg",
    Drafted.By. == "Halladay" ~ "https://imperial-yeast.imgix.net/assets/2019/08/16/5d56f550eef5bImperial-Organic-Yeast-Logo.png",
    Drafted.By. == "Charles" ~ "https://upload.wikimedia.org/wikipedia/commons/f/fb/2017-0717-Big12MD-KliffKingsbury.jpg",
    Drafted.By. == "Coley" ~ "https://www.dancemusicnw.com/wp-content/uploads/2019/05/electric-forest.jpg",   
    Drafted.By. == "Colin" ~ "https://media.king5.com/assets/KING/images/5473bc93-d060-4c2d-9e15-0ed0201a2d52/5473bc93-d060-4c2d-9e15-0ed0201a2d52_1920x1080.jpg", 
    Drafted.By. == "Eric" ~ "https://img.etimg.com/thumb/msid-59738997,width-640,resizemode-4,imgsize-21421/nike.jpg", 
    Drafted.By. == "Jesse" ~ "https://m.media-amazon.com/images/M/MV5BYTZhOGQ3ZjUtYTUxZC00MTg5LWJmMGEtNzk2MjQzMjM4OWU0XkEyXkFqcGdeQXVyMzM3NjQ4OTI@._V1_FMjpg_UX1000_.jpg",  
    Drafted.By. == "Justin" ~ "https://1000logos.net/wp-content/uploads/2021/05/White-Claw-logo.png",
    Drafted.By. == "Patrick" ~ "https://www.commonsensemedia.org/sites/default/files/styles/social_share_image/public/screenshots/csm-movie/babe-ss1.jpg",
    Drafted.By. == "Taylor" ~ "https://sportsteamhistory.com/wp-content/uploads/2020/03/st_louis_rams_2000-2015.png",
   TRUE ~ as.character(NA)
  ),
  Starting_Points_pct = (Total.Points.Started / Total.Points),
  Start_pct = (Weeks.Started / Weeks.on.Roster)) %>%
  rename(Team = Drafted.By.
         ) %>%
  left_join(roster,by=c("Player" = "full_name","Year" = "season","Position" = "position"))


###### lineups
lineups<-lineups %>%
  mutate(img = case_when(
    ManagerMatch == "Vandy" ~ "https://media.socastsrm.com/wordpress/wp-content/blogs.dir/1411/files/2019/03/vandy-logo.jpg",
    ManagerMatch == "Brolson" ~ "https://1000logos.net/wp-content/uploads/2021/07/Wisconsin-Badgers-logo.png",
    ManagerMatch == "Miles" ~ "https://media-blog.zutobi.com/wp-content/uploads/sites/3/2021/09/28173051/three-point-turn-1-scaled.jpg",
    ManagerMatch == "Chet" ~ "https://bringmethenews.com/.image/ar_1:1%2Cc_fill%2Ccs_srgb%2Cq_auto:good%2Cw_1200/MTgwNDMwMTMzNTMyODI4ODIy/screen-shot-2021-04-19-at-83649-am.png",
    ManagerMatch == "Huy" ~ "https://static.wikia.nocookie.net/sportsmascots/images/e/eb/Blaze.jpg",
    ManagerMatch == "Riley" ~ "https://pbs.twimg.com/profile_images/864178054373023745/ItQQsPEa_400x400.jpg",
    ManagerMatch == "Zach" ~ "https://www.gannett-cdn.com/presto/2021/10/15/NREG/54c0cf53-da79-4b87-a30f-94c7e014b608-uomarchingband1.jpg",
    ManagerMatch == "Ian" ~ "https://www.hollywoodreporter.com/wp-content/uploads/2017/04/socal_-h_2017.jpg",
    ManagerMatch == "Quinn" ~ "https://waydev.co/wp-content/uploads/2020/01/moneyball-git-analytics.jpg",
    ManagerMatch == "Halladay" ~ "https://imperial-yeast.imgix.net/assets/2019/08/16/5d56f550eef5bImperial-Organic-Yeast-Logo.png",
    ManagerMatch == "Charles" ~ "https://upload.wikimedia.org/wikipedia/commons/f/fb/2017-0717-Big12MD-KliffKingsbury.jpg",
    ManagerMatch == "Coley" ~ "https://www.dancemusicnw.com/wp-content/uploads/2019/05/electric-forest.jpg",   
    ManagerMatch == "Colin" ~ "https://media.king5.com/assets/KING/images/5473bc93-d060-4c2d-9e15-0ed0201a2d52/5473bc93-d060-4c2d-9e15-0ed0201a2d52_1920x1080.jpg", 
    ManagerMatch == "Eric" ~ "https://img.etimg.com/thumb/msid-59738997,width-640,resizemode-4,imgsize-21421/nike.jpg", 
    ManagerMatch == "Jesse" ~ "https://m.media-amazon.com/images/M/MV5BYTZhOGQ3ZjUtYTUxZC00MTg5LWJmMGEtNzk2MjQzMjM4OWU0XkEyXkFqcGdeQXVyMzM3NjQ4OTI@._V1_FMjpg_UX1000_.jpg",  
    ManagerMatch == "Justin" ~ "https://1000logos.net/wp-content/uploads/2021/05/White-Claw-logo.png",
    ManagerMatch == "Patrick" ~ "https://www.commonsensemedia.org/sites/default/files/styles/social_share_image/public/screenshots/csm-movie/babe-ss1.jpg",
    ManagerMatch == "Taylor" ~ "https://sportsteamhistory.com/wp-content/uploads/2020/03/st_louis_rams_2000-2015.png",
    TRUE ~ as.character(NA)
  )) %>%
  left_join(roster,by=c("name" = "full_name","RegimentYear" = "season","position" = "position"))
  




#### subset 
tab1<-draft %>%
  dplyr::select(Year,Round,Pick..,Team,img,Player,headshot_url,team,Position,Keeper.,Total.Points,Total.Points.on.Roster,Total.Points.Started,
                Draft.Position.by.Position..,Position.Rank.by.End.of.Season.Delta,
                Draft.Position.to.End.of.Season.Rank)




mgr<-draft %>%
  dplyr::select(Team,img) %>%
  distinct()

draft<-draft %>%
  mutate(Position_f = factor(draft$Position,levels=c('RB','WR','QB','TE','D/ST','K')))




results<-lineups %>%
  dplyr::select(week,RegimentYear,ManagerMatch,Result,Opponent,Points,Opp.Points,Difference,img) %>%
  distinct() %>%
  left_join(mgr,by=c("Opponent" = "Team")) %>%
  rename(Team_img = "img.x",
         Opp_img = "img.y") %>%
  mutate(Points = as.numeric(Points),
         Opp.Points = as.numeric(Opp.Points),
         Difference = Points - Opp.Points) %>%
  dplyr::filter(complete.cases(Points))




#### To-Do
# Draft History -> Expand gt table
### be able sort or filter by rank improvement
### remove combo tab
keepers<-draft %>%
  dplyr::filter(Keeper. == "Yes") %>%
  dplyr::select(Round,Player,headshot_url,Position,Team,img,Year,Weeks.on.Roster,Weeks.Started,Total.Points,Total.Points.on.Roster,
                Total.Points.Started,Total.Position.Rank.on.Roster,
                Draft.Position.to.End.of.Season.Rank,Position.Rank.by.End.of.Season.Delta)



  



# Define UI for application that draws a histogram
ui <- shinyUI(
  
  #fluidPage(
  
  #theme = shinytheme("united"),
  # Application title
  #titlePanel("Regiment Fantasy Football History"),
  #tags$div("This site shows Regiment Analytics"),
  #br(),
  #uiOutput('Draft History'),
  # Sidebar with a slider input for number of bins 
  #sidebarLayout(
  navbarPage("Regiment Analytics",
                     tabPanel("Draft History",
                              fluidPage(
                                fluidRow(
                                  column(width = 3, wellPanel(
                                    shiny::selectizeInput(
                                      inputId = "Year",
                                      label = "Year:",
                                      choices = 2014:2021,
                                      selected = 2021,  #NULL
                                      multiple = TRUE
                                    ),
                                    shiny::selectizeInput(
                                      inputId = "Round",
                                      label = "Round (Draft History Only):",
                                      choices = 1:15,
                                      selected = 1,  #NULL
                                      multiple = TRUE
                                    ),
                                    shiny::selectizeInput(
                                      inputId = "Position",
                                      label = "Position:",
                                      choices = c("D/ST","K","QB","RB","TE","WR"),
                                      selected = c("D/ST","K","QB","RB","TE","WR"), #NULL
                                      multiple = TRUE#,
                                      #options = list(...)
                                    ),
                                    shiny::selectizeInput(
                                      inputId = "Team",
                                      label = "Managers:",
                                      choices = c("Vandy","Brolson","Miles","Chet","Huy","Riley",
                                                  "Zach","Ian","Quinn","Halladay",
                                                  "Charles","Coley","Colin","Eric",
                                                  "Jesse","Justin","Patrick","Taylor"),
                                      selected = c("Vandy","Brolson","Miles","Chet","Huy","Riley",
                                                   "Zach","Ian","Quinn","Halladay",
                                                   "Charles","Coley","Colin","Eric",
                                                   "Jesse","Justin","Patrick","Taylor"), #NULL
                                      multiple = TRUE#,
                                      #options = list(...)
                                    ),
                                  ))),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Draft History",
                                             column(12,gt_output("tbl"))),
                                    tabPanel("Draft Analyzer",
                                             fluidRow(
                                               splitLayout(#cellWidths = c("100%","100%"),
                                                           plotOutput("graph1"),
                                                           plotOutput("graph2"))
                                             )),
                                    tabPanel("Keepers",
                                             column(12,gt_output("tbl1")))
                                  
                                ))
                                #fluidRow(
                                #  column(width = 12,
                                #         gt_output("tbl")
                                #  )  ,
                                #  column(width = 12,
                                #         plotOutput("graph2", width=900,height = 450)
                                  #)
                                #)
                                  )
                     ),
             tabPanel("Team Performance",
                      fluidPage(
                        fluidRow(
                          column(width = 3, wellPanel(
                            # Sidebar with  slider input
                            #sidebarPanel(h4("Baseline"),
                            shiny::selectizeInput(
                              inputId = "Year1",
                              label = "Year:",
                              choices = 2014:2021,
                              selected = 2021,  #NULL
                              multiple = TRUE
                            ),
                            
                            #sliderInput(
                            #   inputId =  "Year",
                            #   label = "Year:",
                            #   min = min, max = max,
                            #   value = 2021
                            # ),
                            
                            shiny::selectizeInput(
                              inputId = "week",
                              label = "Week:",
                              choices = 1:17,
                              selected = 1, #NULL
                              multiple = TRUE
                            ),
                            shiny::selectizeInput(
                              inputId = "Team1",
                              label = "Managers:",
                              choices = c("Vandy","Brolson","Miles","Chet","Huy","Riley",
                                          "Zach","Ian","Quinn","Halladay",
                                          "Charles","Coley","Colin","Eric",
                                          "Jesse","Justin","Patrick","Taylor"),
                              selected = c("Vandy","Brolson","Miles","Chet","Huy","Riley",
                                           "Zach","Ian","Quinn","Halladay",
                                           "Charles","Coley","Colin","Eric",
                                           "Jesse","Justin","Patrick","Taylor"), #NULL
                              multiple = TRUE#,
                              #options = list(...)
                            )
                          )         
                          )),
                        
                      
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Weekly Team Performance Summary",
                                     gt_output("tbl2")
                            ),
                            tabPanel("Team Performance Analytics",
                                     fluidRow(
                                       splitLayout(
                                                   plotOutput("graph5"),
                                                   plotOutput("graph6")
                                     )))
                          ))
                        
                      )),
                     tabPanel("Player Performance",
                              fluidPage(
                                fluidRow(
                                  column(width = 3, wellPanel(
      
                                               shiny::selectizeInput(
                                                 inputId = "Year2",
                                                 label = "Year:",
                                                 choices = 2014:2021,
                                                 selected = 2021,  #NULL
                                                 multiple = TRUE
                                               ),
                                               
                                 
                                               
                                               shiny::selectizeInput(
                                                 inputId = "week2",
                                                 label = "Week:",
                                                 choices = 1:17,
                                                 selected = 1, #NULL
                                                 multiple = TRUE
                                               ),
                                               
                                               shiny::selectizeInput(
                                                 inputId = "Position2",
                                                 label = "Position:",
                                                 choices = c("D/ST","K","QB","RB","TE","WR"),
                                                 selected = c("D/ST","K","QB","RB","TE","WR"),
                                                 multiple = TRUE#,
                                                 #options = list(...)
                                               )
                                  )         
                                  )),
                        
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Weekly Player Performance Summary",
                                    gt_output("tbl3")
                                      )
                                  ))
                              
                              ))
             
  
  )
)

  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #options(shiny.maxRequestSize=10*1024^2)
  
  
  
  #observeEvent({
  
  #updateSelectizeInput(
  #  inputId = 'Year',
  #  choices = 2014:2021,
  #  selected = 2021#,
    #server = TRUE
  #) 
  
  #updateSliderInput(
  #  inputId = 'Year',
  #  value = 2021,
  #  min = min,
  #  max = max
  #)
  
  #updateSelectizeInput(
  #  inputId = 'Round',
  #  choices = 1:15,
  #  selected = 1
    #server = TRUE
  #) 
  
  
  #updateSelectizeInput(
  #  inputId = 'Year1',
  #  choices = 2014:2021,
  #  selected = 2021#,
    #server = TRUE
  #) 
  
  #updateSelectizeInput(
  #  inputId = 'week',
  #  choices = 1:17,
  #  selected = 1#,
    #server = TRUE
  #) 
  
  #updateSelectizeInput(
  #  inputId = 'Position1',
  #  choices = c("D/ST","K","QB","RB","TE","WR"),
  #  selected = c("D/ST","K","QB","RB","TE","WR")
    #server = TRUE
  #)  
  
  output$tbl<-render_gt({
    
    tab1<-tab1 %>%
      dplyr::select(Year,Round,Pick..,Team,img,Player,headshot_url,team,Position,Keeper.,Total.Points,Total.Points.on.Roster,Total.Points.Started,
                    Draft.Position.by.Position..,Position.Rank.by.End.of.Season.Delta,
                    Draft.Position.to.End.of.Season.Rank)
    
    tab1<-tab1  %>%
      filter(Year %in% input$Year) %>%
      filter(Round %in% input$Round) %>%
      filter(Position %in% input$Position) %>%
      filter(Team %in% input$Team)
    
    tab1 %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(vars(img)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      text_transform(
        locations = cells_body(vars(headshot_url)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      cols_label(
        img = "",
        headshot_url = "",
        Pick.. = "Pick",
        Team = "Mgr",
        team = "Team",
        Total.Points = "Pts",
        Total.Points.on.Roster = "Roster Pts",
        Total.Points.Started = "Starter Pts",
        Draft.Position.by.Position.. = "Position Draft Pick",
        Position.Rank.by.End.of.Season.Delta = "+/- Final Position Rank",
        Draft.Position.to.End.of.Season.Rank = "+/- Final Fantasy Rank"
      ) %>% 
      tab_options(
        column_labels.background.color = "white",
        column_labels.font.weight = "bold",
        #table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        #table.border.bottom.width = px(3),
        #column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        #column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 12,
        heading.align = "left"
      )  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      )
 
    
    
  },width=px(600))
  
  
  output$graph1<-renderPlot({
    
    temp_draft<-draft %>%
      filter(Year %in% input$Year) %>%
      #filter(Round %in% input$Round) %>%
      filter(Position %in% input$Position) %>%
      filter(Team %in% input$Team)
      
    
    temp_draft %>%
      ggplot() +
      aes(x=Round,y=Team,fill=Total.Points) + 
      #geom_raster() +
      #stat_density() +
      geom_tile(#size=0.2,
        #lwd = 1,
        #linetype = 1
      ) +
      scale_fill_gradient(low="light blue",
                          high= "red") +
      #facet_wrap(~Position_f) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=12),
        panel.grid.major = element_blank(),
        strip.background = element_rect(color="light blue")
      ) +
      labs(
        title = "Total Points by Pick",
        fill = "Total Points",
        x = "",
        y = ""
      ) +
      scale_x_continuous(breaks=seq(0,15,1))
  })
  

  output$graph2<-renderPlot({
    
    temp_draft1<-draft %>%
      filter(Year %in% input$Year) %>%
      #filter(Round %in% input$Round) %>%
      filter(Position %in% input$Position) %>%
      filter(Team %in% input$Team)
    
    temp_draft1 %>%
    ggplot() +
    aes(x=Round,y=Draft.Position.to.End.of.Season.Rank,fill=Draft.Position.to.End.of.Season.Rank) +
    geom_raster(
    ) +
    scale_fill_gradient2(low= "blue",
                         mid = "grey",
                         midpoint = 0,
                         guide = "colorbar",
                         high= "red") +
    geom_smooth(color="grey") +
    geom_hline(yintercept = 0,
               color = "dark grey") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=12),
      panel.grid.major = element_blank(),
      strip.background = element_rect(color="light blue")
    ) +
    labs(
      title = "Draft Value Accuracy",
      subtitle = "Comparison of Draft Position to End of Season Rank",
      fill = "Total Points",
      x = "Draft Round",
      y = "Draft Position v End of Season Rank"
    )  +
    scale_x_continuous(breaks=seq(0,15,1))
  
  })
  
  output$tbl1<-render_gt({
    
    keepers1<-keepers %>%
      filter(Year %in% input$Year) %>%
      #filter(Round %in% input$Round) %>%
      filter(Position %in% input$Position) %>%
      filter(Team %in% input$Team)
    
    keepers1<-keepers1 %>%
      dplyr::select(Round,Player,headshot_url,Position,Team,img,Year,Weeks.on.Roster,
                    Weeks.Started,Total.Points,Total.Points.on.Roster,Total.Points.Started,
                    Draft.Position.to.End.of.Season.Rank,
                    Position.Rank.by.End.of.Season.Delta) %>%
      arrange(Round)
  
    
    keepers1 %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(vars(img)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      text_transform(
        locations = cells_body(vars(headshot_url)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      cols_label(
        img = "",
        headshot_url = "",
        Team = "Mgr",
        Total.Points = "Pts",
        Weeks.on.Roster = "Weeks on Roster",
        Weeks.Started = "Weeks Starter",
        Total.Points.on.Roster = "Roster Pts",
        Total.Points.Started = "Starter Pts",
        Position.Rank.by.End.of.Season.Delta = "+/- Final Position Rank",
        Draft.Position.to.End.of.Season.Rank = "+/- Final Fantasy Rank"
      ) %>% 
      tab_options(
        column_labels.background.color = "white",
        column_labels.font.weight = "bold",
        #table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        #table.border.bottom.width = px(3),
        #column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        #column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 12,
        heading.align = "left"
      )  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      )
    
  })
  
  output$tbl2<-render_gt({
    
    results2<-results %>%
      filter(RegimentYear %in% input$Year1) %>%
      filter(ManagerMatch %in% input$Team1) %>%
      filter(week %in% input$week)
    
    
    results2 %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(vars(Team_img)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      text_transform(
        locations = cells_body(vars(Opp_img)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      cols_label(
        Team_img = "",
        Opp_img = "",
        week = "Week",
        ManagerMatch = "Mgr"
      ) %>% 
      tab_options(
        column_labels.background.color = "white",
        column_labels.font.weight = "bold",
        #table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        #table.border.bottom.width = px(3),
        #column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        #column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 12,
        heading.align = "left"
      )  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      )
    
  })
  
  output$tbl3<-render_gt({
    
    lineups1<-lineups %>%
      dplyr::select(RegimentYear,week,name,headshot_url,position,ManagerMatch,img,Opponent,points,started,yahoo_id)
    
    lineups1<-lineups1  %>%
      filter(RegimentYear %in% input$Year2) %>%
      filter(week %in% input$week2) %>%
      filter(position %in% input$Position2)
    
    lineups1<-lineups1 %>%
      arrange(desc(points))
    
    lineups1 %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(vars(img)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }
      ) %>% 
      text_transform(
        locations = cells_body(vars(headshot_url)),
        fn = function(x){
          web_image(
            url = x,
            height = px(50)
          )
        }           
      ) %>% 
      cols_label(
        img = "",
        headshot_url = "",
        RegimentYear = "Year",
        week = "Week",
        name = "Name",
        position = "Pos.",
        ManagerMatch = "Mgr",
        Opponent = "Opponent",
        points = "Total Points",
        started = "Started?",
        yahoo_id = "Yahoo ID"
      ) %>% 
      tab_options(
        column_labels.background.color = "white",
        column_labels.font.weight = "bold",
        #table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        #table.border.bottom.width = px(3),
        #column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        #column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 12,
        heading.align = "left"
      )  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      )
    
    
    
  },width=px(600))

  
    
 output$graph4<-renderPlot({
   
   #results1<-results %>%
    # dplyr::select(week,RegimentYear,ManagerMatch,Result,Opponent,Points,Opp.Points,
    #               Difference,Team_img,Opp_img)
   
   results1<-results %>%
   filter(RegimentYear %in% input$Year1) %>%
     filter(week %in% input$week) 
   
   results1 %>%
     group_by(ManagerMatch,Team_img) %>%
     summarize(Points = median(Points,na.rm = T),
               Opp.Points = median(Opp.Points,na.rm = T)) %>%
     ggplot() +
     aes(x=Points,y=Opp.Points,label=ManagerMatch) +
     geom_text_repel(size = 2,nudge_y=-0.6) + 
     geom_image(aes(image = Team_img),size=0.05,by='height') +
     geom_vline(xintercept = median(results$Points,na.rm=T),color="grey",linetype='dashed') +
     geom_hline(yintercept = median(results$Opp.Points,na.rm=T),color="red",linetype='dashed') +
     theme_minimal() +
     theme(
       legend.position = "bottom",
       panel.grid.minor = element_blank(),
       plot.title = element_text(size=12),
       # panel.grid.major = element_blank(),
       strip.background = element_rect(color="light blue")
     ) +
     labs(
       title = "Summary Performance"
     ) 
 })   
  
  output$graph5<-renderPlot({
    
    results2<-results %>%
      filter(RegimentYear %in% input$Year1) %>%
      filter(week %in% input$week) 
    
    results2 %>%
      group_by(ManagerMatch,Team_img) %>%
      summarize(Difference = sum(Points,na.rm=T) - sum(Opp.Points,na.rm=T)) %>%
      ggplot(aes(x=Difference,y=reorder(ManagerMatch,Difference))) +
      geom_image(aes(image = Team_img),size=0.06,by="width",
                 asp_ratio = 1.618) +
      #scale_fill_identity(aesthetics = c("fill","color")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=12),
        # panel.grid.major = element_blank(),
        strip.background = element_rect(color="light blue")
      ) +
      labs(
        title = "Scoring Differentials",
        x = "Scoring Differential",
        y = "Team"
      ) 
    
  })
  
  output$graph6<-renderPlot({
    
    results3<-results %>%
      filter(RegimentYear %in% input$Year1) %>%
      filter(week %in% input$week) %>%
      filter(ManagerMatch %in% input$Team1) 
    
    results3 %>%
      ggplot() +
      aes(x=week,y=Points,color=ManagerMatch) +
      geom_jitter(alpha=0.3) + 
      #stat_summary(fun="median",geom="point",size=0.3,
      #             aes(color=ManagerMatch),shape=5,color="grey") +
      geom_smooth(aes(color = ManagerMatch), alpha = 0.05) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12)
      ) +
      theme(legend.position = "bottom") +
      scale_color_brewer(palette = "PuBuGn") +
      labs(
        title = "Points Per Week",
        color = "Mgr"
      ) +
      geom_text_repel(aes(label = ManagerMatch),
                      force = 1, point.padding = 0.1,
                      segment.size = 0.2
      ) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
