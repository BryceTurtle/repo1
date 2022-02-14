#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(DBI)
library(odbc)
library(tidyverse)
library(data.table)
library(ggthemes)
library(RODBC)
library(RPostgreSQL)
library(DT)
library(sqldf)
library(plotly)
library(extrafont)
library(grid)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(rvest)
library(httr)
library(stringi)
library(xml2)
library(jsonlite)
library(ggimage)
library(beepr)
library(ggimage)
library(shinyalert)

# setwd("~/Desktop/Pro Scouting/PD/PD Matrix Visuals/pd_matrix_visual_app")

### replacement to config so I don't have to include file ### 
POSTGRES_IP = "35.194.54.205"
POSTGRES_DB = "postgres"
POSTGRES_PORT = 5432
POSTGRES_UID = "postgres"
POSTGRES_PASSWORD = "P@loDur0!"

pg_con <- dbConnect(dbDriver("PostgreSQL"), dbname = POSTGRES_DB, host = POSTGRES_IP, port = POSTGRES_PORT, user = POSTGRES_UID, password = POSTGRES_PASSWORD)


### transparency for ggimage
transparent <- function(img) {
  magick::image_fx(img, expression = "0.2*a", channel = "alpha")
}

### read csv 
# pd_position_graph_data_ZavalaCauleyMoller <- fread('pd_position_graph_dataV2.csv')
# pd_position_graph_data_ZavalaCauleyMoller <- pd_position_graph_data_ZavalaCauleyMoller %>% mutate(LastEditDate = Sys.time() ) %>% mutate(ID = 1:n(), .before =  Trait)
# dbWriteTable(pg_con, c('pro_scouting', 'pd_matrix_backend'), pd_position_graph_data_ZavalaCauleyMoller, append = TRUE, row.names = FALSE)

########################################
### reading data table from postgres ###
########################################

pd_position_graph_data_ZavalaCauleyMoller =
  
  "select *
   from pro_scouting.pd_matrix_backend
  "

pd_position_graph_data_ZavalaCauleyMoller <- dbGetQuery(pg_con, pd_position_graph_data_ZavalaCauleyMoller, stringsAsFactors = F)
pd_position_graph_data_ZavalaCauleyMoller <- pd_position_graph_data_ZavalaCauleyMoller %>% select(-ID, -LastEditDate) %>% data.table()

### re-init
pd_position_graph_data_ZavalaCauleyMoller <- pd_position_graph_data_ZavalaCauleyMoller %>% separate(Trait, into = c('TraitTitle', 'Description'), sep = ' - ', remove = FALSE)

### changing Trait C

pd_position_graph_data_ZavalaCauleyMoller[TraitTitle == 'C', 'TraitTitle'] <- 'Catching'
pd_position_graph_data_ZavalaCauleyMoller[TraitTitle == 'Pitch recognition', 'TraitTitle'] <- 'Pitch Recognition'


### scale all positions other than catcher
### re-init
pd_position_graph_data_ZavalaCauleyMoller <- pd_position_graph_data_ZavalaCauleyMoller %>% mutate(scale_importance = ifelse(Position != 'Catchers', `Importance (X-Axis)`- 3, `Importance (X-Axis)`), 
                                                                                                  scale_difficultyoffix = ifelse(Position != 'Catchers', `Difficulty of Fix (Y-Axis)` - 3, `Difficulty of Fix (Y-Axis)`))
## set factor
pd_position_graph_data_ZavalaCauleyMoller$Category <- factor(pd_position_graph_data_ZavalaCauleyMoller$Category, c(pd_position_graph_data_ZavalaCauleyMoller$Category %>% unique() ) )

################
### Function ###
################

## gradient color function
### init
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
    cols <- colorRampPalette(cols)(n + 1)
    rad <- deg / (180 / pi)
    mat <- matrix(
        data = rep(seq(0, 1, length.out = n) * cos(rad), n),
        byrow = TRUE,
        ncol = n
    ) +
        matrix(
            data = rep(seq(0, 1, length.out = n) * sin(rad), n),
            byrow = FALSE,
            ncol = n
        )
    mat <- mat - min(mat)
    mat <- mat / max(mat)
    mat <- 1 + mat * n
    mat <- matrix(data = cols[round(mat)], ncol = n)
    grid::rasterGrob(
        image = mat,
        width = unit(1, "npc"),
        height = unit(1, "npc"), 
        interpolate = TRUE
    )
}

g <- make_gradient(deg = 45, n = 500, cols = c('blue', 'white', 'red') )


### app users
appusers_name = c('Parnell, Mike', 'Fenstermaker, Ross', 'Yang, Leonard')
appusers_id = 1:length(appusers_name)
## init
appusers <- data.table(name = appusers_name,
                       id = appusers_id)


########## player list ######################### player list ######################### player list ######################### player list ###############
### PULL FROM STATS API WHEN NEEDED 

# teamids = 'https://statsapi.mlb.com/api/v1/teams?season=2021'
# res = GET(teamids)
# mycontent = content(res, as = 'text')
# 
# team_full = fromJSON(mycontent)
# teams = team_full$teams
# teams = data.table(teams)
# 
# ### just minor leagues
# ### init
# teams_tex = teams %>% filter(parentOrgId == 140 & sport.name %in% c('Triple-A', 'Double-A', 'High-A', 'Low-A', 'Rookie'))
# teams_tex_teamids = teams_tex %>% pull(id)
# 
# ### init
# milb_master_active_roster <- data.table()
# 
# for(i in teams_tex_teamids){
# 
#     print(paste0('currently requesting teamid = ', i) )
#     roster = paste0('https://statsapi.mlb.com/api/v1/teams/', i, '/roster?rosterType=active&season=2022&&hydrate=person(xrefId)')
#     res = GET(roster)
#     mycontent = content(res, as = 'text')
# 
#     roster_full = fromJSON(mycontent)
#     activeroster = roster_full$roster
#     activeroster = data.table(activeroster)
# 
#     ### rows in active roster to unpack xref table
#     ### init
#     xrefmaster <- data.table()
# 
#     for(j in 1:nrow(activeroster) ){
# 
#         mlbidz = activeroster[j, ] %>% pull(person.id)
# 
#         idz <- activeroster$person.xrefIds[[j]] %>% unique() %>% mutate(id = 1) %>% pivot_wider(id_cols = 'id', values_from = 'xrefId', names_from = 'xrefType') %>% mutate(person.id = mlbidz) %>% data.table()
# 
#         ## append
#         xrefmaster <- rbind(xrefmaster, idz, fill = TRUE)
# 
#     }
# 
#     ### add date pulled and teamid
#     activeroster = activeroster %>% select(-person.xrefIds) %>% left_join(xrefmaster, by = 'person.id') %>% mutate(Date = Sys.Date()) %>% mutate(teamid = i, .before = 'person.id')
# 
#     ## append
#     milb_master_active_roster = rbind(milb_master_active_roster, activeroster, fill = TRUE)
# 
# }
# 
# ### join with team details from up top
# milb_master_active_roster <- milb_master_active_roster %>% left_join(teams_tex %>% select(id, name, sport.name), by = c('teamid' = 'id') )
# ### init
# tex_players <- milb_master_active_roster %>% filter(position.abbreviation != 'P')
# tex_players <- tex_players %>% select(teamid, person.id, person.fullName, position.abbreviation, person.batSide.description, person.height, person.weight, person.birthCountry)
# 
# ### put in matrix position code
# tex_players <- tex_players %>% mutate(Position = ifelse(position.abbreviation %in% c('2B'), '2B',
#                                          ifelse(position.abbreviation %in% c('3B'), '3B',
#                                                 ifelse(position.abbreviation %in% c('C'), 'Catchers',
#                                                        ifelse(position.abbreviation %in% c('CF'), 'CF',
#                                                               ifelse(position.abbreviation %in% c('1B', 'LF', 'OF', 'RF', 'IF'), 'Corner Bats',
#                                                                      ifelse(position.abbreviation %in% c('SS'), 'SS', NA) ) ) ) ) ) )
# 
# ### write as csv
# write.csv(tex_players, 'tex_players.csv', row.names = FALSE)

# ## fread
# tex_players <- fread('tex_players.csv')
#
### writing up to postgres table 
# tex_players <- tex_players %>% mutate(LastEditDate = Sys.time() ) %>% mutate(ID = 1:n(), .before =  teamid)
# dbWriteTable(pg_con, c('pro_scouting', 'pd_matrix_backend_tex_players'), tex_players, append = TRUE, row.names = FALSE)

########################################
### reading data table from postgres ###
########################################

tex_players =
  
  "select *
   from pro_scouting.pd_matrix_backend_tex_players
  "

tex_players <- dbGetQuery(pg_con, tex_players, stringsAsFactors = F)
tex_players <- tex_players %>% select(-ID, -LastEditDate)


########## player list ######################### player list ######################### player list ######################### player list ###############

### visual function

pd_viz <- function(table, player, positionz){
    
    # positionz <- tex_players %>% filter(person.fullName == player) %>% pull(Position) %>% unique()
    
    
    graphz <- table %>% filter(Position == positionz) %>% ggplot(aes(x=scale_importance, y = scale_difficultyoffix, shape = Category, label = TraitTitle, col = Category)) +
        annotation_custom(grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_point(aes( alpha = transparency) ) + geom_text_repel(aes( alpha = transparency), show.legend = FALSE, force = 2, fontface = 'bold' ) + #labs(color = 'Category', shape = 'Category') +
        geom_segment(x=0, xend = 0, y=-5, yend = 5, col = 'gray') + geom_segment(x=-5, xend = 5, y= 0, yend = 0, col = 'gray') + theme_clean() +
        # geom_abline(slope = 1, intercept = 0, col = 'blue', size = 0.001, alpha = 0.3) + ### comment out for no line
        xlim(-2.1, 2.1) + ylim(-2.1,2.1) +
        scale_color_manual(name = 'Category', labels = CategoryUnique, values = CategoryUnique_colors) +
        scale_shape_manual(name = 'Category', labels = CategoryUnique, values = 1:4) +
        scale_alpha_manual(values = c(0.1, 1) ) +
        ylab('(Less)     Difficulty of Fix    (More)') + xlab('(Less)     Importance    (More)') + ggtitle(paste0(positionz, ' PD Matrix: ', player) ) +
        theme(plot.title = element_text(size = 22, face = 'bold'), plot.background = element_rect(fill=NA,color="black", size=1, linetype="solid"), 
              legend.position = c(0.925,0.13) ) + guides(alpha = 'none')
    
    return(graphz)
}


###########
### POC ###
###########

### for this example... 
# thisexample <- pd_position_graph_data_ZavalaCauleyMoller ##%>% filter(Position == 'Corner Bats')
# thisexample$transparency <- NULL

### plot colors

### plot colors
### init
CategoryUnique <- pd_position_graph_data_ZavalaCauleyMoller$Category %>% unique()
# CategoryUnique_colors <- brewer.pal(6, 'Set2')[c(1,2,5,6)] ##hue_pal()(4) # rainbow(7) #c('#002D72', '#BA0C2F', '#857874', '#FC4C02', '#006747', '#FBD872', '#33006F')    #rainbow(7)
### alternative
CategoryUnique_colors <- c('midnightblue', 'darkgreen', 'deeppink4', 'gray16')
names(CategoryUnique_colors) <- CategoryUnique

#############################
########## shiny ############
#############################

##########
### UI ###
##########

# Define UI for application that draws a histogram
ui <- navbarPage("PD Matrix",
                 
                 tabPanel('Save Visual',
                 
                          setBackgroundColor("#FCFBF8"),
              
                 fluidRow(
                     column(3, align = 'center',
                            plotOutput('logotex', height = '150px')
                            ),
                     column(6, align = 'center',
                            h1(strong('Texas Rangers Player Development Matrix') )
                            ),
                     column(3, align = 'center',
                            plotOutput('logomlb', height = '150px')
                            )
                 ),
                 
                 br(),
                 
                 fluidRow(
                     column(3, align = 'left',
                            dropdownButton(
                                inputId = 'mydropdown1',
                                label = 'Visual Explanation',
                                icon = icon('baseball-ball'),
                                circle = FALSE, 
                                
                                HTML("<h4><b>What does this visual tell you?</b></h4>
                                     <h5>The goal of this visual is to:</h5>
                                     <ol><li><h5>Provide a quick snapshot of a player's current strengths and weaknesses,</h5></li>
                                     <li><h5>Grasp which traits are most important to a player, and</h5></li>
                                     <li><h5>Observe whether or not a skill is teachable.</h5></li></ol>
                                     
                                     <h5>This graphic show the importance (x-axis) and difficulty of fix (y-axis) of various 
                                     offensive, defensive, body, and makeup traits per position group.</h5>
                                     <h5>Traits further to the right are deemed more important, 
                                     while traits further up are thought as more difficult to fix.</h5>
                                     <h5>For example, traits in quadrant 1 (top right) are deemed 
                                     important to player success, but are not neccesarily adjustable. On the other hand,  
                                     traits in quadrant 4 (bottom right) are those that are vital to player 
                                     success that we believe can be taught.</h5>
                                     <h5>TL;DR- bold in top right is BAD, bold in bottom right is GOOD.<h5>
                                     <h5><b>Graph Quadrants:</b>"),
                                plotOutput('graphquadrants', height = '300px')
                                
                                # HTML(
                                #     paste0(
                                #     h4(strong('Directions:') ),
                                #     h5("Click on each of the below FOUR categories to add/subtract a player's area of strength."),
                                #     h5("Selected characteristics will turn <b>bold</b> in the below visual.")
                                # ) ## paste
                                #) ## html
                                
                            ) ### dropdownbutton close
                            
                     ) ## col
                 ), ## row
                 
                 br(),
                 
                 fluidRow(
                     column(3, align = 'left',
                            dropdownButton(
                                inputId = 'mydropdown2',
                                label = 'How to Use',
                                icon = icon('baseball-ball'),
                                circle = FALSE, 
                               
                                HTML("<h4><b>Directions:</b></h4>
                                     <h5>Select a player of interest from the <em>Select a Player</em> dropdown menu to the right<h5>
                                     <h5>The default visual corresponds to each player's default MLBAM position tag. 
                                     You have the option to <em>change</em> a player's position to see how his traits stack up at different positions.</h5>
                                     <h5>Click on each of the below FOUR categories (Offense, Defense/Baserunning, Body/Athlete, Makeup) 
                                     to add traits that are seen as weaknesses for that player of choice.</h5>
                                     <h5>Selected traits will turn <b>bold</b> in the below visual. 
                                     This will provide a quick snapshot of which traits a player needs to develop. </h5>
                                     <h5><em>Note: The more bold in quadrant 4, the more 'important' traits a player could potentially 
                                     turn into strengths.</em></h5>
                                     <h5>Click on the <em>Save Visual</em> dropdown to save the visual for you to refer back to later.
                                     Select your name under the <em>Name</em> dropdown menu and click <em>Save</em>.</h5>")
                                
                                # HTML(
                                #     paste0(
                                #     h4(strong('Directions:') ),
                                #     h5("Click on each of the below FOUR categories to add/subtract a player's area of strength."),
                                #     h5("Selected characteristics will turn <b>bold</b> in the below visual.")
                                # ) ## paste
                                #) ## html
                                
                            ) ### dropdownbutton close
                            
                        ) ## col
                 ), ## row
                 
                 fluidRow(
                     column(12, align = 'center', 
                            
                            selectInput('player', 'Select a Player:', 
                                        choices = c('Select a Player', tex_players$person.fullName %>% unique() %>% sort() ), selected = 'Select a Player' ) 
                     ) ## column
                 ), ## row
                 
                 fluidRow(
                     column(12, align = 'center', 
                            
                            selectInput('position', 'Select a Position:', 
                                        choices = c('Default', 'Catchers', 'Corner Bats', '2B', '3B', 'SS', 'CF'), selected = 'Default' ) 
                     ) ## column
                 ), ## row
                 
                 fluidRow(
                     column(3, align = 'center',
                            
                            pickerInput('offense', 'Offense:',  #pickerInput()
                                        choices = pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Offense') %>% pull(TraitTitle) %>% unique() %>% sort(),
                                        options = list(`actions-box` = TRUE, title = 'Select Weaknesses'), multiple = T  ##filter(thisexample, Category == 'Offense')$TraitTitle %>% sort(), options = list(`actions-box` = TRUE), multiple = T 
                            ) ## pickerinput
                     ), ## column
                     
                     column(3, align = 'center',
                            
                            pickerInput('defense', 'Defense/Baserunning:',  #pickerInput()
                                        choices = pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Defense / BR') %>% pull(TraitTitle) %>% unique() %>% sort(),
                                        options = list(`actions-box` = TRUE, title = 'Select Weaknesses'), multiple = T ##filter(thisexample, Category == 'Defense / BR')$TraitTitle %>% sort(), options = list(`actions-box` = TRUE), multiple = T 
                            ) ## pickerinput
                     ), ## column
                     
                     column(3, align = 'center',
                            
                            pickerInput('body', 'Body/Athlete:',  #pickerInput()
                                        choices = pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Body / Athlete') %>% pull(TraitTitle) %>% unique() %>% sort(),
                                        options = list(`actions-box` = TRUE, title = 'Select Weaknesses'), multiple = T ##filter(thisexample, Category == 'Body / Athlete')$TraitTitle %>% sort(), options = list(`actions-box` = TRUE), multiple = T 
                            ) ## pickerinput
                     ), ## column
                     
                     column(3, align = 'center',
                            
                            pickerInput('makeup', 'Makeup:',  #pickerInput()
                                        choices = pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Makeup') %>% pull(TraitTitle) %>% unique() %>% sort(),
                                        options = list(`actions-box` = TRUE, title = 'Select Weaknesses'), multiple = T ##filter(thisexample, Category == 'Makeup')$TraitTitle %>% sort(), options = list(`actions-box` = TRUE), multiple = T 
                            ) ## pickerinput
                     ), ## column
                     
                 ), ### Row
                 
                 
                 ### go button for if we want eventReactive
                 # fluidRow(
                 #     column(12, align = 'center', 
                 #            actionButton(inputId = 'go_traits', label = 'Set Player Traits')
                 #     ) ### column
                 # ), ### row
                 
                 hr(),
                 
                 # fluidRow(align = 'center', 
                 #          
                 #     tableOutput("updatetable")
                 # ), 
                 
                 ### download button
                 # fluidRow(
                 #     column(1, align = 'center', 
                 #            downloadButton(outputId = 'down', label = 'Download Visual')
                 #     ) ### column
                 # ), ### row
                 
                 ### save visual dropdown
                 fluidRow(
                     column(1, 
                            dropdownButton(
                              inputId = 'saveviz',
                              label = 'Save Visual',
                              icon = icon('folder'),
                              circle = FALSE, 
                              # column(2, align = 'center', 
                              #        actionButton('reset', 'Reset')
                              #        ),
                              selectInput('appusername', 'Your Name:',
                                          choices = c('-', appusers$name %>% unique() %>% sort() ) ),
                              actionButton('vizsave', 'Save') 
                              
                            ) ### dropdownbutton close 
                      ) ### column close
                     
                     ), ## row
                 
                 br(),
                 
                 # h3('table example'),
                 #
                 # fluidRow(
                 #   column(12, 
                 #          tableOutput('savedtable')
                 #   )
                 # ),
                 
                 ### Plot
                 fluidRow(align = 'center', 
                          
                          plotOutput('plot')
                          
                 ),
                 br(),
                 br()
                 
                 
        ), ## tab  
        
        
        tabPanel('Review Visual',
                 
                 setBackgroundColor("#FCFBF8"),
                 
                 fluidRow(
                   column(3, align = 'center',
                          plotOutput('logotex2', height = '150px')
                   ),
                   column(6, align = 'center',
                          h1(strong('Texas Rangers Player Development Matrix') )
                   ),
                   column(3, align = 'center',
                          plotOutput('logomlb2', height = '150px')
                   )
                 ),
                 
                 fluidRow(
                   column(3, align = 'left',
                          dropdownButton(
                            inputId = 'mydropdown3',
                            label = 'Explanation',
                            icon = icon('baseball-ball'),
                            circle = FALSE, 
                            
                            HTML("<h4><b>Directions:</b></h4>
                                     <h5>This page allows users to look back on previous charts created.</h5>
                                      <h5>Select a user's name under the <em>Select Your Name</em> dropdown menu, and click <em>Load</em> to see 
                                      visuals previously created.<h5>
                                     <h5>Click on a player's row to review his previous visual.<h5>")
                                
                            ) ### dropdownbutton close
                            
                     ) ## col
                 ), ## row
                 
                 fluidRow(
                   column(12, align = 'center',
                          selectInput('appusername_load', 'Select Your Name:',
                                      choices = c('-', appusers$name %>% unique() %>% sort() ) ) 
                          ) ## cols
                 ), ## row
                 fluidRow(
                   column(12, align = 'center',
                          actionButton('vizload', 'Load')
                          ) ## col
                 ), ## row
                 
                 hr(),
                 
                 fluidRow(
                   column(6, align = 'center', offset = 3,
                          dataTableOutput('loadedtable_df')
                          ) ## col
                 ), ## row
                 
                 hr(),
                 
                 fluidRow(
                   column(12, align = 'center',
                          plotOutput('loadz_plot')
                          ) ## col
                 ) ## row
                 
        ) ## tab
        
        
                 
                 
) ### navparPage

##############
### server ###
##############

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # defaultdf <- reactive({
    #     
    #     pos = tex_players %>% filter(person.fullName == input$player) %>% pull(Position) %>% unique()
    #     
    #     pd_position_graph_data_ZavalaCauleyMoller %>% filter(Position == pos) %>% mutate(Player = input$player)
    #     
    # })
    
    thisexample <- reactive({
      
        # pos = tex_players %>% filter(person.fullName == input$player) %>% pull(Position) %>% unique()
        
        pos = if(input$position == 'Default'){
            tex_players %>% filter(person.fullName == input$player) %>% pull(Position) %>% unique()

        } else if(input$position != 'Default'){
            input$position
        }
        
        pd_position_graph_data_ZavalaCauleyMoller %>% filter(Position == pos) %>% mutate(Player = input$player)
        
    })
    
    # observeEvent(thisexample(), {
    #     defaultpos <- tex_players %>% filter(person.fullName == input$player)
    #     # choices <- c(unique(players1()$BATTER_NAME_LAST_FIRST) %>% sort(), 'Select a Player')
    #     choices <- tex_players$Position %>% unique() %>% sort()
    #     updateRadioButtons(inputId = 'position', choices = choices, selected = defaultpos, inline = TRUE ) ##, server = TRUE) ### updateSelectizeInput
    # })
    
    ### observe event for position to DEFAULT
    observeEvent(input$player, {
        
        choice = c('Default', 'Catchers', 'Corner Bats', '2B', '3B', 'SS', 'CF')
        updateSelectInput(inputId = 'position', choices = choice)
    })
    
    ### OFFENSE choices to bold and alpha
    # alphachoiceOffense <- eventReactive(input$player, {
    # 
    #     thisexample() %>% filter(Player == input$player & Category == 'Offense')
    # 
    # })

    ##################### changing pickerinput each time we select a NEW player ##################### changing pickerinput each time we select a NEW player #############################
    observeEvent(input$player, {

        choices <- pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Offense') %>% pull(TraitTitle) %>% unique() %>% sort()
        updatePickerInput(session = session, inputId = 'offense', choices = choices)
    })
    
    
    # ### DEFENSE choices to bold and alpha
    # alphachoiceDefense <- eventReactive(input$player, {
    #     
    #     thisexample() %>% filter(Player == input$player & Category == 'Defense / BR')
    #     
    # })
    
    observeEvent(input$player, {
        
        choices <- pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Defense / BR') %>% pull(TraitTitle) %>% unique() %>% sort()
        updatePickerInput(session = session, inputId = 'defense', choices = choices)
    })
    
    
    # ### BODY choices to bold and alpha
    # alphachoiceBody <- eventReactive(input$player, {
    #     
    #     thisexample() %>% filter(Player == input$player & Category == 'Body / Athlete')
    #     
    # })
    
    observeEvent(input$player, {
        
        choices <- pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Body / Athlete') %>% pull(TraitTitle) %>% unique() %>% sort()
        updatePickerInput(session = session, inputId = 'body', choices = choices)
    })
    
    
    # ### MAKEUP choices to bold and alpha
    # alphachoiceMakeup <- eventReactive(input$player, {
    #     
    #     thisexample() %>% filter(Player == input$player & Category == 'Makeup')
    #     
    # })
    
    observeEvent(input$player, {
        
        choices <- pd_position_graph_data_ZavalaCauleyMoller %>% filter(Category == 'Makeup') %>% pull(TraitTitle) %>% unique() %>% sort()
        updatePickerInput(session = session, inputId = 'makeup', choices = choices)
    })
    
    ##################### changing pickerinput each time we select a NEW player ##################### changing pickerinput each time we select a NEW player #############################
    
    # zavala <- data.frame(
    #     TraitTitle = thisexample()$TraitTitle, 
    #     transparency = 0 )

    transparencytable <- reactive( {

        if(length(c(input$offense, input$defense, input$body, input$makeup ) ) == 0){
            
            data.frame(
                TraitTitle = NA, 
                transparency = NA
            )
            
        } else if(length(c(input$offense, input$defense, input$body, input$makeup ) ) > 0){
            
            rbind(
                
                data.frame(
                    TraitTitle = c(input$offense, input$defense, input$body, input$makeup ),
                    transparency = '1'), 
                
                data.frame(
                    TraitTitle = setdiff(thisexample()$TraitTitle, c(input$offense, input$defense, input$body, input$makeup ) ),
                    transparency = '0'
                )
                
            ) ### rbind end 
            
            
        }
        
    })
    
    # ## to show during process
    # output$updatetable <- renderTable(
    #     
    #     transparencytable()
    # )
    
    
    zavalatable <- reactive({
        
        thisexample() %>% filter(Player == input$player) %>% left_join(transparencytable(), by = 'TraitTitle')
        
    })
    
    output$plot <- renderPlot({
        
        if(input$player == 'Select a Player'){
            
        data.frame(x=0, y=0) %>% ggplot(aes(x=x, y=x, text = 'Choose Player')) + geom_text(aes(x=x, y=y, label = 'Choose a Player from the Above Dropdown Menu'), size = 6) + 
            theme_classic() + theme(axis.text.y = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.ticks.x = element_blank(),
                                    axis.line.x = element_blank(),
                                    axis.line.y = element_blank() ) + ylim(-10,10) + xlim(-10,10) #+
                # geom_image(aes(x = 0, y = 5, image =  paste0('./logos/', 'TEX', '.png') ), size = 0.4, by = 'width', image_fun = transparent) 
            
            
        } else if(input$player != 'Select a Player'){
        
        pd_viz(zavalatable(), input$player, thisexample()$Position %>% unique() )
        
        }
        
    }, width = 1100, height = 800) ## renderPlot
    
    
    # ### for saving download file  
    # plot_output <- reactive({
    #     
    #     if(input$player == 'Select a Player'){
    #         
    #         data.frame(x=0, y=0) %>% ggplot(aes(x=x, y=x, text = 'Choose Player')) + geom_text(aes(x=x, y=y, label = 'Choose a Player from the Above Dropdown Menu'), size = 6) + 
    #             theme_classic() + theme(axis.text.y = element_blank(),
    #                                     axis.title.y = element_blank(),
    #                                     axis.ticks.y = element_blank(),
    #                                     axis.text.x = element_blank(),
    #                                     axis.title.x = element_blank(),
    #                                     axis.ticks.x = element_blank(),
    #                                     axis.line.x = element_blank(),
    #                                     axis.line.y = element_blank() ) + ylim(-10,10) + xlim(-10,10) #+
    #         # geom_image(aes(x = 0, y = 5, image =  paste0('./logos/', 'TEX', '.png') ), size = 0.4, by = 'width', image_fun = transparent) 
    #         
    #         
    #     } else if(input$player != 'Select a Player'){
    #     
    #     pd_viz(zavalatable(), input$player, thisexample()$Position %>% unique() )
    #         
    #     }
    #     
    # })
    
    
    # ### for downloading plot as 9x12 image 
    # 
    # output$down <- downloadHandler(
    #     filename =  function() {
    #         paste0(input$player, '_', thisexample()$Position %>% unique(), '_PDMatrix.png')
    #     },
    #     # content is a function with argument file. content writes the plot to the device
    #     content = function(file) {
    #         
    #         ggsave(file, plot_output(), device = 'png', height = 9, width = 12)
    #         
    #         # png(file) # open the pdf device
    #         # # pd_viz(zavalatable(), input$player)
    #         # dev.off()  # turn the device off
    #         
    #     } 
    # ) ### downloadHandler
    

  ### logo
  output$logotex <-  renderImage({
          
          filename <- normalizePath(file.path('./logos', paste0('TEX.png') ) )
          
          ### generate png
          list(src = filename, width = 150, height = 150 )}, deleteFile = FALSE) ### style="display: block; margin-left: auto; margin-right: auto;"
  
  output$logomlb <-  renderImage({
      
      filename <- normalizePath(file.path('./logos', paste0('MLB.png') ) )
      
      ### generate png
      list(src = filename, width = 150, height = 150 )}, deleteFile = FALSE) ### style="display: block; margin-left: auto; margin-right: auto;"
  
  output$graphquadrants <-  renderImage({
      
      filename <- normalizePath(file.path('./logos', paste0('graphquadrants.PNG') ) )
      
      ### generate png
      list(src = filename, width = 300, height = 300 )}, deleteFile = FALSE) ### style="display: block; margin-left: auto; margin-right: auto;"
  
  
  ##### save visual to postgres table #####
  
  savedvizdf <- reactiveValues(dfmax = data.table() )
  
  ## rbind names who have already gotten saved
  storedvalues <- observeEvent(input$vizsave, {
    
    idz <- Sys.time()
    idz <- gsub('-', '', idz)
    idz <- gsub(':', '', idz)
    idz <- gsub(' ', '', idz)
    namez <- input$appusername
    namez <- gsub(', ', '', namez)
    
    dftobind <- zavalatable() %>% cbind(username = input$appusername, Date = Sys.Date(), tableid = paste0(namez, '_', idz ) )
    savedvizdf$dfmax <- rbind(savedvizdf$dfmax, dftobind )
    
    ### write to postgres, don't write if appusername is not selected
    if(input$appusername == '-'){
        req(FALSE, cancelOutput = TRUE)
    }
    ### write to postgres
    dbWriteTable(pg_con, c("pro_scouting", "pd_matrix_saved"), savedvizdf$dfmax, append = TRUE, row.names = FALSE)
      
      })
  
    ### when you click save, popup message
  observeEvent(input$vizsave,{
    
    if(input$appusername != '-'){
      
      shinyalert('Visual Saved', 'Your visual was saved', type = 'success')
    } else if(input$appusername == '-'){
      
      shinyalert('Oops!', 'Please select your name before saving visual', type = 'error')
    }
    
  })
  
  ### clear after clicking save
  storedvalues <- observeEvent(input$vizsave, {

    savedvizdf$dfmax <- data.table()

  })
  
  
  ### just to show in UI that it's working
  dffinal <- reactive({
    
    savedvizdf$dfmax
    
  }) %>% bindEvent(input$vizsave)
  
  output$savedtable = renderTable({
    
    savedvizdf$dfmax
    
    })
  
  
  
  
  
  ########### recall of saved visuals ########### recall of saved visuals ########### recall of saved visuals ########### recall of saved visuals ############
  
  ### load table from load viz portion on the right 
  loadedtablez <- reactive({
    
    loaded_pg_matrix_df =
      paste0("
        select *
        from pro_scouting.pd_matrix_saved pms 
        where username =", "'", input$appusername_load, "'"
      )
    
    dbGetQuery(pg_con, loaded_pg_matrix_df, stringsAsFactors = F)
    
  }) %>% bindEvent(input$vizload)
  
  
  output$loadz_plot <- renderPlot({  
    
    if(length(input$loadedtable_df_rows_selected) == 0 ){
    
      data.frame(x=0, y=0) %>% ggplot(aes(x=x, y=x, text = 'Choose Player')) + geom_text(aes(x=x, y=y, label = 'Choose a Player from the Above Table'), size = 6) + 
        theme_classic() + theme(axis.text.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x = element_blank(),
                                axis.title.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.line.x = element_blank(),
                                axis.line.y = element_blank() ) + ylim(-10,10) + xlim(-10,10) 
      
    } else if (length(input$loadedtable_df_rows_selected) == 1 ){
      
      tableidz = (loadedtablez() %>% select(Date, Player, Position, tableid) %>% unique() %>% arrange(desc(Date), Player, Position) )[input$loadedtable_df_rows_selected, ]$tableid
      playerz_load = loadedtablez() %>% filter(tableid == tableidz) %>% pull(Player) %>% unique()
      positionz_load = loadedtablez() %>% filter(tableid == tableidz) %>% pull(Position) %>% unique()
      
      pd_viz(loadedtablez() %>% filter(tableid == tableidz) %>% select(-username, -Date, -tableid), 
             playerz_load, positionz_load )
      
    }
    
  }, width = 1100, height = 800)
    
    ### table to show in drop down menu
    output$loadedtable_df <- renderDataTable({
      
      loaded_pg_matrix_df =
        paste0("
        select *
        from pro_scouting.pd_matrix_saved pms 
        where username = ", "'", input$appusername_load, "'"
        )
      
      loaded_pg_matrix_df = dbGetQuery(pg_con, loaded_pg_matrix_df, stringsAsFactors = F) %>% select(Date, Player, Position, tableid) %>% unique() %>% arrange(desc(Date), Player, Position) %>%
        datatable(options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                              list(targets = 4, visible = FALSE)) ), selection = 'single') #, rownames= FALSE)
      
      if(input$appusername_load == '-'){
        
        data.frame()
      } else loaded_pg_matrix_df
      
    }) %>% bindEvent(input$vizload)
    
    
    ### logo
    output$logotex2 <-  renderImage({
      
      filename <- normalizePath(file.path('./logos', paste0('TEX.png') ) )
      
      ### generate png
      list(src = filename, width = 150, height = 150 )}, deleteFile = FALSE) ### style="display: block; margin-left: auto; margin-right: auto;"
    
    output$logomlb2 <-  renderImage({
      
      filename <- normalizePath(file.path('./logos', paste0('MLB.png') ) )
      
      ### generate png
      list(src = filename, width = 150, height = 150 )}, deleteFile = FALSE) ### style="display: block; margin-left: auto; margin-right: auto;"
  

} ### server

# Run the application 
shinyApp(ui = ui, server = server)



