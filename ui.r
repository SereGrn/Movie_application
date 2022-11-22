
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(shinydashboardPlus)
library(rsconnect)
library(shinycssloaders)
library(shinyjs)
library(shinyjs)
library(shinyBS)


ui <- dashboardPage(skin = "midnight",
                    
                    dashboardHeader(
                      
                      titleWidth = '50%',
                      title = "MOVIE APPLICATION",
                      dropdownMenu(type = "notifications",
                                   notificationItem(
                                     text = "5 new connections!",
                                     icon("users")
                                   ),
                                   notificationItem(
                                     text = "3 new likes! ",
                                     icon("heart", lib ="glyphicon"),
                                     status = "danger"
                                   ),
                                   notificationItem(
                                     text = "6 films added!",
                                     icon = icon("film", lib = "glyphicon"),
                                     status = "info"
                                   )
                      )
                    ),
                  
                    
                    dashboardSidebar(
                      
                      sidebarMenu(
                        menuItem("Presentation", tabName = "Menu1", icon = icon("dashboard")),
                        menuItem("Some statistics", tabName = "Menu2",icon = icon("stats", lib = "glyphicon")),
                        menuItem("Some tips ", tabName="Menu3", icon = icon("plus-sign", lib = "glyphicon")),
                        menuItem("Focus on celebrities",tabName = "Menu4",icon=icon("star"))
                        
                      )),
                    
                    dashboardBody(
                      
                      h6("ⓒ Pauline Peydiere & Serena Gruarin"),
                useShinyjs(),
                      
                      tabItems(
                        tabItem(tabName = "Menu1",
                                fluidRow(
                                  column(8, align = "center",
                                         h1(strong("Presentation of the application")),
                                         br(),
                                         
                                         h5("This application has a double utility : "),
                                         h5("Elaborate different statistics on movies, according to the user's needs,"),
                                         h5("Offer various personalized film and acting tips")
                                  ),
                                         img(src = "film2.png", height = 180, width = 230, style="display: block; margin-left: auto; margin-right: auto;")
                                  ),
                                  
                                  
                                
                                
                                hr(),
                                
                                fluidRow(
                                         box(title = "Average user rating :", width = 6,
                                             starBlock(4,color='yellow')),
                                         
                                         box(title = "Login required :", width = 6,
                                                  
                                             
                                             actionBttn("go",
                                                        label = "Login")
                                             )
                                 
                                  
                                
                                ),
                                
                                fluidRow(
                                  tags$head(tags$style(".shiny-output-error{color: red;}")),
                                  
                                  box(
                                    solidHeader = FALSE,
                                    title = "Evolution of the visibility of the application",
                                    width = 12,
                                    collapsible = T, collapsed = T,
                                    status = "danger",
                                    footer = fluidRow(
                                      column(
                                        width = 6,
                                        descriptionBlock(
                                          number = " + 300", 
                                          numberColor = "green", 
                                          
                                          header = "35,210.43", 
                                          text = "Number of users", 
                                          rightBorder = TRUE,
                                          marginBottom = FALSE
                                        )
                                      ),
                                      column(
                                        width = 6,
                                        descriptionBlock(
                                          number = " + 15", 
                                          numberColor = "green", 
                                          
                                          header = "1200", 
                                          text = "Number of likes", 
                                          rightBorder = FALSE,
                                          marginBottom = FALSE
                                        )
                                      )
                                      
                                    )
                                  )
                                )
                                
                        ),
                        
                        tabItem(tabName = "Menu2",
          
                                fluidRow(
                                  box(title = "Some information before you start :", width = 12, status = "info", solidHeader = TRUE,
                                  withSpinner(textOutput("info1"), type = 7),
                                  withSpinner(textOutput("info2"), type = 7)
                                )),
                                
                                fluidRow(
                                  column(width = 6,
                                  
                                  box(title = "The different genres :", width = NULL, status = "info",
                                      
                                      actionBttn("anim",
                                                 label = "Animation",
                                                 style="pill",color="default"),
                                      actionBttn("sci",label = "Science Fiction", style="pill",color="default"),
                                      actionBttn("Comedy",label = "Comedy", style="pill",color="default"),
                                      actionBttn("Romance",label = "Romance", style="pill",color="default"),
                                      actionBttn("Action",label = "Action", style="pill",color="default"),
                                      actionBttn("Drama",label = "Drama", style="pill",color="default"),
                                      actionBttn("Adventure",label = "Adventure", style="pill",color="default"),
                                      actionBttn("War",label = "War", style="pill",color="default"),
                                      actionBttn("Fantasy",label = "Fantasy", style="pill",color="default"),
                                      actionBttn("Bio",label = "Biographical", style="pill",color="default"),
                                      actionBttn("Crime",label = "Crime", style="pill",color="default"),
                                      actionBttn("Thriller",label = "Thriller", style="pill",color="default"),
                                      actionBttn("Horror",label = "Horror", style="pill",color="default"),
                                      actionBttn("Music",label = "Musical", style="pill",color="default"),
                                      actionBttn("Family",label = "Family", style="pill",color="default"),
                                      actionBttn("Western",label = "Western", style="pill",color="default"),
                                      actionBttn("Mystery",label = "Mystery", style="pill",color="default"),
                                      actionBttn("Sport",label = "Sport", style="pill",color="default"),
                                      actionBttn("History",label = "History", style="pill",color="default"),
                                      actionBttn("Docu",label = "Documentary", style="pill",color="default"),
                                      actionBttn("Adult",label = "Adults", style="pill",color="default")
                                      
                                      ),
                                  
                                  
                                   valueBoxOutput("infobox1",width = NULL)
                                 
                                  ),
                                  
                                  column(width = 6,
                                  
                                  tabBox(width = NULL,
                                    tabPanel(
                                      "The ten highest rated films",
                                      tableOutput("best_note")
                                    ),
                                    tabPanel(
                                      "The ten lowest rated films",
                                      tableOutput("bad_note")
                                    )
                                  ))),
                                
                                
                                br(),
                                br(),
                                br(),
                                  
                                  fluidRow(
                                    
                                    column(width = 6, 
                                           pickerInput(
                                        inputId = "pays",
                                        label = "Select a country",
                                        choices = c("France","UK","Germany","USA",
                                                  "Canada","Belgium","Australia","Russia","Spain","Italy","Japan","China",
                                                  "Mexico","Denmark","Norway ","India","Grece","Turkey","Colombia",
                                                  "Cuba","Israel","Brazil","Algeria","Iran","Tunisia","Liban","Czechoslovakia","Poland","Uruguay",
                                                  "Venezuela","Afghanistan","Argentina","Switzerland"),
                                        options = list(
                                        `live-search` = TRUE, size=5, title ="No country selected")
                                    ),

                                    actionBttn("lancer2",label="Generate the country graph",style="minimal",
                                               color="success")
                                    ),
                                  
                                    column(width = 6, 
                                      pickerInput(
                                      inputId = "langue",
                                      label = "Select a language", 
                                      choices = c("French","English","German","Japanese", "Thai",
                                                  "Egyptian", "Arabic", "Hebrew", "Polish", "Vietnamese", "Korean",
                                                  "Bulgarian", "Swedish", "Danish", "Norwegian", "Hindi", "Croatian", "Albanian",
                                                  "Macedonian", "Spanish", "Italian", "Russian", "Greek", "Burmese", "Kurdish",
                                                  "Turkish", "Cantonese", "Swahili", "Zulu", "Wolof", "Portuguese", "Mandarin", "Chinese" ),
                                      options = list(
                                      `live-search` = TRUE, size=5, title ="No language selected")
                                  ),
                                  
                                    actionBttn("lancer",label="Generate the language graph",style="minimal",
                                               color="success"),
                                  
                                  useSweetAlert()
                                  
                                  )),
                              
                                  fluidRow(
                                    column(width = 6,
                                           box(width = NULL, status = "info",
                                               plotOutput("graph3")
                                           )
                                    ),

                                    column(width = 6,
                                           box(width = NULL, status = "info",

                                                      plotOutput("graph1")
                                           )
                                    )
                                  ),

                                br(),
                                br(),
                                br(),
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 6, 
                                         
                                         box( title = "Enter a year to know the number of films released during the decade of the selected year" , width = NULL, status = "info",
                                           searchInput(
                                             inputId = "annee",
                                             placeholder = "No year selected...",
                                             btnSearch = icon("search"), 
                                             btnReset = icon("remove"),
                                             width = "100%"
                                           ),
                                           
                                         textOutput("info3")
                                         
                                         )
                                ),
                              
                                
                                bsModal("plot_graph2", trigger = "annee_search", size = "large", 
                                        plotOutput("graph2"))
                              
                                )
                                
                                
                                ),
                        
                        tabItem(tabName = "Menu3",
                                
                                fluidRow(
                                  column(8, align = "center",
                                         h3("This part of the application allows you to clarify your movie choices."),
                                         br(),
                                         h5("Indeed, while today the offer of films does not stop growing, 
                                       it can be difficult to make a choice, and one can spend hours to find the right movie to watch."),
                                         h5("Let us guide you, enter your preferences, and you will find the rare pearl to watch!")
                                ), 
                                img(src = "film.png", height = 180, width = 230, style="display: block; margin-left: auto; margin-right: auto;")
                                ),
                                
                               br(),
                               br(),
                                
                                fluidRow(
                                  
                                  column(width = 6, 
                                         
                                         box(title = "Choose a film genre:", width = NULL, status = "warning", 
                                           
                                             pickerInput(
                                               inputId = "genre",
                                               label = NULL, 
                                               choices = c("Action", "Adventure", "Animation", "Comedy", "Crime", "Documentary", "Drama",
                                                           "Horror", "Romance", "War", "Adult", "History", "Sport",
                                                           "Mystery", "Sci-Fi", "Biography", "Musical",
                                                           "Family", "Western", "Thriller"),
                                               options = list(
                                                 title = "No genre selected ...")
                                             )
                                         ),
                                         
                                         box(title = "Choose a maximum duration: (in minutes)", br(), 
                                             "The duration of the proposed films cannot exceed the chosen duration", width = NULL,
                                          status = "warning", 
                                             
                                             sliderTextInput(
                                               inputId = "duree",
                                               label = NULL, 
                                               choices = c(seq(50,200,10)),
                                              grid = T
                                             )
                                             
                                             ),
                                         
                                         actionBttn(
                                           inputId = "choixfilm",
                                           label = "I want to know what movies are offered!",
                                           style = "material-flat",
                                           color = "warning")
                                  
                                    
                                        ),
                                  
                                  column(width = 6,
                                         
                                         box(title = "Choose a decade:", width = NULL, status = "warning", 
                                           br(),
                                           awesomeRadio(
                                             inputId = "year",
                                             label = NULL, 
                                             choices = c("1890","1900","1910","1920","1930","1940","1950","1960","1970",
                                                         "1980","1990","2000","2010","2020"
                                             ),
                                             selected = "1890"
                                           )
                                           
                                           
                                           
                                         )
                                         
                                         )
                                  
                                  
                                ),
                                
                             br(),
                                
                                fluidRow(
                                  
                                  column(8, align = "center",
                                         box( width = NULL,

                                           tableOutput("film")
                                         )
                                  )
                                    
                                  
                                  
                                  
                                ),
                                
                                fluidRow(
                                  
                                  column(width = 6,
                                         
                                         
                                         box(title = "First film:", width = NULL,
                                             
                                             htmlOutput("url_film1"),
                                             
                                             br(),
                                             
                                             textOutput("desc_film1"),
                                             br(),
                                             textOutput("act_film1"),
                                             br(),
                                             textOutput("prod_film1")
                                             
                                             
                                         ),
                                         
                                         
                                         
                                         box(title = "Third film :", width = NULL,
                                             htmlOutput("url_film3"),
                                             
                                             br(),
                                             
                                             textOutput("desc_film3"),
                                             br(),
                                             textOutput("act_film3"),
                                             br(),
                                             textOutput("prod_film3")
                                             
                                             
                                         ),
                                         
                                         box(title = "Fifth film:",width = NULL,
                                             htmlOutput("url_film5"),
                                             
                                             br(),
                                             
                                             textOutput("desc_film5"),
                                             br(),
                                             textOutput("act_film5"),
                                             br(),
                                             textOutput("prod_film5")
                                             
                                             
                                         )
                                         
                                  ),
                                  
                                  column(width = 6,
                                         box(title = "Second film:", width = NULL,
                                             htmlOutput("url_film2"),
                                             
                                             br(),
                                             
                                             textOutput("desc_film2"),
                                             br(),
                                             textOutput("act_film2"),
                                             br(),
                                             textOutput("prod_film2")
                                             
                                             
                                         ),
                                         
                                 
                                         
                                         box(title = "Fourth film:", width = NULL,
                                             htmlOutput("url_film4"),
                                             
                                             br(),
                                             
                                             textOutput("desc_film4"),
                                             br(),
                                             textOutput("act_film4"),
                                             br(),
                                             textOutput("prod_film4")
                                             
                                             
                                         )
                            
                                    
                                  )
                                  
                                  
                                 
                                )
                                
                
                                
                          
                        ),
                        tabItem(tabName = "Menu4",
                                
                                fluidRow(
                                  column(8, align = "center",
                                  h3("This part focuses on the celebrities of the film world."),
                                  br(),
                                  h5("Do you have a favorite actor? Two favorite actors? A favorite director? Find here the movies you can watch to find these celebrities in action!")
                                  
                                ),
                                img(src = "fame.png", height = 180, width = 230, style="display: block; margin-left: auto; margin-right: auto;")
                                ),
                                
                                br(),
                                br(),
                                br(),
                                
                                fluidRow(
                                  
                                  column(8, align = "center",
                                         
                                         
                                         box(width = NULL,title = "What would you like to do?",
                                         prettyRadioButtons(
                                           inputId = "choix",
                                           label = NULL,
                                           choices = c("See the films of one actor only", "See if two actors have movies in common"),
                                           icon = icon("check"), 
                                           bigger = TRUE,
                                           status = "success",
                                           animation = "jelly"
                                         )
                                         ),
                                  )
                                  ),
                                  
                                  fluidRow(
                                   
                                           uiOutput("choix1"),
                                           uiOutput("choix12"),
                                           uiOutput("choix2"),
                                           uiOutput("choix21"),
                                           uiOutput("choix22")
                                           
                                           
                                  ),
                                
                                
                                fluidRow(
                                  
                                 column(8, align = "center",
                                     withSpinner(tableOutput("tab_choix1"),6, color = "blue")
                                     ),
                                     
                                 column(8, align = "center", 
                                        withSpinner(tableOutput("tab_choix2"),6, color = "blue")
                                 )
                                  
                                ),
                                
                                
                                  fluidRow(
                                    column(8, align = "center",
                                           
                                    box(width = NULL, title = "List of directors",
                                    pickerInput(
                                      inputId = "directors",
                                      label = NULL,
                                      choices = c("Ernst Lubitsch", "Marcel L'Herbier", "Arnold Fanck", "Alfred Hitchcock", "Harry Beaumont",
                                                  "H.C. Potter", "Jean Renoir", "John Ford", "Luther Reed", "Ivan Pyrev", "Charles Chauvel",
                                                  "Walter Lang", "James Edward Grant", "Mark Donskoy", "Edward Ludwig", "Jack Lee", "Jacques Becker",
                                                  "Robert Allen", "Mark Sandrich", "Gianni Franciolini", "Alfredo Guarini", "Robert Z. Leonard",
                                                  "Henri Verneuil", "Fritz Lang", "Terence Young", "Aarne Tarkas", "Irwin Allen", "Phil Karlson",
                                                  "Kurt Wimmer", "Kurt Anderson", "Jean-Paul Lilienfeld", "Helen De Michiel", "Charlotte Silvera",
                                                  "Lisa Gottlieb", "Dimitri de Clercq", "Alain Robbe-Grillet", "Bronwen Hughes", "Monique Gardenberg",
                                                  "David Douglas", "Tim Douglas", "Tomas Alfredson", "David O. Russell", "Paul Donovan", "David Mitchell",
                                                  "Alejandro Pelayo", "Patrick Alan", "Giancarlo Soldi", "Bob Rafelson", "Ryszard Bugajski", "Michael Lehmann",
                                                  "Chia Yung Liu", "Steven Soderbergh", "Claude Chabrol", "Joel Silberg", "Sigi Rothemund", "Robin Davis",
                                                  "Alain Delon", "Larry Peerce", "Bruno Corbucci","Quentin Tarantino",  "Umberto Lenzi", "Alberto De Martino", "Mel Stuart",
                                                  "Bernardo Bertolucci", "Hugo Grimaldi", "Arthur C. Pierce", "Robert Stevenson", "Marcel Camus",
                                                  "James Ivory", "Robert Montgomery", "Walter Lantz"),
                                      options = list(
                                        `live-search` = TRUE, size=5, title ="No director selected")
                                    )),

                                    actionBttn("lancer5",label=" See the films of this director", style="stretch",
                                               color="success")

                                  )),


                                  fluidRow(
                                    
                                    column(8, align = "center",
                                   
                                        withSpinner(tableOutput("table_directors"), 5, color = "blue")
                                    )
       
                                 
                                )
                                
                                
                      
                    ))
                      )
)
            
           

