server <- function(input,output, session){
  
 
  
data <- eventReactive(input$go ,{

  read.csv("data/movies.csv")
 
})


output$info1 <- renderPrint({
  nombre = nrow(data())
  year1 <- data() %>% arrange(year) %>% select(year) %>%  slice(1) 
  year_last <- data() %>% arrange(desc(year)) %>% select(year) %>% slice(1)
  cat("The application has",nombre, "films, for a period from",year1[1,1], " to ",year_last[1,1], ".")
})
  
output$info2 <- renderPrint({
  unique_c <- length(unique(data()$country))
  unique_l <- length(unique(data()$language))
  cat("Almost",unique_c,"countries are represented, as well as",unique_l,"original languages")
})
  

rv <- reactiveValues()

observeEvent(input$anim, {

  rv$cat <- data() %>% filter(genre == "Animation") 
})

observeEvent(input$sci, {
  rv$cat <- data() %>% filter(genre == "Sci-Fi") 
})

observeEvent(input$Comedy, {
  rv$cat <- data() %>% filter(genre == "Comedy") 
})

observeEvent(input$Romance, {
  rv$cat <- data() %>% filter(genre == "Romance") 
})

observeEvent(input$Action, {
  rv$cat <- data() %>% filter(genre == "Action") 
})

observeEvent(input$Drama, {
  rv$cat <- data() %>% filter(genre == "Drama") 
})

observeEvent(input$Adventure, {
  rv$cat <- data() %>% filter(genre == "Adventure") 
})

observeEvent(input$War, {
  rv$cat <- data() %>% filter(genre == "War") 
})

observeEvent(input$Fantasy, {
  rv$cat <- data() %>% filter(genre == "Fantasy") 
})

observeEvent(input$Bio, {
  rv$cat <- data() %>% filter(genre == "Biography") 
})

observeEvent(input$Crime, {
  rv$cat <- data() %>% filter(genre == "Crime") 
})

observeEvent(input$Thriller, {
  rv$cat <- data() %>% filter(genre == "Thriller") 
})

observeEvent(input$Horror, {
  rv$cat <- data() %>% filter(genre == "Horror") 
})

observeEvent(input$Music, {
  rv$cat <- data() %>% filter(genre == "Music" | genre == "Musical") 
 })

observeEvent(input$Family, {
  rv$cat <- data() %>% filter(genre == "Family") 
})

observeEvent(input$Western, {
  rv$cat <- data() %>% filter(genre == "Western") 
})

observeEvent(input$Mystery, {
  rv$cat <- data() %>% filter(genre == "Mystery") 
})

observeEvent(input$Sport, {
  rv$cat <- data() %>% filter(genre == "Sport") 
})

observeEvent(input$History, {
  rv$cat <- data() %>% filter(genre == "History") 
})

observeEvent(input$Docu, {
  rv$cat <- data() %>% filter(genre == "Documentary") 
})

observeEvent(input$Adult, {
  rv$cat <- data() %>% filter(genre == "Adult") 
})
  
  
output$best_note <- renderTable({
  
validate(
  need(rv$cat !="","Please choose a genre or login to the homepage !")
)
  
  rv$cat %>% arrange(desc(avg_vote)) %>% select(title, year) %>% slice(1:10) %>% rename("Film title" = title, "Year of release" = year)
  
})
  

output$bad_note <- renderTable({
  
  validate(
    need(rv$cat !="","Please choose a genre or login to the homepage !")
  )
  rv$cat %>% arrange(avg_vote) %>% select(title, year) %>% slice(1:10) %>% rename("Film title" = title, "Year of release" = year)
  
})

output$infobox1 <- renderInfoBox({
  nbre <- nrow(rv$cat)
  cat <- rv$cat[1,"genre"]
  titre <- paste("Number of films in the genre", cat)
  valueBox(width = NULL,
    nbre,
    titre,
    icon = icon("film", lib = "glyphicon"))
})

data2 <- eventReactive(input$lancer, {
  
  data() %>% filter(str_detect(language, input$langue)) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # if(input$langue == "Francais"){
  #  data() %>% filter(str_detect(language,"French")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Anglais"){
  #   data() %>% filter(str_detect(language,"English")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Allemand"){
  #   data() %>% filter(str_detect(language, "Dutch") | str_detect(language,"German")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if (input$langue == "Japonais"){
  #   data() %>% filter(str_detect(language,"Japanese")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Thailandais"){
  #   data() %>% filter(str_detect(language,"Thai")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Egyptien"){
  #   data() %>% filter(str_detect(language,"Egyptian ")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Arabe"){
  #    data() %>% filter(str_detect(language,"Arabic")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Hebreu"){
  #   data() %>% filter(str_detect(language,"Hebrew")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Polonais"){
  #   data() %>% filter(str_detect(language,"Polish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Vietnamien"){
  #   data() %>% filter(str_detect(language,"Vietnamese")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Coreen"){
  #   data() %>% filter(str_detect(language,"Korean")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Bulgare"){
  #    data() %>% filter(str_detect(language,"Bulgarian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Suedois"){
  #    data() %>% filter(str_detect(language,"Swedish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Danois"){
  #     data() %>% filter(str_detect(language,"Danish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Norvegien"){
  #    data() %>% filter(str_detect(language,"Norwegian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Hindou"){
  #    data() %>% filter(str_detect(language,"Hindi")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Croate"){
  #    data() %>% filter(str_detect(language,"Croatian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Albanais"){
  #    data() %>% filter(str_detect(language,"Albanian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Macedonien"){
  #    data() %>% filter(str_detect(language,"Macedonian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Espagnol"){
  #    data() %>% filter(str_detect(language,"Spanish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Italien"){
  #    data() %>% filter(str_detect(language,"Italian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Russe"){
  #    data() %>% filter(str_detect(language,"Russian")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Grec"){
  #     data() %>% filter(str_detect(language,"Greek")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Birman"){
  #    data() %>% filter(str_detect(language,"Burmese")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Kurde"){
  #    data() %>% filter(str_detect(language,"Kurdish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Turc"){
  #    data() %>% filter(str_detect(language,"Turkish")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Cantonais"){
  #    data() %>% filter(str_detect(language,"Cantonese")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Swahili"){
  #    data() %>% filter(str_detect(language,"Swahili")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Zulu"){
  #    data() %>% filter(str_detect(language,"Zulu")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Wolof"){
  #    data() %>% filter(str_detect(language,"Wolof")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Portuguais"){
  #    data() %>% filter(str_detect(language,"Portuguese")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$langue == "Chinois"){
  #    data() %>% filter(str_detect(language,"Chinese"))%>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }
})



output$graph1 <- renderPlot({

    ggplot(data2(),aes(x = reorder(genre, -table(genre)[genre]), fill = genre)) +
    geom_bar(width = 0.3) + labs(x = "Genre", y = "Number") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Number of films by genre containing the language", input$langue))

})





data4 <- eventReactive(input$lancer2, {
  
  data() %>% filter(str_detect(country, input$pays)) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # if(input$pays == "France"){
  #   data() %>% filter(str_detect(country,"France")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Royaume Uni"){
  #   data() %>% filter(str_detect(country,"UK")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Allemagne"){
  #   data() %>% filter(str_detect(country, "Germany") | str_detect(language,"West Germany")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if (input$pays == "Etats Unis"){
  #   data() %>% filter(str_detect(country,"USA")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Canada"){
  #   data() %>% filter(str_detect(country,"Canada")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Belgique"){
  #   data() %>% filter(str_detect(country,"Belgium ")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Australie"){
  #   data() %>% filter(str_detect(country,"Australia")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Russie"){
  #   data() %>% filter(str_detect(country,"Russia")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Espagne"){
  #   data() %>% filter(str_detect(country,"Spain")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Italie"){
  #   data() %>% filter(str_detect(country,"Italy")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Japon"){
  #   data() %>% filter(str_detect(country,"Japan")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Chine"){
  #   data() %>% filter(str_detect(country, "China") | str_detect(country,"Hong Kong")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Mexique"){
  #   data() %>% filter(str_detect(country,"Mexico")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Danemark"){
  #   data() %>% filter(str_detect(country,"Denmark")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Norvege"){
  #   data() %>% filter(str_detect(country,"Norway")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Inde"){
  #   data() %>% filter(str_detect(country,"India")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Grece"){
  #   data() %>% filter(str_detect(country,"Grece")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Turquie"){
  #   data() %>% filter(str_detect(country,"Turkey")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Colombie"){
  #   data() %>% filter(str_detect(country,"Colombia")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Cuba"){
  #   data() %>% filter(str_detect(country,"Cuba")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Israel"){
  #   data() %>% filter(str_detect(country,"Israel")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Bresil"){
  #   data() %>% filter(str_detect(country,"Brazil")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Algerie"){
  #   data() %>% filter(str_detect(country,"Algeria")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Iran"){
  #   data() %>% filter(str_detect(country,"Iran")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Tunisie"){
  #   data() %>% filter(str_detect(country,"Tunisia")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Liban"){
  #   data() %>% filter(str_detect(country,"Lebanon")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Tchecoslovaquie"){
  #   data() %>% filter(str_detect(country,"Czechoslovakia")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Pologne"){
  #   data() %>% filter(str_detect(country,"Poland")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Uruguay"){
  #   data() %>% filter(str_detect(country,"Uruguay")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Venezuela"){
  #   data() %>% filter(str_detect(country,"Venezuela")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Suisse"){
  #   data() %>% filter(str_detect(country,"Switzerland")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Afghanistan"){
  #   data() %>% filter(str_detect(country,"Afghanistan")) %>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }else if(input$pays == "Argentine"){
  #   data() %>% filter(str_detect(country,"Argentina"))%>% filter(genre %in% c("Action","Animation","Drama","Comedy","Romance","Horror","History"))
  # }
})

observeEvent(input$lancer2, {
  sendSweetAlert(
    session = session,
    title = "Information",
    text = "For the sake of visibility, only 7 main film genres are represented in the chart.",
    type = "info"
  )
})

output$graph3 <- renderPlot({

  ggplot(data4(),aes(x = reorder(genre, -table(genre)[genre]), fill = genre)) +
    geom_bar(width = 0.3) + labs(x = "Genre", y = "Number") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Number of films by genre in the country: ", input$pays))

})


annee <- reactive({
  as.numeric(input$annee)
})

output$info3 <- renderPrint({
decennie <- paste0(annee() - annee() %% 10,"-",(annee() - annee() %% 10 + 10))

nbre <- nrow(data() %>% filter(decennies == decennie))

cat("During the decade ",decennie, nbre, " films were released in theaters. ")
})


observeEvent(input$annee_search, {
  
  rv$annee1 <- annee() - annee() %% 10
  rv$annee2 <- annee() - annee() %% 10 + 10
  
  rv$decennie <- paste0(annee() - annee() %% 10,"-",(annee() - annee() %% 10 + 10))
  
  rv$nbre_film <- nrow(data() %>% filter(year == annee() ))
  rv$data_graph <- data() %>% filter(year >= rv$annee1 & year <= rv$annee2)
  
})

output$graph2 <- renderPlot({ 
  
  rv$data_graph %>% group_by(year) %>% mutate(nbr = n()) %>% 
    ggplot(aes(x = year, y = nbr)) + geom_point(colour = "cornflowerblue") + geom_line() +
    geom_text(aes(label = annee(), x = annee(), y = rv$nbre_film, vjust = -0.4)) +
    scale_x_continuous(breaks = rv$annee1:rv$annee2) + theme_update() + labs(x = "Year during the selected decade", y = "Number of films released") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5) ) +
   
    ggtitle(paste("Evolution of the number of films released during the decade",rv$decennie))
  
  
})

duree <- reactive({
  as.numeric(input$duree)
})


data3 <- eventReactive( input$choixfilm, {
  
  data() %>% filter(genre == input$genre & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  
  # if (input$genre == "Action"){
  #   data() %>%  filter(genre == "Action" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Aventure"){
  #   data() %>% filter(genre == "Adventure" & duration <= duree() & str_detect(decennies,input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Comedie"){
  #   data() %>% filter(genre == "Comedy" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Crime"){
  #   data() %>% filter(genre == "Crime" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Documentaire"){
  #   data() %>% filter(genre == "Documentary" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Dramatique"){
  #   data() %>% filter(genre == "Drama" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Horreur"){
  #   data() %>% filter(genre == "Horror" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Romance"){
  #   data() %>% filter(genre == "Romance" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Guerre"){
  #   data() %>% filter(genre == "War" & duration <= duree() & str_detect(decennies,input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Adultes"){
  #   data() %>% filter(genre == "Adult" & duration <= duree() & str_detect(decennies,input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Historique"){
  #   data() %>% filter(genre == "History" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Sportif"){
  #   data() %>% filter(genre == "Sport" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Mystere"){
  #   data() %>% filter(genre == "Mystery" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Sicence-Fiction"){
  #   data() %>% filter(genre == "Sci-Fi" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Biographique"){
  #   data() %>% filter(genre == "Biography" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Musical"){
  #   data() %>% filter(genre == "Musical" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Famille"){
  #   data() %>% filter(genre == "Family" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Western"){
  #   data() %>% filter(genre == "Western" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Thriller"){
  #   data() %>% filter(genre == "Thriller" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }else if (input$genre == "Animation"){
  #   data() %>% filter(genre == "Animation" & duration <= duree() & str_detect(decennies, input$year)) %>% arrange(desc(avg_vote)) %>% slice(1:5)
  # }
  # 
})


output$film <- renderTable({
  if(nrow(data3()) != 0){
  data3() %>% select(title, year, avg_vote, duration) %>% rename ("Proposed films" = title, "Year of release" = year, "Rating /10" = avg_vote, "Duration (in minutes)" = duration)
  }else{
    data3() %>% mutate("No film in the criteria!" = "No film!") %>% select("No film in the criteria!")
  }
})


output$url_film1 <- renderUI({
  if(nrow(data3()) >= 1){
    film1 <- data3() %>% select(title)
    film1 <- film1[1,1]
    tags$a(href = paste0("https://www.youtube.com/results?search_query=",film1,"+trailer+"), paste("See the trailer of ", film1))
  }
  
  
})
output$desc_film1 <- renderPrint({
  if(nrow(data3()) >= 1){
  film11 <- data3() %>% select(title, description) 
  film11[is.na(film11)] = "No description available"
  desc <- film11[1,2]
  cat("Description du film :",desc)
  }
})

output$act_film1 <- renderPrint({
  if(nrow(data3()) >= 1){
  film11 <- data3() %>% select(title, actors) 
  film11[is.na(film11)] = "Inconnus"
  act <- film11[1,2]
  cat("Acteurs :",act)
  }
})

output$prod_film1 <- renderPrint({
  if(nrow(data3()) >= 1){
    film11 <- data3() %>% select(title,production_company)
    film11[is.na(film11)] = "Production inconnue"
    prod <- film11[1,2]
    cat("Compagnie de production :", prod)
  }
})

# film 2 :

output$url_film2 <- renderUI({
  if(nrow(data3()) >= 2){
    film2 <- data3() %>% select(title)
    film2 <- film2[2,1]
    tags$a(href = paste0("https://www.youtube.com/results?search_query=",film2,"+trailer+"), paste("See the trailer of ", film2))
  }
  
  
})


output$desc_film2 <- renderPrint({
  if(nrow(data3()) >= 2){
  film22 <- data3() %>% select(title, description)
  film22[is.na(film22)] = "No description available"
  desc <- film22[2,2]
  cat("Description of the film :",desc)
  }
})

output$act_film2 <- renderPrint({
  if(nrow(data3()) >= 2){
  film22 <- data3() %>% select(title, actors)
  film22[is.na(film22)] = "Unknown"
  act <- film22[2,2]
  cat("Actors :",act)
  }
})

output$prod_film2 <- renderPrint({
  if(nrow(data3()) >= 1){
    film22 <- data3() %>% select(title,production_company)
    film22[is.na(film22)] = "Unknown production"
    prod <- film22[2,2]
    cat("Production Company :", prod)
  }
})


# film 3 : 

output$url_film3 <- renderUI({
  if(nrow(data3()) >= 3){
    film3 <- data3() %>% select(title)
    film3 <- film3[3,1]
    tags$a(href = paste0("https://www.youtube.com/results?search_query=",film3,"+trailer+"), paste("See the trailer of ", film3))
  }
  
  
})

output$desc_film3 <- renderPrint({
  if(nrow(data3()) >= 3){
  film33 <- data3() %>% select(title, description)
  film33[is.na(film33)] = "No description available"
  desc <- film33[3,2]
  cat("Description of the film :",desc)
  }
})

output$act_film3 <- renderPrint({
  if(nrow(data3()) >= 3){
  film33 <- data3() %>% select(title, actors)
  film33[is.na(film33)] = "Unknown"
  act <- film33[3,2]
  cat("Actors :",act)
  }
})

output$prod_film3 <- renderPrint({
  if(nrow(data3()) >= 1){
    film33 <- data3() %>% select(title,production_company)
    film33[is.na(film33)] = "Unknown production "
    prod <- film33[3,2]
    cat("Production Compangy :", prod)
  }
})


# film 4 : 

output$url_film4 <- renderUI({
  if(nrow(data3()) >= 4){
    film4 <- data3() %>% select(title)
    film4 <- film4[4,1]
    tags$a(href = paste0("https://www.youtube.com/results?search_query=",film4,"+trailer+"), paste("See the trailer of ", film4))
  }
  
  
})
output$desc_film4 <- renderPrint({
  if(nrow(data3()) >= 4){
  film44 <- data3() %>% select(title, description)
  film44[is.na(film44)] = "No description available"
  desc <- film44[4,2]
  cat("Description of the film :",desc)
  }
})

output$act_film4 <- renderPrint({
  if(nrow(data3()) >= 4){
  film44 <- data3() %>% select(title, actors)
  film44[is.na(film44)] = "Unknown"
  act <- film44[4,2]
  cat("Actors :",act)
  }
})

output$prod_film4 <- renderPrint({
  if(nrow(data3()) >= 1){
    film44 <- data3() %>% select(title,production_company)
    film44[is.na(film44)] = "Unknown production "
    prod <- film44[4,2]
    cat("Production Compagny :", prod)
  }
})

# film 5 :

output$url_film5 <- renderUI({
  if(nrow(data3()) >= 5){
    film5 <- data3() %>% select(title)
    film5 <- film5[5,1]
    tags$a(href = paste0("https://www.youtube.com/results?search_query=",film5,"+trailer+"), paste("See the trailer of ", film5))
  }
  
  
})

output$desc_film5 <- renderPrint({
  if(nrow(data3()) >= 5){
  film55 <- data3() %>% select(title, description)
  film55[is.na(film55)] = "No description available"
  desc <- film55[5,2]
  cat("Description of the film :",desc)
  }
})

output$act_film5 <- renderPrint({
  if(nrow(data3()) >= 5){
  film55 <- data3() %>% select(title, actors)
  film55[is.na(film55)] = "Unknown"
  act <- film55[5,2]
  cat("Actors :",act)
  }
})

output$prod_film5 <- renderPrint({
  if(nrow(data3()) >= 1){
    film55 <- data3() %>% select(title,production_company)
    film55[is.na(film55)] = "Unknown production "
    prod <- film55[5,2]
    cat("Production compagny:", prod)
  }
})

observeEvent( input$choixfilm, {
  
  showNotification("Attention, clicking on see the trailer redirects to YouTube, back to the application and reconnect", type = "warning")

})


output$choix1 <- renderUI({
  
  switch (input$choix,
         
          "See the films of one actor only" =   box(title ="List of actors", width = 6,
            pickerInput(
            inputId = "acteurs",
            choices = c("Tudor Owen","Charlotte Gainsbourg", "Laure Calamy","Jean Marais","Lino Ventura","Catherine Deneuve","Alain Delon","Isabelle Huppert","Fanny Ardent","Daniel Auteuil",
                        "Fabrice Luchini","Jean Reno","Gad Elmaleh","Sandrine Kiberlain","Didier Bourdon","Muriel Robin","Marion Cotillard",
                        "Dennis Martin","Peter Moon","Jean-Claude Van Damme","Woody Allen","James Franco","Sarah Jones","Jane Birkin","Eddie Murphy",
                        "Natalie Portman","Lily-Rose Depp","Sandra Oh","Camille Montgomery","Justin Xavier","Daniel Cooper","Tom Kennedy","Richard Martin","Tom Hanks",
                        "Tim Allen","Meg Ryan","Carrie Fisher","Julia Roberts","Amy Adams","Ben Foster",
                        "Brad Pitt","Cameron Diaz","Angelina Jolie","Matt Damon", "Leonardo DiCaprio","Kate Winslet","Margot Robbie","Diane Kruger","Cate Blanchett",
                        "Johnny Depp","Robert Pattinson","Sacha Baron Cohen","Orlando Bloom","Daniel Radcliffe","Emma Watson","Vincent Lacoste"
                        
            ),
            options = list(
              `live-search` = TRUE, size=5, title ="No selected actor")
          )
          )
  )
  
})

output$choix12 <- renderUI({

  switch (input$choix,       
          "See the films of one actor only" =  actionBttn(
              inputId = "filmacteur1",
              label = "See the films of this actor!",
              style = "bordered",
              color = "warning",
              icon = icon("film", lib = "glyphicon")
            )
          
)
})


output$choix2 <- renderUI({
  
  
  switch (input$choix,
          "See if two actors have movies in common" =  box(title ="List of actors 1", width = 6,
                                                                 pickerInput(
                                                                   inputId = "acteur1",
                                                                   choices = c("Tudor Owen","Charlotte Gainsbourg", "Laure Calamy","Jean Marais","Lino Ventura","Catherine Deneuve","Alain Delon","Isabelle Huppert","Fanny Ardent","Daniel Auteuil",
                                                                               "Fabrice Luchini","Jean Reno","Gad Elmaleh","Sandrine Kiberlain","Didier Bourdon","Muriel Robin","Marion Cotillard",
                                                                               "Dennis Martin","Peter Moon","Jean-Claude Van Damme","Woody Allen","James Franco","Sarah Jones","Jane Birkin","Eddie Murphy",
                                                                               "Natalie Portman","Lily-Rose Depp","Sandra Oh","Camille Montgomery","Justin Xavier","Daniel Cooper","Tom Kennedy","Richard Martin","Tom Hanks",
                                                                               "Tim Allen","Meg Ryan","Carrie Fisher","Julia Roberts","Amy Adams","Ben Foster",
                                                                               "Brad Pitt","Cameron Diaz","Angelina Jolie","Matt Damon", "Leonardo DiCaprio","Kate Winslet","Margot Robbie","Diane Kruger","Cate Blanchett",
                                                                               "Johnny Depp","Robert Pattinson","Sacha Baron Cohen","Orlando Bloom","Daniel Radcliffe","Emma Watson","Vincent Lacoste"
                                                                               
                                                                   ),
                                                                   options = list(
                                                                     `live-search` = TRUE, size=5, title ="No selected actor")
                                                                 )
          )
  )
})


output$choix21 <- renderUI({
  
  
  switch (input$choix,
          "See if two actors have movies in common" =  box(title ="List of actors 2", width = 6,
                                                                 pickerInput(
                                                                   inputId = "acteur2",
                                                                   choices = c("Tudor Owen","Charlotte Gainsbourg", "Laure Calamy","Jean Marais","Lino Ventura","Catherine Deneuve","Alain Delon","Isabelle Huppert","Fanny Ardent","Daniel Auteuil",
                                                                               "Fabrice Luchini","Jean Reno","Gad Elmaleh","Sandrine Kiberlain","Didier Bourdon","Muriel Robin","Marion Cotillard",
                                                                               "Dennis Martin","Peter Moon","Jean-Claude Van Damme","Woody Allen","James Franco","Sarah Jones","Jane Birkin","Eddie Murphy",
                                                                               "Natalie Portman","Lily-Rose Depp","Sandra Oh","Camille Montgomery","Justin Xavier","Daniel Cooper","Tom Kennedy","Richard Martin","Tom Hanks",
                                                                               "Tim Allen","Meg Ryan","Carrie Fisher","Julia Roberts","Amy Adams","Ben Foster",
                                                                               "Brad Pitt","Cameron Diaz","Angelina Jolie","Matt Damon", "Leonardo DiCaprio","Kate Winslet","Margot Robbie","Diane Kruger","Cate Blanchett",
                                                                               "Johnny Depp","Robert Pattinson","Sacha Baron Cohen","Orlando Bloom","Daniel Radcliffe","Emma Watson","Vincent Lacoste"
                                                                               
                                                                   ),
                                                                   options = list(
                                                                     `live-search` = TRUE, size=5, title = "No selected actor")
                                                                 )
          )
  )
})

output$choix22 <- renderUI({
  
  switch (input$choix,
          "See if two actors have movies in common" =
            actionBttn(
              inputId = "filmcommun",
              label = "See the common film!",
              style = "bordered",
              color = "warning",
              icon = icon("film", lib = "glyphicon")
            )
          
            
            
  )
  
})

data_choix1 <- eventReactive(input$filmacteur1, {
  
  data() %>% filter(str_detect(actors,input$acteurs))
  
})

output$tab_choix1 <- renderTable({
  data_choix1() %>% arrange(desc(avg_vote)) %>% select(original_title) %>% rename("Top 10 films" = original_title) %>% slice(1:10)
})


data_choix2 <- eventReactive(input$filmcommun, {
  data() %>% filter(str_detect(actors,input$acteur1) & str_detect(actors,input$acteur2))
})

output$tab_choix2 <- renderTable({
 
  data_choix2() %>% select(original_title) %>% rename("Film in common" = original_title)

})



data5 <- eventReactive(input$lancer5, {
  data() %>%  filter(str_detect(director,input$directors)) %>% arrange(desc(avg_vote)) %>% slice(1:10)
})

output$table_directors <- renderTable({
  
  data5()  %>% select(original_title) %>% rename("Top 10 films" = original_title)
})

}