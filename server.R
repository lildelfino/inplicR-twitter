library(shinydashboard)
library(DT)
library(rtweet)
library(httr)
library(purrr)
library(graphics)
library(stringr)
library(tm)
library(syuzhet)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(SPARQL)
library(splus2R)
library(textclean)
library(RMySQL)

####################################################################################################################################
# Text mining #
###############

#' Create and clean a corpus from a list of publications
#'
#' @param list, can be a list of tweet or fb publication
#' @param lang, "fr", "en" or "es"
#'
#' @return the corpus tokenized
#' @export
#'
#' @examples createCorpus(tweets, "fr")
createCorpus <- function(list, lang){
  corpus <- tm::Corpus(tm::VectorSource(list$text))


  corpus <- tm::tm_map(corpus, tm::content_transformer(base::tolower))
  corpus <- tm::tm_map(corpus, tm::removeWords,tm::stopwords(lang))

  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removePunctuation)

  removeURL <- function(x) base::gsub("http[[:alnum:]]*", "", x)
  corpus <- tm::tm_map(corpus, tm::content_transformer(removeURL))

  removeNonAscii <- function(x) textclean::replace_non_ascii(x)
  corpus <- tm::tm_map(corpus, tm::content_transformer(removeNonAscii))

  return(corpus)
}

#' Add a stop words to be removed from a corpus
#'
#' @param sw stop word
#' @param corpus a corpus created with createCorpus() function
#'
#' @return a corpus without the stopwords put in param
#' @export
#'
#' @examples corpusclean <- removeStopWord(corpus,"de")
removeStopWord <- function(corpus, sw){
  corpus <- tm::tm_map(corpus, tm::removeWords, c(sw))
  return(corpus)
}

#' transform a corpus in a data frame of words with their frequency
#'
#' @param corpus created with createCorpus()
#'
#' @return data frame with words and the occurrence they appear in the corpus
#' @export
#'
#' @examples dataWords <- wordsToData(corpus)
wordsToData <- function(corpus){
  dtm <- tm::TermDocumentMatrix(corpus)
  m <- base::as.matrix(dtm)
  v <- base::sort(base::rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

#' Create a word cloud from a frame of words and frequency
#'
#' @param data dataframe created with wordsToData()
#' @param maxWords number max of words displayed
#' @param minFreq minimum frequency of a word to be displayed on the wword cloud
#' @param random TRUE or FALSE ; if FALSE the words with more frequencies will be displayed on the center of the cloud
#' @param rotation 0.0 to 1.0 ; 0 to make all words straight and more to make some words turned by 90 degrees
#'
#' @return
#' @export
#'
#' @examples wordcloud(data,100,2,FALSE,0.3)
wordcloud <- function(data, maxWords, minFreq, random, rotation){
  set.seed(1234)
  wordcloud::wordcloud(words = data$word, freq = data$freq, min.freq = minFreq,
                       max.words=maxWords, random.order=random, rot.per=rotation,
                       colors=RColorBrewer::brewer.pal(8, "Accent"))
}

#' Sentiment analysis of a corpus
#'
#' @param corpus
#' @param lang language of the corpus : "english" ,"french", or "spanish"
#'
#' @return
#' @export
#'
#' @examples sentimentAnalysis(corpus, "french")
sentimentAnalysis <- function(corpus, lang){
  method <- "nrc"
  emotions <- syuzhet::get_nrc_sentiment(corpus$content, language = lang)
  graphics::barplot(colSums(emotions),cex.names = .7,
                    col = c("firebrick","darkmagenta","darkolivegreen","orange","palevioletred1","grey","yellow1","royalblue","red","limegreen"),
                    main = "Sentiment Analysis"
  )
  return(emotions)
}

#####################################################################################################################################

######################################################################################################################################
# Twitter #
###########



#' Twitter token with generic API keys
#'
#' @param appname https://developer.twitter.com/
#' @param api_key https://developer.twitter.com/
#' @param api_secret https://developer.twitter.com/
#' @param access_token https://developer.twitter.com/
#' @param access_token_secret https://developer.twitter.com/
#'
#' @return the token
#' @export
twitterToken <- function(appname, api_key, api_secret, access_token, access_token_secret){
  return(rtweet::create_token(app = appname, api_key, api_secret, access_token = access_token, access_secret = access_token_secret))
}

#' Search twitter by combining each word of the sparql list with a single keyword with an "AND"
#'
#' @param kw of words in the sparql list
#' @param nbr number max of tweets returned
#' @param geo the second keyword
#'
#' @return data frame of all tweets
#' @export
#'
#' @examples tweetsSparql <- searchTwitterSparql(requeteCrisis("inondation"),100,"Troyes")
searchTwitterCombineGeo <- function(kw, nbr, geo, token){
  tweets <- data.frame()
  #si c'est un géocode => recherche localisée (https://www.coordonnees-gps.fr/)
  # si c'est un lieu ou un mot : recherche par combinaison
  # si geocode non remplis, "0" pour ne pas faire une recherche combinée ou geocodée
  if(str_detect(as.String(geo),"[0-999]\\.[0-999]")==TRUE){
    tweets <- rtweet::search_tweets(kw,nbr,include_rts = FALSE,geocode = geo, lang= "fr", token = token)
  }else if(as.String(geo)=="0"){
    tweets <- rtweet::search_tweets(kw,nbr,include_rts = FALSE, lang = "fr", token = token)
  }else if(as.String(geo)!=""){
    tweets <- rtweet::search_tweets(paste0(as.String(kw)+" AND "+as.String(geo)),nbr,include_rts = FALSE, lang = "fr", token = token)
  }else{
    tweets <- rtweet::search_tweets(as.String(kw),nbr,include_rts = FALSE, lang = "fr", token = token)
  }

  return(tweets)
}

#' Search on twitter with a three words max at the same time
#' can also return a timeline if the input contains @
#'
#' @param kw1 principal keyword
#' @param geo geocode, or word reffering to a geolocation (we can put another kw but that's not the purpose)
#' @param kw2 secondary keyword
#' @param nbr number max of tweets extracted
#'
#' @return a list of tweets
#' @export
#'
#' @examples tweets <- searchTwitterTwoKWgeo("covid","Lille","peur",100)
searchTwitterTwoKWgeo <- function(kw1, geo, kw2, nbr, token){

  if(str_detect(as.String(kw1),"@")){
    tweets <- rtweet::get_timeline(kw1,nbr, token = token)
  }else if(kw2==""){
    tweets <- searchTwitterCombineGeo(kw1,nbr,geo, token = token)
  }else{
    tweets <- searchTwitterCombineGeo(paste0(as.String(kw1)+" AND "+as.String(kw2)),nbr,geo, token = token)
  }

  return(tweets)
}

####################################################################################################################################

#######################################################################################################################################
# SPARQL #
##########


#' Requête à l'ontologie de crise avec les sous catégories
#'
#' @param sujet mot présent dans l'ontologie de crise
#'
#' @return liste des mots clefs en rapport avec le sujet donné
#' @export
#'
#' @examples listeMot <- requeteCrisisSub("inondation")
requeteCrisisSub <- function(sujet, uri = "https://fuseki-inplic.herokuapp.com/ds/query"){
  query <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX matta: <http://www.semanticweb.org/matta/ontologies/2020/10/crisiskeywords#>

  SELECT ?entity
  WHERE {
  {?entity rdf:type ?type.
    ?type rdfs:subClassOf matta:",sujet,"}UNION{
  ?entity rdf:type matta:",sujet,"}
}
  LIMIT 500")

  #"purification" des mots de la liste
  list <- SPARQL::SPARQL(uri,query)$results
  for (i in (1:ncol(list))) {
    list[i] <- extractWord(list[i])
  }

  return(list)
}

#' Requête à l'ontologie de crise sans sous catégories
#'
#' @param sujet mot présent dans l'ontologie de crise
#'
#' @return liste des mots clefs en rapport avec le sujet donné
#' @export
#'
#' @examples listeMot <- requeteCrisisSub("inondation")
requeteCrisis <- function(sujet, uri = "https://fuseki-inplic.herokuapp.com/ds/query"){
  query <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX matta: <http://www.semanticweb.org/matta/ontologies/2020/10/crisiskeywords#>

  SELECT ?entity
  WHERE {
  {?entity rdf:type matta:",sujet,"}
}
  LIMIT 500")

  list <- SPARQL::SPARQL(uri,query)$results
  for (i in (1:ncol(list))) {
    list[i] <- extractWord(list[i])
  }

  return(list)
}

#' Extrait les classes (thèmes) de l'ontologie de crise
#'
#' @return liste de toutes les classes et sous-classes présentent dans l'ontologe de crise
#' @export
#'
#' @examples listeClasses <- extraireClasses()
extrairesClasses <- function(uri = "https://fuseki-inplic.herokuapp.com/ds/query"){
  query <- paste0("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX matta: <http://www.semanticweb.org/matta/ontologies/2020/10/crisiskeywords#>

  SELECT ?entity
  WHERE {
    ?entity rdfs:subClassOf ?n
  }
  LIMIT 500")

  list <- SPARQL::SPARQL(uri,query)$results
  for (i in (1:ncol(list))) {
    list[i] <- extractWord(list[i])
  }

  return(list)
}

#' Extraction de mots clefs à partir d'une URI  de l'ontologie
#'
#' @param uri d'un mot dans l'ontologie de crise
#'
#' @return un mot
#' @export
#'
#' @examples mot <- extractWord("http://www.semanticweb.org/matta/ontologies/2020/10/crisiskeywords#inondation")
extractWord <- function(uri){
  word <- stringr::str_extract(uri,"#[A-Z,a-z,_,',-,à,ê,é,è,ï,î,û,Ã©]+")
  word <- stringr::str_remove(word,"#")
  word <- stringr::str_replace_all(word,"_"," ")
  word <- stringr::str_replace_all(word,"Ã©","é") #règle les problème avec l'accent é
  return(word)
}


###################################################################################################################################################

parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

deleteButtonColumn <- function(df, id, ...) {
  # fonciton pour créer le bouton sur une cellule
  f <- function(i) {
    as.character(
      actionButton(
        # id prefix avec l'index de l'objet à suppr
        paste(id, i, sep="_"),
        label = NULL,
        icon = icon('trash'),
        #pour déclencher un trigger quand cliqué
        onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
  }

  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Recréation de la table
  DT::datatable(cbind(delete = deleteCol, df),
                escape = FALSE,
                options = list(
                  # Evite à la colonne delete d'être "classable"
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}

deleteButtonColumnUser <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(id, i, sep="_"),
        label = NULL,
        icon = icon('trash'),
        onclick = 'Shiny.setInputValue(\"deletePressedUser\", this.id, {priority: "event"})'))
  }

  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}

deleteButtonColumnKW <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(id, i, sep="_"),
        label = NULL,
        icon = icon('trash'),
        onclick = 'Shiny.setInputValue(\"deletePressedKW\", this.id, {priority: "event"})'))
  }

  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}



shinyServer(function(input, output, session) {

  tokenModal <- function(failed = FALSE){
    modalDialog(
      title = "Twitter API",
      textInput("API_key", "API_key", placeholder = "nvYE2ZRIWK5mB2Jjd07LWo6wF"),
      textInput("API_key_secret", "API_key_secret", placeholder = 'PCGk9D5ab2BYTOqzLzGTYRQacxX0vQvo5FgZhzZKBXGdEskVL6'),
      textInput("Access_token", "Access_token", placeholder = '377517541-qt9DcoYXoefqvL7NmgZY9i4nDXu30LqLL0mPmOMc'),
      textInput("Access_token_secret", "Access_token_secret", placeholder = 'FpGCXHfxS2kDrq5HUpxaItymeO4pExyse6gbWMv8pT79y'),
      easyClose = TRUE,
      footer = tagList(modalButton("Annuler"),
                       actionButton("api_valider", "Valider"))
    )
  }

  #connection à la BDD cloudDB (connectée à heroku)
  DB <- RMySQL::dbConnect(MySQL(), user = "b7d2b3749f8e8a", host = "eu-cdbr-west-01.cleardb.com", password = "26371387", dbname = "heroku_c8d54b00aad5131")
  k <- RMySQL::dbReadTable(DB, "twikey")
  deco <- dbDisconnect(DB)

  token <- reactiveValues(token = twitterToken("Twitter words analysis",as.String(k[1,2]),as.String(k[1,3]),as.String(k[1,4]),as.String(k[1,5])))
  kwList <- reactiveValues(kw = data.frame(matrix(ncol = 3)),index = 0)
  newList <- reactiveValues(newList = data.frame(), listTable = data.frame(), listUser = data.frame())
  errtk <- reactiveValues(err = FALSE)

  #lecture des clefs
  tryCatch({

    rtweet::search_tweets("a",n=1, token = isolate(token$token))
  }, warning = function(war){
    print(war)
    showModal(tokenModal())
    output$key <- renderText({"pas de clef API valide"})
  }, error = function(err){
    print(err)
    showModal(tokenModal())
    output$key <- renderText({"pas de clef API valide"})
  }, finally = {
  })
  #création de l'affichage d'erreur de recherche
  errorModal <- function(failed = FALSE) {

    modalDialog(
      title = "Erreur de recherche",
      "Aucun tweet n'a été trouvé",
      easyClose = TRUE,
      footer = NULL
    )
  }

  tokenError <- function(){
    #output$key <- renderText({"pas de clef API valide"})
    modalDialog(
      title = "Erreur",
      "Clé incorrecte",
      easyClose = TRUE,
      footer = NULL
    )
  }

  tokenSuccess <- function(){
    output$key <- renderText({" "})
    modalDialog(
      title = "Succès",
      "Succès",
      easyClose = TRUE,
      footer = NULL
    )
  }

  observeEvent(input$api, {

    showModal(tokenModal())

  })

  observeEvent(input$api_valider, {
    DB <- RMySQL::dbConnect(MySQL(), user = "b7d2b3749f8e8a", host = "eu-cdbr-west-01.cleardb.com", password = "26371387", dbname = "heroku_c8d54b00aad5131")
    token_temp <- twitterToken("Twitter words analysis",input$API_key,input$API_key_secret,input$Access_token,input$Access_token_secret)
    #k <- data.frame(api_key = input$API_key, api_key_secret = input$API_key_secret, access_token = input$Access_token, access_token_secret = input$Access_token_secret)
    tryCatch({
      rtweet::search_tweets("a",n=1, token = token_temp)
    }, warning = function(war){

      errtk$err <- TRUE
      print(war)
      showModal(tokenError())
    }, error = function(err){

      errtk$err <- TRUE
      print(err)
      showModal(tokenError())
    }, finally = {
      if(errtk$err==FALSE){
        token$token <- token_temp
        showModal(tokenSuccess())
        #dbWriteTable(DB, "twitter", k, overwrite = TRUE)
        dbSendQuery(DB, paste0("UPDATE twikey SET api_key = '",input$API_key,"', api_key_secret = '",input$API_key_secret,"', access_token = '",input$Access_token,"', access_token_secret = '",input$Access_token_secret,"' WHERE row_names = 1"))
      }

      deco <- dbDisconnect(DB)
      cat(deco)
    }
    )
  })


  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)

    # Delete the row from the data frame
    newList$newList <- newList$newList[-rowNum,]
    newList$listTable <- newList$listTable[-rowNum,]
  })

  observeEvent(input$deletePressedKW, {
    rowNum <- parseDeleteEvent(input$deletePressedKW)

    # Delete the row from the data frame
    kwList$kw <- kwList$kw[-rowNum,]
  })

  observeEvent(input$deletePressedUser, {
    rowNum <- parseDeleteEvent(input$deletePressedUser)
    nR <- nrow(newList$listTable)

    i<-1
    while(i<=nR){
      if(as.String(newList$listTable[i,1])==as.String(newList$listUser[rowNum,1])){
        newList$listTable <- newList$listTable[-i,]
        i <- i-1
      }
      i<-i+1
    }

    newList$listUser <- newList$listUser[-rowNum,]
  })

  #vider les tableaux
  observeEvent(input$reset, {
    newList$newList <- data.frame()
    newList$listTable <- data.frame()
    newList$listUser <- data.frame()
    kwList$kw <- data.frame()
  })

  #Déclenchement bouton ajouter par thématique
  observeEvent(input$add, {

    sparqlList <- data.frame()

    if(input$Associer!="aucun"){
      kwassoc <- requeteCrisis(input$Associer)
    }
    if(input$Recherche=="Thématique"){

      #requête SPARQL
      sparqlList <- requeteCrisis(input$Crisis)


    }else if(input$Recherche=="Personnalisée"){

      sparqlList[1,1] <- input$kw

    }

    #attribution des mots clefs dans le tableau en fonction de ceux deja présents
    i <- 1
    while(i <= ncol(sparqlList)){


      if(input$Associer=="aucun"){
        nR <- nrow(kwList$kw)+1
        kwList$kw[nR,1] <- sparqlList[i]
        kwList$kw[nR,2] <- input$geo
        kwList$kw[nR,3] <- ""
      }else {
        j <- 1
        while (j <= ncol(kwassoc)){
          nR <- nrow(kwList$kw)+1
          kwList$kw[nR,1] <- sparqlList[i]
          kwList$kw[nR,2] <- input$geo
          kwList$kw[nR,3] <- kwassoc[j]
          j <- j+1
        }
      }

      i <- i+1
    }


    #Noms des colonnes
    colnames(kwList$kw)<-c("Mot-clef","Geolocalisation","Mots associés")


    kwList$index <- kwList$index + 1
    #suppression bug qui laisse une colonne vide au premier ajout
    if(kwList$index==1){
      kwList$kw <- kwList$kw[-1,]
    }


    output$motsClefs = DT::renderDataTable({
      deleteButtonColumnKW(kwList$kw,'delete_button')
    })

  })

  #lancement du text mining
  observeEvent(input$launch, {

    if(as.String(kwList$kw[1,1])!="NA"){
      #Recherche pour chaque ligne du tableau de mots clefs
      for(i in(1:nrow(kwList$kw))){

        newList$newList <- rbind(newList$newList,searchTwitterTwoKWgeo(kwList$kw[i,1],kwList$kw[i,2],kwList$kw[i,3],input$numberTweets/nrow(kwList$kw),token$token))


      }
    }else{
      showModal(errorModal())
    }




    if(nrow(newList$newList)!=0){

      #Réglage pour permettre de classer par média

      # for(i in(1:nrow(newList$newList))){
      #
      #   if(is.na(newList$newList[[i,25]])){
      #
      #     newList$newList[[i,91]] <- "NA"
      #
      #   }else{
      #
      #     newList$newList[[i,91]] <- "Media"
      #
      #   }
      #
      #   colnames(newList$newList)[91] <- "media"
      # }

      newList$listTable <- newList$newList[, colnames(newList$newList)[c(4,5,3,13,14,32,22)]]
      newList$listUser <- unique(newList$newList[, colnames(newList$newList)[c(4,73,74,75,83,84,78,81)]])



      output$tweetsTable = DT::renderDataTable({
        deleteButtonColumn(newList$listTable,'delete_button')
      })

      output$usersTable = DT::renderDataTable({
        deleteButtonColumnUser(newList$listUser,'delete_button')
      })
    }else {
      showModal(errorModal())
    }



    #création du word cloud
    output$WordCloud <- renderPlot({


      wordcloud(
        wordsToData(createCorpus(newList$newList,"fr")),
        100,
        2,
        FALSE,
        0.0)

    })

    #création de l'analyse des sentiments
    output$Sentiment <- renderPlot({
      emotions <- sentimentAnalysis(createCorpus(newList$newList,"fr"),"french")
      graphics::barplot(colSums(emotions),cex.names = .7,
                        col = c("firebrick","darkmagenta","darkolivegreen","orange","palevioletred1","grey","yellow1","royalblue","red","limegreen"),
                        main = "Sentiment Analysis"
      )
    })

  })

})
