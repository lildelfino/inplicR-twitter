fluidPage(
  titlePanel("Inplic R twitter analysis"),

  sidebarLayout(
    sidebarPanel(
      h2("Options : "),
      selectInput("Recherche","Recherche : ",
                  choices = c("Thématique","Personnalisée")),
      textInput("geo","Geocode ou localisation :"),
      conditionalPanel(condition = "input.Recherche == 'Thématique'",
                       selectInput("Crisis","Crise traitée : ",
                                   choices = c("inondation","accident_technologique","attentat","feux_de_foret","neige_verglas","tempete","sanitaire"))
      ),
      conditionalPanel(condition = "input.Recherche == 'Personnalisée'",
                       textInput("kw","Mot-clef :")
      ),
      selectInput("Associer","Mots Associés : ",
                  choices = c("aucun","detresse")),
      actionButton("add", "Ajouter aux mots-clefs"),
      hr(),
      sliderInput("numberTweets","Nombre de tweets max",
                  min=1, max=3600, value=100, step=1),
      actionButton("launch", "Analyser"),
      actionButton("reset", "Réinitialiser")
    ),
    mainPanel(
      h2("Résultats : "),
      selectInput("Result","",
                  choices = c("Mes mots clefs","Tweets","Utilisateurs","Graphs")),
      conditionalPanel(condition = "input.Result == 'Mes mots clefs'",
                       DT::dataTableOutput("motsClefs")),
      conditionalPanel(condition = "input.Result == 'Graphs'",
                       plotOutput("WordCloud"),
                       plotOutput("Sentiment")),
      conditionalPanel(condition = "input.Result == 'Tweets'",
                       DT::dataTableOutput("tweetsTable")),
      conditionalPanel(condition = "input.Result == 'Utilisateurs'",
                       DT::dataTableOutput("usersTable"))

    )
  )

)
