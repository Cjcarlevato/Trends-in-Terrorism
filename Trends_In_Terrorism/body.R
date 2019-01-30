body <- dashboardBody(
  tabItems(
    tabItem(
      tabName= "intro",
      box(
      title = "Understanding the Data Source",
      skin = "red", 
      status = "primary", 
      solidHeader = TRUE,
      width = 12,
      htmlTemplate("index.html")
     )
    ),
    tabItem(
      tabName = "methods",
        box(
        title = "Terrorist Attack Methods Line Plots", 
        skin = 'red', 
        status = "primary", 
        solidHeader = TRUE,
        "Select an attack method to see the evolution of its frequency from 1970 to 2017", 
        width = 12,
        selectInput("method_dataframeinput", label = "Method", choices = c("Hijackings", 
                                                                           "Armed Assaults",
                                                                           "Assassinations",
                                                                           "Bombings",
                                                                           "Hostage Takings",
                                                                           "Kidnappings", 
                                                                           "Facility Attacks",
                                                                           "Unarmed Assaults",
                                                                           "Unknown Method"))
      ), 
      fluidRow(
        # # box(
        # #   title = "Terrorist Attack Method Bar Chart", 
        # #   solidHeader = TRUE,
        # #   plotlyOutput("method_barplot", 
        # #                height = 800 
        # #                ), 
        # #   width = 6
        # ),
        box(
          title = "Terrorist Attack Method Line Plot ", 
          solidHeader = TRUE,
          plotlyOutput("method_lineplot",
                       height = 800 
                       ), 
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "Map",
      box(
        title = "Choropleth Map of U.S. for Terrorist Attack Counts",
        skin = 'red', 
        status = "primary", 
        solidHeader = TRUE,
        "Select an input to see its cumulative totals from 1970 to 2017 on the U.S. map", 
        width = 12,
        selectInput("method_choose_map", label = "Count", choices = c("Deaths",
                                                                      "Injuries",
                                                                      "Hijackings",
                                                                      "Armed Assaults",
                                                                      "Assassinations",
                                                                      "Bombings",
                                                                      "Hostage Takings",
                                                                      "Kidnappings", 
                                                                      "Facility Attacks",
                                                                      "Unarmed Assaults",
                                                                      "Unknown Method"))
      ),
      fluidRow(
        box(
          title = "Cumulative Terrorist Attacks Counts by U.S. State", 
          solidHeader = TRUE,
          plotlyOutput("method_choropleth", 
                       height = 1000 
          ), 
          width = 12
        )
      )  
    ),
    tabItem(
      tabName = "cloud",
      fluidRow(
      box(title= "Generate a word cloud based on 3 categories",
          skin = 'red', 
          status = "primary", 
          solidHeader = TRUE,
          "Generate word clouds based on national origin of attacker(s), city of attack, and terror group. 
          Bigger words indicate higher frequencies in database", tags$br(),
          width = 12,
          selectInput("method_choose_cloud", label = "", choices = c("Nationality of Attacker", 
                                                                                                        "City of Attack",
                                                                                                        "Terrorist Group"))),
          box(title= "Select a year or range of years",
                 skin = 'red', 
                 status = "primary", 
                 solidHeader = TRUE,
            sliderInput("select_year", label = h3("Select Range of Years"), 
                      min = 1970, max = 2017, value = c(1970, 2017), format="####", sep = ""), width = 12)
      
      ),
        fluidRow(
          box(
            title = "Hover over word to see frequency for selected time range",
            solidHeader = TRUE,
            wordcloud2Output("word_cloud", height = 500, width = '1500px'),
            width = 12, height = 600)
        ) 
      )  
    ) 
)


