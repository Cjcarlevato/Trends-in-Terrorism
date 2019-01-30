source("./header.R")
source("./sidebar.R")
source("./body.R")


shinyUI(fluidPage(theme = shinytheme("united"),
  dashboardPage(skin = 'green',
      header,
      sidebar,
      body)))