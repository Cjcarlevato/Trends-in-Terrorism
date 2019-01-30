sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Description of Data", tabName='intro', startExpanded = TRUE
  ),
  menuItem("Trends in Methods", tabName = "methods"
  ),
  menuItem("Terrorist Attacks by U.S. State", tabName='Map'
  ),
  menuItem("Word Clouds", tabName = "cloud"
  )
  )
)



# 
# tag$blockquotes
# ("Is terrorism on the rise or decline? This app generates plots
#   and interactive U.S. maps with data from the Global
#   Terrorism Database.")