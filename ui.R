library(shinydashboard)
library(tmap)

ui <- dashboardPage(
  dashboardHeader(title = "Municípios do Brasil"),
  dashboardSidebar(
    uiOutput("selectEstadoOutput"),
    uiOutput("selectSubcategoria"),
    sidebarMenu(
      menuItem("Tabela", tabName = "table", icon = icon("table")),
      menuItem("Mapa", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "table",
        box(width = 12, dataTableOutput("table"))
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, uiOutput("selectVar")),
                box(width = 12,  tmapOutput("map", height = "800px"))
              )
      )
    )
  )
)
