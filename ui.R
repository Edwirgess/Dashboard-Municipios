library(shinydashboard)
library(tmap)

ui <- dashboardPage(
  dashboardHeader(title = "Municípios do Brasil"),
  dashboardSidebar(
    uiOutput("selectEstadoOutput"),
    sidebarMenu(
      menuItem("Tabela", tabName = "table", icon = icon("table")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("Óbitos", tabName = "obitos", icon = icon("table"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "table",
              fluidRow(box(width = 12, dataTableOutput("popMunDataTable"))
              )),
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, uiOutput("selectVar")),
                box(width = 12,  tmapOutput("map", height = "800px"))
              )),
      tabItem(tabName = "obitos",
              fluidRow(
                box(width = 12, uiOutput("selectSubcategoria")),
                box(width = 12,dataTableOutput("obitosTable"))
              ))
    )
    
  )
)


