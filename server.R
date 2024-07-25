library(censobr)
library(dplyr)
library(DT)
library(shiny)
library(plotly)
library(geobr)
library(ggplot2)
library(sf)
library(leaflet)
library(tmap)
library(tmaptools)
library(microdatasus)
library(stringr)


setores_basico <- censobr::read_tracts(2010, dataset = "Basico")

municipio_data <- read.csv("C:/Users/Edwirges/OneDrive/Área de Trabalho/cd edwirges/data/municipios_data.csv")

estado_data <- read.csv("C:/Users/Edwirges/OneDrive/Área de Trabalho/cd edwirges/data/estados_data.csv")

# sim <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022, uf = "PB", information_system = "SIM-DO")
# 
# obitos_mun <- sim |> group_by(code_muni6 = as.numeric(CODMUNRES))  |> summarise(obitos = n())

# pop_mun <- setores_basico |>
#   group_by(code_muni = as.numeric(code_muni)) |>
#   summarise(
#     domicilios = as.integer(sum(V001)),
#     residentes = as.integer(sum(V002))
#   ) |>
#   collect()

# pop_estado <- setores_basico |>
#   group_by(code_state = as.numeric(code_state)) |>
#   summarise(
#     domicilios = as.integer(sum(V001)),
#     residentes = as.integer(sum(V002))
#   ) |>
#   collect()


municipios <- read_municipality(year=2010) |>
  mutate(code_muni6 = as.double(str_sub(code_muni, end = 6)))

estados <- read_state(year = 2010)


municipios <- municipios |>
  left_join(municipio_data, by = "code_muni6") |>
  st_as_sf()

estados <- estados |>
  left_join(estado_data, "code_state") |>
  st_as_sf()

estados_sigla <- estados$abbrev_state |> sort()
##

server <- function(input, output, session) {
  
  output$selectEstadoOutput <- renderUI({
    selectInput(inputId = 'estado',
                label = "Estado",
                choices = c("Todos", estados_sigla))
  })
  
  output$selectSubcategoria <- renderUI({
    subcategorias <- setNames(cid_subcat$cid, cid_subcat$descabrev)
    selectInput("descrabrev_filter", "Filtrar por Decrição:",
                choices = c("Todos", subcategorias),
                selected = "Todos")
  })
  
  populacao <- reactive({
    estado_sel <- input$estado
    if (is.null(estado_sel) || estado_sel == "Todos") {
      estados
    } else {
      filter(municipios, abbrev_state == estado_sel)
    }
  })
  
  
  output$popMunDataTable <- renderDataTable({
    res <- populacao()
    if ("name_muni" %in% names(res)) {
      as.data.frame(res) |>
      select(`Nome do municipio` = name_muni,
             `Sigla da UF` = abbrev_state,
             `Domicilios` = domicilios,
             `Residentes` = residentes,
             `Óbitos` = obitos) |>
      datatable()
    }else{
      as.data.frame(res) |>
        select(`Nome do estado` = name_state,
               `Sigla da UF` = abbrev_state,
               `Domicilios` = domicilios,
               `Residentes` = residentes) |>
        datatable()
    }
  })
  
  
  obitos <- reactive({
    subcat_sel <- input$selectSubcategoria
    if (is.null(subcat_sel) || subcat_sel == "Todos") {
      municipio_data
    } else {
      filter(municipio_data, cid == subcat_sel)
    }
  })
  
  output$obitosTable <- renderDataTable({
    res <- obitos()
    if ("cid" %in% names(res)) {
      as.data.frame(res) |>
        select(`Nome do municipio` = name_muni,
               `Sigla da UF` = abbrev_state,
               `Domicilios` = domicilios,
               `Residentes` = residentes,
               `Óbitos` = obitos,
               `Causa Basica` = cid) |>
        datatable()
    }
  })
  
 
  
  
  #Mapa com tmap

  output$map <- renderTmap({

    tmap_options(check.and.fix = TRUE)

    res <- populacao()
    

    if ("name_muni" %in% colnames(res)){

      tm_shape(res) +
        tm_polygons("residentes",
                    palette = "viridis",
                    title = "População",
                    popup.vars = c("Nome do municipio" = "name_muni", "Residentes" = "residentes", "Obitos" = "obitos")
                    ) +
        tm_layout(title = "População dos Municípios Brasileiros",
                  legend.outside = TRUE)
    } else {
      tm_shape(res) +
        tm_polygons("residentes",
                    palette = "viridis",
                    title = "População",
                    popup.vars = c("Nome do estado" = "name_state", "Residentes" = "residentes")
        ) +
        tm_layout(title = "População dos Estados Brasileiros",
                  legend.outside = TRUE)
    }

  })

  session$onSessionEnded(stopApp)
  
  
}
#shinyApp(ui = ui, server = server)

