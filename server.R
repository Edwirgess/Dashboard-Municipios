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
library(cid10)


setores_basico <- censobr::read_tracts(2010, dataset = "Basico")

municipio_data <- read.csv("/data/municipios_data.csv")

estado_data <- read.csv("/data/estados_data.csv")


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
    selectizeInput(inputId = 'estado',
                   label = "Estado",
                   choices = c("Todos", estados_sigla))
  })
  
  output$selectSubcategoria <- renderUI({
    subcategorias <- setNames(cid_subcat$cid, cid_subcat$descabrev)
    selectizeInput("descrabrev_filter", "Filtrar por Decrição:",
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
  
  filt_data <- reactive({
    estado_sel <- input$estado
    subcat_sel <- input$descrabrev_filter
    if (is.null(estado_sel) || estado_sel == "Todos") {
      data_f <- estados
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        data_f <- filter(data_f, cid == subcat_sel)
      }
      data_f <- data_f |>
        group_by(name_state, abbrev_state) |>
        summarise(
          domicilios = sum(domicilios),
          residentes = sum(residentes),
          obitos = sum(obitos)
        )
      data_f
    } else {
      data_f <- filter(municipios, abbrev_state == estado_sel)
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        data_f <- filter(data_f, cid == subcat_sel)
      }
      data_f <- data_f |>
        group_by(name_muni, abbrev_state) |>
        summarise(
          domicilios = sum(domicilios),
          residentes = sum(residentes),
          obitos = sum(obitos)
        )
      data_f
    }
    
  })
  
  output$popMunDataTable <- renderDataTable({
    res <- filt_data()
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
               `Residentes` = residentes,
               `Óbitos` = obitos) |>
        datatable()
    }
  })
  
  
  obitos <- reactive({
    subcat_sel <- input$descrabrev_filter
    if (is.null(subcat_sel) || subcat_sel == "Todos") {
      municipios
    } else {
      filter(municipios, cid == subcat_sel)
    }
  })
  
  output$obitosTable <- renderDataTable({
    res <- filt_data()
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
               `Residentes` = residentes,
               `Óbitos` = obitos) |>
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

