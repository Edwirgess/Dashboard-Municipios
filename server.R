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
library(arrow)
library(lwgeom)


setores_basico <- censobr::read_tracts(2010, dataset = "Basico")


municipio_data <- open_dataset("../Dashboard-Municipios/data/municipios_data", format = "parquet")
estado_data <- open_dataset("../Dashboard-Municipios/data/estados_data", format = "parquet")

estado_data <- estado_data |>
  mutate(code_state = as.integer(code_state))

municipios_geo <- read_municipality(year=2010) |>
  mutate(code_muni6 = as.integer(str_sub(code_muni, end = 6)),
         code_state = as.integer(code_state))

estados_geo <- read_state(year = 2010) |>
  mutate(code_state = as.double(code_state)) |>
  arrange(abbrev_state)

estados_sigla <- setNames(estados_geo$code_state, estados_geo$abbrev_state)

#server

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
  
  output$selectVar <- renderUI({
    selectizeInput("var_select", "Selecione a Variável:",
                   choices = c("Residentes", "Domicilios", "Óbitos"),
                   selected = "Residentes")
  })
  #Filtro
  
  filt_data <- reactive({
    estado_sel <- input$estado
    subcat_sel <- input$descrabrev_filter
    
    if (is.null(estado_sel) || estado_sel == "Todos") {
      data_f <- estado_data
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        data_f <- filter(data_f, cid == subcat_sel)
      }
      
      data_f <- data_f |>
        group_by(code_state) |>
        summarise(
          domicilios = min(domicilios, na.rm = TRUE),
          residentes = min(residentes, na.rm = TRUE),
          obitos = sum(obitos)
        ) |>
        collect()
      data_f <- estados_geo |>
        right_join(data_f, by = "code_state") |>
        st_as_sf()
      
    } else {
      estado_sel_dbl <- as.double(estado_sel)
      data_f <- filter(municipio_data, code_state == estado_sel_dbl) |> collect()
      print(data_f)
      
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        data_f <- filter(data_f, cid == subcat_sel)
      }
      
      data_f <- data_f |>
        group_by(code_muni6) |>
        summarise(
          domicilios = min(domicilios, na.rm = TRUE),
          residentes = min(residentes, na.rm = TRUE),
          obitos = sum(obitos)
        ) |>
        collect()
      data_f <- municipios_geo |>
        right_join(data_f, by =  "code_muni6") |>
        st_as_sf()
      print(data_f)
    }
    data_f
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
  
  #Mapa
  
  output$map <- renderTmap({
    
    tmap_options(check.and.fix = TRUE)
    
    res <- filt_data()
    
    selected_var <- input$var_select
    
    
    if ("name_muni" %in% colnames(res)){
      
      tm_shape(res) +
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = c("Nome do municipio" = "name_muni", "Residentes" = "residentes", "Obitos" = "obitos")
        ) +
        tm_layout(title = "Mapa dos Municípios Brasileiros",
                  legend.outside = TRUE)
    } else {
      tm_shape(res) +
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = c("Nome do estado" = "name_state", "Residentes" = "residentes")
        ) +
        tm_layout(title = "Mapa dos Estados Brasileiros",
                  legend.outside = TRUE)
    }
    
  })
  
  session$onSessionEnded(stopApp)
  
  
}
#shinyApp(ui = ui, server = server)

