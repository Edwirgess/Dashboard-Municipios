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

<<<<<<< Updated upstream
municipio_data <- read.csv("/data/municipios_data.csv")

estado_data <- read.csv("/data/estados_data.csv")
=======
municipio_data <- open_dataset("../Dashboard-Municipios/data/municipios_data", format = "parquet")
estado_data <- open_dataset("../Dashboard-Municipios/data/municipios_data", format = "parquet")
municipio_censo <- open_dataset("../Dashboard-Municipios/data/municipios_censo", format = "parquet")
estado_censo <- open_dataset("../Dashboard-Municipios/data/estados_censo", format = "parquet")
municipio_mortalidade <- open_dataset("../Dashboard-Municipios/data/municipios_mortalidade", format = "parquet")
estado_mortalidade <- open_dataset("../Dashboard-Municipios/data/estados_mortalidade", format = "parquet")
>>>>>>> Stashed changes


municipios <- read_municipality(year=2010) |>
  mutate(code_muni6 = as.double(str_sub(code_muni, end = 6)))

estados <- read_state(year = 2010)


<<<<<<< Updated upstream
municipios <- municipios |>
  left_join(municipio_data, by = "code_muni6") |>
  st_as_sf()

estados <- estados |>
  left_join(estado_data, "code_state") |>
  st_as_sf()

estados_sigla <- estados$abbrev_state |> sort()
##
=======
var_choices <- setNames(c("residentes", "domicilios", "obitos"),
                        c("Residentes", "Domicilios", "Óbitos"))

#server
>>>>>>> Stashed changes

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
  
<<<<<<< Updated upstream
  populacao <- reactive({
    estado_sel <- input$estado
    if (is.null(estado_sel) || estado_sel == "Todos") {
      estados
    } else {
      filter(municipios, abbrev_state == estado_sel)
    }
=======
  output$selectVar <- renderUI({
    selectizeInput("var_select", "Selecione a Variável:",
                   choices = var_choices,
                   selected = "residentes")
>>>>>>> Stashed changes
  })
  
  filt_data <- reactive({
    estado_sel <- input$estado
    subcat_sel <- input$descrabrev_filter
    if (is.null(estado_sel) || estado_sel == "Todos") {
<<<<<<< Updated upstream
      data_f <- estados
=======
      censo <- estado_censo
      mortalidade <- estado_mortalidade
      
>>>>>>> Stashed changes
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        mortalidade <- filter(mortalidade, cid == subcat_sel)
      }
<<<<<<< Updated upstream
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
=======
      
      censo_data <- censo |>
        group_by(code_state) |>
        summarise(
          domicilios = min(domicilios, na.rm = TRUE),
          residentes = min(residentes, na.rm = TRUE)
          #obitos = sum(obitos)
        ) |>
        collect()
      
      mortalidade_data <- mortalidade|>
        group_by(code_state) |>
        summarise(
          obitos = sum(obitos)
        ) |>
        collect()
      
      data_f <- censo_data |>
        left_join(mortalidade_data, "code_state") |>
        right_join(estados_geo, by = "code_state") |>
        st_as_sf()
      data_f
      
    } else {
      
      censo <- municipio_censo
      mortalidade <- municipio_mortalidade
      
      #estado_sel_dbl <- as.double(estado_sel)
      #data_f <- filter(municipio_data , code_state == input$estado) |> collect()
      #print(data_f)
      
>>>>>>> Stashed changes
      if (!is.null(subcat_sel) && subcat_sel != "Todos") {
        mortalidade <- filter(mortalidade, cid == subcat_sel)
      }
<<<<<<< Updated upstream
      data_f <- data_f |>
        group_by(name_muni, abbrev_state) |>
        summarise(
          domicilios = sum(domicilios),
          residentes = sum(residentes),
          obitos = sum(obitos)
        )
=======
      
      censo_data <- censo |>
        filter(code_state == estado_sel) |>
        group_by(code_muni6) |>
        summarise(
          domicilios = min(domicilios, na.rm = TRUE),
          residentes = min(residentes, na.rm = TRUE)
        ) |>
        collect()
      
      mortalidade_data <- mortalidade |>
        filter(code_state == estado_sel) |>
        group_by(code_muni6) |>
        summarise(
          obitos = sum(obitos)
        ) |>
        collect()
      
      data_f <- censo_data |>
        left_join(mortalidade_data, "code_muni6")
        right_join(municipios_geo, by =  "code_muni6") |>
        st_as_sf()
      print(data_f)
>>>>>>> Stashed changes
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
    
    validate(
      need(input$var_select != "", "Carregando...")
    )
    
    tmap_options(check.and.fix = TRUE)
    
    res <- populacao()
    
    
    if ("name_muni" %in% colnames(res)){
      
<<<<<<< Updated upstream
      tm_shape(res) +
        tm_polygons("residentes",
                    palette = "viridis",
                    title = "População",
                    popup.vars = c("Nome do municipio" = "name_muni", "Residentes" = "residentes", "Obitos" = "obitos")
=======
      tm_shape(relocate(res, name_muni)) +
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = c("Nome do municipio" = "name_muni", selected_var)
>>>>>>> Stashed changes
        ) +
        tm_layout(title = "População dos Municípios Brasileiros",
                  legend.outside = TRUE)
    } else {
<<<<<<< Updated upstream
      tm_shape(res) +
        tm_polygons("residentes",
                    palette = "viridis",
                    title = "População",
                    popup.vars = c("Nome do estado" = "name_state", "Residentes" = "residentes")
=======
      tm_shape(relocate(res, name_state)) + 
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = selected_var
>>>>>>> Stashed changes
        ) +
        tm_layout(title = "População dos Estados Brasileiros",
                  legend.outside = TRUE)
    }
    
  })
  
  session$onSessionEnded(stopApp)
  
  
}
#shinyApp(ui = ui, server = server)

