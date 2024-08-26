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


municipio_censo <- open_dataset("data/municipios_censo", format = "parquet")
estado_censo <- open_dataset("data/estados_censo", format = "parquet")
municipio_mortalidade <- open_dataset("data/municipios_mortalidade", format = "parquet")
estado_mortalidade <- open_dataset("data/estados_mortalidade", format = "parquet")
municipios <- readRDS("data/municipios_geo.rds") |>
  arrange(abbrev_state, name_muni)
estados <- readRDS("data/estados_geo.rds") |>
  arrange(name_state)

estados_choices <- setNames(c("", estados$code_state),
                            c("Todos", estados$name_state))

var_choices <- setNames(c("residentes", "domicilios", "obitos"),
                        c("Residentes", "Domicilios", "Óbitos"))

cid_choices <- setNames(c("", cid10::cid_subcat$cid),
                        c("Todos", cid10::cid_subcat$descrabrev))

server <- function(input, output, session) {
  
  output$selectEstadoOutput <- renderUI({
    selectizeInput(inputId = 'estado',
                   label = "Estado",
                   choices = estados_choices)
  })
  
  output$selectSubcategoria <- renderUI({
    selectizeInput("descrabrev_filter", "CID-10 Subcategoria",
                   choices = cid_choices)
  })
  
  output$selectVar <- renderUI({
    selectizeInput("var_select", "Selecione a Variável:",
                   choices = var_choices)
  })
  
  filt_data <- reactive({
    cod_estado_sel <- input$estado
    subcat_sel <- input$descrabrev_filter
    if (is.null(cod_estado_sel) || cod_estado_sel == "") {
      censo_data <- estado_censo |>
        group_by(code_state) |>
        summarise(
          domicilios = sum(domicilios, na.rm = TRUE),
          residentes = sum(residentes, na.rm = TRUE)
        ) |>
        collect()
      
      mortalidade_data <- estado_mortalidade
      if (!is.null(subcat_sel) && subcat_sel != "") {
        mortalidade_data <- filter(mortalidade_data, cid == subcat_sel)
      }
      mortalidade_data <- mortalidade_data |>
        group_by(code_state) |>
        summarise(obitos = sum(obitos, na.rm = TRUE)) |>
        collect()
      
      data_f <- estados_geo |>
        right_join(censo_data, by = "code_state") |>
        left_join(mortalidade_data, by = "code_state") |>
        replace_na(list(obitos = 0)) |>
        arrange(name_state)
      data_f
    } else {
      cod_estado_sel_num <- as.numeric(cod_estado_sel)
      censo_data <- municipio_censo |>
        filter(code_state == cod_estado_sel_num) |>
        group_by(code_muni6) |>
        summarise(
          domicilios = sum(domicilios, na.rm = TRUE),
          residentes = sum(residentes, na.rm = TRUE)
        ) |>
        collect()
      
      mortalidade_data <- municipio_mortalidade |>
        filter(code_state == cod_estado_sel_num)
      if (!is.null(subcat_sel) && subcat_sel != "") {
        mortalidade_data <- filter(mortalidade_data, cid == subcat_sel)
      }
      mortalidade_data <- mortalidade_data |>
        group_by(code_muni6) |>
        summarise(obitos = sum(obitos, na.rm = TRUE)) |>
        collect()
      
      data_f <- municipios_geo |>
        right_join(censo_data, by = "code_muni6") |>
        left_join(mortalidade_data, by = "code_muni6") |>
        replace_na(list(obitos = 0)) |>
        arrange(name_muni)
      data_f
    }
  })
  
  output$table <- renderDataTable({
    res <- filt_data()
    if ("name_muni" %in% names(res)) {
      as.data.frame(res) |>
        select(`UF` = abbrev_state,
               `Nome do municipio` = name_muni,
               `Domicilios` = domicilios,
               `Residentes` = residentes,
               `Óbitos` = obitos) |>
        datatable()
    }else{
      as.data.frame(res) |>
        select(`UF` = abbrev_state,
               `Nome do estado` = name_state,
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
    selected_var <- input$var_select
    
    tmap_options(check.and.fix = TRUE)
    
    res <- filt_data()
    
    if ("name_muni" %in% colnames(res)){
      tm_shape(relocate(res, name_muni)) +
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = c(selected_var)
        ) +
        tm_layout(title = "População dos Municípios Brasileiros",
                  legend.outside = TRUE)
    } else {
      tm_shape(relocate(res, name_state)) + 
        tm_polygons(selected_var,
                    palette = "viridis",
                    title = selected_var,
                    popup.vars = selected_var) +
        tm_layout(title = "População dos Estados Brasileiros",
                  legend.outside = TRUE)
    }
    
  })
  
  session$onSessionEnded(stopApp)
  
  
}
#shinyApp(ui = ui, server = server)

