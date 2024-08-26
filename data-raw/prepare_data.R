library(arrow)
library(dplyr)
library(geobr)
library(microdatasus)
library(censobr)
library(cid10)

sim <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SIM-DO")

cid_subcat <- cid10::cid_subcat

sim <- left_join(sim, cid_subcat, by = c("CAUSABAS" = "cid"))

municipio_mortalidade <- sim |> 
  group_by(code_state = as.numeric(str_sub(CODMUNRES, end = 2)),
           code_muni6 = as.numeric(CODMUNRES), cid = CAUSABAS)  |> 
  summarise(obitos = n()) |>
  ungroup()

estado_mortalidade <- sim |>
  group_by(code_state = as.numeric(str_sub(CODMUNRES, end = 2)), cid = CAUSABAS) |>
  summarise(obitos = n()) |>
  ungroup()

setores_basico <- censobr::read_tracts(2010, dataset = "Basico")|>
  mutate(code_muni6 = as.double(str_sub(code_muni, end = 6)))

municipio_censo <- setores_basico |>
  group_by(code_state = as.numeric(code_state),
           code_muni6 = as.numeric(code_muni6)) |>
  summarise(
    domicilios = as.integer(sum(V001, na.rm = TRUE)),
    residentes = as.integer(sum(V002, na.rm = TRUE))
  )

estado_censo <- setores_basico |>
  group_by(code_state = as.numeric(code_state)) |>
  summarise(
    domicilios = as.integer(sum(V001, na.rm = TRUE)),
    residentes = as.integer(sum(V002, na.rm = TRUE))
  ) 

municipios_geo <- geobr::read_municipality(year=2010) |>
  mutate(code_muni6 = as.double(str_sub(code_muni, end = 6)))

estados_geo <- geobr::read_state(year = 2010)

write_dataset(municipio_censo, "data/municipios_censo",
              format = "parquet", partitioning = "code_state")
write_dataset(estado_censo, "data/estados_censo", format = "parquet")
write_dataset(municipio_mortalidade, "data/municipios_mortalidade",
              format = "parquet", partitioning = "code_state")
write_dataset(estado_mortalidade, "data/estados_mortalidade",
              format = "parquet")
saveRDS(municipios_geo, "data/municipios_geo.rds")
saveRDS(estados_geo, "data/estados_geo.rds")
