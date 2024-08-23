library(dplyr)
library(microdatasus)
library(censobr)
library(cid10)

sim <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SIM-DO")


cid_subcat <- cid10::cid_subcat

data <- left_join(sim, cid_subcat, by = c("CAUSABAS" = "cid"))

municipio_mortalidade <- data |> 
  group_by(code_muni6 = as.numeric(CODMUNRES), cid = CAUSABAS)  |> 
  summarise(obitos = n())
  

estado_mortalidade <- data |>
  group_by(code_state = as.numeric(str_sub(CODMUNRES, end = 2)), cid = CAUSABAS) |>
  summarise(obitos = n())

setores_basico <- censobr::read_tracts(2010, dataset = "Basico")|>
  mutate(code_muni6 = as.double(str_sub(code_muni, end = 6)))

municipio_censo <- setores_basico |>
  group_by(code_state = as.numeric(code_state) , code_muni6 = as.numeric(code_muni6)) |>
  summarise(
    domicilios = as.integer(sum(V001)),
    residentes = as.integer(sum(V002))
  ) |>
  collect() |>
<<<<<<< Updated upstream
  left_join(obitos_mun, by = "code_muni6") 
  

estado_data <- setores_basico |>
=======
  mutate(code_state = as.double(str_sub(code_muni6, end = 2)))
  
estado_censo <- setores_basico |>
>>>>>>> Stashed changes
  group_by(code_state = as.numeric(code_state)) |>
  summarise(
    domicilios = as.integer(sum(V001)),
    residentes = as.integer(sum(V002))
  ) |>
<<<<<<< Updated upstream
  collect() |>
  left_join(obitos_est, by = "code_state") 
  
  

write.csv(municipio_data, "/data/municipios_data.csv")
write.csv(estado_data, "/data/estados_data.csv")
=======
  collect()
  

# municipio_data <- setores_basico |>
#   group_by(code_state = as.numeric(code_state) , code_muni6 = as.numeric(code_muni6)) |>
#   summarise(
#     domicilios = as.integer(sum(V001)),
#     residentes = as.integer(sum(V002))
#   ) |>
#   collect() |>
#   mutate(code_state = as.double(str_sub(code_muni6, end = 2))) |>
#   left_join(obitos_mun, by = "code_muni6") 
# 
# 
# estado_data <- setores_basico |>
#   group_by(code_state = as.numeric(code_state)) |>
#   summarise(
#     domicilios = as.integer(sum(V001)),
#     residentes = as.integer(sum(V002))
#   ) |>
#   collect() |>
#   left_join(obitos_est, by = "code_state") 




write_dataset(municipio_censo, "../Dashboard-Municipios/data/municipios_censo", format = "parquet")
write_dataset(estado_censo, "../Dashboard-Municipios/data/estados_censo", format = "parquet")
write_dataset(municipio_mortalidade, "../Dashboard-Municipios/data/municipios_mortalidade", format = "parquet")
write_dataset(estado_mortalidade, "../Dashboard-Municipios/data/estados_mortalidade", format = "parquet")


>>>>>>> Stashed changes

