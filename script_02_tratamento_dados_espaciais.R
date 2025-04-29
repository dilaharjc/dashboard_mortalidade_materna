#_______________________________________________________________________________
##### Obtendo os dados geográficos e adicionando-os como uma nova coluna #######
######## Essa tabela será usada para a criação dos mapas no dashboard ##########
#_______________________________________________________________________________

# Carregando os pacotes necessários
library(geobr)
library(readr)
library(dplyr)
library(tidyr)
library(sf)

# Carregando a base geral para ficar salva na memoria
dados_mortalidade_materna = read_csv("dados_mortalidade/dados_mortalidade_materna.csv")

# Transformando a coluna de codigo do muicipio em numerico
dados_mortalidade_materna$cod_munic = as.numeric(dados_mortalidade_materna$cod_munic)

# Obtendo dados geográficos dos estados e municípios
municipios = read_municipality(year = 2020)

# Criando código de 6 dígitos para os municípios
municipios$cod_munic = as.numeric(substr(municipios$code_muni, 1, 6))

# Selecionando apenas as colunas necessárias
municipios = municipios |> 
  dplyr::select(cod_munic, geom)

# Juntando os dados geográficos com as tabelas padronizadas atualizadas
dados_mortalidade_materna_geo = dados_mortalidade_materna |> 
  inner_join(municipios, by = "cod_munic")

# Transformando os dataframes em um objeto sf
dados_mortalidade_materna_sf = st_as_sf(dados_mortalidade_materna_geo)

# Transformação para CRS correto
dados_mortalidade_materna_sf = st_transform(dados_mortalidade_materna_sf, crs = 4326)

# Salvando as bases em rds para usa-las posteriormente
saveRDS(dados_mortalidade_materna_sf, "dados_mortalidade/dados_mortalidade_materna_sf.rds")
