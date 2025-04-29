
#_______________________________________________________________________________
###################### Processamento Inicial dos Dados #########################
#_______________________________________________________________________________

#-------------------------------------------------------------------------------
#----------------------------------PARTE 01-------------------------------------
#-------------------------------------------------------------------------------

## Carregando e tratando os dados de mortalidade materna das capitais entre os anos de 
## 2017 e 2022

# Carregando os pacotes necessários
library(tidyr)
library(dplyr)
library(readxl)
library(stringi)
library(readr)

# Carregando a base com os dados gerais
dados_mortalidade_materna <- read_excel("dados_mortalidade/Dados_Artigo_Mortalidade_Materna.xlsx", 
                                               col_types = c("numeric", "text", "numeric", 
                                                             "numeric", "skip", "skip", "skip", 
                                                             "skip", "skip", "skip", "skip", "skip", 
                                                             "numeric", "numeric", "skip", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "skip", "skip", "skip", "skip", "skip", 
                                                             "skip", "skip", "skip", "skip", "skip", 
                                                             "skip", "skip", "skip", "skip", "numeric", 
                                                             "numeric", "skip", "skip", "skip", 
                                                             "skip", "skip", "skip", "skip", "skip", 
                                                             "skip", "skip", "skip"))

# Renomeando colunas com titulos grandes e removendo NA´s
dados_mortalidade_materna = dados_mortalidade_materna |> 
  dplyr::rename(despesas_gastos_saude = `Despesas totais no setor da saúde: total de gasto/total população`) |> 
  dplyr::mutate(mort_materna_10a14_anos = replace_na(mort_materna_10a14_anos, 0),
                mort_materna_15a19_anos = replace_na(mort_materna_15a19_anos,0),
                mort_materna_20a29_anos = replace_na(mort_materna_20a29_anos,0),
                mort_materna_30a39_anos = replace_na(mort_materna_30a39_anos,0),
                mort_materna_40a49_anos = replace_na(mort_materna_40a49_anos,0),
                mort_materna_50a59_anos = replace_na(mort_materna_50a59_anos,0),
                mort_materna_obst_direta = replace_na(mort_materna_obst_direta,0),
                mort_materna_obst_indireta = replace_na(mort_materna_obst_indireta,0),
                obitos_maternos = replace_na(obitos_maternos,0)
                )  # Substitui NAs por 0)

# Removendo o 7º dígito do código dos municípios
dados_mortalidade_materna$cod_munic = substr(dados_mortalidade_materna$cod_munic, 1, 6)

# Arredondar os valores da coluna "despesas_gastos_saude" para 2 casas decimais
dados_mortalidade_materna$despesas_gastos_saude <- round(dados_mortalidade_materna$despesas_gastos_saude, 2)

# Salvando a base de dados geral em csv
write.csv(dados_mortalidade_materna, file = "dados_mortalidade/dados_mortalidade_materna.csv", row.names = FALSE)
