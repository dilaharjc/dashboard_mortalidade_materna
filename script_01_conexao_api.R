#_______________________________________________________________________________
############### Carregando e salvando os dados de dengue VIA API ###############
#_______________________________________________________________________________

# Carregando os pacotes necessários
library(httr)
library(jsonlite)

# Definindo chave da API
api_key = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJ5dWJ1cmNhbHZjcGFudHR0Ym5zIiwicm9sZSI6ImFub24iLCJpYXQiOjE3Mzg2MzMxMzIsImV4cCI6MjA1NDIwOTEzMn0.2kdKyCeAS4OqsU7Fjoi660NUzocaGwOF_2x5-GTmJQ4"

# Fazendo a requisição GET da tabela geral
response = GET(
  url = "https://ryuburcalvcpantttbns.supabase.co/rest/v1/dados_mortalidade_materna",
  add_headers(
    `apikey` = api_key,
    `Authorization` = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
)

# Verificando o status da resposta
if (status_code(response) == 200) {
  # Parsear o conteúdo JSON
  dados_mortalidade_materna_api = fromJSON(content(response, "text"))
} else {
  print(paste("Erro na requisição:", status_code(response)))
}

# Salvando os dados em csv na pasta
write.csv(dados_mortalidade_materna_api, file = "dados_mortalidade/dados_mortalidade_materna_api.csv", row.names = FALSE)
