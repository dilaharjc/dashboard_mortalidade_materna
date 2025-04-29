#_______________________________________________________________________________
########################### Criação do Shiny App ###############################
#_______________________________________________________________________________
# Carregando os pacotes necessários
library(plotly)
library(bslib)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(sf)
library(DT)
library(scales) # Adicionado para formatação de números

# Carregando a base de dados geral (Sem geolocalização)
dados_mortalidade_materna <- read_csv("dados_mortalidade/dados_mortalidade_materna.csv")

# Carregando as bases de estados e municípios (com geolocalização)
dados_mortalidade_materna_sf <- readRDS("dados_mortalidade/dados_mortalidade_materna_sf.rds")

# Garantir que a coluna geom seja um objeto sf
if (!inherits(dados_mortalidade_materna_sf$geom, "sfc")) {
  dados_mortalidade_materna_sf <- st_as_sf(dados_mortalidade_materna_sf, wkt = "geom")
}

# Gerando o dashboard
# UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Dashboard Mortalidade Materna",
    titleWidth = 340
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Série Histórica", tabName = "Graficos", icon = icon("stats", lib = "glyphicon")),
      menuItem("Mapas e Ranking", tabName = "Mapas", icon = icon("globe", lib = "glyphicon")),
      menuItem("Informações", tabName = "Info", icon = icon("info-sign", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab: Gráficos
      tabItem(
        tabName = "Graficos",
        h1("Panorama de Mortalidade Materna nas Capitais do Brasil (2017 a 2022)"),
        fluidRow(
          infoBoxOutput("total_obitos"),
          infoBoxOutput("totais_despesas"),
          infoBoxOutput("cobertura_vacinal")
        ),
        fluidRow(
          box(
            title = "Filtros",
            status = "primary",
            width = 12, # Ocupa toda a largura
            solidHeader = TRUE,
            selectInput("capital_selecionada", "Selecione a Capital:", choices = c("Todas", unique(dados_mortalidade_materna$capital))),
            sliderInput("intervalo_anos", "Selecione o Intervalo de Anos:", 
                        min = min(dados_mortalidade_materna$ano), 
                        max = max(dados_mortalidade_materna$ano), 
                        value = c(min(dados_mortalidade_materna$ano), max(dados_mortalidade_materna$ano)))
          )
        ),
        # Primeira linha: Óbitos, Mortalidade Direta e Indireta
        fluidRow(
          column(
            width = 4, # Um terço da tela
            box(
              title = "Óbitos Maternos",
              status = "danger",
              solidHeader = TRUE,
              width = NULL, # Deixa a box ajustar automaticamente à coluna
              plotlyOutput("grafico_obitos", height = 300)
            )
          ),
          column(
            width = 4, # Um terço da tela
            box(
              title = "Mortalidade Obstétrica Direta",
              status = "warning",
              solidHeader = TRUE,
              width = NULL, # Deixa a box ajustar automaticamente à coluna
              plotlyOutput("grafico_mort_direta", height = 300)
            )
          ),
          column(
            width = 4, # Um terço da tela
            box(
              title = "Mortalidade Obstétrica Indireta",
              status = "info",
              solidHeader = TRUE,
              width = NULL, # Deixa a box ajustar automaticamente à coluna
              plotlyOutput("grafico_mort_indireta", height = 300)
            )
          )
        ),
        # Segunda linha: Gastos em Saúde e Cobertura Vacinal
        fluidRow(
          column(
            width = 6, # Metade da tela
            box(
              title = "Gastos em Saúde",
              status = "primary",
              solidHeader = TRUE,
              width = NULL, # Deixa a box ajustar automaticamente à coluna
              plotlyOutput("grafico_gastos_saude", height = 300)
            )
          ),
          column(
            width = 6, # Metade da tela
            box(
              title = "Cobertura Vacinal",
              status = "success",
              solidHeader = TRUE,
              width = NULL, # Deixa a box ajustar automaticamente à coluna
              plotlyOutput("grafico_cobertura_vacinal", height = 300)
            )
          )
        ),
        # Fonte dos Dados
        fluidRow(
          column(
            width = 12, # Ocupa toda a largura
            h5("Fonte: Dados disponibilizados pela Fiocruz", style = "text-align: center; color: gray;")
          )
        )
      ),
      
      # Tab: Mapas e Ranking
      tabItem(
        tabName = "Mapas",
        h1("Ranking das 10 capitais com maiores valores observados entre 2017 e 2022"),
        
        # Filtro para Rankings
        fluidRow(
          box(
            title = "Filtro para gerar os Rankings das Capitais",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("ano_ranking", "Selecione o ano desejado:", choices = unique(dados_mortalidade_materna$ano))
          )
        ),
        
        # Rankings
        fluidRow(
          column(
            width = 3,
            box(
              title = "Ranking de Óbitos",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              DTOutput("ranking_obitos")
            )
          ),
          column(
            width = 3,
            box(
              title = "Ranking de Gastos em Saúde",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              DTOutput("ranking_gastos")
            )
          ),
          column(
            width = 3,
            box(
              title = "Ranking de Cobertura Vacinal",
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              DTOutput("ranking_cobertura")
            )
          ),
          column(
            width = 3,
            box(
              title = "Ranking de IDH",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              DTOutput("ranking_idh")
            )
          )
        ),
        
        # Segundo título: Parte dos mapas
        h1("Comparação de informações entre as capitais"),
        
        # Filtros para Comparação de Capitais
        fluidRow(
          column(
            width = 6,
            box(
              title = "Filtro para Mapa 1",
              status = "warning",
              solidHeader = TRUE,
              width = NULL,
              selectInput("ano_mapa1", "Selecione o Ano:", choices = unique(dados_mortalidade_materna$ano)),
              selectInput("municipio_mapa1", "Selecione a Capital:", choices = unique(dados_mortalidade_materna$capital))
            )
          ),
          column(
            width = 6,
            box(
              title = "Filtro para Mapa 2",
              status = "warning",
              solidHeader = TRUE,
              width = NULL,
              selectInput("ano_mapa2", "Selecione o Ano:", choices = unique(dados_mortalidade_materna$ano)),
              selectInput("municipio_mapa2", "Selecione a Capital:", choices = unique(dados_mortalidade_materna$capital))
            )
          )
        ),
        
        # Mapas para Comparação
        fluidRow(
          column(
            width = 6,
            box(
              title = "Mapa 1",
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              leafletOutput("mapa_interativo1", height = 400)
            )
          ),
          column(
            width = 6,
            box(
              title = "Mapa 2",
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              leafletOutput("mapa_interativo2", height = 400)
            )
          )
        ),
        # Fonte dos Dados
        fluidRow(
          column(
            width = 12, # Ocupa toda a largura
            h5("Fonte: Dados disponibilizados pela Fiocruz", style = "text-align: center; color: gray;")
          )
        )
      ),
    
      # Tab: Informações
      tabItem(
        tabName = "Info",
        h1("Informações Gerais"),
        
        # Logos das Instituições
        fluidRow(
          column(
            width = 6,
            
            img(src = "logo_nupec_ufam.jpeg", height = "100px", style = "display: block; margin: 0 auto;")
          ),
          column(
            width = 6,
            
            img(src = "logo_cee_fiocruz.png",height = "100px")
          )
        ),
        
        # Descrição da Parceria
        p(
          strong("Parceria entre NUPEC UFAM e CEE Fiocruz"),
          br(),
          "Este dashboard é um fruto de uma parceria entre o Núcleo de Pesquisa em Ciência de Dados e Otimização (NUPEC) da Universidade Federal do Amazonas (UFAM) e o Centro de Estudos Epidemiológicos (CEE) da Fundação Oswaldo Cruz (Fiocruz). O projeto, intitulado ",
          em("Análise e Ações de Apoio ao Aprimoramento das Políticas e Práticas Vinculadas à Atenção Primária à Saúde no Âmbito do Sistema Único de Saúde (SUS)"),
          ", visa acompanhar o panorama de mortalidade materna no Brasil de maneira pública e aberta."
        ),
        
        # Cabeçalho das Variáveis
        h2("Descrição das Variáveis Utilizadas"),
        
        # Tabela com as descrições das variáveis
        DT::datatable(
          data.frame(
            Variável = c(
              "Mortalidade Materna",
              "Cobertura Vacinal",
              "Morte Materna Obstétrica Direta",
              "Morte Materna Obstétrica Indireta",
              "Despesas Totais no Setor da Saúde",
              "IDH-M"
            ),
            Fonte = c(
              "Sistema de Informações de Mortalidade - SIM",
              "Sistema de Informação do Programa Nacional de Imunizações (SISPIN)",
              "Sistema de Informações de Mortalidade - SIM",
              "Sistema de Informações de Mortalidade - SIM",
              "SIA e SIH",
              "Censo 2010"
            ),
            Descrição = c(
              "Óbitos maternos por ano e na capital",
              "Cobertura vacinal média de todas as vacinas oferecidas pelo SUS",
              "Óbitos maternos diretos por ano e na capital",
              "Óbitos maternos indiretos por ano e na capital",
              "Repasse de todos os blocos de financiamento da saúde referente ao município",
              "Índice de Desenvolvimento Humano Municipal"
            )
          ),
          rownames = FALSE,
          options = list(
            pageLength = 6,
            searching = TRUE,
            paging = TRUE,
            scrollX = TRUE
          ),
          colnames = c("Variável", "Fonte", "Descrição")
        ),
        # Links para replicar o trabalho
        h3("Replicando o Trabalho"),
        p(
          "Os códigos utilizados para criação deste dashboard estão disponíveis no GitHub:",
          br(),
          a("Acesse o repositório no GitHub", href = "https://github.com/dilaharjc/dashboard_mortalidade_materna", target = "_blank"),
          br(),
          "A base de dados utilizada na análise foi disponibilizada através de uma API no Supabase:",
          br(),
          a("Acesse a API no Supabase", href = "https://supabase.com/dashboard/project/ryuburcalvcpantttbns/editor/17643?schema=public", target = "_blank")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Filtro dinâmico dos dados
  dados_filtrados <- reactive({
    dados <- dados_mortalidade_materna %>%
      filter(ano >= input$intervalo_anos[1] & ano <= input$intervalo_anos[2])
    
    if (input$capital_selecionada != "Todas") {
      dados <- dados %>% filter(capital == input$capital_selecionada)
    }
    
    return(dados)
  })
  
  # Botões: Total de Óbitos
  output$total_obitos <- renderInfoBox({
    total <- sum(dados_filtrados()$obitos_maternos, na.rm = TRUE)
    formatted_value <- formatC(total, format = "d", big.mark = ".", decimal.mark = ",")
    infoBox("Total de Óbitos Maternos", formatted_value, icon = icon("heartbeat"), color = "red")
  })
  
  # Botões: Totais de Despesas
  output$totais_despesas <- renderInfoBox({
    total <- sum(dados_filtrados()$despesas_gastos_saude, na.rm = TRUE)
    formatted_value <- formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    infoBox("Total de Gastos em Saúde/População (R$)", formatted_value, icon = icon("usd"), color = "blue")
  })
  
  # Botões: Cobertura Vacinal
  output$cobertura_vacinal <- renderInfoBox({
    media <- mean(dados_filtrados()$cobertura_vacinal, na.rm = TRUE)
    infoBox("Cobertura Vacinal Média (%)", round(media, 2), icon = icon("medkit"), color = "green")
  })
  
  # Gráfico: Óbitos Maternos
  output$grafico_obitos <- renderPlotly({
    dados <- dados_filtrados() %>%
      group_by(ano) %>%
      summarise(obitos_maternos = sum(obitos_maternos))
    
    ggplotly(
      ggplot(dados, aes(x = ano, y = obitos_maternos)) +
        geom_line(color = "red", size = 1) +
        geom_point(color = "red", size = 3) +
        labs(title = "Óbitos Maternos ao Longo do Tempo", x = "Ano", y = "Número de Óbitos") +
        theme_minimal()
    )
  })
  
  # Gráfico: Mortalidade Obstétrica Direta
  output$grafico_mort_direta <- renderPlotly({
    dados <- dados_filtrados() %>%
      group_by(ano) %>%
      summarise(mort_direta = sum(mort_materna_obst_direta))
    
    ggplotly(
      ggplot(dados, aes(x = ano, y = mort_direta)) +
        geom_line(color = "orange", size = 1) +
        geom_point(color = "orange", size = 3) +
        labs(title = "Série: Mortalidade Obstétrica Direta", x = "Ano", y = "Número de Casos") +
        theme_minimal()
    )
  })
  
  # Gráfico: Mortalidade Obstétrica Indireta
  output$grafico_mort_indireta <- renderPlotly({
    dados <- dados_filtrados() %>%
      group_by(ano) %>%
      summarise(mort_indireta = sum(mort_materna_obst_indireta))
    
    ggplotly(
      ggplot(dados, aes(x = ano, y = mort_indireta)) +
        geom_line(color = "purple", size = 1) +
        geom_point(color = "purple", size = 3) +
        labs(title = "Série: Mortalidade Obstétrica Indireta", x = "Ano", y = "Número de Casos") +
        theme_minimal()
    )
  })
  
  # Gráfico: Gastos em Saúde
  output$grafico_gastos_saude <- renderPlotly({
    dados <- dados_filtrados() %>%
      group_by(ano) %>%
      summarise(despesas_saude = mean(despesas_gastos_saude, na.rm = TRUE))
    
    ggplotly(
      ggplot(dados, aes(x = ano, y = despesas_saude)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "blue", size = 3) +
        labs(title = "Gastos em Saúde ao Longo do Tempo", x = "Ano", y = "Valor Médio (R$)") +
        theme_minimal()
    )
  })
  
  # Gráfico: Cobertura Vacinal
  output$grafico_cobertura_vacinal <- renderPlotly({
    dados <- dados_filtrados() %>%
      group_by(ano) %>%
      summarise(cobertura_vacinal = mean(cobertura_vacinal, na.rm = TRUE))
    
    ggplotly(
      ggplot(dados, aes(x = ano, y = cobertura_vacinal)) +
        geom_line(color = "green", size = 1) +
        geom_point(color = "green", size = 3) +
        labs(title = "Cobertura Vacinal ao Longo do Tempo", x = "Ano", y = "Percentual (%)") +
        theme_minimal()
    )
  })
  
  # Rankings
  gerar_ranking <- function(coluna, ano) {
    dados_mortalidade_materna %>%
      filter(ano == input$ano_ranking) %>%
      select(capital, .data[[coluna]]) %>%
      arrange(desc(.data[[coluna]])) %>%
      slice_head(n = 10)
  }
  
  output$ranking_obitos <- renderDT({
    ranking <- gerar_ranking("obitos_maternos", input$ano_ranking)
    if (nrow(ranking) == 0) return(NULL)
    datatable(ranking, options = list(pageLength = 10, searching = FALSE), rownames = FALSE, colnames = c("Capital", "Quantidade de Óbitos"))
  })
  
  output$ranking_gastos <- renderDT({
    ranking <- gerar_ranking("despesas_gastos_saude", input$ano_ranking)
    if (nrow(ranking) == 0) return(NULL)
    datatable(ranking, options = list(pageLength = 10, searching = FALSE), rownames = FALSE, colnames = c("Capital", "Gastos em Saúde (R$)"))
  })
  
  output$ranking_cobertura <- renderDT({
    ranking <- gerar_ranking("cobertura_vacinal", input$ano_ranking)
    if (nrow(ranking) == 0) return(NULL)
    datatable(ranking, options = list(pageLength = 10, searching = FALSE), rownames = FALSE, colnames = c("Capital", "Cobertura Vacinal (%)"))
  })
  
  output$ranking_idh <- renderDT({
    ranking <- gerar_ranking("idh", input$ano_ranking)
    if (nrow(ranking) == 0) return(NULL)
    datatable(ranking, options = list(pageLength = 10, searching = FALSE), rownames = FALSE, colnames = c("Capital", "IDH"))
  })
  
  # Mapas para Comparação
  gerar_mapa1 <- function(ano, capital) {
    # Filtra os dados pelo ano selecionado
    filtered_data <- dados_mortalidade_materna_sf %>%
      filter(ano == input$ano_mapa1)
    
    # Filtra os dados pela capital selecionada
    capital_data <- filtered_data %>%
      filter(capital == input$municipio_mapa1)
    
    # Verifica se há dados para a capital selecionada
    if (nrow(capital_data) == 0) {
      return(NULL) # Retorna NULL se não houver dados
    }
    
    # Calcula o centróide da geometria da capital selecionada
    centro <- st_coordinates(st_centroid(capital_data$geom))
    
    # Define uma paleta de cores para o mapa de calor
    color_palette <- colorNumeric(palette = c("blue", "red"), domain = filtered_data$obitos_maternos)
    
    # Gera o mapa
    leaflet(filtered_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_palette(obitos_maternos),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = paste(
          "Município:", filtered_data$capital,
          "<br>Óbitos:", filtered_data$obitos_maternos,
          "<br>Gastos em Saúde:", filtered_data$despesas_gastos_saude,
          "<br>Cobertura Vacinal:", filtered_data$cobertura_vacinal,
          "<br>IDH:", filtered_data$idh,
          "<br>Mortalidade Obstétrica Direta:", filtered_data$mort_materna_obst_direta,
          "<br>Mortalidade Obstétrica Indireta:", filtered_data$mort_materna_obst_indireta
        ),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      setView(lng = centro[1], lat = centro[2], zoom = 8) %>%
      addLegend(
        pal = color_palette,
        values = ~obitos_maternos,
        position = "bottomright",
        title = "Mapa de Calor: Óbitos Maternos",
        opacity = 0.7
      )
  }
  
  output$mapa_interativo1 <- renderLeaflet({
    req(input$ano_mapa1, input$municipio_mapa1) # Garante que os inputs estão definidos
    gerar_mapa1(input$ano_mapa1, input$municipio_mapa1)
  })
  
  gerar_mapa2 <- function(ano, capital) {
    # Filtra os dados pelo ano selecionado
    filtered_data <- dados_mortalidade_materna_sf %>%
      filter(ano == input$ano_mapa2)
    
    # Filtra os dados pela capital selecionada
    capital_data <- filtered_data %>%
      filter(capital == input$municipio_mapa2)
    
    # Verifica se há dados para a capital selecionada
    if (nrow(capital_data) == 0) {
      return(NULL) # Retorna NULL se não houver dados
    }
    
    # Calcula o centróide da geometria da capital selecionada
    centro <- st_coordinates(st_centroid(capital_data$geom))
    
    # Define uma paleta de cores para o mapa de calor
    color_palette <- colorNumeric(palette = c("blue", "red"), domain = filtered_data$obitos_maternos)
    
    # Gera o mapa
    leaflet(filtered_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_palette(obitos_maternos),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = paste(
          "Município:", filtered_data$capital,
          "<br>Óbitos:", filtered_data$obitos_maternos,
          "<br>Gastos em Saúde:", filtered_data$despesas_gastos_saude,
          "<br>Cobertura Vacinal:", filtered_data$cobertura_vacinal,
          "<br>IDH:", filtered_data$idh,
          "<br>Mortalidade Obstétrica Direta:", filtered_data$mort_materna_obst_direta,
          "<br>Mortalidade Obstétrica Indireta:", filtered_data$mort_materna_obst_indireta
        ),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      setView(lng = centro[1], lat = centro[2], zoom = 8) %>%
      addLegend(
        pal = color_palette,
        values = ~obitos_maternos,
        position = "bottomright",
        title = "Mapa de Calor: Óbitos Maternos",
        opacity = 0.7
      )
  }
  
  output$mapa_interativo2 <- renderLeaflet({
    req(input$ano_mapa2, input$municipio_mapa2) # Garante que os inputs estão definidos
    gerar_mapa2(input$ano_mapa2, input$municipio_mapa2)
  })
  
}

# Executar o app
shinyApp(ui, server)
