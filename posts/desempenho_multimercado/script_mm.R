library(dplyr)
library(dbplyr)
library(tidyr)
library(furrr)
library(reactable)
library(htmltools)
library(ggplot2)
library(scales)
library(ggimprensa)
library(zoo)
library(echarts4r)

# Conectando a base de dados
con <- DBI::dbConnect(duckdb::duckdb(), 
                      dbdir = "posts/desempenho_fundos_rf/fundos_db.duckdb",
                      read_only = FALSE)

informes_diarios <- tbl(con,"informes_diarios") 

informes_diarios %>% 
  # Substitui valores ausentes em CNPJ_FUNDO e TP_FUNDO pelos valores da classe correspondente
  mutate(
    CNPJ_FUNDO = case_when(is.na(CNPJ_FUNDO) ~ CNPJ_FUNDO_CLASSE,
                           .default = CNPJ_FUNDO),
    TP_FUNDO = case_when(is.na(TP_FUNDO) ~ TP_FUNDO_CLASSE,
                         .default = TP_FUNDO)
  ) %>% 
  # Remove qualquer caractere que não seja número do CNPJ_FUNDO usando expressão regular SQL
  mutate(CNPJ_FUNDO = sql("regexp_replace(CNPJ_FUNDO, '[^0-9]', '', 'g')")) %>% 
  
  # Junta com a tabela fundos_175_anbima para enriquecer os dados com metainformações dos fundos
  left_join(
    tbl(con, "fundos_175_anbima") %>% 
      filter(`CNPJ do Fundo` != "58137699000116") %>%  # Esse fundo em especifico possuia valores duplicados
      select(
        `Código ANBIMA`, Estrutura, `CNPJ do Fundo`, `Nome Comercial`, Status,
        `Categoria ANBIMA`, `Tipo ANBIMA`, `Composição do Fundo`,
        `Aberto Estatutariamente`, `Tributação Alvo`, `Primeiro Aporte`,
        `Tipo de Investidor`, `Característica do Investidor`,
        `Cota de Abertura`, `Aplicação Inicial Mínima`, 
        `Prazo Pagamento Resgate em dias`
      ),
    by = c("CNPJ_FUNDO" = "CNPJ do Fundo")
  ) %>% 
  
  # Filtra apenas fundos classificados como "Renda Fixa"
  filter(`Categoria ANBIMA` == "Multimercados") -> fundos_mm

fundos_mm %>%
  distinct(CNPJ_FUNDO, `Categoria ANBIMA`, `Tipo ANBIMA`) %>%  
  count(`Categoria ANBIMA`, `Tipo ANBIMA`) %>%  
  collect() %>% 
  arrange(-n) %>%  
  rename(Quantidade = n) %>%  
  mutate(`Part (%)` = Quantidade / sum(Quantidade) * 100) %>%  
  mutate(`Part Acum (%)` = cumsum(`Part (%)`)) %>%  
  reactable::reactable(
    pagination = FALSE,  
    defaultColDef = colDef(minWidth = 120),  
    resizable = TRUE,  
    columns = list(
      `Categoria ANBIMA` = colDef(width = 120),  
      `Tipo ANBIMA` = colDef(width = 360),  
      Quantidade = colDef(
        width = 120,
        align = "right",  
        format = colFormat(separators = TRUE),  
        footer = function(values) {  
          htmltools::tags$b("Total: ") %>%  
            htmltools::tagAppendChildren(htmltools::tags$span(sum(values)))  
        }
      ),
      `Part (%)` = colDef(
        width = 80,
        align = "right",  
        format = colFormat(suffix = "%", digits = 2),  
        footer = function(values) {  
          htmltools::tags$b("100%")  
        }
      ),
      `Part Acum (%)` = colDef(
        width = 120,
        align = "right",  
        format = colFormat(suffix = "%", digits = 2),  
        footer = function(values) {  
          htmltools::tags$b("100%")  
        }
      )
    )
  )

extrato_fi <- readr::read_delim("posts/desempenho_fundos_rf/extrato_fi.csv", 
                  delim = ";", 
                  escape_double = FALSE, 
                  locale = readr::locale(encoding = "WINDOWS-1252"), 
                  trim_ws = TRUE,
                  quote = "") %>% 
  mutate(CNPJ_FUNDO_CLASSE = stringr::str_remove_all(CNPJ_FUNDO_CLASSE,"\\.|/|-")) 


fundos_mm %>% 
  left_join(
    tbl(con,"extrato_fi") %>% 
      select(CNPJ_FUNDO_CLASSE,PUBLICO_ALVO, DISTRIB, POLIT_INVEST, 
             FUNDO_COTAS,FUNDO_ESPELHO, APLIC_MIN, ATUALIZ_DIARIA_COTA,
             TAXA_SAIDA_PAGTO_RESGATE, TAXA_ADM, EXISTE_TAXA_PERFM,
             TAXA_PERFM, PARAM_TAXA_PERFM, CALC_TAXA_PERFM,
             EXISTE_TAXA_INGRESSO, EXISTE_TAXA_SAIDA,
             INVEST_EXTERIOR, ATIVO_CRED_PRIV),
    by = c("CNPJ_FUNDO" = "CNPJ_FUNDO_CLASSE") 
  ) -> fundos_mm


fundos_mm %>% 
  distinct(CNPJ_FUNDO,`Tipo de Investidor`,`Característica do Investidor`) %>% 
  count(`Tipo de Investidor`,`Característica do Investidor`) %>% 
  collect() %>% 
  arrange(-n) %>% 
  rename(Quantidade = n) %>% 
  reactable::reactable(
    pagination = FALSE,  # Desativa a paginação da tabela
    resizable = TRUE,  # Permite redimensionamento das colunas
    columns = list(
      Quantidade = colDef(
        align = "right",  # Alinha os valores numéricos à direita
        format = colFormat(separators = TRUE),  # Formata os números com separadores de milhar
        footer = function(values) {  # Adiciona um rodapé à coluna Quantidade
          tags$b("Total: ") %>%  # Coloca a palavra "Total" em negrito
            tagAppendChildren(tags$span(sum(values)))  # Adiciona a soma total ao lado
        }
      )
    )
  )


tbl(con,"extrato_fi")  %>% 
  distinct(DT_COMPTC, CNPJ_FUNDO_CLASSE, POLIT_INVEST, FUNDO_ESPELHO, FUNDO_COTAS) %>%
  collect() %>% 
  filter(CNPJ_FUNDO_CLASSE %in% collect(distinct(fundos_mm, CNPJ_FUNDO))$CNPJ_FUNDO) %>% 
  mutate(DT_COMPTC = as.Date(DT_COMPTC)) %>%  # Converte a data para o formato adequado
  arrange(DT_COMPTC) %>%  # Ordena as datas de competência
  group_by(CNPJ_FUNDO_CLASSE) %>% 
  summarise_all(last) %>%  # Mantém apenas a última entrada por fundo
  ungroup() %>% 
  mutate(
    FUNDO_ESPELHO = case_when(
      is.na(FUNDO_ESPELHO) ~ "Não",
      FUNDO_ESPELHO == "N" ~ "Não",
      .default = "Sim"
    ),
    FUNDO_COTAS = case_when(
      FUNDO_COTAS == "N" ~ "Não",
      .default = "Sim"
    )
  ) %>% 
  count(POLIT_INVEST, FUNDO_ESPELHO, FUNDO_COTAS) %>% 
  rename(
    `Política de Investimento` = POLIT_INVEST,
    `Fundo Espelho` = FUNDO_ESPELHO,
    `Fundo de Cotas` = FUNDO_COTAS,
    Quantidade = n
  ) %>%
  reactable(
    pagination = FALSE,  # Desativa a paginação
    resizable = TRUE,  # Permite redimensionamento das colunas
    columns = list(
      `Política de Investimento` = colDef(width = 250),  # Define largura da coluna
      `Fundo Espelho` = colDef(width = 150),  # Define largura da coluna
      `Fundo de Cotas` = colDef(width = 150),  # Define largura da coluna
      Quantidade = colDef(
        align = "right",  # Alinha os valores numéricos à direita
        format = colFormat(separators = TRUE),  # Formata os números com separadores de milhar
        footer = function(values) {  # Adiciona um rodapé à coluna Quantidade
          tags$b("Total: ") %>%  # Deixa a palavra "Total" em negrito
            tagAppendChildren(tags$span(sum(values)))  # Exibe a soma total ao lado
        }
      )
    )
  )

tbl(con,"extrato_fi") %>% 
  collect() %>% 
  filter(CNPJ_FUNDO_CLASSE %in% collect(distinct(fundos_mm, CNPJ_FUNDO))$CNPJ_FUNDO) %>%
  distinct(DT_COMPTC, 
           CNPJ_FUNDO_CLASSE, 
           POLIT_INVEST, 
           EXISTE_TAXA_INGRESSO, 
           EXISTE_TAXA_PERFM, 
           EXISTE_TAXA_SAIDA) %>% 
  count(POLIT_INVEST, 
        EXISTE_TAXA_INGRESSO, 
        EXISTE_TAXA_PERFM, 
        EXISTE_TAXA_SAIDA) %>%
  mutate(
    EXISTE_TAXA_INGRESSO = case_match(
      EXISTE_TAXA_INGRESSO, 
      "S" ~ "Sim", 
      "N" ~ "Não", 
      .default = EXISTE_TAXA_INGRESSO),
    EXISTE_TAXA_PERFM = case_match(
      EXISTE_TAXA_PERFM, 
      "S" ~ "Sim", 
      "N" ~ "Não", 
      .default = EXISTE_TAXA_PERFM),
    EXISTE_TAXA_SAIDA = case_match(
      EXISTE_TAXA_SAIDA, 
      "S" ~ "Sim", 
      "N" ~ "Não", 
      .default = EXISTE_TAXA_SAIDA)
  ) %>% 
  rename(
    `Política de Investimento` = POLIT_INVEST,
    `Taxa de Ingresso` = EXISTE_TAXA_INGRESSO,
    `Taxa de Performance` = EXISTE_TAXA_PERFM,
    `Taxa de Saída` = EXISTE_TAXA_SAIDA,
    Quantidade = n
  ) %>%
  reactable(
    pagination = FALSE,  # Desativa a paginação
    resizable = TRUE,  # Permite redimensionamento das colunas
    columns = list(
      `Política de Investimento` = colDef(width = 300),  # Define largura da coluna
      `Taxa de Ingresso` = colDef(width = 120),  # Define largura da coluna
      `Taxa de Performance` = colDef(width = 120),  # Define largura da coluna
      `Taxa de Saída` = colDef(width = 120),  # Define largura da coluna
      Quantidade = colDef(
        align = "right",  # Alinha os valores numéricos à direita
        format = colFormat(separators = TRUE),  # Formata os números com separadores de milhar
        footer = function(values) {  # Adiciona um rodapé à coluna Quantidade
          tags$b("Total: ") %>%  # Deixa a palavra "Total" em negrito
            tagAppendChildren(tags$span(sum(values)))  # Exibe a soma total ao lado
        }
      )
    )
  )


fundos_mm %>%
  filter(!is.na(POLIT_INVEST)) %>%
  select(DT_COMPTC,`Categoria ANBIMA`,CAPTC_DIA,RESG_DIA) %>% 
  collect() %>% 
  mutate(Competência = as.yearqtr(DT_COMPTC)) %>%
  filter(Competência >= "2010 Q1" & Competência <= "2024 Q4") %>%
  summarise(
    Captação = sum(CAPTC_DIA) / 1e9,
    Resgates = -sum(RESG_DIA) / 1e9,
    .by = c("Competência", "Categoria ANBIMA")
  ) %>%
  mutate(CAPTC_LIQ = Captação + Resgates) %>%
  pivot_wider(
    names_from = "Categoria ANBIMA",
    values_from = "CAPTC_LIQ"
  ) %>%
  rename(`Captação líquida` = Multimercados) %>%
  arrange(Competência) %>%
  mutate(`Captação líquida acumulada` = cumsum(`Captação líquida`),
         Competência = as.Date(Competência)) %>%
  arrange(Competência) %>% 
  e_charts(Competência) %>%
  e_bar(Captação, name = "Captação", 
        color = "steelblue", 
        stack = "total") %>%
  e_bar(Resgates, 
        name = "Resgates", 
        color = "tomato", 
        stack = "total") %>%
  e_line(`Captação líquida`, 
         name = "Captação líquida", 
         color = "darkgreen") %>%
  e_line(`Captação líquida acumulada`, 
         name = "Captação líquida acumulada", 
         color = "purple") %>%
  e_title(
    text = "Histórico de Captação",
    subtext = "Captação e Resgates com sua evolução trimestral desde o início de 2010"
  ) %>%
  e_legend(right = TRUE) %>%
  e_tooltip(trigger = "axis") %>%
  e_y_axis(
    axisLabel = list(formatter = JS("function(value){ return value + ' B'; }"))
  ) %>%
  e_x_axis(axisLabel = list(rotate = 45)) %>%
  e_grid(left = "10%", right = "10%") 

fundos_mm %>%
  filter(!is.na(POLIT_INVEST)) %>%
  mutate(
    ano = strftime(DT_COMPTC, "%Y"),
    mes = sql("CAST(strftime(DT_COMPTC, '%m') AS INTEGER)"),
    trimestre = paste0("T", (mes - 1) %/% 3 + 1),
    Competência = paste(ano, trimestre)
  ) %>%
  filter(Competência >= "2010 T1" & Competência <= "2024 T4") %>%
  summarise(
    Captação = sum(CAPTC_DIA) / 1e9,
    Resgates = -sum(RESG_DIA) / 1e9,
    .by = c("Competência", "Tipo ANBIMA")
  ) %>%
  mutate(
    CAPTC_LIQ = Captação + Resgates
  ) %>%
  arrange(Competência) %>%
  collect() %>% 
  group_by(`Tipo ANBIMA`) %>%
  e_charts(Competência) %>%
  e_bar(CAPTC_LIQ, stack = "total", series = "Tipo ANBIMA") %>%
  e_tooltip(trigger = "axis") %>%
  e_title("Captação Líquida por Tipo ANBIMA ao Longo do Tempo") %>%
  e_x_axis(name = "Competência") %>%
  e_y_axis(name = "Captação Líquida (em bilhões)") %>%
  e_legend(
    orient = "horizontal",
    bottom = 10,
    left = "center"
  ) %>%
  e_grid(
    bottom = 120,  
    top = 60
  )


fundos_mm %>%
  filter(!is.na(POLIT_INVEST)) %>%
  mutate(
    ano = strftime(DT_COMPTC, "%Y"),
    mes = sql("CAST(strftime(DT_COMPTC, '%m') AS INTEGER)"),
    trimestre = paste0("T", (mes - 1) %/% 3 + 1),
    Competência = paste(ano, trimestre)
  ) %>%
  filter(Competência >= "2010 T1" & Competência <= "2024 T4") %>% 
  summarise(`Custódia` = sum(VL_PATRIM_LIQ),
            .by = c("Competência","Tipo ANBIMA")) %>% 
  collect() %>% 
  arrange(Competência) %>% 
  mutate(`Custódia` = `Custódia` / 1e9) %>% 
  group_by(`Tipo ANBIMA`) %>% 
  e_charts(Competência) %>%
  e_bar(`Custódia`,stack = "total",series = "Tipo ANBIMA") %>% 
  e_tooltip(trigger = "axis") %>%
  e_title("Patrimônio Líquido por Tipo ANBIMA ao Longo do Tempo") %>%
  e_y_axis(name = "Custódia (em bilhões)") %>%
  e_legend(
    orient = "horizontal",
    bottom = 10,
    left = "center"
  ) %>%
  e_grid(
    bottom = 120,
    top = 60
  )



fundos_mm %>%
  filter(!is.na(POLIT_INVEST)) %>%
  mutate(
    Ano = as.numeric(strftime(DT_COMPTC, "%Y"))
  ) %>%
  filter(Ano >= 2010 & Ano <= 2024) %>% 
  select(Ano, CNPJ_FUNDO, DT_COMPTC,VL_QUOTA) %>% 
  arrange(Ano, CNPJ_FUNDO, DT_COMPTC) %>% 
  collect() %>% 
  summarise(`Rent (%)` = ((last(VL_QUOTA)/first(VL_QUOTA)) - 1)*100,
            .by = c("Ano", "CNPJ_FUNDO")) -> rentabilidade_fundos_mm



rentabilidade_fundos_mm %>% 
  left_join(
    fundos_mm %>% 
      distinct(CNPJ_FUNDO, `Tipo ANBIMA`) %>% 
      collect()
  ) %>% 
  left_join(
    rbcb::get_series(4391) %>% 
      mutate(Ano = lubridate::year(date)) %>% 
      filter(Ano >= 2010) %>% 
      summarise(CDI = last((cumprod((`4391`/100)+1)-1)*100),
                .by = Ano)
  ) -> rentabilidade_fundos_mm

rentabilidade_fundos_mm %>% 
  mutate(Outlier = case_when(`Rent (%)` > 200 | `Rent (%)` < -200 ~ "Out",
                             .default = "In")) %>% 
  filter(Outlier == "Out") %>% 
  distinct(CNPJ_FUNDO) -> fundos_out


linhas_cdi <- rentabilidade_fundos_mm %>%
  distinct(`Tipo ANBIMA`, Ano, CDI) %>%
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Livre",
    "Multimercados Macro" ,
    "Multimercados Investimento no Exterior",
    "Multimercados Juros e Moedas")) %>% 
  mutate(
    x = as.factor(Ano),
    x_num = as.numeric(x)
  )


rentabilidade_fundos_mm %>% 
  filter(!CNPJ_FUNDO %in% fundos_out$CNPJ_FUNDO) %>% 
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Livre",
    "Multimercados Macro" ,
    "Multimercados Investimento no Exterior",
    "Multimercados Juros e Moedas")) %>% 
  ggplot(aes(x = factor(Ano), y = `Rent (%)`)) +
  geom_violin(fill = "skyblue", color = "gray30", alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_segment(
    data = linhas_cdi,
    aes(x = x_num - 0.6, xend = x_num + 0.6, y = CDI, yend = CDI),
    color = "red", linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ `Tipo ANBIMA`, scales = "free_y", ncol = 2) +
  labs(
    title = "Distribuição da Rentabilidade por Tipo ANBIMA e Ano",
    x = "Ano",
    y = "Rentabilidade (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )


linhas_cdi <- rentabilidade_fundos_mm %>%
  distinct(`Tipo ANBIMA`, Ano, CDI) %>%
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Balanceados",
    "Multimercados Dinâmico",
    "Multimercados Trading",
    "Multimercados Capital Protegido")) %>% 
  mutate(
    x = as.factor(Ano),
    x_num = as.numeric(x)
  )


rentabilidade_fundos_mm %>% 
  filter(!CNPJ_FUNDO %in% fundos_out$CNPJ_FUNDO) %>% 
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Balanceados",
    "Multimercados Dinâmico",
    "Multimercados Trading",
    "Multimercados Capital Protegido")) %>% 
  ggplot(aes(x = factor(Ano), y = `Rent (%)`)) +
  geom_violin(fill = "skyblue", color = "gray30", alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_segment(
    data = linhas_cdi,
    aes(x = x_num - 0.6, xend = x_num + 0.6, y = CDI, yend = CDI),
    color = "red", linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ `Tipo ANBIMA`, scales = "free_y", ncol = 2) +
  labs(
    title = "Distribuição da Rentabilidade por Tipo ANBIMA e Ano",
    x = "Ano",
    y = "Rentabilidade (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )


linhas_cdi <- rentabilidade_fundos_mm %>%
  distinct(`Tipo ANBIMA`, Ano, CDI) %>%
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Long and Short - Direcional",
    "Multimercados Long and Short - Neutro",
    "Multimercados Estratégia Específica")) %>% 
  mutate(
    x = as.factor(Ano),
    x_num = as.numeric(x)
  )


rentabilidade_fundos_mm %>% 
  filter(!CNPJ_FUNDO %in% fundos_out$CNPJ_FUNDO) %>% 
  filter(`Tipo ANBIMA` %in% c(
    "Multimercados Long and Short - Direcional",
    "Multimercados Long and Short - Neutro",
    "Multimercados Estratégia Específica")) %>% 
  ggplot(aes(x = factor(Ano), y = `Rent (%)`)) +
  geom_violin(fill = "skyblue", color = "gray30", alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_segment(
    data = linhas_cdi,
    aes(x = x_num - 0.6, xend = x_num + 0.6, y = CDI, yend = CDI),
    color = "red", linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ `Tipo ANBIMA`, scales = "free_y", ncol = 2) +
  labs(
    title = "Distribuição da Rentabilidade por Tipo ANBIMA e Ano",
    x = "Ano",
    y = "Rentabilidade (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )
