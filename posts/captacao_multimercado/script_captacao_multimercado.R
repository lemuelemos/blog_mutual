###############################################################################
# PARÂMETROS DO PIPELINE
# Defina estas variáveis antes de source() para sobrescrever os defaults.
###############################################################################

periodo_inicial <- "2015-01"
periodo_final <- "2025-12"


###############################################################################
# ETAPA 1 - DOWNLOAD OPCIONAL DA CVM
# Baixa cadastro, extratos anuais e informes mensais quando solicitado.
###############################################################################

anos_download <- seq(2015, 2020, 1)
meses_download <- format(
  seq(as.Date("2021-01-01"), as.Date("2025-12-01"), by = "month"),
  "%Y%m"
)

# Definindo o número de workers
future::plan(future::multisession, workers = 10)

# Fazendo donwload dos arquivos de 2000 a 2020

furrr::future_walk(
  anos_download,
  function(ano) {
    options(timeout = 600)
    url <- paste0(
      "https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_",
      ano,
      ".zip"
    )
    destfile <- paste0(
      "./data-raw/informes/",
      ano,
      ".zip"
    )
    utils::download.file(url, destfile, mode = "wb")
  },
  .progress = T
)


# Fazendo donwload dos arquivos de 2021 a janeiro de 2025

furrr::future_walk(
  meses_download,
  function(mes) {
    options(timeout = 600)
    url <- paste0(
      "https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_",
      mes,
      ".zip"
    )
    destfile <- paste0(
      "./data-raw/informes/",
      mes,
      ".zip"
    )
    download.file(url, destfile)
  },
  .progress = T
)

# Dezipando arquivos

zipfiles <- list.files(
  "./data-raw/informes/",
  full.names = T,
  pattern = ".zip"
)

furrr::future_walk(zipfiles, function(path) {
  unzip(path, exdir = "./data-raw/informes/")
})

# Deletando arquivos baixados

invisible(file.remove(list.files(
  "./data-raw/informes/",
  full.names = T,
  pattern = ".zip"
)))

# Excluindo os workers da paralelização
future::plan(future::sequential)


# Lista todos os arquivos CSV da pasta "informe_diario"
arquivos_csv <- list.files(
  "./data-raw/informes/",
  full.names = TRUE,
  pattern = ".csv"
)

# Configura o plano de execução paralela com 6 núcleos (multisession é compatível com Windows)
future::plan(future::multisession, workers = 10)

# Lê todos os arquivos CSV em paralelo e armazena os resultados como uma lista de dataframes
furrr::future_map(
  arquivos_csv,
  function(arquivo) {
    readr::read_delim(
      arquivo,
      delim = ";",
      escape_double = FALSE,
      trim_ws = TRUE
    )
  },
  .progress = TRUE
) -> informes_diarios

# Restaura o plano de execução para modo sequencial após a paralelização
future::plan(future::sequential)

# Junta todos os dataframes da lista em um único dataframe consolidado
informes_diarios %>%
  dplyr::bind_rows() -> informes_diarios

# Deletando arquivos baixados

invisible(file.remove(list.files(
  "./data-raw/informes/",
  full.names = T,
  pattern = ".csv"
)))

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "./data-raw/dados_fundos.duckdb",
  read_only = FALSE
)

DBI::dbWriteTable(con, "informes_diarios", informes_diarios, overwrite = T)
rm(informes_diarios)

dados_anbima <- readxl::read_excel(
  "./data-raw/dados_anbima/FUNDOS-175-CARACTERISTICAS-PUBLICO.xlsx"
)

DBI::dbWriteTable(con, "dados_anbima", dados_anbima)
2356325236253
