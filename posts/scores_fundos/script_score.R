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
library(quantmod)

# Conectando a base de dados
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "fundos_db.duckdb",
  read_only = FALSE
)

tbl(con, "informes_diarios") |>
  dplyr::filter(TP_FUNDO == "FI") |>
  mutate(CNPJ_FUNDO = sql("regexp_replace(CNPJ_FUNDO, '[^0-9]', '', 'g')"))


tbl(con, "fundos_175_anbima") |>
  dplyr::filter(
    `Categoria ANBIMA` %in%
      c("Renda Fixa", "Ações", "Multimercados", "Previdência")
  ) |>
  dplyr::select(
    CNPJ_FUNDO = `CNPJ do Fundo`,
    Status,
    Inicio = `Data de Início de Atividade`,
    `Categoria_ANBIMA` = `Categoria ANBIMA`,
    `Tipo_ANBIMA` = `Tipo ANBIMA`,
    `Aberto Estatutariamente`,
    `Fundo ESG`,
    `Tributação Alvo`,
    `Tipo de Investidor`,
    `Característica do Investidor`,
    `Aplicação Inicial Mínima`
  ) -> dados_anbima


tbl(con, "fundos_175_anbima") |>
  dplyr::filter(
    `Categoria ANBIMA` %in%
      c("Renda Fixa", "Ações", "Multimercados", "Previdência")
  ) |>
  dplyr::count(Status) |>
  dplyr::mutate(
    Part = n / sum(n),
    Peso = sum(n) / (n * 2)
  ) -> pesos


tbl(con, "informes_diarios") |>
  dplyr::filter(TP_FUNDO == "FI") |>
  mutate(CNPJ_FUNDO = sql("regexp_replace(CNPJ_FUNDO, '[^0-9]', '', 'g')")) |>
  dplyr::semi_join(
    dados_anbima
  ) |>
  dplyr::left_join(
    dados_anbima
  ) |>
  dplyr::collect() -> dados_fundos

dados_anbima |> collect() -> dados_anbima


dados_fundos |>
  dplyr::filter(Status == "Encerrado") |>
  dplyr::mutate(
    VL_PATRIM_LIQ = VL_PATRIM_LIQ / 1000000,
    CAPTC_DIA = CAPTC_DIA / 1000000,
    RESG_DIA = RESG_DIA / 1000000,
    IDADE = (dplyr::last(DT_COMPTC) - as.Date(Inicio)) / lubridate::dyears(1)
  ) |>
  dplyr::filter(IDADE > 1) |>
  dplyr::reframe(
    Status = dplyr::last(Status),
    IDADE = dplyr::last(IDADE),
    VAR_PL30 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 30)
    ),
    VAR_PL60 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 60)
    ),
    VAR_PL180 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 180)
    ),
    VAR_PL360 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 360)
    ),
    RENT_30 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 30)) - 1
    ) *
      100,
    RENT_60 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 60)) - 1
    ) *
      100,
    RENT_180 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 180)) - 1
    ) *
      100,
    RENT_360 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 360)) - 1
    ) *
      100,
    CAPTC_30 = sum(tail(CAPTC_DIA, 30)),
    CAPTC_60 = sum(tail(CAPTC_DIA, 60)),
    CAPTC_180 = sum(tail(CAPTC_DIA, 180)),
    CAPTC_360 = sum(tail(CAPTC_DIA, 360)),
    RESGT_30 = sum(tail(RESG_DIA, 30)),
    RESGT_60 = sum(tail(RESG_DIA, 60)),
    RESGT_180 = sum(tail(RESG_DIA, 180)),
    RESGT_360 = sum(tail(RESG_DIA, 360)),
    .by = CNPJ_FUNDO
  ) -> dados_fundos_fechados

dados_fundos |>
  dplyr::filter(Status != "Encerrado") |>
  dplyr::mutate(
    VL_PATRIM_LIQ = VL_PATRIM_LIQ / 1000000,
    CAPTC_DIA = CAPTC_DIA / 1000000,
    RESG_DIA = RESG_DIA / 1000000,
    IDADE = (dplyr::last(DT_COMPTC) - as.Date(Inicio)) / lubridate::dyears(1)
  ) |>
  dplyr::filter(IDADE > 1) |>
  dplyr::reframe(
    Status = dplyr::last(Status),
    IDADE = dplyr::last(IDADE),
    VAR_PL30 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 30)
    ),
    VAR_PL60 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 60)
    ),
    VAR_PL180 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 180)
    ),
    VAR_PL360 = dplyr::last(
      dplyr::last(VL_PATRIM_LIQ) - dplyr::lag(VL_PATRIM_LIQ, 360)
    ),
    RENT_30 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 30)) - 1
    ) *
      100,
    RENT_60 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 60)) - 1
    ) *
      100,
    RENT_180 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 180)) - 1
    ) *
      100,
    RENT_360 = dplyr::last(
      (dplyr::last(VL_QUOTA) / dplyr::lag(VL_QUOTA, 360)) - 1
    ) *
      100,
    CAPTC_30 = sum(tail(CAPTC_DIA, 30)),
    CAPTC_60 = sum(tail(CAPTC_DIA, 60)),
    CAPTC_180 = sum(tail(CAPTC_DIA, 180)),
    CAPTC_360 = sum(tail(CAPTC_DIA, 360)),
    RESGT_30 = sum(tail(RESG_DIA, 30)),
    RESGT_60 = sum(tail(RESG_DIA, 60)),
    RESGT_180 = sum(tail(RESG_DIA, 180)),
    RESGT_360 = sum(tail(RESG_DIA, 360)),
    .by = CNPJ_FUNDO
  ) -> dados_fundos_abertos

bind_rows(dados_fundos_fechados, dados_fundos_abertos) |>
  left_join(
    dados_fundos |>
      group_by(CNPJ_FUNDO) |>
      select(CNPJ_FUNDO, `Tipo de Investidor`, `Tributação Alvo`) |>
      summarise_all(last) |>
      mutate(
        `Tipo de Investidor` = as.factor(`Tipo de Investidor`),
        `Tributação Alvo` = as.factor(`Tributação Alvo`)
      )
  ) |>
  dplyr::select(-CNPJ_FUNDO) |>
  mutate(Status = as.factor(Status)) -> dados_modelo

dados_modelo %>%
  na.omit() %>%
  filter(
    RENT_30 != Inf,
    RENT_60 != Inf,
    RENT_180 != Inf,
    RENT_360 != Inf
  ) |>
  mutate(
    peso = case_when(
      Status == "Encerrado" ~ 3.22,
      Status == "Ativo" ~ 0.592,
      TRUE ~ NA_real_ # segurança para casos inesperados
    )
  ) -> dados_modelo

glm(
  Status ~
    IDADE +
      VAR_PL30 +
      VAR_PL60 +
      VAR_PL180 +
      VAR_PL360 +
      RENT_30 +
      RENT_60 +
      RENT_180 +
      RENT_360 +
      CAPTC_30 +
      CAPTC_60 +
      CAPTC_180 +
      CAPTC_360 +
      RESGT_30 +
      RESGT_60 +
      RESGT_180 +
      RESGT_360,
  data = dados_modelo,
  weights = peso,
  family = binomial(link = "logit")
) -> fit

mfx <- margins(fit)
summary(mfx)

margins::marginal_effects(fit)
