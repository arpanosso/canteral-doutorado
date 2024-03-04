dados <- readxl::read_xlsx("data-raw/anomalias_classes.xlsx",
                           na = "na")
readr::write_rds(dados, "data/anomalias.rds")
glimpse(dados)
