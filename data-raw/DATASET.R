## code to prepare `DATASET` dataset goes here

library(dplyr)
library(janitor)
library(purrr)
library(readxl)
library(sf)
library(usethis)

data_file <- "./data-raw/Random_Forest.xlsx"
grid_file <- "./data-raw/Grade_Random_Forest.shp"
stopifnot(all(file.exists(data_file, grid_file)))

# Column names in data_file.
col_names_ls <- list(
    "2019" = c(
        id                  = "ID",
        def_1_ly            = "Desmatamento Ano Anterior (km2)",
        def_2_ly            = "Desmatamento Acumulado 2 Anos Anteriores (km2)",
        def_4_ly            = "Desmatamento Acumulado 4 Anos Anteriores (km2)",
        dist_road           = "Distância Rodovia (km)",
        dist_hidro          = "Distância Hidrovia (km)",
        dist_road_hidro     = "Distância Rodovia + Hidrovia (km)",
        dist_1_percent_ly   = "Distância a ponto de grade > 1% desmatamentono ano anterior (km)",
        dist_2_percent_ly   = "Distância a ponto de grade > 2% desmatamento acumulado em 2 anos (km)",
        active_fires_ly     = "Focos de Calor Ano Anterior",
        area_PA             = "Area_PA",
        def_2019            = "Desmatamento 2019 (km2)"
    ),

    "2020" = c(
        id                  = "ID",
        def_1_ly            = "Desmatamento Ano Anterior (km2)",
        def_2_ly            = "Desmatamento Acumulado 2 Anos Anteriores (km2)",
        def_4_ly            = "Desmatamento Acumulado 4 Anos Anteriores (km2)",
        dist_1_percent_ly   = "Distância a ponto de grade > 1% desmatamentono ano anterior (km)",
        dist_2_percent_ly   = "Distância a ponto de grade > 2% desmatamento acumulado em 2 anos (km)",
        active_fires_ly     = "Focos de Calor Ano Anterior",
        def_2020            = "Desmatamento 2020 (km2)"
    ),

    "2021" = c(
        id                  = "ID",
        def_1_ly            = "Desmatamento Ano Anterior (km2)",
        def_2_ly            = "Desmatamento Acumulado 2 Anos Anteriores (km2)",
        def_4_ly            = "Desmatamento Acumulado 4 Anos Anteriores (km2)",
        dist_1_percent_ly   = "Distância a ponto de grade > 1% desmatamentono ano anterior (km)",
        dist_2_percent_ly   = "Distância a ponto de grade > 2% desmatamento acumulado em 2 anos (km)",
        active_fires_ly        = "Focos de Calor Ano Anterior"
    ),

   "2022" = c(
       id                   = "ID",
       def_1_ly             = "Desmatamento Ano Anterior (km2)",
       def_2_ly             = "Desmatamento Acumulado 2 Anos Anteriores (km2)",
       def_4_ly             = "Desmatamento Acumulado 4 Anos Anteriores (km2)",
       dist_1_percent_ly    = "Distância a ponto de grade > 1% desmatamentono ano anterior (km)",
       dist_2_percent_ly    = "Distância a ponto de grade > 2% desmatamento acumulado em 2 anos (km)",
       active_fires_ly      = "Focos de Calor Ano Anterior"
   )
)


# Get the names of the columns that appear once in the spreadsheet's sheets.
constant_vars <- col_names_ls %>%
    purrr::map(names) %>%
    unlist(use.names = FALSE) %>%
    table() %>%
    (function(x) {
         names(x[x == 1])
    })


# Read the the data sheets in data_file.
variable_ls <- list()
constant_ls <- list()
for (my_sheet in readxl::excel_sheets(data_file)) {
    sheet_tb <- data_file %>%
        readxl::read_excel(sheet = my_sheet) %>%
        dplyr::rename(col_names_ls[[my_sheet]]) %>%
        dplyr::mutate(ref_year = my_sheet) %>%
        ensurer::ensure_that(length(unique(.$id)) == nrow(.),
                             err_desc = sprintf("Duplicated ids in data %s!",
                                                my_sheet))

    # Separate the constant from the dinamic variables.
    constant_ls[[my_sheet]] <- sheet_tb %>%
        dplyr::select(id, tidyselect::any_of(c("id", constant_vars)))
    variable_ls[[my_sheet]] <- sheet_tb %>%
        dplyr::select(!tidyselect::any_of(constant_vars))
}


# Merge data.
constant_tb <- tibble::tibble()
for (i in seq_along(constant_ls)) {
    if (i == 1) {
        constant_tb <- constant_ls[[i]]
        next()
    }
    if (ncol(constant_ls[[i]]) == 1)
        next()
    constant_tb <- constant_tb %>%
        dplyr::full_join(constant_ls[[i]], by = "id")
}
deforestation_tb <- constant_tb %>%
    dplyr::select(id,
                  `2019` = def_2019,
                  `2020` = def_2020) %>%
    tidyr::pivot_longer(c(`2019`, `2020`),
                       names_to = "ref_year",
                       values_to = "def")
constant_tb <- constant_tb %>%
    dplyr::select(-def_2019, -def_2020)


# Build data set.
deforestation_grid <- variable_ls %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(constant_tb, by = "id") %>%
    dplyr::left_join(deforestation_tb, by = c("id", "ref_year")) %>%
    dplyr::select(
        id,
        area_PA,
        dist_hidro,
        dist_road,
        dist_road_hidro,
        ref_year,
        def,
        def_1_ly,
        def_2_ly,
        def_4_ly,
        dist_1_percent_ly,
        dist_2_percent_ly,
        active_fires_ly
    )


# Save data.
usethis::use_data(deforestation_grid, overwrite = TRUE)

