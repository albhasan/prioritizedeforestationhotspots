## code to prepare `DATASET` dataset goes here

library(dplyr)
library(janitor)
library(purrr)
library(readxl)
library(sf)
library(tidyr)
library(usethis)
library(ensurer)

data_file <- "./data-raw/Random_Forest_v4.xlsx"
grid_file <- "./data-raw/Grade_Random_Forest.shp"
area_file <- "./data-raw/Area_Grade.xlsx"
biomass_dir <- "~/Documents/data/esacci/biomass/data/agb/maps/v3.0/geotiff"

stopifnot(all(file.exists(data_file, grid_file, area_file)))
stopifnot(dir.exists(biomass_dir))

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
   ),

   "2023" = c(
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
    sheet_tb <-
        data_file %>%
        readxl::read_excel(sheet = my_sheet) %>%
        ensurer::ensure_that(
            all(col_names_ls[[my_sheet]] %in% colnames(.)),
            err_desc = sprintf("Unknown or missing columns: %s", my_sheet)
        ) %>%
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
deforestation_tb <-
    constant_tb %>%
    dplyr::select(id,
                  `2019` = def_2019,
                  `2020` = def_2020) %>%
    tidyr::pivot_longer(c(`2019`, `2020`),
                       names_to = "ref_year",
                       values_to = "def")
constant_tb <- constant_tb %>%
    dplyr::select(-def_2019, -def_2020)


# Build data set.
deforestation_data <-
    variable_ls %>%
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


# Read and prepare the area data.
area_tb <-
    area_file %>%
    readxl::read_excel(sheet = "Planilha1") %>%
    dplyr::rename(area_km2 = "Área Grade (km2)")

# Read and prepare the grid.
deforestation_grid <-
    grid_file %>%
    sf::st_read() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::select(id = Id) %>%
    dplyr::left_join(area_tb, by = "id")

stopifnot("IDs aren't unique in deforestation_grid" = 
          length(unique(deforestation_grid$id)) == nrow(deforestation_grid))

# Read biomass data.
biomass_df <-
    biomass_dir %>%
    list.files(pattern = "*.tiff$", 
              full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(
        file_name = tools::file_path_sans_ext(basename(file_path))
    ) %>%
    tidyr::separate(col = file_name, 
                    sep = "-",
                    into = c("location", "project", "level", "unit", "code", 
                             "res", "epoch", "version")) %>%
    dplyr::filter(project == "BIOMASS",
                  level == "L4",
                  code == "MERGED",
                  res == "100m", 
                  epoch == "2018",
                  version == "fv3.0")

# Mosaic biomass data.
agb_r <- 
    biomass_df %>%
    dplyr::filter(unit == "AGB") %>% 
    dplyr::pull(file_path) %>%
    gdalUtilities::gdalbuildvrt(output.vrt = tempfile(pattern = "agb_", 
                                                      fileext = ".vrt")) %>%
    terra::rast()

# TODO: Filter biomass raster using PRODES forest.
# forest_sf <- "~/Documents/data/prodes/amazonia/forest_biome_2021.shp" %>%
#     sf::read_sf() %>%
#     sf::st_transform(crs = 4326)
#
# agb_masked_r <- 
#     agb_r %>%
#     terra::mask(mask = forest_sf)

agb_zonal <-
    agb_r %>%
    terra::zonal(z = terra::vect(deforestation_grid["id"]), 
                 fun = "mean")

colnames(agb_zonal) <- "agb_2018"

stopifnot("Invalid number of rows" = 
          nrow(agb_zonal) == nrow(deforestation_grid))
deforestation_grid <- cbind(deforestation_grid, agb_zonal)

# Save data.
usethis::use_data(deforestation_data, deforestation_grid, overwrite = TRUE)

