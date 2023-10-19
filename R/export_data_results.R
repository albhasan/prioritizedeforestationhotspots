# Export data and results.

# library(dplyr)
# library(ggplot2)
# library(purrr)
# library(sf)
# library(tidyr)
# library(tidyselect)
# library(tools)

export_data <- function(out_dir) {

    #out_dir = "/home/alber/Documents/prioritize_export"

    #---- Export results to SHP ----

    # Read the result data from the package.
    priority_sf <-
        system.file("extdata", "results", "priority_classes.shp",
                    package = "prioritizedeforestationhotspots") %>%
        read_sf() %>%
        mutate(id = as.integer(id))

    # Format the data.
    priority_tb <-
        priority_sf %>%
        st_drop_geometry() %>%
        pivot_longer(cols = starts_with("pri"),
                     names_prefix = "pri",
                     names_to = "ref_year",
                     values_to = "priority")

    # Arrange data into a sf object.
    priority_sf <-
        priority_sf %>%
        select(id) %>%
        right_join(priority_tb,
                   by = "id",
                   multiple = "all") %>%
        mutate(priority = factor(priority,
                                 labels = c("High", "Average", "Low"),
                                 ordered = TRUE))

    # Export results as SHPs, one per year.
    save_shp <- function(sf_obj, filename) {
        write_sf(sf_obj, filename)
        return(TRUE)
    }

    priority_sf %>%
        group_by(ref_year) %>%
        group_nest(.key = "sf_obj") %>%
        mutate(filename = file.path(out_dir,
                                    paste0("priority_", ref_year, ".shp"))) %>%
        dplyr::mutate(saved = purrr::map2_lgl(sf_obj, filename, save_shp))



    #---- Export model variables ----

    def_sf <-
        deforestation_grid %>%
        right_join(deforestation_data, by = "id") %>%
        mutate(dist_1_percent_ly = dist_1_percent_ly + 1,
               dist_2_percent_ly = dist_2_percent_ly + 1)

    variables <- tribble(
        ~col_name, ~units, ~description,
        "area_PA", "Area (km2)", "Protected areas or indigenour lands",
        "dist_hidro", "Distance (km2)", "Distance to the closest waterway",
        "dist_road", "Distance (km)", "Distance to the closest highway",
        "dist_road_hidro", "Distance (km)", "Distance to the closest highway or waterway",
        "def", "Deforestation (km2)", "Deforestation",
        "def_1_ly", "Deforestation (km2)", "Deforestation in the year before",
        "def_2_ly", "Deforestation (km2)", "Deforestation 2 years before",
        "def_4_ly", "Deforestation (km2)", "Deforestation 4 years before",
        "dist_1_percent_ly", "Distance (km)", "Distance to the closest grid centroid with more than 1% deforestation in year before",
        "dist_2_percent_ly", "Distance (km)", "Distance to the closest grid centroid with more than 2% deforestation in year before",
        "active_fires_ly", "Number of fires", "Number of active fires in the year before"
    )

    stopifnot("Missing variables" = 
              length(variables$col_name[!(variables$col_name %in% 
              colnames(def_sf))]) == 0)

    ref_years <- unique(def_sf$ref_year)
    
    for (i in seq(nrow(variables))) {
        var_name <- variables$col_name[i]
        new_sf <- def_sf[c("ref_year", var_name)]
        for (y in ref_years) {
            sf::write_sf(new_sf[new_sf$ref_year == y, ], 
                         file.path(out_dir, paste0(var_name, "_", y, ".shp")))
            conn <- file(file.path(out_dir, paste0(var_name, "_", y, ".txt")))
            writeLines(paste0(variables$description[i], " - ", 
                              variables$units[i], " - Reference year: ",
                              y), 
                       conn)
            close(conn)
        } 
    }
}

