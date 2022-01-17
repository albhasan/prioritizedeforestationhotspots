#' @title Add coordinates to an `sf` object.
#' @name add_coords
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function adds coordinate columns to a sf object.
#'
#' @param point_sf    A sf object.
#' @param coord_names A character. The names of the new columns.
#' @return            A sf object.
.add_coords <- function(point_sf, coord_names = c("longitude", "latitude")){
    xy <- point_sf %>%
        sf::st_coordinates() %>%
        magrittr::set_colnames(coord_names) %>%
        tidyr::as_tibble()
    point_sf %>%
        dplyr::bind_cols(xy) %>%
        return()
}



#' @title Check if vector has a value.
#' @name has_any_value
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#'
#' @description This checks if a vector has an element different from NA. It is
#' meant to be used with `dplyr::select_if` to select columns with data.
#'
#' @param x  A vector.
#' @return   A logical.
.has_any_value <- function(x) {
    return(any(!is.na(x)))
}
