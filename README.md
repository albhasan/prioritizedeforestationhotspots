
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prioritizedeforestationhotspots

<!-- badges: start -->
<!-- badges: end -->

This package introduces a method for prioritizing deforestation hotspots
to support law enforcement actions in the Brazilian Amazon, as describen
in the paper entitled: *Science-based planning can support law
enforcement actions to curb deforestation in the Brazilian Amazon* by
Mataveli et at, available at <https://doi.org/10.1111/conl.12908>

This is the abstract of the aforementioned paper:

While Brazil publicly committed to reduce deforestation in Amazonia at
the 26th Conference of the Parties (COP26), the Brazilian parliament is
moving toward weakening environmental laws. Deforestation rates continue
ascending, reaching in 2021 the highest value since 2006 (13,235 km2).
To overcome this paradox, strategies to curb deforestation are
mandatory. The current strategy, “Plano Amazônia 21/22,” prioritizes law
enforcement actions to curb illegal deforestation in only 11 Amazonian
municipalities. Here, we show that this prioritization is likely to be
insufficient since these municipalities account for just 37% of the
current deforestation rate. This strategy may also be undermined by the
leakage of deforestation actions to unmonitored municipalities. Using a
set of spatially explicit datasets integrated into a
deforestation-prediction modeling approach, we propose a science-based
alternative method for ranking deforestation hotspots to be prioritized
by law enforcement actions. Our prioritization method accounts for more
than 60% of the deforestation, detecting larger deforested areas in both
private and public lands, while covering 27% less territory than “Plano
Amazônia 21/22.” Optimizing the detection of priority areas for curbing
deforestation, as proposed here, is the first step to reducing
deforestation rates and comply with the Brazilian legal commitment of
3925 km2 year−1.

## Installation

You can install the development version of
prioritizedeforestationhotspots from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("albhasan/prioritizedeforestationhotspots")
```

## Example

This basic example produces results similar to those shown in the
aforementioned paper:

``` r

library(prioritizedeforestationhotspots)

## Run the model
## NOTE: This can take long!
# out_dir <- "~/Documents/prioritize_res"
# estimate_accuracy(out_dir)
# fit_model(out_dir)
```

This example shows some of the data included in this package.

``` r

library(prioritizedeforestationhotspots)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(sf)
#> Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE
library(ggplot2)

def_sf <- 
    deforestation_grid %>%
    right_join(deforestation_data, by = "id")

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = area_PA), lwd = 0) +
    scale_fill_gradient(name = "Area (km2)", 
                        trans = "log",
                        breaks = c(1, 10, 100, 600),
                        low = "green",
                        high = "red") +
    ggtitle("Protected areas or indigenous lands in the grid")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-1.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_hidro), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)", 
                        trans = "log",
                        breaks = c(1, 10, 50, 150),
                        low = "green",
                        high = "red") +
    ggtitle("Distance to the closest waterway")
```

<img src="man/figures/README-maps-2.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_road), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)", 
                        trans = "log",
                        breaks = c(1, 5, 50),
                        low = "green",
                        high = "red") +
    ggtitle("Distance to the closest highway")
```

<img src="man/figures/README-maps-3.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_road_hidro), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(1, 5, 50),
                        low = "green",
                        high = "red") +
    ggtitle("Distance to the closest highway or waterway")
```

<img src="man/figures/README-maps-4.png" width="100%" />

``` r

def_sf %>%
    drop_na(def) %>%
    ggplot() +
    geom_sf(aes(fill = def), lwd = 0) +
    scale_fill_gradient(name = "Deforestation (km2)",
                        trans = "log",
                        breaks = c(0.1, 1, 10, 100),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle("Deforestation")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-5.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = def_1_ly), lwd = 0) +
    scale_fill_gradient(name = "Deforestation (km2)",
                        trans = "log",
                        breaks = c(0.01, 0.10, 1, 10, 100),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle("Deforestation in the year before")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-6.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = def_2_ly), lwd = 0) +
    scale_fill_gradient(name = "Deforestation (km2)",
                        trans = "log",
                        breaks = c(0.01, 0.10, 1, 10, 100),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle("Deforestation 2 years before")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-7.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = def_4_ly), lwd = 0) +
    scale_fill_gradient(name = "Deforestation (km2)",
                        trans = "log",
                        breaks = c(0.01, 0.10, 1, 10, 100),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle("Deforestation 4 years before")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-8.png" width="100%" />

``` r

def_sf %>%
    mutate(dist_1_percent_ly = dist_1_percent_ly + 1) %>%
    ggplot() +
    geom_sf(aes(fill = dist_1_percent_ly), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(25, 50, 100, 250, 500, 900),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle(paste("Distance to the closest grid centroid with more than 1%",
                  "deforestation in year before"))
```

<img src="man/figures/README-maps-9.png" width="100%" />

``` r

def_sf %>%
    mutate(dist_2_percent_ly = dist_2_percent_ly + 1) %>%
    ggplot() +
    geom_sf(aes(fill = dist_2_percent_ly), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(25, 50, 100, 250, 500, 1000),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle(paste("Distance to the closest grid centroid with more than 2%",
                  "deforestation in year before"))
```

<img src="man/figures/README-maps-10.png" width="100%" />

``` r

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = active_fires_ly), lwd = 0) +
    scale_fill_gradient(name = "Number of fires",
                        trans = "log",
                        breaks = c(1, 5, 50, 500),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    ggtitle("Number of active fires in the year before")
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-maps-11.png" width="100%" />
