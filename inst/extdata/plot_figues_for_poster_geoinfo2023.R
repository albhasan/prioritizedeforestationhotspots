
priority_sf %>%
    ggplot() +
    geom_sf(aes(fill = priority), lwd = 0) +
    facet_wrap(~ref_year) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.title=element_blank())
ggsave(file = "/home/alber/Desktop/plot_results_precomputed.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)




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
    theme(axis.text.x = element_text(angle = 90))
ggsave(file = "/home/alber/Desktop/plot_pail.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_hidro), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(1, 10, 50, 150),
                        low = "green",
                        high = "red") +
    theme(axis.text.x = element_text(angle = 90))
ggsave(file = "/home/alber/Desktop/plot_waterway.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_road), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(1, 5, 50),
                        low = "green",
                        high = "red")
ggsave(file = "/home/alber/Desktop/plot_highway.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = dist_road_hidro), lwd = 0) +
    scale_fill_gradient(name = "Distance (km)",
                        trans = "log",
                        breaks = c(1, 5, 50),
                        low = "green",
                        high = "red") +
    theme(axis.text.x = element_text(angle = 90))
ggsave(file = "/home/alber/Desktop/plot_highway_waterway.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)


def_sf %>%
    drop_na(def) %>%
    ggplot() +
    geom_sf(aes(fill = def), lwd = 0) +
    scale_fill_gradient(name = "Deforestation (km2)",
                        trans = "log",
                        breaks = c(0.1, 1, 10, 100),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))
ggsave(file = "/home/alber/Desktop/plot_deforestation.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)

def_sf %>%
    ggplot() +
    geom_sf(aes(fill = active_fires_ly), lwd = 0) +
    scale_fill_gradient(name = "Number of fires",
                        trans = "log",
                        breaks = c(1, 5, 50, 500),
                        low = "green",
                        high = "red") +
    facet_wrap(~ref_year) +
    theme(axis.text.x = element_text(angle = 90))
ggsave(file = "/home/alber/Desktop/plot_fires.png",
dpi = 600,
width=148,
height=105,
units = "mm"
)

