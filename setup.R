library(tidycensus)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(ggrepel)

# the 
dd <- load_variables(2023, "acs5")
dd

dd %>%
  filter(
    name %in% c("B23025_003", "B23025_005", "B23025_007", "B19013_001")
  ) %>%
  select(-label)

# FIPS Code (standard for identifying geographic areas)
# NC: 37
# Guilford Count (37081)

guilford_acs <- get_acs(
  # a tract typically has b/w 1200 and 8000 people
  geography = "tract",
  variables = c(labor_force = "B23025_003",
                unemployed  = "B23025_005",
                not_in_lf   = "B23025_007",
                med_income  = "B19013_001"),
  state = "NC",
  county = "Guilford",
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

guilford_acs
class(guilford_acs)
glimpse(guilford_acs)

guilford_acs$geometry[[2]]


guilford_acs <- guilford_acs |> 
  mutate(unemployment_rate = unemployedE / labor_forceE)

summary(guilford_acs$unemployment_rate)

# question here.
guilford_acs |> 
  filter(is.na(unemployment_rate))

ggplot(guilford_acs) +
  geom_sf(aes(fill = unemployment_rate)) +
  scale_fill_viridis(name = "Unemployment\nRate", labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Unemployment Rate by census tract, guilford county, NC"
  )

# geocode the career centers
career_centers <- tibble::tribble(
  ~name, ~address,
  "Greensboro NC Works", "2301 W Meadowview Rd",
  "High Point NC Works", "607 Idol St, High Point, NC 27262"
) |> print()

career_centers_geo <- career_centers |> 
  geocode(address, method = "osm", lat = lat, long = long)

career_centers_sf <- career_centers_geo |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

guilford_centroids <- guilford_acs |> st_centroid()

guilford_acs
guilford_centroids

# precise centroid projection (project onto CRS NC State Plane - 2264)
guilford_acs_proj <- st_transform(guilford_acs, crs = 2264)
career_centers_proj <- st_transform(career_centers_sf, crs = 2264)
guilford_centroids_proj <- st_centroid(guilford_acs_proj)


dist_matrix <- st_distance(guilford_centroids_proj, career_centers_proj)
dist_matrix

guilford_acs_proj <- guilford_acs_proj |> 
  mutate(dist_to_nearest_center_miles = apply(dist_matrix, 1, min) / 5280)

guilford_acs_proj

model <- lm(unemployment_rate ~ dist_to_nearest_center_miles + med_incomeE, 
            data = guilford_acs_proj)

summary(model)

guilford_acs_proj %>% nrow
guilford_acs_proj


cor(guilford_acs_proj$dist_to_nearest_center_miles, guilford_acs_proj$med_incomeE, u )
with(
  guilford_acs_proj,
  cor(dist_to_nearest_center_miles, med_incomeE, use = "pairwise.complete.obs")
)

ggplot(
  guilford_acs_proj, 
  aes(x = dist_to_nearest_center_miles, y = unemployment_rate)
  ) +
  geom_point() +
  stat_smooth(method = "lm")


# reference cities --------------------------------------------------------

reference_cities <- tibble::tibble(
  name = c("Greensboro", "High Point", "Summerfield", "Stokesdale",
           "Jamestown", "Oak Ridge", "Sedalia", "Pleasant Garden", "Whitsett"),
  address = c("Greensboro, NC", "High Point, NC", "Summerfield, NC", "Stokesdale, NC",
              "Jamestown, NC", "Oak Ridge, NC", "Sedalia, NC", "Pleasant Garden, NC", "Whitsett, NC")
)

reference_cities_geo <- reference_cities %>%
  geocode(address, method = "osm", lat = lat, long = long)

reference_cities_geo

# add career centers to choropleth map
career_centers_coords <- career_centers_sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
    ) %>%
  st_drop_geometry() %>%
  print

ggplot(guilford_acs) +
  geom_sf(aes(fill = unemployment_rate)) +
  scale_fill_viridis(name = "Unemployment\nRate", labels = scales::percent) +
  geom_point(data = career_centers_coords, aes(x = lon, y = lat),
             color = "red", size = 3, shape = 17) +
  theme_minimal() +
  geom_label_repel(data = career_centers_coords,
                   aes(x = lon, y = lat, label = name),
                   size = 3, fontface = "bold",
                   box.padding = 0.5, segment.color = "red") +
  geom_text(data = reference_cities_geo, 
                  aes(x = long, y = lat, label = name),
                  size = 2.8, color = "gray90", fontface = "italic",
                  box.padding = 0.4,
                  max.overlaps = 15,        # allow more attempts before giving up on a label
                  min.segment.length = 0,   # always draw a leader line, even for short distances
                  seed = 42) +      
  labs(
    title = "Unemployment Rate by census tract, guilford county, NC",
    subtitle = "NCWorks Career Center locations shown in red"
  ) 
  
ggplot(guilford_acs) +
  geom_sf(aes(fill = unemployedE)) +
  scale_fill_viridis(name = "Unemployed\nPopulation") +
  geom_point(data = career_centers_coords, aes(x = lon, y = lat),
             color = "red", size = 3, shape = 17) +
  theme_minimal() +
  geom_label_repel(data = career_centers_coords,
                   aes(x = lon, y = lat, label = name),
                   size = 3, fontface = "bold",
                   box.padding = 0.5, segment.color = "red") +
  geom_text(data = reference_cities_geo, 
            aes(x = long, y = lat, label = name),
            size = 2.8, color = "gray90", fontface = "italic",
            box.padding = 0.4,
            max.overlaps = 15,        # allow more attempts before giving up on a label
            min.segment.length = 0,   # always draw a leader line, even for short distances
            seed = 42) +      
  labs(
    title = "Unemployment Rate by census tract, guilford county, NC",
    subtitle = "NCWorks Career Center locations shown in red"
  ) 

