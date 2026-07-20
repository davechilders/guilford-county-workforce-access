library(sfarrow)
library(ggplot2)
library(ggrepel)
library(arrow)
library(viridis)

guilford_acs <- st_read_parquet("data/guilford_tracts-2026-07-20.parquet")
centers <- st_read_parquet("data/career-centers-geo-2026-07-20.parquet")
ref_cities <- read_parquet("data/ref-cities-2026-07-20.parquet")

guilford_centroids <- guilford_acs |> st_centroid()

# precise centroid projection (project onto CRS NC State Plane - 2264)
guilford_acs_proj <- st_transform(guilford_acs, crs = 2264)
career_centers_proj <- st_transform(centers, crs = 2264)
guilford_centroids_proj <- st_centroid(guilford_acs_proj)


dist_matrix <- st_distance(guilford_centroids_proj, career_centers_proj)

guilford_acs_proj <- guilford_acs_proj |> 
  mutate(dist_to_nearest_center_miles = apply(dist_matrix, 1, min) / 5280)


ggplot(guilford_acs) +
  geom_sf(aes(fill = unemployment_rate)) +
  scale_fill_viridis(name = "Unemployment\nRate", labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Unemployment Rate by census tract, guilford county, NC"
  )

dist_matrix <- st_distance(guilford_centroids_proj, career_centers_proj)

guilford_acs_proj <- guilford_acs_proj |> 
  mutate(dist_to_nearest_center_miles = apply(dist_matrix, 1, min) / 5280)

# add career centers to choropleth map
career_centers_coords <- centers %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  print

# Join the distance column back onto the unprojected version for plotting
guilford_acs <- guilford_acs |>
  left_join(
    st_drop_geometry(guilford_acs_proj) |> select(GEOID, dist_to_nearest_center_miles),
    by = "GEOID"
  )

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
  geom_text_repel(data = ref_cities, 
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
