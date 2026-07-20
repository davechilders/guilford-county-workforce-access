library(tidycensus)
library(sf)
library(sfarrow)
library(arrow)
library(tidyverse)
library(tidygeocoder)



# Explore ACS Variables ---------------------------------------------------

dd <- load_variables(2023, "acs5")

dd %>%
  filter(
    name %in% c(
      "B23025_003", # labor force
      "B23025_005", # unemployment rate
      "B23025_007", # not in labor force
      "B19013_001", # median income
      "B17001_002", # population below poverty level
      "B17001_001", # population?
      "B01003_001", # total population
      "B08201", # no-vehicle households,
      "B17001" # poverty rate
      ) 
  ) %>%
  select(-label)

# FIPS Code (standard for identifying geographic areas)
# NC: 37
# Guilford Count (37081)


# Load ACS Fields for Guilford Tracts -------------------------------------

guilford_acs <- get_acs(
  # a tract typically has b/w 1200 and 8000 people
  geography = "tract",
  variables = c(labor_force   = "B23025_003",
                unemployed    = "B23025_005",
                not_in_lf     = "B23025_007",
                med_income    = "B19013_001",
                poverty_total = "B17001_001",
                poverty_below = "B17001_002",
                hh_total      = "B08201_001",
                hh_no_vehicle = "B08201_002"),
  state = "NC",
  county = "Guilford",
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)


# calculate unemployment rates --------------------------------------------

guilford_acs <- guilford_acs |> 
  mutate(
    unemployment_rate = unemployedE / labor_forceE,
    poverty_rate    = poverty_belowE / poverty_totalE,
    no_vehicle_rate = hh_no_vehicleE / hh_totalE
    )

st_write_parquet(guilford_acs, glue::glue("data/guilford_tracts-{Sys.Date()}.parquet"))


# Career Centers ----------------------------------------------------------

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

st_write_parquet(career_centers_sf, glue::glue("data/career-centers-geo-{Sys.Date()}.parquet"))


# reference cities --------------------------------------------------------

reference_cities <- tibble::tibble(
  name = c("Greensboro", "High Point", "Summerfield", "Stokesdale",
           "Jamestown", "Oak Ridge", "Sedalia", "Pleasant Garden", "Whitsett"),
  address = c("Greensboro, NC", "High Point, NC", "Summerfield, NC", "Stokesdale, NC",
              "Jamestown, NC", "Oak Ridge, NC", "Sedalia, NC", "Pleasant Garden, NC", "Whitsett, NC")
)

reference_cities_geo <- reference_cities %>%
  geocode(address, method = "osm", lat = lat, long = long)

write_parquet(reference_cities_geo, glue::glue("data/ref-cities-{Sys.Date()}.parquet"))
