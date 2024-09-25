# location metadata, 2022-23
# location_meta_23 <-read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv")
# readr::write_csv(location_meta_23, "tests/testthat/fixtures/location_meta_23.csv")

# location metadata, 2023-24
location_meta_24 <- readr::read_csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/auxiliary-data/locations.csv")
readr::write_csv(location_meta_24, "tests/testthat/fixtures/location_meta_24.csv")
