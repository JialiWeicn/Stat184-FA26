library(ggplot2); library(dplyr); library(tidyr); library(readr); library(sf); library(stringr)

data_dir <- "."
lib_file <- list.files(data_dir, pattern = "^Libraries.*\\.csv$")[1]
ca_file  <- list.files(data_dir, pattern = "^Boundaries.*\\.csv$")[1]

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

# Problem 1
# Part A
pA <- ggplot(vehicles, aes(displ, hwy)) +
  geom_point(aes(color = drv), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~drv) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)") +
  theme_bw()
pA
# Three lines because facet_wrap splits the data into 3 panels and
# geom_smooth is fit separately in each one. It doesn't need color for that.

# Part B
pB1 <- ggplot(vehicles, aes(drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count", fill = "factor(cyl)") + theme_bw()

pB2 <- ggplot(vehicles, aes(drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)") + theme_bw()
pB1; pB2
# In the right plot every bar is rescaled to 1, so it shows the cylinder mix
# inside each drive type, not how many cars there are. The left plot shows
# 4 and f have about 100 cars each, while r has only about 25.

# Part C
gems_small <- gems %>% filter(carat <= 3)
cat("kept:", nrow(gems_small), " excluded:", nrow(gems) - nrow(gems_small), "\n")

p_fine <- ggplot(gems_small, aes(carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Bin width = 0.01", x = "Carat", y = "Count") + theme_bw()

p_coarse <- ggplot(gems_small, aes(carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  labs(title = "Bin width = 0.5", x = "Carat", y = "Count") + theme_bw()
p_fine; p_coarse

c(`0.99` = sum(round(gems$carat, 2) == 0.99),
  `1.00` = sum(round(gems$carat, 2) == 1.00))
# With narrow bins you see spikes at round sizes (0.3, 0.5, 1.0...). Almost no
# 0.99 stones but a huge pile at 1.00, so sizes seem rounded up. Wide bins hide this.

# Problem 2
data_sources <- tibble(
  file = c(lib_file, ca_file),
  publisher = "City of Chicago Data Portal",
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  url = "https://data.cityofchicago.org",
  date_accessed = "2026-11-08",
  license = "check the portal page for each dataset")
write_csv(data_sources, "data_sources.csv")

raw <- read_csv(file.path(data_dir, lib_file), show_col_types = FALSE)
n_raw <- nrow(raw)

raw$lat <- as.numeric(str_match(raw$LOCATION, "\\(([-0-9.]+),")[, 2])
raw$lon <- as.numeric(str_match(raw$LOCATION, ", ([-0-9.]+)\\)")[, 2])

bad <- is.na(raw$lat) | is.na(raw$lon) |
  raw$lat < 41 | raw$lat > 43 | raw$lon < -89 | raw$lon > -87
cat("raw records:", n_raw, "\nbad coordinates:", sum(bad), "\n")

clean <- raw[!bad, ] %>%
  mutate(site_id = sprintf("LIB%03d", row_number()),
         site_name = BRANCH)
cat("after cleaning:", nrow(clean), "\n")
cat("duplicate names:", sum(duplicated(clean$site_name)),
    " shared coordinates:", sum(duplicated(clean[, c("lat", "lon")])), "\n")

# NAD83 / UTM zone 16N (EPSG:26916), units = meters
sites_m <- st_as_sf(clean, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  select(site_id, site_name) %>%
  st_transform(26916)
st_crs(sites_m)$units

areas_m <- read_csv(file.path(data_dir, ca_file), show_col_types = FALSE) %>%
  st_as_sf(wkt = "the_geom", crs = 4326) %>%
  transmute(district_id = as.character(AREA_NUMBE), district_name = COMMUNITY) %>%
  st_transform(26916)

b500  <- st_union(st_buffer(sites_m, 500))
b1000 <- st_union(st_buffer(sites_m, 1000))

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = as.numeric(c(st_area(b500), st_area(b1000))) / 1e6)
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])
buffer_summary

cap <- "Source: City of Chicago Data Portal (libraries, community areas), accessed Nov 8 2026"

p_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey60") +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.5) +
  scale_color_manual(NULL, values = c(Library = "red")) +
  labs(title = "Chicago public libraries (all retained sites)", caption = cap) +
  theme_bw()

p_buffers <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey60") +
  geom_sf(data = st_sf(geometry = b500), aes(fill = "500 m buffer"), alpha = 0.5, color = NA) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.2) +
  scale_fill_manual(NULL, values = c("500 m buffer" = "steelblue")) +
  scale_color_manual(NULL, values = c(Library = "red")) +
  labs(title = "Libraries and combined 500 m buffer", caption = cap) +
  theme_bw()
p_sites; p_buffers
# Libraries are spread fairly evenly. 1,000 m covers more than 500 m but less
# than 4x as much, since overlapping circles are counted once. Buffers near the
# lake/city edge go outside the boundary; I kept that part.

# Problem 3
cat("areas:", nrow(areas_m), " dup ids:", sum(duplicated(areas_m$district_id)),
    " empty:", sum(st_is_empty(areas_m)), " invalid:", sum(!st_is_valid(areas_m)), "\n")
areas_m <- st_make_valid(areas_m)

hits <- st_intersects(sites_m, areas_m)
site_match_audit <- tibble(site_id = sites_m$site_id, n_area_matches = lengths(hits))
write_csv(site_match_audit, "site_match_audit.csv")
table(site_match_audit$n_area_matches)
filter(site_match_audit, n_area_matches != 1)

ok_ids <- site_match_audit$site_id[site_match_audit$n_area_matches == 1]
n_unassigned <- nrow(sites_m) - length(ok_ids)

cnt <- sites_m %>%
  filter(site_id %in% ok_ids) %>%
  st_join(areas_m) %>%
  st_drop_geometry() %>%
  count(district_id, name = "n_sites")

district_counts <- areas_m %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  left_join(cnt, by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2) %>%
  st_drop_geometry() %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

stopifnot(all(district_counts$area_km2 > 0),
          sum(district_counts$n_sites) == length(ok_ids))
print(district_counts, n = Inf)

map_dat <- areas_m %>% left_join(district_counts, by = c("district_id", "district_name"))
bb <- st_bbox(areas_m)
cap3 <- paste0("Source: City of Chicago Data Portal, accessed Nov 8 2026. Sites left unassigned: ", n_unassigned)

p_count <- ggplot(map_dat) +
  geom_sf(aes(fill = n_sites), color = "white") +
  scale_fill_viridis_c("Libraries (count)", na.value = "grey50") +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")]) +
  labs(title = "Libraries per community area", caption = cap3) + theme_bw()

p_density <- ggplot(map_dat) +
  geom_sf(aes(fill = sites_per_km2), color = "white") +
  scale_fill_viridis_c("Libraries per km²", na.value = "grey50") +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")]) +
  labs(title = "Library density per community area", caption = cap3) + theme_bw()
p_count; p_density
# Zero-count areas get the lowest color; grey is only for missing values.
# Counts favor big areas just because they're big. Per km2, small dense areas
# move up, so the two maps disagree on which areas are best served.
  
