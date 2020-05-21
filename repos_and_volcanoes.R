library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(tidytuesdayR)

#------------------
#
# Import data
#
#------------------

tt_data <- tt_load("2020-05-12") 
volcanos_raw <- tt_data$volcano

volcanos <- volcanos_raw %>% 
  select(volcano_number, volcano_name, primary_volcano_type,
         last_eruption_year, country, latitude, longitude,
         starts_with("population"))

# Data fetched from the CORE REST API
repos_raw <- read.csv("repositories.csv", stringsAsFactors = FALSE)

repos <- repos_raw %>% 
  rename(Name = allnames,
        CORE_id = allids,
        Metadata = allcountmetadatas,
        Fulltext = allfulltextcount,
        lat = alllat,
        lon = alllon) %>%
  mutate(Name = sub("\\(?<.*", "", Name)) %>% # trouble with Japanese...
  filter(lat != 0,
         lon != 0,
         Metadata > 100)


# Notice the correct order of lon/lat
volcanos_sf <- st_as_sf(volcanos, coords = c("longitude","latitude"), crs = 4326)
repos_sf <- st_as_sf(repos, coords = c("lon","lat"), crs = 4326)

#------------------------------------------------------------------------------------
#
# Distance btw volcanoes and repos. The shorter object (volcanos_sf) is recycled.
# The result is a matrix.
#
#------------------------------------------------------------------------------------
dist_btw_volcanos_repos <- 
  st_distance(volcanos_sf, repos_sf)

#---------------------------------
#
# Row = volcano, column = repo
#
#---------------------------------
dist_df <- as.data.frame(dist_btw_volcanos_repos)
names(dist_df) <- repos_sf$CORE_id
rownames(dist_df) <- volcanos_sf$volcano_number

rm(dist_btw_volcanos_repos)
gc()


#------------------------------------------------------------------------
#
# Filter those volcanoes which have at least one repo within 50 km range
#
#------------------------------------------------------------------------
volcanos_with_repo_within_50_km_distance <- dist_df %>% 
  rownames_to_column("volcano") %>% 
  mutate_if(~class(.) == "units", as.numeric) %>% # st_distance output class is 'units'
  gather(repo, distance, -volcano) %>% 
  filter(distance < 50000) %>% 
  mutate(volcano = as.numeric(volcano),
         repo = as.numeric(repo))

rm(dist_df)
gc()

#------------------------
#
# Join with volcano data
#
#------------------------
volcanos_coords <- volcanos_with_repo_within_50_km_distance %>% 
  left_join(., volcanos_sf, by = c("volcano"="volcano_number")) %>% 
  distinct_at(vars(volcano), .keep_all = TRUE)

#------------------------------------------------
#
#  Transform coordinates to a metric projection
#  for adding a 50 km buffer. Then, back to EPSG:4326.
#
#  Also, build labels without geometry
#
#-----------------------------------------------
Volcanos <- st_as_sf(volcanos_coords, crs = 4326) %>% 
  select(-volcano, -repo, -distance)

Volcanos_no_geom <- st_drop_geometry(Volcanos)

Volcanos_labs <- lapply(seq(nrow(Volcanos_no_geom)), function(i) {
  paste0( '<b>', Volcanos_no_geom[i, "volcano_name"], '</b><br/>', 
          Volcanos_no_geom[i, "country"], '<br/>', 
          Volcanos_no_geom[i, "primary_volcano_type"]) 
})

volcanos_coords_sf_7801 <- st_transform(Volcanos, crs = 7801)
volcanos_coords_sf_7801_buffer <- volcanos_coords_sf_7801 %>% 
   st_buffer(dist = 50000)
 
Volcanos_with_buffer <- st_transform(volcanos_coords_sf_7801_buffer, crs = 4326) %>% 
  rename(Volcano = volcano_name) 

#---------------------------------
# 
# Join repo data with the rest
#
#--------------------------------

repos_coords <- volcanos_with_repo_within_50_km_distance %>% 
  left_join(., repos_sf, by = c("repo"="CORE_id")) %>% 
  left_join(., volcanos, by = c("volcano"="volcano_number")) %>% 
  distinct_at(vars(repo), .keep_all = TRUE)

repos_coords_sf <- st_as_sf(repos_coords, crs = 4326)

Repositories <- st_transform(repos_coords_sf, crs = 4326) %>% 
  select(Name, Metadata, Fulltext) 

Repositories_no_geom <- st_drop_geometry(Repositories)
Repositories_labs <- lapply(seq(nrow(Repositories_no_geom)), function(i) {
  paste0( '<b>', Repositories_no_geom[i, "Name"], '</b><br/>', 
          'Metadata count ', Repositories_no_geom[i, "Metadata"], '<br/>', 
          'Fulltext count ', Repositories_no_geom[i, "Fulltext"]) 
})


#----------
# 
# Map
#
#----------

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = Repositories$Metadata
)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: #FFFFE5;
    font-weight: bold;
    font-family: verdana;
    font-size: 20px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Repositories under the volcano")
)  


m <- 
  leaflet() %>% # TODO change the default page title
  addProviderTiles(providers$Stamen.Toner) %>%  
  addControl(title, position = "topleft", className = "map-title") %>% 
  setView(lng = -84.233, lat = 10.2, zoom = 9) %>% # Focus on Costa Rica
  addTiles(urlTemplate = "", 
           attribution = '|Repository data core.ac.uk|Volcano data {tidytuesdayR} 2020-05-12|@ttso') %>% 
  addMeasure(
    primaryLengthUnit = "meters"
    ) %>%
  addCircles(
    data = Volcanos,  
    radius = 5000, 
    color = "white",
    fillColor = "red",
    stroke = TRUE,
    fillOpacity = 0.4,
    label = lapply(Volcanos_labs, HTML),
    labelOptions = labelOptions(noHide = T, 
                                direction = "auto",
                                opacity = 0.7,
                                style = list(
                                  "color" = "coral4",
                                  "font-family" = "verdana",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)"))
  ) %>% 
  addPolygons(
    data = Volcanos_with_buffer,
    color = "red",
    weight = 1,
    fillOpacity = 0.1
    ) %>%
  addCircles(
    data = Repositories,
    label = lapply(Repositories_labs, HTML),
    labelOptions = labelOptions(noHide = F, 
                                direction = "auto",
                                offset=c(20,-15),
                                style = list(
                                  "color" = "coral4",
                                  "font-family" = "verdana",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)")),
    radius = 3000, 
    fillColor = ~pal(Metadata),
    stroke = TRUE,
    color = "black",
    weight = 3,
    fillOpacity = 0.8
    ) %>% 
  addLegend(
    data = Repositories,
    position = "bottomright", 
    pal = pal, 
    values = ~Metadata,
    title = "Repository metadata count",
    opacity = 1)

mapview::mapshot(m, url = "repos_under_volcanoes_map.html")
