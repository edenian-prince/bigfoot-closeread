options(box.path = getwd())
box::use(
  dplyr[...],
  rvest[...],
  magrittr[`%>%`],
  janitor[...],
  purrr[...],
  usmap[...],
  ggplot2[...],
  stringr[...],
  leaflet[...],
  sf[...],
  plotly[...],
  ggmap[...],
  scales[...],
  hrbrthemes[...],
  lubridate[...],
  htmlwidgets[...],
  webshot[...]

)

# get the url for the bigfoot website for washington state data
wa_url <- "https://www.bfro.net/GDB/state_listing.asp?state=wa"
or_url <- "https://www.bfro.net/GDB/state_listing.asp?state=or"
bc_url <- "https://www.bfro.net/GDB/#usa"

# ----- get washington data ----- #
wa_list <- wa_url %>%
  read_html() %>%
  html_elements("table") %>%
  html_table() %>%
  map(~ row_to_names(.x, row_number = 1)) %>%
  map(~ clean_names(.x))

# im hard coding this, screw it. this website is so old, i doubt it's gonna change anytime soon.
wa_df <- bind_rows(wa_list[[7]], wa_list[[8]]) %>%
  mutate(abbr = "WA")


# ----- get oregon data ----- #
or_list <- or_url %>%
  read_html() %>%
  html_elements("table") %>%
  html_table() %>%
  map(~ row_to_names(.x, row_number = 1)) %>%
  map(~ clean_names(.x))

or_df <- bind_rows(or_list[[7]], or_list[[8]]) %>%
  mutate(abbr = "OR")


# bc_df <- url %>%
#   read_html() %>%
#   html_elements("table") %>%
#   html_table()

# ----- combine them ----- # 

df <- bind_rows(wa_df,or_df)


# ----- create map ----- #

# base map
# plot_usmap(include = .pacific, exclude = c("AK","HI","CA"),regions = "counties")

# combine the map with the bigfoot data
base_map <- us_map(include = .pacific,exclude = c("AK","HI","CA"),regions = "counties") %>%
  mutate(county = str_remove(county, " County$")) %>%
  # join to bigfootdata
  inner_join(df,by=c("county","abbr")) %>%
  mutate(n = as.numeric(number_of_listings))

base_map |>
  ggplot() +
  geom_sf(aes(fill=n)) +
  scale_fill_viridis_c(direction = 1,option = "viridis") +
  theme_minimal(base_size = 18, base_family = 'IBM Plex Mono') 

wa_counties %>%




load("data/Bigfoot_county_aggregations.rda")
load("data/bigfoot_county_date_aggregations.rda")
load("data/bigfoot_points.rda")
load("data/season_columns.rda")
load("data/statewide_date_aggregations.rda")
load("data/wa_counties.rda")
load("data/wa_counties2.rda")

map_labels <- paste0("<strong>County: </strong>",  # <strong> create bold text
                     wa_counties$JURISDICT_LABEL_NM,
                     "<br>",                      # br creates a line break
                     "<strong>Total Number of Bigfoot Sightings: </strong>",
                     wa_counties$sightings_count,
                     "<br>",
                     "<strong>Percent of Total sightings: </strong>",
                     round(wa_counties$percent_of_total, digits = 2),
                     "<br>",
                     "<br>",
                     "Counts that are below 10 are suppressed for<br> the Bigfeets privacy and represented with an '*'") %>%
  # there's a pipe to the right of the line above
  # we use lapply, because we're creating a separate HTML string for each row in the shape file
  lapply(htmltools::HTML)

BigFootIcon <- makeIcon(
    iconUrl = "https://images.fineartamerica.com/images/artworkimages/medium/3/gluten-free-cute-bigfoot-cartoon-noirty-designs-transparent.png",
    iconWidth = 38 ,
    iconAnchorX = 22,
    iconAnchorY = 24
  )

idk <- bigfoot_points %>%

  count(year_as_date) %>%
  mutate(percent_of_total = round(n / nrow(bigfoot_points) * 100, digits = 1)) %>%
  rename(sightings_count = n)

base <- wa_counties %>%

  #pipes the shape files into a leaflet map
  # more info about leaflet:  https://rstudio.github.io/leaflet/

  leaflet() %>%

  # this adds the county shape files onto the map
  addPolygons(
    # this tells leaflet to make a cloropleth based
    # on sightings count values, the palette and breaks
    # were defined in pre-processing steps
    # fillColor = sightings_count,

    # this is an internal id that will be useful for getting
    # mouse hover and click data back to the server
    layerId = wa_counties$JURISDICT_LABEL_NM,

    #line border thickness
    weight = 2,

    # transperency of the border lines
    opacity = 1,

    # border lines color
    color = "#595959",

    # make the border lines dashed with size three dashes
    dashArray = "1",

    # transparency of the shape
    fillOpacity = 0.7,

    # What happens when the mouse is over a shape:
    highlightOptions = highlightOptions(
      weight = 5,
      fillColor = "green",
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),

    # this is the label the map uses
    # we defined the label pattern in the pre-processing steps
    # note: labels are not dynamically rendered through shiny
    # each county's label's asociated html is pre-defined in a column
    # in the sf dataframe

    # label = map_labels,

    # this defines the css for the label
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                    padding = "3px 8px",
                    "margin-left" = "auto",
                    "margin-right" = "auto"),
      textsize = "15px",
      direction = "auto"))%>%

  addProviderTiles("Esri.WorldGrayCanvas")



  source("scripts/shiny_common_functions.R")

# This creates a sequence of dates from the earliest sighting to the last sighting
# the sequence is every year, we use it for factor levels


sightings_date_range <- seq.Date(from = min(bigfoot_points$year_as_date),
                                 to = max(bigfoot_points$year_as_date),
                                 by = "years")




### Make missing values  for under chart labels---------------------------
# see shiny_common_functions.R for details

report_missing <- make_missing_values_labels("classification", df = bigfoot_points)
weekday_missing <- make_missing_values_labels("report_weekday", df = bigfoot_points)
season_missing <- make_missing_values_labels("season", df = bigfoot_points)




### aesthetic specifications for cloropleth------------------------------------------------

# this is an easter egg for someone to find!
# probably won't make it to the final draft QQ

BigFootIcon <- makeIcon(
  iconUrl = "https://images.fineartamerica.com/images/artworkimages/medium/3/gluten-free-cute-bigfoot-cartoon-noirty-designs-transparent.png",
  iconWidth = 38 ,
  iconAnchorX = 22,
  iconAnchorY = 24
)




### County Cloropleth labels ---------------------------------------------------

# these are the popup labels for the map
# it creates a vector of text strings
# that we then lapply into html
# it defines the labels in html


map_labels <- paste0("<strong>County: </strong>",  # <strong> create bold text
                     wa_counties$JURISDICT_LABEL_NM,
                     "<br>",                      # br creates a line break
                     "<strong>Total Number of Bigfoot Sightings: </strong>",
                     wa_counties$sightings_count,
                     "<br>",
                     "<strong>Percent of Total sightings: </strong>",
                     round(wa_counties$percent_of_total, digits = 2),
                     "<br>",
                     "<br>",
                     "Counts that are below 10 are suppressed for<br> the Bigfeets privacy and represented with an '*'") %>%
  # there's a pipe to the right of the line above
  # we use lapply, because we're creating a separate HTML string for each row in the shape file
  lapply(htmltools::HTML)



# Extract the coordinates from the sfc_POINT column
coords <- st_coordinates(bigfoot_points)

# Create new columns in the original sf data for longitude and latitude
bigfoot_points$lon <- coords[, 1]  # longitude
bigfoot_points$lat <- coords[, 2]  # latitude


popup <- paste0(
  "<p id='popup-title'><strong>", bigfoot_points$summary, "</strong></p>",
  "<div id='first-popbox'>",
  "<strong>Report Date: </strong>", format(as.Date(bigfoot_points$report_date2), "%B %d, %Y"),
  "<br><strong>Report Classification: </strong>", bigfoot_points$classification,
  "<br><strong>Length of Report: </strong>", bigfoot_points$report_length, " characters",
  "<br><strong>Report Season: </strong>", bigfoot_points$season,
  "<br><br><strong>County: </strong>", bigfoot_points$county,
  "<br><strong>Nearest Town: </strong>", bigfoot_points$nearest_town,
  "<br><strong>Environment: </strong>", bigfoot_points$environment,
  "</div>",
  "<div id='second-popbox'>",
  "<p id='popbox-report-text'><strong>Report text</strong></p><br>",
  substr(bigfoot_points$observed, 1, 400),"... ",
  "<br><a href='",bigfoot_points$url, "'>click to see full report</a></div>"
) %>%
  lapply(htmltools::HTML)


df <- bigfoot_points %>%
  count(year_as_date) |>
      arrange(year_as_date) |>
      mutate(cumulative = cumsum(n))

df %>%
ggplot(aes(x=year_as_date,y=cumulative)) +
                  geom_line(color="#05382c", size=2, alpha=0.9, linetype=1) +
                  scale_x_date(date_breaks = "20 year", 
                  labels=date_format("%b-%Y"),
                  limits = as.Date(c('1920-01-01','2025-04-01'))) +
                  ylim(0,500) +
                  # theme_minimal(base_size = 30) +
                  guides(colour = "none", shape = "none") +
  labs(x="Year", y="Cumulative Count",
       title="Cumulative Count of Bigfoot Sightings in WA",
       subtitle="A really rough estimate..",
       caption="Note: the year is the year a person reported seeing bigfoot") + 
  theme_ipsum(grid="Y")


bigfoot_points %>%
  count(year_as_date) %>%
  mutate(percent_of_total = round(n / nrow(bigfoot_points) * 100, digits = 1)) %>%
  rename(sightings_count = n) %>%


  # This pipes the filtered dataset into a ggplot
  ggplot() +
  
  ggplot_standard_theme +

  # create bar chart
  geom_col(aes(x = year_as_date, y = sightings_count), fill = "#0D6ABF") +

  # define plot aesthetics unique to this plot

  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 15)) +

  #define plot labels
  labs( title = "TOTAL ANNUAL SIGHTINGS COUNT",
        y = element_blank(),
        x = "\nYear of Sighting")


  bigfoot_points %>%


    count(year_as_date) %>%
    mutate(percent_of_total = round(n / nrow(bigfoot_points) * 100, digits = 1)) %>%
    rename(sightings_count = n) %>%


    # This pipes the filtered dataset into a ggplot
    ggplot() +
    
    ggplot_standard_theme +

    # create bar chart
    geom_col(aes(x = year_as_date, y = sightings_count), fill = "#0D6ABF") +

    # define plot aesthetics unique to this plot

    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 15)) +

    #define plot labels
    labs( title = "TOTAL ANNUAL SIGHTINGS COUNT",
          y = element_blank(),
          x = "\nYear of Sighting")


# Assuming wa_counties is an sf object
st_write(wa_counties, "wa_counties.geojson", driver = "GeoJSON")
# Assuming 'bigfoot_points' is a spatial points dataframe
st_write(bigfoot_points, "bigfoot_points.geojson", driver = "GeoJSON")


final<- leaflet()  %>%

    # This adds a counties outline
    addPolylines(data = wa_counties,
                 color = "#595959") %>%

    # this is the style guide recommended grey
    # addProviderTiles("Esri.WorldImagery") %>%
    addTiles() %>%

    # this was the fun trees and stuff version
   # addProviderTiles(providers$Stadia.StamenTerrain)   %>%
    addMarkers( data = bigfoot_points,
                label = ~summary,
                popup = popup,
                icon = BigFootIcon,
                group = "default_feets"
    )

final 

base
## save html to png
saveWidget(base, "base.html", selfcontained = FALSE)
webshot("base.html", file = "Rplot.png",
        cliprect = "viewport")

final

lat <- 47.7511  # Latitude for Washington State
lon <- -120.7401  # Longitude for Washington State

# Create the map
base <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = lon, lat = lat, zoom = 7)

df <- bigfoot_points %>% filter(year_as_date < '1970-01-01')
map1 <- base %>%
    addMarkers( data = df,
                label = ~summary,
                popup = popup,
                icon = BigFootIcon,
                group = "default_feets"
    )
## save html to png
saveWidget(map1, "base.html", selfcontained = FALSE)
webshot("base.html", file = "map1.png",
        cliprect = "viewport")

df <- bigfoot_points %>% filter(year_as_date < '1990-01-01')
map2 <- base %>%
    addMarkers( data = df,
                label = ~summary,
                popup = popup,
                icon = BigFootIcon,
                group = "default_feets"
    )
## save html to png
saveWidget(map2, "base.html", selfcontained = FALSE)
webshot("base.html", file = "map2.png",
        cliprect = "viewport")

df <- bigfoot_points %>% filter(year_as_date < '2025-01-01')
map3 <- base %>%
    addMarkers( data = df,
                label = ~summary,
                popup = popup,
                icon = BigFootIcon,
                group = "default_feets"
    )
## save html to png
saveWidget(map3, "base.html", selfcontained = FALSE)
webshot("base.html", file = "map3.png",
        cliprect = "viewport")
        
