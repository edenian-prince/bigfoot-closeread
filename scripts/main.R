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
  ggmap[...]

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

# Create a plotly map to show the points
plot_ly(data = bigfoot_points, 
        x = ~lon, 
        y = ~lat, 
        type = 'scattermapbox', 
        mode = 'markers') %>%
  layout(
    mapbox = list(
      style = "open-street-map",  # Use your preferred map style
      center = list(lon = mean(points_df$lon), lat = mean(points_df$lat)),
      zoom = 10
    )
  )

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5,
  style = "satellite",  # You can use other styles like "carto-positron" or "satellite"
  center = list(lon = -120.7401, lat = 47.7511),  # Coordinates for Washington State
  zoom = 7  # Adjust zoom level to fit Washington State (7-8 is a good range)
)

fig <- plot_geo(bigfoot_points, lat = ~lat, lon = ~lon)


fig %>% add_markers(
  text = ~paste(observed)
) %>%
  layout(
    plot_mapbox(), 
    mapbox = list(style = "light"),
    updatemenus = list(
      list(y = 0.8)
    )
  )
fig <- fig %>% add_markers(
    text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
    color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
  )
fig <- fig %>% colorbar(title = "Incoming flights<br />February 2011")
fig <- fig %>% layout(
    title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
  )

fig

bigfoot_points %>%


    # this filters the date range based on what the user has set as the date
    # range with the slider
    dplyr::filter(year_as_date %in% seq.Date(from = input$startdate[1],
                                              to = input$startdate[2],
                                              by = 1)) %>%

    count(year_as_date) %>%
    mutate(percent_of_total = round(n / nrow(bigfoot_points) * 100, digits = 1)) %>%
    rename(sightings_count = n) %>%


    # This pipes the filtered dataset into a ggplot
    ggplot() +

    # create bar chart
    geom_col(aes(x = year_as_date, y = sightings_count), fill = "#0D6ABF") +


    ggplot_standard_theme +

    # define plot aesthetics unique to this plot

    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 15)) +

    #define plot labels
    labs( title = "TOTAL ANNUAL SIGHTINGS COUNT",
          y = element_blank(),
          x = "\nYear of Sighting")

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Lato", color = "#22211d"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "lightblue", color = NA),
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.margin = margin(0,0,0,0,"cm"), # <- set to negative to remove white border
      panel.border = element_blank(),
      ...
    )
}

base <- ggplot(wa_counties) +
  geom_sf(
    color = "white",
    fill = "#dfdfdf",
    size = 0.2) +
  theme_map()

base +
  geom_sf(
    data=bigfoot_points,aes(color=factor(report_no))
  ) +
    theme(legend.position="none")

map <- get_googlemap("Washington, USA")

