options(box.path = getwd())
box::use(
  dplyr[...],
  rvest[...],
  magrittr[`%>%`],
  janitor[...],
  purrr[...],
  usmap[...],
  ggplot2[...],
  stringr[...]

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
