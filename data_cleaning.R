####~~~~~~~~~~~~~~~~~~~~~~Cleaning WRAS data~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the cleaning of WRAS data as far as possible to be used in future
##          analysis to test the efficacy of the WRAS, as well as any other data needs.
## Date written: 2022-12-21
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## Currently not able to automate this data process, so to download the data, ensure to change the
## "user" 
library(magrittr)

####~~~~~~~~~~~~~~~~~~~~~~Data Import~~~~~~~~~~~~~~~~~~~~~~~####
user <- "AlexMitchell"

# WRAS_data_import <- readxl::read_xlsx(paste0("/Users/", 
#                                             user, 
#                                             "/Ocean Wise Conservation Association/Whales Initiative - WRAS Alert Data/All Alerts upto Sept 2021.xlsx"),
#                                       sheet = "Alerts") %>% 
WRAS_data_import <- readxl::read_xlsx(paste0("C:/Users/",
                                             user,
                                             "/Ocean Wise Conservation Association/Whales Initiative - General/OceanWise Data/WRAS/WRAS_main.xlsx")) %>% 
  janitor::clean_names() %>% 
  dplyr::select(c("alert_sent",
                  # "sighted_at",
                  "latitude", 
                  "longitude", 
                  "skipper", 
                  "vessel", 
                  "species")) %>% 
  # dplyr::filter(lubridate::year(alert_sent) == 2021 | lubridate::year(alert_sent) == 2020) %>%
  dplyr::mutate(dplyr::across(!longitude & !latitude & !alert_sent, fedmatch::clean_strings)) %>%  ## cleans strings to remove special characters and formatting
  dplyr::filter(!vessel %in% c("skana", "kellahan","tsitika","no vessel", 
                               "pilot underway")) %>% 
  ## Assigns a unique key to the data set for the fuzzy matching later
  dplyr::mutate(key1 = 1:nrow(.)) %>% 
  dplyr::mutate(vessel = stringr::str_remove_all(vessel, "mv |m v ")) %>% 
  ## The next mutates clean up some of the common spelling errors and bits the fuzzy matching misses. Repeated for vessel data.
  dplyr::mutate(vessel = dplyr::case_when(vessel == "charles hays amwaal" ~ "charles hays",
                                          vessel == "amwaal" ~ "charles hays",
                                          vessel == "cowichan" ~ "queen of cowichan",
                                          vessel == "sobc" ~ "spirit of british columbia",
                                          vessel == "qalb" ~ "queen of alberni",
                                          vessel == "spirit of bc" ~ "spirit of british columbia",
                                          vessel == "reliant" ~ "seaspan reliant",
                                          vessel == "queen of newwest" ~ "queen of new westminster",
                                          vessel == "q of alberni" ~ "queen of alberni",
                                          vessel == "mazuru bishamon" ~ "maizuru bishamon",
                                          vessel == "coastal renn" ~ "coastal renaissance",
                                          vessel == "sovc" ~ "spirit of vancouver island",
                                          vessel == "q of alberni" ~ "queen of alberni",
                                          vessel == "qnw" ~ "queen of new westminster",
                                          vessel == "laurier" ~ "sir wilfrid laurier",
                                          vessel == "suquamish" ~ "wsf suquamish",
                                          vessel == "howe sounbd queen" ~ "howe sound queen",
                                          vessel == "inspiration" ~ "coastal inspiration",
                                          vessel == "suquwamish" ~ "wsf suquamish",
                                          vessel == "seapan zambizi" ~ "seaspan zambezi",
                                          vessel == "sprit of british columbia" ~ "spirit of british columbia",
                                          vessel == "roald almundsen" ~ "roald amundsen",
                                          vessel == "ovean clio" ~ "ocean clio",
                                          vessel == "coroleader ol" ~ "coreleader ol",
                                          vessel == "zuidetdam" ~ "zuiderdam",
                                          vessel == "seaspan anadonis" ~ "seaspan adonis",
                                          vessel == "berge yotie" ~ "berge yotei",
                                          vessel == "carnval splendor" ~ "carnival splendor",
                                          vessel == "blackball" ~ "coho",
                                          vessel == "zeta" ~ "star zeta",
                                          vessel == "cma cgm rigalito" ~ "cma cgm rigoletto",
                                          vessel == "gulf islands spirit" ~ "spirit of vancouver island",
                                          vessel == "zeda" ~ "star zeta",
                                          vessel == "zeta" ~ "star zeta",
                                          TRUE ~ vessel))

WRAS_users <- readxl::read_xlsx(paste0("C:/Users/",
                                       user,
                                       "/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/WhaleReport Alert System/Participants/WRASUSERS_main.xlsx"),
                                       sheet = "Authorized") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(dplyr::across(!email, fedmatch::clean_strings)) ## cleans strings to remove special characters and formatting (all cols but email)
  # dplyr::filter(include_in_efficacy_analysis == "yes")



WRAS_vessels <- readxl::read_xlsx("/Users/AlexMitchell/Downloads/Vessels_main.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(is.na(vessel_name) == F) %>% 
  dplyr::mutate(dplyr::across(!loa_x_be, fedmatch::clean_strings)) %>%
  tidyr::separate(loa_x_be, c("loa", "be"),sep = "x") %>%  ## split loa x be column to two seperate columns
  dplyr::mutate(dplyr::across(c(loa, be), readr::parse_number)) %>%  ## extract numerics from the new columns
  dplyr::mutate(key2 = 1:nrow(.)) %>% 
  ## As with imported data, cleans up common errors in the alerts data so the data sets can be matched
  dplyr::mutate(vessel_name = dplyr::case_when(vessel_name == "charles hays amwaal" ~ "charles hays",
                                          vessel_name == "amwaal" ~ "charles hays",
                                          TRUE ~ vessel_name)) %>% 
  dplyr::mutate(vessel_name = stringr::str_remove_all(vessel_name, "mv |m v ")) %>% 
  dplyr::select(vessel_name, mmsi, imo, class, loa, be, notes, key2)


####~~~~~~~~~~~~~~~~~~~~~~Cleaning~~~~~~~~~~~~~~~~~~~~~~~####

## WRAS alert data does not contain MMSI number of vessel which we need to use to identify vessel tracks in AIS.
## WRAS alert data contains spelling mistakes in the vessel name due to human error.
## We need a way to match the WRAS alert data to the vessel data, and we can only do that by name.
## Fuzzy matching will allow us to account for human error in spelling in data imports from WRAS data to match vessel names in our main vessel dataset.
## These settings have been changed a little bit, with the maxDist set at 0.5 we were getting incorrect matches
## The "checkMatch" object contains a df of vessels names which don't exactly match. I manually checked through this 
## Dataframe and manually renamed vessel names in the WRAS_data_import. With this many vessel names some are 
## to close (in string distance) for the function to accurately match correctly in some instances.
WRAS_data_matched <- fedmatch::merge_plus(data1 = WRAS_data_import, 
                                          match_type = "fuzzy",
                                          data2 = WRAS_vessels,
                                          by.x = "vessel", by.y = "vessel_name",
                                          unique_key_1 = "key1", unique_key_2 = "key2", 
                                          suffixes = c("_1", "_2"),
                                          fuzzy_settings = fedmatch::build_fuzzy_settings(maxDist = 0.1))


## Which vessels do not have an MMSI associated with them? What additional data do I need to get from marinetraffic.com 
vessel_missing_class <- unique(WRAS_data_matched$matches$name[is.na(WRAS_data_matched$matches$mssi)==T])

## To manually check vessels with different names which are matched
# checkMatch <- WRAS_data_matched %>% dplyr::filter(vessel != vessel_name)

View(tibble::as_tibble(unique(WRAS_data_matched$data1_nomatch$vessel)))

## Cleaned data
WRAS_data_cleaned <- tibble::as_tibble(WRAS_data_matched$matches)

####~~~~~~~~~~~~~~~~~~~~~~Creating map data~~~~~~~~~~~~~~~~~~~~~~~####

## Import the shapefiles used to identify slowdown areas for 2022 
## The following is a little complex as there is three multiline objects, two of which are not closed and therefore I cannot test whether points
## are within lines or not.
## My plan is to split the Slowdown2022 into five seperate polygons and then merge.
slowdown2022 <- sf::read_sf(dsn = "../../OWSN/2022 Slowdown/", layer = "Slowdown") %>% 
  sf::st_transform(., crs = 4326) %>% 
  sf::st_zm(., drop = T) ## The shapefile is coded with a Z dimension, albeit being 0, which we have to remove before plotting 
  
overlap <- slowdown2022 %>% 
  dplyr::filter(Name == "Swiftsure Overlap") %>% 
  sf::st_cast(., "POLYGON") %>% 
  dplyr::select(geometry) %>% 
  plot()

inbound <- slowdown2022 %>% 
  dplyr::filter(Name == "Swiftsure Inbound") %>% 
  sf::st_cast(., "MULTIPOLYGON") %>% 
  dplyr::select(geometry)

outbound_messy <- slowdown2022 %>% 
  dplyr::filter(Name == "Swiftsure Outbound")

outbound1 <- outbound_messy %>% 
  .[1,] %>% 
  sf::st_cast(., "POINT") %>% 
  concaveman::concaveman(.) %>% 
  plot()

outbound2 <- outbound_messy %>% 
  .[2,] %>% 
  sf::st_cast(., "POINT") %>% 
  concaveman::concaveman(.)
  
slowdown_pre_2021 <- sf::st_union(outbound1, outbound2) %>% 
  sf::st_union(overlap) 

slowdown_post_2021 <- sf::st_union(outbound1, outbound2) %>% 
  sf::st_union(inbound) %>%
  sf::st_union(overlap) 

slowdown2022_haro <- sf::read_sf(dsn = "../../OWSN/2022 HaroSlowdown/", layer = "HaroSlowdown") %>%
  sf::st_transform(., crs = 4326) %>% 
  sf::st_cast(., "POLYGON") %>%
  sf::st_zm(., drop = T)

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(data = slowdown2022_haro) %>% 
  leaflet::addPolygons(data = outbound)

  

slowdown2022 <- ?raster::union(slowdown2022_, slowdown2022_haro)
  
