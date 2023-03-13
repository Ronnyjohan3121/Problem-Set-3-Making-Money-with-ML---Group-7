require(pacman)
p_load(
  tidyverse, rio, skimr, viridis, osmdata,
  ggsn, ## scale bar
  raster, stars, ## datos raster
  ggmap, ## get_stamenmap
  sf, ## Leer/escribir/manipular datos espaciales
  leaflet
) ## Visualizaciones dinámicas

p_load(mapview)

setwd("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3")

train<-read.csv(file = 'train.csv')

test<-read.csv(file = 'test.csv')

houses_preproc <- function(houses) {
  houses <- houses %>% dplyr::select(-c(rooms, title, operation_type))
  # Create binary variable for property type to replace property_type variable
  houses <- houses %>% dplyr::mutate(
    house = ifelse(property_type == "Casa", 1, 0)
  )
  houses <- houses %>% dplyr::select(-property_type)
  # Impute missing data in surface_total using surface_covered
  houses <- houses %>% dplyr::mutate(
    surface_total = dplyr::if_else(
      is.na(surface_total), surface_covered, surface_total
    )
  )
  houses <- houses %>% dplyr::select(-surface_covered)
  
  # Normalize descriptions to lower case
  houses$description <- tolower(houses$description)
  # Replace decimal comma with decimal point in descriptions
  houses$description <- stringr::str_replace_all(
    houses$description,
    "(\\d),(\\d)",
    "\\1.\\2"
  )
  
  # Use descriptions to retrieve property areas
  areas_retrieved <- stringr::str_match(
    houses$description,
    " (\\d*\\.?\\d+)\\s?m(t|etro|2|\\s|:punct:)"
  )[, 2]
  
  # Detect cases where points have been used to mark thousands
  point_thousands <- stringr::str_detect(areas_retrieved, "^\\d\\.\\d{3}")
  point_thousands[is.na(point_thousands)] <- FALSE
  # Remove points marking thousands
  areas_retrieved[point_thousands] <- stringr::str_replace_all(
    areas_retrieved[point_thousands],
    "\\.",
    ""
  )
  # Convert values to numerical
  areas_retrieved <- as.numeric(areas_retrieved)
  # Remove values less than 15 (potential errors in parsing)
  areas_retrieved[areas_retrieved < 15] <- NA
  # Use only 1 decimal figure
  houses$areas_retrieved <- round(areas_retrieved, 1)
  houses <- houses %>% dplyr::mutate(
    surface_total = dplyr::if_else(
      is.na(surface_total), areas_retrieved, surface_total
    )
  )
  houses <- houses %>% dplyr::select(-areas_retrieved)
  
  houses <- houses %>% dplyr::mutate(
    sala_com = dplyr::if_else(
      stringr::str_detect(
        description, "sala|comedor"
      ), 1, 0
    ),
    upgrade_in = dplyr::if_else(
      stringr::str_detect(
        description,
        "chimenea|terraza|social|balc.?n|balcã.n|balc&\\w{6};n"
      ), 1, 0
    ),
    upgrade_out = dplyr::if_else(
      stringr::str_detect(
        description,
        "gimnasio|gym|infantil|ni.?os|jard.?n|niã.os|jardã.n|ni&\\w{6};os|jard&\\w{6};n"
      ), 1, 0
    ),
    garage = dplyr::if_else(
      stringr::str_detect(description, "garaje|garage|parqueadero"), 1, 0
    ),
    light = dplyr::if_else(
      stringr::str_detect(
        description,
        "iluminado|iluminaci.?n|iluminaciã.n|iluminaci&\\w{6};n|luz natural"
      ),
      1, 0
    )
  )
  
  houses <- houses %>% dplyr::select(-c(description))
  houses
}

# Transform training data
train <- houses_preproc(train)
# Transfom test data
test2 <- houses_preproc(test)

mapview(test2, xcol = "lon", ycol = "lat", crs = 4326, grid = FALSE)
mapview(train, xcol = "lon", ycol = "lat", crs = 4326, grid = FALSE)
install.packages("Rcpp")
install.packages("mapview")
library(mapview)
library(Rcpp)

install.packages("spdep")
library("tidyverse")
library("sf")
library("osmdata")
library("spdep")
library("dplyr")


train_geo <- st_as_sf(x = train, coords = c("lon", "lat"), crs = 4326)
test_geo <- st_as_sf(x = test2, coords = c("lon", "lat"), crs = 4326)



# Get neighbors for points in a df
get_nb <- function(df, dist) {
  # Get buffer around points
  df_sp <- df %>%
    st_buffer(dist) %>%
    as_Spatial()
  # Get neighbors
  df_nb <- poly2nb(pl = df_sp, queen = TRUE)
  df_nb
}

# Neighbors to houses in Bogota - 150 meters
train_nb_150 <- get_nb(train_geo, 150)
saveRDS(train_nb_150, file = "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_train_150.Rds")


test_nb_150 <- get_nb(test_geo, 150)
saveRDS(test_nb_150, file = "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_test_150.Rds")


# Get a specific set of polygons corresponding to the geographic feature of interest
get_feature <- function(place, key, value) {
  feature <- opq(bbox = getbb(place)) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf() %>%
    .$osm_polygons %>%
    dplyr::select(osm_id)
  
  feature
}

# Load matrix of pairs of selected key-value pairs
keyvals <- read.csv("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/st_maps_key_val.csv")

# Get and store polygons for the previously defined set of geographic features - train Bogota
for (i in 1:nrow(keyvals)) {
  feature <- get_feature("Bogota Colombia", keyvals$key[i], keyvals$value[i])
  dist_feature <- st_distance(x = train_geo, y = feature)
  path <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_train_", keyvals$value[i], ".Rds")
  saveRDS(dist_feature, path)
}

# Get and store polygons for the previously defined set of geographic features - test
for (i in 1:nrow(keyvals)) {
  feature <- get_feature("Bogota Colombia", keyvals$key[i], keyvals$value[i])
  dist_feature <- st_distance(x = test_geo, y = feature)
  path <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_test_", keyvals$value[i], ".Rds")
  saveRDS(dist_feature, path)
}

p_load(tidyverse,rio,skimr,viridis,
       sf, ## leer/escribir/manipular datos espaciales
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

## houses from geo data


houses_train <- train_geo
houses_test <- test_geo

# obtener poligono 
sf_use_s2(FALSE)

select <- dplyr::select

get_estrato <- function(base){
  if(base == "train"){
    houses <- houses_train
    df <- getbb(place_name = "Bogota", featuretype = "", format_out = "sf_polygon") %>% .$multipolygon
    mnz <- st_read("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/MGN2018_URB_MANZANA/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV")
    viv <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_1VIV_A2_11.CSV")
  }
  
  if(base == "test"){
    houses <- houses_test
    df <- getbb(place_name = "Bogota", featuretype = "", format_out = "sf_polygon") %>% .$multipolygon
    mnz <- st_read("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/MGN2018_URB_MANZANA/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV")
    viv <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_1VIV_A2_11.CSV")
  }
  
  ### Unir datos imputados con manzanas DANE
  houses <- st_join(x=houses , y=mnz)
  
  
  ### Crear data estratos
  ## data manzanas
  mgn <- mgn %>% select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
  
  ## data vivienda
  viv <- viv %>% select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO)
  
  ## joing mnz-hogar-vivienda
  viv_mgn <- left_join(viv, mgn, by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
  
  ##=== collapse data ===##
  db <- viv_mgn %>%
    group_by(COD_DANE_ANM) %>% 
    summarise(med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))
  
  
  ### UNir estratos hogar por manzana
  
  houses <- left_join(houses,db,by=c("MANZ_CCNCT"="COD_DANE_ANM"))
  
  estrato <- houses %>% select(property_id, med_VA1_ESTRATO)
  estrato <- st_drop_geometry(estrato)
  path_exp <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_", base, ".rds")
  export(estrato, path_exp)
}

get_estrato("train")
get_estrato("test")

get_mode <- function(x) { # Create mode function
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  if (length(tabulate_x) > 1) {
    unique_x <- na.omit(unique(x))
    tabulate_x <- tabulate(match(x, unique_x))
    modes <- unique_x[tabulate_x == max(tabulate_x)]
    if (length(modes > 1)) {
      set.seed(10)
      modes <- sample(modes, size = 1)
      return(modes)
    } else {
      return(modes)
    }
  } else {
    return(unique_x[tabulate_x == max(tabulate_x)])
  }
}

#estratos

estrato_train <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_train.rds")
estrato_test <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_test.rds")


houses_train <- houses_train %>% left_join(estrato_train, by = "property_id")
houses_test <- houses_test %>% left_join(estrato_test, by = "property_id")


houses_train <- houses_train %>% dplyr::rename(estrato = med_VA1_ESTRATO)
houses_test <- houses_test %>% dplyr::rename(estrato = med_VA1_ESTRATO)


nei_train_150 <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_train_150.Rds")
nei_test_150 <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_test_150.Rds")

# Impute values using custom mode function that retrieves most repeated values
# in a vecinity
mode_imputer <- function(df, neighbors) {
  # Variables that have values to be imputed
  vars_to_impute <- c(
    "bathrooms",
    "sala_com",
    "upgrade_in",
    "upgrade_out",
    "garage",
    "light",
    "estrato"
  )
  
  for (variable in vars_to_impute) {
    # Create empty column to fill with imputed values
    imputed_var <- paste0("imputed_", variable)
    df[, imputed_var] <- numeric(nrow(df))
    
    for (value in seq_len(nrow(df))) { # For each property in the newly created column
      # Get indices for its neighbors
      values_neighbors <- df[neighbors[[value]], variable][[1]]
      # Apply custom mode function on the currently iterated variable and set of neighbors
      imputed <- get_mode(values_neighbors)
      # Impute the obtained mode to the currently iterated property
      df[value, imputed_var] <- imputed
    }
  }
  df
}


# Repeat same algorithm as in mode_imputer, but using mean instead of mode
mean_imputer <- function(df, neighbors) {
  vars_to_impute <- c("surface_total")
  for (variable in vars_to_impute) {
    imputed_var <- paste0("imputed_", variable)
    df[, imputed_var] <- numeric(nrow(df))
    for (value in seq_len(nrow(df))) {
      values_neighbors <- df[neighbors[[value]], variable][[1]]
      imputed <- mean(values_neighbors, na.rm = TRUE)
      if (is.nan(imputed)) {
        imputed <- NA
      }
      df[value, imputed_var] <- imputed
    }
  }
  df
}
