library(RMySQL)
library(ini)
library(sf)
library(dplyr)
library(stringr)

conecta_mysql_bd <- function(dbname, host, port, user, password, client_flag = CLIENT_LOCAL_FILES) {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = dbname,
                   host = host,
                   port = port,
                   user = user,
                   password = password,
                   client.flag = client_flag)
  return(con)
}


extract_coordinates <- function(data, location_column) {
  # Extraemos todas las observaciones de formato x,y
  coordinates_df <- data.frame(location = data[[location_column]])

  # Extraer las coordenadas
  coordinates <- gsub("POINT \\((-?\\d+\\.\\d+) (-?\\d+\\.\\d+)\\)", "\\1 \\2", coordinates_df$location)

  # Convertir a un dataframe
  coordinates_df <- as.data.frame(do.call(rbind, strsplit(coordinates, " ")))
  colnames(coordinates_df) <- c("lon", "lat")
  coordinates_df$lon <- as.numeric(coordinates_df$lon)
  coordinates_df$lat <- as.numeric(coordinates_df$lat)

  # Crear un objeto espacial
  sf_data <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)

  return(sf_data)
}


# Función para leer un archivo .ini y conectar a una base de datos MySQL
conectar_mysql_desde_ini <- function(archivo_config) {
  # Leer el archivo .ini
  config <- read.ini(archivo_config)

  # Extraer los valores necesarios
  base <- config$hidden_variables$base
  servidor <- config$hidden_variables$servidor
  puerto <- as.integer(config$hidden_variables$puerto)
  usuarioBD <- config$hidden_variables$usuario
  passBD <- config$hidden_variables$pass

  # Establecer la conexión a la base de datos MySQL
  con <- dbConnect(MySQL(), dbname = base, host = servidor, port = puerto,
                   user = usuarioBD, password = passBD)

  # Retornar la conexión
  return(con)
}



reemplaza_ceros_medianas <- function(df, columna) {
  # Calcular la mediana para la columna especificada
  median_value <- median(df[[columna]][df[[columna]] != 0], na.rm = TRUE)

  # Reemplazar ceros con la mediana
  df[[columna]][df[[columna]] == 0] <- median_value

  # Devolver el dataframe modificado
  return(df)
}



