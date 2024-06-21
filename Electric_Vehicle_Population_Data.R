ruta <- "C:\\Users\\jorge\\OneDrive - Enel Spa\\BECA 2024\\05 - Practicas en Enel\\01 - Programacion en R\\Electric_Vehicle_Population_Data.csv"
electric_cars <- read.csv(ruta, sep = ",", header = TRUE) #, nrows = 180, encoding = "UTF-8")
head(electric_cars)
tail(electric_cars)
str(electric_cars)
summary(electric_cars)

library(janitor)
# enlaza el conjunto de datos con un pipe a la función clean_names(), y el resultado lo guarda como "linelist"
electric_cars <- electric_cars %>% clean_names()
library(skimr) # Para resumir datos de manera rápida y concisa, proporcionando estadísticas resumidas útiles.

# ver los nombres de las columnas
names(electric_cars)
skim(electric_cars)


# Eliminar filas con valores perdidos
electric_cars_clean <- na.omit(electric_cars)
skim(electric_cars_clean)

unique(electric_cars$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)

hist(electric_cars$Model.Year)
hist(electric_cars$Electric.Utility)

boxplot(electric_cars$Model.Year ~ electric_cars$Make, las = 3)
boxplot(electric_cars$Model.Year ~ electric_cars$City)
boxplot(electric_cars$Model.Year ~ electric_cars$Electric.Vehicle.Type)
boxplot(electric_cars$Model.Year ~ electric_cars$County, las = 2)
boxplot(electric_cars$Model.Year ~ electric_cars$Electric.Utility, las = 2)
boxplot(electric_cars$Model.Year ~ electric_cars$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility, las = 1)

pie(electric_cars$Model.Year)

?boxplot


# Cargamos la librería
library(ggplot2)
library(ggthemes)

?ggthemes

# Creamos el gráfico
p <- ggplot(electric_cars, aes(x=Make, y=Model.Year)) + geom_boxplot()
# Para etiquetas horizontales
p + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))

p <- p + labs(
  title = "Distribución de compra de vehiculos electricos por usuarios a lo largo de los años", # Título del gráfico
  subtitle = "Por fabricante de automoviles electricos", # Subtítulo del gráfico
  #caption = "", # Nota al pie del gráfico
  x = "Marcas de Automoviles Electricos", # Etiqueta del eje X
  y = "Año de Fabricación" # Etiqueta del eje Y
)+theme(plot.title = element_text(hjust = 0.5))

# Para etiquetas verticales
p + theme(plot.margin = margin(t = 20, r = 50, b = 40, l = 10))
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))


# diferentes temas:
p + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Tema minimalista
p + theme_base() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Tema base
p + theme_calc() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Tema calc
p + theme_clean() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Tema limpio
p + theme_economist() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Tema economista
p + scale_color_tableau() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


library(ggplot2)
p2 <- ggplot(data = electric_cars, aes(x = Electric.Range, y = Base.MSRP)) + geom_point()
print(p2)



# Cargamos las librerías
library(ggplot2)
library(ggthemes)

# Creamos una nueva columna para Base.MSRP en miles de dólares
electric_cars$Base.MSRP.Thousands <- electric_cars$Base.MSRP / 1000

# Creamos el gráfico
p <- ggplot(data = electric_cars, aes(x = Electric.Range, y = Base.MSRP.Thousands)) +
  geom_point()

# Aplicamos el tema y hacemos las etiquetas del eje X verticales
p <- p + theme_clean() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Agregamos la etiqueta del eje y en miles de USD
p <- p + labs(y = "Base MSRP (miles de USD)")

# Personalizamos las etiquetas del eje y
p <- p + scale_y_continuous(labels = function(x) paste0(x, "k"))

# Imprimimos el gráfico
print(p)



# Cargamos las librerías
library(ggplot2)

# Creamos el histograma
p <- ggplot(data = electric_cars, aes(x = Model.Year)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black") +
  geom_density(color = "red")

# Imprimimos el histograma con la curva de densidad
print(p)



# Cargamos la librería
library(ggplot2)

# Creamos el histograma
p1 <- ggplot(data = electric_cars, aes(x = Model.Year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de Model.Year", x = "Año del modelo", y = "Frecuencia")

# Imprimimos el histograma
print(p1)

# Creamos el diagrama de cajas
p2 <- ggplot(data = electric_cars, aes(y = Model.Year)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Diagrama de cajas de Model.Year", y = "Año del modelo")

# Imprimimos el diagrama de cajas
print(p2)




# Contar la cantidad de autos en cada ciudad
conteo_autos <- table(electric_cars$City)

# Crear un gr?fico de barras
barplot(conteo_autos, main = "Cantidad de Autos por Ciudad", xlab = "Ciudad", ylab = "Cantidad de Autos")


# Contar la cantidad de autos en cada ciudad
conteo_autos <- table(electric_cars$City)
barplot(table(electric_cars$Electric.Vehicle.Type), main = "Total vehiculos por tipo de Alimentacion Electrica", xlab = "Tipo Fuente Alimentacion", ylab = "Cantidad de Autos")


# Contar la cantidad de autos de cada marca
conteo_marcas <- table(electric_cars$Make)
# Ordenar los resultados en orden descendente
conteo_marcas <- sort(conteo_marcas, decreasing = TRUE)
# Ver los resultados
print(conteo_marcas)
# Crear un gr?fico de barras
barplot(conteo_marcas, main = "Cantidad de Autos El?ctricos por Marca", xlab = "Marca", ylab = "Cantidad de Autos")


# Eliminar las filas con 'NA' en 'electri.range'
electric_cars <- electric_cars[!is.na(electric_cars$electri.range), ]
# Crear un gr?fico de caja y bigotes para 'electri.range'
boxplot(electric_cars$electri.range, main = "Gr?fico de Caja y Bigotes de Rango El?ctrico", ylab = "Rango El?ctrico")

mean(electric_cars$Electric.Range)
median(electric_cars$Electric.Range)


# Ejemplo de modelado: Regresión lineal simple
model <- lm(Base.MSRP ~ Model.Year + Electric.Vehicle.Type, data = electric_cars_clean)
summary(model)

