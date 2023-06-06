# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar base de percentiles salariales abierto por letra. Empresas privadas y sin apertura de genero
datos <- descarga_DA(index_base = 16)
# O bien: datos <- datos <- descarga_DA(tipo = "Percentil Salario",genero = 'NO',universo = 'Privado',jurisdiccion = 'NO',sector = 'Letra')


# Calcular la diferencia porcentual entre el percentil 10 y el 90 para cada letra 
datos <- datos %>% mutate(dif_porc = abs(p10 - p90) *100 / ((p10+p90)/2))

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

# Desestacionalizar serie 
datos_desestacionalizados <- tibble()
for(i in unique(datos$letra)){
  tmp <- datos %>% filter(letra == i)
  diferencia <- datos %>% filter(letra == i)
  diferencia <- diferencia$dif_porc
  diferencia <- ts(diferencia,start=c(2013),frequency=12)
  #Descomponer con RJDemetra
  descomposicion <- RJDemetra::x13(diferencia)
  # Llevar el resultado a dataframe
  df <- data.frame(.preformat.ts(descomposicion$final$series), stringsAsFactors = FALSE)
  # Pasar el nombre de las filas a variable
  df <- df %>% tibble::rownames_to_column(var = 'fecha')
  # Dar formato de fecha
  df <- df %>% 
    mutate(fecha = lubridate::my(fecha))
  # Seleccionar columnas deseadas
  df <- df %>% 
    select(fecha,sa,t) %>% 
    mutate(letra = i)
  # Unir datos 
  tmp <- tmp %>% 
    left_join(df)
  datos_desestacionalizados <- union_all(datos_desestacionalizados,tmp)
}

# Graficar industria, minas y canteras y comercio
datos_desestacionalizados %>% 
  filter(letra %in% c('B','C','G','P')) %>% 
  ggplot(aes(fecha)) + 
  geom_line(aes(y = t,color = letra_desc),linetype='dotted',size=1.3) + 
  geom_line(aes(y = sa,color = letra_desc)) + 
  labs(title='Diferencia porcentual entre percentil 10 y 90',
       subtitle = 'Sectores seleccionados',
       caption='Linea punteada: tendencia de la serie\nLinea entera: serie desestacionalizada (X13)') + 
  xlab('Fecha') + 
  ylab('% Diferencia p10 y p90') +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.caption = element_text(size=12,face='bold'),
        plot.title = element_text(size=15,face='bold'),
        plot.subtitle = element_text(face='bold')) + 
  guides(color=guide_legend(nrow=3,byrow=TRUE))
