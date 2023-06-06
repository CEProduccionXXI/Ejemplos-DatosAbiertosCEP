# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar serie de EMAE
emae <- descarga_DP(index = 2)
# Para ver mas series disponibles utilizar función View(DPexistentes())

# Descargar base de salario promedio letra. Empresas privadas y sin apertura de genero
salario <- descarga_DA(47)
salario <- salario %>% filter(w_mean > 0)

# Llevar a precios constantes dic.2022
salario <- deflactar_DA(salario,'2022-12-01',variables_monetarias = 'w_mean',pisar_datos = T) 

# Llevar fechas al mismo formato 
emae <- emae %>% 
  mutate(fecha = lubridate::ym(periodo))
emae <- emae %>% 
  select(fecha,emae=valor)

# Desestacionalizar salarios 
salarios_desestacionalizados <- tibble()
for(i in unique(salario$letra)){
  tmp <- salario %>% filter(letra == i)
  diferencia <- salario %>% filter(letra == i)
  diferencia <- diferencia$w_mean
  diferencia <- ts(diferencia,start=c(2007),frequency=12)
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
    left_join(df,by=c('fecha','letra'))
  salarios_desestacionalizados <- union_all(salarios_desestacionalizados,tmp)
}


# Joinear datos 
datos <- salarios_desestacionalizados %>% 
  left_join(emae)

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

#Quedarme con la industria 
datos <- datos %>% 
  filter(letra == 'C')

# Ver evolución hasta 2021
datos <- datos %>% 
  filter(fecha < '2022-01-01')

# Cambiar nombres de variables 
datos <- datos %>% 
  rename(tendencia = t,desestacionalizada = sa)

# Cambiar base de EMAE a '2007-01-01'
datos <- indexar_DA(datos,'2007-01-01',c('emae'),'letra')
datos <- indexar_DA(datos,'2007-01-01',c('desestacionalizada'),'letra')
datos <- indexar_DA(datos,'2007-01-01',c('tendencia'),'letra')

# Graficar scatterplot: salario promedio y share de mujeres 
plot <- ggplot(datos,aes(fecha)) + 
  geom_line(aes(y=tendencia_index,color='Tendencia del salario'),size=1) + 
  geom_line(aes(y=desestacionalizada_index,color='Salarios desestacionalizados'),size=1) + 
  geom_line(aes(y=emae_index,color='EMAE desestacionalizado'),size=1) + 
  scale_color_manual(name = "Series", values = c("Tendencia del salario" = "#01548a", "Salarios desestacionalizados" = "#f7941e",'EMAE desestacionalizado'='#9283be')) +
  scale_x_date(date_breaks = "5 months") +
  ylab('Series indexadas') + 
  xlab('Fecha') + 
  labs(title = 'Evolución del EMAE y los salarios industriales. 2007 - 2021',
       caption = 'Fuente: elaboración propia en base a CEPXXI') +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size=15,face='bold'),
        plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=12,face='bold'),
        axis.text.x = element_text(angle=45,hjust=1))


plot
