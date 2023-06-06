# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar base de percentiles salariales abierto por letra. Empresas privadas y sin apertura de genero
datos <- descarga_DA(index_base = 16)
# O bien: datos <- datos <- descarga_DA(tipo = "Percentil Salario",genero = 'NO',universo = 'Privado',jurisdiccion = 'NO',sector = 'Letra')

# Llevar precios de todas las variables a precios constantes de diciembre 2022
datos <- deflactar_DA(data = datos,
                      mes_base = '2022-12-01') # Al no aclarar las variables se pasan a constantes todas las monetarias

# Calcular la diferencia porcentual entre el percentil 10 y el 90 para cada letra 
datos <- datos %>% mutate(dif_porc = abs(p10 - p90) *100 / ((p10+p90)/2))

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

# Graficar industria, minas y canteras y comercio
datos %>% 
  filter(letra %in% c('B','C','G','P')) %>% 
  ggplot(aes(fecha)) + 
  geom_line(aes(y =w_mean_constante,color = letra_desc)) + 
  labs(title='Media del salario bruto. Precios constantes (Dic. 2022)',
       subtitle = 'Sectores seleccionados'
       ) + 
  xlab('Fecha') + 
  ylab('Salario bruto promedio') +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.caption = element_text(size=12,face='bold'),
        plot.title = element_text(size=15,face='bold'),
        plot.subtitle = element_text(face='bold')) + 
  guides(color=guide_legend(nrow=3,byrow=TRUE))
