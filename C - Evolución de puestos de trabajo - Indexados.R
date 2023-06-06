# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar base de percentiles salariales abierto por letra. Empresas privadas y sin apertura de genero
datos <- descarga_DA(index_base = 72)
# O bien: datos <- datos <- descarga_DA(tipo = "Puestos provincia y sector",genero = 'NO',universo = 'Privado',jurisdiccion = 'Provincia trabajo',sector = 'Letra')

# Indexar puestos contra su maximo por letra y provincia 
datos <- indexar_DA(data = datos,base_indice = 'max',variables_datos_abiertos = c('puestos'),variables_agrupar = c('zona_prov','letra')) 

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

# Graficar industria de Buenos Aires, Cordoba y Santa Fe
datos %>% 
  filter(letra %in% c('C')) %>% 
  filter(zona_prov %in% c('BUENOS AIRES','CORDOBA','SANTA FE')) %>% 
  ggplot(aes(fecha,puestos_index)) + 
  geom_line(aes(color = zona_prov)) + 
  labs(title='Evolución de los puestos de trabajo según su máximo provincial',
       subtitle = 'Industria manufacturera'
       ) + 
  xlab('Fecha') + 
  ylab('Índice de puestos') +
  scale_x_date(date_breaks = "5 months") +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45,hjust=1),
        plot.caption = element_text(size=12,face='bold'),
        plot.title = element_text(size=15,face='bold'),
        plot.subtitle = element_text(face='bold'))
 
