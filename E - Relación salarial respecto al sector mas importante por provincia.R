# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar base de puestos por provincia y letra. Empresas privadas y sin apertura de genero
puestos <- descarga_DA(index_base = 72)
# O bien: datos <- descarga_DA(tipo = "Puestos provincia y sector",genero = 'NO',universo = 'Privado',jurisdiccion = 'Provincia trabajo',sector = 'Letra')

# Descargar base de salario promedio por provincia y letra Empresas privadas y sin apertura de genero
salario <- descarga_DA(81)
# O bien: datos <- descarga_DA(tipo='Salario prom por provincia y sector',genero = 'NO','universo='Privado',jurisdiccion='Provincia trabajo',sector = 'Letra')

# Joinear datos 
datos <- puestos %>% 
  left_join(salario)

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

# Llevar a precios constantes dic.2022
datos <- deflactar_DA(datos,'2022-12-01',variables_monetarias = 'w_mean') 

# Quedarse con ultimo dato disponible
datos <- datos %>% 
  filter(fecha == max(datos$fecha))

# Quedarse con los 10 sectores mas importantes en cuanto empleo
datos <- datos %>% 
  group_by(letra) %>% 
  mutate(Empleo_sector = sum(puestos)) %>% 
  ungroup() %>% 
  arrange(desc(Empleo_sector)) %>% 
  group_by(zona_prov) %>% 
  filter(row_number() < 11)

# Llevar a valores índices de cada provincia los salarios promedio
datos <- indexar_DA(datos,base='max',variables_datos_abiertos = 'w_mean',variables_agrupar='zona_prov')

# Graficar salario promedio de cada sector por provincia 
ggplot(datos,aes(zona_prov,w_mean_index)) + 
  geom_line(size=0.5,color='grey',aes(group=zona_prov)) +
  geom_point(aes(color=letra_desc),size=5,alpha=0.5) + 
  ylab('Índice salarial') + 
  xlab('Provincia') +
  labs(title = 'Salarios sectoriales respecto al de mayor remuneración en la provincia. Oct 2022',
       subtitle = 'Principales sectores en el empleo nacional',
       caption = 'Fuente: elaboración propia en base a CEPXXI') +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size=15,face='bold'),
        plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=12,face='bold'),
        axis.text.x = element_text(angle=45,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  + 
  guides(color=guide_legend(nrow=4,byrow=TRUE))
