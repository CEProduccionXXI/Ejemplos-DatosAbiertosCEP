# Cargar librerias
library(DatosAbiertosCEP)
library(tidyverse)

# Descargar base de puestos por provincia y letra. Empresas privadas y sin apertura de genero
puestos <- descarga_DA(index_base = 113)
# O bien: datos <- descarga_DA(tipo = "Puestos provincia y sector",genero = 'NO',universo = 'Privado',jurisdiccion = 'NO',sector = 'CLAE2')

# Filtro puestos con valor -99 (borrados por cuestiones de secreto estadístico)
puestos <- puestos %>% filter(puestos > 0)


# Descargar base de salario promedio por provincia y letra Empresas privadas y sin apertura de genero
salario <- descarga_DA(41)
# O bien: datos <- descarga_DA(tipo='Salario promedio',genero = 'NO',universo='Privado',jurisdiccion='NO',sector = 'CLAE2')
salario <- salario %>% filter(w_mean > 0)


# Proporcion de mujeres en el sector 
share_mujeres <- descarga_DA(129)
# O bien: datos <- descarga_DA(tipo = '% Muj por sector',genero = 'NO',universo = 'Privado',sector = 'CLAE2',jurisdiccion = 'NO')


# Joinear datos 
datos <- puestos %>% 
  left_join(salario) %>% 
  left_join(share_mujeres)

# Añadir la descripcion de los sectores 
datos <- diccionario_sectores(datos) 

# Llevar a precios constantes dic.2022
datos <- deflactar_DA(datos,'2022-12-01',variables_monetarias = 'w_mean') 

# Quedarse con ultimo dato disponible
datos <- datos %>% 
  filter(fecha == max(datos$fecha))

# Sacar sector NULL
datos <- datos %>% 
  filter(!is.na(letra))

# Graficar scatterplot: salario promedio y share de mujeres 
plot <- ggplot(datos,aes(share_mujer,w_mean_constante,color=letra_desc,size=puestos,label2=clae2_desc)) + 
  geom_point(alpha=0.5) + 
  scale_size(range = c(.1, 24), name="Empleados en el sector",guide = 'none') +
  ylab('Salario promedio') + 
  xlab('% de mujeres') +
  labs(title = 'Relación entre la proporción de mujeres y el salario promedio del sector. Oct 2022',
       caption = 'Fuente: elaboración propia en base a CEPXXI') +
  theme_bw() +
  theme(legend.position = 'none',
        plot.title = element_text(size=15,face='bold'),
        plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=12,face='bold'),
        axis.text.x = element_text(angle=45,hjust=1))  + 
  guides(color=guide_legend(nrow=6,byrow=TRUE))

plotly::ggplotly(plot)
