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

#Calcular masa salarial
datos <- datos %>% 
  mutate(Masa_salarial = puestos * w_mean_constante)

# Importancia relativa del sector en la masa salarial provincial
datos <- datos %>% 
  group_by(fecha,zona_prov) %>% 
  mutate(Proporcion_masa = Masa_salarial / sum(Masa_salarial))

# Graficar sector mas importante por provincia en Octubre 2022
datos %>% 
  ungroup() %>% 
  filter(fecha == '2022-10-01') %>% 
  group_by(zona_prov) %>% 
  arrange(desc(Proporcion_masa)) %>% 
  filter(row_number()==1) %>% 
  ggplot(aes(Proporcion_masa*100,zona_prov)) +
  geom_col(aes(fill=letra_desc)) + 
  theme_bw() +
  labs(title='Sector que explica la mayor parte de la masa salarial provincial. Octubre 2022',
       subtitle='Precios constantes a Dic. 2022',
       caption = 'Fuente: elaboración propia en base a CEPXXI') + 
  xlab('% de la masa salarial total') + 
  ylab('Provincia') +
  theme(legend.position = 'bottom',
        plot.caption = element_text(size=12)) + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

