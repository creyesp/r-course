library('dplyr')

path <- '~/Downloads/dataset_preprocessed.csv'
data <- read.csv(path, stringsAsFactors=FALSE)
glimpse(data)
dim(data)

#
drop_out <- c("acepta_permuta", "altura_permitida", "ambientes", "ambientes_extra", "aptos_por_piso", "banos_extra", "casco", "descripcion", "direccion",  "dormitorios_extra", "extra", "financia", "garajes_extra", "gastos_comunes_moneda", "hectareas", "huespedes", "inmobiliaria", "oficina", "penthouse", "plantas_extra", "precio_moneda", "referencia", "sobre", "tipo_de_publicacion", "titulo_publicacion", "url")

int_feature <- c("ano_de_construccion", "banos", "cantidad_de_pisos", "distancia_al_mar", "dormitorios", "garajes", "vista_al_mar", "vivienda_social", "piso", "plantas",  "precio")
float_feature <- c("gastos_comunes", "longitud_frente", "m2_de_la_terraza", "m2_del_terreno", "m2_edificados")
cat_feature <- c("barrio", "disposicion", "estado",  "tipo_propiedad")  

glimpse(data[, cat_feature])
glimpse(data[, float_feature])
glimpse(data[, int_feature])

head(lapply(data[, int_feature], as.integer))
head(lapply(data[, float_feature], as.numeric))

# Missing values
na_col <- colnames(data)[apply(data, 2, anyNA)]
na_col
colnames(select(data, -na_col))

data[, na_col]
#  
data %>% select_if(is.numeric) %>% summary()

# 
house_data <- data %>%  
  filter(tipo_propiedad == "casas" | tipo_propiedad == "apartamentos" )
house_data

data %>% select_if(., is.numeric) %>% colnames()
