
library(readxl)
library(ggplot2)
library(dplyr)

Data11 <- read_xlsx("C:/Users/josec/Downloads/DataPrueba11.xlsx")

#-------------------------------------------------------------------------
# Obs. 1 : "geom_bar(aes(...), stat = “identity”)" es igual a "geom_col"
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Obs. 2 : 
# Si "mapping = aes(x = Periodos, y = Total)" va al inicio del ggplot
# Entonces se aplica esa misma logíca los demás elementos, ya sea
# geom_col(), geom_bar(), geom_line(), geom_point(), etc
# 
# Si "mapping = aes(x = Periodos, y = Total)" va dentro de un elemento en 
# particular, entonces cada vez que se ingrese otro elemento, se debe mencionar 
# nuevamente esos mismo parámetros
# geom_bar("mapping = aes(x = Periodos, y = Total)")
# geom_col("mapping = aes(x = Periodos, y = Total)")
# geom_point("mapping = aes(x = Periodos, y = Total)"), etc...
# 
# Poner el mapping en cada elemento ayuda a incluir diferentes estructuras 
# al gráfico 
#-------------------------------------------------------------------------

######################
# 1. Caso (Simple)
######################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf11 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  )

######################
# 2. Caso (Girar)
######################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf12 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  coord_flip()

##############################################
# 3. Caso (Cambiar color de borde y relleno)
##############################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf13 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
    , color = "blue"
    , fill = "green"
  ) 

##############################################
# 4. Caso (Cambiar grosor barras)
##############################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf14 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
    , width = .5 # default: 0.9
  ) 

##############################################
# 5. Caso (Agregar etiquetas - Arriba)
##############################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf15 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  geom_text(
    mapping = aes(x = Periodos, y = Total, label = Total)
    , vjust = -1.6
    , color = "red"
    , size = 2
  )

##############################################
# 6. Caso (Agregar etiquetas - Abajo)
##############################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf16 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  geom_text(
    mapping = aes(x = Periodos, y = Total, label = Total)
    , vjust = 1.6
    , color = "red"
    , size = 2
  )

##############################################
# 7. Caso (Establecer rango para el eje y)
##############################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf17 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  scale_y_continuous(
    limits = c(0, 300)
  )

#####################################################################
# 8. Caso (Establecer rango para el eje y - En caso se gira el eje)
#####################################################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre)
# View(Data12)

Graf18 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  coord_flip() + # Se sigue manteniendo el: scale_y_continuous, en caso se giren las coordenadas
  scale_y_continuous(
    limits = c(0, 300)
  )

#####################################################################
# 9. Caso (Mostrar todos los niveles - Se tiene que convertir x en factor)
#####################################################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre) %>% 
  mutate(Periodos = factor(Periodos)) 
# View(Data12)

Graf19 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  )

###########################################################
# 10. Caso (Incluir un estilo de imagen: theme_minimal() - theme_classic())
###########################################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre) %>% 
  mutate(Periodos = factor(Periodos)) 
# View(Data12)

Graf20 <-  Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total) 
    , stat = "identity"
  ) +
  theme_classic()


###########################################################
# 11. Caso (Cambiar el color de las barras)
# - Se debe crear una columna con los colores
# - Incluir en el mapping como fill
# - Agregar el elemento: scale_fill_identity
###########################################################
Data12 <- Data11 %>% 
  filter(MesNombre %in% c("Enero")) %>% 
  mutate(Periodos = 1:nrow(.), .before = MesNombre) %>% 
  mutate(ColorBarras = case_when(
    Total <= 10 ~ "red",
    TRUE ~ "blue"
  ))
# View(Data12)

Graf21 <- Data12 %>% 
  ggplot(
    data = .
  ) +
  geom_bar(
    mapping = aes(x = Periodos, y = Total, fill = ColorBarras) 
    , stat = "identity"
  ) +
  scale_fill_identity()


#######################
# Juntar Imágenes
#######################
library(ggpubr)

DataUnida11 <- ggarrange(
  Graf11, Graf12
  , Graf13, Graf14
  , Graf15, Graf16
  #labels = ,
  , ncol = 2
  , nrow = 3
)

DataUnida12 <- ggarrange(
  Graf17, Graf18
  , Graf19, Graf20
  , Graf21
  #labels = ,
  , ncol = 2
  , nrow = 3
)


tiff("C:/Users/josec/Downloads/Data1Unida11.tiff", units="in", width=9, height=10, res=300)
DataUnida11
dev.off()

tiff("C:/Users/josec/Downloads/Data1Unida12.tiff", units="in", width=9, height=10, res=300)
DataUnida12
dev.off()



