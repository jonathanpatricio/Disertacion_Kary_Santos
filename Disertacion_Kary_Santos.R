#Librerías a utilizar
library(readxl)
library(lme4)
library(dplyr)
library(stargazer)
library(epiR)
library(DescTools)
library(table1)

#Leyendo la base de datos
BD <- read_excel("Cuadros de la Disertacion v1.2.xlsx")
BD <- BD %>% filter(M_Muerte_materna != 99)
BD <- BD[,90:ncol(BD)]

Datos_Enhogar <- read_excel("Datos Enhogar.xlsx")

#Unificando ambas bases
BD <- mutate(BD, key = paste(BD$M_Provincia_residencia, BD$M_Año_atención ,sep = "-"))
Datos_Enhogar <- mutate(Datos_Enhogar, key = paste(Datos_Enhogar$ID, Datos_Enhogar$Año ,sep = "-"))

Base <- left_join(x = BD, y = Datos_Enhogar, by = "key") 

BD <- Base


# Categorizando las variables
BD$M_Muerte_materna <- factor(BD$M_Muerte_materna, labels = c("No", "Si"))
BD$M_Embarazo_adolescante <- factor(BD$M_Embarazo_adolescante, labels = c("No", "Si"))
BD$M_Grupo_edad <- factor(BD$M_Grupo_edad)
BD$M_Embarazada <- factor(BD$M_Embarazada)
BD$M_Region_residencia <- factor(BD$M_Region_residencia); BD$M_Region_residencia <- relevel(BD$M_Region_residencia, ref = "O")
BD$M_Nacionalidad <- factor(BD$M_Nacionalidad)
# BD$Haitiana <- factor(BD$Haitiana, labels = c("No", "Si"))
# BD$M_Año_atención <- as.factor(BD$M_Año_atención); BD$M_Año_atención <- relevel(BD$M_Año_atención, ref = "2015")
BD$M_Tiempo_atención_Dic <- factor(BD$M_Tiempo_atención_Dic, labels = c("No", "Si"))
BD$M_Tiempo_atención_Dic_menor <- factor(if_else(condition = BD$M_Tiempo_atención < 1, true = 1, false = 0), labels = c("No", "Si"))
BD$M_Región_atención <- as.factor(BD$M_Región_atención); BD$M_Región_atención <- relevel(BD$M_Región_atención, ref = "O")
BD$M_Movilidad <- factor(BD$M_Movilidad, labels = c("No", "Si"))
BD$M_Tipo_atención <- factor(BD$M_Tipo_atención); BD$M_Tipo_atención <- relevel(BD$M_Tipo_atención, ref = "Internamiento")
BD$M_Comorbilidad <- factor(BD$M_Comorbilidad, labels = c("No", "Si"))
BD$M_COVID <- factor(BD$M_COVID, labels = c("No", "Si"))
BD$M_Época_pandemia <- factor(BD$M_Época_pandemia, labels = c("No", "Si"))


write.table(BD,"base.xlsx")

#Tabla para análisis exploratorio
table1( ~ M_Edad + M_Grupo_edad  + M_Afiliación_Seguro +
          M_Embarazada + M_Region_residencia + M_Nacionalidad + M_Tiempo_atención + 
          M_Tiempo_atención_Dic + M_Región_atención + M_Movilidad + BD$M_COVID +M_Comorbilidad +
          
          Analfabetismo_femenino + Jefatura_femenina + Grupo_socioeconómicco_muy_bajo +
          Grupo_socioeconómicco_muy_bajo_y_bajo + Población_Mujeres_urbana
        | M_Muerte_materna
          , 
        data = BD )



#Modelo
m_vacio <-  glmer(M_Muerte_materna ~ 1 + (1 | M_Municipio_residencia ),
             family = binomial, 
             data = BD)

summary(m_vacio, correlation = FALSE)


m6 <-  glmer(M_Muerte_materna ~ M_COVID + M_Grupo_edad + M_Embarazada + M_Region_residencia +
               M_Nacionalidad + M_Tiempo_atención_Dic_menor + M_Región_atención + M_Movilidad + 
               M_Tipo_atención + M_Comorbilidad + M_COVID + Analfabetismo_femenino + Jefatura_femenina + 
               Grupo_socioeconómicco_muy_bajo_y_bajo + Población_Mujeres_urbana + Año  + 
               (1 | M_Provincia_residencia ),
             family = binomial, 
             data = BD)
summary(m6, correlation = FALSE)

exp(m6@beta)


m8 <-  glmer(M_Muerte_materna ~ M_COVID + M_Grupo_edad + M_Embarazada + M_Region_residencia +
               M_Nacionalidad + M_Tiempo_atención_Dic_menor + M_Región_atención + M_Movilidad + 
               M_Tipo_atención + M_Comorbilidad + M_COVID + Analfabetismo_femenino + Jefatura_femenina + 
               Grupo_socioeconómicco_muy_bajo_y_bajo + Población_Mujeres_urbana + Año  + 
               (1 | M_Municipio_residencia ),
             family = binomial, 
             data = BD)

summary(m8, correlation = FALSE)

exp(m8@beta)




m7 <-  glm(M_Muerte_materna ~ M_COVID + M_Grupo_edad + M_Embarazada + M_Region_residencia +
               M_Nacionalidad + M_Tiempo_atención_Dic_menor + M_Región_atención + M_Movilidad + 
               M_Tipo_atención + M_Comorbilidad + M_COVID + Analfabetismo_femenino + Jefatura_femenina + 
               Grupo_socioeconómicco_muy_bajo_y_bajo + Población_Mujeres_urbana + Año ,
           family = binomial(link = "logit") , 
             data = BD)

summary(m7)
exp(m7$coefficients)

