{# Librerías a utilizar
library(dplyr)
library(knitr)
}#Librerías a utilizar

{#Enhogar 2015
Enhogar2015 <- read_sav("PERSONAS_ENHOGAR_2015.sav")
  hogar2015 <- read_sav("HOGARES_ENHOGAR_2015.sav")
  
#Analfabetismo
Enhogar2015$analfabetismo <- if_else(condition = Enhogar2015$H209 == 2 & Enhogar2015$H203 == 2, true = 1, false = 0)
Enhogar2015 %>% filter(H203==2 & H204 > 9 & H204 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

#Jefatura de hogar femenina
Enhogar2015$Jefatura_femenina <- if_else(condition = Enhogar2015$H202 == 1 & Enhogar2015$H203 == 2, true = 1, false = 0)
Enhogar2015 %>% filter(H202 == 1 & H204 > 9 & H204 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Jefatura_femenina, na.rm = TRUE)) %>% 
  kable()

#Población urbaha
Enhogar2015$Urbana <- if_else(condition = Enhogar2015$HZONA == 1 & Enhogar2015$H203 == 2, true = 1, false = 0)
Enhogar2015 %>% filter(H203==2 & H204 > 9 & H204 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Urbana, na.rm = TRUE)) %>% 
  kable()


}#Enhogar 2015

{#Enhogar 2016
  #Analfabetismo
Enhogar2016 <- read_sav("Miembros_ENHOGAR2016_PUB.sav")
Enhogar2016$analfabetismo <- if_else(condition = Enhogar2016$H409 == 2 & Enhogar2016$H402 == 2, true = 1, false = 0)
Enhogar2016 %>% filter(H402 == 2 & H403 > 9 & H403 < 50) %>% group_by(HPROVIN) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

#Jefatura de hogar femenina
Enhogar2016$Jefatura_femenina <- if_else(condition = Enhogar2016$H405 == 1 & Enhogar2016$H402 == 2, true = 1, false = 0)
Enhogar2016 %>% filter(H405 == 1 & H403 > 9 & H403 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Jefatura_femenina, na.rm = TRUE)) %>% 
  kable()

#Población urbaha
Enhogar2016$Urbana <- if_else(condition = Enhogar2016$HZONA == 1 & Enhogar2016$H402 == 2, true = 1, false = 0)
Enhogar2016 %>% filter(H402 == 2 & H403 > 9 & H403 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Urbana, na.rm = TRUE)) %>% 
  kable()

}#Enhogar 2016

{#Enhogar 2017
  #analfabetismo
Enhogar2017 <- read_sav("ENH2017-PERSONAS-PUB.sav")
Enhogar2017$analfabetismo <- if_else(condition = Enhogar2017$H301 == 2 & Enhogar2017$H202 == 2, true = 1, false = 0)
Enhogar2017 %>% filter(H202 == 2 & H203 > 9 & H203 < 50) %>% group_by(HPROVIN) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

#Jefatura de hogar femenina
Enhogar2017$Jefatura_femenina <- if_else(condition = Enhogar2017$H205 == 1 & Enhogar2017$H202 == 2, true = 1, false = 0)
Enhogar2017 %>% filter(H205 ==1  & H203 > 9 & H203 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Jefatura_femenina, na.rm = TRUE)) %>% 
  kable()

#Población urbaha
Enhogar2017$Urbana <- if_else(condition = Enhogar2017$HZONA == 1 & Enhogar2017$H402 == 2, true = 1, false = 0)
Enhogar2017 %>% filter(H402 == 2 & H203 > 9 & H203 < 50) %>%  group_by(HPROVIN) %>% 
  summarise(mean(Urbana, na.rm = TRUE)) %>% 
  kable()

}#Enhogar 2017

{#Enhogar 2018
  #analfabetismo
Enhogar2018 <- read_sav("Personas_ENH18.sav")
  
Enhogar2018$analfabetismo <- if_else(condition = Enhogar2018$H301 == 2 & Enhogar2018$H202 == 2, true = 1, false = 0)
Enhogar2018 %>% filter(H202 == 2 & H203 > 9 & H203 < 50)  %>% group_by(HPROVI) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

#Jefatura de hogar femenina
Enhogar2018$Jefatura_femenina <- if_else(condition = Enhogar2018$H205 == 1 & Enhogar2018$H202 == 2, true = 1, false = 0)
Enhogar2018 %>% filter(H205 == 1 & H203 > 9 & H203 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(Jefatura_femenina, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar muy bajo
Enhogar2018$grupo_muy_bajo <- if_else(condition = Enhogar2018$grupsec == 1 & Enhogar2018$H202 == 2, true = 1, false = 0)
Enhogar2018 %>% filter(H203 > 9 & H203 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(grupo_muy_bajo, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar bajo
Enhogar2018$grupo_bajo <- if_else(condition = Enhogar2018$grupsec == 2  & Enhogar2018$H202 == 2, true = 1, false = 0)
Enhogar2018 %>% filter(H203 > 9 & H203 < 50) %>% group_by(HPROVI) %>% 
  summarise(mean(grupo_bajo, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar bajo y muy bajo
Enhogar2018$grupo_bajo_muy_bajo <- if_else(condition = Enhogar2018$grupo_bajo == 1  | Enhogar2018$grupo_muy_bajo == 1, true = 1, false = 0)
Enhogar2018 %>% filter(H203 > 9 & H203 < 50) %>% group_by(HPROVI) %>% 
  summarise(mean(grupo_bajo_muy_bajo, na.rm = TRUE)) %>% 
  kable()

#Población urbaha
Enhogar2018$Urbana <- if_else(condition = Enhogar2018$HZONA == 1 & Enhogar2018$H202 == 2, true = 1, false = 0)
Enhogar2018 %>% filter(H202 == 2 & H203 > 9 & H203 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(Urbana, na.rm = TRUE)) %>% 
  kable()


}#Enhogar 2018

{#Enhogar 2019
Enhogar2019 <- read_sav("ENHOGAR-MICS6-2019-PUB-MIEMBROS-HOGARES.sav")
Enhogar2019$analfabetismo <- if_else(condition = Enhogar2019$ED3A == 2 & Enhogar2019$HL4 == 2, true = 1, false = 0)
Enhogar2019 %>% group_by(HH7) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

}#Enhogar 2019

{#Enhogar 2021
  #analfabetismo
Enhogar2021 <- read_sav("ENHOGAR_BD_2021_PERSONAS.sav")
Enhogar2021$analfabetismo <- if_else(condition = Enhogar2021$H501 == 2 & Enhogar2021$H302 == 2, true = 1, false = 0)
Enhogar2021  %>% filter(H302 == 2 & H303 > 9 & H303 < 50) %>% group_by(HPROVI) %>% 
  summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
  kable()

#Jefatura de hogar femenina
Enhogar2021$Jefatura_femenina <- if_else(condition = Enhogar2021$H305 == 1 & Enhogar2021$H302 == 2, true = 1, false = 0)
Enhogar2021 %>% filter(H305 == 1 & H303 > 9 & H303 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(Jefatura_femenina, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar muy bajo
Enhogar2021$grupo_muy_bajo <- if_else(condition = Enhogar2021$grupo_socioecono == 1 & Enhogar2021$H302 == 2, true = 1, false = 0)
Enhogar2021 %>% filter(H302 == 2 & H303 > 9 & H303 < 50) %>% group_by(HPROVI) %>% 
  summarise(mean(grupo_muy_bajo, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar bajo
Enhogar2021$grupo_bajo <- if_else(condition = Enhogar2021$grupo_socioecono == 2  & Enhogar2021$H302 == 2, true = 1, false = 0)
Enhogar2021 %>% filter(H302 == 2 & H303 > 9 & H303 < 50) %>% group_by(HPROVI) %>% 
  summarise(mean(grupo_bajo, na.rm = TRUE)) %>% 
  kable()

#Grupo Socio económico familiar bajo y muy bajo
Enhogar2021$grupo_bajo_muy_bajo <- if_else(condition = Enhogar2021$grupo_bajo == 1  | Enhogar2021$grupo_muy_bajo == 1, true = 1, false = 0)
Enhogar2021 %>% filter(H302 == 2 & H303 > 9 & H303 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(grupo_bajo_muy_bajo, na.rm = TRUE)) %>% 
  kable()

#Población urbaha
Enhogar2021$Urbana <- if_else(condition = Enhogar2021$HZONA == 1 & Enhogar2021$H302 == 2, true = 1, false = 0)
Enhogar2021 %>% filter(H302 == 2 & H303 > 9 & H303 < 50) %>%  group_by(HPROVI) %>% 
  summarise(mean(Urbana, na.rm = TRUE)) %>% 
  kable()

}#Enhogar 2021

{#ENA 2019
  ENA2019 <- read_sav("ENA2019_MIEMBROS.sav")
  ENA2019$analfabetismo <- if_else(condition = ENA2019$P301_LEE_ESCRIBE_REF == 2 & ENA2019$P204_SEXO == 2, true = 1, false = 0)
  ENA2019 %>% group_by(PROVINCIA) %>% 
    summarise(mean(analfabetismo, na.rm = TRUE)) %>% 
    kable()
  
}#ENA 2019





Enhogar2021 %>% group_by(H501) %>% 
  summarise(n()) 

