###Fecha: 1 de septiembre de 2025
##Script final con el cálculo de modelos de crecimiento y diferentes figuras, a partir de 3 dataframes con los datos tratados.
##Especie: Emys orbicularis (galápago europeo)
###Datos: 3 dataframes con los datos tratados. 

##Cargar packages
library(dplyr)
library(ggplot2)
library(readxl)
library(magrittr)
library(intrval)
library(stringr)
library(readxl)
library(readr)
library(purrr)
library(minpack.lm)
library(DT)
library(tidyr)
library(forcats)
library(patchwork)
library(gt)
library(tibble)
library(glue)

##Adjudicamos directorio
setwd('/Users/laiamontesocardona/Desktop/TFM')

##Cargamos datos
datos_2018 <- read.csv(
  '/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/script + datos 28.07/TFM_datos2018_definitivos.csv',
  sep = ";"
)
sum(!is.na(datos_2018$NUMERO))
str(datos_2018)
datos_2018$PES <- as.numeric(datos_2018$PES)
datos_2018$L_PL <- as.numeric(datos_2018$L_PL)

datos_2019 <- read.csv(
  '/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/script + datos 28.07/TFM_datos2019_definitivos.csv',
  sep = ";"
)
str(datos_2019)
sum(!is.na(datos_2019$NUMERO))

datos_2020 <- read.csv(
  '/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/script + datos 28.07/TFM_datos2020_definitivos.csv',
  sep = ";"
)
sum(!is.na(datos_2020$NUMERO))

str(datos_2020)
datos_2020$NUMERO <- as.integer(datos_2020$NUMERO)


###Juntamos los tres archivos
data_total <- bind_rows(datos_2018, datos_2019, datos_2020)
names(data_total)<-c("ID_ind","birth",
                        "date_birth_julian",
                        "origin" ,
                        "birth_captive",
                        "birth_tancat",
                        "cohort",
                        "date_observation" ,
                        "date_observation_julian",
                        "year",
                        "W",
                        "L_C",
                        "A_C","L_P","H_C",
                        "stage" ,
                        "ID_aquarium",
                        "ID_tupper",
                        "ID_deposit",
                        "ID_outside_Tancat"   ,
                        "ID_acclimatation_cage",
                        "ID_release",
                        "sex", "number",
                        "location_malf" ,
                        "problem","condition","age"  )

sum(!is.na(data_total$number))
str(data_total)
data_total$stage <- as.factor(data_total$stage)
levels(data_total$stage)

##GUARDAMOS EL DATAFRAME FINAL
write.table(data_total, "TFM_datos_definitivos_numero_suelta.csv", sep = ";", row.names = FALSE)

data <- read.csv('/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/DADES/DADES MEVES/DATOS DEFINITIVOS/TFM_datos_definitivos.csv', sep = ";")

data_captive <- read.csv('/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/script + datos 28.07/TFM_datos_definitivos_numero_suelta.csv', sep = ";")

##Transformamos variables
str(data_captive)
data_captive$ID_ind<-as.factor(data_captive$ID_ind)
data_captive$birth<- as.Date(data_captive$birth)
data_captive$origin <-as.factor(data_captive$origin)            
data_captive$birth_captive <-as.factor(data_captive$birth_captive)
data_captive$birth_tancat<-as.factor(data_captive$birth_tancat)
data_captive$cohort<-as.factor(data_captive$cohort)
data_captive$date_observation<- as.Date(data_captive$date_observation)
data_captive$stage<-as.factor(data_captive$stage)         
data_captive$ID_aquarium<-as.factor(data_captive$ID_aquarium)      
data_captive$ID_tupper<-as.factor(data_captive$ID_tupper) 
data_captive$ID_deposit<-as.factor(data_captive$ID_deposit) 
data_captive$ID_outside_Tancat<-as.factor(data_captive$ID_outside_Tancat) 
data_captive$ID_acclimatation_cage<-as.factor(data_captive$ID_acclimatation_cage)
data_captive$ID_release<-as.factor(data_captive$ID_release)
data_captive$sex<-as.factor(data_captive$sex)
data_captive$location_malf<-as.factor(data_captive$location_malf)
data_captive$problem <-as.factor(data_captive$problem)
data_captive$condition <-as.factor(data_captive$condition)
data_captive$year_f<-as.factor(data_captive$year)
data_captive$number<-as.factor(data_captive$number)
levels(data_captive$sex)


data_captive$sex <- fct_recode(data_captive$sex,
                                 "Female" = "1",
                                 "Male" = "3",
                                 "Indeterminate" = "2")

data_llibertat <- read_excel('/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/script + datos 28.07/Captures llibertat.xlsx')
data_llibertat <- data_llibertat %>% select(1,2,6,7,8,9,10,11,12,14,15)
names(data_llibertat)
names(data_llibertat) <-c("place", "date_observation", "sex","number","W","L_C","L_P","A_C", "H_C","observations","year")
str(data_llibertat)
data_llibertat$place <-as.factor(data_llibertat$place)
data_llibertat$sex <-as.factor(data_llibertat$sex)
data_llibertat$number <-as.factor(data_llibertat$number)
data_llibertat$observations <-as.factor(data_llibertat$observations)
data_llibertat$year <-as.integer(data_llibertat$year)
data_llibertat$date_observation <- as.Date((data_llibertat$date_observation), origin = "1899-12-30")
data_llibertat$wildcaptures <- "yes"
str(data_llibertat)
levels(data_llibertat$sex)
data_llibertat$wildcaptures <- as.factor(data_llibertat$wildcaptures)
data_llibertat$sex <- fct_recode(data_llibertat$sex,
                        "Female" = "2",
                        "Male" = "1",
                        "Indeterminate" = "3")

levels(data_llibertat$sex)

names(data_captive)
names(data_llibertat)
levels(data_llibertat$wildcaptures)


data <- dplyr::bind_rows(data_captive, data_llibertat)
data <- data %>%
  rename(capture_place = place)
data <- data %>%
  arrange(ID_ind)

##Hay algunos códigos "number" mal apuntados, los corregimos a como son en realidad
#ID 2018079 -> 1348
#ID 2018137 -> 1365
#ID 2018128 -> 1362
#ID 2018172 -> 1359
#ID 2018197 -> 1568
#ID 2018152 -> 1513

levels(data$wildcaptures)
data <- data %>%
  mutate(number = as.numeric(as.character(number))) %>%  # convertir factor a numérico
  mutate(number = case_when(
    ID_ind == "2018079" ~ 1348,
    ID_ind == "2018137" ~ 1365,
    ID_ind == "2018128" ~ 1362,
    ID_ind == "2018172" ~ 1359,
    ID_ind == "2018197" ~ 1568,
    ID_ind == "2018152" ~ 1513,
    TRUE ~ number
  ))
data %>%
  filter(number == 1568, is.na(ID_ind))


data_junto <- data %>%
  mutate(ID_ind = as.character(ID_ind)) %>%
  group_by(number) %>%
  mutate(ID_ind = ifelse(
    is.na(ID_ind),
    first(na.omit(ID_ind)),
    ID_ind
  )) %>%
  ungroup()

##Arreglamos fechas y ordenamos
data_junto <- data_junto %>%
  arrange(ID_ind)
data_junto <- data_junto %>%
  mutate(ID_ind = as.character(ID_ind)) %>%
  group_by(ID_ind) %>%
  fill(birth, date_birth_julian, origin, birth_captive, birth_tancat, cohort,
       .direction = "downup") %>%
  ungroup()
data_junto <- data_junto %>%
  mutate(
    date_observation_julian = ifelse(
      is.na(date_observation_julian) & !is.na(date_observation),
      as.numeric(format(date_observation, "%j")),  
      date_observation_julian
    )
  )

data_junto <- data_junto %>%
  mutate(
    age = ifelse(
      is.na(age) & !is.na(date_observation) & !is.na(birth),
      as.numeric(date_observation - birth),
      age
    )
  )

levels(data_junto$stage)

##Guardamos dataframe ordenado final
write.table(data_junto, "TFM_datos_definitivos_cautividad_libertad.csv", sep = ";", row.names = FALSE)

##Borramos dataframes que no vamos a utilizar
rm(data,data_junto)

##Cargamos dataframe final
data <- read.csv('/Users/laiamontesocardona/Desktop/TFM/ANÁLISIS/versiones definitivas/ara si/TFM_datos_definitivos_cautividad_libertad.csv', sep = ";")
str(data)
##Transformamos variables
data$ID_ind <- as.factor(data$ID_ind)
data$birth <- as.Date(data$birth)
data$origin <- as.factor(data$origin)
data$birth_captive <- as.factor(data$birth_captive)
data$birth_tancat <- as.factor(data$birth_tancat)
data$cohort <- as.factor(data$cohort)
data$date_observation <- as.Date(data$date_observation)
data$stage <- as.factor(data$stage)
data$ID_aquarium<-as.factor(data$ID_aquarium)      
data$ID_tupper<-as.factor(data$ID_tupper) 
data$ID_deposit<-as.factor(data$ID_deposit) 
data$ID_outside_Tancat<-as.factor(data$ID_outside_Tancat) 
data$ID_acclimatation_cage<-as.factor(data$ID_acclimatation_cage)
data$ID_release<-as.factor(data$ID_release)
data$sex<-as.factor(data$sex)
data$location_malf<-as.factor(data$location_malf)
data$problem <-as.factor(data$problem)
data$condition <-as.factor(data$condition)
data$number <-as.factor(data$number)
data$capture_place <-as.factor(data$capture_place)
data$observations <-as.factor(data$observations)
data$wildcaptures <-as.factor(data$wild)
levels(data$wildcaptures)
levels(data$sex)
levels(data$stage)


###QUITAMOS LOS PUNTOS CON ERROR DEL DATASET
##los puntos que vamos a quitar son los de medidas que no encajan de estos ID: 2018209, 2018085, 2018038, 2018009, 2019190
data <- data %>%
  mutate(W = ifelse(
    ID_ind %in% c(2018209, 2018085, 2018038, 2018009, 2019190) &
      age <= 750 & W > 50 & age < 150,
    NA, W
  ))
data <- data %>%
  mutate(
    W = if_else(
      (ID_ind == 2019164 & age == 1267 & L_C < 200) |
        (ID_ind == 2020056 & age == 1338 & L_C > 400) |
        (ID_ind == 2020157 & age == 1381 & L_C > 400),
      NA_real_,
      W
    )
  )


data <- data %>%
  mutate(
    L_C = case_when(
      ID_ind == 2018164 & age >= 375 & age <= 600 & L_C < 23 ~ NA,
      ID_ind == 2018009 & age < 250 & L_C > 75 ~ NA,
      ID_ind == 2018213 & age > 975 & L_C < 60 ~ NA,
      ID_ind == 2020116 & age > 1000 & L_C > 120 ~ NA,
      ID_ind == 2018209 & age == 145 & L_C > 68 ~ NA,
      ID_ind == 2019164 & age == 1267 & L_C > 100 ~ NA,
      ID_ind == 2020056 & age == 1338 & L_C > 140 ~ NA,
      ID_ind == 2020157 & age == 1381 & L_C > 140 ~ NA,
      ID_ind == 2020073 & age == 1399 & L_C > 110 ~ NA,
      ID_ind %in% c(2018038, 2018085, 2018209) & age < 150 & L_C > 67 ~ NA,
      TRUE ~ L_C
    )
  )

data <- data %>%
  mutate(
    A_C = case_when(
      ID_ind == 2018009 & age < 150 & A_C > 60 ~ NA,
      ID_ind == 2018038 & age < 150 & A_C > 60 ~ NA,
      ID_ind == 2018085 & age < 150 & A_C > 60 ~ NA,
      ID_ind == 2018003 & age < 990 & A_C < 40 ~ NA,
      ID_ind == 2020002 & age == 1369 & A_C > 100 ~ NA,
      ID_ind == 2020056 & age == 1338 & A_C > 100 ~ NA,
      ID_ind == 2020157 & age == 1381 & A_C > 100 ~ NA,
      ID_ind == 2019164 & age == 1267 & A_C < 100 ~ NA,
      ID_ind == 2018213 & age == 1001 & A_C < 50 ~ NA,
      TRUE ~ A_C
    )
  )

data <- data %>%
  mutate(
    L_P = case_when(
      ID_ind == 2018009 & L_P > 65 & age < 250 ~ NA,
      ID_ind == 2018038 & L_P > 65 & age < 250 ~ NA,
      ID_ind == 2018085 & L_P > 65 & age < 250 ~ NA,
      ID_ind == 2018209 & L_P > 60 & age < 250 ~ NA,
      ID_ind == 2018005 & age < 990 & L_P < 40 ~ NA,
      ID_ind == 2018213 & age > 990 & L_P < 50 ~ NA,
      TRUE ~ L_P
    )
  )     
  
data <- data %>%
  mutate(
    H_C = case_when(
      ID_ind == 2018009 & H_C > 27 & age < 250 ~ NA,
      ID_ind == 2018038 & H_C > 27 & age < 250 ~ NA,
      ID_ind == 2018085 & H_C > 27 & age < 250 ~ NA,
      ID_ind == 2018209 & H_C > 27 & age < 250 ~ NA,
      ID_ind == 2019066 & H_C > 60 & age < 500 ~ NA,
      ID_ind == 2018192 & H_C > 50 & age < 500 ~ NA,
      ID_ind == 2019122 & H_C > 50 & age < 500 ~ NA,
      ID_ind == 2018217 & H_C > 70 & age < 750 ~ NA,
      ID_ind == 2020116 & H_C > 77 & age < 1500 ~ NA,
      TRUE ~ H_C
    )
  )  

data <- data %>%
  arrange(ID_ind) %>%  # Ordenar por ID_ind
  group_by(ID_ind) %>%
  filter(!(ID_ind == 2019190 & row_number() == 1)) %>%  # Eliminar primera fila con ese ID
  ungroup()

data <- data %>%
  filter(!(ID_ind == 2018101 & W > 60 & age < 190))

data <- data %>%
  mutate(ID_ind = as.factor(ID_ind))


##GRAFICO LINEAS PUNTOS INDIVIDUALES, CAUTIVIDAD Y LIBERTAD EN COLOR ROJO PARA VER EL CRECIMIENTO
###PESO
data %>%
  filter(!is.na(age), !is.na(W)) %>%
  arrange(ID_ind, age) %>%
  ggplot(aes(x = age, y = W, group = ID_ind, color = ID_ind)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_point(data = . %>% filter(wildcaptures == "yes"),
             aes(x = age, y = W),
             color = "red3", size = 1) +
  facet_grid(rows = vars(cohort), cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Age (days)", y = "Weight (g)")

###L_C
data %>%
  filter(!is.na(age), !is.na(L_C)) %>%
  arrange(ID_ind, age) %>%
  ggplot(aes(x = age, y = L_C, group = ID_ind, color = ID_ind)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  # Puntos con wildcaptures == "yes" en rojo
  geom_point(
    data = data %>% filter(wildcaptures == "yes", !is.na(age), !is.na(L_C)),
    aes(x = age, y = L_C, group = ID_ind),
    color = "red3",
    size = 1
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Edad (días)", y = "Carapace Length (mm)")

####A_C
data %>%
  filter(!is.na(age), !is.na(A_C)) %>%
  arrange(ID_ind, age) %>%
  ggplot(aes(x = age, y = A_C, group = ID_ind, color = ID_ind)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_point(
    data = data %>% filter(wildcaptures == "yes", !is.na(age), !is.na(A_C)),
    aes(x = age, y = A_C, group = ID_ind),
    color = "red3",
    size = 1
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Age (days)", y = "Carapace width (mm)")

####L_P
data %>%
  filter(!is.na(age), !is.na(L_P)) %>%
  arrange(ID_ind, age) %>%
  ggplot(aes(x = age, y = L_P, group = ID_ind, color = ID_ind)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_point(
    data = data %>% filter(wildcaptures == "yes", !is.na(age), !is.na(L_P)),
    aes(x = age, y = L_P, group = ID_ind),
    color = "red3",
    size = 1
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Age (days)", y = "Plastron length (mm)")

####H_C
data %>%
  filter(!is.na(age), !is.na(H_C)) %>%
  arrange(ID_ind, age) %>%
  ggplot(aes(x = age, y = H_C, group = ID_ind, color = ID_ind)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  geom_point(
    data = data %>% filter(wildcaptures == "yes", !is.na(age), !is.na(H_C)),
    aes(x = age, y = H_C, group = ID_ind),
    color = "red3",
    size = 1
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Age (days)", y = "Carapace height (mm)")

sex_labeller<-c("Female" = "Hembras",
                "Indeterminate" = "Indeterminados",
                "Male" = "Machos")
captive_labeller<-c("Cautividad" = "Captive",
                    "Libertad" = "Freedom")
cohort_labeller<-c("2018/2019" = "2018",
                   "2019/2020" = "2019",
                   "2020/2021" = "2020")

##Creamos un dataframe solo con los dos sexos, sin tener en cuenta los individuos que no están sexados
data_sex<-subset(data, data$sex%in%c("Female","Male"))
data_sex$sex<-droplevels(data_sex$sex)
table(data_sex$sex)

data_sex %>%
  filter(is.na(wildcaptures)) %>%           
  count(cohort, sex)
levels(data$sex)

###Calculamos tasa de crecimiento
data$L_C
head(data)
names(data)
data[,c(12:15)]

data$age_years <- data$age / 365

data_growth <- data %>%
  arrange(ID_ind, age_years) %>%
  group_by(ID_ind) %>%
  mutate(
    diff_age = age_years - lag(age_years),
    diff_L_C = L_C - lag(L_C),
    growth_rate = ifelse(!is.na(diff_age) & diff_age > 0, diff_L_C / diff_age, NA),
    growth_rate_percent = ifelse(
      !is.na(diff_age) & diff_age > 0 & !is.na(lag(L_C)) & lag(L_C) > 0,
      (diff_L_C / diff_age) / lag(L_C) * 100,
      NA
    )
  ) %>%
  ungroup()

data_growth <- data_growth %>%
  mutate(growth_rate = ifelse(is.na(growth_rate), 0, growth_rate))

growth_by_sex <- data_growth %>%
  filter(!is.na(growth_rate) & is.finite(growth_rate)) %>%  
  group_by(sex) %>%
  summarise(
    mean_growth = mean(growth_rate),
    sd_growth = sd(growth_rate),
    n = n()
  )

growth_percent_by_sex <- data_growth %>%
  filter(!is.na(growth_rate_percent), is.finite(growth_rate_percent)) %>%
  group_by(sex) %>%
  summarise(
    mean_growth_percent = mean(growth_rate_percent),
    sd_growth_percent = sd(growth_rate_percent),
    n = n()
  )

data <- data %>%
  arrange(ID_ind, age) %>%   
  group_by(ID_ind) %>%        
  mutate(
    Diff_age = age - lag(age),             
    Diff_L_C = L_C - lag(L_C),                 
    Rate_percent = (Diff_L_C / Diff_age) / lag(L_C) * 100  
  ) %>%
  ungroup()

##Ahora lo mismo pero en vez de por días, por años
data$age_years <- data$age / 365

data <- data %>%
  arrange(ID_ind, age_years) %>%   
  group_by(ID_ind) %>%        
  mutate(
    Diff_age_years = age_years - lag(age_years),             
    Diff_L_C_years = L_C - lag(L_C),                 
    Rate_percent_years = (Diff_L_C_years / Diff_age_years / lag(L_C)) * 100  
  ) %>%
  ungroup()
  group_by(ID_ind) %>%
  mutate(Rate_percent = ifelse(is.na(Rate_percent), 0, Rate_percent)) %>%
  ungroup()
data <- data %>%
  group_by(ID_ind) %>%
  mutate(Rate_percent_years = ifelse(is.na(Rate_percent_years), 0, Rate_percent_years)) %>%
  ungroup()

# Tasa de crecimiento anual promedio por sexo
growth_by_sex <- data_sex %>%
  group_by(sex) %>%
  summarise(
    Mean_growth_rate = mean(Rate_percent_years, na.rm = TRUE),
    SD_growth_rate = sd(Rate_percent_years, na.rm = TRUE),
    N = n()
  )
data <- data %>%
  arrange(ID_ind, age_years) %>%
  group_by(ID_ind) %>%
  mutate(
    Diff_age_years = age_years - lag(age_years),
    Diff_L_C = L_C - lag(L_C),
    Rate_percent_years = ifelse(
      !is.na(Diff_age_years) & !is.na(lag(L_C)) & Diff_age_years > 0 & lag(L_C) > 0,
      (Diff_L_C / Diff_age_years / lag(L_C)) * 100,
      0
    )
  ) %>%
  ungroup()

###Figuras combinanado variables
ggplot(data_sex, aes(x = age_years, y = Rate_percent_years)) +
  geom_point(aes(color = sex), alpha = 0.6, show.legend = FALSE) +
  geom_point(
    data = data_sex %>% filter(wildcaptures == "yes" & sex == "Male"),
    aes(color = "Machos (recapturas libertad)"),
    size = 2,
    alpha = 0.6,
    show.legend = TRUE
  ) +
  geom_point(
    data = data_sex %>% filter(wildcaptures == "yes" & sex == "Female"),
    aes(color = "Hembras (recapturas libertad)"),
    size = 2,
    alpha = 0.6,
    show.legend = TRUE
  ) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, color = "maroon3") +
  coord_cartesian(ylim = c(-200, 500)) +
  labs(
    title = "Tasa de crecimiento por edad y sexo",
    x = "Edad (años)",
    y = "Tasa crecimiento (mm por unidad de tiempo)",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Male" = "gold3",
      "Female" = "skyblue",
      "Hembras (recapturas libertad)" = "skyblue4",
      "Machos (recapturas libertad)" = "gold4"
    ),
    labels = c(
      "Hembras (recapturas libertad)" = "Hembras (recapturas libertad)",
      "Machos (recapturas libertad)" = "Machos (recapturas libertad)"
    )) +
    
  facet_grid(cols = vars(sex), labeller = as_labeller(sex_labeller))
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  ) +
  facet_wrap(~ sex)

  ggplot(data_sex, aes(x = age_years, y = Rate_percent_years)) +
    geom_point(aes(color = sex), alpha = 0.6, show.legend = FALSE) +
    geom_point(
      data = data_sex %>% filter(wildcaptures == "yes" & sex == "Male"),
      aes(color = "Machos (recapturas libertad)"),
      size = 2,
      alpha = 0.6,
      show.legend = TRUE
    ) +
    geom_point(
      data = data_sex %>% filter(wildcaptures == "yes" & sex == "Female"),
      aes(color = "Hembras (recapturas libertad)"),
      size = 2,
      alpha = 0.6,
      show.legend = TRUE
    ) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, color = "maroon3") +
    coord_cartesian(ylim = c(-200, 500)) +
    labs(
      title = "Tasa de crecimiento por edad y sexo",
      x = "Edad (años)",
      y = "Tasa crecimiento (mm por unidad de tiempo)",
      color = ""
    ) +
    scale_color_manual(
      values = c(
        "Male" = "gold3",
        "Female" = "skyblue",
        "Hembras (recapturas libertad)" = "skyblue4",
        "Machos (recapturas libertad)" = "gold4"
      ),
      labels = c(
        "Hembras (recapturas libertad)" = "Hembras (recapturas libertad)",
        "Machos (recapturas libertad)" = "Machos (recapturas libertad)"
      )
    ) +
    facet_wrap(~ sex, labeller = as_labeller(sex_labeller)) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),       
      plot.background = element_blank(),        
      strip.background = element_blank(),       
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
############# FIGURAS COMBINANDO VARIABLES
#plot longitud a lo largo de la edad y sexos diferenciados
ggplot(data, aes(age,L_C )) +
  geom_point(aes(colour = factor(sex)))

data %>%
  filter(sex == "Ind" & is.na(cohort)) %>%
  print(width = Inf)
as.data.frame(
  data %>%
    filter(sex == "Ind" & is.na(cohort))
)
data <- data %>%
  filter(!(sex == "Ind" & is.na(cohort) & is.na(ID_ind))) #####las eliminamos porque han nacido en libertad

data <- data %>%
  filter(!is.na(cohort) & cohort != "")

ggplot(data, aes(W, L_C)) +
  geom_point(aes(colour = factor(sex))) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Indeterminate" = "deepskyblue4", "Male" = "midnightblue"),
    labels = c("Female", "Indeterminate", "Male")
  ) +
  facet_grid(
    rows = vars(cohort),
    cols = vars(sex),
    labeller = labeller(sex = sex_labeller, cohort = cohort_labeller)
  ) +
  theme_gray(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray95", colour = "grey50"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA)
  ) +
  xlab("Age (days)") + ylab("Carapace height (mm)")

##plots edad y anchura por cohorte y sexo
ggplot(data, aes(age,A_C )) +
  geom_point(aes(colour = factor(sex)))

ggplot(data, aes(W, A_C)) +
  geom_point(aes(colour = factor(sex))) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Indeterminate" = "deepskyblue4", "Male" = "midnightblue"),
    labels = c("Female", "Indeterminate", "Male")
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex),
             labeller = labeller(sex = sex_labeller, cohort = cohort_labeller)) +
  theme_gray(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray95", colour = "grey50"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA)
  ) +
  xlab("Age") + ylab("A_C")

##plots edad y altura por cohorte y sexo
ggplot(data, aes(age,H_C )) +
  geom_point(aes(colour = factor(sex)))

ggplot(data, aes(W, H_C)) +
  geom_point(aes(colour = factor(sex))) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Indeterminate" = "deepskyblue4", "Male" = "midnightblue"),
    labels = c("Female", "Indeterminate", "Male")
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex),
             labeller = labeller(sex = sex_labeller, cohort = cohort_labeller)) +
  theme_gray(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray95", colour = "grey50"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA)
  ) +
  xlab("Age") + ylab("H_C")

##plots peso y longitud por cohorte y sexo
ggplot(data, aes(W,L_C )) +
  geom_point(aes(colour = factor(sex)))

ggplot(data, aes(W, L_C)) +
  geom_point(aes(colour = factor(sex))) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Indeterminate" = "deepskyblue4", "Male" = "midnightblue"),
    labels = c("Female", "Indeterminate", "Male")
  ) +
  facet_grid(rows = vars(cohort), cols = vars(sex),
             labeller = labeller(sex = sex_labeller, cohort = cohort_labeller)) +
  theme_gray(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray95", colour = "grey50"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA)
  ) +
  xlab("Weight") + ylab("L_C")

getwd()

###Figuras solo con hembras y machos
# Figura 5a: Peso
plot1 <- ggplot(data_sex, aes(age, W)) +
  geom_point(aes(colour = factor(sex)), alpha = 0.6) +
  geom_point(data = data_females %>% filter(wildcaptures == "yes"),
             aes(x = age, y = W, color = "Hembras (recapturas libertad)"), alpha = 0.6) +
  geom_point(data = data_males %>% filter(wildcaptures == "yes"),
             aes(x = age, y = W, color = "Machos (recapturas libertad)"), alpha = 0.6) +
  scale_color_manual(name = "",
                     values = c(
                       "Female" = "skyblue",
                       "Male" = "gold3",
                       "Hembras (recapturas libertad)" = "skyblue4",
                       "Machos (recapturas libertad)" = "gold4"
                     ),
                     labels = c(
                       "Female" = "Hembras",
                       "Male" = "Machos",
                       "Hembras (recapturas libertad)" = "Hembras (recapturas libertad)",
                       "Machos (recapturas libertad)" = "Machos (recapturas libertad)"
                     )) +
  facet_grid(cols = vars(cohort), labeller = as_labeller(cohort_labeller)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    strip.background = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Edad (días)") + ylab("Peso (g)") +
  ggtitle("Relación Peso y Edad - Figura 6a")

# Figura 5b: Longitud
plot2 <- ggplot(data_sex, aes(age, L_C)) +
  geom_point(aes(colour = factor(sex)), alpha = 0.6) +
  geom_point(data = data_females %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Hembras (recapturas libertad)"), alpha = 0.6) +
  geom_point(data = data_males %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Machos (recapturas libertad)"), alpha = 0.6) +
  scale_color_manual(name = "",
                     values = c(
                       "Female" = "skyblue",
                       "Male" = "gold3",
                       "Hembras (recapturas libertad)" = "skyblue4",
                       "Machos (recapturas libertad)" = "gold4"
                     ),
                     labels = c(
                       "Female" = "Hembras",
                       "Male" = "Machos",
                       "Hembras (recapturas libertad)" = "Hembras (recapturas libertad)",
                       "Machos (recapturas libertad)" = "Machos (recapturas libertad)"
                     )) +
  facet_grid(cols = vars(cohort), labeller = as_labeller(cohort_labeller)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    strip.background = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Edad (días)") + ylab("Longitud del caparazón (mm)") +
  ggtitle("Relación Longitud del caparazón y Edad - Figura 5b")

# Combinar con leyenda unificada
(plot1 / plot2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) &
  guides(
    colour = guide_legend(override.aes = list(size = 3))
  )


##Figura procedencia, peso y edad
ggplot(data_sex, aes(age, W)) +
  geom_point(aes(colour = factor(birth_captive)), alpha = 0.5) +
  scale_color_manual(
    name = "Procedencia",
    values = c("Cautividad" = "peru", "Libertad" = "yellowgreen"),
    labels = c("Cautividad" = "Cautividad", "Libertad" = "Libertad")
  ) +
  facet_grid(cols = vars(sex), labeller = as_labeller(c("Female" = "Hembras", "Male" = "Machos"))) +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA)
  ) +
  xlab("Edad (días)") + ylab("Peso (g)")

###Edad vs Peso, facetado por cohorte (columnas) y nacimiento en cautividad (filas), coloreado por sexo
ggplot(data, aes(age, W)) +
  geom_point(aes(colour = factor(sex)), alpha = 0.6) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Indeterminate" = "deepskyblue4", "Male" = "midnightblue"),
    labels = c("Female" = "Female", "Indeterminate" = "Indeterminate", "Male" = "Male")
  ) +
  facet_grid(rows = vars(birth_captive), cols = vars(cohort),
             labeller = labeller(cohort = cohort_labeller,
                                 birth_captive = captive_labeller)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = NA, color = NA),
        panel.grid = element_line(colour = "grey90")) +
  xlab("Age (days)") +
  ylab("Weight (g)")


##Peso vs Longitud del caparazón (L_C), facetado por procedencia
ggplot(data_sex, aes(L_C, W)) +
  geom_point(aes(colour = factor(sex)), alpha = 0.6) +
  scale_color_manual(
    name = "Sex",
    values = c("Female" = "cyan4", "Male" = "midnightblue"),
    labels = c("Female" = "Female", "Male" = "Male")
  ) +
  facet_grid(cols = vars(birth_captive), labeller = as_labeller(captive_labeller)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        strip.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = NA, color = NA),
        panel.grid = element_line(colour = "grey90")) +
  xlab("Carapace Length (mm)") + ylab("Weight (g)")

ggplot(data_sex, aes(L_C, W)) + ###lo ponemos con capas porque sino no se ven los puntos de libertad
  # Capa 1: Captive
  geom_point(
    data = subset(data_sex, birth_captive == "Cautividad"),
    aes(colour = birth_captive),
    alpha = 0.5
  ) +
  # Capa 2: Freedom
  geom_point(
    data = subset(data_sex, birth_captive == "Libertad"),
    aes(colour = birth_captive),
    alpha = 0.5
  ) +
  scale_color_manual(
    name = "",
    values = c("Cautividad" = "peru", "Libertad" = "yellowgreen"),
    labels = c("Cautividad" = "Cautividad", "Libertad" = "Libertad")
  ) +
  facet_grid(
    cols = vars(sex),
    labeller = as_labeller(c("Female" = "Hembras", "Male" = "Machos"))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA, color = NA),
    panel.grid = element_line(colour = "grey90"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  ggtitle("Relación Peso y Longitud del caparazón") +
  xlab("Longitud del caparazón (mm)") +
  ylab("Peso (g)")

##estadisticas para grafico
summary_stats <- data_sex %>%
  group_by(sex, birth_captive) %>%
  summarise(
    n = n(),
    mean_weight = mean(W, na.rm = TRUE),
    median_weight = median(W, na.rm = TRUE),
    sd_weight = sd(W, na.rm = TRUE),
    min_weight = min(W, na.rm = TRUE),
    max_weight = max(W, na.rm = TRUE),
    mean_length = mean(L_C, na.rm = TRUE),
    median_length = median(L_C, na.rm = TRUE),
    sd_length = sd(L_C, na.rm = TRUE),
    min_length = min(L_C, na.rm = TRUE),
    max_length = max(L_C, na.rm = TRUE)
  )
print(summary_stats)


########################## MODELOS CRECIMIENTO
#primero separamos datasets por sexo
data_females <- data %>% filter(sex == "Female")
data_males   <- data %>% filter(sex == "Male")

#Modelos deifnidos
Mlogistic <- function(age, L_inf, k, t0) {
  L_inf / (1 + exp(-k * (age - t0)))
}
MvonBertalanffy <- function(age, L_inf, k, t0) {
  L_inf * (1 - exp(-k * (age - t0)))
}

MRichards <- function(age, L_inf, k, t0,m) {
  L_inf / ((1 + exp(-k *age))^(1/m))
}

Mseasonal <- function(age, L_inf, k, t0,C,ts) {
  L_inf * (1 - exp(-k * (age - t0)) + C * sin(2 * pi * (age - ts)))
}


data_sex$sex <- as.numeric(data_sex$sex) 
data_limpia <- data_sex[!is.na(data_sex$L_C) & !is.na(data_sex$W) & 
                        !is.na(data_sex$age) & !is.na(data_sex$sex), ] ## esto lo hacemos porque sino despues no corre bien, le quitamos los NA

## 2 Male, 1 Female

##PARA L_C
#LOG FEMALES utilizo el start value del inicio
#######para los start values, utilizaremos los máximos del rango establecido en el informe 2021 de Albert
## hembras: rango de 84.2 - 153.9
## machos: rango de 108.2 - 146.1
######## finalmente utilizamos el valor de 118 porque sino no corre bien. 
##este 118 lo sacamos de la medida 118,17

start_values <- list(L_inf = 118, k = 0.1, t0 = -1) 

model_log_females <- nls(L_C ~ Mlogistic(age, L_inf, k, t0),
                 data = data_females,
                 start = start_values,
                 control = nls.control(maxiter = 100))

predict(model_log_females)
model_log_females
start_values_log_females <- list(L_inf = 1.103e+02, k = 3.062e-03, t0 = 3.368e+02)
AIC_log_females <- AIC(model_log_females)
print(AIC_log_females) #53175.17

#LOG MALES
model_log_males <- nls(L_C ~ Mlogistic(age, L_inf, k, t0),
                         data = data_males,
                         start = start_values,
                         control = nls.control(maxiter = 100))

predict(model_log_males)
model_log_males
start_values_log_males <- list(L_inf = 1.094e+02, k = 3.938e-03, t0 = 2.787e+02)
AIC_log_males <- AIC(model_log_males)
print(AIC_log_males) #13275.97

#VB FEMALES (utilizo el start value sacado del modelo LOG FEMALE)
model_VB_females <- nls(L_C ~ MvonBertalanffy(age, L_inf, k, t0),
                data = data_females,
                start = start_values_log_females,
                control = nls.control(maxiter = 100))
predict(model_VB_females)
model_VB_females
AIC_VB_females <- AIC(model_VB_females)
print(AIC_VB_females) #53016.31

start_values_vb_females <- list(L_inf = 1.366e+02, k = 1.249e-03, t0 = -1.397e+02,m = 0 )

#VB MALES (utilizo el start value sacado del modelo LOG MALE)
model_VB_males <- nls(L_C ~ MvonBertalanffy(age, L_inf, k, t0),
                        data = data_males,
                        start = start_values_log_males,
                        control = nls.control(maxiter = 100))
predict(model_VB_males)
model_VB_males
AIC_VB_males <- AIC(model_VB_males)
print(AIC_VB_males) #13201.46

start_values_vb_males <- list(L_inf =  1.296e+02, k = 1.401e-03, t0 = -1.348e+02 ,m = 0 )


###Calculamos rango de edad
# Para hembras
age_seq_fem <- seq(min(data_females$age), max(data_females$age), length.out = 200)

# Para machos
age_seq_mal <- seq(min(data_males$age), max(data_males$age), length.out = 200)


# Predicciones Von Bertalanffy
preds_vb_fem <- data.frame( #FEMALES
  age = age_seq_fem,
  length = MvonBertalanffy(
    age_seq_fem,
    coef(model_VB_females)["L_inf"],
    coef(model_VB_females)["k"],
    coef(model_VB_females)["t0"]
  )
)


preds_vb_mal <- data.frame(  #MALES
  age = age_seq_mal,
  length = MvonBertalanffy(
    age_seq_mal,
    coef(model_VB_males)["L_inf"],
    coef(model_VB_males)["k"],
    coef(model_VB_males)["t0"]
  )
)


#Predicciones logistico
preds_log_fem <- data.frame( #FEMALES
  age = age_seq_fem, 
  length = Mlogistic(
    age_seq_fem,
    coef(model_log_females)["L_inf"],
    coef(model_log_females)["k"],
    coef(model_log_females)["t0"]
  )
)

preds_log_mal <- data.frame( #MALES
  age = age_seq_mal,
  length = Mlogistic(
    age_seq_mal,
    coef(model_log_males)["L_inf"],
    coef(model_log_males)["k"],
    coef(model_log_males)["t0"]
  )
)

##grafico modelo logistico conjunto
colores_ordenados <- c(
  "Hembras (cautividad)",
  "Hembras (recapturas libertad)",
  "Curva crecimiento hembras",
  "Machos (cautividad)",
  "Machos (recapturas libertad)",
  "Curva crecimiento machos"
)

##grafico modelo logistico conjunto
plot3 <- ggplot() +
  geom_point(data = data_females, 
             aes(x = age, y = L_C, color = "Hembras (cautividad)", alpha = 0.3)) + 
  geom_point(data = data_females %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Hembras (recapturas libertad)"), alpha = 0.6) + 
  geom_point(data = data_males, 
             aes(x = age, y = L_C, color = "Machos (cautividad)", alpha = 0.3)) +   
  geom_point(data = data_males %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Machos (recapturas libertad)"), alpha = 0.6) +   
  geom_line(data = preds_log_fem, 
            aes(x = age, y = length, color = "Curva crecimiento hembras"), size = 1) +
  geom_line(data = preds_log_mal, 
            aes(x = age, y = length, color = "Curva crecimiento machos"), size = 1) +
  labs(
    title = "Modelo Logístico - Figura 8a",
    x = "Edad (días)",
    y = "Longitud del caparazón (mm)",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Hembras (cautividad)" = "skyblue",
      "Hembras (recapturas libertad)" = "skyblue4",
      "Curva crecimiento hembras" = "skyblue1",
      "Machos (cautividad)" = "gold3",
      "Machos (recapturas libertad)" = "gold4",
      "Curva crecimiento machos" = "gold"
    ),
    breaks = colores_ordenados 
  ) +
  guides(alpha = "none") +  
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))


##grafico modelo von bertalanffy conjunto
plot4 <- ggplot() +
  geom_point(data = data_females, 
             aes(x = age, y = L_C, color = "Hembras (cautividad)", alpha = 0.3)) + 
  geom_point(data = data_females %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Hembras (recapturas libertad)"), alpha = 0.6) + 
  geom_point(data = data_males, 
             aes(x = age, y = L_C, color = "Machos (cautividad)", alpha = 0.3)) +   
  geom_point(data = data_males %>% filter(wildcaptures == "yes"),
             aes(x = age, y = L_C, color = "Machos (recapturas libertad)"), alpha = 0.6) +   
  geom_line(data = preds_vb_fem, 
            aes(x = age, y = length, color = "Curva crecimiento hembras"), size = 1) +
  geom_line(data = preds_vb_mal, 
            aes(x = age, y = length, color = "Curva crecimiento machos"), size = 1) +
  labs(
    title = "Modelo von Bertalanffy - Figura 8b",
    x = "Edad (días)",
    y = "Longitud del caparazón (mm)",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Hembras (cautividad)" = "skyblue",
      "Hembras (recapturas libertad)" = "skyblue4",
      "Curva crecimiento hembras" = "skyblue1",
      "Machos (cautividad)" = "gold3",
      "Machos (recapturas libertad)" = "gold4",
      "Curva crecimiento machos" = "gold"
    ),
    breaks = colores_ordenados  
  ) +
  guides(alpha = "none") +  
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))

(plot3 / plot4) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) &
  guides(
    colour = guide_legend(override.aes = list(size = 2))
  )


##GRAFICOS POR SEPARADO, CADA SEXO UN GRAFICO, NO CONJUNTAMENTE
# p1 <- ggplot(data_females, aes(x = age, y = L_C)) +
#   geom_point(alpha = 0.3, color = "grey") +
#   geom_point(data = data_females %>% filter(wildcaptures == "yes"),
#              aes(x = age, y = L_C),
#              color = "black", size = 1) +
#   geom_line(data = preds_vb_fem, aes(x = age, y = length), color = "red", size = 1) +
#   labs(
#     title = "Von Bertalanffy Model – Females",
#     x = "Age (days)",
#     y = "Carapace length (mm)"
#   ) +
#   theme_minimal()
# 
# p2 <- ggplot(data_males, aes(x = age, y = L_C)) +
#   geom_point(alpha = 0.3, color = "grey") +
#   geom_point(data = data_males %>% filter(wildcaptures == "yes"),
#              aes(x = age, y = L_C),
#              color = "black", size = 1) +
#   geom_line(data = preds_vb_mal, aes(x = age, y = length), color = "dodgerblue", size = 1) +
#   labs(
#     title = "Von Bertalanffy Model – Males",
#     x = "Age (days)",
#     y = "Carapace length (mm)"
#   ) +
#   theme_minimal()
# 
# p3 <- ggplot(data_females, aes(x = age, y = L_C)) +
#   geom_point(alpha = 0.3, color = "grey") +
#   geom_point(data = data_females %>% filter(wildcaptures == "yes"),
#              aes(x = age, y = L_C),
#              color = "black", size = 1) +
#   geom_line(data = preds_log_fem, aes(x = age, y = length), color = "red", size = 1) +
#   labs(
#     title = "Logistic Model – Females",
#     x = "Age (days)",
#     y = "Carapace length (mm)"
#   ) +
#   theme_minimal()
# 
# p4 <- ggplot(data_males, aes(x = age, y = L_C)) +
#   geom_point(alpha = 0.3, color = "grey") +
#   geom_point(data = data_males %>% filter(wildcaptures == "yes"),
#              aes(x = age, y = L_C),
#              color = "black", size = 1) +
#   geom_line(data = preds_log_mal, aes(x = age, y = length), color = "dodgerblue", size = 1) +
#   labs(
#     title = "Logistic Model – Males",
#     x = "Age (days)",
#     y = "Carapace length (mm)"
#   ) +
#   theme_minimal()
# 
# 
# ##gráfico conjunto de los diferentes gráficos de los dos modelos y sexos
# (p1 + p2) / (p3 + p4)

# Reordenamos columnas: referencias al final
growth_models_L_C <- growth_models_L_C %>%
  select(Modelo, Hembras, Machos, `Referencias del modelo`)

#Creamos tabla gt
gtable <- growth_models_L_C %>%
  gt() %>%
  tab_header(
    title = md("**Modelos de crecimiento en longitud del caparazón**")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 13,          
    data_row.padding = px(6),      
    heading.padding = px(8)
  ) %>%
  tab_style(
    style = cell_text(size = px(9)),
    locations = cells_body(columns = `Referencias del modelo`)
  ) %>%
  cols_width(
    Modelo ~ pct(35),
    Hembras ~ pct(25),
    Machos ~ pct(25),
    `Referencias del modelo` ~ pct(20)
  )

# Guardar como PDF
gtsave(gtable, filename = "growth_models_L_C.pdf")
format_model <- function(aic, params) {
  glue::glue(
    "**AIC:** {round(aic, 2)}  \n",
    "**Parámetros:**  \n",
    "$L_\\infty={params$L_inf}$  \n",
    "$k={params$k}$  \n",
    "$t_0={params$t0}$"
  )
}

growth_models_L_C <- tibble(
  Modelo = c("Logístico", "Von Bertalanffy"),
  Ecuación = c(
    "$L(t) = \\frac{L_\\infty}{1 + e^{-k(t - t_0)}}$",
    "$L(t) = L_\\infty (1 - e^{-k(t - t_0)})$"
  ),
  "Referencias del modelo"= c(
    "Maintainer, R., & Rodriguez, D. (2023)",
    "Ogle, D. (2013)"
  ),
  Hembras = c(
    format_model(AIC_log_females, start_values_log_females),
    format_model(AIC_VB_females, start_values_vb_females)
  ),
  Machos = c(
    format_model(AIC_log_males, start_values_log_males),
    format_model(AIC_VB_males, start_values_vb_males)
  )
)
growth_models_L_C <- growth_models_L_C %>%
  mutate(
    Modelo = glue("**{Modelo}**<br>{Ecuación}")
  ) %>%
  select(Modelo, "Referencias del modelo", Hembras, Machos)

gtable <- growth_models_L_C %>%
  gt() %>%
  tab_header(
    title = md("**Modelos de crecimiento en longitud del caparazón**")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(
    table.font.names = "Times New Roman, serif"
  )
gtsave(gtable, filename = "growth_models_L_C.png")

getwd()

###para W
#LOG FEMALES W
start_values_females_W <- list(L_inf = 900, k = 0.01, t0 = -0.5) #usamos el peso calculado 
L_inf <- max(data_females$W, na.rm = TRUE) #636

model_log_females_W <- nls(W ~ Mlogistic(age, L_inf, k, t0),
                         data = data_females,
                         start = start_values_females_W,
                         control = nls.control(maxiter = 200, minFactor = 0.0001))

predict(model_log_females_W)
model_log_females_W
start_values_log_females_W <- list(L_inf = 4.174e+02, k = 3.109e-03, t0 = 9.358e+02 )
AIC_log_females_W <- AIC(model_log_females_W)
print(AIC_log_females_W) 

#LOG MALES W
start_values_males_W <- list(L_inf = 900, k = 0.01, t0 = -0.5) #usamos el peso calculado 
model_log_males_W <- nls(W ~ Mlogistic(age, L_inf, k, t0),
                       data = data_males,
                       start = start_values_males_W,
                       control = nls.control(maxiter = 100))


predict(model_log_males_W)
model_log_males_W
start_values_log_males_W <- list(L_inf = 2.595e+02, k = 4.175e-03, t0 = 6.605e+02)
AIC_log_males_W <- AIC(model_log_males_W)
print(AIC_log_males_W) 

##########AQUI EMPIEZA ERROR
#VB FEMALES W
# model_VB_females_W <- nls(W ~ MvonBertalanffy(age, L_inf, k, t0),
#                           data = data_females,
#                           start = start_values_log_females_W,
#                           control = nls.control(maxiter = 200, minFactor = 0.0001))
# 
# predict(model_VB_females_W)
# model_VB_females_W
# AIC_VB_females_W <- AIC(model_VB_females_W)
# print(AIC_VB_females_W) #53016.31
# 
# start_values_VB_females_W <- list(L_inf = 1.366e+02, k = 1.249e-03, t0 = -1.397e+02,m = 0 )
# 
# #VB MALES W
# model_VB_males_W <- nls(W ~ MvonBertalanffy(age, L_inf, k, t0),
#                       data = data_males,
#                       start = start_values_W,
#                       control = nls.control(maxiter = 100))
# predict(model_VB_males_W)
# model_VB_males_W
# AIC_VB_males_W <- AIC(model_VB_males_W)
# print(AIC_VB_males_W) #13247.77
# 
# start_values_VB_males_W <- list(L_inf = 1.305e+02, k = 1.382e-03, t0 = -1.361e+02 ,m = 0 )


#Predicciones logistico
preds_log_fem_W <- data.frame( #FEMALES
  age = age_seq_fem,
  length = Mlogistic(
    age_seq_fem,
    coef(model_log_females_W)["L_inf"],
    coef(model_log_females_W)["k"],
    coef(model_log_females_W)["t0"]
  )
)

preds_log_mal_W <- data.frame( #MALES
  age = age_seq_mal,
  length = Mlogistic(
    age_seq_mal,
    coef(model_log_males_W)["L_inf"],
    coef(model_log_males_W)["k"],
    coef(model_log_males_W)["t0"]
  )
)

summary(model_log_males_W)

ggplot() +
  geom_point(data = data_females, 
             aes(x = age, y = W, color = "Hembras (cautividad)", alpha = 0.3)) + 
  geom_point(data = data_females %>% filter(wildcaptures == "yes"),
             aes(x = age, y = W, color = "Hembras (recapturas libertad)"), alpha = 0.6) + 
  geom_point(data = data_males, 
             aes(x = age, y = W, color = "Machos (cautividad)", alpha = 0.3)) +   
  geom_point(data = data_males %>% filter(wildcaptures == "yes"),
             aes(x = age, y = W, color = "Machos (recapturas libertad)"), alpha = 0.6) +   
  geom_line(data = preds_log_fem_W, 
            aes(x = age, y = length, color = "Curva crecimiento hembras"), size = 1) +
  geom_line(data = preds_log_mal_W, 
            aes(x = age, y = length, color = "Curva crecimiento machos"), size = 1) +
  labs(
    title = "Modelo Logístico",
    x = "Edad (días)",
    y = "Peso (g)",
    color = ""
  ) +
  scale_color_manual(values = c(
    "Hembras (cautividad)" = "skyblue",
    "Hembras (recapturas libertad)" = "skyblue4",
    "Curva crecimiento hembras" = "skyblue1",
    "Machos (cautividad)" = "gold3",
    "Machos (recapturas libertad)" = "gold4",
    "Curva crecimiento machos" = "gold"
  )) +
  guides(alpha = "none") +  
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",)

gtable <- growth_models_W %>%
  gt() %>%
  tab_header(
    title = md("**Modelo Logístico Peso**")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 13,        
    data_row.padding = px(4),    ¡
    heading.padding = px(6)
  ) %>%
  tab_style(
    style = cell_text(size = px(9)),   
    locations = cells_body(columns = `Referencias del modelo`)
  ) %>%
  cols_width(
    Modelo ~ pct(35),
    Hembras ~ pct(25),
    Machos ~ pct(25),
    `Referencias del modelo` ~ pct(20)
  )

# Guardar PDF
gtsave(gtable, filename = "growth_models_W.pdf")
getwd()

#####tabla, de momento solo logistico
format_model <- function(aic, params) {
  glue::glue(
    "**Parámetros:**  \n",
    "$L_\\infty={params$L_inf}$  \n",
    "$k={params$k}$  \n",
    "$t_0={params$t0}$"
  )
}

library(tibble)
library(glue)

growth_models_W <- tibble(
  Modelo = c("Logístico"),
  Ecuación = c(
    "$L(t) = \\frac{L_\\infty}{1 + e^{-k(t - t_0)}}$"
  ),
  Hembras = format_model(AIC_log_females_W, start_values_log_females_W),
  Machos = format_model(AIC_log_males_W, start_values_log_males_W),
  "Referencias del modelo" = "Maintainer, R., & Rodriguez, D. (2023)",
)


growth_models_W <- growth_models_W %>%
  mutate(
    Modelo = glue("**{Modelo}**<br>{Ecuación}")
  ) %>%
  select(Modelo, Hembras, Machos, "Referencias del modelo")

gtable <- growth_models_W %>%
  gt() %>%
  tab_header(
    title = md("**Modelo Logístico Peso**")
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(
    table.font.names = "Times New Roman, serif"
  )
gtsave(gtable, filename = "growth_models_W.png")
getwd()



### A PARTIR DE AQUÍ MODELOS PARA LAS BIOMETRÍAS QUE NO UTILIZAREMOS
###para A_C
#LOG FEMALES A_C
start_values_A_C <- list(L_inf = 123.22, k = 0.01, t0 = -0.5) #usamos el peso calculado 
L_inf <- max(data$A_C, na.rm = TRUE) #123.22

model_log_females_A_C <- nls(A_C ~ Mlogistic(age, L_inf, k, t0),
                           data = data_females,
                           start = start_values_A_C,
                           control = nls.control(maxiter = 100))

predict(model_log_females_A_C)
model_log_females_A_C
start_values_log_females_A_C <- list(L_inf = 8.946e+01, k = 4.138e-03, t0 = 2.328e+02)
AIC_log_females_A_C <- AIC(model_log_females_A_C)
print(AIC_log_females_A_C) #50350.23

#LOG MALES A_C
model_log_males_A_C <- nls(A_C ~ Mlogistic(age, L_inf, k, t0),
                         data = data_males,
                         start = start_values_A_C,  
                         control = nls.control(maxiter = 100))

predict(model_log_males_A_C)
model_log_males_A_C
start_values_log_males_A_C<- list(L_inf = 8.783e+01, k = 4.166e-03, t0 = 2.167e+02)
AIC_log_males_A_C <- AIC(model_log_males_A_C)
print(AIC_log_males_A_C) #12213.86

#VB FEMALES A_C
model_VB_females_A_C <- nls(A_C ~ MvonBertalanffy(age, L_inf, k, t0),
                          data = data_females,
                          start = start_values_log_females_A_C,
                          control = nls.control(maxiter = 200, minFactor = 0.0001))
predict(model_VB_females_A_C)
model_VB_females_A_C
AIC_VB_females_A_C <- AIC(model_VB_females_A_C)
print(AIC_VB_females_A_C) #50244.99

start_values_VB_females_A_C <- list(L_inf = 1.065e+02, k = 1.492e-03, t0 = -1.510e+02, m = 0 )

#VB MALES A_C
model_VB_males_A_C <- nls(A_C ~ MvonBertalanffy(age, L_inf, k, t0),
                        data = data_males,
                        start = start_values_log_males_A_C,
                        control = nls.control(maxiter = 100))
predict(model_VB_males_A_C)
model_VB_males_A_C
AIC_VB_males_A_C <- AIC(model_VB_males_A_C)
print(AIC_VB_males_A_C) #12169.87

start_values_VB_males_A_C <- list(L_inf = 9.878e+01, k = 1.723e-03, t0 = -1.442e+02, m = 0 )

format_model <- function(aic, params) {
  glue::glue(
    "**AIC:** {round(aic, 2)}  \n",
    "**Parameters:**  \n",
    "$L_\\infty={params$L_inf}$  \n",
    "$k={params$k}$  \n",
    "$t_0={params$t0}$"
  )
}

library(tibble)
library(glue)

growth_models_A_C <- tibble(
  Model = c("Logistic", "Von Bertalanffy"),
  Equation = c(
    "$L(t) = \\frac{L_\\infty}{1 + e^{-k(t - t_0)}}$",
    "$L(t) = L_\\infty (1 - e^{-k(t - t_0)})$"
  ),
  Females = c(
    format_model(AIC_log_females_A_C, start_values_log_females_A_C),
    format_model(AIC_VB_females_A_C, start_values_VB_females_A_C)
  ),
  Males = c(
    format_model(AIC_log_males_A_C, start_values_log_males_A_C),
    format_model(AIC_VB_males_A_C, start_values_VB_males_A_C)
  ),
  Reference = c(
    "Maintainer, R., & Rodriguez, D. (2023)",
    "Ogle, D. (2013)"
  )
)
gtable1 <- growth_models_A_C %>%
  gt() %>%
  tab_header(
    title = md("**Growth Models Summary A_C**")
  ) %>%
  fmt_markdown(columns = everything())
gtsave(gtable1, filename = "growth_models_A_C.pdf")


###para H_C
#LOG FEMALES H_C
start_values_H_C <- list(L_inf = 68.66, k = 0.01, t0 = -0.5) #usamos el peso calculado 
L_inf <- max(data$H_C, na.rm = TRUE) #68.66

model_log_females_H_C <- nls(H_C ~ Mlogistic(age, L_inf, k, t0),
                             data = data_females,
                             start = start_values_H_C,
                             control = nls.control(maxiter = 100))

predict(model_log_females_H_C)
model_log_females_H_C
start_values_log_females_H_C <- list(L_inf = 4.790e+01, k = 3.336e-03, t0 = 3.031e+02)
AIC_log_females_H_C <- AIC(model_log_females_H_C)
print(AIC_log_females_H_C) #40492.12

#LOG MALES H_C
model_log_males_H_C <- nls(H_C ~ Mlogistic(age, L_inf, k, t0),
                           data = data_males,
                           start = start_values_H_C,  
                           control = nls.control(maxiter = 100))

predict(model_log_males_H_C)
model_log_males_H_C
start_values_log_males_H_C<- list(L_inf = 4.277e+01, k = 3.733e-03, t0 = 2.262e+02)
AIC_log_males_H_C <- AIC(model_log_males_H_C)
print(AIC_log_males_H_C) #9571.157

#VB FEMALES H_C
model_VB_females_H_C <- nls(H_C ~ MvonBertalanffy(age, L_inf, k, t0),
                            data = data_females,
                            start = start_values_log_females_H_C,
                            control = nls.control(maxiter = 200, minFactor = 0.0001))
predict(model_VB_females_H_C)
model_VB_females_H_C
AIC_VB_females_H_C <- AIC(model_VB_females_H_C)
print(AIC_VB_females_H_C) #40151.44

start_values_VB_females_H_C <- list(L_inf =  6.502e+01, k = 9.285e-04, t0 = -2.075e+02, m = 0 )

#VB MALES H_C
model_VB_males_H_C <- nls(H_C ~ MvonBertalanffy(age, L_inf, k, t0),
                          data = data_males,
                          start = start_values_log_males_H_C,
                          control = nls.control(maxiter = 100))
predict(model_VB_males_H_C)
model_VB_males_H_C
AIC_VB_males_H_C <- AIC(model_VB_males_H_C)
print(AIC_VB_males_H_C) #9510.025

start_values_VB_males_H_C <- list(L_inf = 4.914e+01, k = 1.478e-03, t0 = -1.785e+02, m = 0 )

format_model <- function(aic, params) {
  glue::glue(
    "**AIC:** {round(aic, 2)}  \n",
    "**Parameters:**  \n",
    "$L_\\infty={params$L_inf}$  \n",
    "$k={params$k}$  \n",
    "$t_0={params$t0}$"
  )
}

library(tibble)
library(glue)

growth_models_H_C <- tibble(
  Model = c("Logistic", "Von Bertalanffy"),
  Equation = c(
    "$L(t) = \\frac{L_\\infty}{1 + e^{-k(t - t_0)}}$",
    "$L(t) = L_\\infty (1 - e^{-k(t - t_0)})$"
  ),
  Females = c(
    format_model(AIC_log_females_H_C, start_values_log_females_H_C),
    format_model(AIC_VB_females_H_C, start_values_VB_females_H_C)
  ),
  Males = c(
    format_model(AIC_log_males_H_C, start_values_log_males_H_C),
    format_model(AIC_VB_males_H_C, start_values_VB_males_H_C)
  ),
  Reference = c(
    "Maintainer, R., & Rodriguez, D. (2023)",
    "Ogle, D. (2013)"
  )
)
gtable2 <- growth_models_H_C %>%
  gt() %>%
  tab_header(
    title = md("**Growth Models Summary H_C**")
  ) %>%
  fmt_markdown(columns = everything())
gtsave(gtable2, filename = "growth_models_H_C.pdf")

##L_P
L_inf <- max(data$L_P, na.rm = TRUE) #144.51
start_values_L_P <- list(L_inf = 144.51, k = 0.01, t0 = -0.5) 

model_log_females_L_P <- nls(L_P ~ Mlogistic(age, L_inf, k, t0),
                         data = data_females,
                         start = start_values_L_P,
                         control = nls.control(maxiter = 100))

predict(model_log_females)
model_log_females
start_values_log_females <- list(L_inf = 1.080e+02, k = 4.033e-03, t0 = 2.824e+02)
AIC_log_females <- AIC(model_log_females)
print(AIC_log_females) #53175.17

#LOG MALES
model_log_males <- nls(L_C ~ Mlogistic(age, L_inf, k, t0),
                       data = data_males,
                       start = start_values,
                       control = nls.control(maxiter = 100))

predict(model_log_males)
model_log_males
start_values_log_males <- list(L_inf = 1.094e+02, k = 3.938e-03, t0 = 2.787e+02)
AIC_log_males <- AIC(model_log_males)
print(AIC_log_males) #13275.97

#VB FEMALES  utilizo el start value sacado del modelo LOG FEMALE
model_VB_females <- nls(L_C ~ MvonBertalanffy(age, L_inf, k, t0),
                        data = data_females,
                        start = start_values_log_females,
                        control = nls.control(maxiter = 100))
predict(model_VB_females)
model_VB_females
AIC_VB_females <- AIC(model_VB_females)
print(AIC_VB_females) #53016.31

start_values_vb_females <- list(L_inf = 1.366e+02, k = 1.249e-03, t0 = -1.397e+02,m = 0 )

#VB MALES  utilizo el start value sacado del modelo LOG MALE
model_VB_males <- nls(L_C ~ MvonBertalanffy(age, L_inf, k, t0),
                      data = data_males,
                      start = start_values_log_males,
                      control = nls.control(maxiter = 100))
predict(model_VB_males)
model_VB_males
AIC_VB_males <- AIC(model_VB_males)
print(AIC_VB_males) #13201.46

start_values_vb_males <- list(L_inf =  1.296e+02, k = 1.401e-03, t0 = -1.348e+02 ,m = 0 )

########################## CRECIMIENTO POR ETAPAS
# Condiciones: 
# Tupper/acuario --> cuando nacen hasta que pesan 30g
# Depósito --> des de que pesan 30g hasta que miden 75-80mm
# Cercado exterior --> a partir de los 75-80mm hasta pasada una hibernación (se chipan)
# Gabia aclimatació --> las que pasada una hibernación siguen siendo "pequeñas"
# Solta directa --> las que pasadas una hibernación son grandes, o las que ya han estado en la gabia de aclimataci´ó

levels(data$stage)
data$general_stage <- NA

###Creamos tabla resumen de los estadísticos por etapas, solo del PESO y la LONGITUD DEL CAPARAZÓN
levels(data$stage)
tabla_resumen <- data %>%
  filter(!is.na(stage)) %>%
  mutate(stage_nombre = case_when(
    stage == "Aquari" ~ "Acuario",
    stage == "Tupper" ~ "Tupper",
    stage == "Deposit" ~ "Depósito",
    stage == "Tancat Exterior" ~ "Cercado exterior",
    stage == "Suelta" ~ "Suelta",
    stage == "Gabia aclimatacio" ~ "Jaulas de aclimatación",
    stage == "Solta directa" ~ "Suelta directa",
    TRUE ~ as.character(stage)
  )) %>%
  mutate(stage_nombre = factor(stage_nombre, levels = c(
    "Acuario", "Tupper", "Depósito", "Cercado exterior","Suelta", "Jaulas de aclimatación", "Suelta directa"
  ))) %>%
  group_by(stage_nombre) %>%
  summarise(across(
    c(W, L_C),
    list(
      media = ~round(mean(.x, na.rm = TRUE), 2),
      mediana = ~round(median(.x, na.rm = TRUE), 2),
      sd = ~round(sd(.x, na.rm = TRUE), 2),
      min = ~round(min(.x, na.rm = TRUE), 2),
      max = ~round(max(.x, na.rm = TRUE), 2)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  mutate(
    W_rango = paste0(W_min, " - ", W_max),
    L_C_rango = paste0(L_C_min, " - ", L_C_max)
  ) %>%
  select(
    stage = stage_nombre,
    W_media, W_mediana, W_sd, W_min, W_max, W_rango,
    L_C_media, L_C_mediana, L_C_sd, L_C_min, L_C_max, L_C_rango
  )

tabla_gt <- tabla_resumen %>%
  gt() %>%
  cols_label(
    stage = "Etapa",
    W_media = "Media", W_mediana = "Mediana", W_sd = "SD", W_min = "Min", W_max = "Max", W_rango = "Rango",
    L_C_media = "Media", L_C_mediana = "Mediana", L_C_sd = "SD", L_C_min = "Min", L_C_max = "Max", L_C_rango = "Rango"
  ) %>%
  tab_spanner(
    label = "Peso (W)",
    columns = c(W_media, W_mediana, W_sd, W_min, W_max, W_rango)
  ) %>%
  tab_spanner(
    label = "Longitud del caparazón (L_C)",
    columns = c(L_C_media, L_C_mediana, L_C_sd, L_C_min, L_C_max, L_C_rango)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Arial"
  )

getwd()
gtsave(tabla_gt, "tabla.pdf")
gtsave(tabla_gt, filename = "tabla_descriptivos_W_y_L_C.html")

library(gt)
library(webshot2)

# Guardar la tabla como PDF
gtsave(
  data = tabla_gt,
  filename = "tabla_horizontal.pdf",
  expand = 10   
)
gtsave(tabla_gt, "tabla_horizontal.png", vwidth = 3500, vheight = 2000)

###ahora con todas las variables biometricas
tabla_resumen_todos <- data %>%
  filter(!is.na(stage)) %>%
  mutate(stage = factor(stage, levels = c(
    "Acuario", "Tupper", "Deposit", "Tancat Exterior", "Gabia aclimatacio", "Solta directa", "Suelta"
  ))) %>%
  group_by(stage) %>%
  summarise(across(
    c(W, L_C, A_C, L_P, H_C),
    list(
      mitjana = ~round(mean(.x, na.rm = TRUE), 2),
      mediana = ~round(median(.x, na.rm = TRUE), 2),
      sd = ~round(sd(.x, na.rm = TRUE), 2),
      min = ~round(min(.x, na.rm = TRUE), 2),
      max = ~round(max(.x, na.rm = TRUE), 2)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  mutate(
    W_rango = paste0(W_min, " - ", W_max),
    L_C_rango = paste0(L_C_min, " - ", L_C_max),
    A_C_rango = paste0(A_C_min, " - ", A_C_max),
    L_P_rango = paste0(L_P_min, " - ", L_P_max),
    H_C_rango = paste0(H_C_min, " - ", H_C_max)
  ) %>%
  select(
    stage,
    W_mitjana, W_mediana, W_sd, W_rango,
    L_C_mitjana, L_C_mediana, L_C_sd, L_C_rango,
    A_C_mitjana, A_C_mediana, A_C_sd, A_C_rango,
    L_P_mitjana, L_P_mediana, L_P_sd, L_P_rango,
    H_C_mitjana, H_C_mediana, H_C_sd, H_C_rango
  )

tabla_gt_todos <- tabla_resumen_todos %>%
  gt() %>%
  tab_header(title = "Descriptivos por etapa (mitjana, mediana, SD, rango)") %>%
  cols_label(
    stage = "Etapa",
    W_mitjana = "W Mitjana", W_mediana = "W Mediana", W_sd = "W SD", W_rango = "W Rango",
    L_C_mitjana = "L_C Mitjana", L_C_mediana = "L_C Mediana", L_C_sd = "L_C SD", L_C_rango = "L_C Rango",
    A_C_mitjana = "A_C Mitjana", A_C_mediana = "A_C Mediana", A_C_sd = "A_C SD", A_C_rango = "A_C Rango",
    L_P_mitjana = "L_P Mitjana", L_P_mediana = "L_P Mediana", L_P_sd = "L_P SD", L_P_rango = "L_P Rango",
    H_C_mitjana = "H_C Mitjana", H_C_mediana = "H_C Mediana", H_C_sd = "H_C SD", H_C_rango = "H_C Rango"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Times New Roman"
  )

gtsave(tabla_gt_todos, filename = "tabla_descriptivos_biometrias.html")
