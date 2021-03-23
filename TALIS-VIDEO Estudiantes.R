#### Practica 1 TALIS VIDEO ####

library(dplyr)
library(plotly)
library(readr)

### Acá en la ruta coloque la ruta donde descomprió los datos

ruta="C:/Users/nelso/Downloads/GTI Data csv (1)"
setwd(ruta)

#### Cargar los datos
GTI_Student_Data=read_csv("GTI-Student-Data.csv")
GTI_School_Data=read_csv("GTI-School-Data.csv")
GTI_Teacher_Data=read_csv("GTI-Teacher-Data.csv")
GTI_TeachLog_Data=read_csv("GTI-TeachLog-Data.csv")
GTI_Artefact_Data=read_csv("GTI-Artefact-Data.csv")


#### Seleccionar a Colombia
COL_EST=GTI_Student_Data %>% filter(COUNTRY=="Colombia")
COL_SCHO=GTI_School_Data %>% filter(COUNTRY=="Colombia")
COL_TEACH=GTI_Teacher_Data %>% filter(COUNTRY=="Colombia")
COL_TEACHLOG=GTI_TeachLog_Data %>% filter(COUNTRY=="Colombia")
COL_ARTE=GTI_Artefact_Data %>% filter(COUNTRY=="Colombia")

#### Trabajo sobre la base de colegio


### Proporcion por tipo de colegio

## 1. Zona=Urbano o rural
COL_SCHO %>% group_by(URBANICITY) %>% summarise(Conteo=n())

plot_ly(COL_SCHO, labels=~URBANICITY, type="pie")%>%
  layout(title = 'Zona del colegio')

## 2. Sector Privado o publico
COL_SCHO %>% group_by(SCHOOL_CLASSIFICATION) %>% summarise(Conteo=n())

plot_ly(COL_SCHO, labels=~SCHOOL_CLASSIFICATION, type="pie")

## 3. Cruce de los dos

COL_SCHO %>% group_by(SCHOOL_CLASSIFICATION, URBANICITY) %>% summarise(Conteo=n())

# Diagrama de barras con conteos
tabla_PxU=COL_SCHO %>% group_by(SCHOOL_CLASSIFICATION, URBANICITY) %>% summarise(Conteo=n())
plot_ly(tabla_PxU, x=~URBANICITY, y=~Conteo, color=~SCHOOL_CLASSIFICATION, type="bar")


### 4. Genero TQB01 (realizar el pastel del genero de los docentes)

### 5. Agnos de experiencia en matematicas

plot_ly(COL_TEACH, x=~TQB06A, type = "histogram")

### Formación docente (ACTIVIDAD) TB_QUAL
### ¿Que contienen estas variables?
### Realice un diagrama de pastel
### ¿Tiene sentido calcular el promedio?
### Teacher codebook 



####Practica 2 Estudiantes ####

## Pregunta SQA06B 
## (. I can usually give good answers to test questions on mathematic topics.)
#1 Strongly disagree
#2 Disagree
#3 Agree 8778 
#4 Strongly agree
#9999 Missing 
COL_EST %>% group_by(SQA06B) %>% summarise(Conteo=n())


## Debemos quitar los NA
x=na.exclude(COL_EST$STA_PROPCORRECTSCORE)
a=density(x)
plot_ly(x=a$x, y=a$y, type="scatter",mode="lines") %>%
  layout(title = 'Proporción de respuestas correctas en el Pre-test',
         xaxis = list(title = 'Proporción',
                      zeroline = TRUE,range = c(0, 1)),
         yaxis = list(title = 'Densidad'))


x=na.exclude(COL_EST$STB_PROPCORRECTSCORE)
a=density(x)
plot_ly(x=a$x, y=a$y, type="scatter",mode="lines") %>%
  layout(title = 'Proporción de respuestas correctas en el Post-test',
         xaxis = list(title = 'Proporción',
                      zeroline = TRUE,range = c(0, 1)),
         yaxis = list(title = 'Densidad'))

### COmparar los post-Test en diferentes paises
GTI_School_Data%>%group_by(COUNTRY) %>% summarise(Conteo=n())

x1=na.exclude(COL_EST$STA_PROPCORRECTSCORE)
a1=density(x1)

## China 
CHINA_SHAN_EST=GTI_Student_Data %>%filter(COUNTRY=="Shanghai")
x2=na.exclude(CHINA_SHAN_EST$STB_PROPCORRECTSCORE)
a2=density(x2)

## Chile
CHILE_EST=GTI_Student_Data %>%filter(COUNTRY=="Chile")
x3=na.exclude(CHILE_EST$STB_PROPCORRECTSCORE)
a3=density(x3)

## Alemania
ALE_EST=GTI_Student_Data %>%filter(COUNTRY=="Germany")
x4=na.exclude(ALE_EST$STB_PROPCORRECTSCORE)
a4=density(x4)

plot_ly(x=a1$x, y=a1$y, type="scatter",mode="lines", name="Colombia") %>%
  layout(title = 'Proporción de respuestas correctas en el Post-Test',
         xaxis = list(title = 'Proporción',
                      zeroline = TRUE,range = c(0, 1)),
         yaxis = list(title = 'Densidad'))%>%
  add_trace(x=a2$x, y=a2$y, name="China")%>%
  add_trace(x=a3$x, y=a3$y, name="Chile")%>%
  add_trace(x=a4$x, y=a4$y, name="Alemania")


##Practica
## Compare los pre-test por medio de un grafico de densidad
#1 Chile        98
#2 Colombia     83
#3 England      78
#4 Germany      38
#5 Japan        73
#6 Madrid       55
#7 Mexico      103
#8 Shanghai     85


### Cruce con colegio
## Vamos a colocar todas las variables de colegio, en la base de datos de estudiantes
## Y en la base de docentes
## Con llave de enlace el SCHOOL ID (SCH_ID)
COL_TEACH_SCHOOL=COL_TEACH%>% left_join(COL_SCHO,by ="SCH_ID")

COL_EST_SCHOOL=COL_EST%>% left_join(COL_SCHO,by ="SCH_ID")



#### Parte 3 Dominios, Componentes e Indicadores ####

a=density(COL_TEACH$VDOMAIN_CLASSMAN)
plot_ly(x=a$x, y=a$y, type="scatter",mode="lines") %>%
  layout(title = 'Dominio de manejo de la clase',
         xaxis = list(title = 'Promedio de los componentes',
                    zeroline = TRUE,range = c(1, 4)),
        yaxis = list(title = 'Densidad'))

a=density(COL_TEACH$VDOMAIN_SE)
plot_ly(x=a$x, y=a$y, type="scatter",mode="lines") %>%
  layout(title = 'Dominio de apoyo socio emocional',
         xaxis = list(title = 'Promedio de los componentes',
                      zeroline = TRUE,range = c(1, 4)),
         yaxis = list(title = 'Densidad')) 

### Dominio de instrucción
### Promedio de los dominios AR, DC, QS y CE
### Aunque en el deominio de QS quitaron un componente
DOM_INSTRUCT=(3*COL_TEACH$VDOMAIN_AR+3*COL_TEACH$VDOMAIN_DC+2*COL_TEACH$VDOMAIN_QS+
                3*COL_TEACH$VDOMAIN_CE)/11
DOM_INSTRUCT-COL_TEACH$VDOMAIN_INSTRUCT ## Son numericamente iguales


a1=density(COL_TEACH$VDOMAIN_AR)
a2=density(COL_TEACH$VDOMAIN_DC)
a3=density(COL_TEACH$VDOMAIN_QS)
a4=density(COL_TEACH$VDOMAIN_CE)
a5=density(COL_TEACH$VDOMAIN_INSTRUCT)

plot_ly(x=a1$x, y=a1$y, type="scatter",mode="lines", name="AR") %>%
  layout(title = 'Promedio del dominio de instrucción',
         xaxis = list(title = 'Promedio de los componentes',
                      zeroline = TRUE,range = c(1,4)),
         yaxis = list(title = 'Densidad'))%>%
  add_trace(x=a2$x, y=a2$y, name="DC")%>%
  add_trace(x=a3$x, y=a3$y, name="QS")%>%
  add_trace(x=a4$x, y=a4$y, name="CE")%>%
  add_trace(x=a5$x, y=a5$y, name="INSTRUCT")


#### Tarea
#### Comparen el dominio de instrucción entre los diferentes paises