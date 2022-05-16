library(ggpubr)
library(rcompanion)
library(WRS2)

# 1.

instanciaA <- c(129, 109, 28, 178, 74, 16, 87, 108, 149, 78)
tiempoA <- c(1510394, 402929, 885722, 4428151,48667,834565, 70599, 783108,210041, 37449)
algoritmoA <- data.frame(instanciaA, tiempoA)

instanciaB <- c(134, 193, 10, 88, 142, 86, 36, 190, 163, 33)
tiempoB <- c(1252837, 2196277, 120276, 4629726, 5743260, 6701654, 6568968, 180141, 6684497, 35974)
algoritmoB <- data.frame(instanciaB, tiempoB)

algoritmos <- data.frame(tiempoA, tiempoB)

g <- gghistogram(algoritmoA, x = "instanciaA")
print(g)

transformarDatos <- function(datos){
  lambda <- transformTukey(datos, start = -1, end = 1, 
                             int = 0.1, plotit = TRUE, 
                             returnLambda = TRUE)
  
  datosTukey <- transformTukey(datos, start = -1, end = 1, 
                                int = 0.1, plotit = TRUE, 
                                returnLambda = FALSE)
  
  return(list(lambda, datosTukey))
}

transfA <- transformarDatos(algoritmoA$tiempoA)
lambda_A <- transfA[1]
datosTukeyA <- transfA[2]

transfB <- transformarDatos(algoritmoB$tiempoB)
lambda_B <- transfB[1]
datosTukeyB <- transfB[2]

tiemposTukey <- data.frame(datosTukeyA, datosTukeyB)

alfa <- 0.05
prueba <- t.test(x = datosTukeyA[[1]], 
                 y = datosTukeyB[[1]],
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba$p.value)


# 2.

datos2 <- read.csv2("EP11 Datos.csv")

# Contexto: Se desea realizar una aplicaci?n de citas y se 
# requiere evaluar los grupos a los que irá enfocada la app.
# Por ello se desea estudiar el promedio de edad de hombres y mujeres 
# que están solteros(as) en el rango de edad entre 20 y 40 a?os.

adulto_joven <-  datos2 %>% filter(edad > 20 & edad < 40)
set.seed(198)
n <- 350
adulto_joven <- adulto_joven %>% sample_n(n) %>% select(sexo, ecivil, edad)
solteros <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Hombre")
solteros_edad <- solteros$edad
solteras <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Mujer")
solteras_edad <- solteras$edad

edad <- c(solteros_edad, solteras_edad)
genero <- c(rep("M", length(solteros_edad)), rep("F"  length(solteras_edad)))
datos_df <- data.frame(edad, genero)

# Comprobar normalidad
g <- ggqqplot (datos_df, x = "edad",facet.by = "genero" ,
               palette = c ("blue","red") , color = "genero")
print(g)
         
alfa2 <- 0.05

# Ver poda del 20 %
gamma <- 0.2
n_a <- length(solteros_edad)
n_b <- length (solteras_edad)
poda_a <- n_a*gamma
poda_b <- n_b*gamma
a_truncada <- solteros_edad[poda_a:(n_a-poda_a)]
b_truncada <- solteras_edad[poda_b:(n_b-poda_b)]
tiempo <- c(a_truncada, b_truncada)
algoritmo <- c(rep("M", length(a_truncada)), rep("F", length(b_truncada)))
datos_truncados <- data.frame(edad, genero)
g <- ggqqplot(datos_truncados, x="edad",facet.by = "genero",
              palette = c("blue", "red") , color = "genero" )
print(g) 
# Aplicar prueba de Yuen
prueba <- yuen(edad ~ genero, data=datos_df, tr=gamma)
print(prueba)


# 3.

# 2.
# Ej. Idea: dependiendo de cuanto ganan influye en si van a pie, en vehiculo o en transporte publico (2)

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_0: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.


set.seed(891)
n2 <- 450

datos2 <- datos %>% sample_n(n2) %>% select(region, ytotcorh, o25c)

# ezAnova type 2 o 3 
# aov usa tipo 1

motorizado <- datos2 %>% filter(o25c == "Vehículo motorizado particular (auto, camioneta, motocicleta") 
motorizado_ingreso <- motorizado$ytotcorh

publico <- datos2 %>% filter(o25c == "Transporte público (bus, microbús, metro, taxi colectivo, et") 
publico_ingreso <- publico$ytotcorh

pie <- datos2 %>% filter(o25c == "A pie") 
pie_ingreso <- pie$ytotcorh

# Usamos el método robusto

ingreso <- c(motorizado_ingreso, publico_ingreso, pie_ingreso)
transporte <- c(rep("motorizado", length(motorizado_ingreso)),
                rep("publico_ingreso", length(motorizado_ingreso)),
                rep("motorizado", length(motorizado_ingreso))
               )
                    
datos_3 <- data.frame(ingreso, transporte)
alfa <- 0.05
gamma <- 0.2
medias_truncadsa <- t1way(ingreso~transporte, data = datos_3, tr = gamma,
                          alpha = alfa)
           
print(medias_truncadas)

if(medias_truncadas$p.value < alfa){
  set.seed(666)
  post_hoc <- linea(ingreso ~ transporte, data = datos, tr = gamma,
                    alpha = alfa)
  print(post_hoc)
}
        
muestras <- 999
                 
