install.packages("tidyverse")
library(tidyverse)
setwd("~/GitHub/Trabajo-Uno")

set.seed(10)
sample(c("SI","NO"), 10, replace = T)

########################### Ejercicio 1 ####################################

# La Funcion elecciones toma: el total, votosSI, votosNO, sin haber votos blancos, ni nulos
Elecciones <- function(total,votosSI,votosNO){
  if (votosSI >= ((total*0.5)+1)) {
    print("Gana opcion SI")
  } else{
    print("votosSI no tiene electores") 
    if (votosSI >= total*0.3) {
      print("votosSI tiene un 30% de electores")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("Gana opcion SI")
      } else{
        if (votosNO >= ((total*0.5)+1)) {
          print("Gana opcion NO")
        } else{
          print("votosNO tiene un 30% de electores")
          if (votosSI == votosNO) {
            print("La cantidad de electores son iguales, Gana opcion NO porque hay empate")   # electores refiere al quorum
            print("La cantidad de electores son iguales, Gana opción SI porque hay empate")
          } else{
            if (votosSI > votosNO) {
              print("Gana opcion SI")
            } else{
              print("Gana opcion NO")
            }
          }
        }
      }
    } else{
      print("Gana opcion NO")
    }
  }
}

Elecciones(10,6,4)


# Funcion elecciones_solo_total, toma unicamente el "total" de votos.

elecciones_solo_total <- function(total){
# Para cacular los votosSi y votosNO se necesita una semilla"(seed) aleatoria.
  set.seed(total)
  padron <- sample(c("SI","NO"),total,replace = TRUE)
  padron <- as.data.frame(padron)
  names(padron) <- c("votos")
  votosSI <- sum(with(padron,votos == "SI"))
  votosNO <- sum(with(padron,votos == "NO"))
  
  if (votosSI >= ((total*0.5)+1)) {
    print("Gana opcion SI")
  } else{
    print("votosSI no tiene electores") 
    if (votosSI >= total*0.3) {
      print("votosSI tiene un 30% de electores")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("Gana opcion SI")
      } else{
        if (votosNO >= ((total*0.5)+1)) {
          print("Gana opcion NO")
        } else{
          print("votosNO tiene un 30% de electores")
          if (votosSI == votosNO) {
            print("La cantidad de electores son iguales, Gana opción SI porque hay empate")
          } else{
            if (votosSI > votosNO) {
              print("Gana opcion SI")
            } else{
              print("Gana opcion NO")
            }
          }
        }
      }
    } else{
      print("Gana opcion NO")
    }
  }
}
# Se realiza la funcion,  se debe ingresar los 10 votos que se indican en el ejercicio.
elecciones_solo_total(10)

# set.seed() Sirve para producir una secuencia de numeros de forma "aleatoria" almacenando estos en la semilla.
# samples() esta funcion sirve para producir numeros o datos aleatorios.




#################### Ejercicio 2 ########################

# Escribir datos

listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))

CantidadNiños <- list()
for(elemento in listaDocumentos){
  if(elemento[1] == "mp"){
    
# variable que nos dice si detecto o no la CantidadNiños
  detecto <- F
    
# viendo si hay datos en CantidadNiños 
  
  if(length(CantidadNiños) != 0){
  
# repaso de datos guardados de CantidadNiños
  
  for (posicionCantidadNiños in 1:length(CantidadNiños)) {

# rescatar elementos
    
   CantidadNiñosunlist <- unlist(CantidadNiños[posicionCantidadNiños])
    
# se revisa si existe la CantidadNiños para actualizarlos

   if(CantidadNiñosunlist[1] == (length(elemento)-1)){
   CantidadNiñosunlist[2] <- CantidadNiñosunlist[2]+1
   CantidadNiños[posicionCantidadNiños] <- list(c(CantidadNiñosunlist))
   detecto <- T
      }
    }
  }
    # se genera una nueva CantidadNiños
    if(!detecto){
      nuevaCantidadNiños <- c()
      nuevaCantidadNiños[1] <- (length(elemento)-1)
      nuevaCantidadNiños[2] <- 1
      CantidadNiños <- c(CantidadNiños,list(c(nuevaCantidadNiños)))
    }
  }
}
# entregando CantidadNiños

for (CantidadNiños in CantidadNiños) {
  print(paste("Se cuentan con",CantidadNiños[2],"mp de",CantidadNiños[1],"niños"))
}

