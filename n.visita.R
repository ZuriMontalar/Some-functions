
# Función para Calcular nº de visita de los pacientes ----------------------------------------
# creamos una nueva variable, que sea el número de visita de cada paciente. visita 1, 2, 3, etc
# como ya sabemos que en esta bbdd todos los pacientes tienen al menos dos visitas,
# queremos seleccionar las filas que tengan en la nueva variable = 1 y la nueva variable = maximo para cada paciente.

n_visita <- function(datos, id = "id", fevisi = "fevisi") {
  if (sum(names(datos) == parse(text = id)) == 1 &  # si hay UNA variable en la bbdd que se llama como se indica en el argumento id
      sum(names(datos) == parse(text = id)) == 1) { # y hay UNA variable en la bbdd que se llama como se indica en el argumento fevisi
   
    if (sum(names(datos) == "n_visita") == 0) {
      # id: nombre de la columna de la bbdd que indica la identificación del paciente
      # fevisi: nombre de la columna de la bbdd que indica la fecha de visita
      n.id <- which(names(datos) ==  id) # n.id es el nº de la columna de datos que se llama como le indicas en id
      n.fevisi <- which(names(datos) ==  fevisi)
      
      datos <- dplyr:: arrange(datos, datos[[n.id]], datos[[n.fevisi]])    # ordenamos por id y fevisi ascendentemente. otra vez, por si acaso
      datos$n_visita <- NA # está vacía de mometo. vemos que se ha creado
      datos$n_visita[1] <- 1 # asignamos 1 al primer paciente de la datos. Necesitamos un punto de partida
      for (i in 2:nrow(datos)) {
        datos$n_visita[i] <- ifelse(datos[[n.id]][i] == datos[[n.id]][i-1], datos$n_visita[i-1] + 1, 1)
      }
      return(datos)
    } else {
      stop("Ya existe en tu base de datos una variable llamada 'n_visita'")
    }
  } else {
    stop("O bien no existe en tu base de datos una variable llamada como has indicado en el argumento 'id' y/o en 'fevisi', o bien existe más de una variable llamada así.")
  }
}

# setwd("D:/R/prediciendo se hace diabetico")
# load("BBDD_E.Rda")
# which(names(BBDD.E) == "n_visita")
# BBDD.E <- BBDD.E[, -which(names(BBDD.E) == "n_visita")]
# prueba <- n_visita(datos = BBDD.E, id = "id", fevisi = "fevisi")
# prueba2 <- n_visita(datos = BBDD.E)
# prueba3 <- n_visita(datos = BBDD.E, id = "hisclin", fevisi = "fevisi")

