
# Función para Calcular nº de visita de los pacientes ----------------------------------------
# creamos una nueva variable, que sea el número de visita de cada paciente. visita 1, 2, 3, etc
# como ya sabemos que en esta bbdd todos los pacientes tienen al menos dos visitas,
# queremos seleccionar las filas que tengan en la nueva variable = 1 y la nueva variable = maximo para cada paciente.

t_seg <- function(datos, id = "id", fevisi = "fevisi", dias.min = 0) {
  library(dplyr)
  if (sum(names(datos) == parse(text = id)) == 1 &      # si hay UNA variable en la bbdd que se llama como se indica en el argumento id,
      sum(names(datos) == parse(text = fevisi)) == 1) { # y hay UNA variable que se llama como se indica en el argumento fevisi:
     if (sum(names(datos) == "t_seg") == 0) {
       # id: nombre de la columna de la bbdd que indica la identificación del paciente
       # fevisi: nombre de la columna de la bbdd que indica la fecha de visita
       # dias.min: indica el número mínimo de días que queremos que haya entre visitas. Debe ser 0 o un nº positivo
       if (dias.min >= 0) {
        n.id <- which(names(datos) ==  id) # n.id es el nº de la columna de datos que se llama como le indicas en id
        n.fevisi <- which(names(datos) ==  fevisi) 
        datos <- arrange(datos, datos[[n.id]], datos[[n.fevisi]])   # ordenamos por id y fevisi ascendentemente. otra vez, por si acaso
        # Calculamos el tiempo entre visitas
        if (class(datos[[n.fevisi]]) == "Date") {
          datos$t_visita <- ifelse(datos[[n.id]] == lag(datos[[n.id]]), (datos[[n.fevisi]] - lag(datos[[n.fevisi]]))/(365.25), 0)
          datos$t_visita[1] <- 0
        } else {
          stop("La variable de fecha de visita debe ser de clase 'Date'")
        }
        
        # Eliminamos visitas que tienen entre ellas menos de min.dias de diferencia de fevisis
        if(dias.min > 0) {
          datos <- filter(datos, datos$t_visita > (dias.min/365.25))
        }
        # A partir de t_visita, calculamos el tiempo de visita acumulado, al que llamamos t_visita_ac.
        datos$t_visita_ac <- datos$t_visita
        for(i in 2:nrow(datos)){
          if (datos[[n.id]][i] == datos[[n.id]][i-1]) {
            datos$t_visita_ac[i] <- (datos$t_visita_ac[i] + datos$t_visita_ac[i-1])
          }
        }
        # Calcular t_seg (tiempo de seguimiento en años) de cada paciente
        datos_tiempos <- datos[, which(names(datos) %in% c(parse(text = id), parse(text = fevisi), "t_visita", "t_visita_ac"))]
        datos_tiempos$t_seg <- datos_tiempos$t_visita_ac
        datos_tiempos <- arrange(datos_tiempos, datos_tiempos[[n.id]], datos_tiempos[[n.fevisi]])
        datos_tiempos$t_seg <- ifelse(lead(datos_tiempos$t_visita_ac, 1) == 0, datos_tiempos$t_visita_ac, NA)
        datos_tiempos$t_seg[nrow(datos_tiempos)] <- datos_tiempos$t_visita_ac[nrow(datos_tiempos)] # así se soluciona solo la ultima fila del ultimo paciente
        datos_tiempos <- datos_tiempos[is.na(datos_tiempos$t_seg) == FALSE, which(names(datos_tiempos) %in% c(parse(text = id), "t_seg"))]
        datos <- merge(datos, datos_tiempos, all = TRUE)
        datos <- arrange(datos, datos[[n.id]], datos[[n.fevisi]])
        return(datos[, - which(names(datos) %in% c("t_visita"))])
       } else {
         stop("El argumento dias.min debe ser el nº 0 o un nº positivo. \n Indica el número mínimo de días que queremos que haya entre visitas.")
       }
    } else {
      stop("Ya existe en tu base de datos una variable llamada 't_seg'")
    }
  } else {
    stop("Es imprescindible que la base de datos contenga al menos dos columnas: \n 1. una (y sólo una) llamada como se indica en el argumento 'id',\n 2. una (y sólo una) llamada como se indica en el argumento 'fevisi'\n Recuerda que el argumento 'id' por defecto es ''id'' y  el argumento 'fevisi' por defecto es ''fevisi''.")
  }
}

# prueba <- t_seg(datos = BBDD.E, id = "hisclin")
# prueba3 <- t_seg(datos = data, id = "id", fevisi = "fevisi", dias.min = 0)

