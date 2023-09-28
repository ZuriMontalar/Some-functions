
# Función para calcular evoluciones de variables categóricas 0 y 1 -------------------

evo_01 <- function(datos, id = "id", var_ev, nombre_ev, porc = 80, irregular = 2, caso50 = 1 ) {
  # datos será la base de datos
  # var_ev es la varibale de la base de datos de la cual queremos calcular . debe tener valores únicamente 0's y/o 1's
  # porc es un nº entre 50 y 100. es el porcentaje de evolución
  # irregular sólo debería poder tener valores 1 o 2. Indica qué hacer cuando no se llega a cumplir el porcentaje.por defecto es 1. Es decir, en los casos heterogéneos
  # nombre_ev es cómo quieres que se llame la nueva variable de evolución que la función va a calcular. tiene que ser tipo caracter tamaño 1
  # caso50 es 1 o 2 e indica qué hacemos cuando porc = 50 y las variables coinciden en valores el 50% de ocasiones. por defecto, es 1
  # id: nombre de la columna de la bbdd que indica la identificación del paciente. Por defecto es "id", pero podría ser "hisclin" por ejemplo
  
  if (sum(names(datos) == parse(text = id)) == 1) { # si hay UNA variable en la bbdd que se llama como se indica en el argumento id:
    n.id <- which(names(datos) ==  id) # n.id es el nº de la columna de datos que se llama como le indicas en id
    if (sum(names(datos) == parse(text = nombre_ev)) == 0) {
      if (caso50 %in% c(1, 2)) {
        if (irregular %in% c(1, 2)) {
          if (porc >= 50 & porc < 100) { 
            var <- which(names(datos) ==  var_ev) # var es el nº de la columna de datos que se llama como le indicas en var_ev
            frecuencias <- as.data.frame(round(addmargins(prop.table(table(datos[[n.id]], datos[[var]]), margin = 1)), 3))
            frecuencias$Var1 <- as.character(frecuencias$Var1)
            frecuencias$Var1 <- as.numeric(frecuencias$Var1)
            frecuencias2 <- frecuencias
            names(frecuencias2) <- c(parse(text = id), "num_evo", "Freq")
            frecuencias2$num_evo <- as.character(frecuencias2$num_evo)
            frecuencias2$num_evo <- as.numeric(frecuencias2$num_evo)
            
            # nombre_ev  =  0. si var_ev = 0 al menos el porc% de las visitas
            #            = 1. si var_ev = 1 en al menos el porc% de las visitas
            #            = 1 o 2. si var_ev heterogéneo entre visitas, según "irregular"
            
            frecuencias2 <- dplyr::filter(frecuencias2, frecuencias2$num_evo != "Sum" & frecuencias2$Freq != "NaN")
            if (porc > 50) {
              frecuencias2$nombrefunc <- ifelse(is.na(frecuencias2$num_evo) == TRUE, NA,
                                          ifelse(frecuencias2$Freq >= porc/100, frecuencias2$num_evo, NA))
            } else { # si porc es 50, queremos que nombre_ev sea lo que has elegido que sea en el argumento irreguar (1 o 2)
              frecuencias2$nombrefunc <- ifelse(is.na(frecuencias2$num_evo) == TRUE, NA,
                                                       ifelse(frecuencias2$Freq > porc/100, frecuencias2$num_evo,
                                                              ifelse(frecuencias2$Freq == porc/100, caso50, NA)))
              frecuencias2 <- frecuencias2[-which(duplicated(frecuencias2[c("id", "nombrefunc")])), ] 
            }
            
            frecuencias2 <- frecuencias2[, c(1,4)]
            frecuencias2 <- dplyr::filter(frecuencias2, is.na(frecuencias2$nombrefunc) == FALSE) 
            datos_con_evo <- merge(datos, frecuencias2, all.x = TRUE)
            datos_con_evo$nombrefunc <- ifelse(is.na(datos_con_evo$nombrefunc) == TRUE, irregular, datos_con_evo$nombrefunc)
            names(datos_con_evo)[which(names(datos_con_evo) ==  "nombrefunc")] <-  parse(text = nombre_ev)
            return(datos_con_evo)
          } else {
            stop("El argumento 'porc' sólo puede ser un valor numérico dentro del intervalo [50,100). Es el porcentaje de evolución.")
            }
        } else {
          stop("El argumento 'irregular' sólo puede tener valores 1 o 2.\n Indica qué hacer cuando no se llega a cumplir el porcentaje. \n Por defecto es 1. Es decir, en los casos heterogéneos.")
        }
      } else {
        stop("El argumento 'caso50' sólo puede tener valores 1 o 2. \n Indica qué hacer cuando porc = 50 y la variable de la cual quieres calcular su evolución coincide en valores el 50% de ocasiones de un mismo id. \n Por defecto es 1.")
      }
    } else {
      stop("Ya existe en tu base de datos una variable llamada como has indicado en el argumento 'nombre_ev'. \n 'nombre_ev' es cómo quieres que se llame la nueva variable de evolución que la función va a calcular. Por ejemplo, nombre_ev = 'evo_variable'.")
    }
  } else {
    stop("O bien no existe en tu base de datos una variable llamada como has indicado en el argumento 'id', o bien existe más de una variable llamada así.")
  }
}

# prueba <- evo_01(datos = datos2, var_ev = "beta_1", nombre_ev = "ev_beta", porc = 80, irregular = 1, caso50 = 1, id = "hisclin")
