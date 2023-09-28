
### Función para calcular la evolución de TA en el 80% de las visitas ---------------------------------------

ev_TA <- function(datos, irregular = 2, porc = 80, id = "id", psmed = "psmed", pdmed = "pdmed") {
  
  # psmed: nombre de la columna de la bbdd que indica presión arterial sistólica media en mmHg
  # pdmed: nombre de la columna de la bbdd que indica presión arterial diastólica media en mmHg
  # id: nombre de la columna de la bbdd que indica la identificación del paciente. Por defecto es "id", pero podría ser "hisclin" por ejemplo
  if (sum(names(datos) == parse(text = id)) == 1 &      # si hay UNA variable en la bbdd que se llama como se indica en el argumento id,
      sum(names(datos) == parse(text = psmed)) == 1 &   # UNA variable que se llama como se indica en el argumento psmed y
      sum(names(datos) == parse(text = pdmed)) == 1) {  # UNA variable que se llama como se indica en el argumento pdmed:
    n.psmed <- which(names(datos) ==  psmed) # n.psmed es el nº de la columna de datos que se llama como le indicas en psmed
    n.pdmed <- which(names(datos) ==  pdmed) # n.pdmed es el nº de la columna de datos que se llama como le indicas en pdmed
    n.id <- which(names(datos) ==  id) # n.id es el nº de la columna de datos que se llama como le indicas en id
    
    ## evolución de psmed -----------------------------------------------------------------------------------
    # Pero antes, crearemos dos variables anteriores: ev_ps1 y ev_pd1:
    #   ev_ps1*:
    # 0 = normal = psmed <140
    # 1 = alta = psmed =>140
    if (sum(names(datos) == "ev_TA") == 0) {
      if (irregular %in% c(1, 2)) {
        if (porc > 50 & porc < 100) { 
    
          datos$ev_ps1 <- ifelse(is.na(datos[[psmed]]) == TRUE, NA,
                                 ifelse(datos[[psmed]] >= 140, 1, 0))
          frecuencias_ps <- as.data.frame(round(addmargins(prop.table(table(datos[[n.id]], datos$ev_ps1), margin = 1)), 3))
          frecuencias_ps$Var1 <- as.character(frecuencias_ps$Var1)
          frecuencias_ps$Var1 <- as.numeric(frecuencias_ps$Var1)
          frecuencias_ps$Var2 <- as.character(frecuencias_ps$Var2)
          frecuencias_ps$Var2 <- as.numeric(frecuencias_ps$Var2)
          names(frecuencias_ps) <- c(parse(text = id), "num_evo", "Freq")
          
          # Para calcularlo, calcularemos dos variables: 2 variables, ev_pdmed, ev_psmed,
          # ev_psmed*:
          # 0 = normal = psmed < 140 el 80% de las veces
          # 1 = alta = psmed > 140 el 80% de las veces
          # 1 o 2 según "irregular"=  heterogéneo = no cumple ninguno de los casos anteriores
          frecuencias_ps <- dplyr::filter(frecuencias_ps, frecuencias_ps$num_evo != "Sum" & frecuencias_ps$Freq != "NaN")
          frecuencias_ps$ev_psmed <- ifelse(is.na(frecuencias_ps$num_evo) == TRUE, NA,
                                            ifelse(frecuencias_ps$Freq > porc/100, frecuencias_ps$num_evo, NA))
          frecuencias_ps <- frecuencias_ps[, c(1,4)]
          frecuencias_ps <- dplyr::filter(frecuencias_ps, is.na(frecuencias_ps$ev_psmed) == FALSE)
          datos_con_evo_ps <- merge(datos, frecuencias_ps, all.x = TRUE)
          
          # datos_con_evo_ps <- datos_con_evo_ps[, -which(names(datos_con_evo_ps) %in% c("ev_ps1"))]
          datos_con_evo_ps$ev_psmed <- ifelse(is.na(datos_con_evo_ps$ev_psmed) == TRUE, irregular, datos_con_evo_ps$ev_psmed )
          
          ## evolución de pdmed -----------------------------------------------------------------------------------
          # ev_pd1*:
          # 0 = normal = pdmed <90
          # 1 = alta = pdmed =>90 
          datos_con_evo_ps$ev_pd1 <- ifelse(is.na(datos_con_evo_ps[[pdmed]]) == TRUE, NA,
                                            ifelse(datos_con_evo_ps[[pdmed]] >= 90, 1, 0))
          frecuencias_pd <- as.data.frame(round(addmargins(prop.table(table(datos_con_evo_ps$id, datos_con_evo_ps$ev_pd1), margin = 1)), 3))
          frecuencias_pd$Var1 <- as.character(frecuencias_pd$Var1)
          frecuencias_pd$Var1 <- as.numeric(frecuencias_pd$Var1)
          frecuencias_pd$Var2 <- as.character(frecuencias_pd$Var2)
          frecuencias_pd$Var2 <- as.numeric(frecuencias_pd$Var2)
          names(frecuencias_pd) <- c(parse(text = id), "num_evo", "Freq")
          
          # ev_pdmed*:
          # 0 = normal = pdmed < 90 el 80% de las veces
          # 1 = alta = pdmed > 90 el 80% de las veces
          # 1 o 2 según "irregular"= heterogéneo = no cumple ninguno de los casos anteriores
          frecuencias_pd <- dplyr::filter(frecuencias_pd, frecuencias_pd$num_evo != "Sum" & frecuencias_pd$Freq != "NaN")
          frecuencias_pd$ev_pdmed <- ifelse(is.na(frecuencias_pd$num_evo) == TRUE, NA,
                                            ifelse(frecuencias_pd$Freq > porc/100, frecuencias_pd$num_evo, NA))
          frecuencias_pd <- frecuencias_pd[, c(1,4)]
          frecuencias_pd <- dplyr::filter(frecuencias_pd, is.na(frecuencias_pd$ev_pdmed) == FALSE)
          datos_con_evo_ps_pd <- merge(datos_con_evo_ps, frecuencias_pd, all.x = TRUE)
          
          # datos_con_evo_ps_pd <- datos_con_evo_ps_pd[, -which(names(datos_con_evo_ps_pd) %in% c("ev_pd1"))]
          datos_con_evo_ps_pd$ev_pdmed <- ifelse(is.na(datos_con_evo_ps_pd$ev_pdmed) == TRUE, irregular, datos_con_evo_ps_pd$ev_pdmed )
          
          ## Cálculo de ev_TA -----------------------------------------------------------------------------------
          # ev_TA: la evolución de psmed y pdmed (TA en la consulta) a lo largo del tiempo.
          # 0 = Al menos en el 80% de las ocasiones, tanto psmed como pdmed son normales
          # 1 = Al menos en el 80% de las ocasiones, psmed y/o pdmed son altas
          # 1 o 2 según "irregular" = Al menos en el 80% de las ocasiones, una de ellas es irregular
          
          datos_con_evo_ps_pd$ev_TA <- ifelse(datos_con_evo_ps_pd$ev_psmed == 0 & datos_con_evo_ps_pd$ev_pdmed == 0, 0,
                                                 ifelse(datos_con_evo_ps_pd$ev_psmed == 2 | datos_con_evo_ps_pd$ev_pdmed == 2, 2, 1))
                                                
          datos_con_evo_ps_pd <- datos_con_evo_ps_pd[, - which(names(datos_con_evo_ps_pd) %in% c("ev_ps1", "ev_pd1"))]
          datos_con_evo_ps_pd <- dplyr::arrange(datos_con_evo_ps_pd, id, fevisi)
          return(datos_con_evo_ps_pd)
        } else {
          stop("El argumento 'porc' sólo puede ser un valor numérico dentro del intervalo (50,100). Es el porcentaje de evolución.")
        }
      } else {
        stop("El argumento 'irregular' sólo puede tener valores 1 o 2.\n Indica qué hacer cuando no se llega a cumplir el porcentaje.\n Por defecto es 1. Es decir, en los casos heterogéneos.")
      }
    } else {
      stop("Ya existe en tu base de datos una variable llamada 'ev_TA'")
    }
  } else {
    stop("Es imprescindible que la base de datos contenga al menos tres columnas: \n 1. una (y sólo una) llamada como se indica en el argumento 'id',\n 2. una (y sólo una) llamada como se indica en el argumento 'psmed'\n 3. una (y sólo una) llamada como se indica en el argumento 'pdmed'. \n Recuerda que el argumento 'id' por defecto es ''id''.")
  }
}



# prueba1 <- ev_TA(datos = MisDatos, irregular = 2, psmed = "psmed", pdmed = "pdmed", id = "id")
# prueba2 <- ev_TA(datos = MisDatos)
