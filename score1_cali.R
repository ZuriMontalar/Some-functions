score1_cali <- function (datos, edad = "edad1", sexo = "sexo2", tabaco = "tabaco", psmed = "psmed", col = "col") {
  if (sum(names(datos) == "score1_cali") == 0) {
    # Sintaxis para el cálculo de Score 1 calibrado
    # para calcular el score calibrado, es necesario introducir los siguientes argumentos:
    # edad: nombre de la columna de la bbdd que indica la edad del paciente en años sin decimales
    # sexo: nombre de la columna de la bbdd que indica el sexo de los pacientes. 0 para mujeres; 1 para hombres
    # tabaco: nombre de la columna de la bbdd que indica 0 nunca ha fumado; 1 fumador activo
    # psmed: nombre de la columna de la bbdd que indica presión arterial sistólica media en mmHg
    # col: nombre de la columna de la bbdd que indica colesterol total
    
    n.edad <- which(names(datos) ==  edad) # n.edad es el nº de la columna de datos que se llama como le indicas en edad
    n.sexo <- which(names(datos) ==  sexo) # n.sexo es el nº de la columna de datos que se llama como le indicas en sexo
    n.tabaco <- which(names(datos) ==  tabaco)
    n.psmed <- which(names(datos) ==  psmed)
    n.col <- which(names(datos) ==  col)
    
    datos$score1_cali <- NA
    datos$tabaco_cali <- NA
    if (is.factor(datos$tabaco)) {
      datos$tabaco <- as.numeric(datos[[n.tabaco]]) - 1
      datos$tabaco_cali <- ifelse(is.na(datos[[n.tabaco]]) == TRUE, 0, ifelse(datos[[n.tabaco]] == 2, 0, datos[[n.tabaco]]))
      datos[[n.tabaco]] <- as.factor(datos[[n.tabaco]])      
    } else if (is.numeric(datos[[n.tabaco]]) | is.labelled(datos[[n.tabaco]])) {
      # datos[[n.tabaco]] <- as.numeric(datos[[n.tabaco]])
      datos$tabaco_cali <- ifelse(is.na(datos[[n.tabaco]]) == TRUE, 0, ifelse(datos[[n.tabaco]] == 2, 0, datos[[n.tabaco]]))
    }

    #MUJERES NO FUMADORAS
    #Mujeres no fumadoras entre 40 a 44 años
    datos$score1_cali <- ifelse (datos[[n.edad]] < 45 & datos[[n.sexo]]== 0 & datos$tabaco_cali == 0, 0, datos$score1_cali)
    
    #Mujeres no fumadoras entre 45 a 49 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 140, 0, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 200, 0, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 200,  1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160, 1, datos$score1_cali)
    
    #Mujeres no fumadoras entre 50 a 54 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] < 200, 0, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 200, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 160,  1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 200,  1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 200,  2, datos$score1_cali)
    
    #Mujeres no fumadoras entre 55 a 59 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 120, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 250, 1, datos$score1_cali) 
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 300, 2, datos$score1_cali) 
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 250,  4, datos$score1_cali)
    
    #Mujeres no fumadoras entre 60 a 65 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] < 200, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 250,  2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 200,  3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 200 & datos[[col]] < 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300,  5, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 150, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 150 & datos[[col]] < 200, 5, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 300,  6, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 7, datos$score1_cali)
    
    #MUJERES FUMADORAS
    #Mujeres fumadoras entre 40 a 45 años
    datos$score1_cali <- ifelse (datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1, 0, datos$score1_cali)
    
    #Mujeres fumadoras entre 45 a 50 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[col]] < 250 & datos[[psmed]] < 120,0, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[col]] >= 250 & datos[[psmed]] < 120, 1, datos$score1_cali) 
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 160 & datos[[psmed]] >= 120, 1, datos$score1_cali) 
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[col]] < 200 & datos[[psmed]] >= 160, 1, datos$score1_cali) 
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[col]] >= 200 & datos[[psmed]] >= 160, 2, datos$score1_cali) 
    
    #Mujeres fumadoras entre 50 a 55 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 250, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250, 4, datos$score1_cali)
    
    #Mujeres fumadoras entre 55 a 60 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 150, 1, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 150 & datos[[col]] < 300, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 200 & datos[[col]] < 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 150, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 150 & datos[[col]] < 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250, 5, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 200, 5, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250,6, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 7, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 8, datos$score1_cali)
    
    #Mujeres fumadoras entre 60 a 65 años
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 200, 4, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 200 & datos[[col]] < 250, 5, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250 & datos[[col]] < 300,6, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 300, 7, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 200,6, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 200 & datos[[col]] < 250, 7, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250 & datos[[col]] < 300, 8, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 10, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 200,9, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250, 11, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 12, datos$score1_cali)
    datos$score1_cali <- ifelse (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 0 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 14, datos$score1_cali)
    
    #HOMBRES NO FUMADORES
    #Hombres no fumadores entre 40 a 45 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140, 0, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 250, 0, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 150, 0, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 150, 1, datos$score1_cali)
    
    #Hombres no fumadores entre 45 a 50 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] < 300, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 300, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] < 200, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 4, datos$score1_cali)
    
    #Hombres no fumadores entre 50 a 55 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] < 250, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 150, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 150 & datos[[col]] < 300, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 200 & datos[[col]] < 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]]  < 150, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]]  >= 150 & datos[[col]] < 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]]  >= 250 & datos[[col]] < 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 6, datos$score1_cali)
    
    #Hombres no fumadores entre 55 a 60 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] < 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] < 150, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 150 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] < 150, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 150 & datos[[col]] < 200, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 200 & datos[[col]] < 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 300, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 150, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 150 & datos[[col]] < 200, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 9, datos$score1_cali)
    
    #Hombres no fumadores entre 60 a 65 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] < 150, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 150 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 250 & datos[[col]] < 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 120 & datos[[col]] >= 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] < 200, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 200 & datos[[col]] < 250, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 250 & datos[[col]] < 300, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 140 & datos[[psmed]] >= 120 & datos[[col]] >= 300, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] < 150, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 150 & datos[[col]] < 200, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 200 & datos[[col]] < 250, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] < 160 & datos[[psmed]] >= 140 & datos[[col]] >= 250 & datos[[col]] < 300, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 10, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] < 150, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 150 & datos[[col]] < 200, 9, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250, 10, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 12, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 0 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 14, datos$score1_cali)
    
    #HOMBRES FUMADORES
    #Hombres fumadores entre 40 a 44 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 250, 0, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 250, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 150, 0, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 150, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 250, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250, 2, datos$score1_cali)
    
    #Hombres fumadores entre 45 a 49 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 200, 1, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 250, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 150, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 150 & datos[[col]] < 250, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250 & datos[[col]] < 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 200, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 7, datos$score1_cali)
    
    #Hombres fumadores entre 50 a 54 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 200, 2, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 200 & datos[[col]] < 300, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 300, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] < 200, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 200 & datos[[col]] < 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 250 & datos[[col]] < 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140 & datos[[col]] >= 300, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] < 150, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 150 & datos[[col]] < 200, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 200 & datos[[col]] < 250, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 250 & datos[[col]] < 300, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160 & datos[[col]] >= 300, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] < 150, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 150 & datos[[col]] < 200, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 200 & datos[[col]] < 250, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 250 & datos[[col]] < 300, 10, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160 & datos[[col]] >= 300, 12, datos$score1_cali)
    
    #Hombres fumadores entre 55 a 59 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 150, 3, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 150 & datos[[col]] < 250, 4, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 250 & datos[[col]] < 300, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 300, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] < 200, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 200 & datos[[col]] < 250, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 250 & datos[[col]] < 300, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 300, 9, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] < 150, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 150 & datos[[col]]< 200, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 200 & datos[[col]]< 250, 9, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 250 & datos[[col]]< 300, 11, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 300, 13, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] < 150, 10, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 150 & datos[[col]] < 200, 11, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 200 & datos[[col]] < 250, 13, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 250 & datos[[col]] < 300, 15, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 300, 18, datos$score1_cali)
    
    #Hombres fumadores entre 60 a 56 años
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] < 200, 5, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60 & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 200 & datos[[col]] < 250, 6, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 250 & datos[[col]] < 300, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] < 120 & datos[[col]] >= 300, 9, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] < 150, 7, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 150 & datos[[col]] < 200, 8, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 200 & datos[[col]] < 250, 9, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 250 & datos[[col]] < 300, 11, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 120 & datos[[psmed]] < 140  & datos[[col]] >= 300, 13, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] < 150, 10, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 150 & datos[[col]] < 200, 12, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 200 & datos[[col]] < 250, 14, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 250 & datos[[col]] < 300, 16, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 140 & datos[[psmed]] < 160  & datos[[col]] >= 300, 19, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] < 150, 15, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 150 & datos[[col]] < 200, 17, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 200 & datos[[col]] < 250, 20, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 250 & datos[[col]] < 300, 23, datos$score1_cali)
    datos$score1_cali <- ifelse  (datos[[n.edad]] >= 60  & datos[[n.sexo]] == 1 & datos$tabaco_cali == 1 & datos[[psmed]] >= 160  & datos[[col]] >= 300, 26, datos$score1_cali)
    
    return(datos[, - which(names(datos) == "tabaco_cali")])
  } else {
    stop("Ya existe en tu base de datos una variable llamada 'score1_cali'.")
  }
}

# setwd("D:/R/prediciendo se hace diabetico")
# load("BBDD_E.Rda")
# which(names(BBDD.E) == "score1_cali")
# BBDD.E <- BBDD.E[, -which(names(BBDD.E) == "score1_cali")]
# prueba <- score1_cali(datos = BBDD.E, edad= "edad1", sexo = "sexo2", tabaco = "tabaco", psmed = "psmed", col = "col")
# prueba2 <- score1_cali(datos = BBDD.E)
