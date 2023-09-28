score2 <-function(datos, edad = "edad1", sexo = "sexo2", tabaco = "tabaco", psmed = "psmed", col = NULL, hdl = NULL, col_noHDL = NULL){
  # LOS ARGUMENTOS DE COLESTEROL SON OBLIGATORIOS.
  
  # para calcular el score 2, es necesario introducir los siguientes argumentos:
  # edad: nombre de la columna de la bbdd que indica la edad del paciente en años sin decimales
  # sexo: nombre de la columna de la bbdd que indica el sexo de los pacientes. 0 para mujeres; 1 para hombres
  # tabaco: nombre de la columna de la bbdd que indica 0 nunca ha fumado; 1 fumador activo
  # psmed: nombre de la columna de la bbdd que indica presión arterial sistólica media en mmHg
  # col: nombre de la columna de la bbdd que indica colesterol total
  # hdl: nombre de la columna de la bbdd que indica colesterol hdl
  # col_noHDL : nombre de la columna de la bbdd que indica colesterol no hdl
  
  # esta función, o bien usará los valores de col y had, o bien los de col_noHDL
  if (sum(names(datos) == parse(text = edad)) == 1 &      # si hay UNA variable en la bbdd que se llama como se indica en el argumento edad,
      sum(names(datos) == parse(text = psmed)) == 1 &   # UNA variable que se llama como se indica en el argumento psmed y
      sum(names(datos) == parse(text = tabaco)) == 1 &   # UNA variable que se llama como se indica en el argumento tabaco y
      sum(names(datos) == parse(text = sexo)) == 1) {  # UNA variable que se llama como se indica en el argumento sexo:
    if (sum(names(datos) == "score2") == 0) {
      n.edad <- which(names(datos) ==  edad) # n.edad es el nº de la columna de datos que se llama como le indicas en edad
      n.sexo <- which(names(datos) ==  sexo) # n.sexo es el nº de la columna de datos que se llama como le indicas en sexo
      n.tabaco <- which(names(datos) ==  tabaco)
      n.psmed <- which(names(datos) ==  psmed)
      
      if(is.null(col) == FALSE & is.null(hdl) == FALSE) {
        if(sum(names(datos) == parse(text = col)) == 1 &  # si hay UNA variable en la bbdd que se llama como se indica en el argumento col
           sum(names(datos) == parse(text = hdl)) == 1) { # y hay UNA variable que se llama como se indica en el argumento hdl:
          n.col <- which(names(datos) ==  col)
          n.hdl <- which(names(datos) ==  hdl)
          datos$col_noHDL <- datos[[n.col]] - datos[[n.hdl]]
          n.col_noHDL <- which(names(datos) ==  "col_noHDL")
        } else {
          stop("O bien no existen en tu base de datos unas variables llamada como has indicado en los argumentos 'col' y/o 'hdl', o bien existen más de una de cada variables llamadas así. \n Es imprescindible que la base de datos contenga: \n 1. Bien una (y sólo una) llamada como se indica en el argumento 'col' Y otra (sólo una) como indica el argumento 'hdl',\n 2. O bien una (y sólo una) variable llamada como se indica en el argumento 'col_noHDL'.")
        }
      } else if (is.null(col_noHDL) == FALSE) {
        if(sum(names(datos) == parse(text = col_noHDL)) == 1) {
          n.col_noHDL <- which(names(datos) ==  col_noHDL)
        } else{
          stop("O bien no existe en tu base de datos una variable llamada como has indicado en el argumento 'col_noHDL', o bien existe más de una variable llamada así. \n Es imprescindible que la base de datos contenga: \n 1. Bien una (y sólo una) llamada como se indica en el argumento 'col' Y otra (sólo una) como indica el argumento 'hdl',\n 2. O bien una (y sólo una) variable llamada como se indica en el argumento 'col_noHDL'.")
        }
      } else {
        stop("Es imprescindible que la base de datos contenga: \n 1. Bien una (y sólo una) llamada como se indica en el argumento 'col' Y otra (sólo una) como indica el argumento 'hdl',\n 2. O bien una (y sólo una) variable llamada como se indica en el argumento 'col_noHDL'.")
      }
      
      # Sintaxis para el cálculo de Score 2
      datos$score2 <- NA
      
      #MUJERES NO FUMADORAS
      #Mujeres no fumadoras entre 40 a 44 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 140, 1, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 1, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.col_noHDL]] >= 150 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.col_noHDL]] < 250 & datos[[n.psmed]] >= 160, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.col_noHDL]] >= 250 & datos[[n.psmed]] >= 160, 3, datos$score2)
      
      #Mujeres no fumadoras entre 45 a 49 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120, 1, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] <150, 1, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 250, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150, 3, datos$score2)
      
      #Mujeres no fumadoras entre 50 a 55 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 250, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150, 4, datos$score2)
      
      #Mujeres no fumadoras entre 55 a 59 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]]< 200, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]]>= 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150, 5, datos$score2)
      
      #Mujeres no fumadoras entre 60 a 64 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >=  200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200, 7, datos$score2)
      
      #Mujeres no fumadoras entre 65 a 69 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200, 9, datos$score2)
      
      #MUJERES FUMADORAS
      #Mujeres fumadoras entre 40 y 45 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 6, datos$score2)
      
      #Mujeres fumadoras entre 45 y 50 años 
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 7, datos$score2)
      
      #Mujeres fumadoras entre 50 y 55 años 
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      
      #Mujeres fumadoras entre 55 y 60 años 
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      
      #Mujeres fumadoras entre 60 y 65 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >=  200, 11, datos$score2)
      
      # Mujeres fumadoras entre  65 y 70 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 12, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 0 & datos[[n.tabaco]] == 1 &  datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200, 13, datos$score2)
      
      #HOMBRES NO FUMARES
      #Hombres no fumadores de entre 40 y 45 años, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 1, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140  & datos[[n.col_noHDL]] >= 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160  & datos[[n.col_noHDL]] < 150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160  & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] <45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160  & datos[[n.col_noHDL]] >= 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160  & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200, 5, datos$score2)
      
      #Hombres no fumadores de entre 45 y 50 años, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] <150, 2, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] <150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] <150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] <200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200, 6, datos$score2)
      
      #Hombres no fumadores entre 50 y 55 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 250, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      
      #Hombres no fumadores entre 55 y 60 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >=150 & datos[[n.col_noHDL]] < 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >=150 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >=200 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 200, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >=  200 & datos[[n.col_noHDL]] < 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 9, datos$score2)
      
      #Hombres no fumadores entre 60 y 65 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] <150, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 11, datos$score2)
      
      #Hombres no  fumadores entre 65 y 70 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140  & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140  & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140  & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 12, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 0 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 13, datos$score2)
      
      #HOMBRES FUMADORES
      #Hombre fumadores entre 40 y 45 años 
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 40 & datos[[n.edad]] < 45 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      
      #Hombres fumadores de entre 45 y 50 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 3, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 45 & datos[[n.edad]] < 50 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 11, datos$score2)
      
      #Hombres fumadores de entre 50 y 55 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 4, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 5, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 50 & datos[[n.edad]] < 55 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 13, datos$score2)
      
      #Hombre fumador entre 55 y 60 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 200, 6, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 12, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 12, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 13, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 55 & datos[[n.edad]] < 60 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 15, datos$score2)
      
      #Hombres fumadores entre 60 y 65 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 7, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 8, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 250, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 250, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 13, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 14, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 13, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 14, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 15, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 60 & datos[[n.edad]] < 65 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 17, datos$score2)
      
      #Hombres fumadores de 65 a 70 años
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] < 150, 9, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 10, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] < 120 & datos[[n.col_noHDL]] >= 200, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] < 150, 11, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200 , 12, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 120 & datos[[n.psmed]] < 140 & datos[[n.col_noHDL]] >= 200 , 13, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] < 150, 13, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 14, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 15, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 140 & datos[[n.psmed]] < 160 & datos[[n.col_noHDL]] >= 250, 16, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] < 150, 15, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 150 & datos[[n.col_noHDL]] < 200, 16, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 200 & datos[[n.col_noHDL]] < 250, 17, datos$score2)
      datos$score2 <- ifelse (datos[[n.edad]] >= 65 & datos[[n.edad]] < 70 & datos[[n.sexo]] == 1 & datos[[n.tabaco]] == 1 & datos[[n.psmed]] >= 160 & datos[[n.col_noHDL]] >= 250, 19, datos$score2)
      
      # datos <- datos[, - which(names(datos) == "col_noHDL")]
      return(datos)
    } else {
      stop("Ya existe en tu base de datos una variable llamada 'score2'")
    }
  } else {
    stop("Es imprescindible que la base de datos contenga al menos estas columnas: \n 1. una (y sólo una) llamada como se indica en el argumento 'sexo',\n 2. una (y sólo una) llamada como se indica en el argumento 'psmed'\n 3. una (y sólo una) llamada como se indica en el argumento 'tabaco'. \n 4. una (y sólo una) llamada como se indica en el argumento 'edad'.")
  }
}

# setwd("D:/R/prediciendo se hace diabetico")
# load("BBDD_E.Rda")
# which(names(BBDD.E) == "score2")
# which(names(BBDD.E) == "col_noHDL")
# BBDD.E <- BBDD.E[, -which(names(BBDD.E) == "score2")]
# # BBDD.E <- BBDD.E[, -which(names(BBDD.E) == "col_noHDL")]
# prueba <- score2(datos = BBDD.E)
# prueba2 <- score2(datos = BBDD.E, edad= "edad1", sexo = "sexo2", tabaco = "tabaco", psmed = "psmed", col = "col", hdl = "hdl")
# prueba3 <- score2(datos = BBDD.E, col_noHDL = "col_noHDL")


