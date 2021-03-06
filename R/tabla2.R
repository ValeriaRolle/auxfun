

tabla2 <- function(dd, digits = 2, decimal.symbol = ","){

     n_numeric_names <- colnames(dd)[sapply(dd, class) == "numeric"] # y sus nombres
     n_others_names  <- colnames(dd)[sapply(dd, class) != "numeric"]

     numeric <- dd[,n_numeric_names] # subset de numéricas
     other   <- dd[,n_others_names]  # subset del resto

     n_numeric <- sum(sapply(dd, class) == "numeric") # extraigo cuántas continuas hay

     if(ncol(other) == 0){ # Para que no pete a la hora de crear el data.frame, si no hay ninguna categórica
          rm(other)
     }

     if(ncol(numeric) == 0){ # Para que no pete a la hora de crear el data.frame, si no hay ninguna continua
          rm(numeric)
     }


     # Numéricas ----

     if(exists("numeric")){

          tabla2 <- data.frame("Variable" = rep(NA, n_numeric), "Mean" = NA, "SD" = NA, "Median" = NA, "IQR" = NA) # data frame para continuas


          for(i in 1:ncol(numeric)){

               tabla2[i,"Variable"] <- colnames(numeric)[i]
               tabla2[i,"Mean"]     <- round(mean(numeric[,i], na.rm = TRUE), digits)
               tabla2[i,"SD"]       <- round(sd(numeric[,i], na.rm = TRUE), digits)
               tabla2[i,"Median"]   <- round(median(numeric[,i], na.rm = TRUE), digits)
               tabla2[i,"IQR"]      <- paste(paste(round(quantile(numeric[,i], na.rm = TRUE)[2], digits), "to", paste(round(quantile(numeric[,i], na.rm = TRUE)[4], digits))))

          }

     }else{
          tabla2 <- NA
     }

     # Categóricas ----

     if(exists("other")){

          n_others  <- sum(sum(sapply(other, function(x) length(table(x))))) # extraigo cuántas categóricas hay contando contando con que cada categórica cuenta su número de categor�?as


          tabla3 <- data.frame("Variable" = rep(NA, n_others), "Category" = NA, "N" = NA, "Percentage" = NA) # data frame para categóricas

          lista_aux_other <- sapply(other, function(x) paste(levels(as.factor(x))))

          categorias <- paste(unlist(lista_aux_other))

          variables <- rep(names(lista_aux_other)[1], length(lista_aux_other[[1]])) # repite el primer nombre de la lista, la longitud de su interior veces
          for(i in (names(lista_aux_other))[2:length(names(lista_aux_other))]){ # Añade el resto de nombres

               variables <- c(variables,rep(names(lista_aux_other)[names(lista_aux_other) == i], length(lista_aux_other[[i]])) )

          }


          valores_N <- unlist(sapply(unique(variables), function(x) table(dd[,x])))
          valores_per <- unlist(sapply(unique(variables), function(x) prop.table(table(dd[,x]))))




          for(i in 1:length(categorias)){

               tabla3[i,"Variable"]  <- variables[i]
               tabla3[i,"Category"] <- categorias[i]
               tabla3[i,"N"] <- valores_N[[i]]
               tabla3[i,"Percentage"] <- round(valores_per[[i]]*100,digits)


          }

     }else{
          tabla3 <- NA
     }
     return(list("Cont" = tabla2, "Cat" = tabla3))
}

