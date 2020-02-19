




tabla3 <- function(dd, grupo,grupo_char, digits = 3, decimal.symbol = ",", test = "median"){
     
     stopifnot(is.character(grupo))
     require(coin)
     # grupo_get <- dd[, grupo]
     
     n_numeric_names <- colnames(dd)[sapply(dd, class) == "numeric"] # guardo los nombres de las variables numéricas
     n_others_names  <- colnames(dd)[sapply(dd, class) != "numeric"] # guardo los  nombres de las variables no numéricas
     
     numeric <- dd[,n_numeric_names, drop = FALSE] # subset de numéricas
     other   <- dd[,n_others_names[-which(n_others_names == grupo_char)], drop = FALSE]  # subset del resto
     
     n_numeric <- sum(sapply(dd, class) == "numeric") # extraigo cuántas continuas hay
     
     if(ncol(other) == 0){ # Para que no pete a la hora de crear el data.frame, si no hay ninguna categórica, elimino el subset vacío
          rm(other)
     }     

     
     if(ncol(numeric) == 0){ # Para que no pete a la hora de crear el data.frame, si no hay ninguna continua, elimino el subset vacío
          rm(numeric)
     }
     
     unique.grupo <- unique(grupo)[order(unique(grupo))]
     
     n_grupo <- length(unique(grupo)) # Extraigo el número de categorías en la variable de agrupación
     

     
     # Numéricas ----
     
     if(exists("numeric", mode = "list")){ # Si el subset existe...
          
          
         
          
          tabla2 <- as.data.frame(matrix(ncol = length(unique(grupo))*4+1)) # Creo una matriz que tenga cuatro columnas por cada categoría más una para indicar variable

          
          colnames(tabla2)[1] <- "Variable" # La primera columna indica variable
          
          
          # Colnames ----
          
               j <- 2 # j determina columnas
          for(i in 1:n_grupo){ # mientras i cicla por el número de categorías de la variable de agrupación

               colnames(tabla2)[j]   <- paste("Mean",   unique(grupo)[i])
               colnames(tabla2)[j+1] <- paste("SD",     unique(grupo)[i])
               colnames(tabla2)[j+2] <- paste("Median", unique(grupo)[i])
               colnames(tabla2)[j+3] <- paste("IQR",    unique(grupo)[i])
               j <- j + 4
               
          }
               rm(list = c("i", "j"))
               
          # Descriptivo ----
          
          for(i in 1:ncol(numeric)){ 
               
               for(j in 1:n_grupo){
               tabla2[i,"Variable"   ]                      <- colnames(numeric)[i]
               tabla2[i, paste("Mean",   unique(grupo)[j])] <- round(mean(          numeric[grupo == unique(grupo)[j],i], na.rm = TRUE), digits)
               tabla2[i, paste("SD",     unique(grupo)[j])] <- round(sd(            numeric[grupo == unique(grupo)[j],i], na.rm = TRUE), digits)
               tabla2[i, paste("Median", unique(grupo)[j])] <- round(median(        numeric[grupo == unique(grupo)[j],i], na.rm = TRUE), digits)
               tabla2[i, paste("IQR",    unique(grupo)[j])] <- paste(paste(quantile(numeric[grupo == unique(grupo)[j],i], na.rm = TRUE)[2], "to", paste(quantile(numeric[,i], na.rm = TRUE)[4])))
               
               }
               
          }
               
          if(n_grupo == 2){
          
                    if(test %in% c("both", "mean")){
                         # t-test ----
                    
                    tabla2$t.test.CI.lw  <- round(sapply(numeric,
                         function(x) t.test(as.numeric(x[grupo == unique.grupo[1]]),      as.numeric(x[grupo == unique.grupo[2]]))$conf.int)[1,],digits)
                    tabla2$t.test.CI.up  <- round(sapply(numeric,
                         function(x) t.test(as.numeric(x[grupo == unique.grupo[1]]),      as.numeric(x[grupo == unique.grupo[2]]))$conf.int)[2,],digits)
                    tabla2$t.test.dif    <- round(sapply(numeric,
                         function(x)  (mean(as.numeric(x[grupo == unique.grupo[1]]), na.rm = TRUE)-mean(as.numeric(x[grupo == unique.grupo[2]]),na.rm = TRUE))),digits)
                    tabla2$t.test.pval   <- signif(sapply(numeric,
                         function(x) t.test(as.numeric(x[grupo == unique.grupo[1]]),      as.numeric(x[grupo == unique.grupo[2]]))$p.value),3)          
                    
                    }
                        
                       
                    if(test %in% c("both", "median")){
                              
                    # Wilcoxon ----
                    
                    tabla2$wx.test.CI.lw  <- as.numeric(round(sapply(numeric, function(x) c(confint(wilcox_test(x ~ as.factor(grupo), conf.int = TRUE))[[1]][1])),digits))
                    tabla2$wx.test.CI.up  <- as.numeric(round(sapply(numeric, function(x) confint(wilcox_test(x ~ as.factor(grupo), conf.int = TRUE))[[1]][2]),digits))
                    tabla2$wx.test.dif    <- round(sapply(numeric, function(x) confint(wilcox_test(x ~ as.factor(grupo), conf.int = TRUE))[[2]]),digits)
                    tabla2$wx.test.pval   <- signif(sapply(numeric, function(x) pvalue(wilcox_test(x ~ as.factor(grupo)))),3)
                    
                    
                    }
               
          
          
          } else if(n_grupo > 2){  # end n_grupo == 2
               
               print("not yet")
          }
               
          
     }else{ # end exists("numeric")
          tabla2 <- NA
     } # end exists("numeric")
     
     
     
     
     
     
     
     
     
     # Categóricas ----
     
     if(exists("other")){
          
          unique.grupo <- unique(grupo)[order(unique(grupo))]
          n_others  <- sum(sum(sapply(other, function(x) length(table(x))))) # extraigo cuántas categóricas hay contando contando con que cada categórica cuenta su número de categorías
          
          
          lista_aux_other <- sapply(other, function(x) paste(levels(as.factor(x)))) # extraigo las categorías en cada columna a una lista
          
          if(is.null(names(lista_aux_other))){
               
               
               lista_aux_other <- list("name" = lista_aux_other[,1])
               names(lista_aux_other) <- names(other)
          }

          lista_aux_other <- lista_aux_other[names(lista_aux_other)[order(names(lista_aux_other))]] # ordeno la lista alfabéticamente por nombre
          
          categorias <- lista_aux_other # renombro por conveniencia
          
          
          
          
          
          
          tabla3 <- as.data.frame(matrix(ncol = length(unique.grupo)*2+2)) # Creo una matriz que tenga cuatro columnas por cada categoría más una para indicar variable
          tabla3[1:length(matrix(unlist(categorias))),1] <- NA # alargo tabla 3 hasta el número de categorías para poder insertar los nombres de las variables y las categorías como columnas
          
          colnames(tabla3)[1] <- "Variable" # La primera columna indica variable
          colnames(tabla3)[2] <- "Category" # La primera columna indica variable
          
          j <- 3 # j determina columnas
          
          for(i in 1:n_grupo){ # mientras i cicla por el número de categorías de la variable de agrupación
               
               colnames(tabla3)[j]   <- paste("N",          unique.grupo[i])
               colnames(tabla3)[j+1] <- paste("Percentage", unique.grupo[i])
               j <- j + 2
               
          }
          
          rm(list = c("i", "j"))
          
          
          
          
 
          
          variables <- rep(names(lista_aux_other)[1], length(lista_aux_other[[1]])) # repite el primer nombre de la lista, la longitud de su interior veces
          
          
          if(length(names(other)) > 1){
               for(i in (names(lista_aux_other))[2:length(names(lista_aux_other))]){ # Añade el resto de nombres
                    
                    variables <- c(variables,rep(names(lista_aux_other)[names(lista_aux_other) == i], length(lista_aux_other[[i]])) )
                    
               }
          
          }
          variables <- split(variables, variables)
          
          # valores_N   <-  sapply(other, function(x) c(table(x, grupo)))
          # valores_N <- valores_N[names(valores_N)[order(names(valores_N))]]
          # valores_per <- (sapply(other, function(x) c(prop.table(table(x, grupo),2))))
          # valores_per <- valores_per[names(valores_per)[order(names(valores_per))]]
          
     
          tabla3$Variable <- matrix(unlist(variables))
          tabla3$Category <- matrix(unlist(categorias))
          
          
          list.n <- list()
          
          for(i in 1:length(unique(variables))){
               
               list.n[[i]] <- data.frame(table(other[,unique(names(variables))[i]], grupo))
               
          }
          
          names(list.n) <- unique(names(variables))
          
          
          
          list.porc <- list()
          
          for(i in 1:length(unique(variables))){
               
               list.porc[[i]] <- data.frame(prop.table(table(other[,unique(names(variables))[i]], grupo),2))
               
          }
          
          names(list.porc) <- unique(names(variables))
          
          
          

          for(g in unique.grupo){
               
               for(v in unique(tabla3$Variable)){
                    
                    for(j in unique(tabla3$Category[tabla3$Variable == v])){
                         
                         tabla3[tabla3$Variable == v & tabla3$Category == j, paste("N", g)] <- list.n[[v]][list.n[[v]]$Var1 == j & list.n[[v]]$grupo == g,3]
                         tabla3[tabla3$Variable == v & tabla3$Category == j, paste("Percentage", g)] <- round(list.porc[[v]][list.porc[[v]]$Var1 == j & list.porc[[v]]$grupo == g,3], digits)
                         
                              
                    }
                     
                    
               }
          
          }
          
          
          no_cons <- colnames(other[,-which(sapply(other, function(x) length(unique(na.omit(x)))==1) != FALSE)])
          
          pvals <- sapply(other[,no_cons], function(x) fisher.test(x, grupo, conf.int = TRUE)$p.value)
          pvals <- data.frame(var = rownames(data.frame(pvals)), pvalue = pvals, row.names = NULL)
          
          
          tabla3$"Fisher p-value" <- NA
          pvals$pvalue <- round(pvals$pvalue, digits)
          
          for(i in pvals$var){
               tabla3$"Fisher p-value"[tabla3$Variable == i] <- pvals$pvalue[pvals$var == i]
               
          }
          
           
          
          
     }else{
          
          tabla3 <- NA
          
     }
     return(list("Cont" = tabla2, "Cat" = tabla3))
     
}
