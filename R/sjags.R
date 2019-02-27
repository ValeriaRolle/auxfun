# Gaussian
sjags <- function(form, data, family = "gaussian", n.iter = 10000, n.chains = 4, n.burnin = 500, n.thin = 2){

      require(R2jags)
      ##################################################################################################################################

      ################################################################## GAUSSIAN ######################################################

      ##################################################################################################################################
      if(family == "gaussian"){

            daticos <- as.data.frame(data)
            mm <- model.matrix(form, daticos)
            mm <- as.data.frame(mm)

            aux <- 1
            y <-all.vars(form)[1]
            aux1 <- list()
            for(i in 2:ncol(mm)){
                  aux1[i] <- assign(paste('x', aux, sep=''), colnames(mm[i]))
                  aux <- aux + 1
            }
            aux1 <- aux1[2:length(aux1)]


            fixedform <- paste("model{
                               for (i in 1:N){
                               y[i] ~ dnorm(pred[i],tau)
                               ")

            formulamodel <- list("pred[i] <- beta0")
            for(i in 1:length(aux1)){
                  formulamodel[1] <- paste(formulamodel[1]," + beta", i,"*x",i,"[i]", sep = "")
            }
            futnames <- vector("character")
            futnames[1] <- "y"
            for(i in 1:length(aux1)){
                  futnames[i+1] <- paste("x", i, sep = "")
            }

            formulamodel[1] <- paste(formulamodel[1], "}")

            betasmodel <- list("\n\nbeta0 ~ dunif(-1000, 1000)\n")
            for(i in 1:length(aux1)){
                  betasmodel[1] <- paste(betasmodel[1],"beta",i," ~ dunif(-1000, 1000)\n", sep = "")
            }

            fixed2 <- paste("\ne <- y[]-pred[]
                            sce <- pow(sd(e[]),2)*(N-1)
                            scy <- pow(sd(y[]),2)*(N-1)
                            r2 <- 1-(sce/scy)
                            tau ~ dgamma(0.5,0.5)
                            sigma2 <- 1/tau")


            modelsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  modelsmodel[1] <- paste(modelsmodel[1],"m",i," <- step(beta",i,")-equals(beta",i,",0)\n",sep = "")

            }

            lsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  lsmodel[1] <- paste(lsmodel[1],"l",i," <- (",i,"-step(beta",i,"))-equals(beta",i,",0)\n",sep = "")
            }

            cat(fixedform,formulamodel[[1]], betasmodel[[1]], fixed2, modelsmodel[[1]],lsmodel[[1]],"}", file = "gaumodel.txt")

            jp1 <- vector()
            jp1 <- c("beta0","r2")
            i <- 1
            while(i <= length(aux1)){

                  jp1 <- c(jp1, paste("beta",i, sep = ""))

                  jp1 <- c(jp1, paste("m",i, sep = ""))

                  jp1 <- c(jp1,paste("l",i, sep = ""))

                  i <- i + 1
            }


            mm$`(Intercept)` <- daticos[,y]
            mm <- as.list(mm)
            N <- nrow(daticos)
            mm[length(mm)+1] <- N
            futnames[length(futnames)+1] <- "N"
            names(mm) <- futnames



            jm1 <- jags(data = mm, parameters.to.save = jp1, model.file = "gaumodel.txt",
                        n.iter = n.iter, n.chains = n.chains, n.burnin = n.burnin, n.thin = n.thin)

            require(ggplot2)
            require(ggthemes)

            plotdata <- jm1$BUGSoutput$sims.list[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list))]
            plotdata <- as.data.frame(plotdata)


            conft <- as.data.frame(jm1$BUGSoutput$summary[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list)),])

            plotlist <- list()
            for(i in c(1:ncol(plotdata))){
                  my.data <- data.frame(V1 = plotdata[,i])

                  x1 <- conft$`2.5%`[i]
                  xend1 <- conft$`97.5%`[i]
                  p1 <- ggplot(data = my.data, aes(x = V1)) +
                        geom_density() +
                        geom_vline(xintercept = 1, linetype = "dashed") +
                        geom_segment(x=x1,y = 0,yend = 0, xend=xend1, color="red") +
                        theme_light()

                  plotlist <- c(plotlist, list(p1))
            }
            names(plotlist)<- names(mm)[2:(length(mm)-1)]



                               }

      ##################################################################################################################################

      ################################################################## BINOMIAL ######################################################

      ##################################################################################################################################
      if(family == "binomial"){


            daticos <- as.data.frame(data)
            rm(data)

            mm <- model.matrix(form, daticos)
            mm <- as.data.frame(mm)

            aux <- 1
            y <-all.vars(form)[1]
            aux1 <- list()
            for(i in 2:ncol(mm)){
                  aux1[i] <- assign(paste('x', aux, sep=''), colnames(mm[i]))
                  aux <- aux + 1
            }
            aux1 <- aux1[2:length(aux1)]


            fixedform <- paste("model{
                               for(i in 1:N){
                               y[i] ~ dbern(prob[i])
                               ")

            formulamodel <- list("logit(prob[i]) <- beta0")
            for(i in 1:length(aux1)){
                  formulamodel[1] <- paste(formulamodel[1]," + beta", i,"*x",i,"[i]", sep = "")
            }

            formulamodel[1] <- paste(formulamodel[1], "}")

            futnames <- vector("character")
            futnames[1] <- "y"
            for(i in 1:length(aux1)){
                  futnames[i+1] <- paste("x", i, sep = "")
            }

            betasmodel <- list("\n\nbeta0 ~ dunif(-1000, 1000)\n")
            for(i in 1:length(aux1)){
                  betasmodel[1] <- paste(betasmodel[1],"beta",i," ~ dunif(-1000, 1000)\n", sep = "")
            }



            modelsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  modelsmodel[1] <- paste(modelsmodel[1],"m",i," <- step(beta",i,")-equals(beta",i,",0)\n",sep = "")

            }

            lsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  lsmodel[1] <- paste(lsmodel[1],"or",i," <- exp(beta",i,")\n",sep = "")
            }

            cat(fixedform,formulamodel[[1]], betasmodel[[1]], modelsmodel[[1]],lsmodel[[1]],"}", file = "binmodel.txt")

            jp1 <- vector()
            jp1 <- c("beta0")
            i <- 1
            while(i <= length(aux1)){

                  jp1 <- c(jp1, paste("beta",i, sep = ""))

                  jp1 <- c(jp1, paste("m",i, sep = ""))

                  jp1 <- c(jp1,paste("or",i, sep = ""))

                  i <- i + 1
            }



            mm$`(Intercept)` <- daticos[,y]
            mm <- as.list(mm)
            N <- nrow(daticos)
            mm[length(mm)+1] <- N
            futnames[length(futnames)+1] <- "N"
            names(mm) <- futnames


            jm1 <- jags(data = mm, parameters.to.save = jp1, model.file = "binmodel.txt",
                  n.iter = n.iter, n.chains = n.chains, n.burnin = n.burnin, n.thin = n.thin)


            require(ggplot2)
            require(ggthemes)

            plotdata <- jm1$BUGSoutput$sims.list[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list))]
            plotdata <- as.data.frame(plotdata)


            conft <- as.data.frame(jm1$BUGSoutput$summary[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list)),])

            plotlist <- list()
            for(i in c(1:ncol(plotdata))){
                  my.data <- data.frame(V1 = plotdata[,i])

                  x1 <- conft$`2.5%`[i]
                  xend1 <- conft$`97.5%`[i]
                  p1 <- ggplot(data = my.data, aes(x = V1)) +
                        geom_density() +
                        geom_vline(xintercept = 1, linetype = "dashed") +
                        geom_segment(x=x1,y = 0,yend = 0, xend=xend1, color="red") +
                        theme_light()

                  plotlist <- c(plotlist, list(p1))
            }
            names(plotlist)<- names(mm)[2:(length(mm)-1)]



      }

      ##################################################################################################################################

      ################################################################## POISSON  ######################################################

      ##################################################################################################################################




      if(family == "poisson"){


            daticos <- as.data.frame(data)
            rm(data)
            mm <- model.matrix(form, daticos)
            mm <<- as.data.frame(mm)

            aux <- 1
            y <-all.vars(form)[1]
            aux1 <- list()
            for(i in 2:ncol(mm)){
                  aux1[i] <- assign(paste('x', aux, sep=''), colnames(mm[i]))
                  aux <- aux + 1
            }
            aux1 <- aux1[2:length(aux1)]


            fixedform <- paste("
                               model{
                               for (i in 1:N){
                               y[i] ~ dpois(lambda[i])
                               log(lambda[i]) <- mu[i]
                               ")

            formulamodel <- list("mu[i] <- beta0")
            for(i in 1:length(aux1)){
                  formulamodel[1] <- paste(formulamodel[1]," + beta", i,"*x",i,"[i]", sep = "")
            }

            formulamodel[1] <- paste(formulamodel[1], "}")

            futnames <- vector("character")
            futnames[1] <- "y"
            for(i in 1:length(aux1)){
                  futnames[i+1] <- paste("x", i, sep = "")
            }

            betasmodel <- list("\n\nbeta0 ~ dunif(-1000, 1000)\n")
            for(i in 1:length(aux1)){
                  betasmodel[1] <- paste(betasmodel[1],"beta",i," ~ dnorm(0,0.001)\n", sep = "")
            }




            modelsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  modelsmodel[1] <- paste(modelsmodel[1],"m",i," <- step(beta",i,")-equals(beta",i,",0)\n",sep = "")

            }

            lsmodel <- list("\n")
            for(i in 1:length(aux1)){
                  lsmodel[1] <- paste(lsmodel[1],"rr",i," <- exp(beta",i,")\n",sep = "")
            }

            cat(fixedform,formulamodel[[1]], betasmodel[[1]], modelsmodel[[1]],lsmodel[[1]],"}", file = "poimodel.txt")

            jp1 <- vector()
            jp1 <- c("beta0","r2")
            i <- 1
            while(i <= length(aux1)){

                  jp1 <- c(jp1, paste("beta",i, sep = ""))

                  jp1 <- c(jp1, paste("m",i, sep = ""))

                  jp1 <- c(jp1,paste("l",i, sep = ""))

                  i <- i + 1
            }


            mm$`(Intercept)` <- daticos[,y]
            mm <- as.list(mm)
            N <- nrow(daticos)
            mm[length(mm)+1] <- N
            names(mm)[length(mm)] <- "N"
            names(mm)[1] <- "y"


            jm1 <- jags(data = mm, parameters.to.save = jp1, model.file = "poimodel.txt",
                         n.iter = n.iter, n.chains = n.chains, n.burnin = n.burnin, n.thin = n.thin)

            require(ggplot2)
            require(ggthemes)

            plotdata <- jm1$BUGSoutput$sims.list[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list))]
            plotdata <- as.data.frame(plotdata)


            conft <- as.data.frame(jm1$BUGSoutput$summary[(length(aux1)*2+3):(length(jm1$BUGSoutput$sims.list)),])

            plotlist <- list()
            for(i in c(1:ncol(plotdata))){
                  my.data <- data.frame(V1 = plotdata[,i])

                  x1 <- conft$`2.5%`[i]
                  xend1 <- conft$`97.5%`[i]
                  p1 <- ggplot(data = my.data, aes(x = V1)) +
                        geom_density() +
                        geom_vline(xintercept = 1, linetype = "dashed") +
                        geom_segment(x=x1,y = 0,yend = 0, xend=xend1, color="red") +
                        theme_light()

                  plotlist <- c(plotlist, list(p1))
            }
            names(plotlist)<- names(mm)[2:(length(mm)-1)]

      }


      bayesults <- list(jm = jm1, plotlist = plotlist)
return(bayesults)
                               }
