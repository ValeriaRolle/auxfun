ggtoppt <- function(gg, export = FALSE, pptname = "exportedplots.pptx", width = 8, height = 6){
   require(rvg)
   require(officer)

   if(export == FALSE){
      if(class(gg)[1] == "gg"){

         if(exists("filename")){
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename, dml(ggobj = gg), location = ph_location(width = width, height = height))
         }else{
            filename <- read_pptx()
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename, dml(ggobj = gg), location = ph_location(width = width, height = height))
            filename <<- filename
         }

      }else if(class(gg)[1] == "function"){
         if(exists("filename")){
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename, dml(gg()), location = ph_location(width = width, height = height))
         }else{
            filename <- read_pptx()
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename, dml(gg()), location = ph_location(width = width, height = height))
            filename <<- filename
         }
      }else if(class(gg)[1] == "ggsurvplot"){
         if(exists("filename")){
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename, dml(ggobj = gg), location = ph_location(width = width, height = height))
         }else{
            filename <- read_pptx()
            filename <- add_slide(filename, layout = "Title and Content", master = "Office Theme")
            filename <- ph_with(filename,dml(ggobj = gg), location = ph_location(width = width, height = height))
            filename <<- filename
         }
      }else{
         print("Check class")
      }
   }else{
      print(filename, target = pptname)
   }
}

fptable <- function(columna, var_name = "Variable", caption = "", na = TRUE, format = "markdown", digits = 3){

      require(knitr)

      if(na == TRUE){
      # Creo op.table, que contiene un table() de la variable ----
      op.table <- as.data.frame(table(columna, useNA = "always"))

      # Anado a op.table las proporciones de cada valor, redondeadas a digits. ----
      op.table$prop  <- (round(prop.table(table(columna, useNA = "always")),digits)*100)
      }
      if(na == FALSE){
            # Creo op.table, que contiene un table() de la variable ----
            op.table <- as.data.frame(table(columna))

            # Anado a op.table las proporciones de cada valor, redondeadas a 3. ----
            op.table$prop  <- (round(prop.table(table(columna)),digits)*100)
      }
      # Creo una fila nueva en blanco al final de op.table ----
      auxdt <- matrix(c(op.table[2,1],1,1),nrow = 1,ncol = 3) # Creo una matriz con tres filas y una sola columna, la primera celda sera el segundo valor de la primera columna, para que no se queje de que no es un factor.

      colnames(auxdt) <- colnames(op.table) # le pongo los mismos colnames que op.table para que no proteste
      op.table[,1] <- as.character(op.table[,1])
      op.table <- rbind(op.table, auxdt) # las junto, ahora ya tengo la fila nueva en "blanco".

      #op.table[,1] <- as.character(op.table[,1]) # Si no, no admite que le ponga "TOTAL"
      colnames(op.table) <- c("Variable", "Frecuencia", "Proporcion")
      op.table[nrow(op.table),1] <- "TOTAL" # Le digo que el ultimo valor de esta columna se llame TOTAL

      op.table$Frecuencia <- as.numeric(op.table$Frecuencia) # Para que lo alinee al lado que es.

      op.table$Frecuencia[nrow(op.table)] <- sum(op.table$Frecuencia[1:(nrow(op.table)-1)]) # Suma de las frecuencias
      op.table$Proporcion[nrow(op.table)] <- (round(sum(round(prop.table(table(columna)),7)),digits)*100) # Suma de las proporciones
      #op.table$Proporcion <- as.numeric(op.table$Proporcion) # a numerico???
      colnames(op.table) <- c(var_name, "Freq.", "%")
      kable(op.table, caption = caption, format = format)
}

ortable <- function(m1, nvar){

      if(class(m1)[1] == "coxph") {
            m1 <- summary(m1)
            table1 <- round(m1$conf.int,4)
            table1 <- cbind(table1, coef(m1)[,5])
            colnames(table1) <- c("HazardRatio", "Inv.HazardRatio", "Limite.inf.", "Limite.sup.", "p.value")
            return(table1)
      }else if(class(m1)[1] == "glm"){
            OR <- exp(summary(m1)$coefficients[1:(nvar+1),1])
            `Limite.sup.` <- exp(summary(m1)$coefficients[1:(nvar+1),1]+1.96*summary(m1)$coefficients[1:(nvar+1),2])
            `Limite.inf.` <- exp(summary(m1)$coefficients[1:(nvar+1),1]-1.96*summary(m1)$coefficients[1:(nvar+1),2])
            `p.valor` <- summary(m1)$coefficients[,4]
            AIC <- summary(m1)$aic
            sum1 <- round(cbind(`Limite.inf.`, OR, `Limite.sup.`, `p.valor`, AIC),3)

            sum1 <- as.data.frame(sum1)
            sum1 <- sum1[-1,]
            return(sum1)
      }else{
            print("The model you entered is not a survival model nor glm model")
      }
}

ssummary <- function(columna, var_name = "Variable", caption = "", decimals = 2, formato = "markdown", na = TRUE){
      require(knitr)

      if(any(is.na(columna)) == TRUE){

            aux <- round(summary(columna, useNA = "always"),3)
            aux <- as.factor(aux)
            aux <- as.data.frame(aux, names(aux))
            aux$aux <- round(as.numeric(summary(columna)),decimals)
            sd  <- sd(columna, na.rm = TRUE)
            sd  <- data.frame(aux = round(sd,decimals))
            aux <- rbind(aux, sd)
            rownames(aux)[8] <- "SD"

            aux1 <- rbind(aux[1,],aux[2,],aux[3,],aux[5,],aux[6,],aux[4,],aux[8,],aux[7,])
            rownames(aux1) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.", "Mean", "SD", "NA")
            # kable(aux1, caption = caption, col.names = var_name, format = formato)

      }else if(any(!is.na(columna)) == TRUE | na == FALSE){

            aux <- round(summary(columna),3)
            aux <- as.factor(aux)
            aux <- as.data.frame(aux, names(aux))
            aux$aux <- round(as.numeric(summary(columna)),decimals)
            sd <- round(sd(columna), decimals)
            sd <- data.frame(aux = sd)
            aux <- rbind(aux, sd)

            aux1 <- rbind(aux[1,],aux[2,],aux[3,],aux[5,],aux[6,],aux[4,],aux[7,])
            rownames(aux1) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.", "Mean", "SD")

      }
      kable(aux1, caption = caption, col.names = var_name, format = formato)
}

vcol <- function(datos, ver = FALSE){
      if(ver){
            View(as.data.frame(colnames(datos)))
      }else if(ver == FALSE){
            data.frame(ColNames = colnames(datos), N = c(1:length(colnames(datos))))
      }
}

spsstoR <- function(columna, as.Date = TRUE){
      columna <- ISOdate(1582,10,14) + columna
      columna <- as.Date(columna)
      return(columna)
}


crl_ga_eq <- function(){

   noquote("23.73+8.052*(1.037*dd$crl)^0.5")
}

#

crl_ga <- function(crl){

   23.73+8.052*(1.037*crl)^0.5
}

coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {

   # Source: https://alistaire.rbind.io/blog/coalescing-joins/

   joined <- join(x, y, by = by, suffix = suffix, ...)
   # names of desired output
   cols <- union(names(x), names(y))

   to_coalesce <- names(joined)[!names(joined) %in% cols]
   suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
   # remove suffixes and deduplicate
   to_coalesce <- unique(substr(
      to_coalesce,
      1,
      nchar(to_coalesce) - nchar(suffix_used)
   ))

   coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
   ))
   names(coalesced) <- to_coalesce

   dplyr::bind_cols(joined, coalesced)[cols]
}


plot_coefs1 <- function (..., ci_level = 0.95, inner_ci_level = NULL, model.names = NULL, 
          coefs = NULL, omit.coefs = c("(Intercept)", "Intercept"), 
          colors = "CUD Bright", plot.distributions = FALSE, rescale.distributions = FALSE, 
          exp = FALSE, point.shape = TRUE, point.size = 3, legend.title = "Model", 
          groups = NULL, facet.rows = NULL, facet.cols = NULL, facet.label.pos = "top", 
          color.class = colors, resp = NULL, dpar = NULL) 
{
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop_wrap("Install the broom package to use the plot_coefs function.")
  }
  if (!requireNamespace("ggstance", quietly = TRUE)) {
    stop_wrap("Install the ggstance package to use the plot_coefs function.")
  }
  if (!all(color.class == colors)) 
    colors <- color.class
  model <- term <- estimate <- conf.low <- conf.high <- conf.low.inner <- conf.high.inner <- curve <- est <- NULL
  dots <- list(...)
  if (inherits(dots[[1]], "list")) {
    mods <- dots[[1]]
    if (is.null(model.names) && !is.null(names(mods))) {
      if (is.null(model.names)) 
        model.names <- names(mods)
    }
    if (length(dots) > 1) {
      ex_args <- dots[-1]
    }
    else {
      ex_args <- NULL
    }
  }
  else if (!is.null(names(dots))) {
    if (all(nchar(names(dots))) > 0) {
      models <- !is.na(sapply(dots, function(x) {
        out <- find_S3_class("tidy", x, package = "generics")
        if (out %in% c("list", "character", "logical", 
                       "numeric", "default")) {
          out <- NA
        }
      }))
      mods <- dots[models]
      if (is.null(model.names)) 
        model.names <- names(dots)[models]
      if (!all(models)) {
        ex_args <- dots[models]
      }
      else {
        ex_args <- NULL
      }
    }
    else {
      mods <- dots[names(dots) == ""]
      ex_args <- dots[names(dots) != ""]
    }
  }
  else {
    mods <- dots
    ex_args <- NULL
  }
  if (!is.null(omit.coefs) && !is.null(coefs)) {
    if (any(omit.coefs %nin% c("(Intercept)", "Intercept"))) {
      msg_wrap("coefs argument overrides omit.coefs argument. Displaying\n               coefficients listed in coefs argument.")
    }
    omit.coefs <- NULL
  }
  if (!is.null(model.names)) {
    names(mods) <- model.names
  }
  
  
  tidies <- make_tidies(mods = mods, ex_args = ex_args, ci_level = ci_level, 
                        model.names = model.names, omit.coefs = omit.coefs, coefs = coefs, 
                        resp = resp, dpar = dpar)
  print(tidies)
  
  
  
  
  
  
  
  
  
  
  n_models <- length(unique(tidies$model))
  
  if (!is.null(inner_ci_level)) {
    if (plot.distributions == FALSE || n_models == 1) {
      tidies_inner <- make_tidies(mods = mods, ex_args = ex_args, 
                                  ci_level = inner_ci_level, model.names = model.names, 
                                  omit.coefs = omit.coefs, coefs = coefs)
      tidies_inner$conf.low.inner <- tidies_inner$conf.low
      tidies_inner$conf.high.inner <- tidies_inner$conf.high
      tidies_inner <- tidies_inner[names(tidies_inner) %nin% 
                                     c("conf.low", "conf.high")]
      tidies <- merge(tidies, tidies_inner, by = c("term", 
                                                   "model"), suffixes = c("", ".y"))
    }
    else {
      msg_wrap("inner_ci_level is ignored when plot.distributions == TRUE and\n                more than one model is used.")
      inner_ci_level <- NULL
    }
  }
  if (exp == TRUE) {
    if (plot.distributions == TRUE) {
      warn_wrap("Distributions cannot be plotted when exp == TRUE.")
      plot.distributions <- FALSE
    }
    exp_cols <- c("estimate", "conf.low", "conf.high")
    if (!is.null(inner_ci_level)) {
      exp_cols <- c(exp_cols, "conf.low.inner", "conf.high.inner")
    }
    tidies[exp_cols] <- exp(tidies[exp_cols])
  }
  
  
  if (!is.null(groups)) {
    tidies$group <- NA
    for (g in seq_len(length(groups))) {
      if (is.null(names(groups)) || names(groups)[g] == 
          "") {
        tidies$group[tidies$term %in% groups[[g]]] <- as.character(g)
      }
      else {
        tidies$group[tidies$term %in% groups[[g]]] <- names(groups)[g]
      }
    }
    if (plot.distributions == TRUE) {
      warn_wrap("Distributions cannot be plotted when groups are used.")
    }
  }
  
  
  p <- ggplot(data = tidies, aes(y = term, x = estimate, xmin = conf.low, 
                                 xmax = conf.high))
  if (!is.null(groups)) {
    if (is.null(facet.rows) && is.null(facet.cols)) {
      facet.cols <- 1
    }
    p <- p + facet_wrap(group ~ ., nrow = facet.rows, ncol = facet.cols, 
                        scales = "free_y", strip.position = facet.label.pos)
  }
  if (length(colors) == 1 || length(colors) != n_models) {
    colors <- get_colors(colors, n_models)
  }
  
  else {
    colors <- colors
  }
  
  dh <- as.numeric(!plot.distributions) * 0.5

  if (!is.null(inner_ci_level)) {
    p <- p + ggstance::geom_linerangeh(aes(y = term, xmin = conf.low.inner, 
                                           xmax = conf.high.inner, colour = model), position = ggstance::position_dodgev(height = dh), 
                                       size = 2, show.legend = length(mods) > 1)
  }
  if (plot.distributions == FALSE || n_models == 1) {
    require(ggh4x)
    p <- p + ggstance::geom_pointrangeh(aes(y = interaction(term, model), x = estimate, 
                                            xmin = conf.low, xmax = conf.high, fill = model), position = ggstance::position_dodgev(height = dh), 
                                        colour = "black", fatten = point.size, size = 0.5, shape = 22,
                                        show.legend = length(mods) > 1) +
      scale_y_discrete(guide = "axis_nested") 
  }
  else {
    p <- p + geom_point(aes(y = term, x = estimate, colour = "black", 
                            shape = model), fill = model, size = point.size, 
                        stroke = 1, show.legend = TRUE)
  }
  
  
  if (length(point.shape) == 1 && point.shape == TRUE) {
    oshapes <- c(21:25, 15:18, 3, 4, 8)
    shapes <- oshapes[seq_len(n_models)]
  }
  
  else if (length(point.shape) == 1 && is.logical(point.shape[1]) && 
           point.shape[1] == FALSE) {
    shapes <- rep(21, times = n_models)
  }
  else {
    if (length(point.shape) != n_models && length(point.shape) != 
        1) {
      stop_wrap("You must provide the same number of point shapes as the\n                number of models.")
    }
    else if (length(point.shape) == 1) {
      shapes <- rep(23, times = n_models)
    }
    else {
      shapes <- point.shape
    }
  }
   p <- p + geom_vline(xintercept = 1 - !exp, linetype = 2, 
                      size = 0.25) + 
    scale_colour_manual(values = rep("black", length(colors)), limits = rev(levels(tidies$model)), 
                                                         breaks = rev(levels(tidies$model)), labels = rev(levels(tidies$model)), 
                                                         name = legend.title, guide = "none") + 
    # scale_shape_manual(limits = rev(levels(tidies$model)), values = shapes, name = legend.title) + 
    theme_nice() + 
    drop_y_gridlines() +
    theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 10), panel.grid.major.x = element_line(linetype = "solid")) + 
    xlab(ifelse(exp, no = "Estimate", yes = "exp(Estimate)")) +
      theme(ggh4x.axis.nestline = element_line(linetype = 1, colour = "gray92"))
  
  
  
  
  
  if (plot.distributions == TRUE) {
    p <- p + scale_y_discrete(limits = levels(tidies$term), 
                              name = legend.title)
    yrange <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    xrange <- ggplot_build(p)$layout$panel_params[[1]]$x.range
    if (is.null(yrange) && is.null(xrange)) {
      yrange <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range
      xrange <- ggplot_build(p)$layout$panel_ranges[[1]]$x.range
    }
    if (yrange[2] <= (length(unique(tidies$term)) + 0.8)) {
      upper_y <- length(unique(tidies$term)) + 0.8
    }
    else {
      upper_y <- yrange[2]
    }
    lower_y <- 0.8
    p <- p + coord_cartesian(ylim = c(lower_y, upper_y), 
                             xlim = xrange, expand = FALSE)
  }
  return(p)
}

reg_match <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  matches <- gregexpr(pattern, text, ignore.case, perl, fixed, useBytes)
  # If only 1 match, return just the one match rather than a list
  if (length(matches) == 1) {matches <- matches[[1]]}
  regmatches(text, matches, invert)
  
}

find_S3_class <- function(generic, ..., package) {
  
  # not going to provide function, just function name as character
  # ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for (m in .S3methods(generic, envir = getNamespace(package))) {
    assign(sub(generic, "X", m, fixed = TRUE), "body<-"(f, value = m))
  }
  
  char_meth <- tryCatch(X(...), error = function(e) {return(NA)})
  if (is.na(char_meth)) {return(char_meth)}
  # Return the stub for dispatch to getS3method as class
  return(reg_match("(?<=\\.).*", char_meth, perl = TRUE))
}

make_tidies <- function(mods, ex_args, ci_level, model.names, omit.coefs,
                        coefs, resp = NULL, dpar = NULL) {
  
  # Need to handle complexities of resp and dpar arguments
  dpars <- NULL
  dpar_fits <- FALSE
  resps <- NULL
  if ("brmsfit" %in% sapply(mods, class)) {
    
    if (!requireNamespace("broom.mixed")) {
      stop_wrap("Please install the broom.mixed package to process `brmsfit`
                objects.")
    }
    
    mv_fits <- sapply(mods, function(x) "mvbrmsformula" %in% class(formula(x)))
    if (any(mv_fits)) {
      if (!is.null(resp) && length(resp) %nin% c(sum(mv_fits), 1)) {
        stop_wrap("The length of the `resp` argument must be either equal to
                  the number of multivariate `brmsfit` objects or 1.")
      } else if (is.null(resp)) { # Need to retrieve first DV
        resp <- lapply(mods[mv_fits], function(x) names(formula(x)[["forms"]])[1])
      }
      # Create new vector that includes the non-multivariate models
      resps <- as.list(rep(NA, length(mods)))
      # Now put resp into that vector
      resps[mv_fits] <- resp
    } 
    
    # Need to detect which models have distributional parameters
    dpar_fits <- sapply(mods, function(x) {
      if ("brmsfit" %nin% class(x)) return(FALSE)
      if ("mvbrmsformula" %in% class(formula(x))) {
        any(sapply(
          brms::brmsterms(formula(x))$terms, function(x) length(x$dpars)
        ) > 1)
      } else {
        length(brms::brmsterms(formula(x))$dpars) > 1
      }
    })
    if (!is.null(dpar)) {
      if (length(dpar) %nin% c(sum(dpar_fits), 1)) {
        stop_wrap("The length of the `dpar` argument must be either equal to
                  the number of `brmsfit` objects with a distributional model or
                  1.")
      }
      dpars <- as.list(rep(NA, length(mods)))
      dpars[dpar_fits] <- dpar 
    } 
  }
  
  # Create empty list to hold tidy frames
  tidies <- as.list(rep(NA, times = length(mods)))
  
  for (i in seq_along(mods)) {
    
    # Major kludge for methods clash between broom and broom.mixed
    # Making namespace environment with broom.mixed before generics 
    # to try to put those methods in the search path
    # Will drop after update to broom 0.7.0
    if (requireNamespace("broom.mixed")) {
      nse <- as.environment(unlist(sapply(c(asNamespace("broom.mixed"), 
                                            asNamespace("generics")),
                                          as.list)))
    } else {
      nse <- asNamespace("generics")
    }
    method_stub <- find_S3_class("tidy", mods[[i]], package = "generics")
    if (getRversion() < 3.5) {
      # getS3method() only available in R >= 3.3
      the_method <- get(paste0("tidy.", method_stub), nse,
                        mode = "function")
    } else {
      the_method <- utils::getS3method("tidy", method_stub, envir = nse)
    }
    if (!is.null(ex_args)) {
      method_args <- formals(the_method)
      
      method_args <-
        method_args[names(method_args) %nin% c("intervals", "prob")]
      
      if (method_stub == "brmsfit" && "par_type" %nin% ex_args) {
        ex_args <- c(ex_args, par_type = "non-varying", effects = "fixed")
      } 
      
      extra_args <- ex_args[names(ex_args) %in% names(method_args)]
      
    } else if (method_stub == "brmsfit" && is.null(ex_args)) {
      extra_args <- list(effects = "fixed")
    } else {
      extra_args <- NULL
    }
    
    all_args <- as.list(c(x = list(mods[[i]]), conf.int = TRUE,
                          conf.level = ci_level, extra_args))
    
    tidies[[i]] <- do.call(generics::tidy, args = all_args)
    if (!is.null(names(mods)) && any(names(mods) != "")) {
      tidies[[i]]$model <- names(mods)[i]
    } else {
      modname <- paste("Model", i)
      tidies[[i]]$model <- modname
    }
    # Deal with glht with no `term` column
    if ("term" %nin% names(tidies[[i]]) && "lhs" %in% names(tidies[[i]])) {
      tidies[[i]]$term <- tidies[[i]]$lhs
    }
    if ("brmsfit" %in% class(mods[[i]]) && (!is.null(resps) || !is.null(dpars))) {
      # See if we're selecting a DV in a multivariate model
      if (!is.null(resps) && !is.na(resps[[i]])) {
        # Now see if we're also dealing with a distributional outcome
        if (is.null(dpars) || is.na(dpars[[i]])) {
          # If not, then just select the terms referring to this DV
          tidies[[i]] <- tidies[[i]][tidies[[i]]$response == resps[[i]], ]
        } else {
          # Otherwise, select those referring to this distributional DV
          tidies[[i]] <- tidies[[i]][tidies[[i]]$response == dpars[[i]], ]
          this_dv <- grepl(paste0("^", resps[[i]], "_"), tidies[[i]]$term)
          tidies[[i]] <- tidies[[i]][this_dv, ]
          # Also want to manicure those term names because they're confusing
          tidies[[i]]$term <- 
            gsub(paste0("^", resps[[i]], "_"), "", tidies[[i]]$term)
        }
      } else if (!is.null(dpars) && !is.na(dpars[[i]])) {
        # Everything's different for non-multivariate models -__-
        # Prefixed with, e.g., sigma_
        this_dv <- grepl(paste0("^", dpars[[i]], "_"), tidies[[i]]$term)
        tidies[[i]] <- tidies[[i]][this_dv, ]
        # Drop the prefix now
        tidies[[i]]$term <- 
          gsub(paste0("^", dpars[[i]], "_"), "", tidies[[i]]$term)
      }
    } else if ("brmsfit" %in% class(mods[[i]]) && 
               any(dpar_fits) && dpar_fits[[i]]) {
      # Need to drop dpar parameters...first, need to identify them
      the_dpars <- names(brms::brmsterms(formula(mods[[i]]))$dpars)
      # Loop through them, other than the first (location) parameter
      for (the_dpar in the_dpars[-1]) {
        tidies[[i]] <-
          tidies[[i]][!grepl(paste0("^", the_dpar, "_"), tidies[[i]]$term),]
      }
    }
  }
  
  # Keep only columns common to all models
  # TODO: replicate dplyr::bind_rows behavior of keeping all columns and 
  # filling empty rows with NA
  tidies <- lapply(tidies, function(x) {
    x[Reduce(intersect, lapply(tidies, names))]
  })
  
  # Combine the tidy frames into one, long frame
  tidies <- do.call(rbind, tidies)
  
  # For consistency in creating the factors apply contrived names to model.names
  if (is.null(model.names)) {
    model.names <- unique(tidies$model)
  }
  
  # Drop omitted coefficients
  if (!is.null(omit.coefs)) {
    tidies <- tidies[tidies$term %nin% omit.coefs,]
  }
  print(tidies)
  # Creating factors with consistent ordering for coefficients too
  if (is.null(coefs)) {
    coefs <- unique(tidies$term)
    names(coefs) <- coefs
  } else {
    tidies <- tidies[tidies$term %in% coefs,] #Valeria coefs must be specified as varnamecatname 
    if (is.null(names(coefs))) {
      names(coefs) <- coefs
    }
  }
  print(tidies)
  
  # For some reason, the order of the legend and the dodged colors
  # only line up when they are reversed here and in the limits arg of
  # scale_colour_brewer...no clue why that has to be the case
  tidies$model <- factor(tidies$model, levels = rev(model.names))
  tidies$term <- factor(tidies$term, levels = rev(coefs),
                        labels = rev(names(coefs)))
  
  if (all(c("upper", "lower") %in% names(tidies)) &&
      "conf.high" %nin% names(tidies)) {
    tidies$conf.high <- tidies$upper
    tidies$conf.low <- tidies$lower
  }
  
  # For merMod and other models, we may not get CIs for the random terms
  # and don't want blank rows on the plot.
  which_complete <- which(!is.na(tidies$conf.high) & !is.na(tidies$conf.low) &
                            !is.na(tidies$estimate))
  tidies <- tidies[which_complete,]
  tidies$term <- droplevels(tidies$term)
  
  return(tidies)
  
}
