#' Automatically select the random structure using "lme"
#'
#' @param dat The data to use. Should be a data frame.
#' @param fixed_structure The fixed structure of the statistical model, e.g. `z ~ x + y`. Must have a match in the headers of `dat`.
#' @param random_variables The variables that should be used, e.g. `c("d", "e")`. Must have a match in the headers of `dat`.
#' @param random_slopes The fixed independent variables that should be affected by `random_variables`, e.g.: `c("x", "y")`. The default is to the logical `F`. Must have a match in the headers of `dat`. Default
#' @param keep Should **any** variable from `random_variables` or `random_slopes` not be excluded? Default to the logical `F`, else a vector of character strings, e.g. c("d").
#' @param sort_by Should the data be sorted by `AIC` or `BIC`? Default is to BIC.
#' @param compute_gls Should a model without any random structure be included? Default to `T`.
#' @param MC Should multiple cores be used? The function runs per default (`MC = T`) in parallel if enough random variables are added.
#' @param noC The number of cores that should be used. The default setting uses all possible cores/threads: `parallel::detectCores()`. For a smaller number of random variables, a lower core count might increase the speed of the function.
#'
#' @import foreach
#' @import nlme
#'
#' @return A data frame including the (1) random structure, (2) the AIC values, (3) the BIC values, (4) the dAIC values, (5) the dBIC values. The default sorting is by BIC.
#'
#' @export
#'
#' @examples
#'
#' library("nlme") # for data
#'
#' ds <- data.frame(distance = Orthodont$distance,
#'                  age = Orthodont$age,
#'                  sex = Orthodont$Sex,
#'                  subject = Orthodont$Subject)
#'
#' select_random(dat = ds,
#'               fixed_structure = c("distance ~ age * sex"),
#'               random_variables = c("subject"),
#'               random_slopes = c("age"))
#'
#'
#' select_random(dat = ds,
#'               fixed_structure = c("distance ~ age"),
#'               random_variables = c("subject", "sex"),
#'               random_slopes = c("age"))

select_random <- function(dat,
                          fixed_structure,
                          random_variables,
                          random_slopes = F,
                          keep = F,
                          sort_by = "BIC",
                          compute_gls = T,
                          MC = T,
                          noC = parallel::detectCores()){

  ## check for logical mismatches and issues
  if(keep != F) compute_gls = F
  if(!is.data.frame(dat)){
    error("The data must be a data frame.")
  }

  ## create all possible combinations of random slopes
  if(random_slopes == F){
    rs <- "~ 1"
  } else{
    rs <- foreach::foreach(i = 1:length(random_slopes), .combine = "c") %do% {
      x <- expand.grid(rep(list(random_slopes),i))
      if(ncol(x) == 1){
        apply(x, 1, as.character)
      } else {
        foreach::foreach(j = 1:nrow(x), .combine = "c") %do% {
          tmp <- length(unique(as.numeric(x[j,])))
          if(tmp == i){
            do.call("paste", c(x[j,], sep = " + "))
          }
        }
      }
    }
    rs <- paste("~ 1 +", rs)
    rs <- c("~ 1", rs)
  }

  ## create all possible combinations of random variables
  rv <- foreach::foreach(i = 1:length(random_variables), .combine = "c") %do% {
    x <- expand.grid(rep(list(random_variables),i))
    if(ncol(x) == 1){
      apply(x, 1, as.character)
    } else {
      foreach::foreach(j = 1:nrow(x), .combine = "c") %do% {
        tmp <- length(unique(as.numeric(x[j,])))
        if(tmp == i){
          do.call("paste", c(x[j,], sep = "/"))
        }
      }
    }
  }

  ## combine rs and rv
  if(length(rs) == 1){
    rstruct <- paste(rs, rv, sep = " | ")
  } else {
    rstruct <- foreach::foreach(i = 1:length(rs), .combine = "c") %do% {
      paste(rs[i], rv, sep = " | ")
    }
  }

  ## exclude cases where keep-argument is not fulfilled
  if(keep != F){
    keeps <- foreach::foreach(i = 1:length(keep), .combine = "cbind") %do% {
      grepl(keep[i], rstruct, fixed = T)
    }
    rstruct <- rstruct[keeps]
  }

  ## check if parallel computing should be done and change settings:
  if(noC > length(rstruct) & MC == T){
    noC <- length(rstruct)
    warning(paste("Number of cores more than possible random structures. Set noC to: ", noC))
  }

  if(noC <= 1 & MC == T){
    warning(paste("Number of cores is ", noC, ". Set MC == F"))
    MC <- F
  }

  ## estimate information criteria
  if(MC == T){
    srstruct <- split(rstruct, 1:noC)
    cl <- parallel::makeCluster(noC)
    doParallel::registerDoParallel(cl)
    ICs <- foreach::foreach(i = 1:noC, .combine = "rbind", .packages = c("foreach", "nlme")) %dopar% {
      foreach::foreach(j = 1:length(srstruct[[i]]),.combine = "rbind") %do% {
        AICx <- Inf
        BICx <- Inf
        try({
          x <- nlme::lme(as.formula(fixed_structure),
                         data = dat,
                         random = as.formula(srstruct[[i]][j]),
                         method = "REML")
          AICx <- AIC(x)
          BICx <- BIC(x)
        }, silent = T)
        data.frame(rstruct = srstruct[[i]][j], AIC = AICx, BIC = BICx)
      }
    }
    parallel::stopCluster(cl)

  } else {
    ICs <- foreach::foreach(i = 1:length(rstruct), .combine = "rbind") %do% {
      AICx <- Inf
      BICx <- Inf
      try({
        x <- nlme::lme(as.formula(fixed_structure),
                       data = dat,
                       random = as.formula(rstruct[i]),
                       method = "REML")
        AICx <- AIC(x)
        BICx <- BIC(x)
      }, silent = T)
      data.frame(rstruct = rstruct[i], AIC = AICx, BIC = BICx)
    }
  }

  ## compute gls (no random structure)
  if(compute_gls == T){
    AICx <- Inf
    BICx <- Inf
    try({
      x <- nlme::gls(as.formula(fixed_structure),
                     data = dat,
                     method = "REML")
      AICx <- AIC(x)
      BICx <- BIC(x)
    },
    silent = T)
    ICs <- rbind(ICs, data.frame(rstruct = "none", AIC = AICx, BIC = BICx))
  }

  ICs$dAIC <- ICs$AIC-min(ICs$AIC)
  ICs$dBIC <- ICs$BIC-min(ICs$BIC)
  if(sort_by == "AIC"){
    ICs <- ICs[order(ICs$dAIC),]
  } else {
    if(sort_by == "BIC"){
      ICs <- ICs[order(ICs$dBIC),]
    } else {
      warning("Neither AIC nor BIC selected for sorting - no sorting performed!")
    }
  }
  return(ICs)
}
