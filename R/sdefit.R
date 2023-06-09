# File contents: trfuns, sdefit, str2fun_theta

# Environment for passing transformation functions:
trfuns <- new.env()
# The functions for computing phi and its derivative phiprime are generated and
# stored here by sdefit(). They are read from this environment inside uvector().
# A regular assignment within sdefit() does not work, because nls() and nlme()
# somehow alter the environments when processing functions given in formulas.
# Passing the functions as arguments to uvector() does not work either, nls()
# gets confused and it crashes. Simply placing the functions in the global
# environment with <<- or assign() works fine without any changes to uvector(),
# but global variables are often frowned upon, e.g., by CRAN.

# Stop the R checker complaining about phi and phiprime in uvector():
if(getRversion() >= "2.15.1") utils::globalVariables(c("phi", "phiprime"))


#' Fit SDE model
#'
#' ML estimation of parameters for a reducible SDE
#'
#' @param model   Model specification, as produced by \code{\link{sdemodel}()}.
#' @param x,t     Vectors with variables, or names of columns in data frame.
#' @param unit    If applicable, unit id vector, or name of its column in data frame.
#' @param data    Data frame, if data not given directly in \code{x}, \code{t}, \code{unit}.
#' @param start   Named vector or named list with starting parameter values
#'                for non-hierarchical models. They can also be given
#'                in global.
#' @param global  Named vector or list of global parameters and their starting
#'                values for hierarchical models. Can also contain
#'                starting values for non-hierarchical models.
#' @param local   Named vector or list of local parameters and their starting values for
#'                hierarchical models. The value can be a vector with
#'                values for each unit, or a single scalar that
#'                applies to all the units.
#' @param known   Named vector or list with any parameters that should be fixed at given values.
#' @param method  \code{'nls'} for non-hierarchical models (default). For
#'                hierarchical models it can be \code{'nls'}, for fixed
#'                locals, or \code{'nlme'} for mixed effects.
#' @param control Optional control list for \code{nls()} or \code{nlme()}.
#' @param phi   Optional transformation function. If \code{NULL} (default), it is automatically generated.
#' @param phiprime Optional derivative function. If \code{NULL} (default), it is automatically generated.
#' @return  List with two components: a list \code{fit} containing the output from
#'          the optimizer (\code{nls} or \code{nlme}), and a list \code{more} containing
#'          sigma estimates, log-likelihood, AIC and BIC. Note that in \code{fit}, "residual sum-of-squares"
#'          corresponds to \code{uvector}, not to \code{x} or \code{y}. Same for \code{nls} and \code{nlme}
#'          methods like \code{fitted} or \code{residuals} applied to \code{fit}.
#' @export
#' @import stats
#'
#' @examples
#' m <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b)
#' mod1 <- sdefit(m, "height", "age", data=Loblolly[Loblolly$Seed=="301",],
#'                start=c(a=70, b=0.1, c=1))
#' mod2 <- sdefit(m, "height", "age", "Seed", Loblolly, global=c(b=0.1, c=0.5),
#'                local=c(a=72))
#' @usage sdefit(model, x, t, unit=NULL, data=NULL, start=NULL,
#'   global=NULL, local=NULL, known=NULL, method="nls",
#'   control=NULL, phi=NULL, phiprime=NULL)

sdefit <- function(model, x, t, unit=NULL, data=NULL, start=NULL,
                   global=NULL, local=NULL, known=NULL, method="nls",
                   control=NULL, phi=NULL, phiprime=NULL)
{
  # Convert and check arguments
  m <- model$m
  pars <- sort(model$pars)
  lambda <- model$lambda
  if(length(local) != 0 && length(start) != 0)
    warning("Hierarchical model with parameters in 'start'. Taken as global.",
            call.=FALSE)
  global <- as.list(c(start, global))  # ignore start for now
  local <- as.list(local)
  known <- as.list(known)
  datasrc <- substitute(data) # call value
  if(is.null(unit) && length(local) > 0)
    stop("Local parameters without unit ids", call.=FALSE)
  if(!is.null(unit) && length(local) == 0){
    warning("Unit ids but no local parameters", call.=FALSE)
    local <- NULL
  }
  u <- known
  u$eta <- NULL
  if(!identical(pars, sort(c(names(global),names(local),names(u))))){
    stop("Missing or repeated parameters, should have values for ",
         paste0(pars, collapse=", "), call.=FALSE)
  }
  if(!identical(method, "nls") && !identical(method, "nlme"))
    stop("Unknown method ", method, call.=FALSE)

  # Make data frame if data given in x and t
  if(is.null(data)){
    data <- data.frame(x=x, t=t)
    x <- "x"
    t <- "t"
    if(!is.null(unit)){ # unit ids given
      data$unit <- unit
      unit <- "unit"
    }
  }
  # Make sure that the data is sorted on increasing t
  if(is.null(unit)){
    data <- data[order(data[,t]), ]
  } else {
    for(u in unique(data[, unit])){
      i <- data[, unit] == u
      d <- data[i,]
      data[i,] <- d[order(d[,t]),]
    }
  }

  # Figure-out eta
  if(!is.null(known$eta) && (identical(m$mup, "0") || identical(m$mum, "0"))){
    stop("If a known eta is given, mup and mum cannot be 0", call.=FALSE)
  }
  if(identical(m$mum, "0")){
    eta <- 0
  } else if(identical(m$mup, "0")){
    eta <- 1
  } else if(!is.null(known$eta)){
    eta <- known$eta
    known$eta <- NULL
  } else {
    global$eta <- 0.5
    eta <- "eta"
  }
  # And eta0
  if(identical(m$mu0, "0")){
    eta0 <- 0
  } else {
    global$eta0 <- 0.5
    eta0 <- "eta0"
  }

  # Transformation and derivative
  if(is.null(phi)) phi <- str2fun_theta(m$phi)
  if(is.null(phiprime)) phiprime <- str2fun_theta(m$phiprime)
  # Store them in the trfuns environment, see comments at the top of this file
  assign("phi", phi, trfuns)  # or trfuns$phi <- phi
  assign("phiprime", phiprime, trfuns)

  # Build formula using uvector()
  if(is.null(unit)) u <- "NULL"
  else u <- unit
  uargs <- c("x", "t", "unit", "beta0", "beta1", "eta", "eta0", "x0", "t0",
             "lambda", "mum", "mu0", "mup", "sorted")
  lambda <- paste0("list(", paste(lambda, lambda, sep="=", collapse=","), ")")
  uvals <- with (m, c(x, t, u, beta0, beta1, eta, eta0, x0, t0,
                      lambda, mum, mu0, mup, TRUE))
  frml <- as.formula(paste0("0~uvector(", paste(uargs, uvals, sep="=",
                                                collapse=", "), ")"))
  if(!is.null(known)) frml <- as.formula(do.call("substitute",
                              list(frml, known)))  # fill-in known values

  # Replicate locals to one for each unit if necessary
  if(!is.null(unit)){
    nunits <- length(unique(data[, unit]))
    if(!is.null(local)){
      for(i in seq_along(local)){
        if(length(local[[i]]) == 1) local[[i]] <- rep(local[[i]], nunits)
        else if(length(local[[i]]) != nunits)
          stop("Length of local ", names(local)[i], " should be 1 or ", nunits,
               call.=FALSE)
      }
      local <- as.data.frame(local)
    }
  }

  # Fit with nlme
  if(identical(method, "nlme")){
    if(length(local) == 0) # do we have locals?
      stop("No locals for method 'nlme'", call.=FALSE)
    fixed <- global
    fixed$eta <- NULL  # exclude eta, if present
    fixed <- c(unlist(fixed), colMeans(local))
#    random <- as.matrix(local - colMeans(local))
    start <- fixed # list(fixed=fixed, random=random)
    fixed <- as.formula(paste0(paste0(names(fixed), collapse="+"), "~1"))
    random <- as.formula(paste0(paste0(colnames(local), collapse="+"), "~1"))
    groups <- as.formula(paste0("~", unit))
    # Run nlme
    if(is.numeric(eta)){ # no free eta parameter, simple call
      fit <- nlme::nlme(frml, data, fixed=fixed, random=random,
                        groups=groups, start=start, control=control)
    } else { # estimate eta between 0 and 1, nested call
      # Use function factory tricks, see
      # https://adv-r.hadley.nz/function-factories.html
      fit <- NULL  # prepare to receive nlme result
      factory <- function(start){ # function factory
        start <- start  # store start parameters for next iteration here
        function(eta){  # function to be manufactured
          frml_eta <- as.formula(do.call("substitute", list(frml,
            list(eta=eta))))  # substitute eta value in formula
          fit <<- nlme::nlme(frml_eta, data, fixed=fixed, random=random,
            groups=groups, start=start, control=control)  # stored outside
          start <<- nlme::fixef(fit)  # next start
#           start <<- list(fixed = nlme::fixef(fit),
#             random = as.matrix(nlme::randef(fit))) # next start
          return(logLik(fit))
        }  # end ef returned function
      }
      f <- factory(start) # generate function of eta to be optimized
      eta <- optimize(f, c(0, 1), maximum=TRUE, tol=1e-8)[["maximum"]]
      # Older simpler code
#     f <- function(eta){
#       frml_eta <- as.formula(do.call("substitute", list(frml,
#         list(eta=eta))))  # substitute eta value in formula
#       fit <- nlme::nlme(frml_eta, data, fixed=fixed, random=random,
#         groups=groups, start=start, control=control)
#       return(logLik(fit))
#     }
#     eta <- optimize(f, c(0, 1), maximum=TRUE, tol=1e-8)[["maximum"]]
#     frml_eta <- as.formula(do.call("substitute", list(frml,
#       list(eta=eta))))  # substitute eta value in formula
#     fit <- nlme::nlme(frml_eta, data, fixed=fixed, random=random,
#       groups=groups, start=start, control=control)
    }
    # Grab parameter estimates
    cf <- coef(fit)
    global <- c(cf[1, ], eta=eta)[names(global)]
    local <- cf[!names(cf) %in% names(global)]
    npar <- length(global) + length(local)*(length(local)+3)/2 + 1
  } # done with nlme

  # Append [unit] to local names in formula
  if(!is.null(unit) && !is.null(local)){
    nms <- names(local)
    u <- paste0("[", unit, "]")
    e <- lapply(paste0(nms, u), str2lang)
    names(e) <- nms
    frml <- as.formula(do.call("substitute", list(frml, e)))
  }

  # Fit with nls
  if(method == "nls"){
    start <- c(global, local)
    if(is.null(global$eta)){  # no free eta parameter, unconstrained fit
      fit <- nls(frml, data, start=start, control=control)
    } else { # constrained to 0 <= eta <= 1
      lo <- up <- unlist(start)
      lo[] <- -Inf
      lo["eta"] <- 0
      up[] <- Inf
      up["eta"] <- 1
      fit <- nls(frml, data, start=start, control=control, 
                 algorithm="port", lower=lo, upper=up)
    }
    fit$data <- datasrc  # call value, usually data set name

    # Grab parameter estimates
    cf <- coef(fit)
    global <- cf[names(global)]
    cf <- cf[!(names(cf) %in% names(global))]
    if(!is.null(unit)) local <- matrix(cf, nrow=nunits, dimnames=list(NULL,
                               names(local)))
    npar <- length(global) + length(local) + 1  # number of parameters
    if(!is.null(unit)) local <- as.data.frame(local)
  } # done with nls

  # Additional fit statistics
  # String with uvector call adding final=TRUE
  s <- as.character(frml[3])  # exclude '0 ~'
  s <- paste0(substr(s, 1, nchar(s)-1), ", final=TRUE)")
  # Execute it
  more <- eval(str2lang(s), envir=c(global, local, data))
  # alternative: more <- with(c(global, local, known, data), eval(str2lang(s)))
  names(more) <- c("sigma_p", "sigma_m", "sigma_0", "logLik")
  more <- more[c(m$mup, m$mum, m$mu0, "x") != "0"]  # drop unused
  if(identical(method, "nlme")){
    more$logLik <- logLik(fit)
  } else if(isFALSE(all.equal(more$logLik, logLik(fit)))){
    stop("A bug! Something wrong with the logLikelihood computation")
  }
  more$AIC <- 2 * (npar - more$logLik)
  more$BIC <- log(nrow(data)) * npar - 2 * more$logLik
  if(isFALSE(all.equal(more[c("AIC", "BIC")], c(AIC(fit) + 2*exists("f",
    mode="function"),
    BIC(fit) + log(nrow(data))*exists("f", mode="function"))))){
    stop("A bug! Something wrong with the AIC or BIC computation")
  }
  # Chck for boundary solutions (zero sigmas)
  if(!is.null(more$sigma_p) && !is.null(more$sigma_m)) {
    msg <- "Solution at boundary, it may be a local optimum. Compare logLik with "
    if (abs(more$sigma_p) < 1e-5) warning(msg, "mum = 0", call.=FALSE)
    else if (abs(more$sigma_m) < 1e-5) warning(msg, "mup = 0", call.=FALSE)
  }

  return(list(fit=fit, more=unlist(more)))
}


#' String to function, with parameters in theta
#'
#' Normally not called by the user directly, used by \code{\link{sdefit}()}.
#'   Converts an expression, in a character string, to a function. 
#'
#' @param s String representation of a function of \code{x} and parameters
#'
#' @return Function of \code{x} and \code{theta}, \code{theta} being a named vector or list of parameters.
#' @export
#'
#' @examples str2fun_theta("x^c / a")

str2fun_theta <- function(s){
  t <- paste("alist(x=, theta=, with(theta, ", s, "))")
  return(as.function(eval(str2lang(t))))
}
