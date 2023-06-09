# File contents: uvector, uvector_noh, logdet.and.v

#' ML estimation vector for reducible SDEs
#'
#' @describeIn uvector Estimation vector, general
#' @order 1
#'
#' @description These functions are not normally called directly by the user.
#' Function \code{uvector()} is used by \code{\link{sdefit}()}. Function
#' \code{uvector_noh()} is a more limited version, maintained for documentation
#' purposes. Function \code{logdet_and_v()} is used by \code{uvector()} and
#' \code{uvector_noh()}.
#'
#' @details \code{uvector()} and \code{uvector_noh()} calculate a vector of
#' residuals for sum of squares minimization by \code{nls()} or \code{nlme()}.
#' The first one works both for single-unit and for bilevel hierarchical models.
#' It is backward-compatible with \code{uvector_noh()}, which is only for
#' single-unit models but simpler and easier to understand. They require a
#' transformation function \code{phi(x, theta)}, and a function
#' \code{phiprime(x, theta)} for the derivative dy/dx, where \code{theta} is a
#' list containing the transformation parameters.
#'
#' \code{logdet_and_v()} calculates \eqn{\log[\det(L)]}{log[det(L)]} and \eqn{v
#' = L^{-1} z}{v = L^-1 z}, where \eqn{C = LL'}, with \eqn{L} lower-triangular.
#'
#' The three functions are essentially unchanged from García (2019)
#' <\doi{10.1007/s00180-018-0837-4}>, except for a somewhat safer computation
#' for very small \code{beta1}, and adding in \code{logdet_and_v()} a shortcut
#' for when \eqn{L} is diagonal (e.g., when \eqn{\sigma_m = 0}). The
#' transformation functions \code{phi} and \code{phiprime} can be passed as
#' globals, as in the original, or in an environment named \code{trfuns}.
#'
#' @param x,t Data vectors
#' @param unit Unit id vector, if any.
#' @param beta0,beta1,eta,eta0,x0,t0 SDE parameters or re-parameterizations.
#' @param lambda Named list of parameters(s) for \code{phi()}, possibly local
#'   vectors.
#' @param mum,mu0,mup Optional \eqn{\sigma} multipliers.
#' @param sorted Data already ordered by increasing t?
#' @param final Mode, see below.
#'
#' @param cdiag Vector with the diagonal elements \eqn{c_{ii}}{c[i, i]} of
#'   \eqn{C}.
#' @param csub Vector with sub-diagonal \eqn{c_{i, i-1}}{c[i, i-1]} for \eqn{i >
#'   1}.
#' @param z A numeric vector
#'
#' @return \code{uvector()} and \code{uvector_noh()}: If \code{final = FALSE}
#' (default), return a vector whose sum of squares should be minimized over the
#' parameters to obtain maximum-likelihood estimates. If \code{final = TRUE},
#' passing the ML parameter estimates returns a list with the sigma estimates,
#' the maximized log-likelihood, and AIC and BIC criteria..
#'
#' \code{logdet_and_v()}: List with elements \code{logdet} and \code{v}.
#'
#' @export
#'
# @examples
# uvector(x=runif(5), t=1:5, unit = NULL, beta0=0, beta1=1, eta=0, eta0=0,
#  x0=0, t0=0, lambda=NULL)
# logdet.and.v(1:3, c(0,.2,.1), 3:1)
#' @usage uvector(x, t, unit = NULL, beta0, beta1, eta, eta0, x0, t0, lambda,
#'   mum = 1, mu0 = 1, mup = 1, sorted = FALSE, final = FALSE)

uvector <- function(x, t, unit = NULL, beta0, beta1, eta, eta0, x0, t0, lambda,
                    mum = 1, mu0 = 1, mup = 1, sorted = FALSE, final = FALSE)
{
  # Retrieve the transformation functions, see comment at the top of sdefit.R
  if(!exists("phi")) phi <- get("phi", trfuns)  # or phi <- trfuns$phi
  if(!exists("phiprime")) phiprime <- get("phiprime", trfuns)
  #    the conditionals preserve compatibility with the original uvector()

  if (is.null(unit)) unit <- rep(1, length(x)) # single unit
  if (length(unique(eta)) > 1 || length(unique(eta0)) > 1)
    stop("eta and eta0 must be global")
  theta <- data.frame(unit, beta0, beta1, eta, eta0, x0, t0, mum, mu0, mup)
  if(length(lambda) > 0) theta <- cbind(theta, lambda)  # not if empty
  theta <- theta[!duplicated(unit),] # one row per unit
  v <- c()
  n <- logJ <- 0
  for (id in theta$unit) {
    theta.j <- theta[match(id, theta$unit),]
    j <- unit == id
    x.j <- x[j]
    t.j <- t[j]
    if (!sorted && is.unsorted(t.j)) { # ensure increasing t
      s <- order(t.j)
      x.j <- x.j[s]
      t.j <- t.j[s]
    }
    n.j <- length(x.j)
    y <- phi(x.j, theta.j)
    y0 <- phi(theta.j$x0, theta.j)
    Dt <- diff(c(theta.j$t0, t.j))
    muetam <- theta.j$mum ^ 2 * theta.j$eta
    mueta0 <- theta.j$mu0 ^ 2 * theta.j$eta0
    muetap <- theta.j$mup ^ 2 * (1 - theta.j$eta)
    if (abs(theta.j$beta1) > 1e-300) { # was == 0 in the original
      ex <- exp(theta.j$beta1 * Dt)
      ex2 <- ex ^ 2
      z <- y + theta.j$beta0 / theta.j$beta1 - ex *
        (c(y0, y[-n.j]) + theta.j$beta0 / theta.j$beta1)
      cdiag <- (ex2 + 1) * muetam + muetap *
        expm1(2 * theta.j$beta1 * Dt) / (2 * theta.j$beta1)
        # changed to expm1() from (ex2 - 1) in the published version
      cdiag[1] <- cdiag[1] - ex2[1] * (muetam - mueta0)
      csub <- -ex * muetam
    } else { # beta1 == 0
      z <- y - c(y0, y[-n.j]) - theta.j$beta0 * Dt
      cdiag <- 2 * muetam + muetap * Dt
      cdiag[1] <- cdiag[1] - muetam + mueta0
      csub <- rep(-muetam, n.j)
    }
    ld.v <- logdet.and.v(cdiag, csub, z)
    v <- c(v, ld.v$v)
    logJ <- logJ + sum(log(abs(phiprime(x.j, theta.j)))) -
      ld.v$logdet
    n <- n + n.j
  }
  if (n != length(x)) stop("Should not happen, something wrong!")
  Jn <- exp(logJ / n)
  # J^(1/n)
  u <- v / Jn
  if (!final) return (u) # "normal" exit
  # Else, at optimum, calculate sigma.P, sigma.M and sigma.Z
  #   estimates, and the log - likelihood:
  ms <- sum(u ^ 2) / n # mean square
  sigma2 <- Jn ^ 2 * ms # estimate for sigma^2
  list(sigma.P = sqrt((1 - eta) * sigma2),
       sigma.M = sqrt(eta * sigma2),
       sigma.Z = sqrt(eta0 * sigma2),
       logLikelihood = -(n / 2) * (log(ms) + log(2 * pi) + 1))
}


#' ML estimation vector for reducible SDEs
#'
#' @describeIn uvector Estimation vector, non-hierarchical
#' @order 2

uvector_noh <- function(x, t, beta0, beta1, eta, eta0, x0, t0, lambda, final = FALSE)
{
  theta <- c(list(beta0 = beta0, beta1 = beta1, eta = eta, eta0 = eta0,
                  x0 = x0, t0 = t0), lambda)
  s <- order(t); t <- t[s]; x <- x[s] # ensure increasing t
  n <- length(x)
  y <- phi(x, theta)
  y0 <- phi(x0, theta)
  Dt <- diff(c(t0, t))
  if (beta1 != 0) {
    ex <- exp(beta1 * Dt)
    ex2 <- ex ^ 2
    z <- y + beta0 / beta1 - ex * (c(y0, y[-n]) + beta0 / beta1)
    cdiag <- ex2 * eta + eta + (1 - eta) * (ex2 - 1) / (2 * beta1)
    cdiag[1] <- cdiag[1] - ex2[1] * (eta - eta0)
    csub <- -ex * eta
  } else { # beta1 == 0
    z <- y - c(y0, y[-n]) - beta0 * Dt
    cdiag <- 2 * eta + (1 - eta) * Dt
    cdiag[1] <- cdiag[1] - eta + eta0
    csub <- -rep(eta, n)
  }
  ld.v <- logdet.and.v(cdiag, csub, z)
  logJ <- sum(log(abs(phiprime(x, theta)))) - ld.v$logdet
  Jn <- exp(logJ / n)
  # J^(1/n)
  u <- ld.v$v / Jn
  if (!final) return (u) # "normal" exit
  # Else, at optimum, calculate sigma.p, sigma.m and sigma.0
  #   estimates, and the log-likelihood:
  ms <- sum(u ^ 2) / n # mean square
  sigma2 <- Jn ^ 2 * ms # estimate for sigma^2
  list(sigma.p = sqrt((1 - eta) * sigma2),
       sigma.m = sqrt(eta * sigma2),
       sigma.0 = sqrt(eta0 * sigma2),
       loglikelihood = -(n / 2) * (log(ms) + log(2 * pi) + 1))
}


#' Log of determinant and v vector
#'
#' @describeIn uvector Logarithm of determinant, and \eqn{v} vector
#' @order 3

logdet.and.v <- function(cdiag, csub=NULL, z)
{
  if(is.null(csub) || all(cdiag == 0)){ # diagonal
    return(list(logdet = sum(log(cdiag))/2, v = z / sqrt(cdiag)))
  } # not in the published version
  v <- z
  ldiag <- sqrt(cdiag[1])
  logdet <- log(ldiag)
  v[1] <- z[1] / ldiag
  for (i in 2:length(z)) {
    lsub <- csub[i] / ldiag
    ldiag <- sqrt(cdiag[i] - lsub ^ 2)
    logdet <- logdet + log(ldiag)
    v[i] <- (z[i] - lsub * v[i - 1]) / ldiag
  }
  list(logdet = logdet, v = v)
}
