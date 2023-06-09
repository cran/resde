% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdefit.R
\name{sdefit}
\alias{sdefit}
\title{Fit SDE model}
\usage{
sdefit(model, x, t, unit=NULL, data=NULL, start=NULL,
  global=NULL, local=NULL, known=NULL, method="nls",
  control=NULL, phi=NULL, phiprime=NULL)
}
\arguments{
\item{model}{Model specification, as produced by \code{\link{sdemodel}()}.}

\item{x, t}{Vectors with variables, or names of columns in data frame.}

\item{unit}{If applicable, unit id vector, or name of its column in data frame.}

\item{data}{Data frame, if data not given directly in \code{x}, \code{t}, \code{unit}.}

\item{start}{Named vector or named list with starting parameter values
for non-hierarchical models. They can also be given
in global.}

\item{global}{Named vector or list of global parameters and their starting
values for hierarchical models. Can also contain
starting values for non-hierarchical models.}

\item{local}{Named vector or list of local parameters and their starting values for
hierarchical models. The value can be a vector with
values for each unit, or a single scalar that
applies to all the units.}

\item{known}{Named vector or list with any parameters that should be fixed at given values.}

\item{method}{\code{'nls'} for non-hierarchical models (default). For
hierarchical models it can be \code{'nls'}, for fixed
locals, or \code{'nlme'} for mixed effects.}

\item{control}{Optional control list for \code{nls()} or \code{nlme()}.}

\item{phi}{Optional transformation function. If \code{NULL} (default), it is automatically generated.}

\item{phiprime}{Optional derivative function. If \code{NULL} (default), it is automatically generated.}
}
\value{
List with two components: a list \code{fit} containing the output from
         the optimizer (\code{nls} or \code{nlme}), and a list \code{more} containing
         sigma estimates, log-likelihood, AIC and BIC. Note that in \code{fit}, "residual sum-of-squares"
         corresponds to \code{uvector}, not to \code{x} or \code{y}. Same for \code{nls} and \code{nlme}
         methods like \code{fitted} or \code{residuals} applied to \code{fit}.
}
\description{
ML estimation of parameters for a reducible SDE
}
\examples{
m <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b)
mod1 <- sdefit(m, "height", "age", data=Loblolly[Loblolly$Seed=="301",],
               start=c(a=70, b=0.1, c=1))
mod2 <- sdefit(m, "height", "age", "Seed", Loblolly, global=c(b=0.1, c=0.5),
               local=c(a=72))
}
