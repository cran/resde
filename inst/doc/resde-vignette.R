## ----setup, include=FALSE, cache=FALSE---------------------------
library(knitr)
opts_chunk$set(fig.align='center', # fig.show='hold',
dev='pdf', out.width='.45\\textwidth') # , highlight=FALSE)
old_options <- options(width=67)

## ----sdemodel, eval=FALSE----------------------------------------
#  sdemodel(phi=~x, phiprime=NULL, beta0=~beta0, beta1=~beta1, t0=0,
#           x0=0, mu0=0, mup=1, mum=1)

## ----------------------------------------------------------------
library(resde)
exmpl <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b, mup=~sqrt(b))

## ----------------------------------------------------------------
lob301 <- Loblolly[Loblolly$Seed == 301, ]

## ----------------------------------------------------------------
m <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b)  # else, defaults

## ----------------------------------------------------------------
f <- sdefit(m, x="height", t="age", data=lob301,
            start=c(a=60, b=0.1, c=1))
f

## ----------------------------------------------------------------
m <- sdemodel(phi=~x^c, beta0=~b*a^c, beta1=~-b, mum=0)
sdefit(m, x="height", t="age", data=lob301,
       start=c(a=60, b=0.1, c=1))

## ----------------------------------------------------------------
summary(f$fit)

## ----------------------------------------------------------------
m <- sdemodel(~log(abs(a^c - x^c)), beta0=~-b, beta1=0, mup=~b)
sdefit(m, x="height", t="age", data=lob301,
       start=c(a=70, b=0.1, c=1))

## ----------------------------------------------------------------
plot(c(0,height) ~ c(0, age), data=lob301)
curve(77.10687 * (1 - exp(-0.08405 * x))^(1/0.54946), add=T,
      col="red")
curve(72.5459 * (1 - exp(-0.0967 * x))^(1/0.5024), add=T,
      col="blue")

## ----------------------------------------------------------------
m <- sdemodel(phi=~bc(x/a, c), beta0=0, beta1=~-b,
              mup=~sqrt(abs(b)), mum=0)

## ----------------------------------------------------------------
alocal <- sdefit(m, x="height", t="age", unit="Seed",
          data=Loblolly, global=c(b=0.1, c=0.5), local=c(a=70))
alocal

## ----------------------------------------------------------------
(blocal <- sdefit(m, x="height", t="age", unit="Seed",
           data=Loblolly, global=c(a=70, c=0.5), local=c(b=0.1)))

## ----------------------------------------------------------------
(ablocal <- sdefit(m, x="height", t="age", unit="Seed",
            data=Loblolly, global=c(c=0.5), local=c(a=70, b=0.1)))

## ----------------------------------------------------------------
(blocal_mx <- sdefit(m, x="height", t="age", unit="Seed",
              data=Loblolly, global=c(a=70, c=0.5), local=c(b=0.1),
              method="nlme", control = nlme::nlmeControl(pnlsTol =
              0.01)))

## ----------------------------------------------------------------
coef(blocal_mx$fit)

## ----------------------------------------------------------------
m <- sdemodel(phi=~bc(x/a, c), beta0=0, beta1=~-b,
              mup=~sqrt(abs(b)))
sdefit(m, x="height", t="age", unit="Seed", data=Loblolly, 
       global=c(a=70, c=0.5), local=c(b=0.1))

## ----------------------------------------------------------------
lgLik <- eta <- seq(from=0, to=1, by=0.05)
for(i in seq_along(eta)){
  lgLik[i] <- (sdefit(m, x="height", t="age", unit="Seed",
               data=Loblolly, global=c(a=73, c=0.49),
               local=c(b=0.095), known=c(eta=eta[i]))$more
              )["logLik"]
}   
plot(lgLik ~ eta)

## ----------------------------------------------------------------
curve(unitran(x, alpha=-1, beta=0))

## ----------------------------------------------------------------
m <- sdemodel(phi=~unitran(x/a, alpha=alpha, beta=beta,
  reverse="no"), phiprime=~unitran_prime(x/a, alpha=alpha,
  beta=beta, reverse="no")/a, beta0=~b, beta1=0, mum=0)
(f <- sdefit(m, x="height", t="age", data=lob301, start=c(a=70,
  b=0.1, alpha=0.5, beta=0)))

## ----cleanup, include=FALSE, cache=FALSE--------------------------------------
options(old_options)

