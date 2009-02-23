methods::setOldClass("jags")
methods::setOldClass("bugs")
methods::setOldClass("mcmc.list")

methods::setClass("rjags",
     representation(
            model = "jags",
            BUGSoutput = "bugs")
)
