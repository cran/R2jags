methods::setOldClass("jags")
methods::setOldClass("bugs")

methods::setClass("rjags",
     representation(
            model = "jags",
            BUGSoutput = "bugs")
)
