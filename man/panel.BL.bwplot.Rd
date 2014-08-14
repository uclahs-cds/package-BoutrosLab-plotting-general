\name{panel.BL.bwplot}
\alias{panel.BL.bwplot}
\title{A lattice::panel.bwplot replacement that fixes colouring issues}
\description{Function lattice::bwplot() shows unexpected and unintuitive behaviour when colouring parameters of par.settings are vectors. The function panel.BL.bwplot fixes these issues. It should be called only from lattice::bwplot().
Use with caution. This function is invoked by create.boxplot}

\arguments{
    \item{...}{Pass through argument. See lattice::bwplot() for further details.}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
}
\author{Mehrdad Shamsi}
\seealso{\code{\link{create.boxplot}}}
\keyword{aplot}
