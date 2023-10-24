\name{auto.axis}
\alias{auto.axis}
\title{Create ideal labels and values for a given numeric vector (detects log scales)}
\description{Takes a numeric vector and several parameters and outputs an object with values and labels ideal for given data}
\usage{
auto.axis(
    x,
    pretty = TRUE,
    log.scaled = NA,
    log.zero = 0.1,
    max.factor = 1,
    min.factor = 1,
    include.origin = TRUE,
    num.labels = 5,
    max.min.log10.diff = 2
)
}
\arguments{
    \item{x}{Numeric vector to be scaled}
    \item{pretty}{Parameter flag for if output should be in pretty format}
    \item{log.scaled}{parameter set to determine if scaling is logarithmic or not}
    \item{log.zero}{log 0 starting point}
    \item{max.factor}{maximum factor for y variable}
    \item{min.factor}{minimum factor for y variable}
    \item{include.origin}{flag to include the origin value or not}
    \item{num.labels}{number of labels to output}
    \item{max.min.log10.diff}{the max and min diffrence for dataset to be determined logarithmic}
}
\author{Takafumi N. Yamaguchi}
\seealso{\code{\link[lattice]{stripplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(223);
simple.data <- data.frame(
    x = sample(1:15, 10),
    y = LETTERS[1:10]
    );

auto.axis(simple.data$x)

data2 <- c(1,10,100,1000)

auto.axis(data2)
}
