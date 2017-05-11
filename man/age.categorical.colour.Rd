\name{age.categorical.colour}
\alias{age.categorical.colour}
\title{Provide age colour schemes.}
\description{Returns the standardized colours for patient age given a vector of values, either in numeric or binned form. Note function will only work for particular bin schemes.}
\usage{
age.categorical.colour(x, scheme);
}
\arguments{
    \item{x}{Vector of characters, corresponding to age colour scheme annotation function.}
    \item{scheme}{Default is categorizing age <50, 50 - 60, 60 - 70, and >=70. If scheme is specified then standardized colours for that particular scheme are given.}
}
\author{Veronica Sabelnykova}
\examples{
set.seed(42);

# data
age.numeric <- sample( seq(25, 80, 1), 200, replace = TRUE);
age.bins.default <- c( '<50', '50 - 60', '60 - 70', '>= 70');
age.bins.prostate <- c( '<40', '40 - 50', '50 - 65', '65 - 70', '>= 70');

# apply function
age.categorical.colour( age.numeric, scheme = "prostate");
age.categorical.colour( age.bins.default, scheme = "default");
age.categorical.colour( age.bins.prostate, scheme = "prostate");
}
\keyword{age}
\keyword{categorical}
\keyword{colour}
