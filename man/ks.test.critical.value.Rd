\name{ks.test.critical.value}
\alias{ks.test.critical.value}
\title{Critical Value for Kolmogorov-Smirnov Test}
\description{Takes a sample size and a confidence level and computes the corresponding critical value basing on the kolmogorov-smirnov test}
\usage{
ks.test.critical.value(n, conf, alternative = "two.sided");
}
\arguments{
  \item{n}{The sample size}
  \item{conf}{The confidence level}
  \item{alternative}{Indicates the alternative hypothesis and must be one of "two.sided"(default), "one-sided".}
}
\value{The corresponding critical value}
\author{Ying Wu}
\examples{
ks.test.critical.value(10, 0.95);
ks.test.critical.value(100, 0.95, alternative = "one-sided");
}
\keyword{htest}
