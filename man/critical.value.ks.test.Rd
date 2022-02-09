\name{critical.value.ks.test}
\alias{critical.value.ks.test}
\title{Critical Value for Kolmogorov-Smirnov Test}
\description{Takes a sample size and a confidence level and computes the corresponding critical value basing on the kolmogorov-smirnov test}
\usage{
critical.value.ks.test(n, conf, alternative = "two.sided");
}
\arguments{
  \item{n}{The sample size}
  \item{conf}{The confidence level}
  \item{alternative}{Indicates the alternative hypothesis and must be one of "two.sided"(default), "one-sided".}
}
\value{The corresponding critical value}
\author{Ying Wu}
\examples{
critical.value.ks.test(10, 0.95);
critical.value.ks.test(100, 0.95, alternative = "one-sided");
}
\keyword{htest}
