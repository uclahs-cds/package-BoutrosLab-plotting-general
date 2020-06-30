# The BoutrosLab.statistics.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

ks.test.critical.value <- function(n, conf, alternative = "two.sided") {
	
	if(alternative == "one-sided") conf <- 1- (1-conf)*2;

	# for the small sample size
	
	if (n < 50) {
		# use the exact distribution from the C code in R
		exact.kolmogorov.pdf <- function(x) {
			p <- .Call("pKolmogorov2x", p = as.double(x), as.integer(n), PACKAGE = "BoutrosLab.plotting.general");
			return(p - conf);
		}
		
		critical.value <- uniroot(exact.kolmogorov.pdf, lower = 0, upper = 1)$root;
		}

	# if the sample size is large(>50), under the null hypothesis, the absolute value of the difference
	# of the empirical cdf and the theoretical cdf should follow a kolmogorov distribution
	
	if (n >= 50) {
		# pdf of the kolmogorov distribution minus the confidence level
		kolmogorov.pdf <- function(x) { 
			i <- 1:10^4;
			sqrt(2*pi) / x * sum(exp(-(2*i - 1)^2*pi^2/(8*x^2))) - conf;	
			}

		# the root of the function above
		# is the critical value for a specific confidence level multiplied by sqrt(n);
		critical.value <- uniroot(kolmogorov.pdf , lower = 10^(-6), upper = 3)$root / sqrt(n);
		}

	return(critical.value);
	}
