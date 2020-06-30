# The BoutrosLab.statistics.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

get.correlation.p.and.corr <- function(x, y, alternative = 'two.sided', method = 'pearson') {
	x = as.numeric(x);
	y = as.numeric(y);
	if( (method == 'spearman') & ( anyDuplicated(x) | anyDuplicated(y) ) ) {exact = FALSE;}
	else {exact = TRUE;}
	tryCatch(
		expr = as.vector(
			unlist(
				cor.test(
					x = x,
					y = y,
					alternative = alternative,
					method = method,
					exact = exact
					)[c('estimate', 'p.value')]
				)
			),
		error = function(e) { return ( c(NA, NA)); }
		);
	}
