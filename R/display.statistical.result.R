# The BoutrosLab.plotting.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO DISPLAY STATISTICAL RESULT ########################################################
# Description:
#	Produces an expression containing a statistical result in scientific notation,
#	ready to be displayed in a plot.
# Input variables:
#	- The number to be put in scientific notation
#	- The statistics type ("P", "Q", etc.)
# Output variables: An expression.
display.statistical.result <- function(
	x, lower.cutoff = 2.2e-50, scientific.cutoff = 0.001, digits = 2, statistic.type = 'P', symbol = ': '
	) {

	# If x is smaller or equal to lower.cutoff, then the expression returned describes x
	# simply as being smaller than lower.cutoff
	if (lower.cutoff >= x) {
		pvalue <- substitute(
			expr = paste(statistic.type, ' < ', base %*% 10 ^ exponent, phantom('|')[phantom('|')]),
			env = list(
				base = unlist(
					BoutrosLab.plotting.general::scientific.notation(
						x = lower.cutoff,
						digits = digits,
						type = 'list'
						)[1]
					),
				exponent = unlist(
					BoutrosLab.plotting.general::scientific.notation(
						x = lower.cutoff,
						digits = digits,
						type = 'list'
						)[2]
					),
				statistic.type = statistic.type
				)
			);

		pvalue <- as.expression(pvalue);
		}

	# If x is greater or equal to scientific.cutoff, standard decimal notation is used in
	# the returned expression, rather than scientific notation
	else if (scientific.cutoff <= x) {
		pvalue <- as.expression(paste(statistic.type, symbol, signif(x, digits = digits), sep = ''));
		}

	# The following branch of the conditional statement is executed if and only if
	# lower.cutoff < x < scientific.cutoff.  In this case, in the final expression
	# x will be displayed in scientific notation.
	else {
		pvalue <- substitute(
			expr = paste(statistic.type, symbol, base %*% 10 ^ exponent, phantom('|')[phantom('|')]),
			env = list(
				base = unlist(
					BoutrosLab.plotting.general::scientific.notation(
						x = x,
						digits = digits,
						type = 'list'
						)[1]
					),
				exponent = unlist(
					BoutrosLab.plotting.general::scientific.notation(
						x = x,
						digits = digits,
						type = 'list'
						)[2]
					),
				statistic.type = statistic.type,
				symbol = symbol
				)
			);

		pvalue <- as.expression(pvalue);
		}

	return(pvalue);
	}
