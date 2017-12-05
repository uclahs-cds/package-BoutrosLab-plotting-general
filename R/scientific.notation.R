# The BoutrosLab.plotting.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO CONVERT TO SCIENTIFIC NOTATION ####################################################
scientific.notation <- function(x, digits = 1, type = 'expression') {

	# handle lists appropriately
	if (length(x) > 1) {
		return(
			append(
				BoutrosLab.plotting.general::scientific.notation(x[1]),
				BoutrosLab.plotting.general::scientific.notation(x[-1])
				)
			);
		}

	# handle zeros
	if (!x) { return(0); }

	# determine the exponent & base
	exponent <- floor(log10(x));
	base <- sprintf(paste('%.', digits, 'f', sep = ''), x / 10 ^ exponent);

	# return the value
	if (type == 'expression') {
		return(as.expression(substitute(base %*% 10 ^ exponent, list(base = base, exponent = exponent))));
		}
	else if (type == 'list') {
		return(list(base = base, exponent = exponent));
		}
	else {
		warning("Return type can only be 'expression' or 'list'.");
		}
	}
