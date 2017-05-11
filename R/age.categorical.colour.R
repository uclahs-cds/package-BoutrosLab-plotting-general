# The cpcgene.utilities package is copyright (c) 2014 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO OBTAIN AGE COLOUR SCHEME ##########################################################
age.categorical.colour <- function(x, scheme = "") {

	### default colour scheme
	default.bins <- c('<50', '50 - 60', '60 - 70','>= 70');

	if ( scheme %in% c("", "default")) {
		if (is.numeric(x) == TRUE) {
			warning("The default age colour scheme is applied. If want other age colour schemes, specify scheme.");
			}

		if (is.numeric(x) == FALSE & length(which(x %in% default.bins)) == 0) {
			stop("If want default colours the character values must be one of: '<50', '50 - 60', '60 - 70', or '>= 70' and scheme='' or 'default'. \nElse, must specify option: scheme.");
			}

		if (is.numeric(x) == FALSE & length(which(x %in% default.bins)) != 0) {
			x.processed <- x;
			}

		if (length(grep(x = x, '-|>|<')) == 0) {
			x <- as.numeric(x);
			x.processed <- rep('<50', length(x));
			x.processed[x >= 50 & x < 60] <- '50 - 60';
			x.processed[x >= 60 & x < 70] <- '60 - 70';
			if (any(x >= 70, na.rm = TRUE)) {
				x.processed[x >= 70] <- '>= 70';
				}
			if (any(is.na(x))) {
				x.processed[is.na(x)] <- NA;
				}
			}
		cols <- list(
			'<50' = "gray100",
			'50 - 60' = "gray66",
			'60 - 70' = "gray33",
			'>= 70' = "gray0"
			);
		}

	### prostate colour scheme
	prostate.bins <- c('<40', '40 - 50', '50 - 65', '65 - 70','>= 70');

	if (tolower(scheme) %in% c("pca", "prostate", "prad")) {
		if (is.numeric(x) == FALSE & length(which(x %in% prostate.bins)) == 0) {
			stop("If want prostate colours the character values must be one of: '<40', '40 - 50', '50 - 65', '65 - 70', or '>= 70', and scheme='prostate'.");
			}

		if (is.numeric(x) == FALSE & length(which(x %in% prostate.bins)) !=0 ) {
			x.processed <- x;
			}

		if (length(grep(x = x, '-|>|<')) == 0) {
			x <- as.numeric(x);
			x.processed <- rep('<40', length(x));
			x.processed[x >= 40 & x < 50] <- '40 - 50';
			x.processed[x >= 50 & x < 65] <- '50 - 65';
			x.processed[x >= 65 & x < 70] <- '65 - 70';
			if (any(x >= 70, na.rm = TRUE)) {
				x.processed[x >= 70] <- '>= 70';
				}
			if (any(is.na(x))) {
				x.processed[is.na(x)] <- NA;
				}
			}
		cols <- list(
			'<40' = "gray100",
			'40 - 50' = "gray75",
			'50 - 65' = "gray50",
			'65 - 70' = "gray25",
			'>= 70' = "gray0"
			);
		}

	return(
		as.vector( sapply( x.processed, function(y) { if (is.na(y)) return( 'slategrey' ); cols[[y]] } ))
		);
	}
