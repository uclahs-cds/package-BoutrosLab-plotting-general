# The BoutrosLab.plotting.general package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.


thousands.split <- function(nums) {
	to.return <- {};
	for (k in c(1:length(nums))) {

		text <- format(nums[k], scientific = FALSE);

		num.sets <- trunc(nchar(text) / 3); #number of sets of 3
		remaining.letters <- nchar(text) %% 3; # remainder
		final.str <- ''; #string to be manipulated

		#handle remainder first (first set of < 3)
		if (remaining.letters != 0) {
			final.str <- substring(text, 1, remaining.letters);
			text <- substring(text, remaining.letters + 1, nchar(text));
			}

		#split the text up
		sst <- strsplit(text, '')[[1]];
		#grab sets of 3
		out <- paste0(sst[c(TRUE, FALSE, FALSE)], sst[c(FALSE, TRUE, FALSE)], sst[c(FALSE, FALSE, TRUE)]);
		#insert comma in between each set
		if (num.sets != 0) {
			for (i in c(1:num.sets)) {
				if (remaining.letters == 0 && i == 1) {
					final.str <- out[i];
					}
				else {
					final.str <- paste0(final.str, ',', out[i]);
					}
				}
			}
		to.return[[k]] <- final.str;
		}
	return(to.return);
	}
