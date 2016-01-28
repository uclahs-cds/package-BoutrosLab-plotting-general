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

psa.categorical.colour <- function(x){
	x.processed <- x;
	if(length(grep(x=x, '-|>')) == 0){
        x <- as.numeric(x);
        x.processed <- rep('0 - 9.9', length(x));
        x.processed[x >=10 & x < 20] <- '10 - 19.9';
        if(any(x>=20, na.rm=TRUE)){
                x.processed[x>=20] <- '>= 20';
                }
        if(any(is.na(x))){
                x.processed[is.na(x)] <- NA;
                }
        #x.processed <- factor(unique(x.processed, levels = c('0 - 9.9', '10 - 19.9', '>= 20')));
        }
	cols <- list(
        '0 - 9.9' = "#FEE6CE",
        '10 - 19.9' = "#FDAE6B",
        '>= 20' = "#E6550D"
        );
	return(
        as.vector( sapply( x.processed, function(y) { if(is.na(y)) return( 'slategrey' ); cols[[y]] } ))
        );
    };
