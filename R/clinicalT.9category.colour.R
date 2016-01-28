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

clinicalT.9category.colour <- function(x){
    x.processed <- toupper(as.character(x));
    cols <- list(
        'T1A' = rgb(143,161,82, maxColorValue = 255),
        'T1B' = rgb(178,200,105, maxColorValue = 255),
        'T1C' = rgb(221,246,139, maxColorValue = 255),
        'T2A' = rgb(56,145,58, maxColorValue = 255),
        'T2B' = rgb(109,196,110, maxColorValue = 255),
        'T2C' = rgb(179,238,180, maxColorValue = 255),
        'T3A' = rgb(47,109,96, maxColorValue = 255),
        'T3B' = rgb(94,194,170, maxColorValue = 255),
        'T3C' = rgb(192,231,222, maxColorValue = 255)
        );
    return(
        as.vector( sapply( x.processed, function(y) { if(is.na(y)) return( 'slategrey' ); cols[[y]] } ))
        );

	}