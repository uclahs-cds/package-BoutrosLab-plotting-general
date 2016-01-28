# The cpcgene.utilities package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

gleason.score.colour <- function(x) {

	# check input class
	if(!is.character(x)){
		warning("gleason.score.colour function expects input to be of type character. Attempting to coerce to character.");
		x <- as.character(x);
		}

	gleason.score.colours <- list(
		'3+3' = 'white',
		'3+4' = 'yellow',
		'4+3' = 'orange',
		'4+4' = 'red',
		'4+5' = 'brown',
		'3+5' = 'maroon3',
		'5+3' = 'magenta4',
		'5+4' = 'mediumblue',
		'5+5' = 'black',
		'missing' = 'slategrey',
		'NA' = 'slategrey'
		);
	return(
		as.vector( sapply( x, function(y){ if(is.na(y)) return( 'slategrey' ); gleason.score.colours[[y]] } )) 
		);
}
