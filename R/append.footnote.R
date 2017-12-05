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

### FUNCTION TO APPEND FOOTNOTE ###################################################################
append.footnote <- function(text = Sys.Date(), size = 0.5, colour = 'black', position = 'bottomright') {

	if (!is.null(text)) {
		pushViewport(viewport());
		if (position == 'middle') {
			grid.text(label = text,
				x = unit(0.5, 'npc'),
				y = unit(0.5, 'npc'),
				just = c('centre'),
				gp = gpar(cex = size, col = colour)
				);
			}
		else if (position == 'bottomleft') {
			grid.text(label = text,
				x = unit(2, 'mm'),
				y = unit(2, 'mm'),
				just = c('left', 'bottom'),
				gp = gpar(cex = size, col = colour)
				);
			}
		else if (position == 'topleft') {
			grid.text(label = text,
				x = unit(2, 'mm'),
				y = unit(1, 'npc') - unit(2, 'mm'),
				just = c('left', 'top'),
				gp = gpar(cex = size, col = colour)
				);
			}
		else if (position == 'topright') {
			grid.text(label = text,
				x = unit(1, 'npc') - unit(2, 'mm'),
				y = unit(1, 'npc') - unit(2, 'mm'),
				just = c('right', 'top'),
				gp = gpar(cex = size, col = colour)
				);
			}
		else {
			grid.text(label = text,
				x = unit(1, 'npc') - unit(2, 'mm'),
				y = unit(2, 'mm'),
				just = c('right', 'bottom'),
				gp = gpar(cex = size, col = colour)
				);
			}
		popViewport();
		}
	}
