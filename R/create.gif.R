# The BoutrosLab plotting.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO CREATE GIF  #######################################################################
create.gif <- function(exec.func, parameters, number.of.frames, delay = 40, filename) {
	png(filename = 'writeGifTemp%03d.png');
	for (i in (1:number.of.frames)) {
		print(do.call(exec.func, parameters[[i]]));
		}
	dev.off();
	# convert pngs to one gif using ImageMagick
	system(paste(paste('convert -delay ', delay), paste(' writeGifTemp* ', filename)));
	# cleaning up
	file.remove(list.files(pattern = 'writeGifTemp'));
	}
