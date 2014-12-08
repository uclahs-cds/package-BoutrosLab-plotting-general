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

### FUNCTION TO WRITE PLOT  #######################################################################
write.plot <- function(trellis.object, filename = NULL, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, description = NULL) {

	# if requested create the image file
	if (!is.null(filename)) {

		# check all potential locations for the extension->functionname mapping file
		data.directories <- file.path(.libPaths(), 'BoutrosLab.plotting.general');
		file.checks <- file.exists(file.path(data.directories, 'ext2function.txt'));

		if (any(file.checks)) {
			data.directory <- data.directories[ order(file.checks, decreasing = TRUE)[1] ];
			}
		else {
			stop('Unable to find ext2function file');
			}

		# read in the mapping table
		mapping.object <- read.table(
			paste(data.directory, 'ext2function.txt', sep='/'),
			sep = "\t",
			header = TRUE,
			as.is = TRUE
			);
		rownames(mapping.object) <- mapping.object$FileExt;

		# set the graphics driver
		old.type <- getOption('bitmapType');
		options(bitmapType = 'cairo');

		# determine which function to use
		extension <- sub('(.+)\\.', '', filename, perl = TRUE);

		# if no extension is passed, use TIFF
		if (extension == filename) {
			extension = 'tiff';
			}

		# set ps.options for eps
		if ('eps' == extension) {
			setEPS();
			}

		# grab the mapping object
		if (! extension %in% rownames(mapping.object)) {
			stop(paste('Unknown extension:', extension));
			}
		call.param <- mapping.object[extension, , drop = FALSE];

		# set up the list of arguments
		call.args <- list();
		for (param in strsplit(call.param$Args, ';')[[1]]) {
			param.split <- strsplit(param, '=')[[1]];
			arg <- param.split[1];
			val <- param.split[2];
			# do not perform 'get' if the value begins with $ (our marker)
			if (any(grep('^\\$', val, perl = TRUE))) {
				val <- gsub('^\\$', '', val, perl = TRUE);
				call.args[[arg]] <- val;
				}
			else {
				call.args[[arg]] <- get(val);
				}
			}

		do.call(call.param$Function, call.args);

		# plot the object to the file
		plot(trellis.object);
		dev.off();
		options(bitmapType = old.type);

		if ('eps' == extension) {
			# revert back to standard configuration
			ps.options(reset = TRUE);
			}
	
		# write image metadata
		BoutrosLab.plotting.general::write.metadata(
			filename = filename, 
			description = description
			);

		# return a success marker
		return(1);
		}

	# check if graphics device is postscript
	if ('postscript' %in% rownames(as.matrix(dev.cur()))) {
		ps.options(family = 'sans');
		}
	
	# check if graphics device is not set i-e "null device"
	if (enable.warnings && 1 == dev.cur()) {
		warning("\nIf you wish to print this plot to postscript device, please set family param as: postscript(family=\"sans\")\n");
		}
	
	# if no plot requested, return the trellis object itself
	return(trellis.object);
	}
