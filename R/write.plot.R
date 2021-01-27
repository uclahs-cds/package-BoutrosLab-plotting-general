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
### FUNCTION TO WRITE PLOT  #######################################################################
write.plot <- function(
	trellis.object, filename = NULL, additional.trellis.objects = NULL, additional.trellis.locations = NULL,
	height = 6, width = 6, size.units = 'in', resolution = 1000, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general'
	) {
	if (!is.null(filename)) {

		# check all potential locations for the extension->functionname mapping file
		data.directories <- file.path(.libPaths(), 'BoutrosLab.plotting.general');
		file.checks <- file.exists(file.path(data.directories, 'ext2function.txt'));

		if (any(file.checks)) {
			data.directory <- data.directories[order(file.checks, decreasing = TRUE)[1]];
			}
		else {
			stop('Unable to find ext2function file.');
			}

		# read in the mapping table
		mapping.object <- read.table(
			paste(data.directory, 'ext2function.txt', sep = '/'),
			sep = '\t',
			header = TRUE,
			as.is = TRUE
			);

		rownames(mapping.object) <- mapping.object$FileExt;

		# set the graphics driver
		old.type <- getOption('bitmapType');
		if (capabilities('cairo')) {
			options(bitmapType = 'cairo');
			}

		for (i in c(1:length(filename))) {

			# determine which function to use
			extension <- file_ext(filename[i]);

			# if no extension is passed, use TIFF
			if ('' == extension) {
				extension <- 'tiff';
				}

			# set ps.options for eps
			if ('eps' == extension) {
				setEPS();
				}

			# grab the mapping object
			if (!extension %in% rownames(mapping.object)) {
				stop(paste('Unknown extension:', extension, '.'));
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

			call.args$filename <- filename[i];
			do.call(call.param$Function, call.args);

			# plot the object to the file
			plot(trellis.object, newpage = FALSE);

			# MANY checks for correctness of additional plots to embedded and parameters
			if (
				(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) != 'list') ||
				(!is.null(additional.trellis.locations) && typeof(additional.trellis.locations) != 'list')
				) {
				# checks if trellis objects and locations are provided in a list
				stop('Additional trellis objects and their locations must be provided in a list.');
				}
			else if (
				(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) == 'list') &&
				(is.null(additional.trellis.locations) || typeof(additional.trellis.locations) != 'list')
				) {
				# checks if coordinates are specified in a list if trellis objects are provided
				stop('If trellis objects are specified, their coordinates must be provided in a list.');
				}
			else if (
				(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) == 'list') &&
				(!is.null(additional.trellis.locations) && typeof(additional.trellis.locations) == 'list')
				) {
				# checks if elements denoting coordinates for trellis objects are appropriately named
				# once the type of the parameters are deemed correct
				# trellis locations object should have the following elements: xleft, ybottom, xright, ytop
				if (
					!exists('xleft', where = additional.trellis.locations) ||
					!exists('ybottom', where = additional.trellis.locations) ||
					!exists('xright', where = additional.trellis.locations) ||
					!exists('ytop', where = additional.trellis.locations)
					) {
					stop('Locations for trellis objects must be specified using: xleft, ybottom, xright, ytop.');
					}

				# checking lengths of inputs
				input.lengths <- list(
					length(additional.trellis.objects),
					length(additional.trellis.locations$xleft),
					length(additional.trellis.locations$ybottom),
					length(additional.trellis.locations$xright),
					length(additional.trellis.locations$ytop)
					);

				# only proceed if inputs are equal
				if (length(unique(input.lengths)) != 1) {
					stop('Lists of trellis objects and coordinates provided not equal in length.');
					}
				else if (length(unique(input.lengths)) == 1) {
					for (i in 1:length(additional.trellis.objects)) {
						print(
							x = additional.trellis.objects[[i]],
							position = c(
								additional.trellis.locations$xleft[i],
								additional.trellis.locations$ybottom[i],
								additional.trellis.locations$xright[i],
								additional.trellis.locations$ytop[i]
								),
							newpage = FALSE
							);
						}
					}
				}

			dev.off();
			options(bitmapType = old.type);

			if ('eps' == extension) {
				# revert back to standard configuration
				ps.options(reset = TRUE);
				}

			# write image metadata
			BoutrosLab.plotting.general::write.metadata(
				filename = filename[i],
				description = description
				);
			}
		}

	else if (is.null(filename)) {
		# MANY checks for correctness of additional plots to embedded and parameters
		if (
			(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) != 'list') ||
			(!is.null(additional.trellis.locations) && typeof(additional.trellis.locations) != 'list')
			) {
			# checks if trellis objects and locations are provided in a list
			stop('Additional trellis objects and their locations must be provided in a list.');
			}
		else if (
			(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) == 'list') &&
			(is.null(additional.trellis.locations) || typeof(additional.trellis.locations) != 'list')
			) {
			# checks if coordinates are specified in a list if trellis objects are provided
			stop('If trellis objects are specified, their coordinates must be provided in a list.');
			}
		else if (
			(!is.null(additional.trellis.objects) && typeof(additional.trellis.objects) == 'list') &&
			(!is.null(additional.trellis.locations) && typeof(additional.trellis.locations) == 'list')
			) {
			# checks if elements denoting coordinates for trellis objects are appropriately named
			# once the type of the parameters are deemed correct
			# trellis locations object should have the following elements: xleft, ybottom, xright, ytop
			if (
				!exists('xleft', where = additional.trellis.locations) ||
				!exists('ybottom', where = additional.trellis.locations) ||
				!exists('xright', where = additional.trellis.locations) ||
				!exists('ytop', where = additional.trellis.locations)
				) {
				stop('Locations for trellis objects must be specified using: xleft, ybottom, xright, ytop.');
				}
			# checking lengths of inputs
			input.lengths <- list(
				length(additional.trellis.objects),
				length(additional.trellis.locations$xleft),
				length(additional.trellis.locations$ybottom),
				length(additional.trellis.locations$xright),
				length(additional.trellis.locations$ytop)
				);
			# only proceed if inputs are equal
			if (length(unique(input.lengths)) != 1) {
				stop('Lists of trellis objects and coordinates provided not equal in length.');
				}
			else if (length(unique(input.lengths)) == 1) {
				# plot the object to the file
				plot(trellis.object, newpage = TRUE);
				for (i in 1:length(additional.trellis.objects)) {
					print(
						x = additional.trellis.objects[[i]],
						position = c(
							additional.trellis.locations$xleft[i],
							additional.trellis.locations$ybottom[i],
							additional.trellis.locations$xright[i],
							additional.trellis.locations$ytop[i]
							),
						newpage = FALSE
						);
					}
				}
			}
		else {
			return(trellis.object);
			}
		}

	# check if graphics device is postscript
	if ('postscript' %in% rownames(as.matrix(dev.cur()))) {
		ps.options(family = 'sans');
		}

	# check if graphics device is not set i-e 'null device'
	if (enable.warnings && 1 == dev.cur()) {
		warning('\nIf you wish to print this plot to postscript device, please set family param as: postscript(family=\"sans\").\n');
		}
	}
