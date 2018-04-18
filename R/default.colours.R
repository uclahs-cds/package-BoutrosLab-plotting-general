# The BoutrosLab.plotting.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO RETURN DEFAULT COLOUR PALETTES ####################################################
default.colours <- default.colors <- function(
	number.of.colours = 2, palette.type = 'qual', is.greyscale = TRUE, is.venn = FALSE
	) {

	### HANDLE VENN DIAGRAM CASES #################################################################
	# Default Venn diagram colour schemes
	venn.cols <- c('red', 'dodgerblue', 'yellow');
	venn.text <- c('darkred', 'darkblue', 'darkorange');
	venn4.cols <- c('dodgerblue', 'springgreen', 'mediumpurple', 'palevioletred');
	venn4.text <- c('darkblue', 'darkgreen', 'darkorchid4', 'darkred');

	# Check if input combinations are valid
	if (!is.null(palette.type) && is.venn == TRUE) {
		warning('Do not specify a palette type if using a venn diagram. Setting palette type to NULL.');
		palette.type <- NULL;
		}

	else if (is.null(palette.type) && is.venn == FALSE) {
		stop('No palette type is specified.');
		}

	else if (length(number.of.colours) > 1 && is.venn == TRUE) {
		warning('Multiple venn diagram colour schemes is not supported. Setting number.of.colours to first number specified.');
		number.of.colours <- number.of.colours[1];
		}

	# Create venn diagram palette
	if (is.venn) {
		if (number.of.colours <= 3) {
			palette <- venn.cols[1:number.of.colours];
			palette.text <- venn.text[1:number.of.colours];
			}
		else if (number.of.colours == 4) {
			palette <- venn4.cols;
			palette.text <- venn4.text;
			}
		else {
			stop('There is no venn diagram colour scheme consisting of more than 4 colours available.');
			}

		return(list(palette, palette.text));
		}

	### CREATE ALL OTHER PALETTES ##################################################################
	div.one <- rgb(179, 43, 43, maxColorValue = 255);
	div.two <- rgb(221, 78, 78, maxColorValue = 255);
	div.thr <- rgb(235, 124, 124, maxColorValue = 255);
	div.fou <- rgb(247, 190, 190, maxColorValue = 255);
	div.fiv <- rgb(190, 244, 247, maxColorValue = 255);
	div.six <- rgb(128, 205, 209, maxColorValue = 255);
	div.sev <- rgb(69, 180, 187, maxColorValue = 255);
	div.eig <- rgb(24, 139, 145, maxColorValue = 255);

	col.fiv <- rgb(255 / 255, 225 / 255, 238 / 255);
	col.fou <- rgb(244 / 255, 224 / 255, 166 / 255);
	col.thr <- rgb(177 / 255, 211 / 255, 154 / 255);
	col.two <- rgb(101 / 255, 180 / 255, 162 / 255);
	col.one <- rgb(51 / 255, 106 / 255, 144 / 255);

	pastel.one <- rgb(250, 229, 161, maxColorValue = 255);
	pastel.two <- rgb(153, 193, 154, maxColorValue = 255);
	pastel.thr <- rgb(114, 95, 122, maxColorValue = 255);
	pastel.fou <- rgb(180, 105, 112, maxColorValue = 255);
	pastel.fiv <- rgb(253, 252, 183, maxColorValue = 255);
	pastel.six <- rgb(135, 179, 196, maxColorValue = 255);
	pastel.sev <- rgb(133, 161, 115, maxColorValue = 255);
	pastel.eig <- rgb(192, 153, 104, maxColorValue = 255);
	pastel.nin <- rgb(203, 116, 245, maxColorValue = 255);
	pastel.ten <- rgb(140, 201, 174, maxColorValue = 255);
	pastel.ele <- rgb(190, 123, 187, maxColorValue = 255);
	pastel.twe <- rgb(172, 232, 233, maxColorValue = 255);

	spiral.morning.one <- rgb(84, 42, 133, maxColorValue = 255);
	spiral.morning.two <- rgb(185, 47, 90, maxColorValue = 255);
	spiral.morning.thr <- rgb(217, 113, 62, maxColorValue = 255);
	spiral.morning.fou <- rgb(225, 199, 93, maxColorValue = 255);
	spiral.morning.fiv <- rgb(234, 255, 128, maxColorValue = 255);
	spiral.morning.six <- rgb(204, 247, 210, maxColorValue = 255);

	spiral.dusk.one <- rgb(60, 78, 176, maxColorValue = 255);
	spiral.dusk.two <- rgb(130, 94, 188, maxColorValue = 255);
	spiral.dusk.thr <- rgb(198, 129, 216, maxColorValue = 255);
	spiral.dusk.fou <- rgb(248, 180, 227, maxColorValue = 255);
	spiral.dusk.fiv <- rgb(255, 229, 226, maxColorValue = 255);

	spiral.afternoon.one <- rgb(132, 58, 28, maxColorValue = 255);
	spiral.afternoon.two <- rgb(164, 141, 35, maxColorValue = 255);
	spiral.afternoon.thr <- rgb(91, 203, 142, maxColorValue = 255);
	spiral.afternoon.fou <- rgb(137, 195, 208, maxColorValue = 255);
	spiral.afternoon.fiv <- rgb(214, 222, 255, maxColorValue = 255);

	spiral.dawn.one <- rgb(143, 56, 185, maxColorValue = 255);
	spiral.dawn.two <- rgb(215, 99, 195, maxColorValue = 255);
	spiral.dawn.thr <- rgb(224, 134, 150, maxColorValue = 255);
	spiral.dawn.fou <- rgb(232, 190, 162, maxColorValue = 255);
	spiral.dawn.fiv <- rgb(241, 235, 148, maxColorValue = 255);

	spiral.noon.one <- rgb(19, 10, 102, maxColorValue = 255);
	spiral.noon.two <- rgb(13, 80, 140, maxColorValue = 255);
	spiral.noon.thr <- rgb(90, 191, 87, maxColorValue = 255);
	spiral.noon.fou <- rgb(230, 226, 92, maxColorValue = 255);
	spiral.noon.fiv <- rgb(255, 193, 206, maxColorValue = 255);

	spiral.night.one <- rgb(67, 26, 44, maxColorValue = 255);
	spiral.night.two <- rgb(43, 35, 120, maxColorValue = 255);
	spiral.night.thr <- rgb(21, 67, 137, maxColorValue = 255);
	spiral.night.fou <- rgb(43, 185, 197, maxColorValue = 255);
	spiral.night.fiv <- rgb(205, 231, 64, maxColorValue = 255);

	### ORGANIZE PALETTES ##########################################################################
	# Add new schemes to this list
	schemes <- list(
		dotmap = c('dodgerblue2', 'darkorange1'),
		seq = BoutrosLab.plotting.general::colour.gradient('chartreuse4', 5),
		div = c(div.one, div.two, div.thr, div.fou, div.fiv, div.six, div.sev, div.eig),
		survival = c('royalblue2', 'firebrick', 'chartreuse3', 'purple4', 'plum1', 'orange', 'maroon3', 'turquoise3', 'chocolate4', 'lightcoral'),
		qual = c('orange', 'chartreuse4', 'darkorchid4', 'gold', 'dodgerblue', 'firebrick3',
			'yellowgreen', 'darkorange1', 'slateblue4', 'seagreen3', 'violetred3', 'turquoise3'),
		pastel = c(pastel.one, pastel.two, pastel.thr, pastel.fou, pastel.fiv, pastel.six, pastel.sev,
			pastel.eig, pastel.nin, pastel.ten, pastel.ele, pastel.twe),
		spiral.dawn =  c(spiral.dawn.one, spiral.dawn.two, spiral.dawn.thr, spiral.dawn.fou, spiral.dawn.fiv),
		spiral.sunrise = c(col.one, col.two, col.thr, col.fou, col.fiv),
		spiral.morning = c(spiral.morning.one, spiral.morning.two, spiral.morning.thr, spiral.morning.fou, spiral.morning.fiv, spiral.morning.six),
		spiral.noon = c(spiral.noon.one, spiral.noon.two, spiral.noon.thr, spiral.noon.fou, spiral.noon.fiv),
		spiral.afternoon = c(spiral.afternoon.one, spiral.afternoon.two, spiral.afternoon.thr, spiral.afternoon.fou, spiral.afternoon.fiv),
		spiral.dusk = c(spiral.dusk.one, spiral.dusk.two, spiral.dusk.thr, spiral.dusk.fou, spiral.dusk.fiv),
		spiral.night = c(spiral.night.one, spiral.night.two, spiral.night.thr, spiral.night.fou, spiral.night.fiv),
		old.seq = c('darkolivegreen3', 'darkolivegreen4', 'darkolivegreen', 'darkgreen', 'black'),
		old.div = c('darkorange', 'darkolivegreen4', 'goldenrod1', 'darkgreen', 'darkolivegreen3', 'orange',
			'darkolivegreen', 'darkorange3'),
		old.qual1 = c('orange', 'chartreuse4', 'darkorchid4', 'firebrick3', 'lightgrey', 'tan4', 'dodgerblue', 'orchid', 'black'),
		old.qual2 = c('orange', 'chartreuse4', 'darkorchid4', 'firebrick3', 'gold', 'dodgerblue', 'yellowgreen',
			'darkorange1', 'slateblue4', 'seagreen3', 'violetred3', 'turquoise3'),
		chromosomes = c('darkred', 'firebrick1', 'pink1', 'darkorange3', 'darkorange', 'tan1', 'goldenrod3',
			'gold', 'khaki', 'darkgreen', 'forestgreen', 'greenyellow', 'darkblue', 'dodgerblue', 'skyblue',
			'darkslateblue', 'slateblue3', 'mediumpurple1', 'darkorchid4', 'orchid3', 'plum', 'violetred', 'grey31', 'grey0'),
		seq.yellowgreen = c('lightyellow', 'darkolivegreen1', 'lawngreen', 'chartreuse3', 'green4', 'darkgreen'),
		seq.green = c('mintcream', 'darkseagreen1', 'lightgreen', 'springgreen3', 'springgreen4', 'darkgreen'),
		seq.greenblue = c('lightcyan', 'paleturquoise', 'turquoise1', 'darkturquoise', 'darkcyan', 'darkslategray'),
		seq.blue = c('aliceblue', 'lightblue1', 'lightskyblue', 'deepskyblue', 'dodgerblue3', 'dodgerblue4'),
		seq.bluepurple = c('aliceblue', 'lightsteelblue1', 'cornflowerblue', 'mediumslateblue', 'blueviolet', 'slateblue4'),
		seq.purple = c('thistle1', 'plum1', 'orchid1', 'orchid3', 'orchid4', 'mediumpurple4'),
		seq.purplered = c('lavenderblush', 'pink', 'palevioletred1', 'violetred1', 'maroon', 'violetred4'),
		seq.redorange = c('peachpuff', 'lightsalmon', 'coral', 'orangered', 'orangered3', 'orangered4'),
		seq.orange = c('papayawhip', 'navajowhite', 'darkgoldenrod1', 'darkorange', 'darkorange3', 'darkorange4')
		);

	# additional sequential colour schemes for cases when multiple sequential or binary schemes are requested
	# this list specifies the order in which they are returned
	sequential.schemes <- list(
		seq.yellowgreen = schemes$seq.yellowgreen,
		seq.green = schemes$seq.green,
		seq.greenblue = schemes$seq.greenblue,
		seq.blue = schemes$seq.blue,
		seq.bluepurple = schemes$seq.bluepurple,
		seq.purple = schemes$seq.purple,
		seq.purplered = schemes$seq.purplered,
		seq.redorange = schemes$seq.redorange,
		seq.orange = schemes$seq.orange
		);


	### CREATE PALETTE #############################################################################
	# check if number of colour schemes requested equals number of palette types specified (else return error)
	if (length(number.of.colours) != length(palette.type)) {
		stop('The number of colours schemes requested must equal the number of palette types specified.');
		}

	# Create palette
	if (length(number.of.colours) > 1) {
		palette <- vector(mode = 'list', length = length(number.of.colours));
		}
	else { palette <- c(); }

	# Return all the palettes for display
	if (1 == length(palette.type) && palette.type == 'all') { return(schemes); }

	# Keep track of used schemes
	used.schemes <- c();

	for (i in 1:length(number.of.colours)) {

		# Checking if palette.types are available
		if (length(schemes[[palette.type[i]]]) < 1 && 'binary' != palette.type[i]) {
			stop(paste('Invalid palette type', palette.type[i], 'is specified.'));
			}

		# check that the scheme has not been repeated
		if ('seq' != palette.type[i] && 'binary' != palette.type[i]) {
			if (palette.type[i] %in% used.schemes) {
				warning('Duplicate palettes will be returned.');
				}
			}

		# count how many sequential schemes have already been used
		seq.count <- sum(c(used.schemes == 'seq', used.schemes == 'binary'));

		used.schemes <- c(used.schemes, palette.type[i]);

		# the first binary colour scheme will be white/black
		binary.colour <- 'black';

		if (seq.count > 0 && seq.count < length(sequential.schemes)) {
			if ('seq' == palette.type[i]) {
				palette.type[i] <- names(sequential.schemes[i]);
				}
			else if ('binary' == palette.type[i]) {
				binary.colour <- sequential.schemes[[names(sequential.schemes[i])]][4];
				print(binary.colour);
				}
			}

		# check if binary schemes requested
		if (palette.type[i] == 'binary' && as.numeric(number.of.colours[i]) > 2) {
			warning('Binary colour schemes only return white and one other colour.');
			}

		# warning if the duplicate scheme returned
		if (9 <= seq.count) {
			warning('Duplicate palettes will be returned.');
			}

		# check that the reqested number of colours exists for the palette
		if (length(schemes[[palette.type[i]]]) > 1 && number.of.colours[i] > length(schemes[[palette.type[i]]])) {
			stop(paste0('The requested ', palette.type[i], ' colour scheme has a length of ',
					length(schemes[[palette.type[i]]]), ' colours. You requested ', number.of.colours[i], '.'));
			}
		else if (length(sequential.schemes[[palette.type[i]]]) > 1 && number.of.colours[i] > length(sequential.schemes[[palette.type[i]]])) {
			stop(paste0('The requested ', palette.type[i], ' colour scheme has a length of ',
					length(sequential.schemes[[palette.type[i]]]), ' colours. You requested ', number.of.colours[i], '.'));
			}

		# Add to the final palette
		if (length(schemes[[palette.type[i]]]) > 1) {
			if (1 == length(number.of.colours)) { palette <- schemes[[palette.type[i]]][1:number.of.colours[i]]; }
			else { palette[[i]] <- schemes[[palette.type[i]]][1:number.of.colours[i]]; }
			}

		else if (length(sequential.schemes[[palette.type[i]]]) > 1) {
			if (1 == length(number.of.colours)) { palette <- sequential.schemes[[palette.type[i]]][1:number.of.colours[i]]; }
			else { palette[[i]] <- sequential.schemes[[palette.type[i]]][1:number.of.colours[i]]; }
			}

		else if ('binary' == palette.type[i]) {
			if (1 == length(number.of.colours)) { palette <- c('white', 'black'); }
			else { palette[[i]] <- c('white', binary.colour); }
			}

		### CHECK GREYSCALE ############################################################################
		# internal function to estimate greyscale compatibility
		check.greyscale <- function(palette.to.check) {

			grey.cols <- vector(length = length(palette.to.check));
			i <- 0;

			for (col in palette.to.check) {
				rgbcol <- col2rgb(col);
				greyval <- (0.2989 * rgbcol[1, 1]) + (0.5870 * rgbcol[2, 1]) + (0.1140 * rgbcol[3, 1]);
				greyval <- greyval / 2.55;
				greyval <- round(greyval);
				i <- i + 1;
				grey.cols[i] <- greyval;
				}

			minimum.difference <- min(diff(sort(grey.cols)));

			# The cutoff value could be more scientifically chosen...
			if (minimum.difference < 10) {
				warning('Colour scheme may not be greyscale compatible.');
				}
			}

		# run greyscale check (only if there is more than one colour selected)
		if (is.greyscale && palette.type[i] != 'binary') {
			if (1 == number.of.colours[i]) { next; }
			else if (1 == length(number.of.colours)) { check.greyscale(palette); }
			else { check.greyscale(palette[[i]]); }
			}
		}

	return(palette);
	}
