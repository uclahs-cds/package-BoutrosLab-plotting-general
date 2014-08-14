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

# NB: follow these instructions to add a new palette type
# 1.  Add name of new palette to possible.palette.types
# 2.  Add new palette to the "colour schemes" section.  
# 3.  Add else if branch to conditional statement at the end of the function
#  This branch will be executed only if the value of palette.type is the
#  name of your new palette.  Include if statement to check if the user asked
#  for more colours than there are available - if so, throw an error.
#  Find the largest integer k such that the first k colours in your colour
#  scheme are greyscale compatible, and throw a warning if the user requests
#  more than that many colours and is.greyscale = TRUE

### FUNCTION TO RETURN DEFAULT COLOUR PALETTES ####################################################
default.colours <- function(number.of.colours = 2, palette.type = 'qual', is.greyscale = TRUE, is.venn = FALSE) {

	# List of all possible palette types (i.e. all valid values for palette.type parameter)
	possible.palette.types <- c('seq', 'old.seq', 'div', 'old.div', 'qual', 'old.qual', 'chromosomes', 'survival', 'dotmap', 'binary', 'spiral.sunrise', 'pastel', 'spiral.morning', 'spiral.dusk', 'spiral.afternoon', 'spiral.dawn');

	# legacy colour schemes
	old.sequential <- c("darkolivegreen3", "darkolivegreen4", "darkolivegreen", "darkgreen", "black");
	old.diverging <- c("darkorange","darkolivegreen4", "goldenrod1", "darkgreen", "darkolivegreen3", "orange", "darkolivegreen", "darkorange3");
	old.qualitative <- c("orange","chartreuse4","darkorchid4", "firebrick3", "lightgrey", "tan4", "dodgerblue", "orchid", "black");

	# standard colour schemes
	sequential <- BoutrosLab.plotting.general::colour.gradient('chartreuse4', 5);
		
	div_one <- rgb(179, 43, 43, maxColorValue = 255);
	div_two <- rgb(221, 78, 78, maxColorValue = 255);
	div_thr <- rgb(235, 124, 124, maxColorValue = 255);
	div_fou <- rgb(247, 190, 190, maxColorValue = 255);
	div_fiv <- rgb(190, 244, 247, maxColorValue = 255);
	div_six <- rgb(128, 205, 209, maxColorValue = 255);
	div_sev <- rgb(69, 180, 187, maxColorValue = 255);
	div_eig <- rgb(24, 139, 145, maxColorValue = 255);
	diverging <- c(div_one, div_two, div_thr, div_fou, div_fiv, div_six, div_sev, div_eig); 
	
	qualitative <- c("orange", "chartreuse4", "darkorchid4", "firebrick3", "gold", "dodgerblue", "yellowgreen", "darkorange1", "slateblue4", "seagreen3", "violetred3", "turquoise3")
	chromosomes <- c("darkred", "firebrick1", "pink1","darkorange3","darkorange","tan1","goldenrod3","gold","khaki","darkgreen","forestgreen","greenyellow", "darkblue","dodgerblue","skyblue","darkslateblue","slateblue3","mediumpurple1","darkorchid4","orchid3","plum","violetred","grey31","grey0");
	survival <- c("royalblue2","firebrick","chartreuse3","purple4","plum1", "orange","maroon3","turquoise3","chocolate4","lightcoral");
	dotmap <- c("darkorange1", "dodgerblue2");

	# general use colour schemes

	col_fiv <- rgb(255/255, 225/255, 238/255);
	col_fou <- rgb(244/255, 224/255, 166/255);
	col_thr <- rgb(177/255, 211/255, 154/255);
	col_two <- rgb(101/255, 180/255, 162/255);
	col_one <- rgb(51/255, 106/255, 144/255);
	spiral.sunrise <- c(col_one, col_two, col_thr, col_fou, col_fiv);

	pastel_one <- rgb(250, 229, 161, maxColorValue = 255);
	pastel_two <- rgb(153, 193, 154, maxColorValue = 255);
	pastel_thr <- rgb(114, 95, 122, maxColorValue = 255);
	pastel_fou <- rgb(180, 105, 112, maxColorValue = 255);
	pastel_fiv <- rgb(253, 252, 183, maxColorValue = 255);
	pastel_six <- rgb(135, 179, 196, maxColorValue = 255);
	pastel_sev <- rgb(133, 161, 115, maxColorValue = 255);
	pastel_eig <- rgb(192, 153, 104, maxColorValue = 255);
	pastel_nin <- rgb(203, 116, 245, maxColorValue = 255);
	pastel_ten <- rgb(140, 201, 174, maxColorValue = 255);
	pastel_ele <- rgb(190, 123, 187, maxColorValue = 255);
	pastel_twe <- rgb(172, 232, 233, maxColorValue = 255);
	pastel <- c(pastel_one, pastel_two, pastel_thr, pastel_fou, pastel_fiv, pastel_six, pastel_sev, pastel_eig, pastel_nin, pastel_ten, pastel_ele, pastel_twe);

	spiral.morning_one <- rgb(84, 42, 133, maxColorValue = 255);
	spiral.morning_two <- rgb(185, 47, 90, maxColorValue = 255);
	spiral.morning_thr <- rgb(217, 113, 62, maxColorValue = 255);
	spiral.morning_fou <- rgb(225, 199, 93, maxColorValue = 255);
	spiral.morning_fiv <- rgb(234, 255, 128, maxColorValue = 255);
	spiral.morning_six <- rgb(204, 247, 210, maxColorValue = 255);
	spiral.morning <- c(spiral.morning_one, spiral.morning_two, spiral.morning_thr, spiral.morning_fou, spiral.morning_fiv, spiral.morning_six);

	spiral.dusk_one <- rgb(60, 78, 176, maxColorValue = 255);
	spiral.dusk_two <- rgb(130, 94, 188, maxColorValue = 255);
	spiral.dusk_thr <- rgb(198, 129, 216, maxColorValue = 255);
	spiral.dusk_fou <- rgb(248, 180, 227, maxColorValue = 255);
	spiral.dusk_fiv <- rgb(255, 229, 226, maxColorValue = 255);
	spiral.dusk <- c(spiral.dusk_one, spiral.dusk_two, spiral.dusk_thr, spiral.dusk_fou, spiral.dusk_fiv);

	spiral.afternoon_one <- rgb(132, 58, 28, maxColorValue = 255);
	spiral.afternoon_two <- rgb(164, 141, 35, maxColorValue = 255);
	spiral.afternoon_thr <- rgb(91, 203, 142, maxColorValue = 255);
	spiral.afternoon_fou <- rgb(137, 195, 208, maxColorValue = 255);
	spiral.afternoon_fiv <- rgb(214, 222, 255, maxColorValue = 255);
	spiral.afternoon <- c(spiral.afternoon_one, spiral.afternoon_two, spiral.afternoon_thr, spiral.afternoon_fou, spiral.afternoon_fiv);

	spiral.dawn_one <- rgb(143, 56, 185, maxColorValue = 255); 
	spiral.dawn_two <- rgb(215, 99, 195, maxColorValue = 255);
	spiral.dawn_thr <- rgb(224, 134, 150, maxColorValue = 255);
	spiral.dawn_fou <- rgb(232, 190, 162, maxColorValue = 255);
	spiral.dawn_fiv <- rgb(241, 235, 148, maxColorValue = 255);
	spiral.dawn <- c(spiral.dawn_one, spiral.dawn_two, spiral.dawn_thr, spiral.dawn_fou, spiral.dawn_fiv);

	# additional sequential colour schemes for cases when multiple sequential schemes are requested
	seq.yellowgreen <- c("lightyellow","darkolivegreen1","lawngreen","chartreuse3","green4","darkgreen");
	seq.green <- c("mintcream","darkseagreen1","lightgreen","springgreen3","springgreen4","darkgreen");
	seq.greenblue <- c("lightcyan","paleturquoise","turquoise1","darkturquoise","darkcyan","darkslategray");
	seq.blue <- c("aliceblue","lightblue1","lightskyblue","deepskyblue","dodgerblue3","dodgerblue4");
	seq.bluepurple <- c("aliceblue","lightsteelblue1","cornflowerblue","mediumslateblue","blueviolet","slateblue4");
	seq.purple <- c("thistle1","plum1","orchid1","orchid3","orchid4","mediumpurple4");
	seq.purplered <- c("lavenderblush","pink","palevioletred1","violetred1","maroon","violetred4");
	seq.redorange <- c("peachpuff","lightsalmon","coral","orangered","orangered3","orangered4");
	seq.orange <- c("papayawhip","navajowhite","darkgoldenrod1","darkorange","darkorange3","darkorange4");

	# order in which sequential/binary colour schemes will be provided
	scheme.order <- list(seq.yellowgreen, seq.purplered, seq.blue, seq.orange, seq.greenblue, seq.redorange, seq.bluepurple, seq.purple, seq.green);

	# Default Venn diagram colour schemes
	venn.cols <- c("red","dodgerblue","yellow");
	venn.text <- c("darkred", "darkblue", "darkorange");
	venn4.cols <- c("dodgerblue", "springgreen", "mediumpurple", "palevioletred");
	venn4.text <- c("darkblue", "darkgreen", "darkorchid4", "darkred");

	# Check if input combinations are valid
	if (!is.null(palette.type) && is.venn == TRUE) {
		warning("Do not specify a palette type if using a venn diagram. Setting palette type to NULL.");
		palette.type = NULL;
		}

	else if (!is.null(palette.type) && is.venn == FALSE) {
		for (i in length(palette.type)){
			if (!(palette.type[i] %in% possible.palette.types)) {
				stop("Invalid palette type is specified");
				}
			}
		}

	else if (length(number.of.colours) > 1 && is.venn == TRUE){
		warning("Multiple venn diagram colour schemes is not supported. Setting number.of.colours to first number specified.");
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
			stop("There is no venn diagram colour scheme consisting of more than 4 colours available");
			}
		}
	
	# check if number of colour schemes requested equals number of palette types specified (else return error)
	if(length(number.of.colours) == length(palette.type)){
		
		# Create palette
		if (length(number.of.colours) > 1){
			palette <- vector(mode = "list", length = length(number.of.colours));
			}

		seq.bin.counter <- 1;
		div.counter <- 1;
		qual.counter <- 1;
		old.seq.counter <- 1;
		old.div.counter <- 1;
		old.qual.counter <- 1;
		chrom.counter <- 1;
		surv.counter <- 1;
		dotmap.counter <- 1;
		spiral.sunrise.counter <- 1;
		pastel.counter <- 1;
		spiral.morning.counter <- 1;
		spiral.dusk.counter <- 1;
		spiral.afternoon.counter <- 1;
		spiral.dawn.counter <- 1;

		for(i in 1:length(number.of.colours)){

			if (!is.null(palette.type[i]) && (palette.type[i] == 'seq' || palette.type[i] == 'binary')) {
				if (seq.bin.counter > 9){
					stop("Only nine sequential or binary colour schemes are available.");
					}

				if (as.numeric(number.of.colours[i]) > 6) {
					stop("There is no sequential colour scheme consisting of more than 6 colours available.");
					}

				if (palette.type[i] == 'binary' && as.numeric(number.of.colours[i]) > 2){
					warning("Binary colour schemes only return white and one other colour.");
					}
				
				# default sequential scheme
				if (length(number.of.colours) == 1){
					if (number.of.colours[i] > 5) {
						stop("The default sequential colour scheme consists of only 5 colours. See the colour.gradient function to generate sequential palettes of more colours.");
						}					
					if (palette.type[i] == 'seq'){
						palette <- sequential[1:number.of.colours[i]];
						}
					else{
						palette[[i]] <- c("white","black");
						}	
					}
				else{
					if (palette.type[i] == 'seq'){
						subset <- c(1:number.of.colours[i]);
						palette[[i]] <- as.vector(sapply(scheme.order[seq.bin.counter], "[", subset));
						}
					else{
						binary.colour <- as.vector(sapply(scheme.order[seq.bin.counter], "[", c(4)));
						palette[[i]] <- c("white", binary.colour);
						}	
					}

				seq.bin.counter <- seq.bin.counter + 1;
				}
	
			else if (!is.null(palette.type[i]) && palette.type[i] == 'div') {
				if (div.counter > 1){
					warning("Only one diverging colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 8) {
					stop("There is no diverging colour scheme consisting of more than 8 colours available");
					}
				else if (as.numeric(number.of.colours[i]) > 4 && is.greyscale == TRUE) {
					warning ("The diverging colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				div.counter <- div.counter + 1;
				if (length(number.of.colours) == 1){
					palette <- diverging[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- diverging[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'qual') {
				if (qual.counter > 1){
					warning("Only one qualitative colour scheme is available. Colour scheme provided will be repeated");
					}

				if (as.numeric(number.of.colours[i]) > 12) {
					stop("There is no qualitative colour scheme consisting of more than 12 colours available.");
					}
				else if (as.numeric(number.of.colours[i]) > 5 && is.greyscale == TRUE) {
					warning("The qualitative colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				qual.counter <- qual.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- qualitative[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- qualitative[1:number.of.colours[i]];
					}
				}
			
			# this old colour scheme is provided for backwards-compatibility
			else if (!is.null(palette.type[i]) && palette.type[i] == 'old.seq') {
				if (old.seq.counter > 1){
					warning("Only one legacy sequential colour scheme is available. Colour scheme provided will be repeated");
					}

				if (as.numeric(number.of.colours[i]) > 5) {
					stop("The old legacy sequential colour scheme consists of a maximum of 5 colours.");
					}
				old.seq.counter <- old.seq.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- old.sequential[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- old.sequential[1:number.of.colours[i]];
					}
				}

			# this old colour scheme is provided for backwards-compatibility
			else if (!is.null(palette.type[i]) && palette.type[i] == 'old.div') {
				if (old.div.counter > 1){
					warning("Only one legacy diverging colour scheme is available. Colour scheme provided will be repeated");
					}

				if (as.numeric(number.of.colours[i]) > 8) {
					stop("The old diverging colour scheme consists of a maximum of 8 colours.");
					}
				else if (as.numeric(number.of.colours[i]) > 4 && is.greyscale == TRUE) {
					warning("The diverging colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				old.div.counter <- old.div.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- old.diverging[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- old.diverging[1:number.of.colours[i]];
					}
				}

			# this old colour scheme is provided for backwards-compatibility
			else if (!is.null(palette.type[i]) && palette.type[i] == 'old.qual') {
				if (old.qual.counter > 1){
					warning("Only one qualitative colour scheme is available. Colour scheme provided will be repeated");
					}

				if (as.numeric(number.of.colours[i]) > 9) {
					stop("The old qualitative colour scheme consists of a maximum of 9 colours.");
					}
				else if (as.numeric(number.of.colours[i]) > 5 && is.greyscale == TRUE) {
					warning("The qualitative colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				old.qual.counter <- old.qual.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- old.qualitative[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- old.qualitative[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'chromosomes') {
				if (chrom.counter > 1){
					warning("Only one chromosome colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 24) {
					stop("There are only 24 colours in the chromosome palette.");
					}
				else if (as.numeric(number.of.colours[i]) > 5 && is.greyscale == TRUE) {
					warning("The chromosome colour scheme provided is not greyscale compatible.");
					}
				chrom.counter <- chrom.counter + 1;
				if (length(number.of.colours) == 1){
					palette <- chromosomes[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- chromosomes[1:number.of.colours[i]];
					}
				
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'survival') {
				if (surv.counter > 1){
					warning("Only one survival colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 10) {
					stop("There are only 10 colours in the survival palette.");
					}
				else if (as.numeric(number.of.colours[i]) > 5 && is.greyscale == TRUE) {
					warning("The survival colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				surv.counter <- surv.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- survival[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- survival[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'dotmap') {
				if (dotmap.counter > 1){
					warning("Only one dotmap colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) != 2) {
					stop("There are only 2 colours in the dotmap palette.");
					}
				dotmap.counter <- dotmap.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- dotmap;
					}
				else{
					palette[[i]] <- dotmap;
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'spiral.sunrise') {
				if (spiral.sunrise.counter > 1){
					warning("Only one spiral.sunrise colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 5) {
					stop("There are only 5 colours in the spiral.sunrise palette.");
					}
				spiral.sunrise.counter <- spiral.sunrise.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- spiral.sunrise[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- spiral.sunrise[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'pastel') {
				if (pastel.counter > 1){
					warning("Only one pastel colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 12) {
					stop("There are only 12 colours in the dotmap palette.");
					}
				else if (as.numeric(number.of.colours[i]) > 5 && is.greyscale == TRUE) {
					warning("The pastel colour scheme provided is not greyscale compatible. Consider changing another feature (such as line style or plotting character) to ensure data points can be distinguished.");
					}
				pastel.counter <- pastel.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- pastel[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- pastel[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'spiral.morning') {
				if (spiral.morning.counter > 1){
					warning("Only one spiral.morning colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 6) {
					stop("There are only 6 colours in the spiral.morning palette.");
					}
				spiral.morning.counter <- spiral.morning.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- spiral.morning[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- spiral.morning[1:number.of.colours[i]];
					}
				}
				
			else if (!is.null(palette.type[i]) && palette.type[i] == 'spiral.dusk') {
				if (spiral.dusk.counter > 1){
					warning("Only one spiral.dusk colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 5) {
					stop("There are only 5 colours in the spiral.dusk palette.");
					}
				spiral.dusk.counter <- spiral.dusk.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- spiral.dusk[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- spiral.dusk[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'spiral.afternoon') {
				if (spiral.afternoon.counter > 1){
					warning("Only one spiral.afternoon colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 5) {
					stop("There are only 5 colours in the spiral.afternoon palette.");
					}
				spiral.afternoon.counter <- spiral.afternoon.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- spiral.afternoon[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- spiral.afternoon[1:number.of.colours[i]];
					}
				}

			else if (!is.null(palette.type[i]) && palette.type[i] == 'spiral.dawn') {
				if (spiral.afternoon.counter > 1){
					warning("Only one spiral.dawn colour scheme is available. Colour scheme provided will be repeated");
					}
				if (as.numeric(number.of.colours[i]) > 5) {
					stop("There are only 5 colours in the spiral.dawn palette.");
					}
				spiral.dawn.counter <- spiral.dawn.counter + 1;

				if (length(number.of.colours) == 1){
					palette <- spiral.dawn[1:number.of.colours[i]];
					}
				else{
					palette[[i]] <- spiral.dawn[1:number.of.colours[i]];
					}
				}

			}
		}

	# check for multiple colour schemes requested, but improper palette types requested
	else if (is.venn == FALSE){
		stop("The number of colours schemes requested must equal the number of palette types specified");
		}

	# Output palette
	if (is.venn) {
		return(list(palette, palette.text));
		}
	else{
		return(palette);
		}
	}
