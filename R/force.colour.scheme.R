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

### FUNCTION TO SET COLOUR SCHEMES ################################################################
force.colour.scheme <- force.color.scheme <- function(
	x = NA, scheme, fill.colour = 'slategrey', include.names = FALSE, return.factor = FALSE, return.scheme = FALSE
	) {

	# some error checking
	if (as.character(class(x) == 'factor')) {
		stop("Argument 'x' cannot be a factor: please coerce to character before passing.");
		}

	if (scheme == 'psa.categorical') {
		x.processed <- x;
        	if (length(grep(x = x, '-|>')) == 0) {
        		x <- as.numeric(x);
        		x.processed <- rep('0 - 9.9', length(x));
        		x.processed[x >= 10 & x < 20] <- '10 - 19.9';
        		if (any(x >= 20, na.rm = TRUE)) {
                		x.processed[x >= 20] <- '>= 20';
                		}
        		if (any(is.na(x))) {
                		x.processed[is.na(x)] <- NA;
                		}
			}
		x <- x.processed;
		}

	else if (scheme == 'age.categorical.default') {
		x.processed <- x;
		if (length(grep(x = x, '-|>|<')) == 0) {
                        x <- as.numeric(x);
                        x.processed <- rep('<50', length(x));
                        x.processed[x >= 50 & x < 60] <- '50 - 60';
                        x.processed[x >= 60 & x < 70] <- '60 - 70';
                        if (any(x >= 70, na.rm = TRUE)) {
                                x.processed[x >= 70] <- '>= 70';
                                }
                        if (any(is.na(x))) {
                                x.processed[is.na(x)] <- NA;
                                }
                        }
		x <- x.processed;
		}

	else if (scheme == 'age.categorical.prostate') {
		x.processed <- x;
		if (length(grep(x = x, '-|>|<')) == 0) {
                        x <- as.numeric(x);
                        x.processed <- rep('<40', length(x));
                        x.processed[x >= 40 & x < 50] <- '40 - 50';
                        x.processed[x >= 50 & x < 65] <- '50 - 65';
                        x.processed[x >= 65 & x < 70] <- '65 - 70';
                        if (any(x >= 70, na.rm = TRUE)) {
                                x.processed[x >= 70] <- '>= 70';
                                }
                        if (any(is.na(x))) {
                                x.processed[is.na(x)] <- NA;
                                }
                        }
		x <- x.processed;
		}

	else if (scheme == 'age.gradient') {
		x[x < 40] <- 40;
        	x[x > 70] <- 70;

        	colour.x <- x - 40;
        	colour.x <- colour.x / 30.0;

        	colour.scheme <- c('white', 'black');
        	colour.function <- colorRamp(colour.scheme, space = 'rgb');
        	my.palette <- rgb(colour.function(colour.x), maxColorValue = 255);
		return(my.palette);
		}

	else if (scheme == 'psa.gradient') {
		x[x < 0] <- 0;
        	x[x > 20] <- 20;

        	x <- x / 20.0;

        	colour.scheme <- c('white', 'darkred');
        	colour.function <- colorRamp(colour.scheme, space = 'rgb');

        	my.palette <- rgb(colour.function(x), maxColorValue = 255);

        	return(my.palette);
		}

	else if (scheme == 'heteroplasmy') {
                x.processed <- x;
                x <- as.numeric(x);
                x.processed <- rep('NA', length(x));
		x.processed[x >= 0 & x < 0.2] <- '0';
		x.processed[x >= 0.2 & x < 0.4] <- '1';
                x.processed[x >= 0.4 & x < 0.6] <- '2';
                x.processed[x >= 0.6 & x <= 1] <- '3';
                x <- x.processed;
                }

	else if (scheme == 'mt.annotation') {
                x.processed <- x;
                x.processed[grepl('MT-T', x.processed)] <- 'MT-T';
                x.processed[grepl('MT-RNR', x.processed)] <- 'MT-RNR';
                x.processed[grepl('MT-NC', x.processed)] <- 'MT-NC';
                x.processed[grepl('MT-OL', x.processed)] <- 'MT-OL';
		x <- x.processed;
                }

	# Set all input to lower case
	x <- tolower(x);
	scheme <- tolower(scheme);

	# loading tissue colours
	# skeletal
	cartilage	<- rgb(90 / 255, 90 / 255, 90 / 255);
	bone		<- rgb(150 / 255, 150 / 255, 150 / 255);

	# adipose
	adipose		<- rgb(156 / 255, 65 / 255, 13 / 255);

	# excretory
	bladder		<- rgb(98 / 255, 47 / 255, 9 / 255);
	kidney		<- rgb(213 / 255, 119 / 255, 80 / 255);

	# circulatory
	blood		<- rgb(173 / 255, 12 / 255, 9 / 255);
	heart		<- rgb(248 / 255, 15 / 255, 15 / 255);

	# muscular
	muscle		<- rgb(243 / 255, 173 / 255, 178 / 255);

	# endocrine
	hypothalamus	<- rgb(135 / 255, 98 / 255, 44 / 255);
	pituitary	<- rgb(183 / 255, 148 / 255, 75 / 255);
	thyroid		<- rgb(219 / 255, 195 / 255, 132 / 255);
	parathyroid	<- rgb(245 / 255, 233 / 255, 179 / 255);
	skin		<- rgb(255 / 255, 173 / 255, 77 / 255);

	# gastrointestinal
	salivarygland	<- rgb(68 / 255, 88 / 255, 25 / 255);
	esophagus	<- rgb(116 / 255, 140 / 255, 52 / 255);
	stomach		<- rgb(150 / 255, 196 / 255, 82 / 255);
	liver		<- rgb(197 / 255, 230 / 255, 149 / 255);
	gallbladder	<- rgb(4 / 255, 92 / 255, 31 / 255);
	pancreas	<- rgb(44 / 255, 145 / 255, 80 / 255);
	intestine	<- rgb(77 / 255, 194 / 255, 104 / 255);
	colon		<- rgb(133 / 255, 231 / 255, 133 / 255);

	# respiratory
	pharynx		<- rgb(27 / 255, 93 / 255, 99 / 255);
	larynx		<- rgb(36 / 255, 131 / 255, 139 / 255);
	trachea		<- rgb(48 / 255, 175 / 255, 186 / 255);
	diaphragm	<- rgb(91 / 255, 210 / 255, 220 / 255);
	lung		<- rgb(173 / 255, 237 / 255, 243 / 255);

	# nervous
	nerve		<- rgb(32 / 255, 90 / 255, 205 / 255);
	spine		<- rgb(58 / 255, 121 / 255, 245 / 255);
	brain		<- rgb(111 / 255, 156 / 255, 244 / 255);
	eye		<- rgb(154 / 255, 183 / 255, 241 / 255);

	# reproductive
	breast		<- rgb(104 / 255, 15 / 255, 78 / 255);
	ovary		<- rgb(224 / 255, 131 / 255, 197 / 255);
	uterus		<- rgb(246 / 255, 189 / 255, 229 / 255);
	prostate	<- rgb(152 / 255, 41 / 255, 119 / 255);
	testes		<- rgb(204 / 255, 66 / 255, 160 / 255);

	# immune
	lymph		<- rgb(82 / 255, 49 / 255, 166 / 255);
	leukocyte	<- rgb(144 / 255, 114 / 255, 219 / 255);
	spleen		<- rgb(195 / 255, 181 / 255, 225 / 255);

	# load tumour stage colours
	i		<- rgb(255 / 255, 211 / 255, 153 / 255);
	ii		<- rgb(255 / 255, 174 / 255, 69 / 255);
	iii		<- rgb(184 / 255, 114 / 255, 23 / 255);
	iv		<- rgb(119 / 255, 70 / 255, 7 / 255);

	# load risk colours
	high		<- rgb(155 / 255, 19 / 255, 19 / 255);
	low		<- rgb(234 / 255, 196 / 255, 120 / 255);
	old.high	<- rgb(252 / 255, 87 / 255, 75 / 255);
	old.low		<- rgb(112 / 255, 221 / 255, 240 / 255);

	# load MSI colours
	old.msi.high	<- rgb(147 / 255, 183 / 255, 235 / 255);
	old.msi.low	<- rgb(50 / 255, 119 / 255, 216 / 255);
	old.mss		<- rgb(42 / 255, 82 / 255, 138 / 255);

	# new scheme is based on old CNV colours (green-yellow)
	msi.high	<- rgb(84 / 255, 161 / 255, 76 / 255);
	msi.low		<- rgb(186 / 255, 219 / 255, 96 / 255);
	mss		<- rgb(243  / 255, 239 / 255, 82 / 255);

	# load tumour sample type colours
	primary		<- rgb(181 / 255, 127 / 255, 212 / 255);
	metastatic	<- rgb(114 / 255, 23 / 255, 165 / 255);

	# load CNV colours
	amplification	<- rgb(252 / 255, 87 / 255, 75 / 255);
	deletion	<- rgb(112 / 255, 221 / 255, 240 / 255);
	loh		<- rgb(53 / 255, 158 / 255, 87 / 255);
	neutral		<- 'white';

	# organism
	human		<- rgb(158, 82, 53, maxColorValue = 255);
	rat		<- rgb(191, 136, 116, maxColorValue = 255);
	mouse		<- rgb(232, 212, 203, maxColorValue = 255);

	# chromosome
	chr.1	<- rgb(222, 71, 171, maxColorValue = 255);
	chr.2	<- rgb(114, 190, 151, maxColorValue = 255);
	chr.3	<- rgb(247, 247, 151, maxColorValue = 255);
	chr.4	<- rgb(124, 116, 155, maxColorValue = 255);
	chr.5	<- rgb(232, 87, 38, maxColorValue = 255);
	chr.6	<- rgb(179, 149, 248, maxColorValue = 255);
	chr.7	<- rgb(220, 135, 71, maxColorValue = 255);
	chr.8	<- rgb(150, 213, 61, maxColorValue = 255);
	chr.9	<- rgb(220, 133, 238, maxColorValue = 255);
	chr.10	<- rgb(125, 50, 179, maxColorValue = 255);
	chr.11	<- rgb(136, 219, 104, maxColorValue = 255);
	chr.12	<- rgb(120, 170, 241, maxColorValue = 255);
	chr.13	<- rgb(217, 198, 202, maxColorValue = 255);
	chr.14	<- rgb(51, 108, 128, maxColorValue = 255);
	chr.15	<- rgb(247, 202, 68, maxColorValue = 255);
	chr.16	<- rgb(50, 199, 199, maxColorValue = 255);
	chr.17	<- rgb(212, 197, 242, maxColorValue = 255);
	chr.18	<- rgb(153, 84, 147, maxColorValue = 255);
	chr.19	<- rgb(248, 139, 120, maxColorValue = 255);
	chr.20	<- rgb(71, 94, 204, maxColorValue = 255);
	chr.21	<- rgb(224, 189, 140, maxColorValue = 255);
	chr.22	<- rgb(158, 40, 0, maxColorValue = 255);
	chr.x	<- rgb(242, 187, 210, maxColorValue = 255);
	chr.y	<- rgb(182, 235, 234, maxColorValue = 255);

	# biomolecule
	dna		<- rgb(205, 238, 240, maxColorValue = 255);
	rna		<- rgb(169, 192, 236, maxColorValue = 255);
	protein		<- rgb(178, 150, 223, maxColorValue = 255);
	carbohydrate	<- rgb(202, 113, 145, maxColorValue = 255);
	lipid		<- rgb(231, 221, 54, maxColorValue = 255);

	# snv
	nonsynonymous		<- rgb(177 / 255, 213 / 255, 181 / 255);
	stopgain		<- rgb(249 / 255, 179 / 255, 142 / 255);
	frameshiftdeletion	<- rgb(154 / 255, 163 / 255, 242 / 255);

	#clincal T3
	clinical.t3.1 <- rgb(255, 247, 223, maxColorValue = 255);
	clinical.t3.2 <- rgb(221, 246, 139, maxColorValue = 255);
	clinical.t3.3 <- rgb(109, 196, 110, maxColorValue = 255);
	clinical.t3.4 <- rgb(47, 109, 96, maxColorValue = 255);
	clinical.t3.5 <- rgb(25, 55, 81, maxColorValue = 255);
	clinical.t3.6 <- rgb(8, 0, 16, maxColorValue = 255);

	#clinical T9
	clinical.t9.1 <- rgb(143, 161, 82, maxColorValue = 255);
	clinical.t9.2 <- rgb(178, 200, 105, maxColorValue = 255);
	clinical.t9.3 <- rgb(221, 246, 139, maxColorValue = 255);
	clinical.t9.4 <- rgb(56, 145, 58, maxColorValue = 255);
	clinical.t9.5 <- rgb(109, 196, 110, maxColorValue = 255);
	clinical.t9.6 <- rgb(179, 238, 180, maxColorValue = 255);
	clinical.t9.7 <- rgb(47, 109, 96, maxColorValue = 255);
	clinical.t9.8 <- rgb(94, 194, 170, maxColorValue = 255);
	clinical.t9.9 <- rgb(192, 231, 222, maxColorValue = 255);

	#heteroplasmy
	heteroplasmy.0 <- 'white';
	heteroplasmy.1 <- 'lightskyblue';
	heteroplasmy.2 <- 'dodgerblue';
	heteroplasmy.3 <- 'mediumblue';

	#MT annotation
	mt.annotation.1 <- '#B041FF';
	mt.annotation.2 <- '#5EFB6E';
	mt.annotation.3 <- '#005000';
	mt.annotation.4 <- '#2B65EC';
	mt.annotation.5 <- '#FF6E32';
	mt.annotation.6 <- '#A00000';
	mt.annotation.7 <- '#FFFF78';
	mt.annotation.8 <- 'white';

	#ISUP Grade
	isup.grade.1 <- 'cornsilk';
        isup.grade.2 <- 'yellow';
        isup.grade.3 <- 'orange';
        isup.grade.4 <- 'maroon3';
        isup.grade.5 <- 'red';

	# irregular spacing is used here to allow for visual mapping between colours and corresponding values
	avail.schemes <- list(
		tissue = list(
			levels = c('cartilage', 'bone', 'adipose', 'bladder', 'kidney', 'blood', 'heart', 'muscle',
				'hypothalamus', 'pituitary', 'thyroid', 'parathyroid', 'skin', 'salivary gland', 'esophagus',
				'stomach', 'liver', 'gall bladder', 'pancreas', 'intestine', 'colon', 'pharynx', 'larynx', 'trachea',
				'diaphragm', 'lung', 'nerve', 'spine', 'brain', 'eye', 'breast', 'ovary', 'uterus', 'prostate', 'testes',
				'lymph', 'leukocyte', 'spleen'),
			colours = c(cartilage, 	 bone, adipose, bladder, kidney, blood, heart, muscle, hypothalamus, pituitary, thyroid,
				parathyroid, skin, salivarygland, esophagus, stomach, liver, gallbladder, pancreas, intestine, colon,
				pharynx, larynx, trachea, diaphragm, lung, nerve, spine, brain, eye, breast, ovary, uterus, prostate, testes,
				lymph, leukocyte, spleen)
			),
		chromosome = list(
			levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 'x', 'y'),
			colours = c(chr.1, chr.2, chr.3, chr.4, chr.5, chr.6, chr.7, chr.8, chr.9, chr.10, chr.11, chr.12, chr.13, chr.14,
				chr.15, chr.16, chr.17, chr.18, chr.19, chr.20, chr.21, chr.22, chr.x, chr.y)
			),
		old.chromosome = list(
			levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 'x', 'y'),
			colours = c('darkred', 'firebrick1', 'pink1', 'darkorange3', 'darkorange', 'tan1', 'goldenrod3', 'gold', 'khaki',
				'darkgreen', 'forestgreen', 'greenyellow', 'darkblue', 'dodgerblue', 'skyblue', 'darkslateblue', 'slateblue3',
				'mediumpurple1', 'darkorchid4', 'orchid3', 'plum', 'violetred', 'grey31', 'grey0')
			),
		annovar.annotation.collapsed2 = list(
			levels  = c('nonsynonymous', 'stopgain-stoploss', 'splicing', 'frameshift indel', 'synonymous', 'utr5-utr3',
				'nonframeshift indel', 'intronic', 'intergenic', 'other'),
			colours = c('darkseagreen4', 'orchid4', 'darkturquoise', 'darkorange', 'gold1', 'grey30', 'grey50', 'grey70', 'grey90', 'white')
			),
		annovar.annotation = list(
			levels  = c('nonsynonymous snv', 'stopgain snv', 'stoploss snv', 'frameshift deletion', 'frameshift substitution', 'splicing', 'synonymous snv'),
			colours = c('darkseagreen4', 'orchid4', 'darkturquoise', 'darkorange', 'darkorange4', 'gold1', 'darkgrey')
			),
		annovar.annotation.collapsed = list(
			levels  = c('nonsynonymous snv', 'stopgain snv', 'stoploss snv', 'frameshift indel', 'splicing'),
			colours = c('darkseagreen4', 'orchid4', 'darkturquoise', 'darkorange', 'gold1')
			),
		snv = list(
			levels = c('nonsynonymous', 'stop gain', 'frameshift deletion', 'nonframeshift deletion', 'splicing', 'unknown'),
			colours = c(nonsynonymous, stopgain, frameshiftdeletion, 'gold', 'skyblue', 'plum')
			),
		biomolecule = list(
			levels = c('dna', 'rna', 'protein', 'carbohydrate', 'lipid'),
			colours = c(dna, rna, protein, carbohydrate, lipid)
			),
		sex = list(
			levels = c('male', 'female'),
			colours = c('lightskyblue', 'lightpink')
			),
		stage = list(
			levels = c('i', 'ii', 'iii', 'iv'),
			colours = c(i, ii, iii, iv)
			),
		risk = list(
			levels = c('high', 'low'),
			colours = c(high, low)
			),
		old.risk = list(
			levels = c('high', 'low'),
			colours = c(old.high, old.low)
			),
		msi = list(
			levels = c('msi-high', 'msi-low', 'mss'),
			colours = c(msi.high, msi.low, mss)
			),
		old.msi = list(
			levels = c('msi-high', 'msi-low', 'mss'),
			colours = c(old.msi.high, old.msi.low, old.mss)
			),
		tumour = list(
			levels = c('primary', 'metastatic'),
			colours = c(primary, metastatic)
			),
		cnv = list(
			levels = c('amplification', 'deletion', 'loh', 'neutral'),
			colours = c(amplification, deletion, loh, neutral)
			),
		old.cnv = list(
			levels = c('amplification', 'deletion', 'loh', 'neutral'),
			colours = c(msi.high, msi.low, mss, neutral)
			),
		organism = list(
			levels = c('human', 'rat', 'mouse'),
			colours = c(human, rat, mouse)
			),
		clinicalt3 = list(
			levels = c('t0', 't1', 't2', 't3', 't4', 't5'),
			colours = c(clinical.t3.1, clinical.t3.2, clinical.t3.3, clinical.t3.4, clinical.t3.5, clinical.t3.6)
			),
		clinicalt9 = list(
                        levels = c('t1a', 't1b', 't1c', 't2a', 't2b', 't2c', 't3a', 't3b', 't3c'),
                        colours = c(clinical.t9.1, clinical.t9.2, clinical.t9.3, clinical.t9.4, clinical.t9.5, clinical.t9.6,
				clinical.t9.7, clinical.t9.8, clinical.t9.9)
                        ),
		gleason.score = list(
			levels = c('3+3', '3+4', '4+3', '4+4', '4+5', '3+5', '5+3', '5+4', '5+5', 'missing', 'NA'),
			colours = c('white', 'yellow', 'orange', 'red', 'brown', 'maroon3', 'magenta4', 'mediumblue', 'black', 'slategrey', 'slategrey')
			),
		gleason.sum = list(
			levels = c(5, 6, 7, 8, 9, 10, 'missing', 'NA'),
			colours = c('#FEEBE2', '#FBB4B9', '#F768A1', '#C51B8A', '#7A0177', '#4d004b','slategrey', 'slategrey')
			),
		tissue.color = list(
			levels = c('blood', 'frozen', 'ffpe'),
			colours = c('orangered4', 'peachpuff2', 'rosybrown')
			),
		psa.categorical = list(
			levels = c('0 - 9.9', '10 - 19.9', '>= 20'),
			colours = c('#FEE6CE', '#FDAE6B', '#E6550D')
			),
		age.categorical.default = list(
			levels = c('<50', '50 - 60', '60 - 70', '>= 70'),
			colours = c('gray100', 'gray66', 'gray33', 'gray0')
			),
		age.categorical.prostate = list(
			levels = c('<40', '40 - 50', '50 - 65', '65 - 70', '>= 70'),
			colours = c('gray100', 'gray75', 'gray50', 'gray25', 'gray0')
			),
		heteroplasmy = list(
			levels = c('0', '1', '2', '3'),
			colours = c(heteroplasmy.0, heteroplasmy.1, heteroplasmy.2, heteroplasmy.3)
			),
		mt.annotation = list(
			levels = c('mt-dloop', 'mt-t', 'mt-rnr',  'mt-nd1', 'mt-nd2', 'mt-nd3', 'mt-nd4l',  'mt-nd4l/mt-nd4', 'mt-nd4',
				'mt-nd5', 'mt-nd6', 'mt-co1', 'mt-co2', 'mt-co3', 'mt-atp6/mt-co3', 'mt-atp6',  'mt-atp8/mt-atp6',
				'mt-atp8', 'mt-cyb', 'mt-nc', 'mt-ol'),
			colours = c(mt.annotation.1, mt.annotation.2, mt.annotation.3, mt.annotation.4, mt.annotation.4, mt.annotation.4,
				mt.annotation.4, mt.annotation.4, mt.annotation.4, mt.annotation.4, mt.annotation.4, mt.annotation.6,
				mt.annotation.6, mt.annotation.6, mt.annotation.6, mt.annotation.7, mt.annotation.7, mt.annotation.7,
				mt.annotation.5, mt.annotation.8, mt.annotation.8)
			),
                isup.grade = list(
			levels = c('1', '2', '3', '4', '5'),
			colours = c(isup.grade.1, isup.grade.2, isup.grade.3, isup.grade.4, isup.grade.5)
			)
		);

	if (is.null(avail.schemes[[scheme]]) && scheme != 'all') {
		stop('Scheme not found!');
		}

	# format return values
	to.return <- list();

	# if return.scheme is requested
	if (return.scheme) {
		if ('all' == scheme) {
			to.return[['scheme']] <- avail.schemes;
			}
		else {
			to.return[['scheme']] <- avail.schemes[[scheme]];
			}
		}

	# check to see if more than 1 type unknown. 1 is expected for 'none' or similar, but 2+ likely to be error.
	if (1 < sum(!unique(x) %in% avail.schemes[[scheme]]$levels)) {
		warning('More than one unique entry of x is not found in scheme. Perhaps an error?');
		}

	x.as.factor <- factor(x, levels = avail.schemes[[scheme]]$levels);

	# if return.factor is requested
	if (return.factor) {
		to.return[['return.factor']] <- x.as.factor;
		}

	x.colours <- avail.schemes[[scheme]]$colours[x.as.factor];
	x.colours[is.na(x.colours)] <- fill.colour;

	# return the converted list
	if (include.names) {
		names(x.colours) <- x;
		}
	to.return[['colours']] <- x.colours;

	if (1 == length(to.return)) {
		return(to.return[[1]]);
		}
	else {
		return(to.return);
		}
	}
