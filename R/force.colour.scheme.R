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
force.colour.scheme <- function(x, scheme, fill.colour = NA, include.names = FALSE, return.factor = FALSE, return.scheme = FALSE){
	
	if(class(x) == 'factor') { 
		stop('x cannot be a factor: please coerce to character before passing');
		}

	# Set all input to lower case
	x <- tolower(x);
	scheme <- tolower(scheme);

	# loading tissue colours
	# skeletal
	cartilage = rgb(90/255, 90/255, 90/255);
	bone = rgb(150/255, 150/255, 150/255);

	# adipose
	adipose = rgb(156/255, 65/255, 13/255);

	# excretory
	bladder = rgb(98/255, 47/255, 9/255);
	kidney = rgb(213/255, 119/255, 80/255);

	# circulatory 
	blood = rgb(173/255, 12/255, 9/255);
	heart = rgb(248/255, 15/255, 15/255);
	
	# muscular
	muscle = rgb(243/255, 173/255, 178/255);
	
	# endocrine
	hypothalamus = rgb(135/255, 98/255, 44/255);
	pituitary = rgb(183/255, 148/255, 75/255);
	thyroid = rgb(219/255, 195/255, 132/255);
	parathyroid = rgb(245/255, 233/255, 179/255);
	skin = rgb(255/255, 173/255, 77/255);

	# gastrointestinal
	salivarygland = rgb(68/255, 88/255, 25/255);
	esophagus = rgb(116/255, 140/255, 52/255);
	stomach = rgb(150/255, 196/255, 82/255);
	liver = rgb(197/255, 230/255, 149/255);
	gallbladder = rgb(4/255, 92/255, 31/255);
	pancreas = rgb(44/255, 145/255, 80/255);
	intestine = rgb(77/255, 194/255, 104/255);
	colon = rgb(133/255, 231/255, 133/255);

	# respiratory
	pharynx = rgb(27/255, 93/255, 99/255);
	larynx = rgb(36/255, 131/255, 139/255);
	trachea = rgb(48/255, 175/255, 186/255);
	diaphragm = rgb(91/255, 210/255, 220/255);
	lung = rgb(173/255, 237/255, 243/255);
	
	# nervous
	nerve = rgb(32/255, 90/255, 205/255);
	spine = rgb(58/255, 121/255, 245/255);
	brain = rgb(111/255, 156/255, 244/255);
	eye = rgb(154/255, 183/255, 241/255);

	# reproductive 
	breast = rgb(104/255, 15/255, 78/255);
	ovary = rgb(224/255, 131/255, 197/255);
	uterus = rgb(246/255, 189/255, 229/255);
	prostate = rgb(152/255, 41/255, 119/255);
	testes = rgb(204/255, 66/255, 160/255);

	# immune
	lymph = rgb(82/255, 49/255, 166/255);
	leukocyte = rgb(144/255, 114/255, 219/255);
	spleen = rgb(195/255, 181/255, 225/255);
	
	# load tumour stage colours
	I = rgb(255/255, 211/255, 153/255);
	II = rgb(255/255, 174/255, 69/255);
	III = rgb(184/255, 114/255, 23/255); 
	IV = rgb(119/255, 70/255, 7/255);

	# load risk colours
	High = rgb(155/255, 19/255, 19/255);
	Low = rgb(234/255, 196/255, 120/255);
	old_High = rgb(252/255, 87/255, 75/255);
    old_Low = rgb(112/255, 221/255, 240/255);
	
	# load MSI colours
	old_MSI_High = rgb(147/255, 183/255, 235/255);
	old_MSI_Low = rgb(50/255, 119/255, 216/255);
	old_MSS = rgb(42/255, 82/255, 138/255);

	# new scheme is based on old CNV colours (green-yellow)
	MSI_High = rgb(84/255, 161/255, 76/255);
	MSI_Low = rgb(186/255, 219/255, 96/255);
	MSS = rgb(243/255, 239/255, 82/255);

	# load tumour sample type colours
	Primary = rgb(181/255, 127/255, 212/255);
	Metastatic = rgb(114/255, 23/255, 165/255);

	# load CNV colours
	Amplification = rgb(252/255, 87/255, 75/255);
	Deletion = rgb(112/255, 221/255, 240/255);
	LOH = rgb(53/255, 158/255, 87/255);
	Neutral = "white";
	
	# organism
	Human = rgb(158, 82, 53, maxColorValue = 255);
	Rat = rgb(191, 136, 116, maxColorValue = 255);
	Mouse = rgb(232, 212, 203, maxColorValue = 255);

	# chromosome
	chr.1 = rgb(222, 71, 171, maxColorValue = 255);
	chr.2 = rgb(114, 190, 151, maxColorValue = 255);
	chr.3 = rgb(247, 247, 151, maxColorValue = 255);
	chr.4 = rgb(124, 116, 155, maxColorValue = 255);
	chr.5 = rgb(232, 87, 38, maxColorValue = 255);
	chr.6 = rgb(179, 149, 248, maxColorValue = 255);
	chr.7 = rgb(220, 135, 71, maxColorValue = 255);
	chr.8 = rgb(150, 213, 61, maxColorValue = 255);
	chr.9 = rgb(220, 133, 238, maxColorValue = 255);
	chr.10 = rgb(125, 50, 179, maxColorValue = 255);
	chr.11 = rgb(136, 219, 104, maxColorValue = 255);
	chr.12 = rgb(120, 170, 241, maxColorValue = 255);
	chr.13 = rgb(217, 198, 202, maxColorValue = 255);
	chr.14 = rgb(51, 108, 128, maxColorValue = 255);
	chr.15 = rgb(247, 202, 68, maxColorValue = 255);
	chr.16 = rgb(50, 199, 199, maxColorValue = 255);
	chr.17 = rgb(212, 197, 242, maxColorValue = 255);
	chr.18 = rgb(153, 84, 147, maxColorValue = 255);
	chr.19 = rgb(248, 139, 120, maxColorValue = 255);
	chr.20 = rgb(71, 94, 204, maxColorValue = 255);
	chr.21 = rgb(224, 189, 140, maxColorValue = 255);
	chr.22 = rgb(158, 40, 0, maxColorValue = 255);
	chr.x = rgb(242, 187, 210, maxColorValue = 255);
	chr.y = rgb(182, 235, 234, maxColorValue = 255);

	# biomolecule
	DNA = rgb(205, 238, 240, maxColorValue = 255);
	RNA = rgb(169, 192, 236, maxColorValue = 255);
	Protein = rgb(178, 150, 223, maxColorValue = 255);
	Carbohydrate = rgb(202, 113, 145, maxColorValue = 255);
	Lipid = rgb(231, 221, 54, maxColorValue = 255);


	# irregular spacing is used here to allow for visual mapping between colours and corresponding values
	avail.schemes <- list(
		tissue = list(
			levels = c('cartilage', 'bone', 'adipose', 'bladder', 'kidney', 'blood', 'heart', 'muscle', 'hypothalamus', 'pituitary', 'thyroid', 'parathyroid', 'skin', 'salivarygland', 'esophagus', 'stomach', 'liver', 'gallbladder', 'pancreas', 'intestine', 'colon', 'pharynx', 'larynx', 'trachea', 'diaphragm', 'lung', 'nerve', 'spine', 'brain', 'eye', 'breast', 'ovary', 'uterus', 'prostate', 'testes', 'lymph', 'leukocyte', 'spleen'),
			colours = c(cartilage, 	 bone,   adipose,   bladder,   kidney,   blood,   heart,   muscle,   hypothalamus,   pituitary,   thyroid,   parathyroid,   skin,   salivarygland,   esophagus,   stomach,   liver,   gallbladder,   pancreas,   intestine,   colon,   pharynx,   larynx,   trachea,   diaphragm,   lung,   nerve,   spine,   brain,   eye,   breast,   ovary,   uterus,   prostate,   testes,   lymph,   leukocyte,   spleen)
			),
		chromosome = list(
			levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,'x','y'),
			colours = c(chr.1, chr.2, chr.3, chr.4, chr.5, chr.6, chr.7, chr.8, chr.9, chr.10, chr.11, chr.12, chr.13, chr.14, chr.15, chr.16, chr.17, chr.18, chr.19, chr.20, chr.21, chr.22, chr.x, chr.y)
			),
		old.chromosome = list(
			levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,'x','y'),
			colours = c("darkred", "firebrick1", "pink1","darkorange3","darkorange","tan1","goldenrod3","gold","khaki","darkgreen","forestgreen","greenyellow", "darkblue","dodgerblue","skyblue","darkslateblue","slateblue3","mediumpurple1","darkorchid4","orchid3","plum","violetred","grey31","grey0")
			),
		annovar.annotation.collapsed2 = list( 
			levels  = c('nonsynonymous', 'stopgain-stoploss', 'splicing',      'frameshift indel',     'synonymous', 'utr5-utr3', 'nonframeshift indel', 'intronic', 'intergenic', 'other' ),
			colours = c('darkseagreen4', 'orchid4',           'darkturquoise', 'darkorange',           'gold1',      'grey30',    'grey50',              'grey70',   'grey90',     'white' )
			),
		annovar.annotation = list( 
			levels  = c('nonsynonymous snv', 'stopgain snv', 'stoploss snv', 'frameshift deletion', 'frameshift substitution', 'splicing', 'synonymous snv'),
			colours = c('darkseagreen4',     'orchid4',      'darkturquoise','darkorange',          'darkorange4',             'gold1',    'darkgrey')
				),
		annovar.annotation.collapsed = list( 
			levels  = c('nonsynonymous snv', 'stopgain snv', 'stoploss snv', 'frameshift indel', 'splicing'),
			colours = c('darkseagreen4',     'orchid4',      'darkturquoise','darkorange',       'gold1'   )
			),
		biomolecule = list(
			levels = c('dna', 'rna', 'protein', 'carbohydrate', 'lipid'),
			colours = c(DNA,   RNA,   Protein,   Carbohydrate,   Lipid)
			),
		sex = list(
			levels = c('male', 'female'),
			colours = c('lightskyblue', 'lightpink')
			),
		stage = list(
			levels = c('i', 'ii', 'iii', 'iv'),
			colours = c(I,   II,   III,   IV)
			), 
		risk = list(
			levels = c('high', 'low'),
			colours = c(High,   Low)
			),
		old.risk = list(
			levels = c('high', 'low'),
			colours = c(old_High, old_Low)
			),
		msi = list(
			levels = c('msi-high', 'msi-low', 'mss'),
			colours = c(MSI_High,   MSI_Low,   MSS)
			),
		old.msi = list(
			levels = c('msi-high', 'msi-low', 'mss'),
			colours = c(old_MSI_High, old_MSI_Low, old_MSS)
			),
		tumour = list(
			levels = c('primary', 'metastatic'),
			colours = c(Primary,   Metastatic)
			),
		cnv = list(
			levels = c('amplification', 'deletion', 'loh', 'neutral'),
			colours = c(Amplification,   Deletion,   LOH,   Neutral)
			),
		old.cnv = list(
			levels = c('amplification', 'deletion', 'loh', 'neutral'),
			colours = c(MSI_High,   MSI_Low,   MSS,   Neutral)
			),
		organism = list(
			levels = c('human', 'rat', 'mouse'),
			colours = c(Human,   Rat,   Mouse)
			)		
		);

	if(is.null(avail.schemes[[scheme]]) && scheme != 'all'){ stop('Scheme not found!');}

	if(return.scheme){
		if ('all' == scheme) {
			return (avail.schemes)
			}
		else {
			return (avail.schemes[[scheme]])
			}
		}

	# check to see if more than 1 type unknown. 1 is expected for 'none' or similar, but 2+ likely to be error.
	if(1 < sum(!unique(x) %in% avail.schemes[[scheme]]$levels)) { 
		warning('more than one unique entry of x is not found in scheme. Perhaps an error?');
		}

	x.as.factor <- factor(x, levels = avail.schemes[[scheme]]$levels);
	if(return.factor) return( x.as.factor );

	x.colours <- avail.schemes[[scheme]]$colours[x.as.factor];
	x.colours[is.na(x.colours)] <- fill.colour;
	
	if(include.names) names(x.colours) <- x;
	x.colours;
	}  
