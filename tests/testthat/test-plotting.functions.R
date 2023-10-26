### TEST-SUITE #####################################################################################
# Description: This script runs the plotting code in BoutrosLab.plotting.general for testing purposes
# The plots are stored in variables and plotted together in a single multiple in order to save plot-generation time

test_that(
    'Run plotting functions', {
        ### PLOTTING FUNCTIONS #############################################################################
        test.data <- data.frame(
        	x = 1:10,
        	y = LETTERS[1:10],
        	z = rnorm(10),
        	type = rep(LETTERS[1:2], 5),
        	stringsAsFactors = TRUE
        	);

        # testing legend.grob
        covariate.legend <- list(
        	legend = list(
        		colours = default.colours(2),
        		labels = levels(test.data$type)[1:2]
        		)
        	);

        legend.grob2 <- legend.grob(
        	legends = covariate.legend
        	);

        # testing covariates.grob
        covariate.colours1 <- as.character(test.data$type);
        covariate.colours1[covariate.colours1 == 'A'] <- default.colours(2)[1];
        covariate.colours1[covariate.colours1 == 'B'] <- default.colours(2)[2];

        # create an object to draw the covariates from
        covariates1 <- list(
        	rect = list(
        		col = 'black',
        		fill = covariate.colours1,
        		lwd = 1.5
        		)
        	);

        covariates.grob1 <- covariates.grob(
        	covariates = covariates1,
        	ord = c(1:10),
        	side = 'top',
        	size = .8
        	);

        barplot <- create.barplot(
        	formula = y ~ x,
        	data = test.data,
        	legend = list(
        		bottom = list(
        			fun = covariates.grob1
        			),
        		right = list(
        			fun = legend.grob2
        			),
        		inside = list(
        			fun = draw.key,
        			args = list(
        				key = get.corr.key(
        					x = test.data$x,
        					y = test.data$y,
        					label.items = c('spearman', 'spearman.p', 'kendall', 'beta1')
        					)
        				),
        			x = 0.5,
        			y = 0.5
        			)
        		),
        	description = 'testing metadata'
        	);

        # this isn't printed anywhere
        dend <- create.dendrogram(
        	x = data.frame(test.data$z, rnorm(10))
        	);

        dotmap <- create.dotmap(
        	x = test.data$z
        	);

        hexbin <- create.hexbinplot(
        	formula = z ~ x,
        	data = test.data
        	);

        # Also using generate.at.final()
        density <- create.densityplot(
        	x = list(
        		a = test.data$x,
        		b = test.data$z
        		),
        	xat = c(0, 1.1, 1.5, 1, 6, 4),
        	yat = c(0.34, 0.38, 0.7),
        	type = c('g', 'l'),
        	resolution = 100
        	);

        manhattanplot <- create.manhattanplot(
        	formula = x ~ y,
        	data = test.data
        	);

        polygonplot <- create.polygonplot(
        	formula = NA ~ x,
        	data = test.data,
        	max = test.data$x,
        	min = test.data$z,
        	xlimits = c(0, 10),
        	ylimits = c(-2, 10)
        	);

        qqcomp <- create.qqplot.comparison(
        	x = list(test.data$x, test.data$z)
        	);

        scatter <- create.scatterplot(
        	formula = x ~ z,
        	data = test.data
        	);

        seg <- create.segplot(
        	formula = y ~ x + z,
        	data = test.data
        	);

        violin <- create.violinplot(
        	formula = y ~ z,
        	data = test.data
        	);

        strip <- create.stripplot(
        	formula = z ~ y,
        	data = test.data
        	);

        boxplot <- create.boxplot(
        	formula = x ~ y,
        	data = test.data
        	);

        # these do not easily join the multiplot
        histogram <- create.histogram(
        	x = test.data$z
        	);

        qqfit <- create.qqplot.fit(
        	x = test.data$z
        	);

        expect_no_error({
            # Plot everything in one plot for quicker running time
            create.multiplot(
                file = NULL,
                plot.objects = list(polygonplot, manhattanplot, hexbin, dotmap, density, qqcomp, scatter, seg, violin, barplot, strip, boxplot)
                );
            });
        }
    );

test_that(
    'Run helper functions', {
        expect_no_error({
            ### HELPER FUNCTIONS ###############################################################################
            get.line.breaks(1:10);

            scientific.notation(1234, 2, type = 'list');
            scientific.notation(0, 2);
            scientific.notation(c(1234, 1234), 1);

            # This returns the same thing as when type = 'expression' - should something be changed?
            scientific.notation(c(1234, 1234), 1, type = 'list');

            display.statistical.result(1234);

            ### COLOUR FUNCTIONS ###############################################################################
            default.colours(12, is.greyscale = FALSE);
            default.colours(5, palette.type = 'chromosomes', is.greyscale = FALSE);
            default.colours(5, palette.type = 'seq');
            default.colours(c(4, 4), palette.type = c('seq', 'div'));

            force.colour.scheme(c('stopgain snv', 'splicing'), 'annovar.annotation');
            });
        }
    );
