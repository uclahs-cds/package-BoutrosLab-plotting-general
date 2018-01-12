.inside.auto.legend <- function(function.name, filename, trellis.object, height, width, extra.parameters) {
	if (is.null(filename)) {
		height <- 7;
		}
	if (is.null(filename)) {
		width <- 7;
		}
	extension <- file_ext(filename);
	if (!is.null(filename)) {
                if ('tiff' == extension) {
                        tiff(filename = 'temp', type = 'cairo', units = 'in', height = height, width = width, res = 92);
                        }
                else if ('png' == extension) {
                        png(filename = 'temp', type = 'cairo', units = 'in', height = height, width = width, res = 1000);
                        }
                else if ('pdf' == extension) {
                        cairo_pdf(filename = 'temp', height = height, width = width);
                        }
                else if ('svg' == extension) {
                        svg(filename = 'temp', height = height, width = width);
                        }
                else if ('eps' == extension) {
                        postscript(height = height, width = width);
                        }
                else {
                        stop('File type not supported');
                        }
                }
	else {
		cairo_pdf(filename = 'temp', height = height, width = width);
		}
	plot(trellis.object);

	#### START OF INSIDE LEGEND CODE
	### get down to figure viewport

	downViewport('plot_01.figure.vp');

	### get width of figure and grob respectively
	width.fig <- convertUnit(
		current.viewport()$width,
		unitTo = 'points',
		axisFrom = 'x',
		typeFrom = 'dimension',
		valueOnly = TRUE
		);

	height.fig <- convertUnit(
		current.viewport()$height,
		unitTo = 'points',
		axisFrom = 'y',
		typeFrom = 'dimension',
		valueOnly = TRUE
		);

	### ADD A LITTLE EXTRA TO MAKE GROBS MORE PICKY ABOUT SIZING
	height.grob <- convertUnit(
		grobHeight(trellis.object$legend$inside$fun),
		unitTo = 'points',
		axisFrom = 'y',
		typeFrom = 'dimension',
		valueOnly = TRUE
		) * 1.05;

	width.grob <- convertUnit(
		grobWidth(trellis.object$legend$inside$fun),
		unitTo = 'points',
		axisFrom = 'x',
		typeFrom = 'dimension',
		valueOnly = TRUE
		) * 1.05;

	dev.off();
	file.remove('temp');

	### convert data into proper form
	ret.coords <- do.call(paste0('.', function.name, '.inside.legend'), list(height.fig, width.fig, height.grob, width.grob, extra.parameters));
	return(ret.coords);
	}


.create.barplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {
	### VALUES FOR POINT DISTRIBUTION
	data <- extra.parameters$data;
	plot.horizontal <- extra.parameters$plot.horizontal;
	formula <- extra.parameters$formula;
	groups <- extra.parameters$groups;
	stack <- extra.parameters$stack;
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	pass.through.mid <- 1;
        pass.through.top <- 100;

	value.param <- if (plot.horizontal) {formula[[3]]} else {formula[[2]]}
	index.param <- if (plot.horizontal) {formula[[2]]} else {formula[[3]]}

	# handle splitting of data cases

	if (!is.null(eval(substitute(groups), data, parent.frame())) || stack == TRUE) {
		s <- split(data, data[toString(index.param)]);
		f <- list();
		for (x in 1:length(s)) {
			if (stack == TRUE) {
				f[[x]] <- sum(s[[x]][toString(value.param)]);
				}
			else {
				f[[x]] <- max(s[[x]][toString(value.param)]);
				}
			}
		data <- f;
		}
	else {
		data <- data[[toString(value.param)]];
		}

	max.height.val <- list();
	min.height.val <- list();
	max.width.val <- list();
	min.width.val <- list();

	### FIGURE OUT WHERE TO PUT THE GROB
	grob.y <- height.fig - height.fig * 0.025;
	grob.x <- 0  + width.fig * 0.025;

	min.x <- width.fig;
	min.y <- -1;
	min.val <- 100000;

	### FIND OPTIMAL PLACEMENT OF GROB
	if (!plot.horizontal) {

		### GET MAX
		if (is.null(ylimits)) {
			diff <- max(unlist(data)) - min(unlist(data));
			max <- max(unlist(data) + diff * 0.07);
			min <- min(unlist(data) - diff * 0.07);
			}
		else {
			max <- ylimits[2];
			min <- ylimits[1];
			}

		### EVALUATE TRUE LOCATION BASED ON POINTS FROM VALUE
		points.per.value <- height.fig / (max - min);
		points.per.bar <- width.fig / length(data);

		### SET LOCATION OF THE MAX AND MIN WIDTHS
		for (i in 1:length(data)) {
			max.height.val[[i]] <- data[[i]] * points.per.value;
			max.width.val[[i]] <- i * points.per.bar;
			min.width.val[[i]] <- (i - 1) * points.per.bar;
			}

		for (y in seq(grob.y, height.grob * 0.975, -3)) {
			for (x in seq(grob.x, (width.fig - width.grob) * 0.975, 3)) {
				val <- 0;
				for (i in 1:length(max.height.val)) {
					if (x <= max.width.val[[i]] && (x + width.grob) >= min.width.val[[i]]) {
						if (y > max.height.val[[i]] && (y - height.grob) < max.height.val[[i]]) {
							val <- val + pass.through.top;
							}
						else if (y <= max.height.val[[i]] && (y - height.grob) <= max.height.val[[i]]) {
							val <- val + pass.through.mid;
							}
						}
					}
				if (val < min.val || ( ( (val == 0 && y >= min.y ) || (val != 0 && y <= min.y)) && x <= min.x) && min.val == val) {
					min.val <- val;
					min.x <- x;
					min.y <- y;
					}
				}
			}
		}
	else {
		if (is.null(xlimits)) {
			diff <- max(unlist(data)) - min(unlist(data));
			max <- max(unlist(data) + diff * 0.07);
			min <- min(unlist(data) - diff * 0.07);
			}
		else {
			max <- xlimits[2];
			min <- xlimits[1];
			}

		points.per.value <- width.fig / (max - min);
		points.per.bar <- height.fig / length(data);

		for (i in 1:length(data)) {
			max.width.val[[i]] <- data[[i]] * points.per.value;
			max.height.val[[i]] <- height.fig - (i - 1) * points.per.bar;
			min.height.val[[i]] <- height.fig - (i) * points.per.bar;
			}

		max.width.val <- rev(max.width.val);

		for (y in seq(grob.y, height.grob * 0.975, -3)) {
			for (x in seq(grob.x, (width.fig - width.grob) * 0.975, 3)) {
				val <- 0;
				for (i in 1:length(max.height.val)) {
					if ( (y - width.grob) <= max.height.val[[i]] && (y) >= min.height.val[[i]]) {
						if ( (x + width.grob)  > max.width.val[[i]] && x < max.width.val[[i]]) {
							val <- val + pass.through.top;
							}
						else if ( (x + width.grob) <= max.width.val[[i]] && x <= max.width.val[[i]]) {
							val <- val + pass.through.mid;
							}
						}
					}
				if (val < min.val || (y >= min.y && x >= min.x) && min.val == val) {
					min.val <- val;
					min.x <- x;
					min.y <- y;
					}
				}
			}
		}
	return(c(min.x / width.fig, min.y / height.fig));
	}


.create.hexbinplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GATHER ALL INFO FROM HEXBINPLOT
	x <- as.vector(unlist(extra.parameters$x));
	y <- as.vector(unlist(extra.parameters$y));
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	### CALCULATE THE MAX VALUES OF X and Y
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits)) {
                diff <- max(data$x) - min(data$x);
                max.x <- max(unlist(data) + diff * 0.07);
                min.x <- min(unlist(data) - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(data$y) - min(data$y);
                max.y <- max(unlist(data) + diff * 0.07);
                min.y <- min(unlist(data) - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}


	### GET AMOUNT OF POINTS PER VALUE
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

        grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;

	data <- data.frame(x = x, y = y);

	### FIND OPTIMAL PLACEMENT OF GROB
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			crossed <- 0;
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob) ) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob) ) );

			crossed <- nrow(data.to.use);

			if (total.crossed == -1 || (crossed < total.crossed || (y.val >= ret.y && x.val >= ret.x)  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}

	return(c(ret.x / width.fig, ret.y / height.fig));
	}

.create.densityplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET DATA FROM DENSITY PLOT
	data <- extra.parameters$data;
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	### FIND MAX AND MIN VALUES
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits)) {
                diff <- max(data$x) - min(data$x);
                max.x <- max(unlist(data) + diff * 0.07);
                min.x <- min(unlist(data) - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(data$y) - min(data$y);
                max.y <- max(unlist(data) + diff * 0.07);
                min.y <- min(unlist(data) - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### POINTS PER VALUE IN PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

        grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;
	x.vals <- unlist(data[1]);
	y.vals <- unlist(data[2]);

	### FIND OPTIMAL PLACEMENT OF GROB
	for (y in seq(grob.y, height.grob * 0.975, -5)) {
		for (x in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			crossed <- 0;
			for (i in 1:nrow(data)) {
				if ( (x.vals[i] - min.x) * points.per.x <= ( x + width.grob ) && (x.vals[i] - min.x) * points.per.x >= x &&
					( y.vals[i] - min.y ) * points.per.y <= y && ( y.vals[i] - min.y ) * points.per.y >= ( y - height.grob )) {
					crossed <- crossed + 1;
					}
				}
			if (total.crossed == -1 || (crossed < total.crossed || ( y >= ret.y && x >= ret.x )  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x;
				ret.y <- y;
				}
			}
		}
	return(c(ret.x / width.fig, ret.y / height.fig));
	}


.create.histogram.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### VALUES FOR POINT DISTRIBUTION
	data <- extra.parameters$x;
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;
	breaks <- extra.parameters$breaks;
	nint <- extra.parameters$nint;
	type <- extra.parameters$type;

	## convert to proper format
	bar.locations.min <- c();
	bar.locations.max <- c();

	### GET MAX AND MIN VALUES
        max.x <- NULL;
        min.x <- NULL;
        max.y <- NULL;
        min.y <- NULL;

        if (is.null(xlimits)) {
                diff <- max(data) - min(data);
                max.x <- max(unlist(data) + diff * 0.07);
                min.x <- min(unlist(data) - diff * 0.07);
                }
        else {
                max.x <- xlimits[2];
                min.x <- xlimits[1];
                }

	if (!is.null(breaks)) {
		bar.locations.min <- c(min.x, breaks);
		bar.locations.max <- c(breaks, max.x);
		}
	else {
		breaks <- do.breaks(range(data, finite = TRUE), nint);
		bar.locations.min <- breaks[1:length(breaks) - 1];
		bar.locations.max <- breaks[2:length(breaks)];
		}

	### ADJUST BASED ON HISTOGRAM TYPE
	if (type == 'percent') {
		counts <- 100 * do.call('hist', list(x = data, plot = FALSE, breaks = breaks))$counts / length(data);
		}
	else if (type == 'count') {
		counts <- do.call('hist', list(x = data, plot = FALSE, breaks = breaks))$counts;
		}
	else if  (type == 'density') {
		counts <- do.call('hist', list(x = data, plot = FALSE, breaks = breaks))$density;
		}


	if (is.null(ylimits)) {
		diff <- max(counts);
		max.y <- max(counts + diff * 0.07);
		min.y <- min(0 - diff * 0.07);
		}
	else {
		max.y <- ylimits[2];
		min.y <- ylimits[1];
		}

	### GET POINTS PER EACH VALUE
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	pass.through.mid <- 1;
        pass.through.top <- 100;

	bar.locations.min <- (bar.locations.min - min.x) * points.per.x;
	bar.locations.max <- (bar.locations.max - min.x) * points.per.x;
	counts <- (counts - min.y) * points.per.y;

	### FIGURE OUT WHERE TO PUT THE GROB
	grob.y <- height.fig - height.fig * 0.025;
	grob.x <- 0  + width.fig * 0.025;
	x.ret.val <- width.fig;
	y.ret.val <- -1;
	min.val <- 100000;

	for (y.val in seq(grob.y, height.grob * 0.975, -3)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 3)) {
			val <- 0;
			for (i in 1:length(counts)) {
				if ( (y.val - height.grob) <= counts[i] && (y.val) >= 0) {
					if ( (x.val + width.grob)  > bar.locations.min[i] && x.val < bar.locations.max[i]) {
						val <- val + pass.through.top;
						}
					else if ( (x.val + width.grob) <= bar.locations.max[i] && x.val <= bar.locations.max[i]) {
						val <- val + pass.through.mid;
						}
					}
				}
			if (val < min.val || (y.val >= y.ret.val && x.val <= x.ret.val) && min.val == val) {
				min.val <- val;
				x.ret.val <- x.val;
				y.ret.val <- y.val;
				}
			}
		}
	return(c(x.ret.val / width.fig, y.ret.val / height.fig));
	}

.create.polygonplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET DATA FROM POLYGON PLOT
	data <- extra.parameters$data;
	formula <- extra.parameters$formula;
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;
	extra.points <- extra.parameters$extra.points;
	max.vals <- extra.parameters$max;
	min.vals <- extra.parameters$min;

	### GET MAX AND MIN VALUES OF PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	x.data <- unlist(data[toString(formula[[3]])]);

	if (is.null(xlimits)) {
                diff <- max(x.data) - min(x.data);
                max.x <- max(x.data + diff * 0.07);
                min.x <- min(x.data - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(max.vals) - min(min.vals);
                max.y <- max(max.vals + diff * 0.07);
                min.y <- min(min.vals - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### GET POINTS PER EACH VALUE IN PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;


	x.data <- as.vector(x.data);
	names(data) <- c('x', 'max', 'min');
	if (!is.null(extra.points)) {
		extra.points <- as.data.frame(extra.points);
		}

	### FIND OPTIMAL PLACEMENT OF GROB
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			crossed <- 0;
			data.to.use.polygon <- subset(data, ( (x.data - min.x) * points.per.x <= (x.val + width.grob)) & ( (x.data - min.x) * points.per.x >= x.val) &
				( ( (min.vals - min.y) * points.per.y <= y.val) & ( (min.vals - min.y) * points.per.y >= (y.val - height.grob)) |
					( (max.vals - min.y) * points.per.y <= y.val) & ( (max.vals - min.y) * points.per.y >= (y.val - height.grob))));
			if (!is.null(extra.points)) {
				data.to.use.extra.points <- subset(extra.points, ( (extra.points$x - min.x) * points.per.x <= (x.val + width.grob)) &
					( (extra.points$x - min.x) * points.per.x >= x.val) & ( (extra.points$y - min.y) * points.per.y <= y.val) &
					( (extra.points$y - min.y) * points.per.y >= (y.val - height.grob)));
				crossed <- nrow(data.to.use.polygon) + nrow(data.to.use.extra.points);
				}
			else {
				crossed <- nrow(data.to.use.polygon);
				}
			if (total.crossed == -1 || (crossed < total.crossed || (y.val >= ret.y && x.val >= ret.x)  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}

	return(c(ret.x / width.fig, ret.y / height.fig));
	}

.create.manhattanplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET DATA FROM MANHATTAN PLOT
	x <- as.vector(unlist(extra.parameters$x));
	y <- as.vector(unlist(extra.parameters$y));
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	### GET MINIMUM AND MAXIMUM VALUES OF PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits)) {
                diff <- max(x) - min(x);
                max.x <- max(x + diff * 0.07);
                min.x <- min(x - diff * 0.07);
        	}

	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}

	if (is.null(ylimits)) {
                diff <- max(y) - min(y);
                max.y <- max(y + diff * 0.07);
                min.y <- min(y - diff * 0.07);
                }

	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### GET AMOUNT OF POINTS PER VALUE OF PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;

	data <- data.frame(x = x, y = y);

	### FIND OPTIMAL PLACEMENT OF GROB
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob)) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || crossed < total.crossed) {

				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}
	return(c(ret.x / width.fig, ret.y / height.fig));

	}

.create.qqplot.comparison.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET VALUES FROM QQ COMPARISON PLOT
	x <- as.vector(unlist(extra.parameters$x));
	y <- as.vector(unlist(extra.parameters$y));
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;


	### GET MAX AND MIN VALUES OF THE PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits)) {
                diff <- max(x) - min(x);
                max.x <- max(x + diff * 0.07);
                min.x <- min(x - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(y) - min(y);
                max.y <- max(y + diff * 0.07);
                min.y <- min(y - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### GET NUMBER OF POINTS PER VALUE OF PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;

	data <- data.frame(x = x, y = y);

	### FIND OPTIMAL PLACEMENT OF THE GROB
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob)) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || crossed < total.crossed) {

				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}
	return(c(ret.x / width.fig, ret.y / height.fig));
	}


.create.qqplot.fit.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET VALUES FROM QQ PLOT FIT
	y <- as.vector(unlist(extra.parameters$x));
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	n.data <- length(y);
	y <- sort(y);
	x <- c();

	for ( i in 1:n.data) {
		x <- c(x, i / n.data);
		}

	### GET MIN AND MAX VALUE OF THE PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits)) {
                diff <- max(x) - min(x);
                max.x <- max(x + diff * 0.07);
                min.x <- min(x - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(y) - min(y);
                max.y <- max(y + diff * 0.07);
                min.y <- min(y - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### NUMBER OF POINTS PER VALUE OF THE PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;

	data <- data.frame(x = x, y = y);

	### FIND OPTIMAL PLACE FOR THE GROB
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob)) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || crossed < total.crossed) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}

	return(c(ret.x / width.fig, ret.y / height.fig));
	}


.create.segplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET VALUES FROM SEGPLOT
	min.vals <- as.vector(unlist(extra.parameters$x));
	max.vals <- as.vector(unlist(extra.parameters$y));
	y <- c(1:length(min.vals));

	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	### GET MIN AND MAX VALUE OF PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits) || !is.numeric(xlimits)) {
                diff <- max(max.vals) - min(min.vals);
                max.x <- max(max.vals + diff * 0.07);
                min.x <- min(min.vals - diff * 0.07);
        	}

	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}

	if (is.null(ylimits) || !is.numeric(ylimits)) {
                diff <- max(y) - min(y);
                max.y <- max(y + diff * 0.07);
                min.y <- min(y - diff * 0.07);
                }

	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### NUMBER OF POINTS PER VALUE IN PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;

	data <- data.frame(min.vals = min.vals, max.vals = max.vals, y = y);

	### FIND OPTIMAL GROB PLACEMENT
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$min.vals - min.x) * points.per.x <= (x.val + width.grob)) &
				( (data$max.vals - min.x) * points.per.x >= (x.val)) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || (crossed < total.crossed || (y.val >= ret.y && x.val >= ret.x)  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}

	return(c(ret.x / width.fig, ret.y / height.fig));
	}

.create.stripplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	### GET VALUES FROM STRIPPLOT
	horizontal <- extra.parameters$horizontal;
	x <- unlist(extra.parameters$x);
	y <- unlist(extra.parameters$y);
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;
	unique.vals <- NULL;



	### HANDLE HORIZONTAL CASES AND FIND UNIQUE VALUES FOR ROWS
	if (horizontal) {


		if (is.null(levels(y))) {
			unique.vals <- as.character(unique(y));
			}
		else {
			unique.vals <- as.character(levels(y));
			}

		y <- as.character(y);
		for (i in 1:length(unique.vals) ) {
			y[as.character(y) == unique.vals[i]] <- as.character(i);

			}
		y <- as.integer(y);

		}
	else {
		if (is.null(levels(x))) {
			unique.vals <- as.character(unique(x));
			}
		else {
			unique.vals <- as.character(levels(x));
			}
		x <- as.character(x);
		for ( i in 1:length(unique.vals) ) {
			x[as.character(x) == unique.vals[i]] <- as.character(i);

			}
		x <- as.integer(x);
		}



	### GET THE IMN AND MAX VALUES OF PLOT
	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	if (is.null(xlimits) || !is.numeric(xlimits)) {
                diff <- max(x) - min(x);
                max.x <- max(x + diff * 0.07);
                min.x <- min(x - diff * 0.07);
        	}

	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}

	if (is.null(ylimits) || !is.numeric(ylimits)) {
                diff <- max(y) - min(y);
                max.y <- max(y + diff * 0.07);
                min.y <- min(y - diff * 0.07);
                }

	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	### POINTS PER VALUE OF PLOT
	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;
	data <- data.frame(x = x, y = y);

	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob)) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || (crossed < total.crossed || (y.val >= ret.y && x.val >= ret.x)  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}
	return(c(ret.x / width.fig, ret.y / height.fig));

	}
.create.scatterplot.inside.legend <- function(height.fig, width.fig, height.grob, width.grob, extra.parameters) {

	data <- extra.parameters$data;
	formula <- extra.parameters$formula;
	ylimits <- extra.parameters$ylimits;
	xlimits <- extra.parameters$xlimits;

	max.x <- NULL;
	min.x <- NULL;
	max.y <- NULL;
	min.y <- NULL;

	x.data <- unlist(data[toString(formula[[3]])]);
	y.data <- unlist(data[toString(formula[[2]])]);

	if (is.null(xlimits)) {
                diff <- max(x.data) - min(x.data);
                max.x <- max(x.data + diff * 0.07);
                min.x <- min(x.data - diff * 0.07);
        	}
	else {
		max.x <- xlimits[2];
		min.x <- xlimits[1];
		}
	if (is.null(ylimits)) {
                diff <- max(y.data) - min(y.data);
                max.y <- max(y.data + diff * 0.07);
                min.y <- min(y.data - diff * 0.07);
                }
	else {
                max.y <- ylimits[2];
                min.y <- ylimits[1];
		}

	points.per.y <- height.fig / (max.y - min.y);
        points.per.x <- width.fig / (max.x - min.x);

	grob.y <- height.fig - height.fig * 0.025;
        grob.x <- 0  + width.fig * 0.025;

	total.crossed <- -1;
	ret.x <- 0;
	ret.y <- 0;
	names(data) <- c(toString(formula[[3]]), toString(formula[[2]]));
	for (y.val in seq(grob.y, height.grob * 0.975, -5)) {
		for (x.val in seq(grob.x, (width.fig - width.grob) * 0.975, 5)) {
			data.to.use <- subset(data, ( (data$x - min.x) * points.per.x <= (x.val + width.grob)) & ( (data$x - min.x) * points.per.x >= x.val) &
				( (data$y - min.y) * points.per.y <= y.val) & ( (data$y - min.y) * points.per.y >= (y.val - height.grob)));
			crossed <- nrow(data.to.use);
			if (total.crossed == -1 || (crossed < total.crossed || (y.val >= ret.y && x.val >= ret.x)  && crossed == total.crossed)) {
				total.crossed <- crossed;
				ret.x <- x.val;
				ret.y <- y.val;
				}
			}
		}

	return(c(ret.x / width.fig, ret.y / height.fig));
	}
