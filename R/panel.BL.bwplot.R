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

panel.BL.bwplot <- function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio), horizontal = TRUE, pch = box.dot$pch, col = box.dot$col, alpha = box.dot$alpha, cex = box.dot$cex, font = box.dot$font, fontfamily = box.dot$fontfamily, fontface = box.dot$fontface, fill = box.rectangle$fill, varwidth = FALSE, notch = FALSE, notch.frac = 0.5, enable.warnings = FALSE, ..., levels.fos = if (horizontal) sort(unique(y)) else sort(unique(x)), stats = boxplot.stats, coef = 1.5, do.out = TRUE) {
	
	BL.chooseFace <- function(fontface = NULL, font = 1){
		if (is.null(fontface)) { font; } else { fontface; }
		}

	if (all(is.na(x) | is.na(y))) { return() };
	x <- as.numeric(x);
	y <- as.numeric(y);

	box.dot <- trellis.par.get("box.dot");
	box.rectangle <- trellis.par.get("box.rectangle");
	box.umbrella <- trellis.par.get("box.umbrella");
	plot.symbol <- trellis.par.get("plot.symbol");

	fontsize.points <- trellis.par.get("fontsize")$points;
	cur.limits <- current.panel.limits();
	xscale <- cur.limits$xlim;
	yscale <- cur.limits$ylim;

	##### Begin: Correct colour-indexing mess-up created by panel.bwplot when a vector of colors are passed to it #####
	converted.box.col <- box.rectangle$col;
	ncolor <- length(box.rectangle$col);
	convert.index <- (11*(1:ncolor) - 10) %% ncolor;
	convert.index[convert.index == 0] <- ncolor;
	converted.box.col[convert.index] <- box.rectangle$col;

	box.umbrella.col.rep <- rep(box.umbrella$col, length=length(levels.fos));
	box.umbrella.col.rep <- c(box.umbrella.col.rep,box.umbrella.col.rep);
	##### End: Correct colour-indexing mess-up created by panel.bwplot when a vector of colors are passed to it #######
	
	if (!notch) { notch.frac <- 0; }

	if (horizontal) {

		blist <- tapply(
				x,
				factor(y, levels = levels.fos),
				stats,
				coef = coef,
				do.out = do.out
				);

		blist.stats <- t(sapply(blist, "[[", "stats"));
		blist.out <- lapply(blist, "[[", "out");
		blist.height <- box.width; # box.ratio / (1 + box.ratio)

		if (varwidth) {
			maxn <- max(table(y));
			blist.n <- sapply(blist, "[[", "n");
			blist.height <- sqrt(blist.n / maxn) * blist.height;
			}

		## start of major changes to support notches
		blist.conf <- if (notch)
				t(sapply(blist, "[[", "conf"))
			else
				blist.stats[ , c(2,4), drop = FALSE]

		xbnd <- cbind(blist.stats[, 3], blist.conf[, 2],
					blist.stats[, 4], blist.stats[, 4],
					blist.conf[, 2], blist.stats[, 3],
					blist.conf[, 1], blist.stats[, 2],
					blist.stats[, 2], blist.conf[, 1],
					blist.stats[, 3])
		ytop <- levels.fos + blist.height / 2
		ybot <- levels.fos - blist.height / 2
		ybnd <- cbind(ytop - notch.frac * blist.height / 2,
					ytop, ytop, ybot, ybot,
					ybot + notch.frac * blist.height / 2,
					ybot, ybot, ytop, ytop,
					ytop - notch.frac * blist.height / 2)
		xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
		ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]

		## box
		panel.polygon(t(xs), t(ys),
				  lwd = box.rectangle$lwd,
				  lty = box.rectangle$lty,
				  col = fill,
				  alpha = box.rectangle$alpha,
				  border = converted.box.col)
		## end of major changes to support notches


		## whiskers
		panel.segments(
			c(blist.stats[, 2], blist.stats[, 4]),
			rep(levels.fos, 2),
			c(blist.stats[, 1], blist.stats[, 5]),
			rep(levels.fos, 2),
			col = box.umbrella.col.rep,
			alpha = box.umbrella$alpha,
			lwd = box.umbrella$lwd,
			lty = box.umbrella$lty
			);
		panel.segments(
			c(blist.stats[, 1], blist.stats[, 5]),
			levels.fos - blist.height / 2,
			c(blist.stats[, 1], blist.stats[, 5]),
			levels.fos + blist.height / 2,
			col = box.umbrella.col.rep,
			alpha = box.umbrella$alpha,
			lwd = box.umbrella$lwd,
			lty = box.umbrella$lty
			);

		## dot
		if (all(pch == "|")) {
			mult <- if (notch) { 1 - notch.frac } else { 1 }
			panel.segments(blist.stats[, 3],
				levels.fos - mult * blist.height / 2,
				blist.stats[, 3],
				levels.fos + mult * blist.height / 2,
				lwd = box.rectangle$lwd,
				lty = box.rectangle$lty,
				col = box.rectangle$col,
				alpha = alpha
				);
			}
		else {
			panel.points(
				x = blist.stats[, 3],
				y = levels.fos,
				pch = pch,
				col = col,
				alpha = alpha,
				cex = cex,
				fontfamily = fontfamily,
				fontface = BL.chooseFace(fontface, font),
				fontsize = fontsize.points
				);
			}

		## outliers
		plot.symbol.col.rep <- rep(plot.symbol$col, length = length(blist.out));
		plot.symbol.col.rep <- rep(plot.symbol.col.rep, times = sapply(blist.out, length));

		panel.points(x = unlist(blist.out),
			y = rep(levels.fos, sapply(blist.out, length)),
			pch = plot.symbol$pch,
			col = plot.symbol.col.rep,
			alpha = plot.symbol$alpha,
			cex = plot.symbol$cex,
			fontfamily = plot.symbol$fontfamily,
			fontface = BL.chooseFace(plot.symbol$fontface, plot.symbol$font),
			fontsize = fontsize.points)
		}

	else {
		blist <-
			tapply(y, factor(x, levels = levels.fos),
				stats,
				coef = coef,
				do.out = do.out)
		blist.stats <- t(sapply(blist, "[[", "stats"))
		blist.out <- lapply(blist, "[[", "out")
		blist.height <- box.width # box.ratio / (1 + box.ratio)
		if (varwidth) {
			maxn <- max(table(x));
			blist.n <- sapply(blist, "[[", "n");
			blist.height <- sqrt(blist.n / maxn) * blist.height;
			}

		blist.conf <- if (notch)
				sapply(blist, "[[", "conf")
			else
				t(blist.stats[ , c(2,4), drop = FALSE])

		ybnd <- cbind(blist.stats[, 3], blist.conf[2, ],
					blist.stats[, 4], blist.stats[, 4],
					blist.conf[2, ], blist.stats[, 3],
					blist.conf[1, ], blist.stats[, 2],
					blist.stats[, 2], blist.conf[1, ],
					blist.stats[, 3])
		xleft <- levels.fos - blist.height / 2
		xright <- levels.fos + blist.height / 2
		xbnd <- cbind(xleft + notch.frac * blist.height / 2,
					xleft, xleft, xright, xright,
					xright - notch.frac * blist.height / 2,
					xright, xright, xleft, xleft,
					xleft + notch.frac * blist.height / 2)
		xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
		ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]

		## box
		panel.polygon(
			t(xs),
			t(ys),
			lwd = box.rectangle$lwd,
			lty = box.rectangle$lty,
			col = fill,
			alpha = box.rectangle$alpha,
			border = converted.box.col
			);

		## whiskers
		panel.segments(
			rep(levels.fos, 2),
			c(blist.stats[, 2], blist.stats[, 4]),
			rep(levels.fos, 2),
			c(blist.stats[, 1], blist.stats[, 5]),
			col = box.umbrella.col.rep,
			alpha = box.umbrella$alpha,
			lwd = box.umbrella$lwd,
			lty = box.umbrella$lty
			);
		panel.segments(
			levels.fos - blist.height / 2,
			c(blist.stats[, 1], blist.stats[, 5]),
			levels.fos + blist.height / 2,
			c(blist.stats[, 1], blist.stats[, 5]),
			col = box.umbrella.col.rep,
			alpha = box.umbrella$alpha,
			lwd = box.umbrella$lwd,
			lty = box.umbrella$lty
			);

		## dot
		if (all(pch == "|")) {
			mult <- if (notch) 1 - notch.frac else 1
			panel.segments(levels.fos - mult * blist.height / 2,
				blist.stats[, 3],
				levels.fos + mult * blist.height / 2,
				blist.stats[, 3],
				lwd = box.rectangle$lwd,
				lty = box.rectangle$lty,
				col = box.rectangle$col,
				alpha = alpha)
			}
		else {
			panel.points(
				x = levels.fos,
				y = blist.stats[, 3],
				pch = pch,
				col = col, alpha = alpha, cex = cex,
				fontfamily = fontfamily,
				fontface = BL.chooseFace(fontface, font),
				fontsize = fontsize.points
				);
			}

		## outliers
		plot.symbol.col.rep <- rep(plot.symbol$col, length = length(blist.out));
		plot.symbol.col.rep <- rep(plot.symbol.col.rep, times = sapply(blist.out, length));
		panel.points(
			x = rep(levels.fos, sapply(blist.out, length)),
			y = unlist(blist.out),
			pch = plot.symbol$pch,
			col = plot.symbol.col.rep,
			alpha = plot.symbol$alpha,
			cex = plot.symbol$cex,
			fontfamily = plot.symbol$fontfamily,
			fontface = BL.chooseFace(plot.symbol$fontface, plot.symbol$font),
			fontsize = fontsize.points
			);
		}
	}
