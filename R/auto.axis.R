# The BoutrosLab.plotting.general package is copyright (c) 2014 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# Bx downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which max arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

prep.axis <- function(
    at,
    data,
    which.arg
    ) {

    if (is.null(at)) {
        return(at);
        }
    else if (is.logical(at)) {
        return(at);
        }
    else if (length(at) > 1) {
        return(at);
        }

    out <- list();
    if ('auto' == at) {
        out <- auto.axis(
            x = data
            );
        }
    else if ('auto.linear' == at) {
        out <- auto.axis(
            x = data,
            log.scaled = FALSE
            );
        }
    else if ('auto.log' == at) {
        out <- auto.axis(
            x = data,
            log.scaled = TRUE
            );
        }
    else {
        stop(paste0('Invalid input to ', which.arg, ': ', at));
        }
    return(out);

    }

auto.axis <- function(
    x,
    pretty = TRUE,
    log.scaled = NA,
    log.zero = 0.1,
    max.factor = 1,
    min.factor = 1,
    include.origin = TRUE,
    num.labels = 5,
    max.min.log10.diff = 2
    ) {

    out <- list();

    x <- as.numeric(x);

    # Get max and min to plot
    max.x <- max(x, na.rm = TRUE) * max.factor;
    min.x <- min(x, na.rm = TRUE) * min.factor;

    # Make sure x is all > 0 to consider log scale
    if (all(x > 0, na.rm = TRUE)) {
        # Determine scale based on skewness
        skewness.x <- skewness(x, na.rm = TRUE);
        # Handle zero values so that they can still be plotted even log(0) = -Inf
        zero.i <- which(0 == x);
        logx <- log(x, 10);

        if (length(zero.i) > 0) {
            # use a proxy for log(0)
            logx[zero.i] <- log.zero;
            }

        if (max.x - min.x != 0) {
            # x.log10.diff scale should be more than 2
            cond1 <- log10(max.x - min.x) > max.min.log10.diff;

            }
        else {
            cond1 <- FALSE; # vector x only contains 0
            }

        #skewness.x should be larger than skewness.log10(x)
        cond2 <- (skewness.x > skewness(logx, na.rm = TRUE));
        }
    else {
        cond1 <- FALSE;
        cond2 <- FALSE;
        }

    # Decide log scale or not based on cond1 and cond2
    if (cond1 && cond2) {
        out$log.scaled <- TRUE;

        if (!is.na(log.scaled) && !log.scaled) {
            out$log.scaled <- FALSE;
            }
        }
    else {
        out$log.scaled <- FALSE;

        # Force log scale
        if (!is.na(log.scaled) && log.scaled) {
            out$log.scaled <- TRUE;

            if (!all(x > 0, na.rm = TRUE)) {
                stop('can not use log-scale as the input vector contains negative values.');
                }
            }
        }

    if (out$log.scaled) {
        out$x <- logx;
        min.x <- min(out$x, na.rm = TRUE);
        max.x <- max(out$x, na.rm = TRUE);

        out$at <- generate.at(min.x, max.x, pretty, include.origin, num.labels);

        # set axis labels
        out$axis.lab <- sapply(
            out$at[-1], # Remove 1
            FUN = function(x) {
                substitute(bold('10' ^ a), list(a = as.character(x)));
                }
            );

        out$axis.lab <- c(expression(bold('0')), out$axis.lab);
        }
    else {
        # For variables that are continuous and will not be plotted on a log-scale
        # Set axis increments
        out$x <- x;

        out$at <- generate.at(min.x, max.x, pretty, include.origin, num.labels);

        # Set axis labels
        if (abs(mean(x, na.rm = TRUE)) > 1000 || abs(mean(x, na.rm = TRUE)) < 0.001 ) {
            out$axis.lab <- as.power10.expression(out$at);
            }
        else {
            out$axis.lab <- out$at;
            }
        }

    return(out);
    }

generate.at <- function(min.x, max.x, pretty = TRUE, include.origin = TRUE, num.labels = 4) {

    out <- c();

    if (pretty) {
        if (max.x * min.x <= 0 || include.origin == FALSE) {
            out <- pretty(c(min.x, max.x), n = num.labels - 1);
            }
        else {
            if (min.x > 0 && include.origin == TRUE) {
                out <- pretty(c(0, max.x), n = num.labels - 1);
                }
            if (max.x < 0 && include.origin == TRUE) {
                out <- pretty(c(min.x, 0), n = num.labels - 1);
                }
            }
        }
    else {
        if (max.x * min.x <= 0 || include.origin == FALSE) {
            out <- seq(min.x, max.x, length.out = num.labels);
            }
        else {
            if (min.x > 0 && include.origin == TRUE) {
                out <- seq(0, max.x, length.out = num.labels);
                }
            if (max.x < 0 && include.origin == TRUE) {
                out <- seq(min.x, 0, length.out = num.labels);
                }
            }
        }

    return(out);
    }


as.power10.expression <- function(x) {

    x <- unlist(x);

    out <- sapply(x, function(y) {
        y <- as.numeric(y);
        # No need to do anything if x = 0
        if (0 == y) {
            return(expression(bold('0')));
            }

        # Otherwise, convert x to power of 10 and split by e
        y <- as.numeric(unlist(strsplit(sprintf('%e', y), split = 'e')));

        # If x[1] = 1, then a should be omitted.
        if (1 != y[1]) {
            y <- substitute(bold(a %*% '10' ^ b), list(a = as.character(y[1]), b = as.character(y[2])));
            }
        else {
            y <- substitute(bold('10' ^ b), list(b = as.character(y[2])));
            }

        # Return as expression otherwise list
        as.expression(y);
        });

    # Return as expression
    return(out);
    }
