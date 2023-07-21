get.valid.lty.values <- function() {
    valid.lty <- c(
        'blank',
        'solid',
        'dashed',
        'dotted',
        'dotdash',
        'longdash',
        'twodash'
        );
    }

is.valid.lty <- function(lty) {
    valid.values <- get.valid.lty.values();

    is.valid <- if (is.numeric(lty)) {
        lty < length(valid.values) - 1 & lty >= 0;
    } else {
        lty %in% valid.values;
        }
    }

get.invalid.lty.message <- function() {
    valid.lty <- c(
        'blank',
        'solid',
        'dashed',
        'dotted',
        'dotdash',
        'longdash',
        'twodash'
    );

    min.numeric.value <- 0;
    max.numeric.value <- length(valid.lty) - 1;

    msg <- paste(
        'Invalid "lty" value.',
        'Must be one of', paste(
            sapply(
                valid.lty,
                FUN = function(x) paste0('"', x, '"')
                ),
            collapse = ', '
            ),
        'or a numeric value between',
        min.numeric.value, 'and', max.numeric.value
        );

    return(msg);
    }

validate.lty <- function(lty) {
    is.valid <- is.valid.lty(lty);
    
    if (!all(is.valid)) {
        warning(get.invalid.lty.message());
        }
    }
