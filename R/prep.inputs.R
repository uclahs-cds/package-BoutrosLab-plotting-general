prep.sample.order <- function(sample.order) {
    if (length((sample.order) == 1)) {
        return(prep.sample.order.setting(sample.order));
        }

    contains.na <- any(is.na(sample.order));

    if (is.null(sample.order) || contains.na) {
        sample.order <- 'none';

        if (contains.na) {
            warning(paste(
                'NA values found in "sample.order" (using default "none" setting).'
                ));
            }
        }

    result <- prep.sample.order;
    }

prep.sample.order.setting <- function(sample.order) {
    valid.values <- c(
        'none',
        'ascending',
        'descending'
    );
    if (!(sample.order %in% valid.values)) {
        stop(paste('Invalid "sample.order":', paste0('(', sample.order, ')')))
        }
    return(sample.order);
    }
