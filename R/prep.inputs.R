prep.sample.order <- function(sample.order) {
    if (length(sample.order) == 1) {
        return(prep.sample.order.setting(sample.order));
        }

    contains.na <- any(is.na(sample.order));

    if (is.null(sample.order) || contains.na) {
        sample.order <- sample.order.default();

        if (contains.na) {
            warning(paste(
                'NA values found in "sample.order" (using default "none" setting).'
                ));
            }
        }

    return(sample.order);
    }

prep.sample.order.setting <- function(sample.order) {
    if (!(sample.order %in% valid.sample.order.values())) {
        stop(paste('Invalid "sample.order":', paste0('(', sample.order, ')')))
        }
    return(sample.order);
    }

sample.order.default <- function() 'none';
sample.order.increasing <- function() 'increasing';
sample.order.decreasing <- function() 'decreasing';
sample.order.auto.values <- function() {
    c(sample.order.increasing(), sample.order.decreasing());
    }
valid.sample.order.values <- function() {
    c(
        sample.order.default(),
        sample.order.auto.values()
        );
    }
