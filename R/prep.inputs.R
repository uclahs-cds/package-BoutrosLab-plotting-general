prep.sample.order <- function(sample.order) {
    contains.na <- any(is.na(sample.order));

    if (is.null(sample.order) || contains.na) {
        sample.order <- 'none';

        if (contains.na) {
            warning(paste(
                'NA values found in "sample.order" (using default "none" setting).'
                ));
            }
        }

    return(sample.order);
    }
