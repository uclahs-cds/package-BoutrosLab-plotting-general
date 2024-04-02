test_that(
    'prep.sample.order replaces NULL with default',
    {
        sample.order <- NULL;
        result <- prep.sample.order(sample.order);

        expect_equal(result, sample.order.default());
        }
    );

test_that(
    'prep.sample.order replaces with default value if NAs are present',
    {
        sample.order <- c(NA, 'sample', 'order', NA);
        result <- prep.sample.order(sample.order);

        expect_equal(result, sample.order.default());
        }
    );

test_that(
    'prep.sample.order warns if NAs are present',
    {
        sample.order <- c(NA, 'sample', 'order', NA);
        
        expect_warning(
            { prep.sample.order(sample.order); },
            regexp = 'NA'
            );
        }
    );
