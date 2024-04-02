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

test_that(
    'prep.sample.order errors on invalid string input',
    {
        sample.order <- 'invalid sample order setting';
        expect_error(
            {
                prep.sample.order(sample.order);
                },
            regexp = sample.order
            );
        }
    );

test_that(
    'prep.sample.order.setting errors with invalid setting',
    {
        sample.order <- 'invalid sample order setting';
        expect_error(
            {
                prep.sample.order.setting(sample.order);
                },
            regexp = sample.order
            );
        }
    );

test_that(
    'prep.sample.order.setting returns valid setting',
    {
        sample.order <- sample.order.default();
        result <- prep.sample.order.setting(sample.order);
        expect_equal(result, sample.order);
        }
    );
