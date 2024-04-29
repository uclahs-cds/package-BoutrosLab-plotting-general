test_that(
    'prep.axis returns unchanged data with length > 1', {
        yat <- seq(0, 10, 2);
        result <- prep.axis(yat, 1:10, 'yat');
        
        expect_equal(result, yat);
        }
    );

test_that(
    'prep.axis returns unchanged boolean value', {
        xat <- TRUE;
        result <- prep.axis(xat, 1:10, 'xat');

        expect_equal(result, xat);
        }
    );

test_that(
    'prep.axis returns unchanged NULL value', {
        yat <- NULL;
        result <- prep.axis(yat, 1:10, 'yat');

        expect_equal(result, yat);
        }
    );

test_that(
    'prep.axis returns a list with "auto" setting', {
        result <- prep.axis('auto', 1:10, 'yat');
        expect_true(is.list(result));
        }
    );

test_that(
    'prep.axis sets $at using auto.axis with "auto" setting', {
        local({
            expected.at <- 1:10;
            local_mocked_bindings(auto.axis = function(...) list(at = expected.at));
            result <- prep.axis('auto', 1:10, 'yat');

            expect_equal(result$at, expected.at);
            });
        }
    );

test_that(
    'prep.axis sets $labels using auto.axis with "auto" setting', {
        local({
            expected.labels <- 1:5 * 2;
            local_mocked_bindings(auto.axis = function(...) list(labels = expected.labels));
            result <- prep.axis('auto', 1:10, 'yat');

            expect_equal(result$labels, expected.labels);
            });
        }
    );

test_that(
    'prep.axis sets $data using auto.axis with "auto" setting', {
        local({
            expected.data <- 11:20 ** 2;
            local_mocked_bindings(auto.axis = function(...) list(data = expected.data));
            result <- prep.axis('auto', 1:10, 'yat');

            expect_equal(result$data, expected.data);
            });
        }
    );

test_that(
    'prep.axis returns a list with "auto.linear" setting', {
        result <- prep.axis('auto.linear', 1:10, 'yat');
        expect_true(is.list(result));
        }
    );

test_that(
    'prep.axis sets $at using auto.axis with "auto.linear" setting', {
        local({
            expected.at <- 1:10;
            local_mocked_bindings(auto.axis = function(...) list(at = expected.at));
            result <- prep.axis('auto.linear', 1:10, 'yat');

            expect_equal(result$at, expected.at);
            });
        }
    );

test_that(
    'prep.axis sets $labels using auto.axis with "auto.linear" setting', {
        local({
            expected.labels <- 6:11 * 2;
            local_mocked_bindings(auto.axis = function(...) list(labels = expected.labels));
            result <- prep.axis('auto.linear', 1:10, 'yat');

            expect_equal(result$labels, expected.labels);
            });
        }
    );

test_that(
    'prep.axis sets $data using auto.axis with "auto.linear" setting', {
        local({
            expected.data <- 4:16 ** 2;
            local_mocked_bindings(auto.axis = function(...) list(data = expected.data));
            result <- prep.axis('auto.linear', 1:10, 'yat');

            expect_equal(result$data, expected.data);
            });
        }
    );

test_that(
    'prep.axis returns a list with "auto.log" setting', {
        result <- prep.axis('auto.log', 1:10, 'yat');
        expect_true(is.list(result));
        }
    );

test_that(
    'prep.axis sets $at using auto.axis with "auto.log" setting', {
        local({
            expected.at <- 1:10;
            local_mocked_bindings(auto.axis = function(...) list(at = expected.at));
            result <- prep.axis('auto.log', 1:10, 'yat');

            expect_equal(result$at, expected.at);
            });
        }
    );

test_that(
    'prep.axis sets $labels using auto.axis with "auto.log" setting', {
        local({
            expected.labels <- 1:10 * 10;
            local_mocked_bindings(auto.axis = function(...) list(labels = expected.labels));
            result <- prep.axis('auto.log', 1:10, 'yat');

            expect_equal(result$labels, expected.labels);
            });
        }
    );

test_that(
    'prep.axis sets $data using auto.axis with "auto.log" setting', {
        local({
            expected.data <- 1:6 ** 3;
            local_mocked_bindings(auto.axis = function(...) list(data = expected.data));
            result <- prep.axis('auto.log', 1:10, 'yat');

            expect_equal(result$data, expected.data);
            });
        }
    );

test_that(
    'prep.axis throws error on invalid setting', {
        expect_error(
            object = {
                prep.axis('invalid', 1:10, 'yat');
                },
            regexp = 'invalid'
            );
        }
    );
