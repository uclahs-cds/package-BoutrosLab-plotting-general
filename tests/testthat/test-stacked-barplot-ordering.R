test_that("stacked barplot sample ordering works correctly", {
    # Test data with known totals:
    # Sample A: group1=3, group2=1 -> total=4
    # Sample B: group1=5, group2=2 -> total=7  
    # Sample C: group1=1, group2=4 -> total=5
    # Expected order for increasing: A(4), C(5), B(7)
    # Expected order for decreasing: B(7), C(5), A(4)
    
    test.data <- data.frame(
        sample = rep(c("A", "B", "C"), each = 2),
        group = rep(c("group1", "group2"), 3),
        value = c(3, 1, 5, 2, 1, 4),
        stringsAsFactors = TRUE
    )
    
    # Test increasing order for stacked barplot
    expect_no_error({
        result.inc <- create.barplot(
            formula = value ~ sample,
            data = test.data,
            groups = test.data$group,
            stack = TRUE,
            sample.order = 'increasing',
            filename = NULL
        )
        
        # Verify the sample order is correct (A, C, B)
        expect_equal(result.inc$x.scales$labels, c("A", "C", "B"))
    })
    
    # Test decreasing order for stacked barplot
    expect_no_error({
        result.dec <- create.barplot(
            formula = value ~ sample,
            data = test.data,
            groups = test.data$group,
            stack = TRUE,
            sample.order = 'decreasing',
            filename = NULL
        )
        
        # Verify the sample order is correct (B, C, A)
        expect_equal(result.dec$x.scales$labels, c("B", "C", "A"))
    })
    
    # Test that grouped (non-stacked) barplots still work
    expect_no_error({
        result.grouped <- create.barplot(
            formula = value ~ sample,
            data = test.data,
            groups = test.data$group,
            stack = FALSE,
            sample.order = 'increasing',
            filename = NULL
        )
        
        # For grouped plots, ordering should work as before
        expect_true(length(result.grouped$x.scales$labels) == 3)
    })
})