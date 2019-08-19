
## test correctness ------------------------------------------------------------

vec_a = c(0, 1, 2, 0, 3, 4, 5, 1)
vec_b = c(1, 1, 0, 0, 2, 2, 2, 2)

tinytest::expect_equal(
    mtr_mutual_info_score(vec_a, vec_b),
    target = 0.693147180559945,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_normalized_mutual_info_score(vec_a, vec_b),
    # target = 0.5163977794943221,
    # changed test value due to respective example in sklearn.metrics is 
    # for version 0.21. Below value is compatible with version 0.22.
    target = 0.5,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_adjusted_mutual_info_score(vec_a, vec_b),
    # target = -0.10526315789473674,
    # changed test value due to respective example in sklearn.metrics is 
    # for version 0.21. Below value is compatible with version 0.22.
    target = -0.1666666667,
    tol = 1e-7
)
