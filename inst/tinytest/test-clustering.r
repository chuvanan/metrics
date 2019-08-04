
## test correctness ------------------------------------------------------------

vec_a = c(0, 1, 2, 0, 3, 4, 5, 1)
vec_b = c(1, 1, 0, 0, 2, 2, 2, 2)
data(iris)

tinytest::expect_equal(
    mtr_mutual_info_score(vec_a, vec_b),
    target = 0.693147180559945,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_normalized_mutual_info_score(vec_a, vec_b),
    target = 0.5163977794943221,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_adjusted_mutual_info_score(vec_a, vec_b),
    target = -0.10526315789473674,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_adjusted_rand_score(vec_a, vec_b),
    target = -0.12903225806451613,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_homogeneity(vec_a, vec_b),
    target = 0.3999999999999998,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_completeness(vec_a, vec_b),
    target = 0.6666666666666665,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_v_measure(vec_a, vec_b),
    target = 0.4999999999999998,
    tol = 1e-7
)

tinytest::expect_equal(
    mtr_calinski_harabasz(iris[,-5], 
        predicted = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                      1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
                      2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
                      2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)),
    target = 560.3999242466402,
    tol = 1e-2 
    # numerical calculation outcome between python and R return large small 
    # difference, hence relatively large tolerance level
)

tinytest::expect_equal(
    mtr_davies_boulding_score(iris[,-5], 
                          predicted = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                        1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
                                        2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
                                        2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)),
    target = 0.6619715,
    tol = 1e-5
)

tinytest::expect_equal(
    mtr_mean_sihouette_coef(iris[,-5], 
                              predicted = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                            1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
                                            2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
                                            2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)),
    target = 0.5525919445213676,
    tol = 1e-3
    # numerical calculation outcome between python and R return large small 
    # difference, hence relatively large tolerance level
)


