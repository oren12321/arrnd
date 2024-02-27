#include <gtest/gtest.h>

#include <complex>

#include <oc/arrnd.h>
#include <oc/ext/linalg.h>

TEST(linalg_test, can_calculate_transposed_matrix)
{
    oc::arrnd<int> mat({1, 3, 2, 3}, {1, 1, 1, 2, 2, 2, 2, 2, 2, 4, 4, 4, 3, 3, 3, 6, 6, 6});
    oc::arrnd<int> rmat({1, 3, 3, 2}, {1, 2, 1, 2, 1, 2, 2, 4, 2, 4, 2, 4, 3, 6, 3, 6, 3, 6});

    EXPECT_TRUE(oc::all_equal(rmat, oc::linalg::transpose(mat)));

    using namespace std::complex_literals;

    oc::arrnd<std::complex<double>> cmat({3, 1, 2, 3},
        {1.0 + 1i, 1.0 + 1i, 1.0 + 1i, 2.0 + 1i, 2.0 + 1i, 2.0 + 1i, 2.0 + 1i, 2.0 + 1i, 2.0 + 1i, 4.0 + 1i, 4.0 + 1i,
            4.0 + 1i, 3.0 + 1i, 3.0 + 1i, 3.0 + 1i, 6.0 + 1i, 6.0 + 1i, 6.0 + 1i});
    oc::arrnd<std::complex<double>> rcmat({3, 1, 3, 2},
        {1.0 - 1i, 2.0 - 1i, 1.0 - 1i, 2.0 - 1i, 1.0 - 1i, 2.0 - 1i, 2.0 - 1i, 4.0 - 1i, 2.0 - 1i, 4.0 - 1i, 2.0 - 1i,
            4.0 - 1i, 3.0 - 1i, 6.0 - 1i, 3.0 - 1i, 6.0 - 1i, 3.0 - 1i, 6.0 - 1i});

    EXPECT_TRUE(oc::all_equal(rcmat, oc::linalg::transpose(cmat)));
}