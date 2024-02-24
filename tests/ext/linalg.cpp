#include <gtest/gtest.h>

#include <complex>

#include <oc/arrnd.h>
#include <oc/ext/linalg.h>

TEST(linalg_test, can_check_if_array_type_is_numeric_or_complex_at_compile_time)
{
    oc::arrnd<int>{}; // required due to an MSVC compiler issue
    oc::arrnd<double>{};

    static_assert(oc::linalg::numeric_arrnd_complient<oc::arrnd<int>>);
    static_assert(oc::linalg::numeric_arrnd_complient<oc::arrnd<double>>);

    oc::arrnd<std::complex<int>>{}; // required due to an MSVC compiler issue
    oc::arrnd<std::complex<double>>{};

    static_assert(oc::linalg::complex_arrnd_complient<oc::arrnd<std::complex<int>>>);
    static_assert(oc::linalg::complex_arrnd_complient<oc::arrnd<std::complex<double>>>);
}

TEST(linalg_test, can_calculate_transposed_matrix)
{
    oc::arrnd<int> mat({2, 3}, {1, 2, 3, 4, 5, 6});
    oc::arrnd<int> rmat({3, 2}, {1, 4, 2, 5, 3, 6});

    EXPECT_TRUE(oc::all_equal(rmat, oc::linalg::transpose(mat)));

    using namespace std::complex_literals;

    oc::arrnd<std::complex<double>> cmat({2, 2}, {-1i, 2.0 + 1i, 4.0 + 2i, -2i});
    oc::arrnd<std::complex<double>> rcmat({2, 2}, {1i, 4.0 - 2i, 2.0 - 1i, 2i});

    EXPECT_TRUE(oc::all_equal(rcmat, oc::linalg::transpose(cmat)));
}
