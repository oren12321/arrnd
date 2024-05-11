#include <gtest/gtest.h>

#include <cstdint>
#include <array>
#include <stdexcept>
#include <regex>
#include <string>
#include <span>
#include <ranges>
#include <ostream>
#include <charconv>
#include <complex>
#include <random>
#include <thread>

#include <oc/arrnd.h>

TEST(arrnd_test, basic_math_and_trigo)
{
    using namespace oc;

    arrnd<double> arr1({1, 5}, {0.1, 0.2, 0.3, 0.4, 0.5});
    arrnd<double> arr2({1, 5}, {1, 2, 3, 4, 5});

    EXPECT_TRUE(all_close(arrnd<double>({1, 5}, {0.1, 0.2, 0.3, 0.4, 0.5}), abs(arr1)));

    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::acos(0.1), std::acos(0.2), std::acos(0.3), std::acos(0.4), std::acos(0.5)}),
        acos(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::asin(0.1), std::asin(0.2), std::asin(0.3), std::asin(0.4), std::asin(0.5)}),
        asin(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::atan(0.1), std::atan(0.2), std::atan(0.3), std::atan(0.4), std::atan(0.5)}),
        atan(arr1)));

    EXPECT_TRUE(
        all_close(arrnd<double>({1, 5}, {std::acosh(1), std::acosh(2), std::acosh(3), std::acosh(4), std::acosh(5)}),
            acosh(arr2)));
    EXPECT_TRUE(
        all_close(arrnd<double>({1, 5}, {std::asinh(1), std::asinh(2), std::asinh(3), std::asinh(4), std::asinh(5)}),
            asinh(arr2)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::atanh(0.1), std::atanh(0.2), std::atanh(0.3), std::atanh(0.4), std::atanh(0.5)}),
        atanh(arr1)));

    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::cos(0.1), std::cos(0.2), std::cos(0.3), std::cos(0.4), std::cos(0.5)}), cos(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::sin(0.1), std::sin(0.2), std::sin(0.3), std::sin(0.4), std::sin(0.5)}), sin(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::tan(0.1), std::tan(0.2), std::tan(0.3), std::tan(0.4), std::tan(0.5)}), tan(arr1)));

    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::cosh(0.1), std::cosh(0.2), std::cosh(0.3), std::cosh(0.4), std::cosh(0.5)}),
        cosh(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::sinh(0.1), std::sinh(0.2), std::sinh(0.3), std::sinh(0.4), std::sinh(0.5)}),
        sinh(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::tanh(0.1), std::tanh(0.2), std::tanh(0.3), std::tanh(0.4), std::tanh(0.5)}),
        tanh(arr1)));

    EXPECT_TRUE(
        all_close(arrnd<double>({1, 5}, {std::exp(1), std::exp(2), std::exp(3), std::exp(4), std::exp(5)}), exp(arr2)));
    EXPECT_TRUE(
        all_close(arrnd<double>({1, 5}, {std::log(1), std::log(2), std::log(3), std::log(4), std::log(5)}), log(arr2)));
    EXPECT_TRUE(
        all_close(arrnd<double>({1, 5}, {std::log10(1), std::log10(2), std::log10(3), std::log10(4), std::log10(5)}),
            log10(arr2)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::sqrt(1), std::sqrt(2), std::sqrt(3), std::sqrt(4), std::sqrt(5)}), sqrt(arr2)));

    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::pow(1, 2), std::pow(2, 2), std::pow(3, 2), std::pow(4, 2), std::pow(5, 2)}),
        pow(arr2, 2)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::pow(1, 1), std::pow(2, 2), std::pow(3, 3), std::pow(4, 4), std::pow(5, 5)}),
        pow(arr2, arr2)));

    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::round(0.1), std::round(0.2), std::round(0.3), std::round(0.4), std::round(0.5)}),
        round(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::ceil(0.1), std::ceil(0.2), std::ceil(0.3), std::ceil(0.4), std::ceil(0.5)}),
        ceil(arr1)));
    EXPECT_TRUE(all_close(
        arrnd<double>({1, 5}, {std::floor(0.1), std::floor(0.2), std::floor(0.3), std::floor(0.4), std::floor(0.5)}),
        floor(arr1)));

    using namespace std::complex_literals;
    using comparrnd = arrnd<std::complex<double>>;

    EXPECT_TRUE(
        all_equal(real(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::real(3.0 + 2i), std::real(6.0)})));
    EXPECT_TRUE(
        all_equal(imag(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::imag(3.0 + 2i), std::imag(6.0)})));
    EXPECT_TRUE(
        all_equal(arg(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::arg(3.0 + 2i), std::arg(6.0)})));
    EXPECT_TRUE(
        all_equal(norm(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::norm(3.0 + 2i), std::norm(6.0)})));
    EXPECT_TRUE(
        all_equal(conj(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::conj(3.0 + 2i), std::conj(6.0)})));
    EXPECT_TRUE(
        all_equal(proj(comparrnd({1, 2}, {3.0 + 2i, 6.0})), comparrnd({1, 2}, {std::proj(3.0 + 2i), std::proj(6.0)})));

    EXPECT_TRUE(
        all_equal(polar(arrnd<double>({1, 2}, {3.0, 6.0})), comparrnd({1, 2}, {std::polar(3.0), std::polar(6.0)})));
    EXPECT_TRUE(all_equal(polar(arrnd<double>({1, 2}, {3.0, 6.0}), arrnd<double>({1, 2}, {0.3, 0.6})),
        comparrnd({1, 2}, {std::polar(3.0, 0.3), std::polar(6.0, 0.6)})));

    EXPECT_TRUE(
        all_equal(sign(arrnd<double>({1, 5}, {-1.5, -1, 0.0, 1, 1.5})), arrnd<double>({1, 5}, {-1, -1, 0, 1, 1})));
}

TEST(arrnd_test, filter_elements_by_condition)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 0, 5, 6};
    oc::arrnd iarr{dims, idata};

    const int rdata0[]{1, 2, 3, 0, 5, 6};
    oc::arrnd rarr0{{6}, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::filter(iarr, [](int) {
        return 1;
    })));

    const int rdata1[]{1, 2, 3, 5, 6};
    oc::arrnd rarr1{{5}, rdata1};
    EXPECT_TRUE(oc::all_equal(rarr1, oc::filter(iarr, [](int a) {
        return a;
    })));

    const double rdata2[]{2.0, 0.0, 6.0};
    oc::arrnd rarr2{{3}, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::filter(iarr, [](int a) {
        return a % 2 == 0;
    })));

    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(iarr, [](int a) {
        return a > 6;
    })));
    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(oc::arrnd<int>{}, [](int) {
        return 1;
    })));

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({2, 2}, {7, 8, 9, 10})});

        auto r1 = oc::filter(inarr, [](int a) {
            return a % 2 == 0;
        });
        EXPECT_TRUE(oc::all_equal(
            r1, oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({3}, {2, 4, 6}), oc::arrnd<int>({2}, {8, 10})})));

        auto r2 = oc::filter<0>(inarr, [](const auto& a) {
            return std::reduce(a.cbegin(), a.cend(), 0, std::plus<>{}) > 25;
        });
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({2, 2}, {7, 8, 9, 10})})));
    }
}

TEST(arrnd_test, filter_elements_by_maks)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 4, 5, 6};
    oc::arrnd iarr{dims, idata};

    //EXPECT_TRUE(oc::empty(oc::filter(iarr, oc::arrnd<int>{}))); // assertion failure

    const bool imask_data0[]{1, 0, 0, 1, 0, 1};
    oc::arrnd imask0{dims, imask_data0};
    const int rdata0[]{1, 4, 6};
    oc::arrnd rarr0{{3}, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::filter(iarr, imask0)));

    const bool imask_data1[]{0, 0, 0, 0, 0, 0};
    oc::arrnd imask1{dims, imask_data1};
    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(iarr, imask1)));

    const bool imask_data2[]{1, 1, 1, 1, 1, 1};
    oc::arrnd imask2{dims, imask_data2};
    const int rdata2[]{1, 2, 3, 4, 5, 6};
    oc::arrnd rarr2{{6}, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::filter(iarr, imask2)));

    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(oc::arrnd<int>{}, imask0)));

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({3, 1, 2}, {7, 8, 9, 10, 11, 12})});

        auto r1 = oc::filter(inarr, oc::arrnd<bool>({3, 1, 2}, {0, 0, 1, 0, 0, 1}));
        EXPECT_TRUE(oc::all_equal(
            r1, oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({2}, {3, 6}), oc::arrnd<int>({2}, {9, 12})})));

        auto r2 = oc::filter<0>(inarr, oc::arrnd<bool>({1, 2}, {1, 0}));
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})})));
    }
}

TEST(arrnd_test, filter_elements_by_indices)
{
    using namespace oc;

    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 4, 5, 6};
    arrnd iarr{dims, idata};

    //EXPECT_TRUE(oc::empty(oc::filter(iarr, oc::arrnd<int>{}))); // assertion failure

    EXPECT_TRUE(all_equal(filter(iarr, {0, 4}), arrnd<int>({2}, {1, 5})));

    EXPECT_TRUE(all_equal(filter(iarr, arrnd<int>({1, 2}, {0, 4})), arrnd<int>({1, 2}, {1, 5})));
}

TEST(arrnd_test, select_elements_indices_by_condition)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 0, 5, 6};
    oc::arrnd iarr{dims, idata};

    const std::int64_t rdata0[]{0, 1, 2, 3, 4, 5};
    oc::arrnd rarr0{{6}, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::find(iarr, [](int) {
        return 1;
    })));

    const std::int64_t rdata1[]{0, 1, 2, 4, 5};
    oc::arrnd rarr1{{5}, rdata1};
    EXPECT_TRUE(oc::all_equal(rarr1, oc::find(iarr, [](int a) {
        return a;
    })));

    const std::int64_t rdata2[]{1, 3, 5};
    oc::arrnd rarr2{{3}, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::find(iarr, [](int a) {
        return a % 2 == 0;
    })));

    EXPECT_TRUE(oc::all_equal(oc::arrnd<std::int64_t>{}, oc::find(iarr, [](int a) {
        return a > 6;
    })));
    EXPECT_TRUE(oc::all_equal(oc::arrnd<std::int64_t>{}, oc::find(oc::arrnd<int>{}, [](int) {
        return 1;
    })));

    // subarray
    const std::int64_t rdatas[]{2};
    oc::arrnd rarrs{{1}, rdatas};
    EXPECT_TRUE(oc::all_equal(rarrs, oc::find((iarr[{{1, 2}}]), [](int a) {
        return a;
    })));

    // Get subarray, find values indices by predicate,
    // and use this indices in different array.
    {
        oc::arrnd sarr{iarr[{{1, 3}, {0, 1}, {0, 2}}]};
        oc::arrnd not_zeros_inds{oc::find(sarr, [](int a) {
            return a != 0;
        })};

        oc::arrnd<std::int64_t> rinds1{{3}, {2, 4, 5}};
        EXPECT_TRUE(oc::all_equal(rinds1, not_zeros_inds));

        oc::arrnd<std::int64_t> rvals1{{3}, {12, 14, 15}}; // deprecated

        oc::arrnd rallvals1{{3, 1, 2}, {10, 11, 12, 13, 14, 15}};
        EXPECT_TRUE(oc::all_equal(rvals1, static_cast<decltype(rallvals1)>(rallvals1(not_zeros_inds))));
    }

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({2, 2}, {7, 8, 9, 10})});

        auto r1 = oc::find(inarr, [](int a) {
            return a % 2 == 0;
        });
        EXPECT_TRUE(oc::all_equal(
            r1, oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({3}, {1, 3, 5}), oc::arrnd<int>({2}, {1, 3})})));

        auto r2 = oc::find<0>(inarr, [](const auto& a) {
            return std::reduce(a.cbegin(), a.cend(), 0, std::plus<>{}) > 25;
        });
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<int>({1}, {1})));
    }
}

TEST(arrnd_test, select_elements_indices_by_maks)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 4, 5, 6};
    oc::arrnd iarr{dims, idata};

    //EXPECT_TRUE(oc::empty(oc::find(iarr, oc::arrnd<int>{}))); // assertion failure

    const int imask_data0[]{1, 0, 0, 1, 0, 1};
    oc::arrnd imask0{dims, imask_data0};
    const std::int64_t rdata0[]{0, 3, 5};
    oc::arrnd rarr0{{3}, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::find(iarr, imask0)));

    const int imask_data1[]{0, 0, 0, 0, 0, 0};
    oc::arrnd imask1{dims, imask_data1};
    EXPECT_TRUE(oc::all_equal(oc::arrnd<std::int64_t>{}, oc::find(iarr, imask1)));

    const int imask_data2[]{1, 1, 1, 1, 1, 1};
    oc::arrnd imask2{dims, imask_data2};
    const std::int64_t rdata2[]{0, 1, 2, 3, 4, 5};
    oc::arrnd rarr2{{6}, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::find(iarr, imask2)));

    EXPECT_TRUE(oc::all_equal(oc::arrnd<std::int64_t>{}, oc::find(oc::arrnd<std::int64_t>{}, imask0)));

    // Get subarray, find values indices by predicate,
    // and use this indices in different array.
    {
        oc::arrnd sarr{iarr[{{1, 3}, {0, 1}, {0, 2}}]};
        oc::arrnd not_zeros_inds{oc::find(sarr, oc::arrnd{{2, 1, 2}, {0, 1, 0, 1}})};

        oc::arrnd<std::int64_t> rinds1{{2}, {3, 5}};
        EXPECT_TRUE(oc::all_equal(rinds1, not_zeros_inds));

        oc::arrnd<std::int64_t> rvals1{{2}, {13, 15}}; // deprecated

        oc::arrnd rallvals1{{3, 1, 2}, {10, 11, 12, 13, 14, 15}};
        EXPECT_TRUE(oc::all_equal(rvals1, rallvals1(not_zeros_inds)()));
    }

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({3, 1, 2}, {7, 8, 9, 10, 11, 12})});

        auto r1 = oc::find(inarr, oc::arrnd<int>({3, 1, 2}, {0, 0, 1, 0, 0, 1}));
        EXPECT_TRUE(oc::all_equal(
            r1, oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({2}, {2, 5}), oc::arrnd<int>({2}, {2, 5})})));

        auto r2 = oc::find<0>(inarr, oc::arrnd<int>({1, 2}, {1, 0}));
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<int>({1}, {0})));
    }
}

TEST(arrnd_test, callable_operator)
{
    using namespace oc;

    arrnd arr({3, 2, 2}, {5, 7, 10, 2, 8, 6, 1, 9, 0, 3, 11, 4});

    {
        auto r = arr({0, 5, 3, 2})();
        EXPECT_TRUE(all_equal(r, arrnd({4}, {5, 6, 2, 10})));
    }

    {
        auto r = arr(arr <= 5)();
        EXPECT_TRUE(all_equal(r, arrnd({6}, {5, 2, 1, 0, 3, 4})));
    }

    {
        auto r = arr(
            [](int a, int factor) {
                return a <= 5 - factor;
            },
            1)();
        EXPECT_TRUE(all_equal(r, arrnd({5}, {2, 1, 0, 3, 4})));
    }

    {
        EXPECT_TRUE(all_equal(arr(arrnd_shape_preset::vector), arrnd({12}, {5, 7, 10, 2, 8, 6, 1, 9, 0, 3, 11, 4})));

        EXPECT_TRUE(all_equal(arr(arrnd_shape_preset::row), arrnd({1, 12}, {5, 7, 10, 2, 8, 6, 1, 9, 0, 3, 11, 4})));
        EXPECT_TRUE(all_equal(arr(arrnd_shape_preset::column), arrnd({12, 1}, {5, 7, 10, 2, 8, 6, 1, 9, 0, 3, 11, 4})));
    }
}

TEST(arrnd_test, transpose)
{
    const std::int64_t idims[]{4, 2, 3, 2};
    const int idata[]{1, 2, 3, 4, 5, 6,

        7, 8, 9, 10, 11, 12,

        13, 14, 15, 16, 17, 18,

        19, 20, 21, 22, 23, 24,

        25, 26, 27, 28, 29, 30,

        31, 32, 33, 34, 35, 36,

        37, 38, 39, 40, 41, 42,

        43, 44, 45, 46, 47, 48};
    oc::arrnd iarr{idims, idata};

    const std::int64_t rdims[]{3, 4, 2, 2};
    const double rdata[]{1.0, 2.0, 7.0, 8.0,

        13.0, 14.0, 19.0, 20.0,

        25.0, 26.0, 31.0, 32.0,

        37.0, 38.0, 43.0, 44.0,

        3.0, 4.0, 9.0, 10.0,

        15.0, 16.0, 21.0, 22.0,

        27.0, 28.0, 33.0, 34.0,

        39.0, 40.0, 45.0, 46.0,

        5.0, 6.0, 11.0, 12.0,

        17.0, 18.0, 23.0, 24.0,

        29.0, 30.0, 35.0, 36.0,

        41.0, 42.0, 47.0, 48.0};
    oc::arrnd rarr{rdims, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, oc::transpose(iarr, {2, 0, 1, 3})));

    //EXPECT_TRUE(oc::all_equal(rarr, oc::transpose(iarr, { 2, 0, 1, 3, 2 }))); // assertion failure
    //EXPECT_TRUE(oc::empty(oc::transpose(iarr, { 2, 0, 1, 4 }))); // assertion failure

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({1, 6}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({1, 4}, {1, 2, 3, 4})});

        auto r1 = oc::transpose(inarr, {1, 0});
        EXPECT_TRUE(oc::all_equal(r1,
            oc::arrnd<oc::arrnd<int>>(
                {1, 2}, {oc::arrnd<int>({6, 1}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({4, 1}, {1, 2, 3, 4})})));

        auto r2 = oc::transpose<0>(inarr, {1, 0});
        EXPECT_TRUE(oc::all_equal(r2,
            oc::arrnd<oc::arrnd<int>>(
                {2, 1}, {oc::arrnd<int>({1, 6}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({1, 4}, {1, 2, 3, 4})})));
    }
}

//TEST(arrnd_test, nest)
//{
//    using namespace oc;
//
//    arrnd<int> iarr({10, 5, 6, 4});
//    std::iota(iarr.begin(), iarr.end(), 1);
//
//    auto sarr = iarr[{interval<>::between(4, 9, 2), interval<>::at(2), interval<>::between(0, 2), interval<>::at(2)}];
//    EXPECT_TRUE(all_equal(sarr, arrnd<int>({3, 1, 2, 1}, {531, 535, 771, 775, 1011, 1015})));
//
//    auto rarr = nest<2>(sarr);
//    EXPECT_TRUE(all_equal(rarr,
//        arrnd<arrnd<arrnd<int>>>({3},
//            {arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {531, 535})}),
//                arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {771, 775})}),
//                arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {1011, 1015})})})));
//
//    auto ind535 = find(iarr, [](int a) {
//        return a == 535;
//    })[0];
//    EXPECT_EQ(535, iarr[ind535]);
//    EXPECT_EQ(535, (rarr[{0}][{0}][{1, 0}]));
//
//    iarr[ind535] = 0;
//    EXPECT_EQ(0, iarr[ind535]);
//    EXPECT_EQ(0, (rarr[{0}][{0}][{1, 0}]));
//}

TEST(arrnd_test, equal)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {true, true, true, false, true, false};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 == arr2));
    //EXPECT_TRUE(oc::empty(arr1 == Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, not_equal)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {false, false, false, true, false, true};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 != arr2));
    //EXPECT_TRUE(oc::empty(arr1 != Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, greater)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {false, false, false, false, false, false};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 > arr2));
    EXPECT_TRUE(oc::all_equal(rarr, arr1 > 6));
    EXPECT_TRUE(oc::all_equal(rarr, 0 > arr1));
    //EXPECT_TRUE(oc::empty(arr1 > Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, greater_equal)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {true, true, true, false, true, false};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 >= arr2));
    EXPECT_TRUE(oc::all_equal(rarr, arr1 >= 1));

    const bool rdata2[] = {true, true, true, true, true, false};
    oc::arrnd<bool> rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, 5 >= arr2));

    //EXPECT_TRUE(oc::empty(arr1 >= Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, less)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {false, false, false, true, false, true};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 < arr2));
    EXPECT_TRUE(oc::all_equal(rarr, arr1 < 1));

    const bool rdata2[] = {false, true, true, true, true, true};
    oc::arrnd<bool> rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, 1 < arr2));

    //EXPECT_TRUE(oc::empty(arr1 < Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, less_equal)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {true, true, true, true, true, true};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, arr1 <= arr2));
    EXPECT_TRUE(oc::all_equal(rarr, arr1 <= 5));
    EXPECT_TRUE(oc::all_equal(rarr, 0 <= arr1));
    //EXPECT_TRUE(oc::empty(arr1 <= Integer_array{{1}})); // assertion failure
}

TEST(arrnd_test, close)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 1, 1, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const bool rdata[] = {true, true, true, false, true, false};
    oc::arrnd<bool> rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, oc::close(arr1, arr2, 2)));
    EXPECT_TRUE(oc::all_equal(rarr, oc::close(arr1, 3, 2)));
    EXPECT_TRUE(oc::all_equal(rarr, oc::close(3, arr1, 2)));
    //EXPECT_TRUE(oc::empty(oc::close(arr1, Integer_array{{1}}))); // assertion failure
}

TEST(arrnd_test, plus)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {2, 4, 6, 4, 10, 6};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 + arr2));
    arr1 += arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 + Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 += Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {11, 12, 13, 14, 15, 16};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 + 10));
    EXPECT_TRUE(oc::all_equal(rarr2, 10 + arr2));
    arr2 += 10;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, minus)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 0, 0, -4, 0, -6};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 - arr2));
    arr1 -= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 - Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 -= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0, 1, 2, 3, 4, 5};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 - 1));

    const int rdata3[] = {0, -1, -2, -3, -4, -5};
    Integer_array rarr3{{3, 1, 2}, rdata3};

    EXPECT_TRUE(oc::all_equal(rarr3, 1 - arr2));
    arr2 -= 1;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, multiply)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {1, 4, 9, 0, 25, 0};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 * arr2));
    arr1 *= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 * Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 *= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {10, 20, 30, 40, 50, 60};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 * 10));
    EXPECT_TRUE(oc::all_equal(rarr2, 10 * arr2));
    arr2 *= 10;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, divide)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {1, 1, 1, 0, 1, 0};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 / arr2));
    arr1 /= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 / Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 /= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0, 1, 1, 2, 2, 3};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 / 2));

    const int rdata3[] = {2, 1, 0, 0, 0, 0};
    Integer_array rarr3{{3, 1, 2}, rdata3};

    EXPECT_TRUE(oc::all_equal(rarr3, 2 / arr2));
    arr2 /= 2;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, modulu)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 0, 5, 0};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {1, 2, 3, 4, 5, 6};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 0, 0, 0, 0, 0};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 % arr2));
    arr1 %= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 % Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 %= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {1, 0, 1, 0, 1, 0};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 % 2));

    const int rdata3[] = {0, 0, 2, 2, 2, 2};
    Integer_array rarr3{{3, 1, 2}, rdata3};

    EXPECT_TRUE(oc::all_equal(rarr3, 2 % arr2));
    arr2 %= 2;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, xor)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0b000, 0b001, 0b010, 0b011, 0b100, 0b101};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0b000, 0b001, 0b010, 0b000, 0b100, 0b000};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0b000, 0b000, 0b000, 0b011, 0b000, 0b101};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 ^ arr2));
    arr1 ^= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 ^ Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 ^= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0b111, 0b110, 0b101, 0b111, 0b011, 0b111};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 ^ 0b111));
    EXPECT_TRUE(oc::all_equal(rarr2, 0b111 ^ arr2));
    arr2 ^= 0b111;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, and)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0b000, 0b001, 0b010, 0b011, 0b100, 0b101};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0b000, 0b001, 0b010, 0b000, 0b100, 0b000};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0b000, 0b001, 0b010, 0b000, 0b100, 0b000};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 & arr2));
    arr1 &= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 & Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 &= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0b000, 0b001, 0b010, 0b000, 0b100, 0b000};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 & 0b111));
    EXPECT_TRUE(oc::all_equal(rarr2, 0b111 & arr2));
    arr2 &= 0b111;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, or)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0b000, 0b001, 0b010, 0b011, 0b100, 0b101};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0b000, 0b001, 0b010, 0b000, 0b100, 0b000};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0b000, 0b001, 0b010, 0b011, 0b100, 0b101};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 | arr2));
    arr1 |= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 | Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 |= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0b111, 0b111, 0b111, 0b111, 0b111, 0b111};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 | 0b111));
    EXPECT_TRUE(oc::all_equal(rarr2, 0b111 | arr2));
    arr2 |= 0b111;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, shift_left)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 2, 8, 24, 64, 160};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 << arr2));
    arr1 <<= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 << Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 <<= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0, 4, 8, 12, 16, 20};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 << 2));

    const int rdata3[] = {2, 4, 8, 16, 32, 64};
    Integer_array rarr3{{3, 1, 2}, rdata3};

    EXPECT_TRUE(oc::all_equal(rarr3, 2 << arr2));
    arr2 <<= 2;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, shift_right)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 0, 0, 0, 0, 0};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 >> arr2));
    arr1 >>= arr2;
    EXPECT_TRUE(oc::all_equal(rarr1, arr1));

    //EXPECT_TRUE(oc::empty(arr1 >> Integer_array{{1}})); // assertion failure
    //EXPECT_TRUE(oc::all_equal(arr1 >>= Integer_array{{1}}, arr1)); // assertion failure

    const int rdata2[] = {0, 0, 0, 0, 1, 1};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 >> 2));

    const int rdata3[] = {2, 1, 0, 0, 0, 0};
    Integer_array rarr3{{3, 1, 2}, rdata3};

    EXPECT_TRUE(oc::all_equal(rarr3, 2 >> arr2));
    arr2 >>= 2;
    EXPECT_TRUE(oc::all_equal(rarr2, arr2));
}

TEST(arrnd_test, bitwise_not)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    const int rdata[] = {-1, -2, -3, -4, -5, -6};
    Integer_array rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, ~arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, ~Integer_array{}));
}

TEST(arrnd_test, logic_not)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    const int rdata[] = {1, 0, 0, 0, 0, 0};
    Integer_array rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, !arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, !Integer_array{}));
}

TEST(arrnd_test, positive)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    EXPECT_TRUE(oc::all_equal(arr, +arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, +Integer_array{}));
}

TEST(arrnd_test, negation)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    const int rdata[] = {0, -1, -2, -3, -4, -5};
    Integer_array rarr{{3, 1, 2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, -arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, -Integer_array{}));
}

TEST(arrnd_test, logic_and)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0, 1, 2, 0, 3, 0};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 1, 1, 0, 1, 0};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 && arr2));

    //EXPECT_TRUE(oc::empty(arr1 && Integer_array{{1}})); // assertion failure

    const int rdata2[] = {0, 1, 1, 0, 1, 0};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 && 1));
    EXPECT_TRUE(oc::all_equal(rarr2, 1 && arr2));
}

TEST(arrnd_test, logic_or)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr1{{3, 1, 2}, data1};

    const int data2[] = {0, 1, 2, 0, 3, 0};
    Integer_array arr2{{3, 1, 2}, data2};

    const int rdata1[] = {0, 1, 1, 1, 1, 1};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    EXPECT_TRUE(oc::all_equal(rarr1, arr1 || arr2));

    //EXPECT_TRUE(oc::empty(arr1 || Integer_array{{1}})); // assertion failure

    const int rdata2[] = {1, 1, 1, 1, 1, 1};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    EXPECT_TRUE(oc::all_equal(rarr2, arr2 || 1));
    EXPECT_TRUE(oc::all_equal(rarr2, 1 || arr2));
}

TEST(arrnd_test, increment)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    const int rdata1[] = {0, 1, 2, 3, 4, 5};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    const int rdata2[] = {1, 2, 3, 4, 5, 6};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    Integer_array old{arr++};

    EXPECT_TRUE(oc::all_equal(rarr1, old));
    EXPECT_TRUE(oc::all_equal(rarr2, arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, ++Integer_array{}));
    EXPECT_TRUE(oc::all_equal(arr, ++(Integer_array{{3, 1, 2}, data})));
    EXPECT_TRUE(oc::all_equal((Integer_array{{3, 1, 2}, data}), (Integer_array{{3, 1, 2}, data})++));
}

TEST(arrnd_test, decrement)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 1, 2, 3, 4, 5};
    Integer_array arr{{3, 1, 2}, data};

    const int rdata1[] = {0, 1, 2, 3, 4, 5};
    Integer_array rarr1{{3, 1, 2}, rdata1};

    const int rdata2[] = {-1, 0, 1, 2, 3, 4};
    Integer_array rarr2{{3, 1, 2}, rdata2};

    Integer_array old{arr--};

    EXPECT_TRUE(oc::all_equal(rarr1, old));
    EXPECT_TRUE(oc::all_equal(rarr2, arr));
    EXPECT_TRUE(oc::all_equal(Integer_array{}, --Integer_array{}));
    EXPECT_TRUE(oc::all_equal(arr, --(Integer_array{{3, 1, 2}, data})));
    EXPECT_TRUE(oc::all_equal((Integer_array{{3, 1, 2}, data}), (Integer_array{{3, 1, 2}, data})--));
}

TEST(arrnd_test, can_be_all_matched_with_another_array_or_value)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 4, 5, 6};
    std::int64_t dims1[]{3, 1, 2};
    Integer_array arr1{dims1, data1};
    Integer_array arr2{dims1, data1};

    EXPECT_TRUE(oc::all_match(arr1, arr2, [](int a, int b) {
        return a / b == 1;
    }));

    std::int64_t dims2[]{3, 2};
    Integer_array arr3{dims2, data1};

    EXPECT_FALSE(oc::all_match(arr1, arr3, [](int, int) {
        return true;
    }));

    const int data2[] = {1, 2, 3, 4, 5, 5};
    Integer_array arr4{dims1, data2};
    Integer_array arr5{dims2, data2};

    EXPECT_FALSE(oc::all_match(arr1, arr4, [](int a, int b) {
        return a == b;
    }));
    EXPECT_FALSE(oc::all_match(arr1, arr5, [](int a, int b) {
        return a == b;
    }));

    {
        using Integer_array = oc::arrnd<int>;

        const int data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9,

            10, 11, 12, 13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24, 25, 26, 27,

            28, 29, 30, 31, 32, 33, 34, 35, 36};
        const std::int64_t dims[]{2, 2, 3, 3};
        Integer_array arr{dims, data};

        const int rdata[] = {11, 14,

            29, 32};
        const std::int64_t rdims[]{2, 1, 2, 1};
        Integer_array rarr{rdims, rdata};

        Integer_array sarr{arr[{{0, 2}, {1, 2}, {0, 2}, {1, 3, 2}}]};
        EXPECT_TRUE(oc::all_equal(rarr, sarr));
        EXPECT_TRUE(oc::all_match(rarr, sarr, [](int a, int b) {
            return a == b;
        }));
    }

    // nested array
    {
        oc::arrnd<Integer_array> narr1({1}, {arr1});
        oc::arrnd<Integer_array> narr4({1}, {arr4});

        EXPECT_TRUE(oc::all_match(narr1, narr4, [](int a, int b) {
            return a >= b;
        }));
        EXPECT_FALSE(oc::all_match(narr1, narr4, [](int a, int b) {
            return a > b;
        }));
        EXPECT_TRUE(oc::all_match(
            narr1,
            [](int a, int b) {
                return a >= b;
            },
            1));
        EXPECT_FALSE(oc::all_match(
            narr1,
            [](int a, int b) {
                return a <= b;
            },
            1));
    }

    // different ND array types
    {
        const double data1d[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
        std::int64_t dims1d[]{3, 1, 2};
        oc::arrnd<double> arr1d{dims1d, data1d};

        EXPECT_TRUE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::all_match(arr1, arr1d, [](int a, int b) {
            return a == b;
        }));

        arr1d[{0, 0, 0}] = 1.001;
        EXPECT_FALSE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::all_match(arr1, arr1d, [](int a, int b) {
            return a == b;
        }));
    }

    // empty arrays
    {
        EXPECT_TRUE(oc::all_match(Integer_array{}, Integer_array{}, [](int, int) {
            return false;
        }));
        EXPECT_TRUE(oc::all_match(Integer_array{}, Integer_array({}), [](int, int) {
            return false;
        }));
        EXPECT_TRUE(oc::all_match(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0), [](int, int) {
            return false;
        }));
    }

    // scalar
    {
        EXPECT_TRUE(oc::all_match(
            arr1,
            [](int a, int b) {
                return a * b == a;
            },
            1));
        EXPECT_FALSE(oc::all_match(
            arr2,
            [](int a, int b) {
                return a * b == a;
            },
            2));
    }
}

TEST(arrnd_test, can_be_any_matched_with_another_array_or_value)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 4, 5, 6};
    std::int64_t dims1[]{3, 1, 2};
    Integer_array arr1{dims1, data1};
    Integer_array arr2{dims1, data1};

    EXPECT_TRUE(oc::any_match(arr1, arr2, [](int a, int b) {
        return a / b == 1;
    }));

    std::int64_t dims2[]{3, 2};
    Integer_array arr3{dims2, data1};

    EXPECT_FALSE(oc::any_match(arr1, arr3, [](int, int) {
        return true;
    }));

    const int data2[] = {1, 2, 3, 4, 5, 5};
    Integer_array arr4{dims1, data2};
    Integer_array arr5{dims2, data2};

    EXPECT_TRUE(oc::any_match(arr1, arr4, [](int a, int b) {
        return a == b;
    }));
    EXPECT_FALSE(oc::any_match(arr1, arr5, [](int a, int b) {
        return a == b;
    }));

    {
        using Integer_array = oc::arrnd<int>;

        const int data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9,

            10, 11, 12, 13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24, 25, 26, 27,

            28, 29, 30, 31, 32, 33, 34, 35, 36};
        const std::int64_t dims[]{2, 2, 3, 3};
        Integer_array arr{dims, data};

        const int rdata[] = {11, 15,

            29, 32};
        const std::int64_t rdims[]{2, 1, 2, 1};
        Integer_array rarr{rdims, rdata};

        Integer_array sarr{arr[{{0, 2}, {1, 2}, {0, 2}, {1, 3, 2}}]};
        EXPECT_FALSE(oc::all_equal(rarr, sarr));
        EXPECT_TRUE(oc::any_match(rarr, sarr, [](int a, int b) {
            return a == b;
        }));
    }

    // nested array
    {
        oc::arrnd<Integer_array> narr1({1}, {arr1});
        oc::arrnd<Integer_array> narr4({1}, {arr4});

        EXPECT_TRUE(oc::any_match(narr1, narr4, [](int a, int b) {
            return a >= b;
        }));
        EXPECT_TRUE(oc::any_match(narr1, narr4, [](int a, int b) {
            return a > b;
        }));
        EXPECT_TRUE(oc::any_match(
            narr1,
            [](int a, int b) {
                return a >= b;
            },
            1));
        EXPECT_TRUE(oc::any_match(
            narr1,
            [](int a, int b) {
                return a <= b;
            },
            1));
    }

    // different ND array types
    {
        const double data1d[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
        std::int64_t dims1d[]{3, 1, 2};
        oc::arrnd<double> arr1d{dims1d, data1d};

        EXPECT_TRUE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::any_match(arr1, arr1d, [](int a, int b) {
            return a == b;
        }));

        arr1d[{0, 0, 0}] = 1.001;
        EXPECT_FALSE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::any_match(arr1, arr1d, [](int a, int b) {
            return a == b;
        }));
    }

    // empty arrays
    {
        EXPECT_TRUE(oc::any_match(Integer_array{}, Integer_array{}, [](int, int) {
            return false;
        }));
        EXPECT_TRUE(oc::any_match(Integer_array{}, Integer_array({}), [](int, int) {
            return false;
        }));
        EXPECT_TRUE(oc::any_match(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0), [](int, int) {
            return false;
        }));
    }

    // scalar
    {
        EXPECT_TRUE(oc::any_match(
            arr1,
            [](int a, int b) {
                return a * b == a;
            },
            1));
        EXPECT_TRUE(oc::any_match(
            arr2,
            [](int a, int b) {
                return a * b == b;
            },
            2));
    }
}

TEST(arrnd_test, can_be_compared_with_another_array_or_value)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 4, 5, 6};
    std::int64_t dims1[]{3, 1, 2};
    Integer_array arr1{dims1, data1};
    Integer_array arr2{dims1, data1};

    EXPECT_TRUE(oc::all_equal(arr1, arr2));
    EXPECT_TRUE(oc::any_equal(arr1, arr2));

    std::int64_t dims2[]{3, 2};
    Integer_array arr3{dims2, data1};

    EXPECT_FALSE(oc::all_equal(arr1, arr3));
    EXPECT_FALSE(oc::any_equal(arr1, arr3));

    const int data2[] = {1, 2, 3, 4, 5, 5};
    Integer_array arr4{dims1, data2};
    Integer_array arr5{dims2, data2};

    EXPECT_FALSE(oc::all_equal(arr1, arr4));
    EXPECT_FALSE(oc::all_equal(arr1, arr5));
    EXPECT_TRUE(oc::any_equal(arr1, arr4));
    EXPECT_FALSE(oc::any_equal(arr1, arr5));

    {
        using Integer_array = oc::arrnd<int>;

        const int data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9,

            10, 11, 12, 13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24, 25, 26, 27,

            28, 29, 30, 31, 32, 33, 34, 35, 36};
        const std::int64_t dims[]{2, 2, 3, 3};
        Integer_array arr{dims, data};

        const int rdata[] = {11, 14,

            29, 32};
        const std::int64_t rdims[]{2, 1, 2, 1};
        Integer_array rarr{rdims, rdata};

        Integer_array sarr{arr[{{0, 2}, {1, 2}, {0, 2}, {1, 3, 2}}]};
        EXPECT_TRUE(oc::all_equal(rarr, sarr));
        EXPECT_TRUE(oc::any_equal(rarr, sarr));
    }

    // different ND array types
    {
        const double data1d[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
        std::int64_t dims1d[]{3, 1, 2};
        oc::arrnd<double> arr1d{dims1d, data1d};

        EXPECT_TRUE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::any_equal(arr1, arr1d));

        arr1d[{0, 0, 0}] = 1.001;
        EXPECT_FALSE(oc::all_equal(arr1, arr1d));
        EXPECT_TRUE(oc::any_equal(arr1, arr1d));
    }

    // empty arrays
    {
        EXPECT_TRUE(oc::all_equal(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::all_equal(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::all_equal(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0)));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0)));
    }

    // scalar
    {
        EXPECT_FALSE(oc::all_equal(arr1, 1));
        EXPECT_FALSE(oc::all_equal(2, arr2));
        EXPECT_TRUE(oc::any_equal(arr1, 1));
        EXPECT_TRUE(oc::any_equal(2, arr2));
    }
}

TEST(arrnd_test, can_be_compared_by_tolerance_values_with_another_array_or_value)
{
    using Integer_array = oc::arrnd<int>;

    const int data1[] = {1, 2, 3, 4, 5, 6};
    std::int64_t dims1[]{3, 1, 2};
    Integer_array arr1{dims1, data1};
    Integer_array arr2{dims1, data1};

    EXPECT_TRUE(oc::all_close(arr1, arr2));
    EXPECT_TRUE(oc::any_close(arr1, arr2));

    std::int64_t dims2[]{3, 2};
    Integer_array arr3{dims2, data1};

    EXPECT_FALSE(oc::all_close(arr1, arr3, 1));
    EXPECT_FALSE(oc::any_close(arr1, arr3, 1));

    const int data2[] = {1, 2, 3, 4, 5, 5};
    Integer_array arr4{dims1, data2};
    Integer_array arr5{dims2, data2};

    EXPECT_TRUE(oc::all_close(arr1, arr4, 1));
    EXPECT_FALSE(oc::all_close(arr1, arr5, 1));
    EXPECT_TRUE(oc::any_close(arr1, arr4, 1));
    EXPECT_FALSE(oc::any_close(arr1, arr5, 1));

    // nested arrays
    {
        oc::arrnd<Integer_array> narr1({1, 2}, arr1);
        oc::arrnd<Integer_array> narr2({1, 2}, arr4);

        EXPECT_FALSE(oc::all_close(narr1, narr2));
        EXPECT_TRUE(oc::all_close(narr1, narr2, 1));
        EXPECT_TRUE(oc::any_close(narr1, narr2));
        EXPECT_TRUE(oc::any_close(narr1, narr2, 1));

        EXPECT_FALSE(oc::all_close(narr1, 1));
        EXPECT_TRUE(oc::all_close(narr1, 1, 5));
        EXPECT_TRUE(oc::any_close(narr1, 1));
        EXPECT_TRUE(oc::any_close(narr1, 1, 5));

        EXPECT_FALSE(oc::all_close(1, narr1));
        EXPECT_TRUE(oc::all_close(1, narr1, 5));
        EXPECT_TRUE(oc::any_close(1, narr1));
        EXPECT_TRUE(oc::any_close(1, narr1, 5));
    }

    {
        using Integer_array = oc::arrnd<int>;

        const int data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9,

            10, 11, 12, 13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24, 25, 26, 27,

            28, 29, 30, 31, 32, 33, 34, 35, 36};
        const std::int64_t dims[]{2, 2, 3, 3};
        Integer_array arr{dims, data};

        const int rdata[] = {10, 14,

            29, 32};
        const std::int64_t rdims[]{2, 1, 2, 1};
        Integer_array rarr{rdims, rdata};

        Integer_array sarr{arr[{{0, 2}, {1, 2}, {0, 2}, {1, 3, 2}}]};
        EXPECT_TRUE(oc::all_close(rarr, sarr, 1));
        EXPECT_TRUE(oc::any_close(rarr, sarr, 1));
    }

    // different ND array types
    {
        const double data1d[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
        std::int64_t dims1d[]{3, 1, 2};
        oc::arrnd<double> arr1d{dims1d, data1d};

        EXPECT_TRUE(oc::all_close(arr1, arr1d));
        EXPECT_TRUE(oc::any_close(arr1, arr1d));

        arr1d[{0, 0, 0}] = 1.001;
        EXPECT_FALSE(oc::all_close(arr1, arr1d));
        EXPECT_TRUE(oc::any_close(arr1, arr1d));
    }

    // empty arrays
    {
        Integer_array({});
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0)));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array(std::initializer_list<int>{}, 0)));
    }

    // scalar
    {
        EXPECT_TRUE(oc::all_close(arr1, 1, 5));
        EXPECT_FALSE(oc::all_close(1, arr2));
        EXPECT_TRUE(oc::any_close(arr1, 1, 5));
        EXPECT_TRUE(oc::any_close(1, arr2));
    }
}

TEST(arrnd_test, can_return_slice)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[] = {3, 1, 2};
    Integer_array arr{dims, data};

    // empty ranges
    {
        std::initializer_list<oc::interval<std::int64_t>> ranges{};
        Integer_array rarr{arr[ranges]};
        EXPECT_TRUE(oc::all_equal(arr, rarr));
        EXPECT_EQ(arr.storage()->data(), rarr.storage()->data());
    }

    // illegal ranges
    {
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, (arr[{ {0, 0, 0} }]))); // assertion failure
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, (arr[{ {2, 1, 1} }]))); // assertion failure
    }

    // empty array
    {
        EXPECT_TRUE(
            oc::all_equal(Integer_array{}, (Integer_array{}[std::initializer_list<oc::interval<std::int64_t>>{}])));
        EXPECT_TRUE(oc::all_equal(Integer_array{}, (Integer_array{}[{{0, 2}, {0, 5, 2}}])));
    }

    // ranges in dims
    {
        // nranges == ndims
        const int tdata1[] = {1, 5};
        const std::int64_t tdims1[] = {2, 1, 1};
        Integer_array tarr1{tdims1, tdata1};
        Integer_array sarr1{arr[{{0, 3, 2}, {0, 1}, {0, 1}}]};
        EXPECT_TRUE(oc::all_equal(tarr1, sarr1));
        EXPECT_EQ(arr.storage()->data(), sarr1.storage()->data());

        // nranges < ndims
        const int tdata2[] = {3, 4};
        const std::int64_t tdims2[] = {1, 1, 2};
        Integer_array tarr2{tdims2, tdata2};
        Integer_array sarr2{arr[{{1, 3, 2}}]};
        EXPECT_TRUE(oc::all_equal(tarr2, sarr2));
        EXPECT_EQ(arr.storage()->data(), sarr2.storage()->data());

        // nranges > ndims - ignore extra ranges
        Integer_array sarr3{arr[{{0, 3, 2}, {0, 1}, {0, 1}, {100, 101, 5}}]};
        EXPECT_TRUE(oc::all_equal(sarr1, sarr3));
        EXPECT_EQ(arr.storage()->data(), sarr3.storage()->data());

        // out of range and negative indices
        //Integer_array sarr4{ arr[{{-1, 3, -2}, {1}, {-2}}] }; // assertion failure
        //EXPECT_TRUE(oc::all_equal(sarr1, sarr4));
        //EXPECT_EQ(arr.data(), sarr4.data());
    }
}

TEST(arrnd_test, can_access_its_creators_chain_if_available)
{
    using namespace oc;

    // basic chain
    {
        arrnd<int> arr({2, 4}, 0);
        EXPECT_EQ(nullptr, arr.creator());

        auto sarr1 = arr[interval<>::at(0)];
        auto sarr2 = sarr1[interval<>::at(0)];

        EXPECT_EQ(&sarr1, sarr2.creator());
        EXPECT_EQ(&arr, sarr2.creator()->creator());

        auto sarr3 = arr[interval<>::at(0)][interval<>::at(0)];
        // mid subarray is an rvalue, which is being destructed, hence sarr3 creator is invalid.
        EXPECT_EQ(nullptr, sarr3.creator());
    }
    {
        arrnd<int> arr({2, 4}, 0);
        EXPECT_EQ(nullptr, arr.creator());

        auto sarr1 = arr[{interval<>::at(0)}];
        auto sarr2 = sarr1[{interval<>::at(0)}];

        EXPECT_EQ(&sarr1, sarr2.creator());
        EXPECT_EQ(&arr, sarr2.creator()->creator());

        auto sarr3 = arr[{interval<>::at(0)}][{interval<>::at(0)}];
        // mid subarray is an rvalue, which is being destructed, hence sarr3 creator is invalid.
        EXPECT_EQ(nullptr, sarr3.creator());
    }

    // out of scope
    {
        arrnd<int> arr({2, 4}, 0);
        EXPECT_EQ(nullptr, arr.creator());

        arrnd<int> sarr2;
        EXPECT_EQ(nullptr, sarr2.creator());
        {
            arrnd<int> sarr1 = arr[interval<>::at(0)];
            EXPECT_EQ(&arr, sarr1.creator());

            sarr2 = sarr1[interval<>::at(0)];
            EXPECT_EQ(&sarr1, sarr2.creator());
        }
        EXPECT_EQ(nullptr, sarr2.creator());
    }

    // self assignment
    {
        arrnd<int> arr({2, 4}, 0);
        EXPECT_EQ(nullptr, arr.creator());

        arrnd<int> sarr1 = arr[interval<>::at(0)];
        sarr1 = sarr1[interval<>::at(0)];
        // self assignment cancels creator
        EXPECT_EQ(nullptr, sarr1.creator());
    }
}

TEST(arrnd_test, can_be_assigned_with_value)
{
    using Integer_array = oc::arrnd<int>;

    // empty array
    {
        Integer_array arr{};
        arr = 100;
        EXPECT_TRUE(oc::all_equal(Integer_array{}, arr));
    }

    // normal array
    {
        const int data[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t dims[]{3, 1, 2};
        Integer_array arr{dims, data};

        const int tdata[] = {100, 100, 100, 100, 100, 100};
        Integer_array tarr{dims, tdata};

        arr = 100;
        EXPECT_TRUE(oc::all_equal(tarr, arr));
    }

    // subarray
    {
        const int data[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t dims[]{3, 1, 2};
        Integer_array arr{dims, data};

        const int tdata[] = {1, 50, 3, 100, 5, 100};
        Integer_array tarr{dims, tdata};

        arr[{{1, 3}, {0, 1}, {1, 2}}] = 100;
        // assignment of different type
        arr[{{0, 1}, {0, 1}, {1, 2}}] = 50.5;
        EXPECT_TRUE(oc::all_equal(tarr, arr));
    }
}

TEST(arrnd_test, copy_by_reference)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[]{3, 1, 2};
    Integer_array arr{dims, data};

    Integer_array carr1{arr};
    carr1[{2, 0, 0}] = 0;
    const int rdata1[] = {1, 2, 3, 4, 0, 6};
    Integer_array rarr1{dims, rdata1};
    EXPECT_TRUE(oc::all_equal(rarr1, carr1));

    Integer_array carr2{};
    carr2 = carr1;
    carr1[{0, 0, 0}] = 0;
    const int rdata2[] = {0, 2, 3, 4, 0, 6};
    Integer_array rarr2{dims, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, carr2));

    carr2[{{0, 2}, {0, 1}, {0, 2}}] = carr1;
    EXPECT_TRUE(oc::all_equal(rarr2, carr2));

    // slice copying by assignment (rvalue)
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        Integer_array starr3{tarr3[{{0, 6, 2}}]};
        rarr3[{{0, 6, 2}}] = starr3;
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue)
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        Integer_array starr3{tarr3[{{0, 6, 2}}]};
        Integer_array srarr3{rarr3[{{0, 6, 2}}]};
        rarr3 = starr3;
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
    }

    // different template arguments
    {
        const int idata[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t dims[]{3, 1, 2};
        Integer_array iarr{dims, idata};

        const double ddata[] = {1.1, 2.1, 3.1, 4.1, 5.1, 6.1};
        oc::arrnd<double> darr{dims, ddata};

        Integer_array cdarr1{darr};
        EXPECT_TRUE(oc::all_equal(iarr, cdarr1));

        Integer_array cdarr2{};
        cdarr2 = darr;
        EXPECT_TRUE(oc::all_equal(iarr, cdarr2));
    }

    // slice copying by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> starr3{tarr3[{{0, 6, 2}}]};
        rarr3[{{0, 6, 2}}] = starr3;
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> starr3{tarr3[{{0, 6, 2}}]};
        rarr3[{{0, 4, 2}}] = starr3;
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> starr3{tarr3[{{0, 6, 2}}]};
        Integer_array srarr3{rarr3[{{0, 6, 2}}]};
        rarr3 = starr3;
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
    }
}

TEST(arrnd_test, empty_slice)
{
    using namespace oc;

    arrnd<int> arr({1, 5}, {1, 2, 3, 4, 5});

    auto full_slice = arr[{interval<>::full(), interval<>::at(0)}];
    auto empty_slice = arr[{interval<>::full(), interval<>::to(0)}];

    EXPECT_TRUE(empty_slice.empty());
    EXPECT_EQ(full_slice.creator(), empty_slice.creator());
}

TEST(arrnd_test, move_by_reference)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[]{3, 1, 2};
    Integer_array sarr{dims, data};

    Integer_array arr{dims, data};
    Integer_array carr1{std::move(arr)};
    EXPECT_TRUE(oc::all_equal(sarr, carr1));
    EXPECT_TRUE(empty(arr));

    Integer_array carr2{};
    carr2 = std::move(carr1);
    EXPECT_TRUE(oc::all_equal(sarr, carr2));
    EXPECT_TRUE(empty(carr1));

    Integer_array sarr2{dims, data};
    carr2[{{0, 2}, {0, 1}, {0, 2}}] = std::move(sarr2);
    EXPECT_TRUE(empty(sarr2));
    EXPECT_TRUE(oc::all_equal(sarr, carr2));

    // slice moving by assignment
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        rarr3[{{0, 6, 2}}] = std::move(tarr3[{{0, 6, 2}}]);
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        Integer_array srarr3{rarr3[{{0, 6, 2}}]};
        srarr3 = std::move(tarr3[{{0, 6, 2}}]);
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // different template arguments
    {
        const int idata[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t dims[]{3, 1, 2};
        Integer_array iarr{dims, idata};

        const double ddata[] = {1.1, 2.1, 3.1, 4.1, 5.1, 6.1};
        oc::arrnd<double> darr{dims, ddata};

        Integer_array cdarr1{std::move(darr)};
        EXPECT_TRUE(oc::all_equal(iarr, cdarr1));
        EXPECT_TRUE(empty(darr));

        oc::arrnd<double> cdarr2{};
        cdarr2 = std::move(cdarr1);
        EXPECT_TRUE(oc::all_equal((Integer_array{dims, idata}), cdarr2));
        EXPECT_TRUE(empty(cdarr1));
    }

    // slice moving by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const double rdata3[] = {0, 2, 0, 4, 0, 6};
        oc::arrnd<double> rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        rarr3[{{0, 6, 2}}] = std::move(tarr3[{{0, 6, 2}}]);
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const double rdata3[] = {0, 2, 0, 4, 0, 6};
        oc::arrnd<double> rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        rarr3[{{0, 4, 2}}] = std::move(tarr3[{{0, 6, 2}}]);
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> srarr3{rarr3[{{0, 6, 2}}]};
        srarr3 = std::move(tarr3[{{0, 6, 2}}]);
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }
}

TEST(arrnd_test, clone)
{
    using Integer_array = oc::arrnd<int>;

    Integer_array empty_arr{};
    Integer_array cempty_arr{oc::clone(empty_arr)};
    EXPECT_TRUE(oc::all_equal(empty_arr, cempty_arr));

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[]{3, 1, 2};
    Integer_array sarr{dims, data};

    Integer_array carr{oc::clone(sarr)};
    EXPECT_TRUE(oc::all_equal(carr, sarr));
    carr[{0, 0, 0}] = 0;
    EXPECT_FALSE(oc::all_equal(carr, sarr));

    Integer_array csubarr{oc::clone(sarr[{{1, 2}, {0, 1}, {0, 1}}])};
    EXPECT_TRUE(oc::all_equal((sarr[{{1, 2}, {0, 1}, {0, 1}}]), csubarr));
    csubarr[{0, 0, 0}] = 5;
    EXPECT_FALSE(oc::all_equal((sarr[{{1, 2}, {0, 1}, {0, 1}}]), csubarr));

    // nested array
    {
        using namespace oc;

        using arrnd_l2 = arrnd<int>;
        using arrnd_l1 = arrnd<arrnd_l2>;
        using arrnd_l0 = arrnd<arrnd_l1>;

        arrnd_l0 arr_with_vals({2},
            {arrnd_l1({1, 2}, {arrnd_l2({2, 2}, 1), arrnd_l2({2, 1, 3}, {1, 2, 3, 4, 5, 6})}),
                arrnd_l1({1, 1}, {arrnd_l2({2, 2}, 1)})});

        arrnd_l0 arr_no_vals = clone(arr_with_vals);

        EXPECT_TRUE(all_equal(arr_with_vals, arr_no_vals));

        arr_with_vals[{0}][{0, 0}][{0, 0}] = 100;
        EXPECT_NE((arr_with_vals[{0}][{0, 0}][{0, 0}]), (arr_no_vals[{0}][{0, 0}][{0, 0}]));
    }
}

TEST(arrnd_test, copy_from)
{
    using namespace oc;

    // empty src
    {
        arrnd<double> src{};
        arrnd<int> dst{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};

        copy(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{};
        arrnd<int> dst{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};

        copy(src, dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // empty dst
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{};

        copy(src, dst);
        arrnd<int> res{};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{};

        copy(src[{{0, 2}, {0, 1}, {1, 2}}], dst);
        arrnd<int> sres{};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // same dimensions
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            copy(src, dst);
            arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
            EXPECT_TRUE(all_equal(res, dst));
        }

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            copy(src, dst, std::vector<int>{0, 2, 4});
            arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
            EXPECT_TRUE(all_equal(res, dst));
        }

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            copy(src, dst, {0, 2, 4});
            arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
            EXPECT_TRUE(all_equal(res, dst));
        }

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            int inds[]{0, 2, 4};
            copy(src, dst, inds);
            arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
            EXPECT_TRUE(all_equal(res, dst));
        }

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            copy(src, dst, arrnd<int>({3}, {0, 2, 4}));
            arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
            EXPECT_TRUE(all_equal(res, dst));
        }

        {
            arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
            copy(src, dst, arrnd<bool>({3, 1, 2}, {1, 0, 1, 0, 1, 0}));
            arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
            EXPECT_TRUE(all_equal(res, dst));
        }
    }
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src[{{1, 3}, {0, 1}, {0, 1}}], dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {6, 3, 4, 5, 2, 1}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // same sizes
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src[{{0, 2}}], dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {6, 1, 4, 2, 2, 1}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // size(src) < size(dst)
    {
        arrnd<double> src{{3}, {1, 2, 3}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 3, 2, 1}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src[{{1, 3}}], dst[{{0, 3}, {0, 1}, {0, 1}}]);
        arrnd<int> sres{{3, 1, 2}, {2, 5, 3, 3, 2, 1}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // size(src) > size(dst)
    {
        arrnd<double> src{{10}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src[{{1, 6}}], dst[{{0, 2}, {0, 1}, {0, 1}}]);
        arrnd<int> sres{{3, 1, 2}, {2, 5, 3, 3, 2, 1}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // speicifc indices
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<std::int64_t> indices{{3}, {0, 2, 4}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst, indices);
        arrnd<int> res{{3, 1, 2}, {1, 5, 2, 3, 3, 1}};
        EXPECT_TRUE(all_equal(res, dst));
    }

    // specific ranges
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        std::initializer_list<interval<std::int64_t>> ranges{
            interval<std::int64_t>{0, 3}, interval<std::int64_t>{0, 1}, interval<std::int64_t>{1, 2}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst, ranges);
        arrnd<int> res{{3, 1, 2}, {6, 1, 4, 2, 2, 3}};
        EXPECT_TRUE(all_equal(res, dst));
    }

    // nested array
    {
        using arrnd_l2 = arrnd<int>;
        using arrnd_l1 = arrnd<arrnd_l2>;
        using arrnd_l0 = arrnd<arrnd_l1>;

        arrnd_l0 arr_with_vals({2},
            {arrnd_l1({1, 2}, {arrnd_l2({2, 2}, 1), arrnd_l2({2, 1, 3}, {1, 2, 3, 4, 5, 6})}),
                arrnd_l1({1, 1}, {arrnd_l2({2, 2}, 1)})});

        arrnd_l0 arr_no_vals(
            {2}, {arrnd_l1({1, 2}, {arrnd_l2({2, 2}), arrnd_l2({1, 1, 3})}), arrnd_l1({1, 1}, {arrnd_l2({4})})});

        copy(arr_with_vals, arr_no_vals);

        EXPECT_TRUE(all_equal(arr_with_vals[{0}][{0, 0}], arr_no_vals[{0}][{0, 0}]));
        EXPECT_TRUE(all_equal(arr_with_vals[{0}][{0, 1}][{interval<>::at(0), interval<>::full(), interval<>::full()}],
            arr_no_vals[{0}][{0, 1}]));
        EXPECT_TRUE(all_equal(arr_with_vals[{1}][{0, 0}], reshape(arr_no_vals[{1}][{0, 0}], {2, 2})));

        arr_with_vals[{0}][{0, 0}][{0, 0}] = 100;
        EXPECT_NE((arr_with_vals[{0}][{0, 0}][{0, 0}]), (arr_no_vals[{0}][{0, 0}][{0, 0}]));
    }
}

TEST(arrnd_test, set_from)
{
    using namespace oc;

    // empty src
    {
        arrnd<double> src{};
        arrnd<int> dst{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};

        set(src, dst);
        arrnd<int> res{};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{};
        arrnd<int> dst{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};

        auto ref = set(src, dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(sres, dst));
        EXPECT_TRUE(all_equal(src, ref));
    }

    // empty dst
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{};

        set(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{};

        set(src[{{0, 2}, {0, 1}, {1, 2}}], dst);
        arrnd<int> sres{{2, 1, 1}, {2, 4}};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // same dimensions
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        set(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        auto ref = set(src[{{1, 3}, {0, 1}, {0, 1}}], dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {6, 3, 4, 5, 2, 1}};
        arrnd<int> rres{{2, 1, 1}, {3, 5}};
        EXPECT_TRUE(all_equal(sres, dst));
        EXPECT_TRUE(all_equal(rres, ref));
    }

    // same sizes
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        set(src, dst);
        arrnd<int> res{{6}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        auto ref = set(src[{{0, 2}}], dst[{{0, 2}, {0, 1}, {1, 2}}]);
        arrnd<int> sres{{3, 1, 2}, {1, 2, 4, 3, 2, 1}};
        ;
        arrnd<int> rres{{2}, {1, 2}};
        EXPECT_TRUE(all_equal(sres, dst));
        EXPECT_TRUE(all_equal(rres, ref));
    }

    // size(src) < size(dst)
    {
        arrnd<double> src{{3}, {1, 2, 3}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        set(src, dst);
        arrnd<int> res{{3}, {1, 2, 3}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        auto ref = set(src[{{1, 3}}], dst[{{0, 3}, {0, 1}, {0, 1}}]);
        arrnd<int> sres{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
        arrnd<int> rres{{2}, {2, 3}};
        EXPECT_TRUE(all_equal(sres, dst));
        EXPECT_TRUE(all_equal(rres, ref));
    }

    // size(src) > size(dst)
    {
        arrnd<double> src{{10}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        set(src, dst);
        arrnd<int> res{{10}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{6}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        auto ref = set(src[{{1, 6}}], dst[{{0, 2}, {0, 1}, {0, 1}}]);
        arrnd<int> sres{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};
        arrnd<int> rres{{5}, {2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(sres, dst));
        EXPECT_TRUE(all_equal(rres, ref));
    }

    // nested array
    {
        using arrnd_l2 = arrnd<int>;
        using arrnd_l1 = arrnd<arrnd_l2>;
        using arrnd_l0 = arrnd<arrnd_l1>;

        arrnd_l0 arr_with_vals({2},
            {arrnd_l1({1, 2}, {arrnd_l2({2, 2}, 1), arrnd_l2({2, 1, 3}, {1, 2, 3, 4, 5, 6})}),
                arrnd_l1({1, 1}, {arrnd_l2({2, 2}, 1)})});

        arrnd_l0 arr_no_vals;

        set(arr_with_vals, arr_no_vals);

        EXPECT_TRUE(all_equal(arr_with_vals, arr_no_vals));

        arr_with_vals[{0}][{0, 0}][{0, 0}] = 100;
        EXPECT_NE((arr_with_vals[{0}][{0, 0}][{0, 0}]), (arr_no_vals[{0}][{0, 0}][{0, 0}]));
    }
}

//TEST(arrnd_test, copy)
//{
//    using Integer_array = oc::arrnd<int>;
//
//    { // backward cases - copy from other array to created array
//        Integer_array empty_arr{};
//        Integer_array cempty_arr{};
//        oc::copy(Integer_array{}, cempty_arr);
//        EXPECT_TRUE(oc::all_equal(empty_arr, cempty_arr));
//
//        const int data1[] = {
//            1, 2,
//            3, 4,
//            5, 6 };
//        Integer_array arr1{ {3, 1, 2}, data1 };
//
//        const int data2[] = {
//            2, 4,
//            6, 8,
//            10, 12 };
//        Integer_array arr2{ {3, 1, 2}, data2 };
//        EXPECT_FALSE(oc::all_equal(arr1, arr2));
//        oc::copy(arr2, arr1);
//        EXPECT_TRUE(oc::all_equal(arr1, arr2));
//
//        const int data3[] = {
//            10, 12,
//            6, 8,
//            10, 12 };
//        Integer_array arr3{ {3, 1, 2}, data3 };
//        oc::copy(arr2[{ {2, 2}, {0, 0}, {0, 1} }], arr2[{ {0, 0}, {0, 0}, {0, 1} }]);
//        EXPECT_TRUE(oc::all_equal(arr3, arr2));
//
//        oc::copy(arr2, arr2[{ {0, 0}, {0, 0}, {0, 1} }]);
//        EXPECT_TRUE(oc::all_equal(arr3, arr2));
//    }
//
//    { // forward cases - copy from created array to other array
//        Integer_array empty_arr{};
//        Integer_array cempty_arr{};
//        oc::copy(cempty_arr, empty_arr);
//        EXPECT_TRUE(oc::all_equal(empty_arr, cempty_arr));
//
//        const int data1[] = {
//            1, 2,
//            3, 4,
//            5, 6 };
//        Integer_array arr1{ {3, 1, 2}, data1 };
//
//        const int data2[] = {
//            2, 4,
//            6, 8,
//            10, 12 };
//        Integer_array arr2{ {3, 1, 2}, data2 };
//        EXPECT_FALSE(oc::all_equal(arr1, arr2));
//        oc::copy(arr2, arr1);
//        EXPECT_TRUE(oc::all_equal(arr1, arr2));
//
//        const int data3[] = {
//            10, 12,
//            6, 8,
//            10, 12 };
//        Integer_array arr3{ {3, 1, 2}, data3 };
//        oc::copy(arr2[{ {2, 2}, {0, 0}, {0, 1} }], arr2[{ {0, 0}, {0, 0}, {0, 1} }]);
//        EXPECT_TRUE(oc::all_equal(arr3, arr2));
//
//        oc::copy(arr2[{ {2, 2}, {0, 0}, {0, 1} }], arr2[{ {0, 1}, {0, 0}, {0, 1} }]);
//        EXPECT_TRUE(oc::all_equal(arr3, arr2));
//
//        oc::copy(arr3[{ {0, 0}, {0, 0}, {0, 1} }], arr2);
//        EXPECT_TRUE(oc::all_equal((arr3[{ {0, 0}, {0, 0}, {0, 1} }]), (arr2[{ {0, 0}, {0, 0}, {0, 1} }])));
//    }
//
//    // copy to different type ND array
//    {
//        const int data1[] = {
//            1, 2,
//            3, 4,
//            5, 6 };
//        Integer_array arr1{ {3, 1, 2}, data1 };
//
//        const double data2d[] = {
//            2.1, 4.1,
//            6.1, 8.1,
//            10.1, 12.1 };
//        oc::arrnd<double> arr2d{ {3, 1, 2}, data2d };
//        EXPECT_FALSE(oc::all_equal(arr1, arr2d));
//        oc::copy(arr2d, arr1);
//        EXPECT_FALSE(oc::all_equal(arr1, arr2d));
//    }
//
//    // test is incorrect. copy should copy elements not according to subscripts.
//    //// copy to different dimensions and same count
//    //{
//    //    const int data[] = { 1, 2, 3, 4, 5, 6 };
//    //    Integer_array arr{ {6}, data };
//
//    //    Integer_array tarr{ {3, 1, 2}, data };
//
//    //    Integer_array rarr{ {3, 1, 2}, 0 };
//    //    EXPECT_FALSE(oc::all_equal(tarr, rarr));
//    //    oc::copy(arr, rarr);
//    //    EXPECT_TRUE(oc::all_equal(
//    //        Integer_array{ {3, 1, 2}, {
//    //            5, 6,
//    //            0, 0,
//    //            0, 0 } }, rarr));
//    //}
//}

TEST(arrnd_test, reshape)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[]{3, 1, 2};
    Integer_array arr{dims, data};

    {
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::reshape(arr, {}))); // assertion failure
    }

    {
        Integer_array x{};
        EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::reshape(x, {})));
    }

    {
        const int tdata[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t tdims[]{6};
        Integer_array tarr{tdims, tdata};

        Integer_array rarr{oc::reshape(arr, {6})};
        EXPECT_TRUE(oc::all_equal(tarr, rarr));
        EXPECT_EQ(arr.storage()->data(), rarr.storage()->data());
    }

    {
        const std::int64_t tdims[]{3, 1, 2};
        Integer_array rarr{oc::reshape(arr, tdims)};
        EXPECT_TRUE(oc::all_equal(arr, rarr));
        EXPECT_EQ(arr.storage()->data(), rarr.storage()->data());
    }

    {
        const int tdata[] = {1, 5};
        const std::int64_t tdims[]{1, 2};
        Integer_array tarr{tdims, tdata};
        Integer_array x = arr[{{0, 3, 2}, {}, {}}];
        Integer_array rarr{oc::reshape(x, {1, 2})};
        EXPECT_TRUE(oc::all_equal(tarr, rarr));
        EXPECT_NE(arr.storage()->data(), rarr.storage()->data());
    }

    // nested array
    {
        oc::arrnd<Integer_array> inarr({1, 2}, {Integer_array({4}, {1, 2, 3, 4}), Integer_array({1, 4}, {5, 6, 7, 8})});

        oc::arrnd<Integer_array> rnarr1 = oc::reshape(inarr, {2, 2});
        EXPECT_FALSE(oc::all_equal(rnarr1, inarr));
        EXPECT_TRUE(oc::all_equal(Integer_array({2, 2}, {1, 2, 3, 4}), rnarr1[{0, 0}]));
        EXPECT_TRUE(oc::all_equal(Integer_array({2, 2}, {5, 6, 7, 8}), rnarr1[{0, 1}]));
        EXPECT_EQ((rnarr1[{0, 0}].storage()->data()), (inarr[{0, 0}].storage()->data()));
        EXPECT_EQ((rnarr1[{0, 1}].storage()->data()), (inarr[{0, 1}].storage()->data()));

        oc::arrnd<Integer_array> rnarr2 = oc::reshape<0>(inarr, {2});
        EXPECT_FALSE(oc::all_equal(rnarr2, inarr));
        EXPECT_TRUE(oc::all_equal(rnarr2[{0}], inarr[{0, 0}]));
        EXPECT_TRUE(oc::all_equal(rnarr2[{1}], inarr[{0, 1}]));
        EXPECT_EQ((rnarr2[{0}].storage()->data()), (inarr[{0, 0}].storage()->data()));
        EXPECT_EQ((rnarr2[{1}].storage()->data()), (inarr[{0, 1}].storage()->data()));

        auto rnarr3 = oc::reshape(inarr, oc::arrnd_shape_preset::vector);
        EXPECT_TRUE(oc::all_equal(rnarr3,
            oc::arrnd<Integer_array>({1, 2}, {Integer_array({4}, {1, 2, 3, 4}), Integer_array({4}, {5, 6, 7, 8})})));

        auto rnarr4 = oc::reshape<0>(inarr, oc::arrnd_shape_preset::vector);
        EXPECT_TRUE(oc::all_equal(rnarr4,
            oc::arrnd<Integer_array>({2}, {Integer_array({4}, {1, 2, 3, 4}), Integer_array({1, 4}, {5, 6, 7, 8})})));
    }
}

TEST(arrnd_test, resize)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};
    const std::int64_t dims[]{6};
    Integer_array arr{dims, data};

    {
        EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::resize(arr, {})));
    }

    {
        Integer_array x{};
        Integer_array rarr{oc::resize(x, {6})};
        //EXPECT_FALSE(oc::all_equal(arr, rarr));
        EXPECT_EQ(arr.header().dims().size(), rarr.header().dims().size());
        EXPECT_EQ(6, rarr.header().dims().data()[0]);
        EXPECT_NE(arr.storage()->data(), rarr.storage()->data());
    }

    {
        Integer_array rarr{oc::resize(arr, {6})};
        EXPECT_TRUE(oc::all_equal(arr, rarr));
        EXPECT_EQ(arr.storage()->data(), rarr.storage()->data());
    }

    {
        const int tdata[] = {1, 2};
        const std::int64_t tdims[]{2};
        Integer_array tarr{tdims, tdata};

        Integer_array rarr{oc::resize(arr, {2})};
        EXPECT_TRUE(oc::all_equal(tarr, rarr));
        EXPECT_NE(tarr.storage()->data(), rarr.storage()->data());
    }

    {
        const int tdata[] = {1, 2, -1, -1, -1, -1};
        const std::int64_t tdims[]{3, 1, 2};
        Integer_array tarr{tdims, tdata};

        Integer_array rarr{oc::resize(arr, {3, 1, 2})};
        EXPECT_TRUE(oc::all_equal(tarr[{oc::interval<>::at(0)}], rarr[{oc::interval<>::at(0)}]));
        //std::cout << rarr << "\n\n";
        EXPECT_NE(tarr.storage()->data(), rarr.storage()->data());
    }

    {
        const std::int64_t tdims[]{10};
        Integer_array rarr{oc::resize(arr, tdims)};
        EXPECT_FALSE(oc::all_equal(arr, rarr));
        EXPECT_TRUE(oc::all_equal(arr, (rarr[{{0, 6}}])));
        EXPECT_NE(arr.storage()->data(), rarr.storage()->data());
    }

    // nested array
    {
        oc::arrnd<Integer_array> inarr(
            {1, 2}, {Integer_array({5}, {1, 2, 3, 4, 5}), Integer_array({1, 5}, {6, 7, 8, 9, 10})});

        oc::arrnd<Integer_array> rnarr1 = oc::resize(inarr, {2, 2});
        EXPECT_FALSE(oc::all_equal(rnarr1, inarr));
        //std::cout << rnarr1[{0, 0}] << "\n\n";
        //std::cout << rnarr1[{0, 1}] << "\n\n";
        EXPECT_TRUE(oc::all_equal(
            Integer_array({2, 2}, {1, 2, -1, -1})[{oc::interval<>::at(0)}], rnarr1[{0, 0}][{oc::interval<>::at(0)}]));
        EXPECT_TRUE(oc::all_equal(
            Integer_array({2, 2}, {6, 7, -1, -1})[{oc::interval<>::at(0)}], rnarr1[{0, 1}][{oc::interval<>::at(0)}]));
        EXPECT_NE((rnarr1[{0, 0}].storage()->data()), (inarr[{0, 0}].storage()->data()));
        EXPECT_NE((rnarr1[{0, 1}].storage()->data()), (inarr[{0, 1}].storage()->data()));

        oc::arrnd<Integer_array> rnarr2 = oc::resize<0>(inarr, {1});
        EXPECT_FALSE(oc::all_equal(rnarr2, inarr));
        EXPECT_TRUE(oc::all_equal(rnarr2[{0}], inarr[{0, 0}]));
        EXPECT_EQ((rnarr2[{0}].storage()->data()), (inarr[{0, 0}].storage()->data()));
    }
}

TEST(arrnd_test, inserters)
{
    using namespace oc;

    arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

    {
        arrnd<int> res({5}, 0);
        std::copy(arr.cbegin(2, arrnd_returned_slice_iterator_tag{}), arr.cend(2, arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_back_inserter(res));
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {0, 0, 0, 0, 0, 1, 3, 5, 2, 4, 6}), res));
    }

    {
        arrnd<int> res({5}, 0);
        std::transform(arr.cbegin(arrnd_returned_slice_iterator_tag{}), arr.cend(arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_front_inserter(res), [](const auto& slice) {
                return slice * 2;
            });
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {10, 12, 6, 8, 2, 4, 0, 0, 0, 0, 0}), res));
    }

    {
        arrnd<int> res({5}, 0);
        std::copy(arr.cbegin(2, arrnd_returned_slice_iterator_tag{}), arr.cend(2, arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_inserter(res, 1));
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {0, 1, 3, 5, 2, 4, 6, 0, 0, 0, 0}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::copy(arr.cbegin(arrnd_returned_slice_iterator_tag{}), arr.cend(arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_slice_back_inserter(res, 2));
        EXPECT_TRUE(all_equal(arrnd<int>({1, 1, 8}, {0, 0, 1, 2, 3, 4, 5, 6}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::transform(arr.cbegin(arrnd_returned_slice_iterator_tag{}), arr.cend(arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_slice_front_inserter(res, 2), [](const auto& slice) {
                return slice * 2;
            });
        EXPECT_TRUE(all_equal(arrnd<int>({1, 1, 8}, {10, 12, 6, 8, 2, 4, 0, 0}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::copy(arr.cbegin(arrnd_returned_slice_iterator_tag{}), arr.cend(arrnd_returned_slice_iterator_tag{}),
            oc::arrnd_slice_inserter(res, 1, 2));
        EXPECT_TRUE(all_equal(arrnd<int>({1, 1, 8}, {0, 1, 2, 3, 4, 5, 6, 0}), res));
    }
}

TEST(arrnd_test, append)
{
    using Integer_array = oc::arrnd<int>;
    using Double_array = oc::arrnd<double>;

    // No axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6};
        Integer_array arr1{{3, 1, 2}, data1};

        const double data2[] = {7.0, 8.0, 9.0, 10.0, 11.0};
        Double_array arr2{{5}, data2};

        const int rdata1[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        Integer_array rarr{{11}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr, oc::append(arr1, arr2)));

        EXPECT_TRUE(oc::all_equal(arr1, oc::append(arr1, Integer_array{})));
        EXPECT_TRUE(oc::all_equal(arr2, oc::append(Integer_array{}, arr2)));
    }

    // Axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6,

            7, 8, 9, 10, 11, 12};
        Integer_array arr1{{2, 2, 3}, data1};

        const double data2[] = {13.0, 14.0, 15.0, 16.0, 17.0, 18.0,

            19.0, 20.0, 21.0, 22.0, 23.0, 24.0};
        Double_array arr2{{2, 2, 3}, data2};

        const int rdata1[] = {1, 2, 3, 4, 5, 6,

            7, 8, 9, 10, 11, 12,

            13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24};
        Integer_array rarr1{{4, 2, 3}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr1, oc::append(arr1, arr2, 0)));

        const int rdata2[] = {1, 2, 3, 4, 5, 6, 13, 14, 15, 16, 17, 18,

            7, 8, 9, 10, 11, 12, 19, 20, 21, 22, 23, 24};
        Integer_array rarr2{{2, 4, 3}, rdata2};
        EXPECT_TRUE(oc::all_equal(rarr2, oc::append(arr1, arr2, 1)));

        const int rdata3[] = {1, 2, 3, 13, 14, 15, 4, 5, 6, 16, 17, 18,

            7, 8, 9, 19, 20, 21, 10, 11, 12, 22, 23, 24};

        Integer_array rarr3{{2, 2, 6}, rdata3};
        EXPECT_TRUE(oc::all_equal(rarr3, oc::append(arr1, arr2, 2)));

        EXPECT_TRUE(oc::all_equal(arr1, oc::append(arr1, Integer_array{}, 0)));
        EXPECT_TRUE(oc::all_equal(arr2, oc::append(Integer_array{}, arr2, 0)));

        //EXPECT_TRUE(oc::all_equal(rarr1, oc::append(arr1, arr2, 3))); // assertion failure
        const int invalid_data1[] = {1};
        Integer_array invalid_arr1{{1}, invalid_data1};
        Integer_array rinvalid_arr1{};

        //EXPECT_TRUE(oc::all_equal(rinvalid_arr1, oc::append(invalid_arr1, arr2, 3))); // assertion failure
        //EXPECT_TRUE(oc::all_equal(rinvalid_arr1, oc::append(arr2, invalid_arr1, 3))); // assertion failure

        //EXPECT_TRUE(oc::all_equal(rinvalid_arr1, oc::append(arr1, rarr2, 0))); // invalid operation, assertion required
    }

    // multiple append cat like
    {
        // standard
        {
            oc::arrnd<int> arr = oc::append(oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}),
                oc::arrnd<int>({4}, {7, 8, 9, 10}), oc::arrnd<int>({4}, {11, 12, 13, 14}));

            oc::arrnd<int> res({14}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14});

            EXPECT_TRUE(all_equal(arr, res));
        }

        // by axis
        {
            oc::arrnd<int> arr = oc::append(oc::arrnd<int>({1, 1, 2}, {1, 2}),
                std::make_tuple(oc::arrnd<int>({2, 1, 2}, {3, 4, 5, 6}), 0),
                std::make_tuple(oc::arrnd<int>({3, 1, 1}, {14, 15, 16}), 2));

            oc::arrnd<int> res({3, 1, 3}, {1, 2, 14, 3, 4, 15, 5, 6, 16});

            EXPECT_TRUE(all_equal(arr, res));
        }
    }
}

TEST(arrnd_test, insert)
{
    using Integer_array = oc::arrnd<int>;
    using Double_array = oc::arrnd<double>;

    // No axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6};
        Integer_array arr1{{3, 1, 2}, data1};

        const double data2[] = {7.0, 8.0, 9.0, 10.0, 11.0};
        Double_array arr2{{5}, data2};

        const int rdata1[] = {1, 2, 3, 7, 8, 9, 10, 11, 4, 5, 6};
        Integer_array rarr1{{11}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr1, oc::insert(arr1, arr2, 3)));

        const int rdata2[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        Integer_array rarr2{{11}, rdata2};
        EXPECT_TRUE(oc::all_equal(rarr2, oc::insert(arr1, arr2, 6)));

        //const int rdata3[] = { 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6 };
        //Integer_array rarr3{ {11}, rdata3 };
        //EXPECT_TRUE(oc::all_equal(rarr3, oc::insert(arr1, arr2, 7))); // assertion failure

        EXPECT_TRUE(oc::all_equal(arr1, oc::insert(arr1, Integer_array{}, 0)));
        EXPECT_TRUE(oc::all_equal(arr2, oc::insert(Integer_array{}, arr2, 0)));
    }

    // Axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6,

            7, 8, 9, 10, 11, 12};
        Integer_array arr1{{2, 2, 3}, data1};

        const double data2[] = {13.0, 14.0, 15.0, 16.0, 17.0, 18.0,

            19.0, 20.0, 21.0, 22.0, 23.0, 24.0};
        Double_array arr2{{2, 2, 3}, data2};

        const int rdata1[] = {1, 2, 3, 4, 5, 6,

            13, 14, 15, 16, 17, 18,

            19, 20, 21, 22, 23, 24,

            7, 8, 9, 10, 11, 12};
        Integer_array rarr1{{4, 2, 3}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr1, oc::insert(arr1, arr2, 1, 0)));
        //EXPECT_TRUE(oc::all_equal(rarr1, oc::insert(arr1, arr2, 3, 0))); // assertion failure

        const int rdata2[] = {1, 2, 3, 13, 14, 15, 16, 17, 18, 4, 5, 6,

            7, 8, 9, 19, 20, 21, 22, 23, 24, 10, 11, 12};
        Integer_array rarr2{{2, 4, 3}, rdata2};
        EXPECT_TRUE(oc::all_equal(rarr2, oc::insert(arr1, arr2, 1, 1)));
        //EXPECT_TRUE(oc::all_equal(rarr2, oc::insert(arr1, arr2, 3, 1))); // assertion failure

        const int rdata3[] = {1, 13, 14, 15, 2, 3, 4, 16, 17, 18, 5, 6,

            7, 19, 20, 21, 8, 9, 10, 22, 23, 24, 11, 12};
        Integer_array rarr3{{2, 2, 6}, rdata3};
        EXPECT_TRUE(oc::all_equal(rarr3, oc::insert(arr1, arr2, 1, 2)));
        //EXPECT_TRUE(oc::all_equal(rarr3, oc::insert(arr1, arr2, 4, 2))); // assertion failure

        EXPECT_TRUE(oc::all_equal(arr1, oc::insert(arr1, Integer_array{}, 1, 0)));
        EXPECT_TRUE(oc::all_equal(arr2, oc::insert(Integer_array{}, arr2, 1, 0)));

        //EXPECT_TRUE(oc::all_equal(rarr1, oc::insert(arr1, arr2, 1, 3))); // assertion failure
        //const int invalid_data1[] = { 1 };
        //Integer_array invalid_arr1{ {1}, invalid_data1 };
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::insert(invalid_arr1, arr2, 1, 0))); // invalid operation, assertion required
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::insert(arr1, rarr2, 1, 0))); // invalid operation, assertion required
    }

    // nested array
    {
        oc::arrnd<Integer_array> inarr1({1, 2}, {Integer_array({1}, {1}), Integer_array({1, 1}, {6})});

        auto rnarr1 = oc::insert(inarr1,
            oc::arrnd<Integer_array>({1, 2}, {Integer_array({4}, {2, 3, 4, 5}), Integer_array({1, 4}, {7, 8, 9, 10})}),
            1);
        EXPECT_TRUE(oc::all_equal(rnarr1,
            oc::arrnd<Integer_array>(
                {1, 2}, {Integer_array({5}, {1, 2, 3, 4, 5}), Integer_array({5}, {6, 7, 8, 9, 10})})));

        EXPECT_TRUE(oc::all_equal(
            oc::insert(oc::arrnd<Integer_array>({1, 2}),
                oc::arrnd<Integer_array>({2}, {Integer_array({4}, {2, 3, 4, 5}), Integer_array({1, 4}, {7, 8, 9, 10})}),
                0),
            oc::arrnd<Integer_array>({1, 2}, {Integer_array({4}, {2, 3, 4, 5}), Integer_array({4}, {7, 8, 9, 10})})));

        auto rnarr2 = oc::insert<0>(inarr1,
            oc::arrnd<Integer_array>({1, 2}, {Integer_array({4}, {2, 3, 4, 5}), Integer_array({1, 4}, {7, 8, 9, 10})}),
            0);
        EXPECT_TRUE(oc::all_equal(rnarr2,
            oc::arrnd<Integer_array>({4},
                {Integer_array({4}, {2, 3, 4, 5}), Integer_array({1, 4}, {7, 8, 9, 10}), Integer_array({1}, {1}),
                    Integer_array({1, 1}, {6})})));

        oc::arrnd<Integer_array> inarr2({1, 2},
            {Integer_array({1, 2, 3}, {7, 8, 9, 10, 11, 12}), Integer_array({1, 2, 3}, {19, 20, 21, 22, 23, 24})});

        auto rnarr3 = oc::insert(inarr2,
            oc::arrnd<Integer_array>({1, 2},
                {Integer_array({1, 2, 3}, {1, 2, 3, 4, 5, 6}), Integer_array({1, 2, 3}, {13, 14, 15, 16, 17, 18})}),
            0, 0);
        EXPECT_TRUE(oc::all_equal(rnarr3,
            oc::arrnd<Integer_array>({1, 2},
                {Integer_array({2, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}),
                    Integer_array({2, 2, 3}, {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})})));

        auto rnarr4 = oc::insert<0>(inarr2, oc::arrnd<Integer_array>({1, 2}, {Integer_array{}, Integer_array{}}), 0, 0);
        EXPECT_TRUE(oc::all_equal(rnarr4,
            oc::arrnd<Integer_array>({2, 2},
                {Integer_array{}, Integer_array{}, Integer_array({1, 2, 3}, {7, 8, 9, 10, 11, 12}),
                    Integer_array({1, 2, 3}, {19, 20, 21, 22, 23, 24})})));

        EXPECT_TRUE(oc::all_equal(oc::insert(oc::arrnd<Integer_array>{},
                                      oc::arrnd<Integer_array>({1, 2}, {Integer_array{}, Integer_array{}}), 0, 0),
            oc::arrnd<Integer_array>({1, 2}, {Integer_array{}, Integer_array{}})));
    }

    // multiple insert cat like
    {
        // standard
        {
            oc::arrnd<int> arr = oc::insert(oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}),
                std::make_tuple(oc::arrnd<int>({4}, {7, 8, 9, 10}), 6),
                std::make_tuple(oc::arrnd<int>({4}, {11, 12, 13, 14}), 0));

            oc::arrnd<int> res({14}, {11, 12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

            EXPECT_TRUE(all_equal(arr, res));
        }

        // by axis
        {
            oc::arrnd<int> arr = oc::insert(oc::arrnd<int>({1, 1, 2}, {1, 2}),
                std::make_tuple(oc::arrnd<int>({2, 1, 2}, {3, 4, 5, 6}), 1, 0),
                std::make_tuple(oc::arrnd<int>({3, 1, 1}, {14, 15, 16}), 2, 2));

            oc::arrnd<int> res({3, 1, 3}, {1, 2, 14, 3, 4, 15, 5, 6, 16});

            EXPECT_TRUE(all_equal(arr, res));
        }
    }
}

TEST(arrnd_test, repeat)
{
    using namespace oc;

    // standard
    {
        auto arr = repeat(arrnd<int>({2, 2}, {1, 2, 3, 4}), 4);

        arrnd<int> res({16}, {1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4});

        EXPECT_TRUE(all_equal(arr, res));
    }

    // by axis
    {
        auto arr1 = repeat(arrnd<int>({1, 2, 2}, {1, 2, 3, 4}), {std::tuple(2, 2), std::tuple(3, 1), std::tuple(2, 0)});

        arrnd<int> res({2, 6, 4},
            {1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4, 1, 2, 1, 2,
                3, 4, 3, 4, 1, 2, 1, 2, 3, 4, 3, 4});

        EXPECT_TRUE(all_equal(arr1, res));

        arrnd<int> reps({3}, {2, 3, 2});
        arrnd<int> axes({3}, {2, 1, 0});

        auto z = zip(iter_pack(reps), iter_pack(axes));

        auto arr2 = repeat(arrnd<int>({1, 2, 2}, {1, 2, 3, 4}), z.begin(), z.end());

        EXPECT_TRUE(all_equal(arr2, res));

        auto arr3 = repeat(arrnd<int>({1, 2, 2}, {1, 2, 3, 4}), {2, 3, 2});

        EXPECT_TRUE(all_equal(arr3, res));
    }
}

//TEST(arrnd_test, cat)
//{
//    using namespace oc;
//
//    // standard cat
//    {
//        arrnd<int> arr
//            = cat(arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), std::make_pair(oc::arrnd<int>({4}, {7, 8, 9, 10}), 6),
//                std::make_pair(oc::arrnd<int>({4}, {11, 12, 13, 14}), 0));
//
//        arrnd<int> res({14}, {11, 12, 13, 14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
//
//        EXPECT_TRUE(all_equal(arr, res));
//    }
//
//    // cat by axis
//    {
//        arrnd<int> arr = cat(arrnd<int>({1, 1, 2}, {1, 2}), std::tuple(arrnd<int>({2, 1, 2}, {3, 4, 5, 6}), 1, 0),
//            std::tuple(arrnd<int>({3, 1, 1}, {14, 15, 16}), 2, 2));
//
//        arrnd<int> res({3, 1, 3}, {1, 2, 14, 3, 4, 15, 5, 6, 16});
//
//        EXPECT_TRUE(all_equal(arr, res));
//    }
//
//    //// repeat
//    //{
//    //    arrnd<int> arr = arrnd<int>({1, 2, 2}, {1, 2, 3, 4}).repeat({2, 1, 0});
//    //    std::cout << arr << "\n\n";
//    //}
//}

TEST(arrnd_test, remove)
{
    using Integer_array = oc::arrnd<int>;

    // No axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6};
        Integer_array arr1{{3, 1, 2}, data1};

        const int rdata1[] = {1, 2, 3, 6};
        Integer_array rarr1{{4}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr1, oc::remove(arr1, 3, 2)));

        //const int rdata2[] = { 1, 2, 3 };
        //Integer_array rarr2{ {3}, rdata2 };
        //EXPECT_TRUE(oc::all_equal(rarr2, oc::remove(arr1, 3, 4))); // assertion failure

        EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::remove(Integer_array{}, 3, 2)));
    }

    // Axis specified
    {
        const int data1[] = {1, 2, 3, 4, 5, 6,

            7, 8, 9, 10, 11, 12};
        Integer_array arr1{{2, 2, 3}, data1};

        const int rdata1[] = {7, 8, 9, 10, 11, 12};
        Integer_array rarr1{{1, 2, 3}, rdata1};
        EXPECT_TRUE(oc::all_equal(rarr1, oc::remove(arr1, 0, 1, 0)));
        //EXPECT_TRUE(oc::all_equal(Integer_array{}, oc::remove(arr1, 0, 3, 0))); // assertion failure

        const int rdata2[] = {1, 2, 3,

            7, 8, 9};
        Integer_array rarr2{{2, 1, 3}, rdata2};
        EXPECT_TRUE(oc::all_equal(rarr2, oc::remove(arr1, 1, 1, 1)));
        //EXPECT_TRUE(oc::all_equal(rarr2, oc::remove(arr1, 1, 2, 1))); // assertion failure

        const int rdata3[] = {3, 6,

            9, 12};
        Integer_array rarr3{{2, 2, 1}, rdata3};
        EXPECT_TRUE(oc::all_equal(rarr3, oc::remove(arr1, 0, 2, 2)));
        //const int rdata4[] = {
        //    1, 2,
        //    4, 5,

        //    7, 8,
        //    10, 11 };
        //Integer_array rarr4{ { 2, 2, 2 }, rdata4 };
        //EXPECT_TRUE(oc::all_equal(rarr4, oc::remove(arr1, 2, 2, 2))); // assertion failure

        //EXPECT_TRUE(oc::all_equal(rarr1, oc::remove(arr1, 0, 1, 3))); // assertion failure
    }

    // nested array
    {
        oc::arrnd<Integer_array> inarr1(
            {1, 2}, {Integer_array({5}, {1, 2, 3, 4, 5}), Integer_array({1, 5}, {6, 7, 8, 9, 10})});

        auto rnarr1 = oc::remove(inarr1, 2, 2);
        EXPECT_TRUE(oc::all_equal(
            rnarr1, oc::arrnd<Integer_array>({1, 2}, {Integer_array({3}, {1, 2, 5}), Integer_array({3}, {6, 7, 10})})));

        auto rnarr2 = oc::remove<0>(inarr1, 0, 1);
        EXPECT_TRUE(oc::all_equal(rnarr2, oc::arrnd<Integer_array>({1}, {Integer_array({1, 5}, {6, 7, 8, 9, 10})})));

        oc::arrnd<Integer_array> inarr2({1, 2},
            {Integer_array({2, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}),
                Integer_array({2, 2, 3}, {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})});

        auto rnarr3 = oc::remove(inarr2, 0, 1, 0);
        EXPECT_TRUE(oc::all_equal(rnarr3,
            oc::arrnd<Integer_array>({1, 2},
                {Integer_array({1, 2, 3}, {7, 8, 9, 10, 11, 12}),
                    Integer_array({1, 2, 3}, {19, 20, 21, 22, 23, 24})})));

        auto rnarr4 = oc::remove<0>(inarr2, 1, 1, 1);
        EXPECT_TRUE(oc::all_equal(rnarr4,
            oc::arrnd<Integer_array>({1, 1}, {Integer_array({2, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})})));
    }

    // multiple remove cat like
    {
        // standard
        {
            oc::arrnd<int> arr = oc::remove(oc::arrnd<int>({14}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14}),
                std::make_tuple(0, 4), std::make_tuple(4, 6));

            oc::arrnd<int> res({4}, {5, 6, 7, 8});

            EXPECT_TRUE(all_equal(arr, res));
        }

        // by axis
        {
            oc::arrnd<int> arr = oc::remove(oc::arrnd<int>({3, 1, 3}, {1, 2, 14, 3, 4, 15, 5, 6, 16}),
                std::make_tuple(2, 1, 2), std::make_tuple(1, 2, 0));

            oc::arrnd<int> res({1, 1, 2}, {1, 2});

            EXPECT_TRUE(all_equal(arr, res));
        }
    }
}

TEST(arrnd_test, complex_array)
{
    using Integer_array = oc::arrnd<int>;

    const int data[2][2][2][3][3]{{{{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}},

                                       {{10, 11, 12}, {13, 14, 15}, {16, 17, 18}}},

                                      {{{19, 20, 21}, {22, 23, 24}, {25, 26, 27}},

                                          {{28, 29, 30}, {31, 32, 33}, {34, 35, 36}}}},

        {{{{37, 38, 39}, {40, 41, 42}, {43, 44, 45}},

             {{46, 47, 48}, {49, 50, 51}, {52, 53, 54}}},

            {{{55, 56, 57}, {58, 59, 60}, {61, 62, 63}},

                {{64, 65, 66}, {67, 68, 69}, {70, 71, 72}}}}};
    const std::int64_t dims[]{2, 2, 2, 3, 3};
    Integer_array arr{dims, oc::const_array_cast<int>(data)};

    int sdata1[2][1][1][2][1]{{{{{11}, {17}}}}, {{{{47}, {53}}}}};
    const std::int64_t sdims1[]{2, 1, 1, 2, 1};
    Integer_array sarr1{sdims1, oc::array_cast<int>(sdata1)};

    EXPECT_TRUE(oc::all_equal(sarr1, (arr[{{0, 2}, {0, 1}, {1, 2}, {0, 3, 2}, {1, 3, 2}}])));

    int sdata2[1][1][1][1][1]{{{{{17}}}}};
    const std::int64_t sdims2[]{1, 1, 1, 1, 1};
    Integer_array sarr2{sdims2, oc::array_cast<int>(sdata2)};

    EXPECT_TRUE(oc::all_equal(sarr2, (sarr1[{{0, 1}, {0, 1}, {0, 1}, {1, 2}, {0, 1}}])));

    {
        const int edata1[1][2][1]{{{11}, {17}}};
        oc::arrnd<int> rarr1{{1, 2, 1}, oc::const_array_cast<int>(edata1)};
        auto earr1 = arr[{{0, 2}, {0, 1}, {1, 2}, {0, 3, 2}, {1, 3, 2}}][oc::interval<std::int64_t>{0, 1}]
                        [oc::interval<std::int64_t>{0, 1}];
        EXPECT_TRUE(oc::all_equal(rarr1, earr1));

        const int edata2[1][2][1]{{{47}, {53}}};
        oc::arrnd<int> rarr2{{1, 2, 1}, oc::const_array_cast<int>(edata2)};
        auto earr2 = arr[{{0, 2}, {0, 1}, {1, 2}, {0, 3, 2}, {1, 3, 2}}][oc::interval<std::int64_t>{1, 2}]
                        [oc::interval<std::int64_t>{0, 1}];
        EXPECT_TRUE(oc::all_equal(rarr2, earr2));

        const int edata3[]{47, 53};
        oc::arrnd<int> rarr3{{1, 1, 1, 2, 1}, edata3};
        auto earr3 = arr[{{0, 2}, {0, 1}, {1, 2}, {0, 3, 2}, {1, 3, 2}}][{{1, 2}}][{{0, 1}}];
        EXPECT_TRUE(oc::all_equal(rarr3, earr3));
    }
}

TEST(arrnd_test, ostream_operator)
{
    // empty array
    {
        std::stringstream ss;
        ss << oc::arrnd<int>{};
        EXPECT_EQ("[]", ss.str());

        // json
        {
            ss.str(std::string{});
            ss << oc::arrnd_json << oc::arrnd<int>{};
            EXPECT_EQ("{\n"
                      "    \"base_type\": \"int\"\n"
                      "    \"header\": \"empty\",\n"
                      "    \"values\": \"empty\"\n"
                      "}",
                ss.str());
        }
    }

    // one dimensional array
    {
        std::stringstream ss;
        oc::arrnd<int> arr{{6}, {1, 2, 3, 4, 5, 6}};
        ss << arr;
        EXPECT_EQ("[1 2 3 4 5 6]", ss.str());

        {
            ss.str(std::string{});
            ss << oc::arrnd_json << arr;
            EXPECT_EQ("{\n"
                      "    \"base_type\": \"int\"\n"
                      "    \"header\": \"numel: 6\\ndims: [6]\\nstrides: [1]\\noffset: 0\\nlast_index: 5\\nflags: "
                      "vector(1), matrix(0), row(0), column(0), scalar(0), slice(0)\",\n"
                      "    \"values\": \"[1 2 3 4 5 6]\"\n"
                      "}",
                ss.str());
        }
    }

    // multi dimensional array
    {
        std::stringstream ss;
        ss << oc::arrnd<int>{{2, 1, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}};
        EXPECT_EQ("[[[[1 2 3]\n"
                  "   [4 5 6]]]\n"
                  " [[[7 8 9]\n"
                  "   [10 11 12]]]]",
            ss.str());
    }

    // subarray
    {
        std::stringstream ss;
        oc::arrnd<int> arr{{2, 1, 2, 3},
            {1, 2, 3, 4, 5, 6,

                7, 8, 9, 10, 11, 12}};
        auto slice = arr[{
            oc::interval<>::at(1), oc::interval<>::full(), oc::interval<>::full(), oc::interval<>::between(0, 3, 2)}];
        ss << slice;
        EXPECT_EQ("[[[[7 9]\n"
                  "   [10 12]]]]",
            ss.str());

        // array header ostream operator
        {
            ss.str(std::string{});
            ss << arr.header();
            EXPECT_EQ("numel: 12\n"
                      "dims: [2 1 2 3]\n"
                      "strides: [6 6 3 1]\n"
                      "offset: 0\n"
                      "last_index: 11\n"
                      "flags: vector(0), matrix(0), row(0), column(0), scalar(0), slice(0)",
                ss.str());

            ss.str(std::string{});
            ss << slice.squeeze().header();
            EXPECT_EQ("numel: 4\n"
                      "dims: [2 2]\n"
                      "strides: [3 2]\n"
                      "offset: 6\n"
                      "last_index: 11\n"
                      "flags: vector(0), matrix(1), row(0), column(0), scalar(0), slice(1)",
                ss.str());
        }
    }

    // nested array
    {
        std::stringstream ss;
        oc::arrnd<oc::arrnd<oc::arrnd<int>>> arr({2},
            {oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>(), oc::arrnd<int>()}),
                oc::arrnd<oc::arrnd<int>>({2, 2},
                    {oc::arrnd<int>({5}, {1, 2, 3, 4, 5}),
                        oc::arrnd<int>({2, 1, 2, 3}, {6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17}), oc::arrnd<int>(),
                        oc::arrnd<int>({4, 1}, {18, 19, 20, 21})})});
        ss << arr;
        EXPECT_EQ("{{[]\n"
                  "  []}\n"
                  " {[1 2 3 4 5]\n"
                  "  [[[[6 7 8]\n"
                  "     [9 10 11]]]\n"
                  "   [[[12 13 14]\n"
                  "     [15 16 17]]]]\n"
                  "  []\n"
                  "  [[18]\n"
                  "   [19]\n"
                  "   [20]\n"
                  "   [21]]}}",
            ss.str());

        {
            ss.str(std::string{});
            ss << oc::arrnd_json << arr;
            EXPECT_EQ(
                "{\n"
                "    \"base_type\": \"int\"\n"
                "    \"header\": \"numel: 2\\ndims: [2]\\nstrides: [1]\\noffset: 0\\nlast_index: 1\\nflags: vector(1), "
                "matrix(0), row(0), column(0), scalar(0), slice(0)\",\n"
                "    \"arrays\": [\n"
                "        {\n"
                "            \"header\": \"numel: 2\\ndims: [1 2]\\nstrides: [2 1]\\noffset: 0\\nlast_index: "
                "1\\nflags: vector(0), matrix(1), row(1), column(0), scalar(0), slice(0)\",\n"
                "            \"arrays\": [\n"
                "                {\n"
                "                    \"header\": \"empty\",\n"
                "                    \"values\": \"empty\"\n"
                "                },\n"
                "                {\n"
                "                    \"header\": \"empty\",\n"
                "                    \"values\": \"empty\"\n"
                "                }\n"
                "            ]\n"
                "        },\n"
                "        {\n"
                "            \"header\": \"numel: 4\\ndims: [2 2]\\nstrides: [2 1]\\noffset: 0\\nlast_index: "
                "3\\nflags: vector(0), matrix(1), row(0), column(0), scalar(0), slice(0)\",\n"
                "            \"arrays\": [\n"
                "                {\n"
                "                    \"header\": \"numel: 5\\ndims: [5]\\nstrides: [1]\\noffset: 0\\nlast_index: "
                "4\\nflags: vector(1), matrix(0), row(0), column(0), scalar(0), slice(0)\",\n"
                "                    \"values\": \"[1 2 3 4 5]\"\n"
                "                },\n"
                "                {\n"
                "                    \"header\": \"numel: 12\\ndims: [2 1 2 3]\\nstrides: [6 6 3 1]\\noffset: "
                "0\\nlast_index: 11\\nflags: vector(0), matrix(0), row(0), column(0), scalar(0), slice(0)\",\n"
                "                    \"values\": \"[[[[6 7 8]\\n   [9 10 11]]]\\n [[[12 13 14]\\n   [15 16 17]]]]\"\n"
                "                },\n"
                "                {\n"
                "                    \"header\": \"empty\",\n"
                "                    \"values\": \"empty\"\n"
                "                },\n"
                "                {\n"
                "                    \"header\": \"numel: 4\\ndims: [4 1]\\nstrides: [1 1]\\noffset: 0\\nlast_index: "
                "3\\nflags: vector(0), matrix(1), row(0), column(1), scalar(0), slice(0)\",\n"
                "                    \"values\": \"[[18]\\n [19]\\n [20]\\n [21]]\"\n"
                "                }\n"
                "            ]\n"
                "        }\n"
                "    ]\n"
                "}",
                ss.str());
        }
    }
}
