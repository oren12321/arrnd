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

TEST(arrnd_test, matmul)
{
    using namespace oc;

    {
        arrnd<int> arr1({3, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18});
        arrnd<double> arr2({3, 1}, {1, 2, 3});

        auto res = matmul(arr1, arr2);

        EXPECT_TRUE(all_equal(res, arrnd<double>({3, 2, 1}, {14, 32, 50, 68, 86, 104})));
        EXPECT_TRUE(all_equal(res, arr1.as_pages() * arr2.as_pages()));

        auto arr1p = arr1.as_pages();
        arr1p *= arr2.as_pages();

        EXPECT_TRUE(all_equal(res, arr1p));
    }

    {
        arrnd<arrnd<arrnd<int>>> arr1({1, 1},
            {arrnd<arrnd<int>>(
                {1}, {arrnd<int>({3, 2, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18})})});
        arrnd<arrnd<arrnd<double>>> arr2(
            {1, 1}, {arrnd<arrnd<double>>({1}, {arrnd<double>({3, 1, 3, 1}, {1, 2, 3, 4, 5, 6, 7, 8, 9})})});

        auto res = matmul(arr1, arr2);

        EXPECT_TRUE(all_equal(res,
            arrnd<arrnd<arrnd<double>>>(
                {1, 1}, {arrnd<arrnd<double>>({1}, {arrnd<double>({3, 2, 1}, {14, 32, 122, 167, 338, 410})})})));

        /*EXPECT_TRUE(
            all_equal(res, arrnd<arrnd<double>>({1}, {arrnd<double>({3, 2, 1}, {14, 32, 122, 167, 338, 410})})));*/
    }

    {
        arrnd<int> arr1({1, 2, 3}, {1, 2, 3, 4, 5, 6});
        arrnd<double> arr2({1, 3, 1}, {1.5, 2.5, 3.5});
        arrnd<long> arr3({1, 1, 2}, {1, 2});

        auto res = matmul(arr1, arr2, arr3);

        EXPECT_TRUE(all_equal(res, arrnd<double>({1, 2, 2}, {17.0, 34.0, 39.5, 79.0})));
    }
}

TEST(arrnd_test, det)
{
    using namespace oc;
    // 240, -16
    arrnd<arrnd<int>> arr({2},
        {arrnd<int>({2, 4, 4},
             {4, 3, 2, 2, 0, 1, -3, 3, 0, -1, 3, 3, 0, 3, 1, 1, 1, 1, 1, -1, 1, 1, -1, 1, 1, -1, 1, 1, -1, 1, 1, 1}),
            arrnd<int>({3, 3}, {2, -3, 1, 2, 0, -1, 1, 4, 5})});

    EXPECT_TRUE(all_equal(det(arr), arrnd<arrnd<int>>({2}, {arrnd<int>({2, 1}, {-240, -16}), arrnd<int>({1}, {49})})));
}

TEST(arrnd_test, inverse)
{
    using namespace oc;

    arrnd<arrnd<double>> arr({3},
        {arrnd<double>({2, 2, 2}, {1, 2, 5, 6, 9, 10, 13, 14}), arrnd<double>({2, 2}, {1, 0, 0, 1}),
            arrnd<double>({3, 3}, {3, 0, 2, 2, 0, -2, 0, 1, 1})});

    EXPECT_TRUE(all_close(inverse(arr),
        arrnd<arrnd<double>>({3},
            {arrnd<double>({2, 2, 2}, {-1.5, 0.5, 1.25, -0.25, -3.5, 2.5, 3.25, -2.25}),
                arrnd<double>({2, 2}, {1, 0, 0, 1}),
                arrnd<double>({3, 3}, {0.2, 0.2, 0.0, -0.2, 0.3, 1.0, 0.2, -0.3, 0.0})})));
}

TEST(arrnd_test, solve)
{
    using namespace oc;

    arrnd<arrnd<double>> arr({2},
        {arrnd<double>({2, 2}, {1, 2, 3, 5}),
            arrnd<double>({2, 3, 3}, {2, 1, 1, -1, 1, -1, 1, 2, 3, 1, 2, -2, 2, 1, -5, 1, -4, 1})});

    arrnd<arrnd<double>> b({2}, {arrnd<double>({2, 1}, {1, 2}), arrnd<double>({2, 3, 1}, {2, 3, -10, -15, -21, 18})});

    arrnd<arrnd<double>> x({2}, {arrnd<double>({2, 1}, {-1, 1}), arrnd<double>({2, 3, 1}, {3, 1, -5, -1, -4, 3})});

    EXPECT_TRUE(all_close(solve(arr, b), x));
}

TEST(arrnd_test, DISABLED_cholesky)
{
    using namespace oc;

    arrnd<double> arr({3, 3}, {4, 12, -16, 12, 37, -43, -16, -43, 98});
    arrnd<double> l({3, 3}, {2, 0, 0, 6, 1, 0, -8, 5, 3});

    EXPECT_TRUE(all_close(cholesky(arr), l));
}

TEST(arrnd_test, DISABLED_lu)
{
    using namespace oc;

    arrnd<double> arr({3, 3}, {2, -1, -2, -4, 6, 3, -4, -2, 8});

    auto res = lu(arr)(0);

    arrnd<double> l({3, 3}, {1, 0, 0, -2, 1, 0, -2, -1, 1});
    arrnd<double> u({3, 3}, {2, -1, -2, 0, 4, -1, 0, 0, 3});

    EXPECT_TRUE(all_close(std::get<0>(res), l));
    EXPECT_TRUE(all_close(std::get<1>(res), u));
}

TEST(arrnd_test, DISABLED_qr)
{
    using namespace oc;

    {
        arrnd<double> arr({4, 3}, {-1, -1, 1, 1, 3, 3, -1, -1, 5, 1, 3, 7});

        auto [q, r] = qr(arr)(0);

        //arrnd<double> q({4, 3}, {-0.5, 0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5, 0.5, 0.5, 0.5, 0.5});
        //arrnd<double> r({3, 3}, {2, 4, 2, 0, 2, 8, 0, 0, 4});

        //std::cout <<q<< "\n";
        //std::cout << r << "\n\n";

        //std::cout << std::get<0>(res) << "\n";
        //std::cout << std::get<1>(res) << "\n\n";
        //std::cout << std::get<0>(res).matmul(std::get<1>(res)) << "\n\n";

        EXPECT_TRUE(all_close(q,
            arrnd<double>(
                {4, 4}, {-0.5, 0.5, -0.5, 0.5, 0.5, 0.5, -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5, 0.5, 0.5})));
        EXPECT_TRUE(all_close(r,
            arrnd<double>({4, 3},
                {2.0, 4.0, 2.0, -3.05311e-16, 2.0, 8.0, 2.77556e-16, -3.88578e-16, 4.0, -3.33067e-16, -1.11022e-16, 8.88178e-16})));

        EXPECT_TRUE(all_close(matmul(q, r), arr));
    }

    {
        arrnd<double> arr({3, 3}, {3, 2, 4, 2, 0, 2, 4, 2, 3});

        auto [q, r] = qr(arr)(0);

        //arrnd<double> q(
        //    {3, 3}, {0.557086, 0.495188, 0.666667, 0.371391, -0.866578, 0.333333, 0.742781, 0.0618984, -0.666667});
        //arrnd<double> r({3, 3}, {5.38516, 2.59973, 5.19947, 0, 1.11417, 0.433289, 0, 0, 1.33333});

        EXPECT_TRUE(all_close(q,
            arrnd<double>({3, 3},
                {0.557086, 0.495188, 0.666667, 0.371391, -0.866578, 0.333333, 0.742781, 0.0618984, -0.666667})));
        EXPECT_TRUE(all_close(r,
            arrnd<double>({3, 3},
                {5.38516, 2.59973, 5.19947, -7.87957e-16, 1.11417, 0.433289, -1.39389e-15, 5.55112e-17, 1.33333})));

        EXPECT_TRUE(all_close(matmul(q, r), arr));
    }
}

TEST(arrnd_test, DISABLED_hess)
{
    using namespace oc;

    arrnd<double> arr({4, 4}, {2., 3., 4., 5., 4., 2., 5., 6., 5., 7., 2., 7., 6., 8., 10., 2.});

    auto [q, h] = hess(arr)(0);

    //std::cout << q << "\n";
    //std::cout << h << "\n";

    EXPECT_TRUE(all_close(q,
        arrnd<double>({4, 4},
            {1., 0., 0., 0., 0., -0.455842, 0.863773, 0.214719, 0., -0.569803, -0.0978779, -0.815932, 0., -0.683763, -0.494284,
                0.536797})));
    EXPECT_TRUE(all_close(h,
        arrnd<double>({4, 4},
            {2., -7.06556, -0.271611, 0.0644157, -8.77496, 16.1039, -3.95445, 1.79851, -1.18654e-15, -0.604838, -4.16936,
                1.92719, -4.12133e-16, 1.94289e-16, 0.331741, -5.93453})));

    EXPECT_TRUE(all_close(matmul(q, h, transpose(q, {1, 0})), arr));
}

TEST(arrnd_test, DISABLED_schur)
{
    using namespace oc;

    {
        arrnd<double> arr({4, 4}, {2., 3., 4., 5., 4., 2., 5., 6., 5., 7., 2., 7., 6., 8., 10., 2.});

        auto [u, s] = schur(arr)(0);

        //std::cout << u << "\n\n";
        //std::cout << s << "\n\n";
        //std::cout << u.matmul(s, u.transpose({1, 0})) << "\n\n";

        EXPECT_TRUE(all_close(u,
            arrnd<double>({4, 4},
                {0.370278, 0.925419, -0.0802532, -0.00727113, 0.443715, -0.251355, -0.857434, 0.0688785, 0.526578,
                    -0.193754, 0.26634, -0.783733, 0.62348, -0.207072, 0.432931, 0.617224})));
        EXPECT_TRUE(all_close(s,
            arrnd<double>({4, 4},
                {19.7025, 1.45867, -2.84783, -2.11022, 8.00528e-15, -1.35476, 1.32379, 0.801807, 0., 0., -4.07002,
                    -1.70698, 0., 0., 1.47042e-15, -6.27774})));

        EXPECT_TRUE(all_close(matmul(u, s, transpose(u, {1, 0})), arr));
    }

    {
        arrnd<double> rarr({3, 3}, {3, 1, 1, 0, 2, 0, -2, 1, 1});

        auto [ur, sr] = schur(rarr)(0);

        arrnd<std::complex<double>> carr({3, 3}, {3, 1, 1, 0, 2, 0, -2, 1, 1});

        auto [uc, sc] = schur(carr)(0);

        EXPECT_TRUE(all_close(rarr, real(matmul(uc, sc, conj(transpose(uc, {1, 0}))))));
    }
}

TEST(arrnd_test, DISABLED_eig)
{
    using namespace oc;

    arrnd<double> arr({4, 4}, {2., 3., 4., 5., 4., 2., 5., 6., 5., 7., 2., 7., 6., 8., 10., 2.});

    auto [l, v] = eig(arr)(0);

    auto o = zeros<arrnd<double>>({4, 1});

    //    std::cout << l << "\n\n";
    //std::cout << v << "\n\n";

    EXPECT_TRUE(all_close(l, arrnd<double>({4, 1}, {19.7025, -1.35476, -4.07002, -6.27774})));
    EXPECT_TRUE(all_close(v,
        arrnd<double>({4, 4},
            {0.370278, -0.897618, -0.424033, 0.258014, 0.443715, 0.281418, -0.595486, 0.314169, 0.526578, 0.229681,
                0.391643, 0.306355, 0.62348, 0.249664, 0.558755, -0.86074})));

    for (int i = 0; i < l.header().numel(); ++i) {
        EXPECT_TRUE(
            all_close(matmul(arr - l[i] * eye<arrnd<double>>({4, 4}), v[{interval<>::full(), interval<>::at(i)}]), o));
    }
}

TEST(arrnd_test, DISABLED_svd)
{
    using namespace oc;

    arrnd<double> arr({4, 4}, {2., 3., 4., 5., 4., 2., 5., 6., 5., 7., 2., 7., 6., 8., 10., 2.});

    auto [u, s, v] = svd(arr)(0);

    EXPECT_TRUE(all_close(arr, matmul(u, s, v.transpose({1, 0}))));
    //std::cout << u << "\n\n";
    //std::cout << s << "\n\n";
    //std::cout << v << "\n\n";
}

TEST(arrnd_test, zeros)
{
    using namespace oc;

    EXPECT_TRUE(all_equal(zeros<arrnd<int>>({3, 1, 2}), arrnd<int>({3, 1, 2}, 0)));
}

TEST(arrnd_test, eye)
{
    using namespace oc;

    EXPECT_TRUE(all_equal(eye<arrnd<int>>({3, 3}), arrnd<int>({3, 3}, {1, 0, 0, 0, 1, 0, 0, 0, 1})));

    EXPECT_TRUE(all_equal(eye<arrnd<int>>({3, 4}), arrnd<int>({3, 4}, {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0})));
    EXPECT_TRUE(all_equal(eye<arrnd<int>>({4, 3}), arrnd<int>({4, 3}, {1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0})));

    EXPECT_TRUE(all_equal(eye<arrnd<int>>({2, 2, 2}), arrnd<int>({2, 2, 2}, {1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1})));
}

TEST(arrnd_test, diag)
{
    using namespace oc;

    {
        arrnd<arrnd<int>> arr({3},
            {arrnd<int>({3, 5}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({5, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({3, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9})});

        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::from_matrix),
            arrnd<arrnd<int>>(
                {3}, {arrnd<int>({3}, {1, 7, 13}), arrnd<int>({3}, {1, 5, 9}), arrnd<int>({3}, {1, 5, 9})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::from_matrix, 1),
            arrnd<arrnd<int>>({3}, {arrnd<int>({3}, {2, 8, 14}), arrnd<int>({2}, {2, 6}), arrnd<int>({2}, {2, 6})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::from_matrix, 2),
            arrnd<arrnd<int>>({3}, {arrnd<int>({3}, {3, 9, 15}), arrnd<int>({1}, {3}), arrnd<int>({1}, {3})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::from_matrix, -1),
            arrnd<arrnd<int>>({3}, {arrnd<int>({2}, {6, 12}), arrnd<int>({3}, {4, 8, 12}), arrnd<int>({2}, {4, 8})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::from_matrix, -2),
            arrnd<arrnd<int>>({3}, {arrnd<int>({1}, {11}), arrnd<int>({3}, {7, 11, 15}), arrnd<int>({1}, {7})})));
    }

    {
        arrnd<arrnd<int>> arr({3}, {arrnd<int>({2}, {6, 12}), arrnd<int>({3}, {4, 8, 12}), arrnd<int>({2}, {4, 8})});

        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::to_matrix),
            arrnd<arrnd<int>>({3},
                {arrnd<int>({2, 2}, {6, 0, 0, 12}), arrnd<int>({3, 3}, {4, 0, 0, 0, 8, 0, 0, 0, 12}),
                    arrnd<int>({2, 2}, {4, 0, 0, 8})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::to_matrix, 1),
            arrnd<arrnd<int>>({3},
                {arrnd<int>({3, 3}, {0, 6, 0, 0, 0, 12, 0, 0, 0}),
                    arrnd<int>({4, 4}, {0, 4, 0, 0, 0, 0, 8, 0, 0, 0, 0, 12, 0, 0, 0, 0}),
                    arrnd<int>({3, 3}, {0, 4, 0, 0, 0, 8, 0, 0, 0})})));
        EXPECT_TRUE(all_equal(diag(arr, arrnd_diag_type::to_matrix, -1),
            arrnd<arrnd<int>>({3},
                {arrnd<int>({3, 3}, {0, 0, 0, 6, 0, 0, 0, 12, 0}),
                    arrnd<int>({4, 4}, {0, 0, 0, 0, 4, 0, 0, 0, 0, 8, 0, 0, 0, 0, 12, 0}),
                    arrnd<int>({3, 3}, {0, 0, 0, 4, 0, 0, 0, 8, 0})})));
    }
}

TEST(arrnd_test, tril_and_triu)
{
    using namespace oc;

    arrnd<arrnd<int>> arr({3},
        {arrnd<int>({3, 5}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
            arrnd<int>({5, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
            arrnd<int>({3, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9})});

    EXPECT_TRUE(all_equal(tril(arr),
        arrnd<arrnd<int>>({3},
            {arrnd<int>({3, 5}, {1, 0, 0, 0, 0, 6, 7, 0, 0, 0, 11, 12, 13, 0, 0}),
                arrnd<int>({5, 3}, {1, 0, 0, 4, 5, 0, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({3, 3}, {1, 0, 0, 4, 5, 0, 7, 8, 9})})));
    EXPECT_TRUE(all_equal(tril(arr, 1),
        arrnd<arrnd<int>>({3},
            {arrnd<int>({3, 5}, {1, 2, 0, 0, 0, 6, 7, 8, 0, 0, 11, 12, 13, 14, 0}),
                arrnd<int>({5, 3}, {1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({3, 3}, {1, 2, 0, 4, 5, 6, 7, 8, 9})})));
    EXPECT_TRUE(all_equal(tril(arr, 2),
        arrnd<arrnd<int>>({3},
            {arrnd<int>({3, 5}, {1, 2, 3, 0, 0, 6, 7, 8, 9, 0, 11, 12, 13, 14, 15}),
                arrnd<int>({5, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({3, 3}, {1, 2, 3, 4, 5, 6, 7, 8, 9})})));
    EXPECT_TRUE(all_equal(tril(arr, -1),
        arrnd<arrnd<int>>({3},
            {arrnd<int>({3, 5}, {0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 11, 12, 0, 0, 0}),
                arrnd<int>({5, 3}, {0, 0, 0, 4, 0, 0, 7, 8, 0, 10, 11, 12, 13, 14, 15}),
                arrnd<int>({3, 3}, {0, 0, 0, 4, 0, 0, 7, 8, 0})})));
    EXPECT_TRUE(all_equal(tril(arr, -2),
        arrnd<arrnd<int>>({3},
            {arrnd<int>({3, 5}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0}),
                arrnd<int>({5, 3}, {0, 0, 0, 0, 0, 0, 7, 0, 0, 10, 11, 0, 13, 14, 15}),
                arrnd<int>({3, 3}, {0, 0, 0, 0, 0, 0, 7, 0, 0})})));

    EXPECT_TRUE(all_equal(triu(arr) + tril(arr, -1), arr));
    EXPECT_TRUE(all_equal(triu(arr, 1) + tril(arr), arr));
    EXPECT_TRUE(all_equal(triu(arr, 2) + tril(arr, 1), arr));
    EXPECT_TRUE(all_equal(triu(arr, -1) + tril(arr, -2), arr));
    EXPECT_TRUE(all_equal(triu(arr, -2) + tril(arr, -3), arr));
}

TEST(arrnd_test, is_banded)
{
    using namespace oc;

    {
        arrnd<int> arr1({5, 5}, {2, 3, 0, 0, 0, 1, -2, -3, 0, 0, 0, -1, 2, 3, 0, 0, 0, 1, -2, -3, 0, 0, 0, -1, 2});

        EXPECT_TRUE(is_banded(arr1, 1, 1)(0));
        EXPECT_FALSE(is_banded(arr1, 0, 1)(0));
        EXPECT_TRUE(is_banded(arr1, 1, 2)(0));
    }

    {
        arrnd<double> arr1(
            {5, 5}, {2., 3., 0., 1e-15, 0., 1., -2., -3., 0., 0., 0., -1., 2., 3., 0., -1e-20, 0., 1., -2., -3., 0., 0., 0., -1., 2.});

        EXPECT_TRUE(is_banded(arr1, 1, 1)(0));
        EXPECT_FALSE(is_banded(arr1, 0, 1)(0));
        EXPECT_TRUE(is_banded(arr1, 1, 2)(0));
    }
}

TEST(arrnd_test, arrnd_filter_proxy)
{
    using namespace oc;

    // indices
    {
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});

        arr({0, 2, 4}) = 0;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {0, 2, 0, 4, 0, 6})));

        arr({0, 4, 2}) = arrnd<double>({5}, {1.1, 2.2, 3.3, 4.4, 5.5});
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {1, 2, 3, 4, 2, 6})));
    }

    // mask
    {
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});

        arr(arr > 3) = 0;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {1, 2, 3, 0, 0, 0})));

        arr(arr <= 3 && arr > 0) = arrnd<int>({6}, {6, 5, 4, 3, 2, 1});
        //std::cout << arr << "\n";
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {6, 5, 4, 0, 0, 0})));
    }

    // predicate
    {
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});

        arr([](int n) {
            return n >= 5;
        }) = 0;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {1, 2, 3, 4, 0, 0})));

        arr([](int n) {
            return n <= 1;
        }) = arrnd<int>({6}, {6, 5, 4, 3, 2, 1});
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {6, 2, 3, 4, 5, 4})));
    }

    // math operations
    {
        arrnd<int> arr({6}, {1, 2, 3, 4, 5, 6});

        arr({0, 2, 4}) += 1;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({6}, {2, 2, 4, 4, 6, 6})));

        arr(arr > 4) -= 1;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({6}, {2, 2, 4, 4, 5, 5})));

        arr([](int n) {
            return n == 2;
        }) *= 1.2;
        EXPECT_TRUE(all_equal(arr, arrnd<int>({6}, {2, 2, 4, 4, 5, 5})));

        arr(arr == 4) += arrnd<int>({2}, {1, 2});
        EXPECT_TRUE(all_equal(arr, arrnd<int>({6}, {2, 2, 5, 6, 5, 5})));
    }

    //arrnd<int> arr({1, 5}, {1, 2, 3, 4, 5});
    //arrnd<int> inds({1, 2}, {2, 0});
    //arrnd<int> vals({2, 2}, {1000, 2000, 3000, 4000});

    //arrnd_filter_proxy(arr, inds) = vals;
    //arr(
    //    [](int n, int factor) {
    //        return n * factor > 1000;
    //    },
    //    10)
    //    = vals;
    //std::cout << arr << "\n";

    //cg[{0, 0}] = 100;
    //apply(cg, [](int n) {
    //    return n * 2;
    //});

    // requirements:
    // - arr(filter) creates arrnd_proxy (no option for additional filter)
    //   filter could be indices or mask
    // - any cast from arrnd_proxy to its arrnd activates the proxy filter and returns an arrnd
    // - what about the case that arrnd_proxy is begin passed to function?
    //   - arrnd_proxy is not arrnd_compliant, so it shouldn't be passed to a function
    //   - it can't be copied or assigned to, and can only be used as an assigned rvalue
    //   - in case that it should be used in function, it should be casted to Arrnd,
    //     which should activate the arrnd_proxy constraint
    // - proxy is mostly being used for assignment according to filter
    // - if assignment is not required, the recommended way is to use find
    //   e.g.
    //          arr(filter) = 100 -> arrnd
    //          arr(filter) -> proxy => to activate: arrnd arr = arr(filter) or static_cast<arrnd>(arr(filter))
    // - how
    //
    //
    // options
    // - arr(filter) -> acg
    // - auto x = arr(filter) => decltype(x) == acg
    // - arrnd x = arr(filter) => decltype(x) == acg::Arrnd (filter result, i.e cast result from acg to Arrnd)
    // - auto x = arr(filter) = <value,array> => decltype(x) == acg::Arrnd

    //arrnd_filter_proxy(arr, inds) = vals;
    // cg = 100; // only rvalue assignment
    //auto x = arrnd_filter_proxy(arr, arr > 10) = 1000)(arr > 2);
    //arrnd<int> p = arr(arr > 10);
    //std::cout << arr << "\n";
    //std::cout << p << "\n";
}

TEST(arrnd_test, squeeze)
{
    using namespace oc;

    {
        arrnd<int> arr;
        auto sarr = squeeze(arr);
        EXPECT_TRUE(all_equal(sarr, arrnd<int>()));
        EXPECT_FALSE(sarr.header().is_slice());
    }

    {
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});
        auto sarr = squeeze(arr);
        EXPECT_TRUE(all_equal(sarr, arr));
        EXPECT_FALSE(sarr.header().is_slice());
    }

    {
        arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});
        auto sarr = squeeze(arr);
        EXPECT_TRUE(all_equal(sarr, arrnd<int>({3, 2}, {1, 2, 3, 4, 5, 6})));
        EXPECT_TRUE(sarr.header().is_slice());
    }

    {
        arrnd<int> arr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
        arrnd<int> slice = arr[{interval<>::at(1), interval<>::full(), interval<>::between(0, 2)}];

        EXPECT_TRUE(all_equal(slice, arrnd<int>({1, 2, 2}, {9, 10, 13, 14})));

        arrnd<int> sarr = squeeze(slice);
        EXPECT_TRUE(all_equal(sarr, arrnd<int>({2, 2}, {9, 10, 13, 14})));
        EXPECT_TRUE(sarr.header().is_slice());
    }

    {
        arrnd<arrnd<int>> arr(
            {1, 2}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({3, 2, 1}, {7, 8, 9, 10, 11, 12})});

        auto sarr1 = squeeze<0>(arr);
        EXPECT_TRUE(all_equal(sarr1,
            arrnd<arrnd<int>>(
                {2}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({3, 2, 1}, {7, 8, 9, 10, 11, 12})})));
        EXPECT_TRUE(sarr1.header().is_slice());

        auto sarr2 = squeeze(arr);
        EXPECT_TRUE(all_equal(sarr2,
            arrnd<arrnd<int>>(
                {1, 2}, {arrnd<int>({3, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({3, 2}, {7, 8, 9, 10, 11, 12})})));
        EXPECT_TRUE((sarr2[{0, 0}].header().is_slice() && sarr2[{0, 1}].header().is_slice()));
    }
}

TEST(arrnd_test, can_access_relative_array_indices)
{
    using namespace oc;

    arrnd<int> arr({3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});

    for (auto i = 0; i < arr.header().numel(); ++i) {
        EXPECT_EQ(arr[i], arr(i));
    }

    auto slc = arr[{interval<>::at(2), interval<>::full(), interval<>::from(1, 2)}];
    std::vector<int> slc_values{18, 20, 22, 24};

    EXPECT_EQ(slc_values.size(), slc.header().numel());
    for (auto i = 0; i < slc.header().numel(); ++i) {
        EXPECT_EQ(slc_values[i], slc(i));
    }
}

TEST(arrnd_test, can_be_initialized_with_valid_size_and_data)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {0, 0, 0};
    EXPECT_NO_THROW((Integer_array{{1, 1}, data}));
    EXPECT_NO_THROW((Integer_array{{1, 3}, data}));
    EXPECT_NO_THROW((Integer_array{{3, 1}, data}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, {0, 0, 0}}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, {0, 0, 0}}));

    const double ddata[] = {0.0, 0.0, 0.0};
    EXPECT_NO_THROW((Integer_array{{1, 1}, ddata}));
    EXPECT_NO_THROW((Integer_array{{1, 3}, ddata}));
    EXPECT_NO_THROW((Integer_array{{3, 1}, ddata}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, {0.0, 0.0, 0.0}}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, {0.0, 0.0, 0.0}}));

    EXPECT_TRUE(oc::empty(Integer_array{{0, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1}, data}));

    EXPECT_TRUE(oc::empty(Integer_array{{1, 0, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 1, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 0, 1}, data}));

    EXPECT_TRUE(oc::empty(Integer_array{{0, 0, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1, 0}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 0, 1}, data}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1, 1}, data}));
}

TEST(arrnd_test, can_be_initialized_with_valid_size_and_value)
{
    using Integer_array = oc::arrnd<int>;

    const int value{0};
    EXPECT_NO_THROW((Integer_array{{1, 1}, value}));
    EXPECT_NO_THROW((Integer_array{{1, 3}, value}));
    EXPECT_NO_THROW((Integer_array{{3, 1}, value}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, value}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, value}));

    const double dvalue{0.0};
    EXPECT_NO_THROW((Integer_array{{1, 1}, dvalue}));
    EXPECT_NO_THROW((Integer_array{{1, 3}, dvalue}));
    EXPECT_NO_THROW((Integer_array{{3, 1}, dvalue}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, dvalue}));
    EXPECT_NO_THROW((Integer_array{{3, 1, 1}, dvalue}));

    EXPECT_TRUE(oc::empty(Integer_array{{0, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1}, value}));

    EXPECT_TRUE(oc::empty(Integer_array{{1, 0, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 1, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{1, 0, 1}, value}));

    EXPECT_TRUE(oc::empty(Integer_array{{0, 0, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1, 0}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 0, 1}, value}));
    EXPECT_TRUE(oc::empty(Integer_array{{0, 1, 1}, value}));
}

TEST(arrnd_test, parameterized_constructors_compilation)
{
    using namespace oc;

    // type deduction for data iterators

    // dims - iterators
    // data - iterators
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims.cbegin(), dims.cend(), data.cbegin(), data.cend());
    }

    // dims - container
    // data - iterators
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data.cbegin(), data.cend());
    }

    // dims - initializer_list
    // data - iterators
    {
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr1(std::initializer_list<int>{}, data.cbegin(), data.cend());
        arrnd arr2({3, 1, 2}, data.cbegin(), data.cend());
    }

    // dims - array
    // data - iterators
    {
        int dims[]{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data.cbegin(), data.cend());
    }

    // same 4 previous constructors with different specified arrnd type

    // dims - iterators
    // data - iterators
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims.cbegin(), dims.cend(), data.cbegin(), data.cend());
    }

    // dims - container
    // data - iterators
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data.cbegin(), data.cend());
    }

    // dims - initializer_list
    // data - iterators
    {
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr1(std::initializer_list<int>{}, data.cbegin(), data.cend());
        arrnd<double> arr2({3, 1, 2}, data.cbegin(), data.cend());
    }

    // dims - array
    // data - iterators
    {
        int dims[]{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data.cbegin(), data.cend());
    }

    // ------------------------------

    // type deduction for data initializer_list

    // dims - iterators
    // data - initailizer_list
    {
        std::vector<int> dims{3, 1, 2};
        arrnd arr(dims.cbegin(), dims.cend(), {1, 2, 3, 4, 5, 6});
    }

    // dims - container
    // data - initailizer_list
    {
        std::vector<int> dims{3, 1, 2};
        arrnd arr(dims, {1, 2, 3, 4, 5, 6});
    }

    // dims - initializer_list
    // data - initailizer_list
    {
        arrnd arr1(std::initializer_list<int>{}, {1, 2, 3, 4, 5, 6});
        arrnd arr2({3, 1, 2}, {1, 2, 3, 4, 5, 6});
    }

    // dims - array
    // data - initailizer_list
    {
        int dims[]{3, 1, 2};
        arrnd arr(dims, {1, 2, 3, 4, 5, 6});
    }

    // same 4 previous constructors with different specified arrnd type

    // dims - iterators
    // data - initailizer_list
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<double> arr(dims.cbegin(), dims.cend(), {1, 2, 3, 4, 5, 6});
    }

    // dims - container
    // data - initailizer_list
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<double> arr(dims, {1, 2, 3, 4, 5, 6});
    }

    // dims - initializer_list
    // data - initailizer_list
    {
        arrnd<double> arr1(std::initializer_list<int>{}, {1, 2, 3, 4, 5, 6});
        arrnd<double> arr2({3, 1, 2}, {1, 2, 3, 4, 5, 6});
    }

    // dims - array
    // data - initailizer_list
    {
        int dims[]{3, 1, 2};
        arrnd<double> arr(dims, {1, 2, 3, 4, 5, 6});
    }

    // ------------------------------

    // type deduction for data std library container

    // dims - iterators
    // data - container
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims.cbegin(), dims.cend(), data);
    }

    // dims - container
    // data - container
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data);
    }

    // dims - initializer_list
    // data - container
    {
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr1(std::initializer_list<int>{}, data);
        arrnd arr2({3, 1, 2}, data);
    }

    // dims - array
    // data - container
    {
        int dims[]{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data);
    }

    // same 4 previous constructors with different specified arrnd type

    // dims - iterators
    // data - container
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims.cbegin(), dims.cend(), data);
    }

    // dims - container
    // data - container
    {
        std::vector<int> dims{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data);
    }

    // dims - initializer_list
    // data - container
    {
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr1(std::initializer_list<int>{}, data);
        arrnd<double> arr2({3, 1, 2}, data);
    }

    // dims - array
    // data - container
    {
        int dims[]{3, 1, 2};
        std::vector<int> data{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data);
    }

    // ------------------------------

    // type deduction for data c-style array

    // dims - iterators
    // data - array
    {
        std::vector<int> dims{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd arr(dims.cbegin(), dims.cend(), data);
    }

    // dims - container
    // data - array
    {
        std::vector<int> dims{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data);
    }

    // dims - initializer_list
    // data - array
    {
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd arr1(std::initializer_list<int>{}, data);
        arrnd arr2({3, 1, 2}, data);
    }

    // dims - array
    // data - array
    {
        int dims[]{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd arr(dims, data);
    }

    // same 4 previous constructors with different specified arrnd type

    // dims - iterators
    // data - array
    {
        std::vector<int> dims{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims.cbegin(), dims.cend(), data);
    }

    // dims - container
    // data - array
    {
        std::vector<int> dims{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data);
    }

    // dims - initializer_list
    // data - array
    {
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd<double> arr1(std::initializer_list<int>{}, data);
        arrnd<double> arr2({3, 1, 2}, data);
    }

    // dims - array
    // data - array
    {
        int dims[]{3, 1, 2};
        int data[]{1, 2, 3, 4, 5, 6};
        arrnd<double> arr(dims, data);
    }

    // ------------------------------

    // no type deduction for dimensions parameter only

    // dims - iterators
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<int> arr(dims.cbegin(), dims.cend());
    }

    // dims - container
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<int> arr(dims);
    }

    // dims - initializer_list
    {
        arrnd<int> arr1(std::initializer_list<int>{});
        arrnd<int> arr2({3, 1, 2});
    }

    // dims - array
    {
        int dims[]{3, 1, 2};
        arrnd<int> arr(dims);
    }

    // ------------------------------

    // type deduction for data by function parameter

    auto proxy_func = [](auto a) {
        return a;
    };

    // dims - iterators
    // data - function
    {
        std::vector<int> dims{3, 1, 2};
        arrnd arr(dims.cbegin(), dims.cend(), proxy_func, 1);
    }

    // dims - container
    // data - function
    {
        std::vector<int> dims{3, 1, 2};
        arrnd arr(dims, proxy_func, 1);
    }

    // dims - initializer_list
    // data - function
    {
        arrnd arr1(std::initializer_list<int>{}, proxy_func, 1);
        arrnd arr2({3, 1, 2}, proxy_func, 1);
    }

    // dims - array
    // data - function
    {
        int dims[]{3, 1, 2};
        arrnd arr(dims, proxy_func, 1);
    }

    // same 4 previous constructors with different specified arrnd type

    // dims - iterators
    // data - function
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<double> arr(dims.cbegin(), dims.cend(), proxy_func, 1);
    }

    // dims - container
    // data - function
    {
        std::vector<int> dims{3, 1, 2};
        arrnd<double> arr(dims, proxy_func, 1);
    }

    // dims - initializer_list
    // data - function
    {
        arrnd<double> arr1(std::initializer_list<int>{}, proxy_func, 1);
        arrnd<double> arr2({3, 1, 2}, proxy_func, 1);
    }

    // dims - array
    // data - function
    {
        int dims[]{3, 1, 2};
        arrnd<double> arr(dims, proxy_func, 1);
    }

    // ------------------------------

    // type deduction according to iterable guides or by specificed arrnd type

    // dims - iterators
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        int value = 5;
        arrnd arr(dims.cbegin(), dims.cend(), value);
    }

    // dims - container
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        int value = 5;
        arrnd arr(dims, value);
    }

    // dims - initializer_list
    // data - value
    {
        int value = 5;
        arrnd arr1(std::initializer_list<int>{}, value);
        arrnd arr2({3, 1, 2}, value);
    }

    // dims - array
    // data - value
    {
        int dims[]{3, 1, 2};
        int value = 5;
        arrnd arr(dims, value);
    }

    // dims - iterators
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        int value = 5;
        arrnd<double> arr(dims.cbegin(), dims.cend(), value);
    }

    // dims - container
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        int value = 5;
        arrnd<double> arr(dims, value);
    }

    // dims - initializer_list
    // data - value
    {
        int value = 5;
        arrnd<double> arr1(std::initializer_list<int>{}, value);
        arrnd<double> arr2({3, 1, 2}, value);
    }

    // dims - array
    // data - value
    {
        int dims[]{3, 1, 2};
        int value = 5;
        arrnd<double> arr(dims, value);
    }

    // dims - iterators
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        std::string value{"str"};
        arrnd<std::string> arr(dims.cbegin(), dims.cend(), value);
    }

    // dims - container
    // data - value
    {
        std::vector<int> dims{3, 1, 2};
        std::string value{"str"};
        arrnd<std::string> arr(dims, value);
    }

    // dims - initializer_list
    // data - value
    {
        std::string value{"str"};
        arrnd<std::string> arr1(std::initializer_list<int>{}, value);
        arrnd<std::string> arr2({3, 1, 2}, value);
    }

    // dims - array
    // data - value
    {
        int dims[]{3, 1, 2};
        std::string value{"str"};
        arrnd<std::string> arr(dims, value);
    }

    // ------------------------------

    // scalar constructors

    //{
    //    int value = 5;
    //    arrnd arr(value);
    //}

    //{
    //    int value = 5;
    //    arrnd<double> arr(value);
    //}
}

TEST(arrnd_test, subscript_operators_compilation)
{
    using namespace oc;

    arrnd arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

    const auto& x1 = arr[0];
    arr[0] = 0;
    std::vector<int> subs1{0, 0, 0};
    const auto& x2 = arr[std::make_pair(subs1.cbegin(), subs1.cend())];
    arr[std::make_pair(subs1.cbegin(), subs1.cend())] = 0;
    const auto& x3 = arr[subs1];
    arr[subs1] = 0;
    const auto& x4 = arr[{0, 0, 0}];
    arr[{0, 0, 0}] = 0;
    int subs2[]{0, 0, 0};
    const auto& x5 = arr[subs2];
    arr[subs2] = 0;
    arrnd subs3({3}, {0, 0, 0});
    const auto& x6 = arr[subs3];
    arr[subs3] = 0;

    std::vector<interval<>> ranges1{interval<>::full(), interval<>::at(0), interval<>::at(0)};
    std::vector<interval<>> ranges2{interval<>::between(0, 2)};
    auto s1 = arr[std::make_pair(ranges1.cbegin(), ranges1.cend())][std::make_pair(ranges2.cbegin(), ranges2.cend())];
    auto s2 = arr[ranges1][ranges2];
    auto s3 = arr[{interval<>::full(), interval<>::at(0), interval<>::at(0)}][{interval<>::between(0, 2)}];
    interval<> ranges3[]{interval<>::full(), interval<>::at(0), interval<>::at(0)};
    interval<> ranges4[]{interval<>::between(0, 2)};
    auto s4 = arr[ranges3][ranges4];
    auto s5 = arr[interval<>::full()][interval<>::between(0, 2)];
    arrnd ranges5({3}, {interval<>::full(), interval<>::at(0), interval<>::at(0)});
    arrnd ranges6({1}, {interval<>::between(0, 2)});
    const auto& s6 = arr[ranges5][ranges6];
}

TEST(arrnd_test, can_be_explicitly_be_casted_to_value_type_if_scalar)
{
    {
        oc::arrnd<double> arr({1}, 0.5);
        EXPECT_TRUE(arr.header().is_scalar());
        EXPECT_EQ(0.5, static_cast<double>(arr));
    }

    {
        oc::arrnd<double> arr({1, 1, 1}, 0.5);
        EXPECT_TRUE(arr.header().is_scalar());
        EXPECT_EQ(0.5, static_cast<double>(arr));
    }

    {
        oc::arrnd<double> arr({3, 1, 2}, {0.1, 0.2, 0.3, 0.4, 0.5, 0.6});
        auto slc = arr[{oc::interval<>::at(2), oc::interval<>::at(0), oc::interval<>::at(1)}];
        EXPECT_TRUE(slc.header().is_scalar());
        EXPECT_EQ(0.6, static_cast<double>(slc));
    }
}

TEST(arrnd_test, can_be_initialized_by_valid_size_and_function)
{
    std::random_device rd;
    std::mt19937 gen;
    std::uniform_int_distribution dist(0, 9);

    auto urand_0to9 = [&](int factor) {
        return dist(gen) * factor;
    };

    oc::arrnd<int> arr({3, 1, 2}, urand_0to9, 10);

    EXPECT_TRUE(std::all_of(arr.cbegin(), arr.cend(), [](int a) {
        return a >= 0 && a <= 90;
    }));
}

TEST(arrnd_test, can_return_its_data)
{
    using Integer_array = oc::arrnd<int>;

    Integer_array earr{};

    EXPECT_FALSE(earr.storage());

    Integer_array arr{{3, 1, 2}, 0};

    const auto& storage = *arr.storage();
    for (std::int64_t i = 0; i < arr.header().numel(); ++i) {
        EXPECT_EQ(0, storage[i]);
    }
}

TEST(arrnd_test, can_have_complie_time_calculated_depth)
{
    static_assert(0 == oc::arrnd<int>::depth);
    EXPECT_EQ(0, oc::arrnd<int>({1, 1}).depth);
    static_assert(oc::arrnd<int>::is_flat);
    EXPECT_TRUE(oc::arrnd<int>({1, 1}).is_flat);

    static_assert(1 == oc::arrnd<oc::arrnd<int>>::depth);
    EXPECT_EQ(1, oc::arrnd<oc::arrnd<int>>({1, 1}).depth);
    static_assert(!oc::arrnd<oc::arrnd<int>>::is_flat);
    EXPECT_FALSE(oc::arrnd<oc::arrnd<int>>({1, 1}).is_flat);

    static_assert(2 == oc::arrnd<oc::arrnd<oc::arrnd<int>>>::depth);
    EXPECT_EQ(2, oc::arrnd<oc::arrnd<oc::arrnd<int>>>({1, 1}).depth);
    static_assert(!oc::arrnd<oc::arrnd<oc::arrnd<int>>>::is_flat);
    EXPECT_FALSE(oc::arrnd<oc::arrnd<oc::arrnd<int>>>({1, 1}).is_flat);
}

TEST(arrnd_test, can_check_if_array_base_type_is_of_specific_type_at_compile_time)
{
    static_assert(oc::arrnd_compliant_of_type<oc::arrnd<int>, int>);
    static_assert(!oc::arrnd_compliant_of_type<oc::arrnd<int>, double>);
}

TEST(arrnd_test, can_check_if_array_base_type_is_of_specific_template_type_at_compile_time)
{
    oc::arrnd<std::complex<int>>{}; // required due to MSVC compiler issue

    static_assert(oc::arrnd_compliant_of_template_type<oc::arrnd<std::complex<int>>, std::complex>);
    static_assert(!oc::arrnd_compliant_of_template_type<oc::arrnd<std::complex<int>>, std::vector>);
}

TEST(arrnd_test, can_check_if_array_base_type_have_specific_type_trait_at_compile_time)
{
    static_assert(oc::arrnd_compliant_with_trait<oc::arrnd<int>, std::is_integral>);
    static_assert(!oc::arrnd_compliant_with_trait<oc::arrnd<int>, std::is_floating_point>);
}

TEST(arrnd_test, check_nested_arrnd_shape_preset)
{
    using namespace oc;

    using nested_type = arrnd<arrnd<arrnd<arrnd<std::complex<int>>>>>;

    nested_type{}; // required due to MSVC compiler issue

    static_assert(std::is_same_v<nested_type, oc::arrnd_inner_t<nested_type, 0>>);
    static_assert(std::is_same_v<nested_type::value_type, oc::arrnd_inner_t<nested_type, 1>>);
    static_assert(std::is_same_v<nested_type::value_type::value_type, oc::arrnd_inner_t<nested_type, 2>>);
    static_assert(std::is_same_v<nested_type::value_type::value_type::value_type, oc::arrnd_inner_t<nested_type, 3>>);
    static_assert(std::is_same_v<nested_type::value_type::value_type::value_type, oc::arrnd_inner_t<nested_type>>);
    static_assert(std::is_same_v<nested_type::value_type::value_type::value_type::value_type, std::complex<int>>);
    static_assert(std::is_same_v<oc::arrnd_inner_t<nested_type>::value_type, std::complex<int>>);
}

TEST(arrnd_test, can_replace_arrnd_shape_preset_at_specific_depth)
{
    using namespace oc;

    using nested_type = arrnd<arrnd<arrnd<arrnd<int>>>>;

    static_assert(std::is_same_v<nested_type::inner_replaced_type<double, 0>, arrnd<double>>);
    static_assert(std::is_same_v<nested_type::inner_replaced_type<double, 1>, arrnd<arrnd<double>>>);
    static_assert(std::is_same_v<nested_type::inner_replaced_type<double, 2>, arrnd<arrnd<arrnd<double>>>>);
    static_assert(std::is_same_v<nested_type::inner_replaced_type<double, 3>, arrnd<arrnd<arrnd<arrnd<double>>>>>);
}

TEST(arrnd_test, have_read_write_access_to_its_cells)
{
    using Integer_array = oc::arrnd<int>;

    const int data[] = {1, 2, 3, 4, 5, 6};

    Integer_array arr1d{{6}, data};
    const std::int64_t* dims1d{arr1d.header().dims().data()};
    for (std::int64_t i = 0; i < dims1d[0]; ++i) {
        EXPECT_EQ((arr1d[{i}]), data[i]);
    }
    //EXPECT_EQ(1, (arr1d[{ 6 }])); // assertion failure
    //EXPECT_EQ(6, (arr1d[{ -1 }])); // assertion failure
    for (std::int64_t i = 0; i < dims1d[0]; ++i) {
        arr1d[{i}] = 0;
        EXPECT_EQ((arr1d[{i}]), 0);
    }

    Integer_array arr2d{{3, 2}, data};
    const std::int64_t* dims2d{arr2d.header().dims().data()};
    for (std::int64_t i = 0; i < dims2d[0]; ++i) {
        for (std::int64_t j = 0; j < dims2d[1]; ++j) {
            EXPECT_EQ((arr2d[{i, j}]), data[i * dims2d[1] + j]);
        }
    }
    //EXPECT_EQ(1, (arr2d[{ 3, 2 }])); // assertion failure
    //EXPECT_EQ(6, (arr2d[{ -1, -1 }])); // assertion failure
    for (std::int64_t i = 0; i < dims2d[0]; ++i) {
        for (std::int64_t j = 0; j < dims2d[1]; ++j) {
            arr2d[{i, j}] = 0;
            EXPECT_EQ((arr2d[{i, j}]), 0);
        }
    }

    Integer_array arr3d{{3, 1, 2}, data};
    const std::int64_t* dims3d{arr3d.header().dims().data()};
    for (std::int64_t k = 0; k < dims3d[0]; ++k) {
        for (std::int64_t i = 0; i < dims3d[1]; ++i) {
            for (std::int64_t j = 0; j < dims3d[2]; ++j) {
                EXPECT_EQ((arr3d[{k, i, j}]), data[k * (dims3d[1] * dims3d[2]) + i * dims3d[2] + j]);
            }
        }
    }
    //EXPECT_EQ(1, (arr3d[{ 3, 1, 2 }])); // assertion failure
    //EXPECT_EQ(6, (arr3d[{ -1, -1, -1 }])); // assertion failure
    for (std::int64_t k = 0; k < dims3d[0]; ++k) {
        for (std::int64_t i = 0; i < dims3d[1]; ++i) {
            for (std::int64_t j = 0; j < dims3d[2]; ++j) {
                arr3d[{k, i, j}] = 0;
                EXPECT_EQ((arr3d[{k, i, j}]), 0);
            }
        }
    }

    // partial subscripts
    {
        Integer_array parr{{3, 1, 2}, data};

        EXPECT_EQ((parr[{0, 0, 0}]), (parr[{0}]));
        EXPECT_EQ((parr[{0, 0, 1}]), (parr[{1}]));
        EXPECT_EQ((parr[{0, 0, 0}]), (parr[{0, 0}]));
        EXPECT_EQ((parr[{0, 0, 1}]), (parr[{0, 1}]));

        // extra subscripts are being ignored
        //EXPECT_EQ((parr[{ 0, 0, 0 }]), (parr[{ 0, 0, 0, 10 }])); // assertion failure
        //EXPECT_EQ((parr[{ 2, 0, 1 }]), (parr[{ 2, 0, 1, 10 }])); // assertion failure
    }

    // different data type
    {
        const int rdata[6]{0};

        Integer_array arr1({6}, 0.5);
        for (std::int64_t i = 0; i < 6; ++i) {
            EXPECT_EQ(rdata[i], (arr1[{i}]));
        }

        const double data2[]{0.1, 0.2, 0.3, 0.4, 0.5, 0.6};
        Integer_array arr2({6}, data2);
        for (std::int64_t i = 0; i < 6; ++i) {
            EXPECT_EQ(rdata[i], (arr2[{i}]));
        }
    }
}

TEST(arrnd_test, have_read_write_access_to_slice)
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
    const std::int64_t rdims[]{2, 2, 1};
    Integer_array rarr{rdims, rdata};

    Integer_array sarr{arr[{{0, 2}, {1, 2}, {0, 2}, {1, 3, 2}}]};

    for (std::int64_t k = 0; k < rdims[0]; ++k) {
        for (std::int64_t i = 0; i < rdims[1]; ++i) {
            for (std::int64_t j = 0; j < rdims[2]; ++j) {
                EXPECT_EQ((rarr[{k, i, j}]), (sarr[{k, static_cast<std::int64_t>(0), i, j}]));
            }
        }
    }
}

TEST(arrnd_test, element_wise_transformation)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 4, 5, 6};
    oc::arrnd iarr{dims, idata};

    const double odata[]{0.5, 1.0, 1.5, 2.0, 2.5, 3.0};
    oc::arrnd oarr{dims, odata};

    EXPECT_TRUE(oc::all_equal(oarr, oc::transform(iarr, [](int n) {
        return n * 0.5;
    })));

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> narr(
            {1, 2}, {oc::arrnd<int>({1, 5}, {1, 2, 3, 4, 5}), oc::arrnd<int>({1, 5}, {6, 7, 8, 9, 10})});

        auto rnarr = oc::transform<0>(narr, [](const auto& arr) {
            return 0.5 * std::reduce(arr.cbegin(), arr.cend(), 0, std::plus<>{});
        });

        static_assert(std::is_same_v<decltype(rnarr), oc::arrnd<double>>);
        EXPECT_TRUE(oc::all_equal(rnarr, oc::arrnd<double>({1, 2}, {7.5, 20.})));
    }

    // scalar
    {
        using namespace oc;

        arrnd<int> iarr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
        arrnd<int> scalar({1}, {1});

        auto tarr1 = transform(iarr, scalar, std::minus<>{});

        EXPECT_TRUE(all_equal(tarr1,
            arrnd<int>(
                {3, 2, 4}, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23})));

        auto tarr2 = transform(scalar, iarr, std::minus<>{});

        EXPECT_TRUE(all_equal(tarr2,
            arrnd<int>({3, 2, 4},
                {0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22,
                    -23})));
    }

    // reduced dims
    {
        using namespace oc;

        arrnd<int> iarr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
        arrnd<int> rarr({1, 2, 1}, {1, 2});

        auto tarr1 = transform(iarr, rarr, std::minus<>{});

        EXPECT_TRUE(all_equal(tarr1,
            arrnd<int>(
                {3, 2, 4}, {0, 1, 2, 3, 3, 4, 5, 6, 8, 9, 10, 11, 11, 12, 13, 14, 16, 17, 18, 19, 19, 20, 21, 22})));

        auto tarr2 = transform(rarr, iarr, std::minus<>{});

        EXPECT_TRUE(all_equal(tarr2,
            arrnd<int>({3, 2, 4},
                {0, -1, -2, -3, -3, -4, -5, -6, -8, -9, -10, -11, -11, -12, -13, -14, -16, -17, -18, -19, -19, -20, -21,
                    -22})));
    }
}

TEST(arrnd_test, apply_transformation_on_array_elements)
{
    using namespace oc;

    arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

    auto& tarr = apply(arr, [](int val) {
        return 2 * val;
    });
    tarr = apply(tarr, arrnd<int>({3, 1, 2}, {1, 0, 1, 0, 1, 0}), [](int val1, int val2) {
        return val1 * val2;
    });
    tarr = apply(
        tarr,
        [](int val1, int val2) {
            return val1 == 0 ? 0 : val1 + val2;
        },
        2);

    arrnd<int> res({3, 1, 2}, {4, 0, 8, 0, 12, 0});

    EXPECT_TRUE(all_equal(res, arr));

    // nested array
    {
        arrnd<arrnd<int>> narr({1, 4},
            {arrnd<int>({1, 2}, {1, 2}), arrnd<int>({1, 2}, {1, 2}), arrnd<int>({1, 2}, {1, 2}),
                arrnd<int>({1, 2}, {1, 2})});

        auto& tnarr = apply(narr, [](int a) {
            return a * 2;
        });
        tnarr = apply<0>(tnarr, [](const arrnd<int> a) {
            return arrnd<int>({1}, a[{0, 0}]);
        });

        arrnd<arrnd<int>> nres(
            {1, 4}, {arrnd<int>({1}, {2}), arrnd<int>({1}, {2}), arrnd<int>({1}, {2}), arrnd<int>({1}, {2})});

        EXPECT_TRUE(all_equal(nres, narr));
    }

    // scalar
    {
        arrnd<int> iarr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
        arrnd<int> scalar({1}, {1});

        apply(iarr, scalar, [](int a, int b) {
            return a + b;
        });

        EXPECT_TRUE(all_equal(iarr,
            arrnd<int>(
                {3, 2, 4}, {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25})));
    }

    // reduced dims
    {
        arrnd<int> iarr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
        arrnd<int> rarr({1, 2, 1}, {1, 2});

        apply(iarr, rarr, [](int a, int b) {
            return a + b;
        });

        EXPECT_TRUE(all_equal(iarr,
            arrnd<int>(
                {3, 2, 4}, {2, 3, 4, 5, 7, 8, 9, 10, 10, 11, 12, 13, 15, 16, 17, 18, 18, 19, 20, 21, 23, 24, 25, 26})));
    }
}

TEST(arrnd_test, element_wise_transform_operation)
{
    //EXPECT_TRUE(oc::empty(oc::transform(oc::arrnd<int>({3, 1, 2}), oc::arrnd<double>({6}), [](int, double) {
    //    return 0.0;
    //}))); // assertion failure

    std::int64_t dims[]{3, 1, 2};

    const int idata1[]{1, 2, 3, 4, 5, 6};
    oc::arrnd iarr1{dims, idata1};

    const double idata2[]{0.5, 1.0, 1.5, 2.0, 2.5, 3.0};
    oc::arrnd iarr2{dims, idata2};

    oc::arrnd oarr1{dims, 0.5};

    EXPECT_TRUE(oc::all_equal(oarr1, oc::transform(iarr1, iarr2, [](int a, double b) {
        return b / a;
    })));

    const int odata2[] = {0, 1, 2, 3, 4, 5};
    oc::arrnd oarr2{dims, odata2};

    EXPECT_TRUE(oc::all_equal(oarr2,
        oc::transform(
            iarr1,
            [](int a, int b) {
                return a - b;
            },
            1)));

    const int odata3[] = {0, -1, -2, -3, -4, -5};
    oc::arrnd oarr3{dims, odata3};

    EXPECT_TRUE(oc::all_equal(oarr3,
        oc::transform(
            iarr1,
            [](int a, int b) {
                return -a + b;
            },
            1)));

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> narr(
            {1, 2}, {oc::arrnd<int>({1, 5}, {1, 2, 3, 4, 5}), oc::arrnd<int>({1, 5}, {6, 7, 8, 9, 10})});

        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({1, 5}, {1, 2, 3, 4, 5}), oc::arrnd<int>({1, 5}, {6, 7, 8, 9, 10})});

        auto rnarr = oc::transform<0>(narr, inarr, [](const auto& lhs, const auto& rhs) {
            return 0.5 * std::reduce(lhs.cbegin(), lhs.cend(), 0, std::plus<>{})
                + std::reduce(rhs.cbegin(), rhs.cend(), 0, std::plus<>{});
        });

        static_assert(std::is_same_v<decltype(rnarr), oc::arrnd<double>>);
        EXPECT_TRUE(oc::all_equal(rnarr, oc::arrnd<double>({1, 2}, {22.5, 60.})));
    }
}

TEST(arrnd_test, reduce_elements)
{
    std::int64_t dims[]{3, 1, 2};

    const int idata[]{1, 2, 3, 4, 5, 6};
    oc::arrnd<int> iarr{dims, idata};

    EXPECT_EQ((1.0 / 2 / 3 / 4 / 5 / 6), oc::reduce(iarr, [](double a, int b) {
        return a / b;
    }));

    std::int64_t dims2[]{3, 1};
    const double rdata2[]{3.0, 7.0, 11.0};
    oc::arrnd<double> rarr2{dims2, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::reduce(iarr, 2, [](int value, double previous) {
        return previous + value;
    })));

    std::int64_t dims1[]{3, 2};
    const double rdata1[]{1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
    oc::arrnd<double> rarr1{dims1, rdata1};
    EXPECT_TRUE(oc::all_equal(rarr1, oc::reduce(iarr, 1, [](int value, double previous) {
        return previous + value;
    })));

    std::int64_t dims0[]{1, 2};
    const double rdata0[]{9.0, 12.0};
    oc::arrnd<double> rarr0{dims0, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::reduce(iarr, 0, [](int value, double previous) {
        return previous + value;
    })));

    oc::arrnd iarr1d{{6}, idata};
    const double data1d[]{21.0};
    oc::arrnd rarr1d{{1}, data1d};
    EXPECT_TRUE(oc::all_equal(rarr1d, oc::reduce(iarr1d, 0, [](int value, double previous) {
        return previous + value;
    })));

    //EXPECT_TRUE(oc::all_equal(rarr0, oc::reduce(iarr, [](int value, double previous) {return previous + value; }, 3))); // assertion failure

    // reduction with initial value(s)
    {
        oc::arrnd arr{{2, 3}, {1, 2, 5, 6, 10, 11}};

        std::string chain = oc::fold(arr, std::string{}, [](const std::string& s, int n) {
            return s + "-" + std::to_string(n);
        });
        EXPECT_EQ("-1-2-5-6-10-11", chain);

        oc::arrnd<std::string> byaxis = oc::fold(arr[{{0, 2}, {2, 3}}], 1,
            oc::arrnd{{2}, {std::to_string(arr[{0, 0}]), std::to_string(arr[{1, 0}])}},
            [](const std::string& s, int n) {
                return s + "-" + std::to_string(n);
            });
        EXPECT_TRUE(oc::all_equal(oc::arrnd{{2}, {std::string{"1-5"}, std::string{"6-11"}}}, byaxis));
    }

    // complex array reduction
    {
        auto sum = [](int value, double previous) {
            return previous + value;
        };

        EXPECT_TRUE(oc::all_equal(rarr1d, oc::reduce(oc::reduce(oc::reduce(iarr, 2, sum), 1, sum), 0, sum)));
    }

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr1(
            {1, 2}, {oc::arrnd<int>({4}, {1, 2, 3, 4}), oc::arrnd<int>({4}, {5, 6, 7, 8})});

        auto r1 = oc::reduce(inarr1, std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r1, oc::arrnd<int>({1, 2}, {10, 26})));

        auto r2 = oc::reduce<0>(inarr1, std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<int>({4}, {6, 8, 10, 12})));

        oc::arrnd<oc::arrnd<int>> inarr2(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({3, 1, 2}, {7, 8, 9, 10, 11, 12})});

        auto r3 = oc::reduce(inarr2, 0, std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r3,
            oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({1, 2}, {9, 12}), oc::arrnd<int>({1, 2}, {27, 30})})));

        auto r4 = oc::reduce<0>(inarr2, 1, std::plus<>{});
        EXPECT_TRUE(
            oc::all_equal(r4, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({3, 1, 2}, {8, 10, 12, 14, 16, 18})})));

        auto r5 = oc::fold(inarr1, 2, std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r5, oc::arrnd<int>({1, 2}, {12, 28})));

        auto r6 = oc::fold<0>(inarr1, oc::arrnd<int>({4}, 2), std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r6, oc::arrnd<int>({4}, {8, 10, 12, 14})));

        auto r7 = oc::fold(inarr2, 0, oc::arrnd<int>({2}, 2), std::plus<>{});
        EXPECT_TRUE(oc::all_equal(r7,
            oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({1, 2}, {11, 14}), oc::arrnd<int>({1, 2}, {29, 32})})));

        auto r8 = oc::fold<0>(inarr2, 1, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({3, 1, 2}, 2)}), std::plus<>{});
        EXPECT_TRUE(
            oc::all_equal(r8, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({3, 1, 2}, {10, 12, 14, 16, 18, 20})})));
    }
}

TEST(arrnd_test, all)
{
    const bool data[] = {1, 0, 1, 1};
    oc::arrnd<int> arr{{2, 2}, data};

    EXPECT_EQ(false, oc::all(arr));

    const bool rdata[] = {true, false};
    oc::arrnd<bool> rarr{{2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, oc::all(arr, 0)));
}

TEST(arrnd_test, any)
{
    const bool data[] = {1, 0, 0, 0};
    oc::arrnd<int> arr{{2, 2}, data};

    EXPECT_EQ(true, oc::any(arr));

    const bool rdata[] = {true, false};
    oc::arrnd<bool> rarr{{2}, rdata};

    EXPECT_TRUE(oc::all_equal(rarr, oc::any(arr, 0)));
}

TEST(arrnd_test, sum)
{
    using namespace oc;

    arrnd<int> arr({2, 2}, {1, 2, 3, 4});

    EXPECT_EQ(10, sum(arr));

    EXPECT_TRUE(all_equal(arrnd<int>({2}, {3, 7}), sum(arr, 1)));
}

TEST(arrnd_test, prod)
{
    using namespace oc;

    arrnd<int> arr({2, 2}, {1, 2, 3, 4});

    EXPECT_EQ(24, prod(arr));

    EXPECT_TRUE(all_equal(arrnd<int>({2}, {2, 12}), prod(arr, 1)));
}

TEST(arrnd_test, min_max)
{
    using namespace oc;

    arrnd<int> arr({2, 2}, {1, 4, 3, 2});

    EXPECT_EQ(1, min(arr));
    EXPECT_TRUE(all_equal(arrnd<int>({2}, {1, 2}), min(arr, 0)));

    EXPECT_EQ(4, max(arr));
    EXPECT_TRUE(all_equal(arrnd<int>({2}, {3, 4}), max(arr, 0)));
}
