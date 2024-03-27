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

#include <oc/arrnd.h>

TEST(simple_dynamic_vector_test, span_and_iterators_usage)
{
    using simple_vector = oc::simple_dynamic_vector<std::string>;

    auto count_elements = [](std::span<const std::string> s) {
        return s.size();
    };

    simple_vector sv(2, std::array<std::string, 2>{"first string", "second string"}.data());
    EXPECT_EQ(2, count_elements(sv));
    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) {
        return s.find("string") != std::string::npos;
    }));
}

TEST(simple_dynamic_vector_test, basic_functionality)
{
    using simple_vector = oc::simple_dynamic_vector<std::string>;

    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};

    simple_vector sv(16, arr.data());
    EXPECT_EQ(16, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    EXPECT_EQ("p", sv.back());

    int ctr = 0;
    for (const auto& e : sv) {
        EXPECT_EQ(arr[ctr++], e);
    }

    for (int i = 0; i < sv.size(); ++i) {
        EXPECT_EQ(arr[i], sv[i]);
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        oc::simple_dynamic_vector<int> vec1(5, arr1.data());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(simple_static_vector_test, span_usage)
{
    using simple_vector = oc::simple_static_vector<std::string, 2>;

    auto count_elements = [](std::span<const std::string> s) {
        return s.size();
    };

    simple_vector sv(2, std::array<std::string, 2>{"first string", "second string"}.data());
    EXPECT_EQ(2, count_elements(sv));
    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) {
        return s.find("string") != std::string::npos;
    }));
}

TEST(simple_static_vector_test, basic_functionality)
{
    using simple_vector = oc::simple_static_vector<std::string, 16>;

    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};

    simple_vector sv(16, arr.data());
    EXPECT_EQ(16, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    EXPECT_EQ("p", sv.back());

    int ctr = 0;
    for (const auto& e : sv) {
        EXPECT_EQ(arr[ctr++], e);
    }

    for (int i = 0; i < sv.size(); ++i) {
        EXPECT_EQ(arr[i], sv[i]);
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        oc::simple_static_vector<int, 5> vec1(5, arr1.data());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(close_test, two_numbers_can_be_compared_with_specified_percision)
{
    using namespace oc;

    EXPECT_TRUE(close(1, 1));
    EXPECT_TRUE(close(1, 2, 2));
    EXPECT_FALSE(close(1, 2));
    EXPECT_FALSE(close(-1, 1, 1));

    EXPECT_TRUE(close(1e10, 1.00001e10));
    EXPECT_TRUE(close(1e-8, 1e-9));
    EXPECT_TRUE(close(1.0, 1.0));
    EXPECT_TRUE(close(1e-8, 0.0));
    EXPECT_TRUE(close(1e-10, 1e-20));
    EXPECT_TRUE(close(1e-10, 0.0));
    EXPECT_TRUE(close(1e-10, 0.999999e-10, 0.0));
    EXPECT_FALSE(close(1e-7, 1e-8));
    EXPECT_FALSE(close(1e10, 1.0001e10));
    EXPECT_FALSE(close(1e-7, 0.0));
    EXPECT_FALSE(close(1e-100, 0.0, 0.0));
    EXPECT_FALSE(close(1e-7, 0.0, 0.0));
    EXPECT_FALSE(close(1e-10, 1e-20, 0.0));
}

TEST(modulo_test, modulo_opration_can_be_perform_on_positive_zero_or_negative_number)
{
    using namespace oc;

    EXPECT_EQ(0, modulo(0, 5));
    EXPECT_EQ(1, modulo(1, 5));
    EXPECT_EQ(1, modulo(26, 5));
    EXPECT_EQ(4, modulo(-1, 5));
    EXPECT_EQ(4, modulo(-26, 5));
}

TEST(interval_test, initialization)
{
    oc::interval i1{};
    EXPECT_EQ(0, i1.start);
    EXPECT_EQ(0, i1.stop);
    EXPECT_EQ(1, i1.step);

    oc::interval i2{1};
    EXPECT_EQ(1, i2.start);
    EXPECT_EQ(1, i2.stop);
    EXPECT_EQ(1, i2.step);

    oc::interval i3{1, 2};
    EXPECT_EQ(1, i3.start);
    EXPECT_EQ(2, i3.stop);
    EXPECT_EQ(1, i3.step);

    oc::interval i4{1, 2, 3};
    EXPECT_EQ(1, i4.start);
    EXPECT_EQ(2, i4.stop);
    EXPECT_EQ(3, i4.step);
}

TEST(interval_test, reverse)
{
    oc::interval i{oc::reverse(oc::interval{1, 2, 3})};
    EXPECT_EQ(2, i.start);
    EXPECT_EQ(1, i.stop);
    EXPECT_EQ(-3, i.step);
}

TEST(interval_test, modulo)
{
    oc::interval i{oc::modulo(oc::interval{-26, 26, -1}, 5)};
    EXPECT_EQ(4, i.start);
    EXPECT_EQ(1, i.stop);
    EXPECT_EQ(-1, i.step);
}

TEST(interval_test, forward)
{
    oc::interval i1{oc::forward(oc::interval{1, 2, 3})};
    EXPECT_EQ(1, i1.start);
    EXPECT_EQ(2, i1.stop);
    EXPECT_EQ(3, i1.step);

    oc::interval i2{oc::forward(oc::interval{2, 1, -3})};
    EXPECT_EQ(1, i2.start);
    EXPECT_EQ(2, i2.stop);
    EXPECT_EQ(3, i2.step);
}

TEST(interval_test, presets)
{
    oc::interval<std::int64_t> i1 = oc::interval<std::int64_t>::at(5);
    EXPECT_EQ(5, i1.start);
    EXPECT_EQ(5, i1.stop);
    EXPECT_EQ(1, i1.step);

    oc::interval<std::int64_t> i2 = oc::interval<std::int64_t>::full(5);
    EXPECT_EQ(0, i2.start);
    EXPECT_EQ(4, i2.stop);
    EXPECT_EQ(1, i2.step);

    oc::interval<std::int64_t> i3 = oc::interval<std::int64_t>::from(5, 5);
    EXPECT_EQ(5, i3.start);
    EXPECT_EQ(10, i3.stop);
    EXPECT_EQ(1, i3.step);

    oc::interval<std::int64_t> i4 = oc::interval<std::int64_t>::to(5);
    EXPECT_EQ(0, i4.start);
    EXPECT_EQ(5, i4.stop);
    EXPECT_EQ(1, i4.step);

    oc::interval<std::int64_t> i5 = oc::interval<std::int64_t>::between(1, 5);
    EXPECT_EQ(1, i5.start);
    EXPECT_EQ(5, i5.stop);
    EXPECT_EQ(1, i5.step);

    oc::interval<std::int64_t> i6 = oc::interval<std::int64_t>::between(1, 5, 5);
    EXPECT_EQ(1, i6.start);
    EXPECT_EQ(5, i6.stop);
    EXPECT_EQ(5, i6.step);
}

TEST(general_iterable_types_check, iterator_value_type)
{
    static_assert(std::is_same_v<int, oc::iterator_value_type<std::vector<int>::iterator>>);
}

TEST(general_iterable_types_check, iterable_of_type)
{
    static_assert(oc::iterable_of_type<std::vector<int>, int>);
}

TEST(general_iterable_types_check, random_access_type)
{
    static_assert(oc::random_access_type<std::vector<int>>);
}

TEST(arrnd_header_test, can_return_array_info)
{
    oc::arrnd_header ehdr;

    EXPECT_EQ(0, ehdr.dims().size());
    EXPECT_EQ(0, ehdr.numel());
    EXPECT_TRUE(ehdr.dims().empty());
    EXPECT_TRUE(ehdr.strides().empty());
    EXPECT_EQ(0, ehdr.offset());
    EXPECT_FALSE(ehdr.is_sliced());
    EXPECT_FALSE(ehdr.is_reordered());

    oc::arrnd_header hdr({3, 1, 2});

    EXPECT_EQ(3, hdr.dims().size());
    EXPECT_EQ(6, hdr.numel());
    EXPECT_EQ(3, hdr.dims().data()[0]);
    EXPECT_EQ(1, hdr.dims().data()[1]);
    EXPECT_EQ(2, hdr.dims().data()[2]);
    EXPECT_EQ(2, hdr.strides().data()[0]);
    EXPECT_EQ(2, hdr.strides().data()[1]);
    EXPECT_EQ(1, hdr.strides().data()[2]);
    EXPECT_EQ(0, hdr.offset());
    EXPECT_FALSE(hdr.is_sliced());
    EXPECT_FALSE(hdr.is_reordered());

    auto rhdr = hdr.reorder(1);
    EXPECT_TRUE(rhdr.is_reordered());
}

TEST(arrnd_general_indexer, simple_forward_backward_iterations)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims);

    const std::int64_t expected_inds_list[6]{0, 1, 2, 3, 4, 5};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_general_indexer gen(hdr);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        ++gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (--gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_general_indexer, simple_backward_forward_iterations)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{5, 4, 3, 2, 1, 0};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_general_indexer gen(hdr, oc::arrnd_indexer_position::rbegin);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        --gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (++gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_general_indexer, simple_forward_backward_iterations_with_steps_bigger_than_one)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{0, 2, 4};
    const std::int64_t expected_generated_subs{3};

    std::int64_t generated_subs_counter{0};
    arrnd_general_indexer gen(hdr);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        gen += 2;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while ((gen = gen - 2)) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_general_indexer, forward_backward_iterations_by_axis_order)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    const std::int64_t order[]{2, 0, 1};
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{0, 2, 4, 1, 3, 5};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_general_indexer gen(hdr, order, order + 3);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        ++gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (--gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_general_indexer, forward_backward_iterations_by_specific_major_axis)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[][6]{{0, 1, 2, 3, 4, 5},

        {0, 1, 2, 3, 4, 5},

        {0, 2, 4, 1, 3, 5}};
    const std::int64_t expected_generated_subs{6};

    for (std::int64_t axis = 0; axis <= 2; ++axis) {
        std::int64_t generated_subs_counter{0};
        arrnd_general_indexer gen(hdr, axis);

        while (gen) {
            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
            ++generated_subs_counter;
            ++gen;
        }
        EXPECT_EQ(expected_generated_subs, generated_subs_counter);

        while (--gen) {
            --generated_subs_counter;
            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
        }
        EXPECT_EQ(0, generated_subs_counter);
    }
}

TEST(arrnd_general_indexer, random_access)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{0, 5, 4, 1, 2, 3};

    arrnd_general_indexer gen(hdr, oc::arrnd_indexer_position::rbegin);

    EXPECT_EQ(expected_inds_list[0], gen[0]);
    EXPECT_EQ(expected_inds_list[1], gen[5]);
    EXPECT_EQ(expected_inds_list[2], gen[4]);
    EXPECT_EQ(expected_inds_list[3], gen[1]);
    EXPECT_EQ(expected_inds_list[4], gen[2]);
    EXPECT_EQ(expected_inds_list[5], gen[3]);
}

TEST(arrnd_fast_indexer, simple_forward_backward_iterations)
{
    using namespace oc;

    const std::int64_t dims[]{2, 3, 4, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 4);

    const std::int64_t expected_inds_list[48]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47};
    const std::int64_t expected_generated_subs{48};

    std::int64_t generated_subs_counter{0};
    arrnd_fast_indexer gen(hdr);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        ++gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (--gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fast_indexer, simple_backward_forward_iterations)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{5, 4, 3, 2, 1, 0};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_fast_indexer gen(hdr, oc::arrnd_indexer_position::rbegin);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        --gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (++gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fast_indexer, simple_forward_backward_iterations_with_steps_bigger_than_one)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{0, 2, 4};
    const std::int64_t expected_generated_subs{3};

    std::int64_t generated_subs_counter{0};
    arrnd_fast_indexer gen(hdr);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        gen += 2;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while ((gen = gen - 2)) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fast_indexer, forward_backward_iterations_by_specific_major_axis)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[][6]{{0, 1, 2, 3, 4, 5},

        {0, 1, 2, 3, 4, 5},

        {0, 2, 4, 1, 3, 5}};
    const std::int64_t expected_generated_subs{6};

    for (std::int64_t axis = 0; axis <= 2; ++axis) {
        std::int64_t generated_subs_counter{0};
        arrnd_fast_indexer gen(hdr, axis);

        while (gen) {
            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
            ++generated_subs_counter;
            ++gen;
        }
        EXPECT_EQ(expected_generated_subs, generated_subs_counter);

        while (--gen) {
            --generated_subs_counter;
            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
        }
        EXPECT_EQ(0, generated_subs_counter);
    }
}

TEST(arrnd_fast_indexer, random_access)
{
    using namespace oc;

    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const std::int64_t expected_inds_list[6]{0, 5, 4, 1, 2, 3};

    arrnd_fast_indexer gen(hdr, oc::arrnd_indexer_position::rbegin);

    EXPECT_EQ(expected_inds_list[0], gen[0]);
    EXPECT_EQ(expected_inds_list[1], gen[5]);
    EXPECT_EQ(expected_inds_list[2], gen[4]);
    EXPECT_EQ(expected_inds_list[3], gen[1]);
    EXPECT_EQ(expected_inds_list[4], gen[2]);
    EXPECT_EQ(expected_inds_list[5], gen[3]);
}

TEST(arrnd_fixed_axis_ranger, simple_forward_backward_iterations)
{
    using namespace oc;
    using namespace oc::details;

    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const interval<> expected_inds_list[3]{interval<>{0}, interval<>{1}, interval<>{2}};
    const std::int64_t expected_generated_subs{3};

    std::int64_t generated_subs_counter{0};
    arrnd_fixed_axis_ranger gen(hdr, 2);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
        ++generated_subs_counter;
        ++gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (--gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fixed_axis_ranger, simple_backward_forward_iterations)
{
    using namespace oc;
    using namespace oc::details;

    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const interval<> expected_inds_list[3]{interval<>{2}, interval<>{1}, interval<>{0}};
    const std::int64_t expected_generated_subs{3};

    std::int64_t generated_subs_counter{0};
    arrnd_fixed_axis_ranger gen(hdr, 2, true);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
        ++generated_subs_counter;
        --gen;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (++gen) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fixed_axis_ranger, simple_forward_backward_iterations_with_steps_bigger_than_one)
{
    using namespace oc;
    using namespace oc::details;

    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const interval<> expected_inds_list[2]{interval<>{0}, interval<>{2}};
    const std::int64_t expected_generated_subs{2};

    std::int64_t generated_subs_counter{0};
    arrnd_fixed_axis_ranger gen(hdr, 2);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
        ++generated_subs_counter;
        gen += 2;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while ((gen = gen - 2)) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(arrnd_fixed_axis_ranger, random_access)
{
    using namespace oc;
    using namespace oc::details;

    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
    arrnd_header hdr(dims, dims + 3);

    const interval<> expected_inds_list[3]{interval<>{0}, interval<>{2}, interval<>{1}};

    arrnd_fixed_axis_ranger gen(hdr, 2);

    EXPECT_EQ(expected_inds_list[0], gen[0][2]);
    EXPECT_EQ(expected_inds_list[1], gen[2][2]);
    EXPECT_EQ(expected_inds_list[2], gen[1][2]);
}

TEST(arrnd_test, iterators_and_inserters)
{
    using namespace oc;

    const arrnd<int> arr1{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
    arrnd<int> arr2{{3, 1, 2}, {0, 1, 2, 3, 4, 5}};

    auto product = std::inner_product(arr1.cbegin(), arr1.cend(), arr2.begin(), 1);

    EXPECT_EQ(71, product);

    std::transform(arr1.crbegin(), arr1.crend(), arr2.begin(), [](auto c) {
        return c + 1;
    });

    EXPECT_TRUE(all_equal(arrnd<int>({3, 1, 2}, {7, 6, 5, 4, 3, 2}), arr2));

    std::transform(arr1.cbegin() + 1, arr1.cend() - 2, arr2.begin() + 2, [](auto a) {
        return a * 10;
    });

    EXPECT_TRUE(all_equal(arrnd<int>({3, 1, 2}, {7, 6, 20, 30, 40, 2}), arr2));

    std::transform(arr1.crbegin() + 1, arr1.crend() - 2, arr2.rbegin() + 1, [](auto a) {
        return a * 1000;
    });

    EXPECT_TRUE(all_equal(arrnd<int>({3, 1, 2}, {7, 6, 3000, 4000, 5000, 2}), arr2));

    std::transform(arr1.cbegin(), ++(arr1.cbegin()), arr2[{{1, 1, 2}, {0, 0}, {1, 1}}].rbegin(), [](auto a) {
        return a * 100;
    });

    EXPECT_TRUE(all_equal(arrnd<int>({3, 1, 2}, {7, 6, 3000, 100, 5000, 2}), arr2));

    arrnd<int> arr{{3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24}};

    std::vector<std::array<int, 3 * 2 * 4>> inds
        = {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24},
            {1, 2, 3, 4, 9, 10, 11, 12, 17, 18, 19, 20, 5, 6, 7, 8, 13, 14, 15, 16, 21, 22, 23, 24},
            {1, 5, 9, 13, 17, 21, 2, 6, 10, 14, 18, 22, 3, 7, 11, 15, 19, 23, 4, 8, 12, 16, 20, 24},
            {1, 9, 17, 5, 13, 21, 2, 10, 18, 6, 14, 22, 3, 11, 19, 7, 15, 23, 4, 12, 20, 8, 16, 24}};

    for (int axis = 0; axis < 3; ++axis) {
        std::vector<int> res;
        std::copy(arr.cbegin(axis), arr.cend(axis), std::back_inserter(res));

        EXPECT_TRUE(std::equal(inds[axis].begin(), inds[axis].end(), res.begin()));
    }

    std::initializer_list<std::int64_t> order = {2, 1, 0};

    std::vector<int> res;
    std::copy(arr.cbegin(order), arr.cend(order), std::back_inserter(res));

    EXPECT_TRUE(std::equal(inds[3].begin(), inds[3].end(), res.begin()));

    // axis iterators
    std::for_each(arr.begin_subarray(), arr.end_subarray(), [](const auto& sa) {
        auto exsa = sa[interval<std::int64_t>{0, 0}];
        std::for_each(exsa.rbegin_subarray(), exsa.rend_subarray(), [](auto& sa) {
            sa *= 2;
        });
    });

    arrnd<int> axis_iter_res{
        {3, 2, 4}, {2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48}};

    EXPECT_TRUE(all_equal(axis_iter_res, arr));

    // std ranges
    {
        arrnd<int> arr5({2, 1, 3}, {1, 2, 3, 4, 5, 6});

        std::vector<int> vec1;

        for (int i : std::views::transform(std::views::filter(arr5,
                                               [](int i) {
                                                   return i % 2 == 0;
                                               }),
                 [](int i) {
                     return i * 2;
                 })) {
            vec1.push_back(i);
        }

        EXPECT_EQ(std::vector<int>({4, 8, 12}), vec1);
    }
}

TEST(arrnd_test, sortable_using_iterators)
{
    oc::arrnd<int> arr({3, 1, 4}, {5, 7, 10, 2, 8, 6, 1, 9, 0, 3, 11, 4});

    oc::arrnd<int> r1({3, 1, 4}, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11});

    auto c1 = arr.clone();
    std::sort(c1.begin(), c1.end());
    EXPECT_TRUE(oc::all_equal(c1, r1));

    auto c2 = arr.clone();
    std::sort(c2.cbegin(), c2.cend());
    EXPECT_TRUE(oc::all_equal(c2, r1));

    auto c3 = arr.clone();
    std::sort(c3.rbegin(), c3.rend(), std::greater<>{});
    EXPECT_TRUE(oc::all_equal(c3, r1));

    auto c4 = arr.clone();
    std::sort(c4.crbegin(), c4.crend(), std::greater<>{});
    EXPECT_TRUE(oc::all_equal(c4, r1));
}

TEST(arrnd_test, swap_array)
{
    using oc::swap;
    using std::swap;

    oc::arrnd<int> arr1({2}, {1, 2});
    oc::arrnd<int> arr2({4}, {3, 4, 5, 6});

    int x = 0;
    int y = 1;
    swap(x, y); // use std::swap;
    EXPECT_EQ(x, 1);
    EXPECT_EQ(y, 0);

    swap(arr1, arr2); // use oc::swap
    EXPECT_TRUE(oc::all_equal(arr1, oc::arrnd<int>({4}, {3, 4, 5, 6})));
    EXPECT_TRUE(oc::all_equal(arr2, oc::arrnd<int>({2}, {1, 2})));

    swap(arr1[{oc::interval<>::at(0)}], oc::arrnd<int>{}); // no swapping between arrays
    EXPECT_TRUE(oc::all_equal(arr1, oc::arrnd<int>({4}, {3, 4, 5, 6})));
    EXPECT_TRUE(oc::all_equal(arr2, oc::arrnd<int>({2}, {1, 2})));
}

TEST(arrnd_test, expand)
{
    using namespace oc;

    EXPECT_TRUE(all_equal(expand(arrnd<int>(), 0), arrnd<arrnd<int>>()));

    arrnd<arrnd<int>> narr({2, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})});

    EXPECT_TRUE(all_equal(expand(narr, 0),
        arrnd<arrnd<arrnd<int>>>({2, 1},
            {arrnd<arrnd<int>>(
                 {3}, {arrnd<int>({1, 1, 2}, {1, 2}), arrnd<int>({1, 1, 2}, {3, 4}), arrnd<int>({1, 1, 2}, {5, 6})}),
                arrnd<arrnd<int>>({2}, {arrnd<int>({1, 3}, {1, 2, 3}), arrnd<int>({1, 3}, {4, 5, 6})})})));

    EXPECT_TRUE(all_equal(expand<0>(narr, 0),
        arrnd<arrnd<arrnd<int>>>({2},
            {arrnd<arrnd<int>>({1, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})}),
                arrnd<arrnd<int>>({1, 1}, {arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})})})));
}

TEST(arrnd_test, array_copyable_view)
{
    using namespace oc;

    arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

    // can create an array view
    arrnd<int>::view_type arrv1 = arr[interval<>::at(0)];

    // can be cast to array view
    arrnd<int>::view_type arrv2 = arr[interval<>::at(1)];

    swap(arrv1, arrv2);
    EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 1, 2}, {3, 4, 1, 2, 5, 6})));

    swap(static_cast<arrnd<int>::view_type>(arr[interval<>::at(0)]),
        static_cast<arrnd<int>::view_type>(arr[interval<>::at(1)]));
    EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})));
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
    static_assert(oc::arrnd_complient_of_type<oc::arrnd<int>, int>);
    static_assert(!oc::arrnd_complient_of_type<oc::arrnd<int>, double>);
}

TEST(arrnd_test, can_check_if_array_base_type_is_of_specific_template_type_at_compile_time)
{
    oc::arrnd<std::complex<int>>{}; // required due to MSVC compiler issue

    static_assert(oc::arrnd_complient_of_template_type<oc::arrnd<std::complex<int>>, std::complex>);
    static_assert(!oc::arrnd_complient_of_template_type<oc::arrnd<std::complex<int>>, std::vector>);
}

TEST(arrnd_test, can_check_if_array_base_type_have_specific_type_trait_at_compile_time)
{
    static_assert(oc::arrnd_complient_with_trait<oc::arrnd<int>, std::is_integral>);
    static_assert(!oc::arrnd_complient_with_trait<oc::arrnd<int>, std::is_floating_point>);
}

TEST(arrnd_test, check_nested_arrnd_type)
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

TEST(arrnd_test, can_replace_arrnd_type_at_specific_depth)
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

    Integer_array sarr{arr[{{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}]};

    for (std::int64_t k = 0; k < rdims[0]; ++k) {
        for (std::int64_t i = 0; i < rdims[1]; ++i) {
            for (std::int64_t j = 0; j < rdims[2]; ++j) {
                EXPECT_EQ((rarr[{k, i, j}]), (sarr[{k, 0, i, j}]));
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
        EXPECT_TRUE(oc::all_equal(rnarr, oc::arrnd<double>({1, 2}, {7.5, 20})));
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
}

TEST(arrnd_test, element_wise_transform_operation)
{
    EXPECT_TRUE(oc::empty(oc::transform(oc::arrnd<int>({3, 1, 2}), oc::arrnd<double>({6}), [](int, double) {
        return 0.0;
    })));

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
        EXPECT_TRUE(oc::all_equal(rnarr, oc::arrnd<double>({1, 2}, {22.5, 60})));
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

        oc::arrnd<std::string> byaxis = oc::fold(arr[{{0, 1}, {2}}], 1,
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

    const int imask_data0[]{1, 0, 0, 1, 0, 1};
    oc::arrnd imask0{dims, imask_data0};
    const int rdata0[]{1, 4, 6};
    oc::arrnd rarr0{{3}, rdata0};
    EXPECT_TRUE(oc::all_equal(rarr0, oc::filter(iarr, imask0)));

    const int imask_data1[]{0, 0, 0, 0, 0, 0};
    oc::arrnd imask1{dims, imask_data1};
    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(iarr, imask1)));

    const int imask_data2[]{1, 1, 1, 1, 1, 1};
    oc::arrnd imask2{dims, imask_data2};
    const int rdata2[]{1, 2, 3, 4, 5, 6};
    oc::arrnd rarr2{{6}, rdata2};
    EXPECT_TRUE(oc::all_equal(rarr2, oc::filter(iarr, imask2)));

    EXPECT_TRUE(oc::all_equal(oc::arrnd<int>{}, oc::filter(oc::arrnd<int>{}, imask0)));

    // nested array
    {
        oc::arrnd<oc::arrnd<int>> inarr(
            {1, 2}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), oc::arrnd<int>({3, 1, 2}, {7, 8, 9, 10, 11, 12})});

        auto r1 = oc::filter(inarr, oc::arrnd<int>({3, 1, 2}, {0, 0, 1, 0, 0, 1}));
        EXPECT_TRUE(oc::all_equal(
            r1, oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>({2}, {3, 6}), oc::arrnd<int>({2}, {9, 12})})));

        auto r2 = oc::filter<0>(inarr, oc::arrnd<int>({1, 2}, {1, 0}));
        EXPECT_TRUE(oc::all_equal(r2, oc::arrnd<oc::arrnd<int>>({1}, {oc::arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})})));
    }
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
    EXPECT_TRUE(oc::all_equal(rarrs, oc::find((iarr[{{1, 1}}]), [](int a) {
        return a;
    })));

    // Get subarray, find values indices by predicate,
    // and use this indices in different array.
    {
        oc::arrnd sarr{iarr[{{1, 2}, {0}, {0, 1}}]};
        oc::arrnd not_zeros_inds{oc::find(sarr, [](int a) {
            return a != 0;
        })};

        oc::arrnd<std::int64_t> rinds1{{3}, {2, 4, 5}};
        EXPECT_TRUE(oc::all_equal(rinds1, not_zeros_inds));

        oc::arrnd<std::int64_t> rvals1{{3}, {12, 14, 15}};

        oc::arrnd rallvals1{{3, 1, 2}, {10, 11, 12, 13, 14, 15}};
        EXPECT_TRUE(oc::all_equal(rvals1, rallvals1[not_zeros_inds]));
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
        oc::arrnd sarr{iarr[{{1, 2}, {0}, {0, 1}}]};
        oc::arrnd not_zeros_inds{oc::find(sarr, oc::arrnd{{2, 1, 2}, {0, 1, 0, 1}})};

        oc::arrnd<std::int64_t> rinds1{{2}, {3, 5}};
        EXPECT_TRUE(oc::all_equal(rinds1, not_zeros_inds));

        oc::arrnd<std::int64_t> rvals1{{2}, {13, 15}};

        oc::arrnd rallvals1{{3, 1, 2}, {10, 11, 12, 13, 14, 15}};
        EXPECT_TRUE(oc::all_equal(rvals1, rallvals1[not_zeros_inds]));
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

TEST(arrnd_test, nest)
{
    using namespace oc;

    arrnd<int> iarr({10, 5, 6, 4});
    std::iota(iarr.begin(), iarr.end(), 1);

    auto sarr = iarr[{interval<>::between(4, 8, 2), interval<>::at(2), interval<>::between(0, 1), interval<>::at(2)}];
    EXPECT_TRUE(all_equal(sarr, arrnd<int>({3, 1, 2, 1}, {531, 535, 771, 775, 1011, 1015})));

    auto rarr = nest<2>(sarr);
    EXPECT_TRUE(all_equal(rarr,
        arrnd<arrnd<arrnd<int>>>({3},
            {arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {531, 535})}),
                arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {771, 775})}),
                arrnd<arrnd<int>>({1}, {arrnd<int>({2, 1}, {1011, 1015})})})));

    auto ind535 = find(iarr, [](int a) {
        return a == 535;
    })[0];
    EXPECT_EQ(535, iarr[ind535]);
    EXPECT_EQ(535, (rarr[{0}][{0}][{1, 0}]));

    iarr[ind535] = 0;
    EXPECT_EQ(0, iarr[ind535]);
    EXPECT_EQ(0, (rarr[{0}][{0}][{1, 0}]));
}

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
    EXPECT_TRUE(oc::empty(arr1 == Integer_array{{1}}));
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
    EXPECT_TRUE(oc::empty(arr1 != Integer_array{{1}}));
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
    EXPECT_TRUE(oc::empty(arr1 > Integer_array{{1}}));
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

    EXPECT_TRUE(oc::empty(arr1 >= Integer_array{{1}}));
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

    EXPECT_TRUE(oc::empty(arr1 < Integer_array{{1}}));
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
    EXPECT_TRUE(oc::empty(arr1 <= Integer_array{{1}}));
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
    EXPECT_TRUE(oc::empty(oc::close(arr1, Integer_array{{1}})));
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

    EXPECT_TRUE(oc::empty(arr1 + Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 += Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 - Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 -= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 * Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 *= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 / Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 /= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 % Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 %= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 ^ Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 ^= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 & Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 &= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 | Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 |= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 << Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 <<= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 >> Integer_array{{1}}));
    EXPECT_TRUE(oc::all_equal(arr1 >>= Integer_array{{1}}, arr1));

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

    EXPECT_TRUE(oc::empty(arr1 && Integer_array{{1}}));

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

    EXPECT_TRUE(oc::empty(arr1 || Integer_array{{1}}));

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

        Integer_array sarr{arr[{{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}]};
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
        EXPECT_TRUE(oc::all_match(Integer_array{}, Integer_array({}, 0), [](int, int) {
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

        Integer_array sarr{arr[{{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}]};
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
        EXPECT_TRUE(oc::any_match(Integer_array{}, Integer_array({}, 0), [](int, int) {
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

        Integer_array sarr{arr[{{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}]};
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
        EXPECT_TRUE(oc::all_equal(Integer_array{}, Integer_array({}, 0)));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::any_equal(Integer_array{}, Integer_array({}, 0)));
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

        Integer_array sarr{arr[{{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}]};
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
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::all_close(Integer_array{}, Integer_array({}, 0)));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(oc::any_close(Integer_array{}, Integer_array({}, 0)));
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
        EXPECT_TRUE(oc::all_equal(Integer_array{}, (Integer_array{}[{{0, 1}, {0, 4, 2}}])));
    }

    // ranges in dims
    {
        // nranges == ndims
        const int tdata1[] = {1, 5};
        const std::int64_t tdims1[] = {2, 1, 1};
        Integer_array tarr1{tdims1, tdata1};
        Integer_array sarr1{arr[{{0, 2, 2}, {0}, {0}}]};
        EXPECT_TRUE(oc::all_equal(tarr1, sarr1));
        EXPECT_EQ(arr.storage()->data(), sarr1.storage()->data());

        // nranges < ndims
        const int tdata2[] = {3, 4};
        const std::int64_t tdims2[] = {1, 1, 2};
        Integer_array tarr2{tdims2, tdata2};
        Integer_array sarr2{arr[{{1, 2, 2}}]};
        EXPECT_TRUE(oc::all_equal(tarr2, sarr2));
        EXPECT_EQ(arr.storage()->data(), sarr2.storage()->data());

        // nranges > ndims - ignore extra ranges
        Integer_array sarr3{arr[{{0, 2, 2}, {0}, {0}, {100, 100, 5}}]};
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

        arr[{{1, 2}, {0}, {1}}] = 100;
        // assignment of different type
        arr[{{0, 0}, {0}, {1}}] = 50.5;
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

    carr2[{{0, 1}, {0, 0}, {0, 1}}] = carr1;
    EXPECT_TRUE(oc::all_equal(rarr2, carr2));

    // slice copying by assignment (rvalue)
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        Integer_array starr3{tarr3[{{0, 5, 2}}]};
        rarr3[{{0, 5, 2}}] = starr3;
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue)
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        Integer_array starr3{tarr3[{{0, 5, 2}}]};
        Integer_array srarr3{rarr3[{{0, 5, 2}}]};
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
        oc::arrnd<double> starr3{tarr3[{{0, 5, 2}}]};
        rarr3[{{0, 5, 2}}] = starr3;
        EXPECT_TRUE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> starr3{tarr3[{{0, 5, 2}}]};
        rarr3[{{0, 3, 2}}] = starr3;
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        oc::arrnd<double> starr3{tarr3[{{0, 5, 2}}]};
        Integer_array srarr3{rarr3[{{0, 5, 2}}]};
        rarr3 = starr3;
        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
    }
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
    carr2[{{0, 1}, {0, 0}, {0, 1}}] = std::move(sarr2);
    EXPECT_TRUE(empty(sarr2));
    EXPECT_TRUE(oc::all_equal(sarr, carr2));

    // slice moving by assignment
    {
        const int tdata3[] = {1, 2, 3, 4, 5, 6};
        Integer_array tarr3{{6}, tdata3};

        const int rdata3[] = {0, 2, 0, 4, 0, 6};
        Integer_array rarr3{{6}, rdata3};

        EXPECT_FALSE(oc::all_equal(tarr3, rarr3));
        rarr3[{{0, 5, 2}}] = std::move(tarr3[{{0, 5, 2}}]);
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
        Integer_array srarr3{rarr3[{{0, 5, 2}}]};
        srarr3 = std::move(tarr3[{{0, 5, 2}}]);
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
        rarr3[{{0, 5, 2}}] = std::move(tarr3[{{0, 5, 2}}]);
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
        rarr3[{{0, 3, 2}}] = std::move(tarr3[{{0, 5, 2}}]);
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
        oc::arrnd<double> srarr3{rarr3[{{0, 5, 2}}]};
        srarr3 = std::move(tarr3[{{0, 5, 2}}]);
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

    Integer_array csubarr{oc::clone(sarr[{{1, 1}, {0, 0}, {0, 0}}])};
    EXPECT_TRUE(oc::all_equal((sarr[{{1, 1}, {0, 0}, {0, 0}}]), csubarr));
    csubarr[{0, 0, 0}] = 5;
    EXPECT_FALSE(oc::all_equal((sarr[{{1, 1}, {0, 0}, {0, 0}}]), csubarr));

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

        copy(src, dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        copy(src[{{0, 1}, {0, 0}, {1, 1}}], dst);
        arrnd<int> sres{};
        EXPECT_TRUE(all_equal(sres, dst));
    }

    // same dimensions
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src, dst);
        arrnd<int> res{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        EXPECT_TRUE(all_equal(res, dst));
    }
    {
        arrnd<double> src{{3, 1, 2}, {1, 2, 3, 4, 5, 6}};
        arrnd<int> dst{{3, 1, 2}, {6, 5, 4, 3, 2, 1}};

        copy(src[{{1, 2}, {0, 0}, {0, 0}}], dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        copy(src[{{0, 1}}], dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        copy(src[{{1, 2}}], dst[{{0, 2}, {0, 0}, {0, 0}}]);
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

        copy(src[{{1, 5}}], dst[{{0, 1}, {0, 0}, {0, 0}}]);
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
        std::initializer_list<interval<std::int64_t>> ranges{{0, 2}, {0, 0}, {1, 1}};
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
        EXPECT_TRUE(all_equal(arr_with_vals[{0}][{0, 1}][{interval<>::at(0), interval<>::full(1), interval<>::full(3)}],
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

        auto ref = set(src, dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        set(src[{{0, 1}, {0, 0}, {1, 1}}], dst);
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

        auto ref = set(src[{{1, 2}, {0, 0}, {0, 0}}], dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        auto ref = set(src[{{0, 1}}], dst[{{0, 1}, {0, 0}, {1, 1}}]);
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

        auto ref = set(src[{{1, 2}}], dst[{{0, 2}, {0, 0}, {0, 0}}]);
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

        auto ref = set(src[{{1, 5}}], dst[{{0, 1}, {0, 0}, {0, 0}}]);
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
        Integer_array rarr{oc::reshape(arr, {3, 1, 2})};
        EXPECT_TRUE(oc::all_equal(arr, rarr));
        EXPECT_EQ(arr.storage()->data(), rarr.storage()->data());
    }

    {
        const int tdata[] = {1, 5};
        const std::int64_t tdims[]{1, 2};
        Integer_array tarr{tdims, tdata};
        Integer_array x = arr[{{0, 2, 2}, {}, {}}];
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
        const int tdata[] = {1, 2, 3, 4, 5, 6};
        const std::int64_t tdims[]{3, 1, 2};
        Integer_array tarr{tdims, tdata};

        Integer_array rarr{oc::resize(arr, {3, 1, 2})};
        EXPECT_TRUE(oc::all_equal(tarr, rarr));
        EXPECT_NE(tarr.storage()->data(), rarr.storage()->data());
    }

    {
        Integer_array rarr{oc::resize(arr, {10})};
        EXPECT_FALSE(oc::all_equal(arr, rarr));
        EXPECT_TRUE(oc::all_equal(arr, (rarr[{{0, 5}}])));
        EXPECT_NE(arr.storage()->data(), rarr.storage()->data());
    }

    // nested array
    {
        oc::arrnd<Integer_array> inarr(
            {1, 2}, {Integer_array({5}, {1, 2, 3, 4, 5}), Integer_array({1, 5}, {6, 7, 8, 9, 10})});

        oc::arrnd<Integer_array> rnarr1 = oc::resize(inarr, {2, 2});
        EXPECT_FALSE(oc::all_equal(rnarr1, inarr));
        EXPECT_TRUE(oc::all_equal(Integer_array({2, 2}, {1, 2, 3, 4}), rnarr1[{0, 0}]));
        EXPECT_TRUE(oc::all_equal(Integer_array({2, 2}, {6, 7, 8, 9}), rnarr1[{0, 1}]));
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
        std::copy(arr.cbegin_subarray(2), arr.cend_subarray(2), oc::arrnd_back_inserter(res));
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {0, 0, 0, 0, 0, 1, 3, 5, 2, 4, 6}), res));
    }

    {
        arrnd<int> res({5}, 0);
        std::transform(
            arr.cbegin_subarray(), arr.cend_subarray(), oc::arrnd_front_inserter(res), [](const auto& slice) {
                return slice * 2;
            });
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {10, 12, 6, 8, 2, 4, 0, 0, 0, 0, 0}), res));
    }

    {
        arrnd<int> res({5}, 0);
        std::copy(arr.cbegin_subarray(2), arr.cend_subarray(2), oc::arrnd_inserter(res, 1));
        EXPECT_TRUE(all_equal(arrnd<int>({11}, {0, 1, 3, 5, 2, 4, 6, 0, 0, 0, 0}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::copy(arr.cbegin_subarray(), arr.cend_subarray(), oc::arrnd_axis_back_inserter(res, 2));
        EXPECT_TRUE(all_equal(arrnd<int>({1, 1, 8}, {0, 0, 1, 2, 3, 4, 5, 6}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::transform(
            arr.cbegin_subarray(), arr.cend_subarray(), oc::arrnd_axis_front_inserter(res, 2), [](const auto& slice) {
                return slice * 2;
            });
        EXPECT_TRUE(all_equal(arrnd<int>({1, 1, 8}, {10, 12, 6, 8, 2, 4, 0, 0}), res));
    }

    {
        arrnd<int> res({1, 1, 2}, {0, 0});
        std::copy(arr.cbegin_subarray(), arr.cend_subarray(), oc::arrnd_axis_inserter(res, 1, 2));
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
}

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
    Integer_array arr{dims, reinterpret_cast<const int*>(data)};

    const int sdata1[2][1][1][2][1]{{{{{11}, {17}}}}, {{{{47}, {53}}}}};
    const std::int64_t sdims1[]{2, 1, 1, 2, 1};
    Integer_array sarr1{sdims1, reinterpret_cast<const int*>(sdata1)};

    EXPECT_TRUE(oc::all_equal(sarr1, (arr[{{0, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2}}])));

    const int sdata2[1][1][1][1][1]{{{{{17}}}}};
    const std::int64_t sdims2[]{1, 1, 1, 1, 1};
    Integer_array sarr2{sdims2, reinterpret_cast<const int*>(sdata2)};

    EXPECT_TRUE(oc::all_equal(sarr2, (sarr1[{{0, 0}, {0, 0}, {0, 0}, {1, 1}, {0, 0}}])));

    {
        const int edata1[1][2][1]{{{11}, {17}}};
        oc::arrnd<int> rarr1{{1, 2, 1}, reinterpret_cast<const int*>(edata1)};
        auto earr1 = arr[{{0, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2}}][oc::interval<std::int64_t>{0, 0}]
                        [oc::interval<std::int64_t>{0, 0}];
        EXPECT_TRUE(oc::all_equal(rarr1, earr1));

        const int edata2[1][2][1]{{{47}, {53}}};
        oc::arrnd<int> rarr2{{1, 2, 1}, reinterpret_cast<const int*>(edata2)};
        auto earr2 = arr[{{0, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2}}][oc::interval<std::int64_t>{1, 1}]
                        [oc::interval<std::int64_t>{0, 0}];
        EXPECT_TRUE(oc::all_equal(rarr2, earr2));

        const int edata3[]{47, 53};
        oc::arrnd<int> rarr3{{1, 1, 1, 2, 1}, edata3};
        auto earr3 = arr[{{0, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2}}][{{1, 1}}][{{0, 0}}];
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
    }

    // one dimensional array
    {
        std::stringstream ss;
        ss << oc::arrnd<int>{{6}, {1, 2, 3, 4, 5, 6}};
        EXPECT_EQ("[1 2 3 4 5 6]", ss.str());
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
        ss << arr[{
            oc::interval<>::at(1), oc::interval<>::full(1), oc::interval<>::full(2), oc::interval<>::between(0, 2, 2)}];
        EXPECT_EQ("[[[[7 9]\n"
                  "   [10 12]]]]",
            ss.str());
    }

    // nested array
    {
        std::stringstream ss;
        ss << oc::arrnd<oc::arrnd<oc::arrnd<int>>>({2},
            {oc::arrnd<oc::arrnd<int>>({1, 2}, {oc::arrnd<int>(), oc::arrnd<int>()}),
                oc::arrnd<oc::arrnd<int>>({2, 2},
                    {oc::arrnd<int>({5}, {1, 2, 3, 4, 5}),
                        oc::arrnd<int>({2, 1, 2, 3}, {6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17}), oc::arrnd<int>(),
                        oc::arrnd<int>({4, 1}, {18, 19, 20, 21})})});
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
    }
}
