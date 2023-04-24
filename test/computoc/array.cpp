#include <gtest/gtest.h>

#include <cstdint>

#include <array>
#include <stdexcept>
#include <regex>
#include <string>
#include <span>
#include <ranges>

#include <computoc/array.h>

template <typename T, typename U>
[[nodiscard]] inline bool operator==(const std::span<T>& lhs, const std::span<U>& rhs) {
    return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
}

class Require_test : public testing::Test {
protected:
    using Selected_exception = std::runtime_error;

    const bool true_condition_{ true };
    const bool false_condition_{ false };

    const std::regex error_message_pattern_regex_ = std::regex("^.+ exception \\(at line [0-9]+, .+@.+\\), assertion .+ failed: .+$");
};

TEST_F(Require_test, not_throw_exception_if_condition_is_true)
{
    EXPECT_NO_THROW(_REQUIRE(true_condition_, Selected_exception));
}

TEST_F(Require_test, throw_exception_if_condition_is_false)
{
    EXPECT_THROW(_REQUIRE(false_condition_, Selected_exception), Selected_exception);
}

struct CustomTestType {};
template <>
struct std::formatter<CustomTestType> : std::formatter<std::string> {
    auto format(const CustomTestType& vec, format_context& ctx) {
        return std::formatter<std::string>::format("CustomTestType as string", ctx);
    }
};

TEST_F(Require_test, throws_an_exception_with_specific_format)
{
    try {
        _REQUIRE(false_condition_, Selected_exception, std::format("{}", CustomTestType{}));
        FAIL();
    }
    catch (const Selected_exception& ex) {
        EXPECT_TRUE(std::regex_match(ex.what(), error_message_pattern_regex_));
    }
}

namespace optional_test_dummies {
    template <typename T>
    using Optional = computoc::Expected<T>;

    constexpr computoc::None_option nullopt{};

    auto to_int(std::string_view sv) -> Optional<int>
    {
        int r{};
        auto [ptr, ec] { std::from_chars(sv.data(), sv.data() + sv.size(), r) };
        if (ec == std::errc()) {
            return r;
        }
        return nullopt;
    };

    auto inc(int n)
    {
        return n + 1;
    }

    auto to_string(int n)
    {
        return std::to_string(n);
    }

    auto get_null_opt() -> Optional<std::string>
    {
        return "Null";
    }
}

TEST(Expected_test, using_Expected_type_as_Optional)
{
    using namespace optional_test_dummies;

    std::vector<Optional<std::string>> input = {
        "1234", "15 foo", "bar", "42", "5000000000", " 5" };

    std::vector<std::string> results = {
        "1235", "16", "Null", "43", "Null", "Null" };

    auto to_incremented_string = [](auto&& o) {
        return o.and_then(to_int)
            .transform(inc)
            .transform(to_string)
            .or_else(get_null_opt);
    };

    std::vector<std::string> output;

    for (auto&& x : input | std::views::transform(to_incremented_string)) {
        output.push_back(*x);
    }

    EXPECT_EQ(results, output);
}

namespace expected_test_dummies {
    using namespace std::literals;

    auto to_int(std::string_view sv) -> computoc::Expected<int, std::string>
    {
        int r{};
        auto [ptr, ec] { std::from_chars(sv.data(), sv.data() + sv.size(), r) };
        if (ec == std::errc()) {
            return r;
        }
        return computoc::Unexpected{ "Null"s };
    };

    auto inc(int n)
    {
        return n + 1;
    }

    auto get_failure(const std::string&) -> computoc::Expected<int, std::string>
    {
        return computoc::Unexpected{ "conversion failed"s };
    }

    auto decorate_as_error(const std::string& s)
    {
        return std::format("error: {}", s);
    }
}

TEST(Expected_test, using_Expected_with_value_and_error)
{
    using namespace expected_test_dummies;
    using namespace computoc;

    std::vector<Expected<std::string, std::string>> input = {
        "1234", "15 foo", "bar", "42", "5000000000", " 5" };

    std::vector<int> successful_results = { 1235, 16, 0, 43, 0, 0 };

    std::vector<std::string> failed_results = { "error: conversion failed", "error: conversion failed", "error: conversion failed" };

    auto to_incremented_string = [](auto&& o) {
        return o.and_then(to_int)
            .transform(inc)
            .or_else(get_failure)
            .transform_error(decorate_as_error);
    };

    std::vector<int> successful_output;
    std::vector<std::string> failed_output;

    for (auto&& x : input | std::views::transform(to_incremented_string)) {
        successful_output.push_back(x.value_or(0));
        if (!x) {
            failed_output.push_back(x.error());
        }
    }

    EXPECT_EQ(successful_results, successful_output);
    EXPECT_EQ(failed_results, failed_output);
}



TEST(Algorithms_test, two_numbers_can_be_compared_with_specified_percision)
{
    using namespace computoc;

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

    EXPECT_EQ(0, modulo(0, 5));
    EXPECT_EQ(1, modulo(1, 5));
    EXPECT_EQ(1, modulo(26, 5));
    EXPECT_EQ(4, modulo(-1, 5));
    EXPECT_EQ(4, modulo(-26, 5));
}



TEST(Interval_test, initialization)
{
    computoc::Interval i1{};
    EXPECT_EQ(0, i1.start);
    EXPECT_EQ(0, i1.stop);
    EXPECT_EQ(1, i1.step);

    computoc::Interval i2{ 1 };
    EXPECT_EQ(1, i2.start);
    EXPECT_EQ(1, i2.stop);
    EXPECT_EQ(1, i2.step);

    computoc::Interval i3{ 1, 2 };
    EXPECT_EQ(1, i3.start);
    EXPECT_EQ(2, i3.stop);
    EXPECT_EQ(1, i3.step);

    computoc::Interval i4{ 1, 2, 3 };
    EXPECT_EQ(1, i4.start);
    EXPECT_EQ(2, i4.stop);
    EXPECT_EQ(3, i4.step);
}

TEST(Interval_test, reverse)
{
    computoc::Interval i{ computoc::reverse(computoc::Interval{ 1, 2, 3 }) };
    EXPECT_EQ(2, i.start);
    EXPECT_EQ(1, i.stop);
    EXPECT_EQ(-3, i.step);
}

TEST(Interval_test, modulo)
{
    computoc::Interval i{ computoc::modulo(computoc::Interval{-26, 26, -1}, 5) };
    EXPECT_EQ(4, i.start);
    EXPECT_EQ(1, i.stop);
    EXPECT_EQ(-1, i.step);
}

TEST(Interval_test, forward)
{
    computoc::Interval i1{ computoc::forward(computoc::Interval{ 1, 2, 3 }) };
    EXPECT_EQ(1, i1.start);
    EXPECT_EQ(2, i1.stop);
    EXPECT_EQ(3, i1.step);

    computoc::Interval i2{ computoc::forward(computoc::Interval{ 2, 1, -3 }) };
    EXPECT_EQ(1, i2.start);
    EXPECT_EQ(2, i2.stop);
    EXPECT_EQ(3, i2.step);
}

TEST(Simple_dynamic_vector_test, span_and_iterators_usage)
{
    using simple_vector = computoc::details::simple_dynamic_vector<std::string>;

    auto count_elements = [](std::span<const std::string> s) {
        return s.size();
    };

    simple_vector sv(2, std::array<std::string, 2>{ "first string", "second string" }.data());
    EXPECT_EQ(2, count_elements(sv));
    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) { return s.find("string") != std::string::npos; }));
}

TEST(Simple_dynamic_vector_test, basic_functionality)
{
    using simple_vector = computoc::details::simple_dynamic_vector<std::string>;

    std::array<std::string, 16> arr{
        "a", "b", "c", "d", "e", "f", "g", "h",
        "i", "j", "k", "l", "m", "n", "o", "p" };

    simple_vector sv(16, arr.data());
    EXPECT_EQ(16, sv.capacity());
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

    sv.resize(24);
    EXPECT_EQ(24, sv.capacity());
    EXPECT_EQ(24, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.expand(1);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(25, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.shrink(10);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(15, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());
    EXPECT_THROW(sv.shrink(30), std::length_error);

    sv.expand(5);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(20, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(50);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(20, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(45);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(20, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.shrink_to_fit();
    EXPECT_EQ(20, sv.capacity());
    EXPECT_EQ(20, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());
}

TEST(Simple_static_vector_test, span_usage)
{
    using simple_vector = computoc::details::simple_static_vector<std::string, 2>;

    auto count_elements = [](std::span<const std::string> s) {
        return s.size();
    };

    simple_vector sv(2, std::array<std::string, 2>{ "first string", "second string" }.data());
    EXPECT_EQ(2, count_elements(sv));
    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) { return s.find("string") != std::string::npos; }));
}

TEST(Simple_static_vector_test, basic_functionality)
{
    using simple_vector = computoc::details::simple_static_vector<std::string, 16>;

    std::array<std::string, 16> arr{
        "a", "b", "c", "d", "e", "f", "g", "h",
        "i", "j", "k", "l", "m", "n", "o", "p" };

    simple_vector sv(16, arr.data());
    EXPECT_EQ(16, sv.capacity());
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

    EXPECT_THROW(sv.resize(24), std::length_error);
    sv.resize(8);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(8, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    EXPECT_EQ("h", sv.back());
    sv.resize(9);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(9, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("h", sv.back());

    EXPECT_THROW(sv.expand(10), std::length_error);
    sv.expand(1);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(10, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    EXPECT_THROW(sv.shrink(12), std::length_error);
    sv.shrink(5);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(5, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());
}


TEST(Array_test, iterators)
{
    using namespace computoc;

    const Array<int> arr1{ {3, 1, 2}, {
        1, 2,
        3, 4,
        5, 6} };
    Array<int> arr2{ {3, 1, 2}, {0, 1, 2, 3, 4, 5} };

    auto product = std::inner_product(arr1.cbegin(), arr1.cend(), arr2.begin(), 1);

    EXPECT_EQ(71, product);

    std::transform(arr1.crbegin(), arr1.crend(), arr2.begin(), [](auto c) { return c + 1; });

    EXPECT_TRUE(all_equal(Array<int>({ 3, 1, 2 },
        { 7, 6, 5, 4, 3, 2 }), arr2));

    std::transform(arr1.cbegin() + 1, arr1.cend() - 2, arr2.begin() + 2, [](auto a) { return a * 10; });

    EXPECT_TRUE(all_equal(Array<int>({ 3, 1, 2 },
        { 7, 6, 20, 30, 40, 2 }), arr2));

    std::transform(arr1.crbegin() + 1, arr1.crend() - 2, arr2.rbegin() + 1, [](auto a) { return a * 1000; });

    EXPECT_TRUE(all_equal(Array<int>({ 3, 1, 2 },
        { 7, 6, 3000, 4000, 5000, 2 }), arr2));

    std::transform(arr1.cbegin(), ++(arr1.cbegin()), arr2({ {1, 1, 2}, {0, 0}, {1, 1} }).rbegin(), [](auto a) { return a * 100; });

    EXPECT_TRUE(all_equal(Array<int>({ 3, 1, 2 },
        { 7, 6, 3000, 100, 5000, 2 }), arr2));

    const Array<int> arr{ {3, 2, 4}, {
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12,
        13, 14, 15, 16,
        17, 18, 19, 20,
        21, 22, 23, 24} };

    std::vector<std::array<int, 3 * 2 * 4>> inds = {
        {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24},
        {1, 2, 3, 4, 9, 10, 11, 12, 17, 18, 19, 20, 5, 6, 7, 8, 13, 14, 15, 16, 21, 22, 23, 24},
        {1, 5, 9, 13, 17, 21, 2, 6, 10, 14, 18, 22, 3, 7, 11, 15, 19, 23, 4, 8, 12, 16, 20, 24},
        {1, 9, 17, 5, 13, 21, 2, 10, 18, 6, 14, 22, 3, 11, 19, 7, 15, 23, 4, 12, 20, 8, 16, 24}
    };

    for (int axis = 0; axis < 3; ++axis) {
        std::vector<int> res;
        std::copy(arr.cbegin(axis), arr.cend(axis), std::back_inserter(res));

        EXPECT_TRUE(std::equal(inds[axis].begin(), inds[axis].end(), res.begin()));
    }

    std::initializer_list<std::int64_t> order = { 2, 1, 0 };

    std::vector<int> res;
    std::copy(arr.cbegin(order), arr.cend(order), std::back_inserter(res));

    EXPECT_TRUE(std::equal(inds[3].begin(), inds[3].end(), res.begin()));
}

TEST(Simple_array_indices_generator, simple_forward_backward_iterations)
{
    using namespace computoc::details;

    const std::int64_t dims[]{ 3, 1, 2 }; // strides = {2, 2, 1}
    Array_header hdr(std::span(dims, 3));

    const std::int64_t expected_inds_list[6]{
        0, 1,
        2, 3,
        4, 5 };
    const std::int64_t expected_generated_subs{ 6 };

    std::int64_t generated_subs_counter{ 0 };
    Simple_array_indices_generator gen(hdr);

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

TEST(Simple_array_indices_generator, simple_backward_forward_iterations)
{
    using namespace computoc::details;

    const std::int64_t dims[]{ 3, 1, 2 }; // strides = {2, 2, 1}
    Array_header hdr(std::span(dims, 3));

    const std::int64_t expected_inds_list[6]{
        5, 4,
        3, 2,
        1, 0 };
    const std::int64_t expected_generated_subs{ 6 };

    std::int64_t generated_subs_counter{ 0 };
    Simple_array_indices_generator gen(hdr, true);

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

TEST(Simple_array_indices_generator, simple_forward_backward_iterations_with_steps_bigger_than_one)
{
    using namespace computoc::details;

    const std::int64_t dims[]{ 3, 1, 2 }; // strides = {2, 2, 1}
    Array_header hdr(std::span(dims, 3));

    const std::int64_t expected_inds_list[6]{
        0,
        2,
        4 };
    const std::int64_t expected_generated_subs{ 3 };

    std::int64_t generated_subs_counter{ 0 };
    Simple_array_indices_generator gen(hdr);

    while (gen) {
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
        ++generated_subs_counter;
        gen += 2;
    }
    EXPECT_EQ(expected_generated_subs, generated_subs_counter);

    while (gen = gen - 2) {
        --generated_subs_counter;
        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
    }
    EXPECT_EQ(0, generated_subs_counter);
}

TEST(Simple_array_indices_generator, forward_backward_iterations_by_axis_order)
{
    using namespace computoc::details;

    const std::int64_t dims[]{ 3, 1, 2 }; // strides = {2, 2, 1}
    const std::int64_t order[]{ 2, 0, 1 };
    Array_header hdr(std::span(dims, 3));

    const std::int64_t expected_inds_list[6]{
        0, 2, 4,
        1, 3, 5 };
    const std::int64_t expected_generated_subs{ 6 };

    std::int64_t generated_subs_counter{ 0 };
    Simple_array_indices_generator gen(hdr, std::span(order, 3));

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

TEST(Simple_array_indices_generator, forward_backward_iterations_by_specific_major_axis)
{
    using namespace computoc::details;

    const std::int64_t dims[]{ 3, 1, 2 }; // strides = {2, 2, 1}
    Array_header hdr(std::span(dims, 3));

    const std::int64_t expected_inds_list[][6]{
        { 0, 1,
          2, 3,
          4, 5 },

        { 0, 1,
          2, 3,
          4, 5 },

        { 0, 2, 4,
          1, 3, 5} };
    const std::int64_t expected_generated_subs{ 6 };

    for (std::int64_t axis = 0; axis <= 2; ++axis) {
        std::int64_t generated_subs_counter{ 0 };
        Simple_array_indices_generator gen(hdr, axis);

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


TEST(Array_test, can_be_initialized_with_valid_size_and_data)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = { 0, 0, 0 };
    EXPECT_NO_THROW((Integer_array{ {1, 1}, data }));
    EXPECT_NO_THROW((Integer_array{ {1, 3}, data }));
    EXPECT_NO_THROW((Integer_array{ {3, 1}, data }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, { 0, 0, 0 } }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, { 0, 0, 0 } }));

    const double ddata[] = { 0.0, 0.0, 0.0 };
    EXPECT_NO_THROW((Integer_array{ {1, 1}, ddata }));
    EXPECT_NO_THROW((Integer_array{ {1, 3}, ddata }));
    EXPECT_NO_THROW((Integer_array{ {3, 1}, ddata }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, { 0.0, 0.0, 0.0 } }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, { 0.0, 0.0, 0.0 } }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1}, data }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 1, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0, 1}, data }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1, 0}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0, 1}, data }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1, 1}, data }));
}

TEST(Array_test, can_be_initialized_with_valid_size_and_value)
{
    using Integer_array = computoc::Array<int>;

    const int value{ 0 };
    EXPECT_NO_THROW((Integer_array{ {1, 1}, value }));
    EXPECT_NO_THROW((Integer_array{ {1, 3}, value }));
    EXPECT_NO_THROW((Integer_array{ {3, 1}, value }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, value }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, value }));

    const double dvalue{ 0.0 };
    EXPECT_NO_THROW((Integer_array{ {1, 1}, dvalue }));
    EXPECT_NO_THROW((Integer_array{ {1, 3}, dvalue }));
    EXPECT_NO_THROW((Integer_array{ {3, 1}, dvalue }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, dvalue }));
    EXPECT_NO_THROW((Integer_array{ {3, 1, 1}, dvalue }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1}, value }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 1, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {1, 0, 1}, value }));

    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1, 0}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 0, 1}, value }));
    EXPECT_TRUE(computoc::empty(Integer_array{ {0, 1, 1}, value }));
}

TEST(Array_test, can_return_its_header_and_data)
{
    using Integer_array = computoc::Array<int>;

    Integer_array earr{};
    const Integer_array::Header& ehdr{ earr.header() };

    EXPECT_EQ(0, ehdr.dims().size());
    EXPECT_EQ(0, ehdr.count());
    EXPECT_TRUE(ehdr.dims().empty());
    EXPECT_TRUE(ehdr.strides().empty());
    EXPECT_EQ(0, ehdr.offset());
    EXPECT_FALSE(ehdr.is_subarray());
    EXPECT_TRUE(earr.block().empty());

    const int value{ 0 };
    Integer_array arr{ {3, 1, 2}, value };
    const Integer_array::Header& hdr{ arr.header() };

    EXPECT_EQ(3, hdr.dims().size());
    EXPECT_EQ(6, hdr.count());
    EXPECT_EQ(3, hdr.dims().data()[0]); EXPECT_EQ(1, hdr.dims().data()[1]); EXPECT_EQ(2, hdr.dims().data()[2]);
    EXPECT_EQ(2, hdr.strides().data()[0]); EXPECT_EQ(2, hdr.strides().data()[1]); EXPECT_EQ(1, hdr.strides().data()[2]);
    EXPECT_EQ(0, hdr.offset());
    EXPECT_FALSE(hdr.is_subarray());
    EXPECT_FALSE(arr.block().empty());
    for (std::int64_t i = 0; i < hdr.count(); ++i) {
        EXPECT_EQ(0, arr.data()[i]);
    }
}

TEST(Array_test, have_read_write_access_to_its_cells)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };

    Integer_array arr1d{ {6}, data };
    const std::int64_t* dims1d{ arr1d.header().dims().data() };
    for (std::int64_t i = 0; i < dims1d[0]; ++i) {
        EXPECT_EQ(arr1d({ i }), data[i]);
    }
    EXPECT_EQ(1, arr1d({ 6 }));
    EXPECT_EQ(6, arr1d({ -1 }));
    for (std::int64_t i = 0; i < dims1d[0]; ++i) {
        arr1d({ i }) = 0;
        EXPECT_EQ(arr1d({ i }), 0);
    }

    Integer_array arr2d{ {3, 2}, data };
    const std::int64_t* dims2d{ arr2d.header().dims().data() };
    for (std::int64_t i = 0; i < dims2d[0]; ++i) {
        for (std::int64_t j = 0; j < dims2d[1]; ++j) {
            EXPECT_EQ(arr2d({ i, j }), data[i * dims2d[1] + j]);
        }
    }
    EXPECT_EQ(1, arr2d({ 3, 2 }));
    EXPECT_EQ(6, arr2d({ -1, -1 }));
    for (std::int64_t i = 0; i < dims2d[0]; ++i) {
        for (std::int64_t j = 0; j < dims2d[1]; ++j) {
            arr2d({ i, j }) = 0;
            EXPECT_EQ(arr2d({ i, j }), 0);
        }
    }

    Integer_array arr3d{ {3, 1, 2}, data };
    const std::int64_t* dims3d{ arr3d.header().dims().data() };
    for (std::int64_t k = 0; k < dims3d[0]; ++k) {
        for (std::int64_t i = 0; i < dims3d[1]; ++i) {
            for (std::int64_t j = 0; j < dims3d[2]; ++j) {
                EXPECT_EQ(arr3d({ k, i, j }), data[k * (dims3d[1] * dims3d[2]) + i * dims3d[2] + j]);
            }
        }
    }
    EXPECT_EQ(1, arr3d({ 3, 1, 2 }));
    EXPECT_EQ(6, arr3d({ -1, -1, -1 }));
    for (std::int64_t k = 0; k < dims3d[0]; ++k) {
        for (std::int64_t i = 0; i < dims3d[1]; ++i) {
            for (std::int64_t j = 0; j < dims3d[2]; ++j) {
                arr3d({ k, i, j }) = 0;
                EXPECT_EQ(arr3d({ k, i, j }), 0);
            }
        }
    }

    // partial subscripts
    {
        Integer_array parr{ {3, 1, 2}, data };

        EXPECT_EQ(parr({ 0, 0, 0 }), parr({ 0 }));
        EXPECT_EQ(parr({ 0, 0, 1 }), parr({ 1 }));
        EXPECT_EQ(parr({ 0, 0, 0 }), parr({ 0, 0 }));
        EXPECT_EQ(parr({ 0, 0, 1 }), parr({ 0, 1 }));

        // extra subscripts are being ignored
        EXPECT_EQ(parr({ 0, 0, 0 }), parr({ 0, 0, 0, 10 }));
        EXPECT_EQ(parr({ 2, 0, 1 }), parr({ 2, 0, 1, 10 }));
    }

    // different data type
    {
        const int rdata[6]{ 0 };

        Integer_array arr1({ 6 }, 0.5);
        for (std::int64_t i = 0; i < 6; ++i) {
            EXPECT_EQ(rdata[i], arr1({ i }));
        }

        const double data2[]{ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6 };
        Integer_array arr2({ 6 }, data2);
        for (std::int64_t i = 0; i < 6; ++i) {
            EXPECT_EQ(rdata[i], arr2({ i }));
        }
    }
}

TEST(Array_test, have_read_write_access_to_slice)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,

        10, 11, 12,
        13, 14, 15,
        16, 17, 18,

        19, 20, 21,
        22, 23, 24,
        25, 26, 27,

        28, 29, 30,
        31, 32, 33,
        34, 35, 36 };
    const std::int64_t dims[]{ 2, 2, 3, 3 };
    Integer_array arr{ {dims, 4}, data };

    const int rdata[] = {
        11,
        14,

        29,
        32
    };
    const std::int64_t rdims[]{ 2, 2, 1 };
    Integer_array rarr{ {rdims, 3}, rdata };

    Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };

    for (std::int64_t k = 0; k < rdims[0]; ++k) {
        for (std::int64_t i = 0; i < rdims[1]; ++i) {
            for (std::int64_t j = 0; j < rdims[2]; ++j) {
                EXPECT_EQ(rarr({ k, i, j }), sarr({ k, 0, i, j }));
            }
        }
    }

    //std::transform(sarr.cbegin(), sarr.cend(), sarr.begin(), [](auto a) { return a * 100; });
}

TEST(Array_test, element_wise_transformation)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    const double odata[]{
        0.5, 1.0,
        1.5, 2.0,
        2.5, 3.0 };
    computoc::Array oarr{ {dims, 3}, odata };

    EXPECT_TRUE(computoc::all_equal(oarr, computoc::transform(iarr, [](int n) {return n * 0.5; })));
}

TEST(Array_test, element_wise_transform_operation)
{
    EXPECT_TRUE(computoc::empty(computoc::transform(computoc::Array<int>({ 3, 1, 2 }), computoc::Array<double>({ 6 }), [](int, double) {return 0.0; })));

    std::int64_t dims[]{ 3, 1, 2 };

    const int idata1[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr1{ {dims, 3}, idata1 };

    const double idata2[]{
        0.5, 1.0,
        1.5, 2.0,
        2.5, 3.0 };
    computoc::Array iarr2{ {dims, 3}, idata2 };

    computoc::Array oarr1{ {dims, 3}, 0.5 };

    EXPECT_TRUE(computoc::all_equal(oarr1, computoc::transform(iarr1, iarr2, [](int a, double b) { return b / a; })));

    const int odata2[] = {
        0, 1,
        2, 3,
        4, 5 };
    computoc::Array oarr2{ {dims, 3}, odata2 };

    EXPECT_TRUE(computoc::all_equal(oarr2, computoc::transform(iarr1, 1, [](int a, int b) { return a - b; })));

    const int odata3[] = {
        0, -1,
        -2, -3,
        -4, -5 };
    computoc::Array oarr3{ {dims, 3}, odata3 };

    EXPECT_TRUE(computoc::all_equal(oarr3, computoc::transform(1, iarr1, [](int a, int b) { return a - b; })));
}

TEST(Array_test, reduce_elements)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    EXPECT_EQ((1.0 / 2 / 3 / 4 / 5 / 6), computoc::reduce(iarr, [](double a, int b) {return a / b; }));

    std::int64_t dims2[]{ 3, 1 };
    const double rdata2[]{
        3.0,
        7.0,
        11.0
    };
    computoc::Array rarr2{ {dims2, 2}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 2)));

    std::int64_t dims1[]{ 3, 2 };
    const double rdata1[]{
        1.0, 2.0,
        3.0, 4.0,
        5.0, 6.0 };
    computoc::Array rarr1{ {dims1, 2}, rdata1 };
    EXPECT_TRUE(computoc::all_equal(rarr1, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 1)));

    std::int64_t dims0[]{ 1, 2 };
    const double rdata0[]{
        9.0, 12.0 };
    computoc::Array rarr0{ {dims0, 2}, rdata0 };
    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 0)));

    computoc::Array iarr1d{ {6}, idata };
    const double data1d[]{ 21.0 };
    computoc::Array rarr1d{ {1}, data1d };
    EXPECT_TRUE(computoc::all_equal(rarr1d, computoc::reduce(iarr1d, [](int value, double previous) {return previous + value; }, 0)));

    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 3)));

    // reduction with initial value(s)
    {
        computoc::Array arr{ {2, 2}, {1, 2, 5, 6} };

        std::string chain = computoc::reduce(
            arr,
            std::string{},
            [](const std::string& s, int n) { return s + "-" + std::to_string(n); });
        EXPECT_EQ("-1-2-5-6", chain);

        computoc::Array<std::string> byaxis = computoc::reduce(
            arr({ {0,1}, {1} }),
            computoc::Array{ {2}, {std::to_string(arr({0,0})), std::to_string(arr({1,0}))} },
            [](const std::string& s, int n) { return s + "-" + std::to_string(n); },
            1);
        EXPECT_TRUE(computoc::all_equal(computoc::Array{ {2},
            {std::string{"1-2"}, std::string{"5-6"}} }, byaxis));
    }

    // complex array reduction
    {
        auto sum = [](int value, double previous) {
            return previous + value;
        };

        EXPECT_TRUE(computoc::all_equal(rarr1d, computoc::reduce(computoc::reduce(computoc::reduce(iarr, sum, 2), sum, 1), sum, 0)));
    }
}

TEST(Array_test, all)
{
    const bool data[] = {
        1, 0,
        1, 1 };
    computoc::Array<int> arr{ {2, 2}, data };

    EXPECT_EQ(false, computoc::all(arr));

    const bool rdata[] = { true, false };
    computoc::Array<bool> rarr{ {2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, computoc::all(arr, 0)));
}

TEST(Array_test, any)
{
    const bool data[] = {
        1, 0,
        0, 0 };
    computoc::Array<int> arr{ {2, 2}, data };

    EXPECT_EQ(true, computoc::any(arr));

    const bool rdata[] = { true, false };
    computoc::Array<bool> rarr{ {2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, computoc::any(arr, 0)));
}

TEST(Array_test, filter_elements_by_condition)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 0,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    const int rdata0[]{ 1, 2, 3, 0, 5, 6 };
    computoc::Array rarr0{ {6}, rdata0 };
    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::filter(iarr, [](int) {return 1; })));

    const int rdata1[]{ 1, 2, 3, 5, 6 };
    computoc::Array rarr1{ {5}, rdata1 };
    EXPECT_TRUE(computoc::all_equal(rarr1, computoc::filter(iarr, [](int a) { return a; })));

    const double rdata2[]{ 2.0, 0.0, 6.0 };
    computoc::Array rarr2{ {3}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, computoc::filter(iarr, [](int a) { return a % 2 == 0; })));

    EXPECT_TRUE(computoc::all_equal(computoc::Array<int>{}, computoc::filter(iarr, [](int a) { return a > 6; })));
    EXPECT_TRUE(computoc::all_equal(computoc::Array<int>{}, computoc::filter(computoc::Array<int>{}, [](int) {return 1; })));
}

TEST(Array_test, filter_elements_by_maks)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    EXPECT_TRUE(computoc::empty(computoc::filter(iarr, computoc::Array<int>{})));

    const int imask_data0[]{
        1, 0,
        0, 1,
        0, 1 };
    computoc::Array imask0{ {dims, 3}, imask_data0 };
    const int rdata0[]{ 1, 4, 6 };
    computoc::Array rarr0{ {3}, rdata0 };
    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::filter(iarr, imask0)));

    const int imask_data1[]{
        0, 0,
        0, 0,
        0, 0 };
    computoc::Array imask1{ {dims, 3}, imask_data1 };
    EXPECT_TRUE(computoc::all_equal(computoc::Array<int>{}, computoc::filter(iarr, imask1)));

    const int imask_data2[]{
        1, 1,
        1, 1,
        1, 1 };
    computoc::Array imask2{ {dims, 3}, imask_data2 };
    const int rdata2[]{ 1, 2, 3, 4, 5, 6 };
    computoc::Array rarr2{ {6}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, computoc::filter(iarr, imask2)));

    EXPECT_TRUE(computoc::all_equal(computoc::Array<int>{}, computoc::filter(computoc::Array<int>{}, imask0)));
}

TEST(Array_test, select_elements_indices_by_condition)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 0,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    const std::int64_t rdata0[]{ 0, 1, 2, 3, 4, 5 };
    computoc::Array rarr0{ {6}, rdata0 };
    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::find(iarr, [](int) {return 1; })));

    const std::int64_t rdata1[]{ 0, 1, 2, 4, 5 };
    computoc::Array rarr1{ {5}, rdata1 };
    EXPECT_TRUE(computoc::all_equal(rarr1, computoc::find(iarr, [](int a) { return a; })));

    const std::int64_t rdata2[]{ 1, 3, 5 };
    computoc::Array rarr2{ {3}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, computoc::find(iarr, [](int a) { return a % 2 == 0; })));

    EXPECT_TRUE(computoc::all_equal(computoc::Array<std::int64_t>{}, computoc::find(iarr, [](int a) { return a > 6; })));
    EXPECT_TRUE(computoc::all_equal(computoc::Array<std::int64_t>{}, computoc::find(computoc::Array<int>{}, [](int) {return 1; })));

    // subarray
    const std::int64_t rdatas[]{ 2 };
    computoc::Array rarrs{ {1}, rdatas };
    EXPECT_TRUE(computoc::all_equal(rarrs, computoc::find(iarr({ {1, 1} }), [](int a) { return a; })));

    // Get subarray, find values indices by predicate,
    // and use this indices in different array.
    {
        computoc::Array sarr{ iarr({{1, 2}, {0}, {0, 1}}) };
        computoc::Array not_zeros_inds{ computoc::find(sarr, [](int a) {return a != 0; }) };

        computoc::Array<std::int64_t> rinds1{ {3}, {2, 4, 5} };
        EXPECT_TRUE(computoc::all_equal(rinds1, not_zeros_inds));

        computoc::Array<std::int64_t> rvals1{ {3}, {12, 14, 15} };

        computoc::Array rallvals1{ {3, 1, 2}, {
            10, 11,
            12, 13,
            14, 15 } };
        EXPECT_TRUE(computoc::all_equal(rvals1, rallvals1(not_zeros_inds)));
    }
}

TEST(Array_test, select_elements_indices_by_maks)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {dims, 3}, idata };

    EXPECT_TRUE(computoc::empty(computoc::find(iarr, computoc::Array<int>{})));

    const int imask_data0[]{
        1, 0,
        0, 1,
        0, 1 };
    computoc::Array imask0{ {dims, 3}, imask_data0 };
    const std::int64_t rdata0[]{ 0, 3, 5 };
    computoc::Array rarr0{ {3}, rdata0 };
    EXPECT_TRUE(computoc::all_equal(rarr0, computoc::find(iarr, imask0)));

    const int imask_data1[]{
        0, 0,
        0, 0,
        0, 0 };
    computoc::Array imask1{ {dims, 3}, imask_data1 };
    EXPECT_TRUE(computoc::all_equal(computoc::Array<std::int64_t>{}, computoc::find(iarr, imask1)));

    const int imask_data2[]{
        1, 1,
        1, 1,
        1, 1 };
    computoc::Array imask2{ {dims, 3}, imask_data2 };
    const std::int64_t rdata2[]{ 0, 1, 2, 3, 4, 5 };
    computoc::Array rarr2{ {6}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, computoc::find(iarr, imask2)));

    EXPECT_TRUE(computoc::all_equal(computoc::Array<std::int64_t>{}, computoc::find(computoc::Array<std::int64_t>{}, imask0)));

    // Get subarray, find values indices by predicate,
    // and use this indices in different array.
    {
        computoc::Array sarr{ iarr({{1, 2}, {0}, {0, 1}}) };
        computoc::Array not_zeros_inds{ computoc::find(sarr, computoc::Array{{2, 1, 2}, {
            0, 1,
            0, 1}}) };

        computoc::Array<std::int64_t> rinds1{ {2}, {3, 5} };
        EXPECT_TRUE(computoc::all_equal(rinds1, not_zeros_inds));

        computoc::Array<std::int64_t> rvals1{ {2}, {13, 15} };

        computoc::Array rallvals1{ {3, 1, 2}, {
            10, 11,
            12, 13,
            14, 15 } };
        EXPECT_TRUE(computoc::all_equal(rvals1, rallvals1(not_zeros_inds)));
    }
}

TEST(Array_test, transpose)
{
    const std::int64_t idims[]{ 4, 2, 3, 2 };
    const int idata[]{
        1,  2,
        3,  4,
        5,  6,

        7,  8,
        9, 10,
        11, 12,


        13, 14,
        15, 16,
        17, 18,

        19, 20,
        21, 22,
        23, 24,


        25, 26,
        27, 28,
        29, 30,

        31, 32,
        33, 34,
        35, 36,


        37, 38,
        39, 40,
        41, 42,

        43, 44,
        45, 46,
        47, 48 };
    computoc::Array iarr{ {idims, 4}, idata };

    const std::int64_t rdims[]{ 3, 4, 2, 2 };
    const double rdata[]{
        1.0,  2.0,
        7.0,  8.0,

        13.0, 14.0,
        19.0, 20.0,

        25.0, 26.0,
        31.0, 32.0,

        37.0, 38.0,
        43.0, 44.0,


        3.0,  4.0,
        9.0, 10.0,

        15.0, 16.0,
        21.0, 22.0,

        27.0, 28.0,
        33.0, 34.0,

        39.0, 40.0,
        45.0, 46.0,


        5.0,  6.0,
        11.0, 12.0,

        17.0, 18.0,
        23.0, 24.0,

        29.0, 30.0,
        35.0, 36.0,

        41.0, 42.0,
        47.0, 48.0 };
    computoc::Array rarr{ {rdims, 4}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, computoc::transpose(iarr, { 2, 0, 1, 3 })));

    EXPECT_TRUE(computoc::all_equal(rarr, computoc::transpose(iarr, { 2, 0, 1, 3, 2 })));
    EXPECT_TRUE(computoc::empty(computoc::transpose(iarr, { 2, 0, 1, 4 })));
}

TEST(Array_test, equal)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        true, true,
        true, false,
        true, false };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };
    
    EXPECT_TRUE(computoc::all_equal(rarr, arr1 == arr2));
    EXPECT_TRUE(computoc::empty(arr1 == Integer_array{ {1} }));
}

TEST(Array_test, not_equal)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        false, false,
        false, true,
        false, true };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, arr1 != arr2));
    EXPECT_TRUE(computoc::empty(arr1 != Integer_array{ {1} }));
}

TEST(Array_test, greater)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        false, false,
        false, false,
        false, false };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, arr1 > arr2));
    EXPECT_TRUE(computoc::all_equal(rarr, arr1 > 6));
    EXPECT_TRUE(computoc::all_equal(rarr, 0 > arr1));
    EXPECT_TRUE(computoc::empty(arr1 > Integer_array{ {1} }));
}

TEST(Array_test, greater_equal)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        true, true,
        true, false,
        true, false };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, arr1 >= arr2));
    EXPECT_TRUE(computoc::all_equal(rarr, arr1 >= 1));

    const bool rdata2[] = {
        true, true,
        true, true,
        true, false };
    computoc::Array<bool> rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, 5 >= arr2));

    EXPECT_TRUE(computoc::empty(arr1 >= Integer_array{ {1} }));
}

TEST(Array_test, less)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        false, false,
        false, true,
        false, true };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, arr1 < arr2));
    EXPECT_TRUE(computoc::all_equal(rarr, arr1 < 1));

    const bool rdata2[] = {
        false, true,
        true, true,
        true, true };
    computoc::Array<bool> rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, 1 < arr2));

    EXPECT_TRUE(computoc::empty(arr1 < Integer_array{ {1} }));
}

TEST(Array_test, less_equal)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        true, true,
        true, true,
        true, true };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, arr1 <= arr2));
    EXPECT_TRUE(computoc::all_equal(rarr, arr1 <= 5));
    EXPECT_TRUE(computoc::all_equal(rarr, 0 <= arr1));
    EXPECT_TRUE(computoc::empty(arr1 <= Integer_array{ {1} }));
}

TEST(Array_test, close)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 1,
        1, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const bool rdata[] = {
        true, true,
        true, false,
        true, false };
    computoc::Array<bool> rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, computoc::close(arr1, arr2, 2)));
    EXPECT_TRUE(computoc::all_equal(rarr, computoc::close(arr1, 3, 2)));
    EXPECT_TRUE(computoc::all_equal(rarr, computoc::close(3, arr1, 2)));
    EXPECT_TRUE(computoc::empty(computoc::close(arr1, Integer_array{ {1} })));
}

TEST(Array_test, plus)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        2, 4,
        6, 4,
        10, 6 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 + arr2));
    arr1 += arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 + Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 += Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        11, 12,
        13, 14,
        15, 16 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 + 10));
    EXPECT_TRUE(computoc::all_equal(rarr2, 10 + arr2));
    arr2 += 10;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, minus)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 0,
        0, -4,
        0, -6 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 - arr2));
    arr1 -= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 - Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 -= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 - 1));

    const int rdata3[] = {
        0, -1,
        -2, -3,
        -4, -5 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_TRUE(computoc::all_equal(rarr3, 1 - arr2));
    arr2 -= 1;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, multiply)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        1, 4,
        9, 0,
        25, 0 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 * arr2));
    arr1 *= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 * Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 *= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        10, 20,
        30, 40,
        50, 60 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 * 10));
    EXPECT_TRUE(computoc::all_equal(rarr2, 10 * arr2));
    arr2 *= 10;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, divide)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        1, 1,
        1, 0,
        1, 0 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 / arr2));
    arr1 /= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 / Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 /= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0, 1,
        1, 2,
        2, 3 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 / 2));

    const int rdata3[] = {
        2, 1,
        0, 0,
        0, 0 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_TRUE(computoc::all_equal(rarr3, 2 / arr2));
    arr2 /= 2;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, modulu)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 0,
        5, 0 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 0,
        0, 0,
        0, 0 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 % arr2));
    arr1 %= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 % Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 %= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        1, 0,
        1, 0,
        1, 0 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 % 2));

    const int rdata3[] = {
        0, 0,
        2, 2,
        2, 2 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_TRUE(computoc::all_equal(rarr3, 2 % arr2));
    arr2 %= 2;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, xor)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0b000, 0b001,
        0b010, 0b011,
        0b100, 0b101 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0b000, 0b000,
        0b000, 0b011,
        0b000, 0b101 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 ^ arr2));
    arr1 ^= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 ^ Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 ^= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0b111, 0b110,
        0b101, 0b111,
        0b011, 0b111 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 ^ 0b111));
    EXPECT_TRUE(computoc::all_equal(rarr2, 0b111 ^ arr2));
    arr2 ^= 0b111;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, and)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0b000, 0b001,
        0b010, 0b011,
        0b100, 0b101 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 & arr2));
    arr1 &= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 & Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 &= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 & 0b111));
    EXPECT_TRUE(computoc::all_equal(rarr2, 0b111 & arr2));
    arr2 &= 0b111;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, or)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0b000, 0b001,
        0b010, 0b011,
        0b100, 0b101 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0b000, 0b001,
        0b010, 0b011,
        0b100, 0b101 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 | arr2));
    arr1 |= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 | Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 |= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0b111, 0b111,
        0b111, 0b111,
        0b111, 0b111 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 | 0b111));
    EXPECT_TRUE(computoc::all_equal(rarr2, 0b111 | arr2));
    arr2 |= 0b111;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, shift_left)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 2,
        8, 24,
        64, 160 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 << arr2));
    arr1 <<= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 << Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 <<= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0, 4,
        8, 12,
        16, 20 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 << 2));

    const int rdata3[] = {
        2, 4,
        8, 16,
        32, 64 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_TRUE(computoc::all_equal(rarr3, 2 << arr2));
    arr2 <<= 2;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, shift_right)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 0,
        0, 0,
        0, 0 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 >> arr2));
    arr1 >>= arr2;
    EXPECT_TRUE(computoc::all_equal(rarr1, arr1));

    EXPECT_TRUE(computoc::empty(arr1 >> Integer_array{ {1} }));
    EXPECT_TRUE(computoc::all_equal(arr1 >>= Integer_array{ {1} }, arr1));

    const int rdata2[] = {
        0, 0,
        0, 0,
        1, 1 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 >> 2));

    const int rdata3[] = {
        2, 1,
        0, 0,
        0, 0 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_TRUE(computoc::all_equal(rarr3, 2 >> arr2));
    arr2 >>= 2;
    EXPECT_TRUE(computoc::all_equal(rarr2, arr2));
}

TEST(Array_test, bitwise_not)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    const int rdata[] = {
        -1, -2,
        -3, -4,
        -5, -6 };
    Integer_array rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, ~arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, ~Integer_array{}));
}

TEST(Array_test, logic_not)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    const int rdata[] = {
        1, 0,
        0, 0,
        0, 0 };
    Integer_array rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, !arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, !Integer_array{}));
}

TEST(Array_test, positive)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    EXPECT_TRUE(computoc::all_equal(arr, +arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, +Integer_array{}));
}

TEST(Array_test, negation)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    const int rdata[] = {
        0, -1,
        -2, -3,
        -4, -5 };
    Integer_array rarr{ {3, 1, 2}, rdata };

    EXPECT_TRUE(computoc::all_equal(rarr, -arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, -Integer_array{}));
}

TEST(Array_test, logic_and)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0, 1,
        2, 0,
        3, 0 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 1,
        1, 0,
        1, 0 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 && arr2));

    EXPECT_TRUE(computoc::empty(arr1 && Integer_array{ {1} }));

    const int rdata2[] = {
        0, 1,
        1, 0,
        1, 0 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 && 1));
    EXPECT_TRUE(computoc::all_equal(rarr2, 1 && arr2));
}

TEST(Array_test, logic_or)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr1{ { 3, 1, 2 }, data1 };

    const int data2[] = {
        0, 1,
        2, 0,
        3, 0 };
    Integer_array arr2{ { 3, 1, 2 }, data2 };

    const int rdata1[] = {
        0, 1,
        1, 1,
        1, 1 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    EXPECT_TRUE(computoc::all_equal(rarr1, arr1 || arr2));

    EXPECT_TRUE(computoc::empty(arr1 || Integer_array{ {1} }));

    const int rdata2[] = {
        1, 1,
        1, 1,
        1, 1 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_TRUE(computoc::all_equal(rarr2, arr2 || 1));
    EXPECT_TRUE(computoc::all_equal(rarr2, 1 || arr2));
}

TEST(Array_test, increment)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    const int rdata1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    const int rdata2[] = {
        1, 2,
        3, 4,
        5, 6 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    Integer_array old{ arr++ };

    EXPECT_TRUE(computoc::all_equal(rarr1, old));
    EXPECT_TRUE(computoc::all_equal(rarr2, arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, ++Integer_array{}));
    EXPECT_TRUE(computoc::all_equal(arr, ++(Integer_array{ { 3, 1, 2 }, data })));
    EXPECT_TRUE(computoc::all_equal((Integer_array{ { 3, 1, 2 }, data }), (Integer_array{ { 3, 1, 2 }, data })++));
}

TEST(Array_test, decrement)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array arr{ { 3, 1, 2 }, data };

    const int rdata1[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array rarr1{ {3, 1, 2}, rdata1 };

    const int rdata2[] = {
        -1, 0,
        1, 2,
        3, 4 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    Integer_array old{ arr-- };

    EXPECT_TRUE(computoc::all_equal(rarr1, old));
    EXPECT_TRUE(computoc::all_equal(rarr2, arr));
    EXPECT_TRUE(computoc::all_equal(Integer_array{}, --Integer_array{}));
    EXPECT_TRUE(computoc::all_equal(arr, --(Integer_array{ { 3, 1, 2 }, data })));
    EXPECT_TRUE(computoc::all_equal((Integer_array{ { 3, 1, 2 }, data }), (Integer_array{ { 3, 1, 2 }, data })--));
}

TEST(Array_test, can_be_matched_with_another_array_or_value)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::int64_t dims1[]{ 3, 1, 2 };
    Integer_array arr1{ {dims1, 3}, data1 };
    Integer_array arr2{ {dims1, 3}, data1 };

    EXPECT_TRUE(computoc::all_match(arr1, arr2, [](int a, int b) { return a / b == 1; }));

    std::int64_t dims2[]{ 3, 2 };
    Integer_array arr3{ {dims2, 2}, data1 };

    EXPECT_FALSE(computoc::all_match(arr1, arr3, [](int, int) {return true; }));

    const int data2[] = {
        1, 2,
        3, 4,
        5, 5 };
    Integer_array arr4{ {dims1, 3}, data2 };
    Integer_array arr5{ {dims2, 2}, data2 };

    EXPECT_FALSE(computoc::all_match(arr1, arr4, [](int a, int b) {return a == b; }));
    EXPECT_FALSE(computoc::all_match(arr1, arr4, [](int a, int b) {return a == b; }));

    {
        using Integer_array = computoc::Array<int>;

        const int data[] = {
            1, 2, 3,
            4, 5, 6,
            7, 8, 9,

            10, 11, 12,
            13, 14, 15,
            16, 17, 18,

            19, 20, 21,
            22, 23, 24,
            25, 26, 27,

            28, 29, 30,
            31, 32, 33,
            34, 35, 36 };
        const std::int64_t dims[]{ 2, 2, 3, 3 };
        Integer_array arr{ {dims, 4}, data };

        const int rdata[] = {
            11,
            14,

            29,
            32
        };
        const std::int64_t rdims[]{ 2, 1, 2, 1 };
        Integer_array rarr{ {rdims, 4}, rdata };

        Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };
        EXPECT_TRUE(computoc::all_equal(rarr, sarr));
        EXPECT_TRUE(computoc::all_match(rarr, sarr, [](int a, int b) { return a == b; }));
    }

    // different ND array types
    {
        const double data1d[] = {
            1.0, 2.0,
            3.0, 4.0,
            5.0, 6.0 };
        std::int64_t dims1d[]{ 3, 1, 2 };
        computoc::Array<double> arr1d{ {dims1d, 3}, data1d };

        EXPECT_TRUE(computoc::all_equal(arr1, arr1d));
        EXPECT_TRUE(computoc::all_match(arr1, arr1d, [](int a, int b) { return a == b; }));

        arr1d({ 0, 0, 0 }) = 1.001;
        EXPECT_FALSE(computoc::all_equal(arr1, arr1d));
        EXPECT_TRUE(computoc::all_match(arr1, arr1d, [](int a, int b) { return a == b; }));
    }
    
    // empty arrays
    {
        EXPECT_TRUE(computoc::all_match(Integer_array{}, Integer_array{}, [](int, int) { return false; }));
        EXPECT_TRUE(computoc::all_match(Integer_array{}, Integer_array({}), [](int, int) { return false; }));
        EXPECT_TRUE(computoc::all_match(Integer_array{}, Integer_array({}, 0), [](int, int) { return false; }));
    }

    // scalar
    {
        EXPECT_TRUE(computoc::all_match(arr1, 1, [](int a, int b) { return a * b == a; }));
        EXPECT_FALSE(computoc::all_match(2, arr2, [](int a, int b) { return a * b == a; }));
    }
}

TEST(Array_test, can_be_compared_with_another_array_or_value)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::int64_t dims1[]{ 3, 1, 2 };
    Integer_array arr1{ {dims1, 3}, data1 };
    Integer_array arr2{ {dims1, 3}, data1 };

    EXPECT_TRUE(computoc::all_equal(arr1, arr2));

    std::int64_t dims2[]{ 3, 2 };
    Integer_array arr3{ {dims2, 2}, data1 };

    EXPECT_FALSE(computoc::all_equal(arr1, arr3));

    const int data2[] = {
        1, 2,
        3, 4,
        5, 5 };
    Integer_array arr4{ {dims1, 3}, data2 };
    Integer_array arr5{ {dims2, 2}, data2 };

    EXPECT_FALSE(computoc::all_equal(arr1, arr4));
    EXPECT_FALSE(computoc::all_equal(arr1, arr5));

    {
        using Integer_array = computoc::Array<int>;

        const int data[] = {
            1, 2, 3,
            4, 5, 6,
            7, 8, 9,

            10, 11, 12,
            13, 14, 15,
            16, 17, 18,

            19, 20, 21,
            22, 23, 24,
            25, 26, 27,

            28, 29, 30,
            31, 32, 33,
            34, 35, 36 };
        const std::int64_t dims[]{ 2, 2, 3, 3 };
        Integer_array arr{ {dims, 4}, data };

        const int rdata[] = {
            11,
            14,

            29,
            32
        };
        const std::int64_t rdims[]{ 2, 1, 2, 1 };
        Integer_array rarr{ {rdims, 4}, rdata };

        Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };
        EXPECT_TRUE(computoc::all_equal(rarr, sarr));
    }

    // different ND array types
    {
        const double data1d[] = {
            1.0, 2.0,
            3.0, 4.0,
            5.0, 6.0 };
        std::int64_t dims1d[]{ 3, 1, 2 };
        computoc::Array<double> arr1d{ {dims1d, 3}, data1d };

        EXPECT_TRUE(computoc::all_equal(arr1, arr1d));
        
        arr1d({ 0, 0, 0 }) = 1.001;
        EXPECT_FALSE(computoc::all_equal(arr1, arr1d));
    }

    // empty arrays
    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, Integer_array({}, 0)));
    }

    // scalar
    {
        EXPECT_FALSE(computoc::all_equal(arr1, 1));
        EXPECT_FALSE(computoc::all_equal(2, arr2));
    }
}

TEST(Array_test, can_be_compared_by_tolerance_values_with_another_array_or_value)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::int64_t dims1[]{ 3, 1, 2 };
    Integer_array arr1{ {dims1, 3}, data1 };
    Integer_array arr2{ {dims1, 3}, data1 };

    EXPECT_TRUE(computoc::all_close(arr1, arr2));

    std::int64_t dims2[]{ 3, 2 };
    Integer_array arr3{ {dims2, 2}, data1 };

    EXPECT_FALSE(computoc::all_close(arr1, arr3, 1));

    const int data2[] = {
        1, 2,
        3, 4,
        5, 5 };
    Integer_array arr4{ {dims1, 3}, data2 };
    Integer_array arr5{ {dims2, 2}, data2 };

    EXPECT_TRUE(computoc::all_close(arr1, arr4, 1));
    EXPECT_FALSE(computoc::all_close(arr1, arr5, 1));

    {
        using Integer_array = computoc::Array<int>;

        const int data[] = {
            1, 2, 3,
            4, 5, 6,
            7, 8, 9,

            10, 11, 12,
            13, 14, 15,
            16, 17, 18,

            19, 20, 21,
            22, 23, 24,
            25, 26, 27,

            28, 29, 30,
            31, 32, 33,
            34, 35, 36 };
        const std::int64_t dims[]{ 2, 2, 3, 3 };
        Integer_array arr{ {dims, 4}, data };

        const int rdata[] = {
            10,
            14,

            29,
            32
        };
        const std::int64_t rdims[]{ 2, 1, 2, 1 };
        Integer_array rarr{ {rdims, 4}, rdata };

        Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };
        EXPECT_TRUE(computoc::all_close(rarr, sarr, 1));
    }

    // different ND array types
    {
        const double data1d[] = {
            1.0, 2.0,
            3.0, 4.0,
            5.0, 6.0 };
        std::int64_t dims1d[]{ 3, 1, 2 };
        computoc::Array<double> arr1d{ {dims1d, 3}, data1d };

        EXPECT_TRUE(computoc::all_close(arr1, arr1d));

        arr1d({ 0, 0, 0 }) = 1.001;
        EXPECT_FALSE(computoc::all_close(arr1, arr1d));
    }

    // empty arrays
    {
        EXPECT_TRUE(computoc::all_close(Integer_array{}, Integer_array{}));
        EXPECT_TRUE(computoc::all_close(Integer_array{}, Integer_array({})));
        EXPECT_TRUE(computoc::all_close(Integer_array{}, Integer_array({}, 0)));
    }

    // scalar
    {
        EXPECT_TRUE(computoc::all_close(arr1, 1, 5));
        EXPECT_FALSE(computoc::all_close(1, arr2));
    }
}

TEST(Array_test, can_return_slice)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6};
    const std::int64_t dims[] = { 3, 1, 2 };
    Integer_array arr{ {dims, 3}, data };

    // empty ranges
    {
        std::initializer_list<computoc::Interval<std::int64_t>> ranges{};
        Integer_array rarr{ arr(ranges) };
        EXPECT_TRUE(computoc::all_equal(arr, rarr));
        EXPECT_EQ(arr.data(), rarr.data());
    }

    // illegal ranges
    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, arr({ {0, 0, 0} })));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, arr({ {2, 1, 1} })));
    }

    // empty array
    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, Integer_array{}(std::initializer_list<computoc::Interval<std::int64_t>>{})));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, Integer_array{}({ {0,1}, {0,4,2} })));
    }

    // ranges in dims
    {
        // nranges == ndims
        const int tdata1[] = {
            1,
            5 };
        const std::int64_t tdims1[] = { 2, 1, 1 };
        Integer_array tarr1{ {tdims1, 3}, tdata1 };
        Integer_array sarr1{ arr({{0, 2,2}, {0}, {0}}) };
        EXPECT_TRUE(computoc::all_equal(tarr1, sarr1));
        EXPECT_EQ(arr.data(), sarr1.data());

        // nranges < ndims
        const int tdata2[] = {
            3, 4 };
        const std::int64_t tdims2[] = { 1, 1, 2 };
        Integer_array tarr2{ {tdims2, 3}, tdata2 };
        Integer_array sarr2{ arr({{1, 2, 2}}) };
        EXPECT_TRUE(computoc::all_equal(tarr2, sarr2));
        EXPECT_EQ(arr.data(), sarr2.data());

        // nranges > ndims - ignore extra ranges
        Integer_array sarr3{ arr({{0, 2, 2}, {0}, {0}, {100, 100, 5}}) };
        EXPECT_TRUE(computoc::all_equal(sarr1, sarr3));
        EXPECT_EQ(arr.data(), sarr3.data());

        // out of range and negative indices
        Integer_array sarr4{ arr({{-1, 3, -2}, {1}, {-2}}) };
        EXPECT_TRUE(computoc::all_equal(sarr1, sarr4));
        EXPECT_EQ(arr.data(), sarr4.data());
    }
}

TEST(Array_test, can_be_assigned_with_value)
{
    using Integer_array = computoc::Array<int>;

    // empty array
    {
        Integer_array arr{};
        arr = 100;
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, arr));
    }

    // normal array
    {
        const int data[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array arr{ {dims, 3}, data };

        const int tdata[] = {
            100, 100,
            100, 100,
            100, 100 };
        Integer_array tarr{ {dims, 3}, tdata };

        arr = 100;
        EXPECT_TRUE(computoc::all_equal(tarr, arr));
    }

    // subarray
    {
        const int data[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array arr{ {dims, 3}, data };

        const int tdata[] = {
            1, 50,
            3, 100,
            5, 100 };
        Integer_array tarr{ {dims, 3}, tdata };

        arr({ {1,2}, {0}, {1} }) = 100;
        // assignment of different type
        arr({ {0,0}, {0}, {1} }) = 50.5;
        EXPECT_TRUE(computoc::all_equal(tarr, arr));
    }
}

TEST(Array_test, copy_by_reference)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array arr{ {dims, 3}, data };

    Integer_array carr1{ arr };
    carr1({ 2, 0, 0 }) = 0;
    const int rdata1[] = {
        1, 2,
        3, 4,
        0, 6 };
    Integer_array rarr1{ {dims, 3}, rdata1 };
    EXPECT_TRUE(computoc::all_equal(rarr1, carr1));

    Integer_array carr2{};
    carr2 = carr1;
    carr1({ 0, 0, 0 }) = 0;
    const int rdata2[] = {
        0, 2,
        3, 4,
        0, 6 };
    Integer_array rarr2{ {dims, 3}, rdata2 };
    EXPECT_TRUE(computoc::all_equal(rarr2, carr2));

    carr2({ {0, 1}, {0, 0}, {0, 1} }) = carr1;
    EXPECT_TRUE(computoc::all_equal(rarr2, carr2));

    // slice copying by assignment (rvalue)
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        Integer_array starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 5, 2} }) = starr3;
        EXPECT_TRUE(computoc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue)
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        Integer_array starr3{ tarr3({ {0, 5, 2} }) };
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        rarr3 = starr3;
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
    }

    // different template arguments
    {
        const int idata[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array iarr{ {dims, 3}, idata };

        const double ddata[] = {
            1.1, 2.1,
            3.1, 4.1,
            5.1, 6.1 };
        computoc::Array<double> darr{ {dims, 3}, ddata };

        Integer_array cdarr1{ darr };
        EXPECT_TRUE(computoc::all_equal(iarr, cdarr1));

        Integer_array cdarr2{};
        cdarr2 = darr;
        EXPECT_TRUE(computoc::all_equal(iarr, cdarr2));
    }

    // slice copying by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 5, 2} }) = starr3;
        EXPECT_TRUE(computoc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 3, 2} }) = starr3;
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
    }

    // slice copying by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        rarr3 = starr3;
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
    }
}

TEST(Array_test, move_by_reference)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array sarr{ {dims, 3}, data };

    Integer_array arr{ {dims, 3}, data };
    Integer_array carr1{ std::move(arr) };
    EXPECT_TRUE(computoc::all_equal(sarr, carr1));
    EXPECT_TRUE(empty(arr));

    Integer_array carr2{};
    carr2 = std::move(carr1);
    EXPECT_TRUE(computoc::all_equal(sarr, carr2));
    EXPECT_TRUE(empty(carr1));

    Integer_array sarr2{ {dims, 3}, data };
    carr2({ {0, 1}, {0, 0}, {0, 1} }) = std::move(sarr2);
    EXPECT_TRUE(empty(sarr2));
    EXPECT_TRUE(computoc::all_equal(sarr, carr2));

    // slice moving by assignment
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        rarr3({ {0, 5, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_TRUE(computoc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        srarr3 = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // different template arguments
    {
        const int idata[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array iarr{ {dims, 3}, idata };

        const double ddata[] = {
            1.1, 2.1,
            3.1, 4.1,
            5.1, 6.1 };
        computoc::Array<double> darr{ {dims, 3}, ddata };

        Integer_array cdarr1{ std::move(darr) };
        EXPECT_TRUE(computoc::all_equal(iarr, cdarr1));
        EXPECT_TRUE(empty(darr));

        computoc::Array<double> cdarr2{};
        cdarr2 = std::move(cdarr1);
        EXPECT_TRUE(computoc::all_equal((Integer_array{ {dims, 3}, idata }), cdarr2));
        EXPECT_TRUE(empty(cdarr1));
    }

    // slice moving by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const double rdata3[] = { 0, 2, 0, 4, 0, 6 };
        computoc::Array<double> rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        rarr3({ {0, 5, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_TRUE(computoc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const double rdata3[] = { 0, 2, 0, 4, 0, 6 };
        computoc::Array<double> rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        rarr3({ {0, 3, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        computoc::Array<double> srarr3{ rarr3({ {0, 5, 2} }) };
        srarr3 = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_FALSE(computoc::all_equal(tarr3, rarr3));
        EXPECT_FALSE(empty(tarr3));
    }
}

TEST(Array_test, clone)
{
    using Integer_array = computoc::Array<int>;

    Integer_array empty_arr{};
    Integer_array cempty_arr{ computoc::clone(empty_arr) };
    EXPECT_TRUE(computoc::all_equal(empty_arr, cempty_arr));

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array sarr{ {dims, 3}, data };

    Integer_array carr{ computoc::clone(sarr) };
    EXPECT_TRUE(computoc::all_equal(carr, sarr));
    carr({ 0, 0, 0 }) = 0;
    EXPECT_FALSE(computoc::all_equal(carr, sarr));

    Integer_array csubarr{ computoc::clone(sarr({{1, 1}, {0, 0}, {0, 0}})) };
    EXPECT_TRUE(computoc::all_equal(sarr({ {1, 1}, {0, 0}, {0, 0} }), csubarr));
    csubarr({ 0, 0, 0 }) = 5;
    EXPECT_FALSE(computoc::all_equal(sarr({ {1, 1}, {0, 0}, {0, 0} }), csubarr));
}

TEST(Array_test, copy)
{
    using Integer_array = computoc::Array<int>;

    { // backward cases - copy from other array to created array
        Integer_array empty_arr{};
        Integer_array cempty_arr{};
        computoc::copy(Integer_array{}, cempty_arr);
        EXPECT_TRUE(computoc::all_equal(empty_arr, cempty_arr));

        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ {3, 1, 2}, data1 };

        const int data2[] = {
            2, 4,
            6, 8,
            10, 12 };
        Integer_array arr2{ {3, 1, 2}, data2 };
        EXPECT_FALSE(computoc::all_equal(arr1, arr2));
        computoc::copy(arr2, arr1);
        EXPECT_TRUE(computoc::all_equal(arr1, arr2));

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        Integer_array arr3{ {3, 1, 2}, data3 };
        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_TRUE(computoc::all_equal(arr3, arr2));

        computoc::copy(arr2, arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_TRUE(computoc::all_equal(arr3, arr2));
    }

    { // forward cases - copy from created array to other array
        Integer_array empty_arr{};
        Integer_array cempty_arr{};
        computoc::copy(cempty_arr, empty_arr);
        EXPECT_TRUE(computoc::all_equal(empty_arr, cempty_arr));

        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ {3, 1, 2}, data1 };

        const int data2[] = {
            2, 4,
            6, 8,
            10, 12 };
        Integer_array arr2{ {3, 1, 2}, data2 };
        EXPECT_FALSE(computoc::all_equal(arr1, arr2));
        computoc::copy(arr2, arr1);
        EXPECT_TRUE(computoc::all_equal(arr1, arr2));

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        Integer_array arr3{ {3, 1, 2}, data3 };
        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_TRUE(computoc::all_equal(arr3, arr2));

        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 1}, {0, 0}, {0, 1} }));
        EXPECT_TRUE(computoc::all_equal(arr3, arr2));

        computoc::copy(arr3({ {0, 0}, {0, 0}, {0, 1} }), arr2);
        EXPECT_TRUE(computoc::all_equal(arr3({ {0, 0}, {0, 0}, {0, 1} }), arr2({ {0, 0}, {0, 0}, {0, 1} })));
    }

    // copy to different type ND array
    {
        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ {3, 1, 2}, data1 };

        const double data2d[] = {
            2.1, 4.1,
            6.1, 8.1,
            10.1, 12.1 };
        computoc::Array<double> arr2d{ {3, 1, 2}, data2d };
        EXPECT_FALSE(computoc::all_equal(arr1, arr2d));
        computoc::copy(arr2d, arr1);
        EXPECT_FALSE(computoc::all_equal(arr1, arr2d));
    }

    // test is incorrect. copy should copy elements not according to subscripts.
    //// copy to different dimensions and same count
    //{
    //    const int data[] = { 1, 2, 3, 4, 5, 6 };
    //    Integer_array arr{ {6}, data };

    //    Integer_array tarr{ {3, 1, 2}, data };

    //    Integer_array rarr{ {3, 1, 2}, 0 };
    //    EXPECT_FALSE(computoc::all_equal(tarr, rarr));
    //    computoc::copy(arr, rarr);
    //    EXPECT_TRUE(computoc::all_equal(
    //        Integer_array{ {3, 1, 2}, {
    //            5, 6,
    //            0, 0,
    //            0, 0 } }, rarr));
    //}
}

TEST(Array_test, reshape)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array arr{ {dims, 3}, data };

    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::reshape(arr, {})));
    }

    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::reshape(Integer_array{}, {})));
    }

    {
        const int tdata[] = { 1, 2, 3, 4, 5, 6 };
        const std::int64_t tdims[]{ 6 };
        Integer_array tarr{ {tdims, 1}, tdata };

        Integer_array rarr{ computoc::reshape(arr, { 6 }) };
        EXPECT_TRUE(computoc::all_equal(tarr, rarr));
        EXPECT_EQ(arr.data(), rarr.data());
    }

    {
        Integer_array rarr{ computoc::reshape(arr, { 3, 1, 2 }) };
        EXPECT_TRUE(computoc::all_equal(arr, rarr));
        EXPECT_EQ(arr.data(), rarr.data());
    }

    {
        const int tdata[] = { 1, 5 };
        const std::int64_t tdims[]{ 1, 2 };
        Integer_array tarr{ {tdims, 2}, tdata };

        Integer_array rarr{ computoc::reshape(arr({{0, 2, 2}, {}, {}}), {1, 2}) };
        EXPECT_TRUE(computoc::all_equal(tarr, rarr));
        EXPECT_NE(arr.data(), rarr.data());
    }
}

TEST(Array_test, resize)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = { 1, 2, 3, 4, 5, 6 };
    const std::int64_t dims[]{ 6 };
    Integer_array arr{ {dims, 1}, data };

    {
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::resize(arr, {})));
    }

    {
        Integer_array rarr{ computoc::resize(Integer_array{}, {6}) };
        //EXPECT_FALSE(computoc::all_equal(arr, rarr));
        EXPECT_EQ(arr.header().dims().size(), rarr.header().dims().size());
        EXPECT_EQ(6, rarr.header().dims().data()[0]);
        EXPECT_NE(arr.block().data(), rarr.block().data());
    }

    {
        Integer_array rarr{ computoc::resize(arr, {6}) };
        EXPECT_TRUE(computoc::all_equal(arr, rarr));
        EXPECT_NE(arr.block().data(), rarr.block().data());
    }

    {
        const int tdata[] = { 1, 2 };
        const std::int64_t tdims[]{ 2 };
        Integer_array tarr{ {tdims, 1}, tdata };

        Integer_array rarr{ computoc::resize(arr, {2}) };
        EXPECT_TRUE(computoc::all_equal(tarr, rarr));
        EXPECT_NE(tarr.block().data(), rarr.block().data());
    }

    {
        const int tdata[] = { 
            1, 2,
            3, 4,
            5, 6};
        const std::int64_t tdims[]{ 3, 1, 2 };
        Integer_array tarr{ {tdims, 3}, tdata };

        Integer_array rarr{ computoc::resize(arr, {3, 1, 2}) };
        EXPECT_TRUE(computoc::all_equal(tarr, rarr));
        EXPECT_NE(tarr.block().data(), rarr.block().data());
    }

    {
        Integer_array rarr{ computoc::resize(arr, {10}) };
        EXPECT_FALSE(computoc::all_equal(arr, rarr));
        EXPECT_TRUE(computoc::all_equal(arr, rarr({ {0, 5} })));
        EXPECT_NE(arr.block().data(), rarr.block().data());
    }
}

TEST(Array_test, append)
{
    using Integer_array = computoc::Array<int>;
    using Double_array = computoc::Array<double>;

    // No axis specified
    {
        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ { 3, 1, 2 }, data1 };

        const double data2[] = { 7.0, 8.0, 9.0, 10.0, 11.0 };
        Double_array arr2{ {5}, data2 };

        const int rdata1[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
        Integer_array rarr{ {11}, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr, computoc::append(arr1, arr2)));

        EXPECT_TRUE(computoc::all_equal(arr1, computoc::append(arr1, Integer_array{})));
        EXPECT_TRUE(computoc::all_equal(arr2, computoc::append(Integer_array{}, arr2)));
    }

    // Axis specified
    {
        const int data1[] = {
            1, 2, 3,
            4, 5, 6,

            7, 8, 9,
            10, 11, 12 };
        Integer_array arr1{ { 2, 2, 3 }, data1 };

        const double data2[] = {
            13.0, 14.0, 15.0,
            16.0, 17.0, 18.0,

            19.0, 20.0, 21.0,
            22.0, 23.0, 24.0 };
        Double_array arr2{ { 2, 2, 3 }, data2 };

        const int rdata1[] = {
            1,  2,  3,
            4,  5,  6,

            7,  8,  9,
            10, 11, 12,

            13, 14, 15,
            16, 17, 18,

            19, 20, 21,
            22, 23, 24 };
        Integer_array rarr1{ { 4, 2, 3 }, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::append(arr1, arr2, 0)));

        const int rdata2[] = {
            1,  2,  3,
            4,  5,  6,
            13, 14, 15,
            16, 17, 18,

            7,  8,  9,
            10, 11, 12,
            19, 20, 21,
            22, 23, 24 };
        Integer_array rarr2{ { 2, 4, 3 }, rdata2 };
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::append(arr1, arr2, 1)));

        const int rdata3[] = {
            1,  2,  3, 13, 14, 15,
            4,  5,  6, 16, 17, 18,
            
            7,  8,  9, 19, 20, 21,
            10, 11, 12, 22, 23, 24 };
            
        Integer_array rarr3{ { 2, 2, 6 }, rdata3 };
        EXPECT_TRUE(computoc::all_equal(rarr3, computoc::append(arr1, arr2, 2)));

        EXPECT_TRUE(computoc::all_equal(arr1, computoc::append(arr1, Integer_array{}, 0)));
        EXPECT_TRUE(computoc::all_equal(arr2, computoc::append(Integer_array{}, arr2, 0)));

        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::append(arr1, arr2, 3)));
        const int invalid_data1[] = { 1 };
        Integer_array invalid_arr1{ {1}, invalid_data1 };
        Integer_array rinvalid_arr1{};

        EXPECT_TRUE(computoc::all_equal(rinvalid_arr1, computoc::append(invalid_arr1, arr2, 3)));
        EXPECT_TRUE(computoc::all_equal(rinvalid_arr1, computoc::append(arr2, invalid_arr1, 3)));

        EXPECT_TRUE(computoc::all_equal(rinvalid_arr1, computoc::append(arr1, rarr2, 0)));
    }
}

TEST(Array_test, insert)
{
    using Integer_array = computoc::Array<int>;
    using Double_array = computoc::Array<double>;

    // No axis specified
    {
        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ { 3, 1, 2 }, data1 };

        const double data2[] = { 7.0, 8.0, 9.0, 10.0, 11.0 };
        Double_array arr2{ {5}, data2 };

        const int rdata1[] = { 1, 2, 3, 7, 8, 9, 10, 11, 4, 5, 6 };
        Integer_array rarr1{ {11}, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::insert(arr1, arr2, 3)));

        const int rdata2[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
        Integer_array rarr2{ {11}, rdata2 };
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::insert(arr1, arr2, 6)));

        const int rdata3[] = { 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6 };
        Integer_array rarr3{ {11}, rdata3 };
        EXPECT_TRUE(computoc::all_equal(rarr3, computoc::insert(arr1, arr2, 7)));

        EXPECT_TRUE(computoc::all_equal(arr1, computoc::insert(arr1, Integer_array{}, 0)));
        EXPECT_TRUE(computoc::all_equal(arr2, computoc::insert(Integer_array{}, arr2, 0)));
    }

    // Axis specified
    {
        const int data1[] = {
            1, 2, 3,
            4, 5, 6,

            7, 8, 9,
            10, 11, 12 };
        Integer_array arr1{ { 2, 2, 3 }, data1 };

        const double data2[] = {
            13.0, 14.0, 15.0,
            16.0, 17.0, 18.0,

            19.0, 20.0, 21.0,
            22.0, 23.0, 24.0 };
        Double_array arr2{ { 2, 2, 3 }, data2 };

        const int rdata1[] = {
            1,  2,  3,
            4,  5,  6,

            13, 14, 15,
            16, 17, 18,

            19, 20, 21,
            22, 23, 24,

            7,  8,  9,
            10, 11, 12 };
        Integer_array rarr1{ { 4, 2, 3 }, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::insert(arr1, arr2, 1, 0)));
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::insert(arr1, arr2, 3, 0)));

        const int rdata2[] = {
            1,  2,  3,
            13, 14, 15,
            16, 17, 18,
            4,  5,  6,


            7,  8,  9,
            19, 20, 21,
            22, 23, 24,
            10, 11, 12 };
        Integer_array rarr2{ { 2, 4, 3 }, rdata2 };
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::insert(arr1, arr2, 1, 1)));
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::insert(arr1, arr2, 3, 1)));

        const int rdata3[] = {
            1, 13, 14, 15,  2,  3,
            4, 16, 17, 18, 5,  6,

            7, 19, 20, 21, 8,  9,
            10, 22, 23, 24, 11, 12 };
        Integer_array rarr3{ { 2, 2, 6 }, rdata3 };
        EXPECT_TRUE(computoc::all_equal(rarr3, computoc::insert(arr1, arr2, 1, 2)));
        EXPECT_TRUE(computoc::all_equal(rarr3, computoc::insert(arr1, arr2, 4, 2)));
        
        EXPECT_TRUE(computoc::all_equal(arr1, computoc::insert(arr1, Integer_array{}, 1, 0)));
        EXPECT_TRUE(computoc::all_equal(arr2, computoc::insert(Integer_array{}, arr2, 1, 0)));

        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::insert(arr1, arr2, 1, 3)));
        const int invalid_data1[] = { 1 };
        Integer_array invalid_arr1{ {1}, invalid_data1 };
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::insert(invalid_arr1, arr2, 1, 0)));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::insert(arr1, rarr2, 1, 0)));;
    }
}

TEST(Array_test, remove)
{
    using Integer_array = computoc::Array<int>;

    // No axis specified
    {
        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        Integer_array arr1{ { 3, 1, 2 }, data1 };

        const int rdata1[] = { 1, 2, 3, 6 };
        Integer_array rarr1{ {4}, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::remove(arr1, 3, 2)));

        const int rdata2[] = { 1, 2, 3 };
        Integer_array rarr2{ {3}, rdata2 };
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::remove(arr1, 3, 4)));

        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::remove(Integer_array{}, 3, 2)));
    }

    // Axis specified
    {
        const int data1[] = {
            1, 2, 3,
            4, 5, 6,

            7, 8, 9,
            10, 11, 12 };
        Integer_array arr1{ { 2, 2, 3 }, data1 };

        const int rdata1[] = {
            7,  8,  9,
            10, 11, 12 };
        Integer_array rarr1{ { 1, 2, 3 }, rdata1 };
        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::remove(arr1, 0, 1, 0)));
        EXPECT_TRUE(computoc::all_equal(Integer_array{}, computoc::remove(arr1, 0, 3, 0)));

        const int rdata2[] = {
            1, 2, 3,

            7, 8, 9 };
        Integer_array rarr2{ { 2, 1, 3 }, rdata2 };
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::remove(arr1, 1, 1, 1)));
        EXPECT_TRUE(computoc::all_equal(rarr2, computoc::remove(arr1, 1, 2, 1)));

        const int rdata3[] = {
            3,
            6,

            9,
            12 };
        Integer_array rarr3{ { 2, 2, 1 }, rdata3 };
        EXPECT_TRUE(computoc::all_equal(rarr3, computoc::remove(arr1, 0, 2, 2)));
        const int rdata4[] = {
            1, 2,
            4, 5,

            7, 8,
            10, 11 };
        Integer_array rarr4{ { 2, 2, 2 }, rdata4 };
        EXPECT_TRUE(computoc::all_equal(rarr4, computoc::remove(arr1, 2, 2, 2)));

        EXPECT_TRUE(computoc::all_equal(rarr1, computoc::remove(arr1, 0, 1, 3)));
    }
}

TEST(Array_test, complex_array)
{
    using Integer_array = computoc::Array<int>;

    const int data[2][2][2][3][3]{
        {{{{1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}},

        {{10, 11, 12},
        {13, 14, 15},
        {16, 17, 18}}},


        {{{19, 20, 21},
        {22, 23, 24},
        {25, 26, 27}},

        {{28, 29, 30},
        {31, 32, 33},
        {34, 35, 36}}}},



        {{{{37, 38, 39},
        {40, 41, 42},
        {43, 44, 45}},

        {{46, 47, 48},
        {49, 50, 51},
        {52, 53, 54}}},


        {{{55, 56, 57},
        {58, 59, 60},
        {61, 62, 63}},

        {{64, 65, 66},
        {67, 68, 69},
        {70, 71, 72}}}}};
    const std::int64_t dims[]{ 2, 2, 2, 3, 3 };
    Integer_array arr{ {dims, 5}, reinterpret_cast<const int*>(data) };

    const int sdata1[1][1][1][2][1]{ { {{{47},{53}}} } };
    const std::int64_t sdims1[]{ 1, 1, 1, 2, 1 };
    Integer_array sarr1{ {sdims1, 5}, reinterpret_cast<const int*>(sdata1) };

    EXPECT_TRUE(computoc::all_equal(sarr1, arr({ {1, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2} })));

    const int sdata2[1][1][1][1][1]{ { {{{53}}} } };
    const std::int64_t sdims2[]{ 1, 1, 1, 1, 1 };
    Integer_array sarr2{ {sdims2, 5}, reinterpret_cast<const int*>(sdata2) };

    EXPECT_TRUE(computoc::all_equal(sarr2, sarr1({ {0, 0}, {0, 0}, {0, 0}, {1, 1}, {0, 0} })));
}



//#include <thread>
//#include <iostream>
//#include <chrono>
//
//TEST(Parallel_test, add)
//{
//    using namespace computoc;
//    using namespace std;
//    using namespace std::chrono;
//    using namespace computoc::details;
//
//    //{
//    //    auto print = [](const auto& cont) {
//    //        for_each(cont.begin(), cont.end(), [](const auto& e) { cout << e << ' '; });
//    //        cout << '\n';
//    //    };
//
//    //    auto arr = Array<int>({ 5, 4, 3, 2, 15 });
//    //    //arr = arr({ {1,4, 2}, {1, 2}, {1,6, 2} });
//    //    //arr = arr({ {2,2} }); // specific first dimension - continuous indexing - [selected_dimension*strides[0], (selected_dimension + 1)*strides[0] - 1]
//    //    arr = arr({ {0,4}, {0,3}, {0,2}, {0,1,2}, {0,14,2} });
//    //    auto hdr = arr.header();
//
//    //    vector<int64_t> inds;
//    //    for (Simple_array_indices_generator gen(hdr); gen; ++gen) {
//    //        inds.push_back(*gen);
//    //    }
//
//    //    cout << "Dr = "; print(hdr.dims());
//    //    cout << "Sr = ";  print(hdr.strides());
//    //    cout << "offset = " << hdr.offset() << '\n';
//    //    cout << "indices:\n"; print(inds);
//    //}
//
//    auto start = high_resolution_clock::now();
//    for (int i = 0; i < 25; ++i) {
//        Array<int> m({ 4, 1920, 1080 }, 5);
//        auto sm = m({ {1,3}, {10,18}, {2,76} });
//        int ctr = 0;
//        for (auto it = sm.begin(); it != sm.end(); ++it) {
//            *it = ctr++;
//        }
//        //for_each(sm.begin(), sm.end(), [](auto& e) { e = 1; });
//        //auto ptr = sm.data();
//        //for (Simple_array_indices_generator gen(sm.header()); gen; ++gen) {
//        //    ptr[*gen] = ctr++;
//        //}
//    }
//    auto stop = high_resolution_clock::now();
//    cout << "avg serial[us] = " << (static_cast<double>(duration_cast<microseconds>(stop - start).count()) / (1000.0 * 1000.0)) / 25 << "\n";
//}
