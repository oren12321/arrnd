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

//#include <chrono>
//#include <thread>
//#include <execution>
//
//void seq_loop(oc::arrnd<double> arr)
//{
//    std::for_each(arr.begin(), arr.end(), [](auto& val) {
//        val = std::rand();
//    });
//}
//void par_loop(oc::arrnd<double> arr) {
//    auto fill = [](auto arr) {
//        std::for_each(arr.begin(), arr.end(), [](auto& val) {
//            val = std::rand();
//        });
//    };
//
//    auto slices_count = std::thread::hardware_concurrency();
//    auto step_size = arr.header().dims()[0] / slices_count;
//
//    std::vector<std::thread> ts(slices_count);
//
//    for (int i = 0; i < slices_count; ++i) {
//        auto slice = arr[{oc::interval<>::between(i * step_size, (i + 1) * step_size)}];
//        ts[i] = std::thread(fill, slice);
//    }
//
//    for (auto& t : ts) {
//        t.join();
//    }
//}
//
//TEST(dummy, benchmark)
//{
//    using namespace std;
//    using namespace std::chrono;
//    using namespace oc;
//
//    int cycles = 25;
//    std::vector<int> dims{960 * 2 * 1080 * 4};
//    arrnd<double> arr(dims, 0.0);
//
//    std::int64_t duration = 0;
//
//    for (int i = 0; i < cycles; ++i) {
//        auto start = high_resolution_clock::now();
//        seq_loop(arr);
//        auto stop = high_resolution_clock::now();
//        duration += duration_cast<milliseconds>(stop - start).count();
//    }
//    std::cout << "seq duration[ms]: " << (duration / static_cast<double>(cycles)) << "\n";
//
//    duration = 0;
//    for (int i = 0; i < cycles; ++i) {
//        auto start = high_resolution_clock::now();
//        par_loop(arr);
//        auto stop = high_resolution_clock::now();
//        duration += duration_cast<milliseconds>(stop - start).count();
//    }
//    std::cout << "par duration[ms]: " << (duration / static_cast<double>(cycles)) << "\n";
//}

//TEST(dummy, ranger)
//{
//    using namespace oc;
//
//    arrnd_header<> hdr({6, 10, 4});
//
//    arrnd_axis_ranger<> rgr(hdr, 1, false, 4, 2, 2, true);
//
//    for (; rgr; ++rgr) {
//        auto ival = (*rgr)[1];
//        std::cout << "{" << ival.start() << ", " << ival.stop() << ", " << ival.step() << "}\n";
//    }
//}


//TEST(arrnd_test, find_diagonal)
//{
//    using namespace oc;
//
//    arrnd<int> arr({4, 5}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20});
//
//    arr(arr.find_diagonal()) = 0;
//    
//    EXPECT_TRUE(
//        all_equal(arr, arrnd<int>({4, 5}, {0, 2, 3, 4, 5, 6, 0, 8, 9, 10, 11, 12, 0, 14, 15, 16, 17, 18, 0, 20})));
//}

//TEST(arrnd_test, spread_diagonal)
//{
//    using namespace oc;
//
//    arrnd<int> arr({5}, {1, 2, 3, 4, 5});
//
//    EXPECT_TRUE(all_equal(arr.spread_diagonal(1),
//        arrnd<int>({6, 6},
//            {0, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0,
//                0})));
//}


TEST(simple_allocator, can_allocate_and_deallocate_memory)
{
    using namespace oc;

    simple_allocator<int> alloc;

    int* p = alloc.allocate(2);

    p[0] = 0;
    p[1] = 1;

    EXPECT_EQ(p[0], 0);
    EXPECT_EQ(p[1], 1);

    alloc.deallocate(p, 2);
}

TEST(simple_allocator, throws_exception_if_allocation_fails)
{
    using namespace oc;

    simple_allocator<int> alloc;

    EXPECT_THROW(int* p = alloc.allocate(std::numeric_limits<std::size_t>::max() / sizeof(int)), std::bad_alloc);
}

TEST(simple_allocator, is_different_from_another_in_case_of_different_size_type)
{
    using namespace oc;

    simple_allocator<int> alloc1;
    simple_allocator<char> alloc2;

    EXPECT_EQ(alloc1, alloc1);
    EXPECT_NE(alloc1, alloc2);
}

TEST(simple_allocator, is_copyable_and_movable)
{
    using namespace oc;

    simple_allocator<int> alloc1;
    EXPECT_EQ(simple_allocator<int>(alloc1), alloc1);

    EXPECT_NE(simple_allocator<char>(alloc1), alloc1);

    simple_allocator<int> alloc1_copy(alloc1);
    simple_allocator<char> other_alloc1_copy(alloc1);

    EXPECT_EQ(simple_allocator<int>(std::move(alloc1)), alloc1_copy);
    EXPECT_NE(simple_allocator<int>(std::move(alloc1_copy)), other_alloc1_copy);
}

TEST(simple_vector, methods)
{
    using namespace oc;

    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};

    simple_vector<std::string> sv(arr.cbegin(), arr.cend());
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

    sv.append(1);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(25, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.append(5);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(50);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(45);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    sv.shrink_to_fit();
    EXPECT_EQ(30, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    // insert erase
    {
        std::string data[]{"A", "B", "C"};
        simple_vector<std::string> vec(data, data + 3);

        std::string data2[]{"D", "E"};

        // push_back
        vec.insert(vec.cend(), data2, data2 + 2);

        // push_front
        vec.insert(vec.cbegin(), data2, data2 + 2);

        // push_middle
        vec.insert(vec.cbegin() + 2, data2, data2 + 2);

        std::vector<std::string> tmp{"D", "E", "D", "E", "A", "B", "C", "D", "E"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp.cbegin(), tmp.cend()));

        // pop_back
        vec.erase(vec.begin() + 7, vec.end());

        // pop_middle
        vec.erase(vec.cbegin() + 2, vec.cbegin() + 4);

        // pop_front
        vec.erase(vec.cbegin(), vec.cbegin() + 2);

        std::vector<std::string> tmp2{"A", "B", "C"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp2.cbegin(), tmp2.cend()));
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        simple_vector<int> vec1(arr1.cbegin(), arr1.cend());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(simple_vector, int_methods)
{
    using namespace oc;

    std::array<int, 16> arr{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};

    simple_vector<int> sv(arr.cbegin(), arr.cend());
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(16, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    EXPECT_EQ(16, sv.back());

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
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    sv.append(1);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(25, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    sv.append(5);
    EXPECT_EQ(37, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(50);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    sv.reserve(45);
    EXPECT_EQ(50, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    sv.shrink_to_fit();
    EXPECT_EQ(30, sv.capacity());
    EXPECT_EQ(30, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    // insert erase
    {
        int data[]{1, 2, 3};
        simple_vector<int> vec(data, data + 3);

        int data2[]{4, 5};

        // push_back
        vec.insert(vec.cend(), data2, data2 + 2);

        // push_front
        vec.insert(vec.cbegin(), data2, data2 + 2);

        // push_middle
        vec.insert(vec.cbegin() + 2, data2, data2 + 2);

        std::vector<int> tmp{4, 5, 4, 5, 1, 2, 3, 4, 5};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp.cbegin(), tmp.cend()));

        // pop_back
        vec.erase(vec.begin() + 7, vec.end());

        // pop_middle
        vec.erase(vec.cbegin() + 2, vec.cbegin() + 4);

        // pop_front
        vec.erase(vec.cbegin(), vec.cbegin() + 2);

        std::vector<int> tmp2{1, 2, 3};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp2.cbegin(), tmp2.cend()));
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        simple_vector<int> vec1(arr1.cbegin(), arr1.cend());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(simple_vector, insert)
{
    using namespace oc;

    simple_vector<int> c1(3ul, 100);
    {
        std::array<int, 3> res{100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int val1 = 200;
    auto it = c1.begin();
    it = c1.insert(it, &val1, &val1 + 1);
    {
        std::array<int, 4> res{200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    c1.insert(it, 2, 300);
    {
        std::array<int, 6> res{300, 300, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    it = c1.begin();

    std::vector<int> c2(2, 400);
    c1.insert(std::next(it, 2), c2.begin(), c2.end());
    {
        std::array<int, 8> res{300, 300, 400, 400, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int arr[]{501, 502, 503};
    c1.insert(c1.begin(), arr, arr + std::size(arr));
    {
        std::array<int, 11> res{501, 502, 503, 300, 300, 400, 400, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int c3[]{601, 602, 603};
    c1.insert(c1.end(), c3, c3 + 3);
    {
        std::array<int, 14> res{501, 502, 503, 300, 300, 400, 400, 200, 100, 100, 100, 601, 602, 603};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }
}

TEST(simple_vector, erase)
{
    using namespace oc;

    int data[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    simple_vector<int> c(data, data + std::size(data));

    c.erase(c.begin());
    {
        std::array<int, 9> res{1, 2, 3, 4, 5, 6, 7, 8, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }

    c.erase(c.begin() + 2, c.begin() + 5);
    {
        std::array<int, 6> res{1, 2, 6, 7, 8, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }

    for (auto it = c.begin(); it != c.end();) {
        if (*it % 2 == 0) {
            it = c.erase(it);
        } else {
            ++it;
        }
    }
    {
        std::array<int, 3> res{1, 7, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }
}

TEST(simple_array, methods)
{
    using namespace oc;

    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};

    simple_array<std::string, 16> sv(arr.cbegin(), arr.cend());
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

    //EXPECT_THROW(sv.resize(24), std::length_error); // assertion failure
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

    //EXPECT_THROW(sv.expand(10), std::length_error); // assertion failure
    sv.append(1);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(10, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    // reserve and shrink_to_fit are noop
    sv.shrink_to_fit();
    EXPECT_THROW(sv.reserve(1000), std::invalid_argument);
    EXPECT_EQ(16, sv.capacity());

        // insert erase
    {
        std::string data[]{"A", "B", "C"};
        simple_array<std::string, 16> vec(data, data + 3);

        std::string data2[]{"D", "E"};

        // push_back
        vec.insert(vec.cend(), &data2[0], data2 + 2);

        // push_front
        vec.insert(vec.cbegin(), &data2[0], data2 + 2);

        // push_middle
        vec.insert(vec.cbegin() + 2, &data2[0], data2 + 2);

        std::vector<std::string> tmp{"D", "E", "D", "E", "A", "B", "C", "D", "E"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp.cbegin(), tmp.cend()));

        // pop_back
        vec.erase(vec.cbegin() + 7, vec.cend());

        // pop_middle
        vec.erase(vec.cbegin() + 2, vec.cbegin() + 4);

        // pop_front
        vec.erase(vec.cbegin(), vec.cbegin() + 2);

        std::vector<std::string> tmp2{"A", "B", "C"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp2.cbegin(), tmp2.cend()));
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        simple_array<int, 5> vec1(arr1.cbegin(), arr1.cend());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(simple_array, int_methods)
{
    using namespace oc;

    std::array<int, 16> arr{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};

    simple_array<int, 16> sv(arr.cbegin(), arr.cend());
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(16, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    EXPECT_EQ(16, sv.back());

    int ctr = 0;
    for (const auto& e : sv) {
        EXPECT_EQ(arr[ctr++], e);
    }

    for (int i = 0; i < sv.size(); ++i) {
        EXPECT_EQ(arr[i], sv[i]);
    }

    //EXPECT_THROW(sv.resize(24), std::length_error); // assertion failure
    sv.resize(8);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(8, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    EXPECT_EQ(8, sv.back());
    sv.resize(9);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(9, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("h", sv.back());

    //EXPECT_THROW(sv.expand(10), std::length_error); // assertion failure
    sv.append(1);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(10, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ(1, sv.front());
    //EXPECT_EQ("", sv.back());

    // reserve and shrink_to_fit are noop
    sv.shrink_to_fit();
    EXPECT_THROW(sv.reserve(1000), std::invalid_argument);
    EXPECT_EQ(16, sv.capacity());

    // insert erase
    {
        int data[]{1, 2, 3};
        simple_array<int, 16> vec(data, data + 3);

        int data2[]{4, 5};

        // push_back
        vec.insert(vec.cend(), &data2[0], data2 + 2);

        // push_front
        vec.insert(vec.cbegin(), &data2[0], data2 + 2);

        // push_middle
        vec.insert(vec.cbegin() + 2, &data2[0], data2 + 2);

        std::vector<int> tmp{4, 5, 4, 5, 1, 2, 3, 4, 5};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp.cbegin(), tmp.cend()));

        // pop_back
        vec.erase(vec.cbegin() + 7, vec.cend());

        // pop_middle
        vec.erase(vec.cbegin() + 2, vec.cbegin() + 4);

        // pop_front
        vec.erase(vec.cbegin(), vec.cbegin() + 2);

        std::vector<int> tmp2{1, 2, 3};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp2.cbegin(), tmp2.cend()));
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        simple_array<int, 5> vec1(arr1.cbegin(), arr1.cend());

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

TEST(simple_array, insert)
{
    using namespace oc;

    simple_array<int, 14> c1(3ul, 100);
    {
        std::array<int, 3> res{100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int val1 = 200;
    auto it = c1.begin();
    it = c1.insert(it, &val1, &val1 + 1);
    {
        std::array<int, 4> res{200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    c1.insert(it, 2, 300);
    {
        std::array<int, 6> res{300, 300, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    it = c1.begin();

    std::vector<int> c2(2, 400);
    c1.insert(std::next(it, 2), c2.begin(), c2.end());
    {
        std::array<int, 8> res{300, 300, 400, 400, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int arr[]{501, 502, 503};
    c1.insert(c1.begin(), arr, arr + std::size(arr));
    {
        std::array<int, 11> res{501, 502, 503, 300, 300, 400, 400, 200, 100, 100, 100};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }

    int c3[]{601, 602, 603};
    c1.insert(c1.end(), c3, c3 + 3);
    {
        std::array<int, 14> res{501, 502, 503, 300, 300, 400, 400, 200, 100, 100, 100, 601, 602, 603};
        EXPECT_TRUE(std::equal(c1.cbegin(), c1.cend(), res.cbegin(), res.cend()));
    }
}

TEST(simple_array, erase)
{
    using namespace oc;

    int data[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    simple_array<int, 10> c(data, data + std::size(data));

    c.erase(c.begin());
    {
        std::array<int, 9> res{1, 2, 3, 4, 5, 6, 7, 8, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }

    c.erase(c.begin() + 2, c.begin() + 5);
    {
        std::array<int, 6> res{1, 2, 6, 7, 8, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }

    for (auto it = c.begin(); it != c.end();) {
        if (*it % 2 == 0) {
            it = c.erase(it);
        } else {
            ++it;
        }
    }
    {
        std::array<int, 3> res{1, 7, 9};
        EXPECT_TRUE(std::equal(c.cbegin(), c.cend(), res.cbegin(), res.cend()));
    }
}


TEST(simple_view, methods)
{
    using namespace oc;

    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};

    simple_view<std::string> sv(std::span<std::string>{arr.begin(), arr.size()});
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

    //EXPECT_THROW(sv.resize(24), std::length_error); // assertion failure
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

    //EXPECT_THROW(sv.expand(10), std::length_error); // assertion failure
    sv.append(1);
    EXPECT_EQ(16, sv.capacity());
    EXPECT_EQ(10, sv.size());
    EXPECT_FALSE(sv.empty());
    EXPECT_TRUE(sv.data());
    EXPECT_EQ("a", sv.front());
    //EXPECT_EQ("", sv.back());

    // reserve and shrink_to_fit are noop
    sv.shrink_to_fit();
    EXPECT_THROW(sv.reserve(1000), std::invalid_argument);
    EXPECT_EQ(10, sv.capacity());

    // insert erase
    {
        std::string data[]{"A", "B", "C", "", "", "", "", "", ""};
        simple_view<std::string> vec(std::span<std::string>{data, 3}, std::size(data));

        std::string data2[]{"D", "E"};

        // push_back
        vec.insert(vec.cend(), &data2[0], data2 + 2);

        // push_front
        vec.insert(vec.cbegin(), &data2[0], data2 + 2);

        // push_middle
        vec.insert(vec.cbegin() + 2, &data2[0], data2 + 2);

        std::vector<std::string> tmp{"D", "E", "D", "E", "A", "B", "C", "D", "E"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp.cbegin(), tmp.cend()));

        // pop_back
        vec.erase(vec.cbegin() + 7, vec.cend());

        // pop_middle
        vec.erase(vec.cbegin() + 2, vec.cbegin() + 4);

        // pop_front
        vec.erase(vec.cbegin(), vec.cbegin() + 2);

        std::vector<std::string> tmp2{"A", "B", "C"};

        EXPECT_TRUE(std::equal(vec.cbegin(), vec.cend(), tmp2.cbegin(), tmp2.cend()));
    }

    // iterators
    {
        std::array<int, 5> arr1{1, 2, 3, 4, 5};
        simple_view<int> vec1(std::span<int>(arr1.begin(), arr1.size()));

        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
    }
}

//
//TEST(simple_dynamic_vector_test, span_and_iterators_usage)
//{
//    using simple_vector = oc::simple_dynamic_vector<std::string>;
//
//    auto count_elements = [](std::span<const std::string> s) {
//        return s.size();
//    };
//
//    simple_vector sv(2, std::array<std::string, 2>{"first string", "second string"}.data());
//    EXPECT_EQ(2, count_elements(sv));
//    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) {
//        return s.find("string") != std::string::npos;
//    }));
//}
//
//TEST(simple_dynamic_vector_test, basic_functionality)
//{
//    using simple_vector = oc::simple_dynamic_vector<std::string>;
//
//    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};
//
//    simple_vector sv(16, arr.data());
//    EXPECT_EQ(16, sv.size());
//    EXPECT_FALSE(sv.empty());
//    EXPECT_TRUE(sv.data());
//    EXPECT_EQ("a", sv.front());
//    EXPECT_EQ("p", sv.back());
//
//    int ctr = 0;
//    for (const auto& e : sv) {
//        EXPECT_EQ(arr[ctr++], e);
//    }
//
//    for (int i = 0; i < sv.size(); ++i) {
//        EXPECT_EQ(arr[i], sv[i]);
//    }
//
//    // iterators
//    {
//        std::array<int, 5> arr1{1, 2, 3, 4, 5};
//        oc::simple_dynamic_vector<int> vec1(5, arr1.data());
//
//        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
//    }
//
//    // copy
//    EXPECT_NE(arr.data(), sv.data());
//
//    auto svc = sv;
//    EXPECT_NE(svc.data(), sv.data());
//}
//
//TEST(simple_dynamic_vector_test, basic_view_functionality)
//{
//    using simple_vector = oc::simple_dynamic_vector<std::string>;
//
//    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};
//
//    simple_vector sv(16, arr.data(), true);
//    EXPECT_EQ(16, sv.size());
//    EXPECT_FALSE(sv.empty());
//    EXPECT_TRUE(sv.data());
//    EXPECT_EQ("a", sv.front());
//    EXPECT_EQ("p", sv.back());
//
//    int ctr = 0;
//    for (const auto& e : sv) {
//        EXPECT_EQ(arr[ctr++], e);
//    }
//
//    for (int i = 0; i < sv.size(); ++i) {
//        EXPECT_EQ(arr[i], sv[i]);
//    }
//
//    // iterators
//    {
//        std::array<int, 5> arr1{1, 2, 3, 4, 5};
//        oc::simple_dynamic_vector<int> vec1(5, arr1.data());
//
//        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
//    }
//
//    // copy
//    EXPECT_EQ(arr.data(), sv.data());
//
//    auto svc = sv;
//    EXPECT_EQ(svc.data(), sv.data());
//}
//
//TEST(simple_static_vector_test, span_usage)
//{
//    using simple_vector = oc::simple_static_vector<std::string, 2>;
//
//    auto count_elements = [](std::span<const std::string> s) {
//        return s.size();
//    };
//
//    simple_vector sv(2, std::array<std::string, 2>{"first string", "second string"}.data());
//    EXPECT_EQ(2, count_elements(sv));
//    EXPECT_EQ(2, std::count_if(sv.begin(), sv.end(), [](const auto& s) {
//        return s.find("string") != std::string::npos;
//    }));
//}
//
//TEST(simple_static_vector_test, basic_functionality)
//{
//    using simple_vector = oc::simple_static_vector<std::string, 16>;
//
//    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};
//
//    simple_vector sv(16, arr.data());
//    EXPECT_EQ(16, sv.size());
//    EXPECT_FALSE(sv.empty());
//    EXPECT_TRUE(sv.data());
//    EXPECT_EQ("a", sv.front());
//    EXPECT_EQ("p", sv.back());
//
//    int ctr = 0;
//    for (const auto& e : sv) {
//        EXPECT_EQ(arr[ctr++], e);
//    }
//
//    for (int i = 0; i < sv.size(); ++i) {
//        EXPECT_EQ(arr[i], sv[i]);
//    }
//
//    // iterators
//    {
//        std::array<int, 5> arr1{1, 2, 3, 4, 5};
//        oc::simple_static_vector<int, 5> vec1(5, arr1.data());
//
//        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
//    }
//
//    // copy
//    EXPECT_NE(arr.data(), sv.data());
//
//    auto svc = sv;
//    EXPECT_NE(svc.data(), sv.data());
//
//    EXPECT_FALSE(sv.is_view());
//    EXPECT_FALSE(svc.is_view());
//}
//
//TEST(simple_static_vector_test, basic_view_functionality)
//{
//    using simple_vector = oc::simple_static_vector<std::string, 16>;
//
//    std::array<std::string, 16> arr{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"};
//
//    simple_vector sv(16, arr.data(), true);
//    EXPECT_EQ(16, sv.size());
//    EXPECT_FALSE(sv.empty());
//    EXPECT_TRUE(sv.data());
//    EXPECT_EQ("a", sv.front());
//    EXPECT_EQ("p", sv.back());
//
//    int ctr = 0;
//    for (const auto& e : sv) {
//        EXPECT_EQ(arr[ctr++], e);
//    }
//
//    for (int i = 0; i < sv.size(); ++i) {
//        EXPECT_EQ(arr[i], sv[i]);
//    }
//
//    // iterators
//    {
//        std::array<int, 5> arr1{1, 2, 3, 4, 5};
//        oc::simple_static_vector<int, 5> vec1(5, arr1.data());
//
//        EXPECT_EQ(15, std::reduce(vec1.begin(), vec1.end(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.cbegin(), vec1.cend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.rbegin(), vec1.rend(), std::int64_t{0}, std::plus<>{}));
//        EXPECT_EQ(15, std::reduce(vec1.crbegin(), vec1.crend(), std::int64_t{0}, std::plus<>{}));
//    }
//
//    // copy
//    EXPECT_EQ(arr.data(), sv.data());
//
//    auto svc = sv;
//    EXPECT_EQ(svc.data(), sv.data());
//
//    EXPECT_TRUE(sv.is_view());
//    EXPECT_TRUE(svc.is_view());
//}

TEST(arrnd_test, arrnd_view_from_external_type)
{
    using namespace oc;

    // create arrnd view from continuous container (e.g. vector, span, etc.)

    std::vector<int> vec{1, 2, 3, 4, 5, 6};

    auto arrv = view<arrnd<int>>({3, 1, 2}, vec);

    arrv.apply([](int value) {
        return value * 2;
    });

    EXPECT_EQ(vec, (std::vector<int>{2, 4, 6, 8, 10, 12}));
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

    // complex numbers
    {
        using namespace std::complex_literals;

        EXPECT_TRUE(1.0 + 1.0i < 1.0 + 2.0i);
        EXPECT_TRUE(1.0 + 1.0i <= 1.0 + 2.0i);
        EXPECT_TRUE(2.0 + 1.0i > 1.0 + 1.0i);
        EXPECT_TRUE(2.0 + 1.0i >= 1.0 + 1.0i);

        EXPECT_TRUE(1.0 < 1.0 + 2.0i);
        EXPECT_TRUE(1.0 <= 1.0 + 2.0i);
        EXPECT_TRUE(2.0 > 1.0i);
        EXPECT_TRUE(2.0 >= 1.0i);

        EXPECT_TRUE(1.0i < 2.0);
        EXPECT_TRUE(1.0i <= 2.0);
        EXPECT_TRUE(2.0 + 1.0i > 1.0);
        EXPECT_TRUE(2.0 + 1.0i >= 1.0);

        EXPECT_TRUE(close(1.0 + 1.0i, 1.0 + 1.0i));
    }
}
//
//TEST(modulo_test, modulo_opration_can_be_perform_on_positive_zero_or_negative_number)
//{
//    using namespace oc;
//
//    EXPECT_EQ(0, modulo(0, 5));
//    EXPECT_EQ(1, modulo(1, 5));
//    EXPECT_EQ(1, modulo(26, 5));
//    EXPECT_EQ(4, modulo(-1, 5));
//    EXPECT_EQ(4, modulo(-26, 5));
//}

TEST(interval_test, initialization)
{
    oc::interval i1{};
    EXPECT_EQ(0, i1.start());
    EXPECT_EQ(1, i1.stop());
    EXPECT_EQ(1, i1.step());

    //oc::interval i2{1}; // deprecated
    //EXPECT_EQ(1, i2.start()());
    //EXPECT_EQ(2, i2.stop());
    //EXPECT_EQ(1, i2.step());

    oc::interval i3{1, 2};
    EXPECT_EQ(1, i3.start());
    EXPECT_EQ(2, i3.stop());
    EXPECT_EQ(1, i3.step());

    oc::interval i4{1, 2, 3};
    EXPECT_EQ(1, i4.start());
    EXPECT_EQ(2, i4.stop());
    EXPECT_EQ(3, i4.step());
}

//TEST(interval_test, reverse)
//{
//    oc::interval i{oc::reverse(oc::interval{1, 2, 3})};
//    EXPECT_EQ(2, i.start());
//    EXPECT_EQ(1, i.stop());
//    EXPECT_EQ(-3, i.step());
//}
//
//TEST(interval_test, modulo)
//{
//    oc::interval i{oc::modulo(oc::interval{-26, 26, -1}, 5)};
//    EXPECT_EQ(4, i.start());
//    EXPECT_EQ(1, i.stop());
//    EXPECT_EQ(-1, i.step());
//}
//
//TEST(interval_test, forward)
//{
//    oc::interval i1{oc::forward(oc::interval{1, 2, 3})};
//    EXPECT_EQ(1, i1.start());
//    EXPECT_EQ(2, i1.stop());
//    EXPECT_EQ(3, i1.step());
//
//    oc::interval i2{oc::forward(oc::interval{2, 1, -3})};
//    EXPECT_EQ(1, i2.start());
//    EXPECT_EQ(2, i2.stop());
//    EXPECT_EQ(3, i2.step());
//}

TEST(interval_test, presets)
{
    oc::interval<std::int64_t> i1 = oc::interval<std::int64_t>::at(5);
    EXPECT_EQ(5, i1.start());
    EXPECT_EQ(6, i1.stop());
    EXPECT_EQ(1, i1.step());
    EXPECT_FALSE(oc::isunbound(i1));
    EXPECT_FALSE(oc::empty(i1));
    EXPECT_TRUE(oc::isbetween(i1, 5, 6));
    //EXPECT_EQ(oc::interval<std::int64_t>(5, 6, 1, oc::interval_hint::none), i1);

    //oc::interval<std::int64_t> i2 = oc::interval<std::int64_t>::full(5); // deprecated
    //EXPECT_EQ(0, i2.start());
    //EXPECT_EQ(5, i2.stop());
    //EXPECT_EQ(1, i2.step());
    oc::interval<std::int64_t> i2 = oc::interval<std::int64_t>::full(2);
    EXPECT_EQ(2, i2.step());
    EXPECT_TRUE(oc::isunbound(i2));
    EXPECT_FALSE(oc::empty(i2));
    EXPECT_FALSE(oc::isbetween(i2, 5, 6));
    //EXPECT_EQ(oc::interval_hint::full, i2.hint());
    //EXPECT_EQ(oc::interval<std::int64_t>(std::rand(), std::rand(), 2, oc::interval_hint::full), i2);
    //EXPECT_EQ(oc::interval<std::int64_t>(0, 6, 2, oc::interval_hint::none), i2.align(6));
    EXPECT_EQ((oc::interval<std::int64_t>{0, 6, 2}), oc::bound(i2, 0, 6));

    //oc::interval<std::int64_t> i3 = oc::interval<std::int64_t>::from(5, 5); // deprecated
    //EXPECT_EQ(5, i3.start());
    //EXPECT_EQ(10, i3.stop());
    //EXPECT_EQ(1, i3.step());
    oc::interval<std::int64_t> i3 = oc::interval<std::int64_t>::from(6, 2);
    EXPECT_EQ(6, i3.start());
    EXPECT_EQ(2, i3.step());
    EXPECT_TRUE(oc::isunbound(i3));
    EXPECT_FALSE(oc::empty(i3));
    EXPECT_FALSE(oc::isbetween(i3, 5, 6));
    //EXPECT_EQ(oc::interval_hint::from, i3.hint());
    //EXPECT_EQ(oc::interval<std::int64_t>(6, std::rand(), 2, oc::interval_hint::from), i3);
    //EXPECT_EQ(oc::interval<std::int64_t>(6, 10, 2, oc::interval_hint::none), i3.align(10));
    EXPECT_EQ((oc::interval<std::int64_t>{6, 10, 2}), oc::bound(i3, std::rand(), 10));

    oc::interval<std::int64_t> i4 = oc::interval<std::int64_t>::to(5, 3);
    EXPECT_EQ(5, i4.stop());
    EXPECT_EQ(3, i4.step());
    EXPECT_TRUE(oc::isunbound(i4));
    EXPECT_FALSE(oc::empty(i4));
    EXPECT_FALSE(oc::isbetween(i4, 5, 6));
    //EXPECT_EQ(oc::interval<std::int64_t>(0, 5, 3, oc::interval_hint::to), i4);
    //EXPECT_EQ(oc::interval<std::int64_t>(0, 5, 3, oc::interval_hint::none), i4.align(10));
    EXPECT_EQ((oc::interval<std::int64_t>{0, 5, 3}), oc::bound(i4, 0, std::rand()));

    oc::interval<std::int64_t> i5 = oc::interval<std::int64_t>::between(1, 5, 5);
    //EXPECT_EQ(oc::interval<std::int64_t>(1, 5, 5, oc::interval_hint::none), i5);
    //EXPECT_EQ(oc::interval<std::int64_t>(1, 5, 5, oc::interval_hint::none), i5.align(std::rand()));
    EXPECT_EQ((oc::interval<std::int64_t>{1, 5, 5}), oc::bound(i5, std::rand(), std::rand()));

    {
        /*using namespace oc::details;

        std::vector<std::size_t> dims{14, 26, 16};
        std::vector<interval<std::size_t>> boundaries{
            interval<std::size_t>::between(1, 14), interval<std::size_t>::between(5, 16, 2)};

        arrnd_dimensionality<> hdr(dims, boundaries);

        std::vector<interval<std::size_t>> inner_boundaires{
            interval<std::size_t>::between(2, 6), interval<std::size_t>::full(), interval<std::size_t>::from(3, 6)};

        auto slc1 = slice(hdr, inner_boundaires);

        auto slc2 = slice(slc1, interval<std::size_t>::between(1, 2, 2), 2);*/

        //using namespace oc;

        //arrnd_info ai({1, 4});

        //auto sq = roll(ai, 1);

        //std::cout << sq << "\n";
    }
}

//TEST(dummy, dummy)
//{
//    using namespace oc;
//
//    arrnd<int> arr({2, 3}, {1, 2, 3, 4, 5, 6});
//    std::cout << arr << "\n\n";
//    std::cout << resize(arr, {2, 5}) << "\n\n";
//    std::cout << resize(arr, {3, 3}) << "\n\n";
//    std::cout << resize(arr, {10}) << "\n\n";
//    std::cout << resize(arr, {1, 3}) << "\n\n";
//    std::cout << resize(arr, {2, 2}) << "\n\n";
//    std::cout << resize(arr, {2, 1, 2}) << "\n\n";
//}

TEST(general_iterable_types_check, typed_iterator)
{
    static_assert(std::is_same_v<int, oc::iterator_value_type<std::vector<int>::iterator>>);
    static_assert(std::is_same_v<int, oc::iterator_value_type<oc::arrnd<int>::iterator>>);
    static_assert(oc::details::integral_type_iterator<std::vector<int>::iterator>);
    static_assert(oc::details::integral_type_iterator<oc::arrnd<int>::iterator>);
    static_assert(!oc::signed_integral_type_iterator<std::vector<double>::iterator>);
    static_assert(oc::interval_type_iterator<std::vector<oc::interval<int>>::iterator>);
    static_assert(oc::interval_type_iterator<oc::arrnd<oc::interval<int>>::iterator>);
    static_assert(!oc::interval_type_iterator<std::vector<double>::iterator>);
}

TEST(general_iterable_types_check, typed_iterable)
{
    static_assert(oc::iterable<std::vector<int>>);
    static_assert(oc::iterable<oc::arrnd<int>>);

    static_assert(oc::signed_integral_type_iterable<std::vector<int>>);
    static_assert(oc::signed_integral_type_iterable<oc::arrnd<int>>);
    static_assert(!oc::signed_integral_type_iterable<std::vector<double>>);

    static_assert(oc::interval_type_iterable<std::vector<oc::interval<int>>>);
    static_assert(oc::interval_type_iterable<oc::arrnd<oc::interval<int>>>);
    static_assert(!oc::interval_type_iterable<std::vector<double>>);
}

TEST(general_iterable_types_check, random_access_type)
{
    static_assert(oc::random_access_type<std::vector<int>>);
}

TEST(arrnd_header_test, can_return_array_info)
{
    {
        oc::arrnd_info ehdr;

        EXPECT_EQ(0, ehdr.dims().size());
        EXPECT_EQ(0, oc::total(ehdr));
        EXPECT_TRUE(ehdr.dims().empty());
        EXPECT_TRUE(ehdr.strides().empty());
        EXPECT_EQ(0, ehdr.indices_boundary().start());
        EXPECT_FALSE(oc::issliced(ehdr));
        EXPECT_TRUE(!oc::isvector(ehdr) && !oc::ismatrix(ehdr) && !oc::isrow(ehdr) && !oc::iscolumn(ehdr)
            && !oc::isscalar(ehdr));
        //EXPECT_FALSE(ehdr.is_reordered());

        oc::arrnd_info hdr({3, 1, 2});

        EXPECT_EQ(3, hdr.dims().size());
        EXPECT_EQ(6, oc::total(hdr));
        EXPECT_EQ(3, hdr.dims().data()[0]);
        EXPECT_EQ(1, hdr.dims().data()[1]);
        EXPECT_EQ(2, hdr.dims().data()[2]);
        EXPECT_EQ(2, hdr.strides().data()[0]);
        EXPECT_EQ(2, hdr.strides().data()[1]);
        EXPECT_EQ(1, hdr.strides().data()[2]);
        EXPECT_EQ(0, hdr.indices_boundary().start());
        EXPECT_FALSE(oc::issliced(hdr));
        EXPECT_TRUE(!oc::isvector(ehdr) && !oc::ismatrix(ehdr) && !oc::isrow(ehdr) && !oc::iscolumn(ehdr)
            && !oc::isscalar(ehdr));
        //EXPECT_FALSE(hdr.is_reordered());

        auto rhdr = oc::move(hdr, 1, 0);
        //EXPECT_TRUE(rhdr.is_reordered());
        EXPECT_EQ(1, rhdr.dims().data()[0]);
        EXPECT_EQ(3, rhdr.dims().data()[1]);
        EXPECT_EQ(2, rhdr.dims().data()[2]);
        EXPECT_EQ(2, rhdr.strides().data()[0]);
        EXPECT_EQ(2, rhdr.strides().data()[1]);
        EXPECT_EQ(1, rhdr.strides().data()[2]);

        oc::arrnd_info hdr1({2});
        EXPECT_TRUE(oc::isvector(hdr1) && !oc::ismatrix(hdr1) && !oc::isrow(hdr1) && !oc::iscolumn(hdr1)
            && !oc::isscalar(hdr1));
        oc::arrnd_info hdr2({1, 2});
        EXPECT_TRUE(
            !oc::isvector(hdr2) && oc::ismatrix(hdr2) && oc::isrow(hdr2) && !oc::iscolumn(hdr2) && !oc::isscalar(hdr2));
        oc::arrnd_info hdr3({2, 1});
        EXPECT_TRUE(
            !oc::isvector(hdr3) && oc::ismatrix(hdr3) && !oc::isrow(hdr3) && oc::iscolumn(hdr3) && !oc::isscalar(hdr3));
        oc::arrnd_info hdr4({2, 2});
        EXPECT_TRUE(!oc::isvector(hdr4) && oc::ismatrix(hdr4) && !oc::isrow(hdr4) && !oc::iscolumn(hdr4)
            && !oc::isscalar(hdr4));
        oc::arrnd_info hdr5({1, 1, 1, 1});
        EXPECT_TRUE(!oc::isvector(hdr5) && !oc::ismatrix(hdr5) && !oc::isrow(hdr5) && !oc::iscolumn(hdr5)
            && oc::isscalar(hdr5));
    }

    // arrnd_header continuity reordering and slicing
    {
        using namespace oc;

        arrnd_info hdr({2, 4, 3});

        EXPECT_TRUE(!oc::issliced(hdr) && !oc::istransposed(hdr) && oc::iscontinuous(hdr));

        //auto hdr1 = hdr.reorder()
    }
}

TEST(arrnd_header_test, subscripts_and_indices_conversions)
{
    using namespace oc;

    {
        arrnd_info<> hdr({6, 2, 4});

        for (typename arrnd_info<>::extent_type i = 0; i < hdr.dims()[0]; ++i) {
            for (typename arrnd_info<>::extent_type j = 0; j < hdr.dims()[1]; ++j) {
                for (typename arrnd_info<>::extent_type k = 0; k < hdr.dims()[2]; ++k) {
                    typename arrnd_info<>::extent_type subs_data[]{i, j, k};
                    typename arrnd_info<>::extent_storage_type subs(/*3, subs_data*/subs_data, subs_data + 3);
                    typename arrnd_info<>::extent_storage_type res_subs(3);
                    auto ind = sub2ind(hdr, subs);
                    ind2sub(hdr, ind, std::begin(res_subs));
                    EXPECT_EQ(res_subs, subs);
                }
            }
        }

        arrnd_info<> shdr = slice(hdr, {interval<>::between(1, 6, 2), interval<>::at(0), interval<>::from(2)});

        for (typename arrnd_info<>::extent_type i = 0; i < shdr.dims()[0]; ++i) {
            for (typename arrnd_info<>::extent_type j = 0; j < shdr.dims()[1]; ++j) {
                for (typename arrnd_info<>::extent_type k = 0; k < shdr.dims()[2]; ++k) {
                    typename arrnd_info<>::extent_type subs_data[]{i, j, k};
                    typename arrnd_info<>::extent_storage_type subs(/*3, subs_data*/ subs_data, subs_data + 3);
                    typename arrnd_info<>::extent_storage_type res_subs(3);
                    auto ind = sub2ind(shdr, subs);
                    ind2sub(shdr, ind, std::begin(res_subs));
                    EXPECT_EQ(res_subs, subs);
                }
            }
        }
    }
}

//TEST(arrnd_indexer, simple_forward_backward_iterations)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//
//    arrnd_header hdr(dims);
//    const std::int64_t expected_inds_list[6]{0, 1, 2, 3, 4, 5};
//    const std::int64_t expected_generated_subs{6};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_indexer gen(hdr);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}

TEST(experimental_arrnd_indexer, simple_forward_backward_iterations)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[6]{0, 1, 2, 3, 4, 5};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_indexer gen(hdr);

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

//TEST(arrnd_indexer, simple_backward_forward_iterations)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const std::int64_t expected_inds_list[6]{5, 4, 3, 2, 1, 0};
//    const std::int64_t expected_generated_subs{6};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        --gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (++gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}

TEST(experimental_arrnd_indexer, simple_backward_forward_iterations)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[6]{5, 4, 3, 2, 1, 0};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);

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

//TEST(arrnd_indexer, simple_forward_backward_iterations_with_steps_bigger_than_one)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const std::int64_t expected_inds_list[6]{0, 2, 4};
//    const std::int64_t expected_generated_subs{3};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_indexer gen(hdr);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        gen += 2;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while ((gen = gen - 2)) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}

TEST(experimental_arrnd_indexer, simple_forward_backward_iterations_with_steps_bigger_than_one)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[6]{0, 2, 4};
    const std::int64_t expected_generated_subs{3};

    std::int64_t generated_subs_counter{0};
    arrnd_indexer gen(hdr);

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

//TEST(arrnd_indexer, forward_backward_iterations_by_axis_order)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    const std::int64_t order[]{2, 0, 1};
//    arrnd_header hdr(dims);
//
//    const std::int64_t expected_inds_list[6]{0, 2, 4, 1, 3, 5};
//    const std::int64_t expected_generated_subs{6};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_indexer gen(hdr, order);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}

TEST(experimental_arrnd_indexer, forward_backward_iterations_by_axis_order)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    const std::size_t order[]{2, 0, 1};
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[6]{0, 2, 4, 1, 3, 5};
    const std::int64_t expected_generated_subs{6};

    std::int64_t generated_subs_counter{0};
    arrnd_indexer gen(oc::transpose(hdr, order));

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

//TEST(arrnd_indexer, forward_backward_iterations_by_specific_major_axis)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const std::int64_t expected_inds_list[][6]{{0, 1, 2, 3, 4, 5},
//
//        {0, 1, 2, 3, 4, 5},
//
//        {0, 2, 4, 1, 3, 5}};
//    const std::int64_t expected_generated_subs{6};
//
//    for (std::int64_t axis = 0; axis <= 2; ++axis) {
//        std::int64_t generated_subs_counter{0};
//        arrnd_indexer gen(hdr, axis);
//
//        while (gen) {
//            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
//            ++generated_subs_counter;
//            ++gen;
//        }
//        EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//        while (--gen) {
//            --generated_subs_counter;
//            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
//        }
//        EXPECT_EQ(0, generated_subs_counter);
//    }
//}

TEST(experimental_arrnd_indexer, forward_backward_iterations_by_specific_major_axis)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[][6]{{0, 1, 2, 3, 4, 5},

        {0, 1, 2, 3, 4, 5},

        {0, 2, 4, 1, 3, 5}};
    const std::int64_t expected_generated_subs{6};

    for (std::int64_t axis = 0; axis <= 2; ++axis) {
        std::int64_t generated_subs_counter{0};
        arrnd_indexer gen(move(hdr, axis, 0));

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

//TEST(arrnd_indexer, random_access)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const std::int64_t expected_inds_list[6]{0, 5, 4, 1, 2, 3};
//
//    arrnd_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);
//
//    EXPECT_EQ(expected_inds_list[0], gen[0]);
//    EXPECT_EQ(expected_inds_list[1], gen[5]);
//    EXPECT_EQ(expected_inds_list[2], gen[4]);
//    EXPECT_EQ(expected_inds_list[3], gen[1]);
//    EXPECT_EQ(expected_inds_list[4], gen[2]);
//    EXPECT_EQ(expected_inds_list[5], gen[3]);
//}

TEST(experimental_arrnd_indexer, random_access)
{
    using namespace oc::experimental;

    const std::size_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
    oc::arrnd_info hdr(dims);

    const std::int64_t expected_inds_list[6]{0, 5, 4, 1, 2, 3};

    arrnd_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);

    EXPECT_EQ(expected_inds_list[0], *(gen[0]));
    EXPECT_EQ(expected_inds_list[1], *(gen[5]));
    EXPECT_EQ(expected_inds_list[2], *(gen[4]));
    EXPECT_EQ(expected_inds_list[3], *(gen[1]));
    EXPECT_EQ(expected_inds_list[4], *(gen[2]));
    EXPECT_EQ(expected_inds_list[5], *(gen[3]));
}

//TEST(experimental_window_slider, dummy)
//{
//    oc::arrnd_info ai({/*2*/ 6, 4});
//
//    //oc::arrnd_info slc = oc::slice(ai, {oc::interval<std::size_t>::from(1, 2), oc::interval<std::size_t>::from(2, 3)});
//    //std::cout << slc << "\n\n";
//    //std::vector</*oc::interval<std::size_t>*/std::size_t> window{/*oc::interval<std::size_t>::full(),*/
//    //    2, 3};
//
//    oc::experimental::arrnd_sliding_window window(oc::interval<>::between(-1, 3));
//
//    std::vector windows{
//        oc::experimental::arrnd_sliding_window(oc::interval<>::between(-1, 3), oc::experimental::arrnd_sliding_window_type::partial)};
//
//    int i = 0;
//    for (oc::experimental::arrnd_window_slider ws(
//             /*slc*/ ai, /*windows*/ 0, oc::experimental::arrnd_sliding_window(oc::interval<>(-1, 3)));
//         ws; ++ws) {
//        std::cout << "{ ";
//        for (const auto& b : *ws) {
//            std::cout << b << " ";
//        }
//        std::cout << "}\n";
//        std::cout << slice(/*slc*/ ai, *(ws[i])) << "\n";
//    }
//}

//TEST(arrnd_fast_indexer, simple_forward_backward_iterations)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{2, 3, 4, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims, dims + 4);
//
//    const std::int64_t expected_inds_list[48]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
//        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47};
//    const std::int64_t expected_generated_subs{48};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_fast_indexer gen(hdr);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_fast_indexer, simple_backward_forward_iterations)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims, dims + 3);
//
//    const std::int64_t expected_inds_list[6]{5, 4, 3, 2, 1, 0};
//    const std::int64_t expected_generated_subs{6};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_fast_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        --gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (++gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_fast_indexer, simple_forward_backward_iterations_with_steps_bigger_than_one)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims, dims + 3);
//
//    const std::int64_t expected_inds_list[6]{0, 2, 4};
//    const std::int64_t expected_generated_subs{3};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_fast_indexer gen(hdr);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//        ++generated_subs_counter;
//        gen += 2;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while ((gen = gen - 2)) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], *gen);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_fast_indexer, forward_backward_iterations_by_specific_major_axis)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims, dims + 3);
//
//    const std::int64_t expected_inds_list[][6]{{0, 1, 2, 3, 4, 5},
//
//        {0, 1, 2, 3, 4, 5},
//
//        {0, 2, 4, 1, 3, 5}};
//    const std::int64_t expected_generated_subs{6};
//
//    for (std::int64_t axis = 0; axis <= 2; ++axis) {
//        std::int64_t generated_subs_counter{0};
//        arrnd_fast_indexer gen(hdr, axis);
//
//        while (gen) {
//            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
//            ++generated_subs_counter;
//            ++gen;
//        }
//        EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//        while (--gen) {
//            --generated_subs_counter;
//            EXPECT_EQ(expected_inds_list[axis][generated_subs_counter], *gen);
//        }
//        EXPECT_EQ(0, generated_subs_counter);
//    }
//}
//
//TEST(arrnd_fast_indexer, random_access)
//{
//    using namespace oc;
//
//    const std::int64_t dims[]{3, 1, 2}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims, dims + 3);
//
//    const std::int64_t expected_inds_list[6]{0, 5, 4, 1, 2, 3};
//
//    arrnd_fast_indexer gen(hdr, oc::arrnd_iterator_position::rbegin);
//
//    EXPECT_EQ(expected_inds_list[0], gen[0]);
//    EXPECT_EQ(expected_inds_list[1], gen[5]);
//    EXPECT_EQ(expected_inds_list[2], gen[4]);
//    EXPECT_EQ(expected_inds_list[3], gen[1]);
//    EXPECT_EQ(expected_inds_list[4], gen[2]);
//    EXPECT_EQ(expected_inds_list[5], gen[3]);
//}

// TODO: add arrnd_window_slider type tests (event though is being tested via arrnd type functionality) 

//TEST(arrnd_axis_ranger, simple_forward_backward_iterations)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[3]{interval<>{0, 1}, interval<>{1, 2}, interval<>{2, 3}};
//    const std::int64_t expected_generated_subs{3};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, simple_forward_backward_iterations_with_interval_width_bigger_than_one_in_contained_window)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 6}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[6]{interval<>{0, 3}, interval<>{1, 4}, interval<>{2, 5}, interval<>{3, 6}};
//    const std::int64_t expected_generated_subs{4};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2, interval<>{0, 2}, true);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, simple_forward_backward_iterations_with_interval_width_bigger_than_one_in_none_contained_window)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 6}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[6]{
//        interval<>{0, 3}, interval<>{0, 4}, interval<>{0, 5}, interval<>{1, 6}, interval<>{2, 6}, interval<>{3, 6}};
//    const std::int64_t expected_generated_subs{6};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2, interval<>{-2, 2}, false);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        ++gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (--gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, simple_backward_forward_iterations)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[3]{interval<>{2, 3}, interval<>{1, 2}, interval<>{0, 1}};
//    const std::int64_t expected_generated_subs{3};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2, interval<>{0, 0}, true, arrnd_iterator_position::rbegin);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        --gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (++gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, simple_backward_forward_iterations_with_interval_width_bigger_than_one)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 6}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[4]{interval<>{3, 6}, interval<>{2, 5}, interval<>{1, 4}, interval<>{0, 3}};
//    const std::int64_t expected_generated_subs{4};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2, interval<>{0, 2}, true, arrnd_iterator_position::rbegin);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        --gen;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while (++gen) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, simple_forward_backward_iterations_with_steps_bigger_than_one)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[2]{interval<>{0, 1}, interval<>{2, 3}};
//    const std::int64_t expected_generated_subs{2};
//
//    std::int64_t generated_subs_counter{0};
//    arrnd_axis_ranger gen(hdr, 2);
//
//    while (gen) {
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//        ++generated_subs_counter;
//        gen += 2;
//    }
//    EXPECT_EQ(expected_generated_subs, generated_subs_counter);
//
//    while ((gen = gen - 2)) {
//        --generated_subs_counter;
//        EXPECT_EQ(expected_inds_list[generated_subs_counter], (*gen)[2]);
//    }
//    EXPECT_EQ(0, generated_subs_counter);
//}
//
//TEST(arrnd_axis_ranger, random_access)
//{
//    using namespace oc;
//    using namespace oc::details;
//
//    const std::int64_t dims[]{2, 1, 3}; // strides = {2, 2, 1}
//    arrnd_header hdr(dims);
//
//    const interval<> expected_inds_list[3]{interval<>{0, 1}, interval<>{2, 3}, interval<>{1, 2}};
//
//    arrnd_axis_ranger gen(hdr, 2);
//
//    EXPECT_EQ(expected_inds_list[0], gen[0][2]);
//    EXPECT_EQ(expected_inds_list[1], gen[2][2]);
//    EXPECT_EQ(expected_inds_list[2], gen[1][2]);
//}

TEST(arrnd_test, indexer_deprecated)
{
    using namespace oc;

    arrnd<int> arr({3, 1, 2});
    oc::experimental::arrnd_indexer<typename arrnd<int>::header_type> indexer(move(arr.header(), 2, 0));

    std::vector<int> indices;
    for (; indexer; ++indexer) {
        indices.push_back(*indexer);
    }
    std::vector<int> result{0, 2, 4, 1, 3, 5};

    EXPECT_EQ(result, indices);
}

TEST(arrnd_test, ranger_deprecated)
{
    using namespace oc;

    //auto ranger = arrnd<int>({3, 1, 2}).ranger(2);
    arrnd<int> arr({3, 1, 2});
    arrnd<int>::ranger_type ranger(arr.header(), 2);
    std::vector<interval<>> fisrt_ranges{{0, 3}, {0, 1}, {0, 1}};

    EXPECT_TRUE(std::equal(fisrt_ranges.cbegin(), fisrt_ranges.cend(), (*ranger).cbegin()));
}

TEST(arrnd_test, iterators_and_inserters)
{
    using namespace oc;

    arrnd<int> earr;
    EXPECT_EQ(earr.begin(), earr.end());
    EXPECT_EQ(earr.cbegin(), earr.cend());
    EXPECT_EQ(earr.rbegin(), earr.rend());
    EXPECT_EQ(earr.crbegin(), earr.crend());
    EXPECT_EQ(earr.begin(arrnd_returned_slice_iterator_tag{}), earr.end(arrnd_returned_slice_iterator_tag{}));
    EXPECT_EQ(earr.cbegin(arrnd_returned_slice_iterator_tag{}), earr.cend(arrnd_returned_slice_iterator_tag{}));
    EXPECT_EQ(earr.rbegin(arrnd_returned_slice_iterator_tag{}), earr.rend(arrnd_returned_slice_iterator_tag{}));
    EXPECT_EQ(earr.crbegin(arrnd_returned_slice_iterator_tag{}), earr.crend(arrnd_returned_slice_iterator_tag{}));

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

    std::transform(arr1.cbegin(), ++(arr1.cbegin()), arr2[{{1, 2, 2}, {0, 1}, {1, 2}}].rbegin(), [](auto a) {
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
        std::copy(arr.cbegin(axis, arrnd_returned_element_iterator_tag{}),
            arr.cend(axis, arrnd_returned_element_iterator_tag{}), std::back_inserter(res));

        EXPECT_TRUE(std::equal(inds[axis].begin(), inds[axis].end(), res.begin()));
    }

    std::initializer_list<std::int64_t> order = {2, 1, 0};

    std::vector<int> res;
    std::copy(arr.cbegin(order), arr.cend(order), std::back_inserter(res));

    EXPECT_TRUE(std::equal(inds[3].begin(), inds[3].end(), res.begin()));

    // axis iterators
    std::for_each(arr.begin(arrnd_returned_slice_iterator_tag{}), arr.end(arrnd_returned_slice_iterator_tag{}),
        [](const auto& sa) {
            auto exsa = sa[interval<std::int64_t>{0, 1}];
            std::for_each(exsa.rbegin(arrnd_returned_slice_iterator_tag{}),
                exsa.rend(arrnd_returned_slice_iterator_tag{}), [](auto& sa) {
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

    // free arrnd iterator functions compilation
    {
        arrnd<int> arr{};

        begin(arr);
        cbegin(arr);
        end(arr);
        cend(arr);
        rbegin(arr);
        crbegin(arr);
        rend(arr);
        crend(arr);

        begin(arr, arrnd_returned_element_iterator_tag{});
        cbegin(arr, arrnd_returned_element_iterator_tag{});
        end(arr, arrnd_returned_element_iterator_tag{});
        cend(arr, arrnd_returned_element_iterator_tag{});
        rbegin(arr, arrnd_returned_element_iterator_tag{});
        crbegin(arr, arrnd_returned_element_iterator_tag{});
        rend(arr, arrnd_returned_element_iterator_tag{});
        crend(arr, arrnd_returned_element_iterator_tag{});
    }
}

TEST(arrnd_test, complex_type_array_ordering_compilation)
{
    using namespace oc;

    arrnd<std::complex<double>> arrc1{};
    arrnd<std::complex<double>> arrc2{};
    arrnd<double> arrd{};

    auto cond = (arrc1 < arrc2) && (arrc1 <= arrc2) && (arrc1 > arrc2) && (arrc1 >= arrc2) && (arrc1 < arrd)
        && (arrc1 <= arrd) && (arrc1 > arrd) && (arrc1 >= arrd) && (arrd < arrc1) && (arrd <= arrc1) && (arrd > arrc1)
        && (arrd >= arrc1) && close(arrc1, arrc2) && close(arrc1, arrd) && close(arrd, arrc1);
}

TEST(arrnd_test, zip)
{
    using namespace oc;

    // collect vector of tuples from two arrays in reverse iteration
    {
        std::vector<std::tuple<int, int>> pairs;

        arrnd<int> arr1({3, 2}, {1, 2, 3, 4, 5, 6});
        arrnd<int> arr2({6, 1}, {1, 2, 3, 4, 5, 6});

        auto z = zip(zipped_cont(arr1, 1, arrnd_returned_element_iterator_tag{}), zipped_cont(arr2));

        std::for_each(z.rbegin(), z.rend(), [&pairs](const auto& t) {
            auto [a, b] = t;
            pairs.push_back(std::make_tuple(a, b));
        });

        EXPECT_EQ(pairs,
            (std::vector{std::make_tuple(6, 6), std::make_tuple(4, 5), std::make_tuple(2, 4), std::make_tuple(5, 3),
                std::make_tuple(3, 2), std::make_tuple(1, 1)}));
    }

    // reduce sum of array only on even indices
    {
        std::vector<int> inds{0, 1, 2, 3, 4, 5};
        arrnd<int> vals({3, 2}, {1, 2, 3, 4, 5, 6});

        auto z = zip(zipped_cont(inds), zipped_cont(vals));

        auto [_, sum] = std::reduce(z.begin(), z.end(), std::make_tuple(0, 0), [](std::tuple<int, int> acc, auto t) {
            auto [ind, val] = t;
            return std::make_tuple(0, ind % 2 == 0 ? std::get<1>(acc) + val : std::get<1>(acc));
        });

        EXPECT_EQ(sum, 9);
    }

    // sort vector and array by indices
    {
        std::vector<int> indices{0, 2, 1, 3, 5, 4};
        std::vector<std::string> vec{"a", "b", "c", "d", "e", "f"};
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});

        auto z = zip(zipped_cont(indices), zipped_cont(vec), zipped_cont(arr));

        std::sort(z.begin(), z.end(), [](const auto& a, const auto& b) {
            return std::get<0>(a) < std::get<0>(b);
        });

        EXPECT_EQ(vec, (std::vector<std::string>{"a", "c", "b", "d", "f", "e"}));

        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 2}, {1, 3, 2, 4, 6, 5})));
    }

    // using reorder, sort an array by indices
    {
        std::vector<int> order{0, 2, 1, 3, 5, 4};
        arrnd<int> arr({3, 2}, {1, 2, 3, 4, 5, 6});

        auto res = arr.reorder(order);

        EXPECT_TRUE(all_equal(res, arrnd<int>({3, 2}, {1, 3, 2, 4, 6, 5})));
    }

    // using reorder, sort an array by axis and indices
    {
        std::vector<int> order{0, 2, 3, 1};
        arrnd<int> arr({4, 2}, {1, 2, 3, 4, 5, 6, 7, 8});

        auto res = arr.reorder(0, order);

        EXPECT_TRUE(all_equal(res, arrnd<int>({4, 2}, {1, 2, 7, 8, 3, 4, 5, 6})));
    }

    // zip using both container and iterator arguments
    {
        std::vector<int> res = {5, 8, 9, 8, 5};

        std::vector<int> vals1 = {1, 2, 3, 4, 5};
        std::vector<int> vals2 = {5, 4, 3, 2, 1};

        std::vector<int> valst;

        valst.clear();
        auto z1 = zip(zipped_cont(vals1), zipped_iter(vals2.begin(), vals2.end()));
        std::transform(z1.begin(), z1.end(), std::back_inserter(valst), [](auto t) {
            return std::get<0>(t) * std::get<1>(t);
        });
        EXPECT_EQ(valst, res);

        valst.clear();
        auto z2 = zip(zipped_cont(vals1), zipped_iter(vals2.rbegin(), vals2.rend()));
        std::transform(z2.rbegin(), z2.rend(), std::back_inserter(valst), [](auto t) {
            return std::get<0>(t) * std::get<1>(t);
        });
        EXPECT_EQ(valst, res);
    }

    //std::vector<int> indices{0, 2, 1, 3, 5, 4};
    //std::vector<int> values{10, 20, 30, 40, 50, 60};
    //arrnd<int> arr({3, 1, 2}, {10, 20, 30, 40, 50, 60});
    //
    //auto expanded = arr.expand(0);

    //for (auto [i, v] : zip(zipped_cont{indices}, zipped_cont{expanded, 0, arrnd_returned_element_iterator_tag{}})) {
    //    std::cout << i << ", " << v << "\n";
    //}
    //zip pack(zipped_cont{indices}, zipped_cont{expanded, 0, arrnd_returned_element_iterator_tag{}});
    //auto t1 = begin(pack);
    ////swap(*t1, *t1);
    //std::sort(pack.begin(), pack.end(), [](auto a, auto b) {
    //    return std::get<0>(a) < std::get<0>(b);
    //});

    //for (auto [i, v] : zip(zipped_cont{indices}, zipped_cont{expanded, 0, arrnd_returned_element_iterator_tag{}})) {
    //    std::cout << i << ", " << v << "\n";
    //    /*i = 10;
    //    v[{0,0}] = 100;*/
    //}

    //zip z(zipped_cont{indices}, zipped_cont{arr, 0, arrnd_returned_slice_iterator_tag{}});

    //std::for_each(z.rbegin(), z.rend(), [](const auto& t) {
    //    auto [i, v] = t;
    //    //std::cout << i << ", " << v << "\n";
    //});
}

TEST(arrnd_test, basic_sorting_using_std_sort_and_iterators)
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

TEST(arrnd_test, sort)
{
    using namespace oc;

    {
        auto dummy_less = [](const auto&, const auto&) {
            return true;
        };
        EXPECT_TRUE(all_equal(arrnd<int>().sort(dummy_less), arrnd<int>()));
        EXPECT_TRUE(all_equal(arrnd<int>().sort(0, dummy_less), arrnd<int>()));
        EXPECT_TRUE(all_equal(arrnd<arrnd<int>>().sort /*<0>*/ (dummy_less), arrnd<arrnd<int>>()));
        EXPECT_TRUE(all_equal(
            /*sort<1>(arrnd<arrnd<int>>(), dummy_less)*/ transform<0>(arrnd<arrnd<int>>(),
                [dummy_less](const auto& val) {
                    return val.sort(dummy_less);
                }),
            arrnd<arrnd<int>>()));
        EXPECT_TRUE(all_equal(arrnd<arrnd<int>>() .sort /*<0>*/ (0, dummy_less), arrnd<arrnd<int>>()));
        EXPECT_TRUE(all_equal(
            /*sort<1>(arrnd<arrnd<int>>(), 0, dummy_less)*/ transform<0>(arrnd<arrnd<int>>(),
                [dummy_less](const auto& val) {
                    return val.sort(0, dummy_less);
                }),
            arrnd<arrnd<int>>()));
    }

    arrnd<arrnd<int>> iarr({1, 2},
        {arrnd<int>({6, 4}, {2, 5, 4, 7, 6, 3, 9, 2, 1, 3, 5, 5, 8, 4, 3, 5, 7, 4, 2, 8, 5, 6, 2, 3}),
            arrnd<int>({3, 1, 2}, {4, 6, 8, 2, 3, 6})});

    auto sum_less = [](const auto& lhs, const auto& rhs) {
        return std::reduce(lhs.cbegin(), lhs.cend(), 0, std::plus<>{})
            < std::reduce(rhs.cbegin(), rhs.cend(), 0, std::plus{});
    };

    // standard sort
    {
        EXPECT_TRUE(all_equal(/*is_sorted(iarr, std::less<>{})*/
            transform<0>(iarr,
                [](const auto& val) {
                    return val.is_sorted(std::less<>{});
                }),
            arrnd<bool>({1, 2}, {false, false})));
        //auto sarr1 = sort(iarr, std::less<>{});
        auto sarr1 = transform<0>(iarr, [](const auto& val) {
            return val.sort(std::less<>{});
        });
        EXPECT_TRUE(all_equal(/*is_sorted(sarr1, std::less<>{})*/
            transform<0>(sarr1,
                [](const auto& val) {
                    return val.is_sorted(std::less<>{});
                }),
            arrnd<bool>({1, 2}, {true, true})));
        //std::cout << sarr1 << "\n";
        EXPECT_TRUE(all_equal(sarr1,
            arrnd<arrnd<int>>({1, 2},
                {arrnd<int>({6, 4}, {1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 7, 7, 8, 8, 9}),
                    arrnd<int>({3, 1, 2}, {2, 3, 4, 6, 6, 8})})));

        EXPECT_FALSE(iarr.is_sorted/*<0>*/(sum_less));
        auto sarr2 = iarr.sort/*<0>*/(sum_less);
        EXPECT_TRUE(sarr2.is_sorted/*<0>*/(sum_less));
        EXPECT_TRUE(all_equal(sarr2,
            arrnd<arrnd<int>>({1, 2},
                {arrnd<int>({3, 1, 2}, {4, 6, 8, 2, 3, 6}),
                    arrnd<int>({6, 4}, {2, 5, 4, 7, 6, 3, 9, 2, 1, 3, 5, 5, 8, 4, 3, 5, 7, 4, 2, 8, 5, 6, 2, 3})})));
    }

    // sort by axis
    {
        EXPECT_TRUE(all_equal(/*is_sorted(iarr, 0, sum_less)*/
            transform<0>(iarr,
                [sum_less](const auto& val) {
                    return val.is_sorted(0, sum_less);
                }),
            arrnd<bool>({1, 2}, {false, false})));
        //auto sarr1 = sort(iarr, 0, sum_less);
        auto sarr1 = transform<0>(iarr, [sum_less](const auto& val) {
            return val.sort(0, sum_less);
        });
        EXPECT_TRUE(all_equal(/*is_sorted(sarr1, 0, sum_less)*/
            transform<0>(sarr1,
                [sum_less](const auto& val) {
                    return val.is_sorted(0, sum_less);
                }),
            arrnd<bool>({1, 2}, {true, true})));
        EXPECT_TRUE(all_equal(sarr1,
            arrnd<arrnd<int>>({1, 2},
                {arrnd<int>({6, 4}, {1, 3, 5, 5, 5, 6, 2, 3, 2, 5, 4, 7, 6, 3, 9, 2, 8, 4, 3, 5, 7, 4, 2, 8}),
                    arrnd<int>({3, 1, 2}, {3, 6, 4, 6, 8, 2})})));

        EXPECT_TRUE(all_equal(/*is_sorted(iarr, 1, sum_less)*/
            transform<0>(iarr,
                [sum_less](const auto& val) {
                    return val.is_sorted(1, sum_less);
                }),
            arrnd<bool>({1, 2}, {false, true})));
        //auto sarr2 = sort(iarr, 1, sum_less);
        auto sarr2 = transform<0>(iarr, [sum_less](const auto& val) {
            return val.sort(1, sum_less);
        });
        EXPECT_TRUE(all_equal(/*is_sorted(sarr2, 1, sum_less)*/
            transform<0>(sarr2,
                [sum_less](const auto& val) {
                    return val.is_sorted(1, sum_less);
                }),
            arrnd<bool>({1, 2}, {true, true})));
        EXPECT_TRUE(all_equal(sarr2,
            arrnd<arrnd<int>>({1, 2},
                {arrnd<int>({6, 4}, {5, 4, 2, 7, 3, 9, 6, 2, 3, 5, 1, 5, 4, 3, 8, 5, 4, 2, 7, 8, 6, 2, 5, 3}),
                    arrnd<int>({3, 1, 2}, {4, 6, 8, 2, 3, 6})})));

        EXPECT_FALSE(iarr.is_sorted/*<0>*/(1, [sum_less](const auto& lhs, const auto& rhs) {
            return sum_less(lhs[{0}], rhs[{0}]);
        }));
        auto sarr3 = iarr.sort/*<0>*/(1, [sum_less](const auto& lhs, const auto& rhs) {
            return sum_less(lhs[{0}], rhs[{0}]);
        });
        EXPECT_TRUE(sarr3.is_sorted/*<0>*/(1, [sum_less](const auto& lhs, const auto& rhs) {
            return sum_less(lhs[{0}], rhs[{0}]);
        }));
        EXPECT_TRUE(all_equal(sarr3,
            arrnd<arrnd<int>>({1, 2},
                {arrnd<int>({3, 1, 2}, {4, 6, 8, 2, 3, 6}),
                    arrnd<int>({6, 4}, {2, 5, 4, 7, 6, 3, 9, 2, 1, 3, 5, 5, 8, 4, 3, 5, 7, 4, 2, 8, 5, 6, 2, 3})})));
    }
}

TEST(arrnd_test, expand)
{
    using namespace oc;

    EXPECT_TRUE(all_equal(arrnd<int>().expand(0), arrnd<arrnd<int>>()));

    arrnd<arrnd<int>> narr({2, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})});

    EXPECT_TRUE(all_equal(/*expand(narr, 0)*/ transform<0>(narr,
                              [](const auto& val) {
                                  return val.expand(0);
                              }),
        arrnd<arrnd<arrnd<int>>>({2, 1},
            {arrnd<arrnd<int>>({3, 1, 1},
                 {arrnd<int>({1, 1, 2}, {1, 2}), arrnd<int>({1, 1, 2}, {3, 4}), arrnd<int>({1, 1, 2}, {5, 6})}),
                arrnd<arrnd<int>>({2, 1}, {arrnd<int>({1, 3}, {1, 2, 3}), arrnd<int>({1, 3}, {4, 5, 6})})})));

    EXPECT_TRUE(all_equal(narr.expand/*<0>*/(0),
        arrnd<arrnd<arrnd<int>>>({2, 1},
            {arrnd<arrnd<int>>({1, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})}),
                arrnd<arrnd<int>>({1, 1}, {arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})})})));

    {
        arrnd<int> iarr({6, 1, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});

        EXPECT_TRUE(all_equal(iarr.expand(0, 1),
            arrnd<arrnd<int>>({1, 1, 1}, {arrnd<int>({6, 1, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12})})));

        EXPECT_TRUE(all_equal(iarr.expand(0, 2),
            arrnd<arrnd<int>>(
                {2, 1, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({3, 1, 2}, {7, 8, 9, 10, 11, 12})})));

        EXPECT_TRUE(all_equal(iarr.expand(0, 3),
            arrnd<arrnd<int>>({3, 1, 1},
                {arrnd<int>({2, 1, 2}, {1, 2, 3, 4}), arrnd<int>({2, 1, 2}, {5, 6, 7, 8}),
                    arrnd<int>({2, 1, 2}, {9, 10, 11, 12})})));

        EXPECT_TRUE(all_equal(iarr.expand(0, 4),
            arrnd<arrnd<int>>({4, 1, 1},
                {arrnd<int>({2, 1, 2}, {1, 2, 3, 4}), arrnd<int>({2, 1, 2}, {5, 6, 7, 8}),
                    arrnd<int>({1, 1, 2}, {9, 10}), arrnd<int>({1, 1, 2}, {11, 12})})));

        EXPECT_TRUE(all_equal(iarr.expand(0, 5),
            arrnd<arrnd<int>>({5, 1, 1},
                {arrnd<int>({2, 1, 2}, {1, 2, 3, 4}), arrnd<int>({1, 1, 2}, {5, 6}), arrnd<int>({1, 1, 2}, {7, 8}),
                    arrnd<int>({1, 1, 2}, {9, 10}), arrnd<int>({1, 1, 2}, {11, 12})})));

        EXPECT_TRUE(all_equal(iarr.expand(0, 6),
            arrnd<arrnd<int>>({6, 1, 1},
                {arrnd<int>({1, 1, 2}, {1, 2}), arrnd<int>({1, 1, 2}, {3, 4}), arrnd<int>({1, 1, 2}, {5, 6}),
                    arrnd<int>({1, 1, 2}, {7, 8}), arrnd<int>({1, 1, 2}, {9, 10}), arrnd<int>({1, 1, 2}, {11, 12})})));
    }

    // expand function e.g. can be used for parallel array processing
    {
        // e.g. apply function on array values

        arrnd<int> res(
            {12, 1, 2}, {2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48});

        auto multiply = [](int value/*, int factor*/) {
            return value * 2/*factor*/;
        };

        // serial
        {
            arrnd<int> arr(
                {12, 1, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
            apply(arr, multiply/*, 2*/);
            EXPECT_TRUE(all_equal(res, arr));
        }

        // parallel
        {
            std::vector<std::thread> threads(std::thread::hardware_concurrency());

            arrnd<int> arr(
                {12, 1, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});
            auto works = arr.expand(0, std::ssize(threads));

            for (auto i = 0; i < std::ssize(threads); ++i) {
                threads[i] = std::thread([&multiply, &works, i]() {
                    apply(works[i], multiply/*, 2*/);
                });
            }

            std::for_each(threads.begin(), threads.end(), [](std::thread& t) {
                t.join();
            });

            EXPECT_TRUE(all_equal(res, arr));
        }
    }
}

TEST(arrnd_test, access_slice_and_track_dimensions)
{
    using namespace oc;

    oc::arrnd<int> arr({3, 4, 3},
        {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
            31, 32, 33, 34, 35, 36});

    EXPECT_TRUE(all_equal(arr(interval<>{1, 3}, 0)(interval<>{0, 2}, 1)(interval<>{0, 2}, 2),
        arr[{interval<>{1, 3}, interval<>{0, 2}, interval<>{0, 2}}]));

    EXPECT_FALSE(all_equal(arr[interval<>{1, 3}][interval<>{0, 2}][interval<>{0, 2}],
        arr[{interval<>{1, 3}, interval<>{0, 2}, interval<>{0, 2}}]));
}

TEST(arrnd_header_test, reordering_slicing_and_array_memory_buffer_continuity)
{
    using namespace oc;

    //arrnd<int> arr({3, 4, 3},
    //    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    //        31, 32, 33, 34, 35});
    arrnd_info hdr({3, 4, 3});
    EXPECT_TRUE(!issliced(hdr) && !istransposed(hdr) && iscontinuous(hdr));
    //EXPECT_TRUE(hdr.order().empty());

    //std::cout << arr << "\n\n";

    //auto header = arr.header();

    std::vector<int> order{1, 2, 0};

    auto rhdr = transpose(hdr, order);
    EXPECT_TRUE(!issliced(rhdr) && istransposed(rhdr) && iscontinuous(rhdr));
    //EXPECT_TRUE(std::equal(rhdr.order().cbegin(), rhdr.order().cend(), order.cbegin(), order.cend()));

    //std::cout << arr << "\n\n";
    // {4, 3, 3}
    auto shdr1 = slice(hdr, {interval<>::at(2), interval<>::full(), interval<>::at(1)});
    EXPECT_TRUE(issliced(shdr1) && !istransposed(shdr1) && !iscontinuous(shdr1));
    //EXPECT_TRUE(shdr1.order().empty());

    auto shdr2 = slice(rhdr, {interval<>::at(2), interval<>::full(), interval<>::at(1)});
    EXPECT_TRUE(issliced(shdr2) && istransposed(shdr2) && iscontinuous(shdr2));
    //EXPECT_TRUE(std::equal(shdr2.order().cbegin(), shdr2.order().cend(), order.cbegin(), order.cend()));

    //arr.header() = arr.header().subheader({interval<>::at(2), interval<>::full(), interval<>::at(1)});
    //std::cout << arr << "\n\n";
}

TEST(arrnd_test, exclude_and_merge)
{
    using namespace oc;

    // empty
    {
        arrnd<int> arr;

        EXPECT_TRUE(arr.exclude({}, {100}).empty());

        EXPECT_TRUE(arr.exclude({}, {100}).merge().empty());
    }

    // 1d
    {
        arrnd<int> arr({6}, {1, 2, 3, 4, 5, 6});

        EXPECT_TRUE(all_equal(
            arr.exclude({}, {2}), arrnd<arrnd<int>>({2}, {arrnd<int>({2}, {1, 2}), arrnd<int>({3}, {4, 5, 6})})));
        EXPECT_TRUE(all_equal(arr.exclude({}, {5}), arrnd<arrnd<int>>({1}, {arrnd<int>({5}, {1, 2, 3, 4, 5})})));

        auto exc = arr.exclude({}, {0});
        EXPECT_TRUE(all_equal(exc, arrnd<arrnd<int>>({1}, {arrnd<int>({5}, {2, 3, 4, 5, 6})})));
        exc[0](0) = 100;
        EXPECT_EQ(100, arr[1]);
    }

    // 2d
    {
        arrnd<arrnd<int>> arr({1},
            {arrnd<int>(
                {6, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})});

        //auto exc = exclude(arr, {}, {2});
        auto exc = transform<0>(arr, [](const auto& val) {
            return val.exclude({}, {2});
        });

        EXPECT_TRUE(all_equal(exc,
            arrnd<arrnd<arrnd<int>>>({1},
                arrnd<arrnd<int>>({2, 2},
                    {arrnd<int>({2, 2}, {1, 2, 5, 6}), arrnd<int>({2, 1}, {4, 8}),
                        arrnd<int>({3, 2}, {13, 14, 17, 18, 21, 22}), arrnd<int>({3, 1}, {16, 20, 24})}))));

        EXPECT_TRUE(all_equal(/*merge(exc)*/
            transform<0>(exc,
                [](const auto& val) {
                    return val.merge();
                }),
            arrnd<arrnd<int>>({1}, {arrnd<int>({5, 3}, {1, 2, 4, 5, 6, 8, 13, 14, 16, 17, 18, 20, 21, 22, 24})})));
    }

    // 3d
    {
        arrnd<int> arr({3, 4, 3},
            {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                30, 31, 32, 33, 34, 35, 36});

        auto exc = arr.exclude({0, 2}, {1});

        EXPECT_TRUE(all_equal(exc,
            arrnd<arrnd<int>>({2, 1, 2},
                {arrnd<int>({1, 4, 1}, {1, 4, 7, 10}), arrnd<int>({1, 4, 1}, {3, 6, 9, 12}),
                    arrnd<int>({1, 4, 1}, {25, 28, 31, 34}), arrnd<int>({1, 4, 1}, {27, 30, 33, 36})})));

        EXPECT_TRUE(
            all_equal(exc.merge(), arrnd<int>({2, 4, 2}, {1, 3, 4, 6, 7, 9, 10, 12, 25, 27, 28, 30, 31, 33, 34, 36})));
    }

    // multiple indices
    {
        arrnd<int> arr({12}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});

        EXPECT_TRUE(all_equal(arr.exclude({}, {0, 4, 11}),
            arrnd<arrnd<int>>({2}, {arrnd<int>({3}, {2, 3, 4}), arrnd<int>({6}, {6, 7, 8, 9, 10, 11})})));
    }
}

TEST(arrnd_test, split)
{
    using namespace oc;

    // empty
    {
        arrnd<int> arr;

        EXPECT_TRUE(arr.split({}, {100}).empty());
    }

    // 1d
    {
        arrnd<int> arr({6}, {1, 2, 3, 4, 5, 6});

        EXPECT_TRUE(all_equal(
            arr.split({}, {2}), arrnd<arrnd<int>>({2}, {arrnd<int>({2}, {1, 2}), arrnd<int>({4}, {3, 4, 5, 6})})));
        EXPECT_TRUE(all_equal(
            arr.split({}, {5}), arrnd<arrnd<int>>({2}, {arrnd<int>({5}, {1, 2, 3, 4, 5}), arrnd<int>({1}, {6})})));

        auto exc = arr.split({}, {0});
        EXPECT_TRUE(all_equal(exc, arrnd<arrnd<int>>({1}, {arrnd<int>({6}, {1, 2, 3, 4, 5, 6})})));
        exc[0](0) = 100;
        EXPECT_EQ(100, arr[0]);
    }

    // 2d
    {
        arrnd<arrnd<int>> arr({1},
            {arrnd<int>(
                {6, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})});

        EXPECT_TRUE(all_equal(/*split(arr, {}, {2})*/
            transform<0>(arr,
                [](const auto& val) {
                    return val.split({}, {2});
                }),
            arrnd<arrnd<arrnd<int>>>({1},
                arrnd<arrnd<int>>({2, 2},
                    {arrnd<int>({2, 2}, {1, 2, 5, 6}), arrnd<int>({2, 2}, {3, 4, 7, 8}),
                        arrnd<int>({4, 2}, {9, 10, 13, 14, 17, 18, 21, 22}),
                        arrnd<int>({4, 2}, {11, 12, 15, 16, 19, 20, 23, 24})}))));
    }

    // 3d
    {
        oc::arrnd<int> arr({3, 4, 3},
            {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                30, 31, 32, 33, 34, 35, 36});

        EXPECT_TRUE(all_equal(arr.split({}, {2}),
            arrnd<arrnd<int>>({2, 2, 2},
                {arrnd<int>({2, 2, 2}, {1, 2, 4, 5, 13, 14, 16, 17}), arrnd<int>({2, 2, 1}, {3, 6, 15, 18}),
                    arrnd<int>({2, 2, 2}, {7, 8, 10, 11, 19, 20, 22, 23}), arrnd<int>({2, 2, 1}, {9, 12, 21, 24}),
                    arrnd<int>({1, 2, 2}, {25, 26, 28, 29}), arrnd<int>({1, 2, 1}, {27, 30}),
                    arrnd<int>({1, 2, 2}, {31, 32, 34, 35}), arrnd<int>({1, 2, 1}, {33, 36})})));
    }

    // multiple indices
    {
        arrnd<int> arr({6, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});

        std::vector<int> axes{1};
        int inds[]{0, 2, 3};

        EXPECT_TRUE(all_equal(arr.split(axes, inds),
            arrnd<arrnd<int>>({1, 3},
                {arrnd<int>({6, 2}, {1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22}),
                    arrnd<int>({6, 1}, {3, 7, 11, 15, 19, 23}), arrnd<int>({6, 1}, {4, 8, 12, 16, 20, 24})})));
    }

    // specific slices division
    {
        arrnd<int> arr({6, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});

        EXPECT_TRUE(all_equal(arr.split({0}, 2),
            arrnd<arrnd<int>>({2, 1},
                {arrnd<int>({3, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}),
                    arrnd<int>({3, 4}, {13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})})));
    }
}

TEST(arrnd_test, pages)
{
    using namespace oc;

    {
        //EXPECT_TRUE(all_equal(pages(arrnd<int>{}, 100), arrnd<arrnd<int>>{})); // assertion failure
    }

    {
        // already a page
        //arrnd<int> mat({2, 3}, {1, 2, 3, 4, 5, 6});
        //auto p = pages(mat);
        //EXPECT_TRUE(all_equal(p, arrnd<arrnd<int>>({1}, {arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})})));
        //p[0][0] = 100;
        //EXPECT_EQ(100, mat[0]);
    }

    {
        arrnd<int> arr({3, 1, 1, 2}, {1, 2, 3, 4, 5, 6});
        auto p = arr.pages(2);
        EXPECT_TRUE(all_equal(p,
            arrnd<arrnd<int>>(
                {3, 1}, {arrnd<int>({1, 2}, {1, 2}), arrnd<int>({1, 2}, {3, 4}), arrnd<int>({1, 2}, {5, 6})})));
        //EXPECT_TRUE(all_equal(pages(arr, false),
        //    arrnd<arrnd<int>>({3, 1},
        //        {arrnd<int>({1, 1, 1, 2}, {1, 2}), arrnd<int>({1, 1, 1, 2}, {3, 4}),
        //            arrnd<int>({1, 1, 1, 2}, {5, 6})})));
        p[0][0] = 100;
        EXPECT_EQ(100, arr[0]);

        auto merged_pages = p.book();

        EXPECT_TRUE(all_equal(merged_pages, arr));
        EXPECT_EQ(100, merged_pages[0]);

        auto p2 = transform(arr, [](int a) {
            return a * 2;
        }).pages();

        EXPECT_TRUE(all_equal(p2,
            arrnd<arrnd<int>>(
                {3, 1}, {arrnd<int>({1, 2}, {200, 4}), arrnd<int>({1, 2}, {6, 8}), arrnd<int>({1, 2}, {10, 12})})));

        auto merged_pages2 = p2.book();

        EXPECT_TRUE(all_equal(merged_pages2, arrnd<int>({3, 1, 1, 2}, {200, 4, 6, 8, 10, 12})));
    }

    {
        //arrnd<arrnd<int>> narr({3, 1, 2},
        //    {arrnd<int>({2}, {1, 2}), arrnd<int>({2}, {3, 4}), arrnd<int>({2}, {5, 6}), arrnd<int>({2}, {7, 8}),
        //        arrnd<int>({2}, {9, 10}), arrnd<int>({2}, {11, 12})});
        //auto p = pages/*<0>*/(narr, 0, 0, true);

        //EXPECT_TRUE(all_equal(p,
        //    arrnd<arrnd<arrnd<int>>>({3},
        //        {arrnd<arrnd<int>>({1, 2}, {arrnd<int>({2}, {1, 2}), arrnd<int>({2}, {3, 4})}),
        //            arrnd<arrnd<int>>({1, 2}, {arrnd<int>({2}, {5, 6}), arrnd<int>({2}, {7, 8})}),
        //            arrnd<arrnd<int>>({1, 2}, {arrnd<int>({2}, {9, 10}), arrnd<int>({2}, {11, 12})})})));
        //p[0][0][0] = 100;
        //EXPECT_EQ(100, narr[0][0]);
    }

    //// e.g. difference between expand and pages functions
    //{
    //    arrnd<int> arr({5, 1, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20});

    //    int axis = 0;
    //    int division = 3;
    //    bool no_axis_norm = false;

    //    auto exp = expand(arr, axis, division, no_axis_norm);

    //    EXPECT_TRUE(all_equal(exp,
    //        arrnd<arrnd<int>>({3, 1, 1},
    //            {arrnd<int>({2, 1, 4}, {1, 2, 3, 4, 5, 6, 7, 8}),
    //                arrnd<int>({2, 1, 4}, {9, 10, 11, 12, 13, 14, 15, 16}), arrnd<int>({1, 1, 4}, {17, 18, 19, 20})})));

    //    auto pgs = pages(arr, axis, division, no_axis_norm);

    //    EXPECT_TRUE(all_equal(pgs,
    //        arrnd<arrnd<int>>({3},
    //            {arrnd<int>({2, 1, 4}, {1, 2, 3, 4, 5, 6, 7, 8}),
    //                arrnd<int>({2, 1, 4}, {9, 10, 11, 12, 13, 14, 15, 16}), arrnd<int>({1, 4}, {17, 18, 19, 20})})));
    //}
}

TEST(arrnd_test, slide)
{
    using namespace oc;

    {
        arrnd<arrnd<int>> arr(
            {1, 2}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({10}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})});

        auto weigted_sum = [](const auto& slice/*, double weight*/) {
            return 0.5/*weight*/ * slice.sum();
        };

        {
            EXPECT_TRUE(
                all_equal(arrnd<arrnd<double>>(), slide/*<0>*/(arrnd<arrnd<int>>(), 0, {-1, 2}, false, weigted_sum/*, 0.5*/)));
        }

        {
            //auto res = slide(arr, 0, {-1, 2}, false, weigted_sum, 0.5);
            auto res = transform<0>(arr, [weigted_sum](const auto& val) {
                return slide(val, 0, {-1, 3}, false, weigted_sum/*, 0.5*/);
            });

            EXPECT_TRUE(all_equal(res,
                arrnd<arrnd<double>>({1, 2},
                    {arrnd<double>({3}, {10.5, 10.5, 9.}),
                        arrnd<double>({10}, {3., 5., 7., 9., 11., 13., 15., 17., 13.5, 9.5})})));
        }

        {
            //auto res = slide(arr, 0, {-1, 2}, true, weigted_sum, 0.5);
            auto res = transform<0>(arr, [weigted_sum](const auto& val) {
                return slide(val, 0, {-1, 2}, true, weigted_sum/*, 0.5*/);
            });
            EXPECT_TRUE(all_equal(res,
                arrnd<arrnd<double>>(
                    {1, 2}, {arrnd<double>({1}, {10.5}), arrnd<double>({8}, {3., 4.5, 6., 7.5, 9., 10.5, 12., 13.5})})));
        }
    }
}

TEST(arrnd_test, accumulate)
{
    using namespace oc;

    {
        arrnd<arrnd<int>> arr(
            {1, 2}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({10}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})});

        auto slice_sum = [](arrnd<int> slice) {
            return slice.sum();
        };

        auto add_prev = [](int acc, int processed) {
            return acc + processed;
        };

        {
            EXPECT_TRUE(all_equal(arrnd<int>(), accumulate(arrnd<int>(), 0, {-1, 3}, false, add_prev, slice_sum)));
        }

        {
            //auto res = accumulate(arr, 0, {-1, 2}, false, add_prev, slice_sum);
            auto res = transform<0>(arr, [add_prev, slice_sum](const auto& val) {
                return accumulate(val, 0, {-1, 3}, false, add_prev, slice_sum);
            });

            EXPECT_TRUE(all_equal(res,
                arrnd<arrnd<int>>({1, 2},
                    {arrnd<int>({3}, {21, 42, 60}), arrnd<int>({10}, {6, 16, 30, 48, 70, 96, 126, 160, 187, 206})})));
        }

        {
            //auto res = accumulate(arr, 0, {-1, 2}, true, add_prev, slice_sum);
            auto res = transform<0>(arr, [add_prev, slice_sum](const auto& val) {
                return accumulate(val, 0, {-1, 2}, true, add_prev, slice_sum);
            });
            EXPECT_TRUE(all_equal(res,
                arrnd<arrnd<int>>(
                    {1, 2}, {arrnd<int>({1}, {21}), arrnd<int>({8}, {6, 15, 27, 42, 60, 81, 105, 132})})));
        }
    }
}

TEST(arrnd_test, collapse)
{
    using namespace oc;

    // form known array creator
    {
        arrnd<int> arr(
            {2, 3, 2, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});

        auto exp = arr.expand(2);

        auto col = exp.collapse();

        EXPECT_TRUE(all_equal(arr, col));

        arr[0] = 100;
        EXPECT_EQ(100, col[0]);
    }

    // form unknown array creator
    {
        auto exp = arrnd<int>(
            {2, 3, 2, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24})
                       .expand(1, 2);

        auto col = exp.collapse();

        EXPECT_TRUE(
            all_equal(arrnd<int>({2, 3, 2, 2},
                          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24}),
                col));
    }

    // nested array
    {
        arrnd<arrnd<int>> arr(
            {2, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})});

        auto exp = arr.expand/*<1>*/(0);

        auto col = exp.collapse/*<2>*/();

        EXPECT_TRUE(all_equal(arr, col));

        arr[0][0] = 100;
        EXPECT_EQ(100, col[0][0]);
    }

    // nested array
    {
        auto exp = /*<1>*/ arrnd<arrnd<int>>(
            {2, 1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})})
                       .expand(0);

        auto col = exp.collapse/*<2>*/();

        EXPECT_TRUE(all_equal(arrnd<arrnd<int>>({2, 1},
                                  {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}), arrnd<int>({2, 3}, {1, 2, 3, 4, 5, 6})}),
            col));
    }
}

TEST(arrnd_test, browse)
{
    using namespace oc;

    // empty array
    {
        auto res = browse(arrnd<int>{}, 2, [](arrnd<int> page) {
            return page;
        });

        EXPECT_TRUE(all_equal(res, arrnd<int>{}));
    }

    // matrix
    {
        arrnd<int> arr({1, 2}, {1, 2});

        auto res = browse(arr, 2, [](arrnd<int> page) {
            return page.transpose({1, 0});
        });

        EXPECT_TRUE(all_equal(res, arrnd<int>({2, 1}, {1, 2})));
    }

    // apply opration
    {
        arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

        auto res = browse(arr, 2, [](arrnd<int> page) {
            page.apply([](int value) {
                return value * 2;
            });
        });

        EXPECT_TRUE(all_equal(arr, res));
        EXPECT_TRUE(all_equal(arr, arrnd<int>({3, 1, 2}, {2, 4, 6, 8, 10, 12})));
    }
    // nested
    {
        arrnd<arrnd<int>> arr({1}, arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6}));

        //auto res = browse<1>(arr, 2, [](arrnd<int> page) {
        //    page.apply([](int value) {
        //        return value * 2;
        //    });
        //});
        auto res = transform<0>(arr, [](const auto& val) {
            return browse(val, 2, [](arrnd<int> page) {
                page.apply([](int value) {
                    return value * 2;
                });
            });
        });

        EXPECT_TRUE(all_equal(arr, res));
        EXPECT_TRUE(all_equal(arr, arrnd<arrnd<int>>({1}, arrnd<int>({3, 1, 2}, {2, 4, 6, 8, 10, 12}))));
    }

    // transform operation
    {
        arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

        auto res = browse(arr, 2, [](arrnd<int> page) {
            return page.transpose({1, 0});
        });

        EXPECT_TRUE(all_equal(res, arrnd<int>({3, 2, 1}, {1, 2, 3, 4, 5, 6})));
    }
    // nested
    {
        arrnd<arrnd<int>> arr({1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})});

        //auto res = browse<1>(arr, 2, [](arrnd<int> page) {
        //    return page.transpose({1, 0});
        //});
        auto res = transform<0>(arr, [](const auto& val) {
            return browse(val, 2, [](arrnd<int> page) {
                return page.transpose({1, 0});
            });
        });

        EXPECT_TRUE(all_equal(res, arrnd<arrnd<int>>({1}, {arrnd<int>({3, 2, 1}, {1, 2, 3, 4, 5, 6})})));
    }

    // reduce operation
    {
        arrnd<int> arr1({3, 1, 2}, {1, 2, 3, 4, 5, 6});

        auto res1 = browse(arr1, 2, [](arrnd<int> page) {
            return 0.5 * page.sum();
        });

        EXPECT_TRUE(all_equal(res1, arrnd<double>({3, 1}, {1.5, 3.5, 5.5})));

        auto res2 = browse(arr1, 1, [](arrnd<int> page) {
            return 0.5 * page.sum();
        });

        EXPECT_TRUE(all_equal(res2, arrnd<double>({3, 1, 1}, {1.5, 3.5, 5.5})));

        arrnd<int> arr2({2, 3, 1, 2}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});

        auto res3 = browse(arr2, 3, [](arrnd<int> page) {
            return page.sum();
        });

        EXPECT_TRUE(all_equal(res3, arrnd<int>({2, 1}, {21, 57})));
    }
    // nested
    {
        arrnd<arrnd<int>> arr({1}, {arrnd<int>({3, 1, 2}, {1, 2, 3, 4, 5, 6})});

        //auto res = browse<1>(arr, 2, [](arrnd<int> page) {
        //    return 0.5 * page.sum();
        //});
        auto res = transform<0>(arr, [](const auto& val) {
            return browse(val, 2, [](arrnd<int> page) {
                return 0.5 * page.sum();
            });
        });

        EXPECT_TRUE(all_equal(res, arrnd<arrnd<double>>({1}, {arrnd<double>({3, 1}, {1.5, 3.5, 5.5})})));
    }

    // expanded operation (type of reduction since resulted type array is in differernt depth)
    {
        arrnd<int> arr({3, 1, 2}, {1, 2, 3, 4, 5, 6});

        auto res = browse(arr, 2, [](arrnd<int> page) {
            return arrnd<arrnd<int>>({1}, {page});
        });

        EXPECT_TRUE(all_equal(res,
            arrnd<arrnd<arrnd<int>>>({3, 1},
                {arrnd<arrnd<int>>({1}, {arrnd<int>({1, 2}, {1, 2})}),
                    arrnd<arrnd<int>>({1}, {arrnd<int>({1, 2}, {3, 4})}),
                    arrnd<arrnd<int>>({1}, {arrnd<int>({1, 2}, {5, 6})})})));
    }
}

TEST(arrnd_type, nested_type)
{
    using namespace oc;

    static_assert(std::is_same_v<arrnd<int>::nested_t<0>, arrnd<int>>);
    static_assert(std::is_same_v<arrnd<int>::nested_t<1>, arrnd<arrnd<int>>>);
    static_assert(std::is_same_v<arrnd<int>::nested_t<2>, arrnd<arrnd<arrnd<int>>>>);
    static_assert(std::is_same_v<arrnd<int>::nested_t<3>, arrnd<arrnd<arrnd<arrnd<int>>>>>);
    static_assert(std::is_same_v<arrnd<int>::nested_t<4>, arrnd<arrnd<arrnd<arrnd<arrnd<int>>>>>>);
}

TEST(arrnd_type, find_adjacents)
{
    using namespace oc;

    {
        arrnd<int> arr(
            {3, 2, 4}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24});

        arr(arr.find_adjacents({1, 1, 2}, 1)) = 0;

        EXPECT_TRUE(all_equal(
            arr, arrnd<int>({3, 2, 4}, {1, 0, 0, 0, 5, 0, 0, 0, 9, 0, 0, 0, 13, 0, 15, 0, 17, 0, 0, 0, 21, 0, 0, 0})));
    }
}