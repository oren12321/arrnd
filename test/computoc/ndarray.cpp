#include <gtest/gtest.h>

#include <computoc/ndarray.h>


TEST(ND_range, fields_initialization)
{
    computoc::ND_range r1{};
    EXPECT_EQ(0, r1.start);
    EXPECT_EQ(0, r1.stop);
    EXPECT_EQ(1, r1.step);

    computoc::ND_range r2{1};
    EXPECT_EQ(1, r2.start);
    EXPECT_EQ(1, r2.stop);
    EXPECT_EQ(1, r2.step);

    computoc::ND_range r3{1, 2};
    EXPECT_EQ(1, r3.start);
    EXPECT_EQ(2, r3.stop);
    EXPECT_EQ(1, r3.step);

    computoc::ND_range r4{1, 2, 3};
    EXPECT_EQ(1, r4.start);
    EXPECT_EQ(2, r4.stop);
    EXPECT_EQ(3, r4.step);
}

TEST(ND_array_test, can_be_initialized_with_valid_size_and_data)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data[] = { 0, 0, 0 };
    EXPECT_NO_THROW((Integer_nd_array{ {1, 1}, data }));
    EXPECT_NO_THROW((Integer_nd_array{ {1, 3}, data }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1}, data }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1, 1}, data }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1, 1}, data }));

    EXPECT_THROW((Integer_nd_array{ {0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1}, data }), std::invalid_argument);

    EXPECT_THROW((Integer_nd_array{ {1, 0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 0, 1}, data }), std::invalid_argument);

    EXPECT_THROW((Integer_nd_array{ {0, 0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 0, 1}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1, 1}, data }), std::invalid_argument);
}

TEST(ND_array_test, can_be_initialized_with_valid_size_and_value)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int value{ 0 };
    EXPECT_NO_THROW((Integer_nd_array{ {1, 1}, value }));
    EXPECT_NO_THROW((Integer_nd_array{ {1, 3}, value }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1}, value }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1, 1}, value }));
    EXPECT_NO_THROW((Integer_nd_array{ {3, 1, 1}, value }));

    EXPECT_THROW((Integer_nd_array{ {0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1}, value }), std::invalid_argument);

    EXPECT_THROW((Integer_nd_array{ {1, 0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {1, 0, 1}, value }), std::invalid_argument);

    EXPECT_THROW((Integer_nd_array{ {0, 0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 0, 1}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_nd_array{ {0, 1, 1}, value }), std::invalid_argument);
}

TEST(ND_array_test, can_return_its_header_and_data)
{
    using Integer_nd_array = computoc::ND_array<int>;

    Integer_nd_array earr{};
    const Integer_nd_array::Header& ehdr{ earr.header() };

    EXPECT_EQ(0, ehdr.ndims());
    EXPECT_EQ(0, ehdr.count());
    EXPECT_FALSE(ehdr.dims());
    EXPECT_FALSE(ehdr.strides());
    EXPECT_EQ(0, ehdr.offset());
    EXPECT_FALSE(ehdr.is_subarray());
    EXPECT_FALSE(earr.data());

    const int value{ 0 };
    Integer_nd_array arr{ {3, 1, 2}, value };
    const Integer_nd_array::Header& hdr{ arr.header() };

    EXPECT_EQ(3, hdr.ndims());
    EXPECT_EQ(6, hdr.count());
    EXPECT_EQ(3, hdr.dims()[0]); EXPECT_EQ(1, hdr.dims()[1]); EXPECT_EQ(2, hdr.dims()[2]);
    EXPECT_EQ(2, hdr.strides()[0]); EXPECT_EQ(2, hdr.strides()[1]); EXPECT_EQ(1, hdr.strides()[2]);
    EXPECT_EQ(0, hdr.offset());
    EXPECT_FALSE(hdr.is_subarray());
    EXPECT_TRUE(arr.data());
    for (std::size_t i = 0; i < hdr.count(); ++i) {
        EXPECT_EQ(0, arr.data()[i]);
    }
}

TEST(ND_array_test, have_read_write_access_to_its_cells)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };

    Integer_nd_array arr1d{ { 6 }, data };
    const std::size_t* dims1d{ arr1d.header().dims() };
    for (std::size_t i = 0; i < dims1d[1]; ++i) {
        EXPECT_EQ(arr1d({ i }), data[i]);
    }
    for (std::size_t i = 0; i < dims1d[1]; ++i) {
        arr1d({ i }) = 0;
        EXPECT_EQ(arr1d({ i }), 0);
    }

    EXPECT_THROW(arr1d({ dims1d[1] }), std::out_of_range);
    EXPECT_THROW(arr1d({ 0, 0, 0 }), std::invalid_argument);

    Integer_nd_array arr2d{ { 3, 2 }, data };
    const std::size_t* dims2d{ arr2d.header().dims() };
    for (std::size_t i = 0; i < dims2d[0]; ++i) {
        for (std::size_t j = 0; j < dims2d[1]; ++j) {
            EXPECT_EQ(arr2d({ i, j }), data[i * dims2d[1] + j]);
        }
    }
    for (std::size_t i = 0; i < dims2d[0]; ++i) {
        for (std::size_t j = 0; j < dims2d[1]; ++j) {
            arr2d({ i, j }) = 0;
            EXPECT_EQ(arr2d({ i, j }), 0);
        }
    }

    EXPECT_THROW(arr2d({ dims2d[0], 0 }), std::out_of_range);
    EXPECT_THROW(arr2d({ 0, dims2d[1] }), std::out_of_range);
    EXPECT_THROW(arr2d({ 0, 0, 0 }), std::invalid_argument);

    Integer_nd_array arr3d{ {3, 1, 2}, data };
    const std::size_t* dims3d{ arr3d.header().dims() };
    for (std::size_t k = 0; k < dims3d[0]; ++k) {
        for (std::size_t i = 0; i < dims3d[1]; ++i) {
            for (std::size_t j = 0; j < dims3d[2]; ++j) {
                EXPECT_EQ(arr3d({ k, i, j }), data[k * (dims3d[1] * dims3d[2]) + i * dims3d[2] + j]);
            }
        }
    }
    for (std::size_t k = 0; k < dims3d[0]; ++k) {
        for (std::size_t i = 0; i < dims3d[1]; ++i) {
            for (std::size_t j = 0; j < dims3d[2]; ++j) {
                arr3d({ k, i, j }) = 0;
                EXPECT_EQ(arr3d({ k, i, j }), 0);
            }
        }
    }

    EXPECT_THROW(arr3d({ dims3d[0], 0, 0 }), std::out_of_range);
    EXPECT_THROW(arr3d({ 0, dims3d[1], 0 }), std::out_of_range);
    EXPECT_THROW(arr3d({ 0, 0, dims3d[2] }), std::out_of_range);
    EXPECT_THROW(arr3d({ 0, 0, 0, 0 }), std::invalid_argument);
}

TEST(ND_array_test, reshape)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::size_t dims1[]{ 3, 1, 2 };
    Integer_nd_array arr1{ 3, dims1, data1 };

    const int data2[] = { 1, 2, 3, 4, 5, 6 };
    std::size_t dims2[]{ 6 };
    Integer_nd_array arr2{ 1, dims2, data2 };

    Integer_nd_array rarr1{ computoc::reshaped(arr1, {1, 6}) };
    EXPECT_EQ(arr2, rarr1);

    const int data3[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::size_t dims3[]{ 3, 1, 2 };
    Integer_nd_array arr3{ 3, dims3, data3 };

    Integer_nd_array rarr2{ computoc::reshaped(arr1, {3, 1, 2}) };
    EXPECT_EQ(arr3, rarr2);

    EXPECT_THROW(computoc::reshaped(Integer_nd_array{}, {}), std::runtime_error);
    EXPECT_THROW(computoc::reshaped(arr2, { 1, 1 }), std::invalid_argument);
}

TEST(ND_array_test, can_be_compared_with_another_nd_array)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::size_t dims1[]{ 3, 1, 2 };
    Integer_nd_array arr1{ 3, dims1, data1 };
    Integer_nd_array arr2{ 3, dims1, data1 };

    EXPECT_EQ(arr1, arr2);

    std::size_t dims2[]{ 3, 2 };
    Integer_nd_array arr3{ 2, dims2, data1 };

    EXPECT_NE(arr1, arr3);

    const int data2[] = {
        1, 2,
        3, 4,
        5, 5 };
    Integer_nd_array arr4{ 3, dims1, data2 };
    Integer_nd_array arr5{ 2, dims2, data2 };

    EXPECT_NE(arr1, arr4);
    EXPECT_NE(arr1, arr5);
}

TEST(ND_array_test, can_return_slice)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data[] = {
    1, 2,
    3, 4,
    5, 6 };

    std::size_t dims1[]{ 6 };
    Integer_nd_array arr1{ 1, dims1, data };

    const int sdata1[] = { 1, 2, 3 };
    std::size_t sdims1[]{ 3 };
    Integer_nd_array sarr1{ 1, sdims1, sdata1 };
    EXPECT_EQ(arr1({ {}, {0, sdims1[0] - 1} }), sarr1);
    EXPECT_EQ(arr1({ {0, 0} }), arr1);

    EXPECT_THROW(arr1({ {0, dims1[0]} }), std::out_of_range);
    EXPECT_THROW(arr1({ {sdims1[0] - 1, 0} }), std::invalid_argument);

    std::size_t dims2[]{ 3, 2 };
    Integer_nd_array arr2{ 2, dims2, data };

    const int sdata2[] = {
        1, 2,
        3, 4 };
    std::size_t sdims2[]{ 2, 2 };
    Integer_nd_array sarr2{ 2, sdims2, sdata2 };
    EXPECT_EQ(arr2({ {0, sdims2[0] - 1}, {0, sdims2[1] - 1} }), sarr2);

    EXPECT_THROW(arr2({ { 0, 0 }, { 0, 0 }, {0, 0} }), std::invalid_argument);
    EXPECT_THROW(arr2({ {0, dims2[0]}, {0, 0} }), std::out_of_range);
    EXPECT_THROW(arr2({ {0, 0}, {0, dims2[1]} }), std::out_of_range);
    EXPECT_THROW(arr2({ {sdims2[0] - 1, 0}, {0, sdims2[1] - 1} }), std::invalid_argument);
    EXPECT_THROW(arr2({ {0,sdims2[0] - 1}, {sdims2[1] - 1, 0} }), std::invalid_argument);

    std::size_t dims3[]{ 3, 1, 2 };
    Integer_nd_array arr3{ 3, dims3, data };

    const int sdata3[] = {
        1, 2,
        3, 4 };
    std::size_t sdims3[]{ 2, 1, 2 };
    Integer_nd_array sarr3{ 3, sdims3, sdata3 };
    EXPECT_EQ(arr3({ {0, sdims3[0] - 1}, {0, sdims3[1] - 1}, {0, sdims3[2] - 1} }), sarr3);

    EXPECT_THROW(arr3({ { 0, 0 }, { 0, 0 }, {0, 0}, {0, 0} }), std::invalid_argument);
    EXPECT_THROW(arr3({ {0, dims3[0]}, {0, 0}, {0, 0} }), std::out_of_range);
    EXPECT_THROW(arr3({ {0, 0}, {0, dims3[1]}, {0, 0} }), std::out_of_range);
    EXPECT_THROW(arr3({ {0, 0}, {0, 0}, {0, dims3[2]} }), std::out_of_range);
    EXPECT_THROW(arr3({ {sdims3[0] - 1, 0}, {0, sdims3[1] - 1}, {0, sdims3[2] - 1} }), std::invalid_argument);
    EXPECT_THROW(arr3({ {0,sdims3[0] - 1}, {1, 0}, {0, sdims3[2] - 1} }), std::invalid_argument);
    EXPECT_THROW(arr3({ {0,sdims3[0] - 1}, {0, sdims3[1] - 1}, {sdims3[2] - 1, 0} }), std::invalid_argument);

    // Complex slicing - dimensions reduction and element step bigger than one.
    const int sdata4[] = {
        1 };
    std::size_t sdims4[]{ 1 };
    Integer_nd_array sarr4{ 1, sdims4, sdata4 };
    EXPECT_EQ(arr3({ {0, 0}, {0, 0}, {0, 1, 2} }), sarr4);
}

/*
TEST(ND_array_test, copy_by_reference)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    computoc::Dims dims{ 1, 2, 3 };
    Integer_nd_array arr{ dims, data };

    Integer_nd_array carr1{ arr };
    carr1({ 0, 0, 2 }) = 0;
    const int rdata1[] = {
        1, 2,
        3, 4,
        0, 6 };
    Integer_nd_array rarr1{ dims, rdata1 };
    EXPECT_EQ(rarr1, carr1);

    Integer_nd_array carr2{};
    carr2 = carr1;
    carr1({ 0, 0, 0 }) = 0;
    const int rdata2[] = {
        0, 2,
        3, 4,
        0, 6 };
    Integer_nd_array rarr2{ dims, rdata2 };
    EXPECT_EQ(rarr2, carr2);
    EXPECT_THROW(carr2({ 0, 0, 0 }, { 1, 1, 1 }) = carr1, std::runtime_error);
}

TEST(ND_array_test, move_by_reference)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    computoc::Dims dims{ 1, 2, 3 };
    Integer_nd_array sarr{ dims, data };

    Integer_nd_array arr{ dims, data };
    Integer_nd_array carr1{ std::move(arr) };
    EXPECT_EQ(sarr, carr1);
    EXPECT_TRUE(is_empty(arr.header().dims));

    Integer_nd_array carr2{};
    carr2 = std::move(carr1);
    EXPECT_EQ(sarr, carr2);
    EXPECT_TRUE(is_empty(carr1.header().dims));
    EXPECT_THROW(carr2({ 0, 0, 0 }, { 1, 1, 1 }) = std::move(sarr), std::runtime_error);
}

TEST(ND_array_test, copy_of)
{
    using Integer_nd_array = computoc::ND_array<int>;

    Integer_nd_array empty_arr{};
    Integer_nd_array cempty_arr{ computoc::copy_of(empty_arr) };
    EXPECT_EQ(empty_arr, cempty_arr);

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    computoc::Dims dims{ 1, 2, 3 };
    Integer_nd_array sarr{ dims, data };

    Integer_nd_array carr{ computoc::copy_of(sarr) };
    EXPECT_EQ(carr, sarr);
    carr({ 0, 0 }) = 0;
    EXPECT_NE(carr, sarr);

    Integer_nd_array csubarr{ computoc::copy_of(sarr({0, 0, 0}, {1, 1, 1})) };
    EXPECT_EQ(sarr({ 0, 0, 0 }, { 1, 1, 1 }), csubarr);
    csubarr({ 0, 0 }) = 5;
    EXPECT_NE(sarr({ 0, 0, 0 }, { 1, 1, 1 }), csubarr);
}

TEST(ND_array_test, copy_to)
{
    using Integer_nd_array = computoc::ND_array<int>;

    { // backward cases - copy from other arrrix to created arrrix
        Integer_nd_array empty_arr{};
        Integer_nd_array cempty_arr{};
        computoc::copy_to(Integer_nd_array{}, cempty_arr);
        EXPECT_EQ(empty_arr, cempty_arr);

        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        computoc::Dims dims1{ 1, 2, 3 };
        Integer_nd_array arr1{ dims1, data1 };

        const int data2[] = {
            2, 4,
            6, 8,
            10, 12 };
        computoc::Dims dims2{ 1, 2, 3 };
        Integer_nd_array arr2{ dims2, data2 };
        EXPECT_NE(arr1, arr2);
        computoc::copy_to(arr2, arr1);
        EXPECT_EQ(arr1, arr2);

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        computoc::Dims dims3{ 1, 2, 3 };
        Integer_nd_array arr3{ dims3, data3 };
        computoc::copy_to(arr2({ 0, 0, 2 }, { 1, 2, 1 }), arr2({ 0, 0, 0 }, { 1, 2, 1 }));
        EXPECT_EQ(arr3, arr2);

        EXPECT_THROW(computoc::copy_to(arr2, arr2({ 0, 0, 0 }, { 1, 2, 1 })), std::runtime_error);
    }

    { // forward cases - copy from other arrrix to created arrrix
        Integer_nd_array empty_arr{};
        Integer_nd_array cempty_arr{};
        computoc::copy_to(empty_arr, cempty_arr);
        EXPECT_EQ(empty_arr, cempty_arr);

        const int data1[] = {
            1, 2,
            3, 4,
            5, 6 };
        computoc::Dims dims1{ 1, 2, 3 };
        Integer_nd_array arr1{ dims1, data1 };

        const int data2[] = {
            2, 4,
            6, 8,
            10, 12 };
        computoc::Dims dims2{ 1, 2, 3 };
        Integer_nd_array arr2{ dims2, data2 };
        EXPECT_NE(arr1, arr2);
        computoc::copy_to(arr2, arr1);
        EXPECT_EQ(arr1, arr2);

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        computoc::Dims dims3{ 1, 2, 3 };
        Integer_nd_array arr3{ dims3, data3 };
        computoc::copy_to(arr2({ 0, 0, 2 }, { 1, 2, 1 }), arr2({ 0, 0, 0 }, { 1, 2, 1 }));
        EXPECT_EQ(arr3, arr2);
        EXPECT_THROW(computoc::copy_to(arr2({ 0, 0, 2 }, { 1, 2, 1 }), arr2({ 0, 0, 0 }, { 1, 2, 2 })), std::runtime_error);

        computoc::copy_to(arr3({ 0, 0, 0 }, { 1, 2, 1 }), arr2);
        EXPECT_EQ(arr3({ 0, 0, 0 }, { 1, 2, 1 }), arr2);
    }
}

TEST(ND_array_test, resize)
{
    using Integer_nd_array = computoc::ND_array<int>;

    const int data1[] = { 1, 2, 3, 4, 5, 6 };
    computoc::Dims dims1{ 1, 6 };
    Integer_nd_array arr1{ dims1, data1 };

    const int data2[] = {
        1, 2,
        3, 4,
        5, 6 };
    computoc::Dims dims2{ 1, 2, 3 };
    Integer_nd_array arr2{ dims2, data2 };

    arr1 = computoc::resized(arr1, { 1, 2, 3 });
    EXPECT_EQ(arr2, arr1);

    const int data3[] = { 1, 2 };
    computoc::Dims dims3{ 1, 2 };
    Integer_nd_array arr3{ dims3, data3 };

    arr1 = computoc::resized(arr1, { 1, 2 });
    EXPECT_EQ(arr3, arr1);

    const int data4[] = { 1, 2, 3, 4 };
    computoc::Dims dims4{ 1, 4 };
    Integer_nd_array arr4{ dims4, data4 };

    arr1 = computoc::resized(arr1, { 1, 4 });
    EXPECT_NE(arr4, arr1);
    EXPECT_EQ(arr4({ 0, 0 }), arr1({ 0, 0 }));
    EXPECT_EQ(arr4({ 0, 1 }), arr1({ 0, 1 }));

    Integer_nd_array arr5{ computoc::resized(arr1, {1, 2}) };
    EXPECT_NE(arr1.header().dims, arr5.header().dims);
    EXPECT_EQ(arr3, arr5);

    EXPECT_THROW(computoc::resized(arr1({ 0, 0, 0 }, { 1, 1, 1 }), {}), std::runtime_error);
}
*/