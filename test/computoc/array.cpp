#include <gtest/gtest.h>

#include <cstdint>

#include <computoc/utils.h>
#include <computoc/array.h>


TEST(ND_subscriptor, subscripts_generation_by_dimensions_of_an_array)
{
    const std::int64_t ndims{ 4 };
    const std::int64_t dims[]{ 2, 1, 3, 2 };

    const std::int64_t nsubs{ 12 };
    const std::int64_t rsubs_list[][4]{
        {0, 0, 0, 0},
        {0, 0, 0, 1},
        {0, 0, 1, 0},
        {0, 0, 1, 1},
        {0, 0, 2, 0},
        {0, 0, 2, 1},
        {1, 0, 0, 0},
        {1, 0, 0, 1},
        {1, 0, 1, 0},
        {1, 0, 1, 1},
        {1, 0, 2, 0},
        {1, 0, 2, 1} };

    computoc::Array<int>::Subscriptor counter{ {ndims, dims} };

    // prefix increment
    {
        std::int64_t nsubs_counter{ 0 };
        while (counter && nsubs_counter < nsubs) {
            const std::int64_t* subs{ counter.subs().p() };
            const std::int64_t* rsubs{ rsubs_list[nsubs_counter++] };

            EXPECT_EQ(rsubs[0], subs[0]);
            EXPECT_EQ(rsubs[1], subs[1]);
            EXPECT_EQ(rsubs[2], subs[2]);
            EXPECT_EQ(rsubs[3], subs[3]);

            ++counter;
        }
        EXPECT_EQ(nsubs, nsubs_counter);
        EXPECT_FALSE(counter);
    }

    counter.reset();
    const std::int64_t* subs{ counter.subs().p() };

    EXPECT_EQ(0, subs[0]);
    EXPECT_EQ(0, subs[1]);
    EXPECT_EQ(0, subs[2]);
    EXPECT_EQ(0, subs[3]);

    // postfix increment
    {
        std::int64_t nsubs_counter{ 0 };
        for (; counter; counter++) {
            const std::int64_t* subs{ counter.subs().p() };
            const std::int64_t* rsubs{ rsubs_list[nsubs_counter] };

            EXPECT_EQ(rsubs[0], subs[0]);
            EXPECT_EQ(rsubs[1], subs[1]);
            EXPECT_EQ(rsubs[2], subs[2]);
            EXPECT_EQ(rsubs[3], subs[3]);

            ++nsubs_counter;
        }

        EXPECT_EQ(nsubs, nsubs_counter);
        EXPECT_FALSE(counter);
    }

    // with negative initial subscript value
    {
        const std::int64_t nsubs1{ 6 };
        const std::int64_t rsubs_list1[][3]{
            {-1, 0, -2},
            {-1, 0, -1},
            {0, 0, -2},
            {0, 0, -1},
            {1, 0, -2},
            {1, 0, -1} };

        std::initializer_list<std::int64_t> from{ -1, 0, -2 };
        std::initializer_list<std::int64_t> to{ 2, 0, 0 };
        std::int64_t nsubs_counter{ 0 };
        for (computoc::Array<int>::Subscriptor counter{ from, to }; counter; ++counter) {
            const std::int64_t* subs{ counter.subs().p() };
            const std::int64_t* rsubs{ rsubs_list1[nsubs_counter] };

            EXPECT_EQ(rsubs[0], subs[0]);
            EXPECT_EQ(rsubs[1], subs[1]);
            EXPECT_EQ(rsubs[2], subs[2]);

            ++nsubs_counter;
        }

        EXPECT_EQ(nsubs1, nsubs_counter);
        EXPECT_FALSE(counter);
    }

    // with initial subscripts value
    {
        std::initializer_list<std::int64_t> from{ 1, 0, 0, 0 };
        std::initializer_list<std::int64_t> to{ 2, 1, 3, 2 };
        std::int64_t nsubs_counter{ 6 };
        for (computoc::Array<int>::Subscriptor counter{ from, to }; counter; ++counter) {
            const std::int64_t* subs{ counter.subs().p() };
            const std::int64_t* rsubs{ rsubs_list[nsubs_counter] };

            EXPECT_EQ(rsubs[0], subs[0]);
            EXPECT_EQ(rsubs[1], subs[1]);
            EXPECT_EQ(rsubs[2], subs[2]);
            EXPECT_EQ(rsubs[3], subs[3]);

            ++nsubs_counter;
        }

        EXPECT_EQ(nsubs, nsubs_counter);
        EXPECT_FALSE(counter);

        // test counter reset with initial subscripts
        {
            computoc::Array<int>::Subscriptor counter{ from, to };
            EXPECT_EQ(1, counter.subs().p()[0]);
            EXPECT_EQ(0, counter.subs().p()[1]);
            EXPECT_EQ(0, counter.subs().p()[2]);
            EXPECT_EQ(0, counter.subs().p()[3]);
            ++counter;
            EXPECT_EQ(1, counter.subs().p()[0]);
            EXPECT_EQ(0, counter.subs().p()[1]);
            EXPECT_EQ(0, counter.subs().p()[2]);
            EXPECT_EQ(1, counter.subs().p()[3]);
            counter.reset();
            EXPECT_EQ(1, counter.subs().p()[0]);
            EXPECT_EQ(0, counter.subs().p()[1]);
            EXPECT_EQ(0, counter.subs().p()[2]);
            EXPECT_EQ(0, counter.subs().p()[3]);
        }
    }

    // 1d subs with index
    {
        EXPECT_THROW(computoc::Array<int>::Subscriptor({ 5 }, 1), std::invalid_argument);

        const std::int64_t nsubs{ 6 };
        const std::int64_t subs[]{ 0, 1, 2, 3, 4, 5 };
        std::initializer_list<std::int64_t> from{ 1 };
        std::initializer_list<std::int64_t> to{ 6 };
        computoc::Array<int>::Subscriptor counter(from, to, 0);
        std::int64_t nsubs_counter{ 1 };
        for (; counter; ++counter) {
            EXPECT_EQ(subs[nsubs_counter], counter.subs().p()[0]);
            ++nsubs_counter;
        }
        EXPECT_EQ(nsubs, nsubs_counter);
        EXPECT_FALSE(counter);
    }

    // 4d subs with different axis
    {
        EXPECT_THROW(computoc::Array<int>::Subscriptor({ ndims, dims }, 5), std::invalid_argument);

        // axis 0
        {
            const std::int64_t rsubs_list0[][4]{
                {0, 0, 0, 0},
                {1, 0, 0, 0},
                {0, 0, 0, 1},
                {1, 0, 0, 1},
                {0, 0, 1, 0},
                {1, 0, 1, 0},
                {0, 0, 1, 1},
                {1, 0, 1, 1},
                {0, 0, 2, 0},
                {1, 0, 2, 0},
                {0, 0, 2, 1},
                {1, 0, 2, 1} };
            computoc::Array<int>::Subscriptor counter({ ndims, dims }, 0);
            std::int64_t nsubs_counter{ 0 };
            for (; counter; counter++) {
                const std::int64_t* subs{ counter.subs().p() };
                const std::int64_t* rsubs{ rsubs_list0[nsubs_counter] };

                EXPECT_EQ(rsubs[0], subs[0]);
                EXPECT_EQ(rsubs[1], subs[1]);
                EXPECT_EQ(rsubs[2], subs[2]);
                EXPECT_EQ(rsubs[3], subs[3]);

                ++nsubs_counter;
            }
            EXPECT_EQ(nsubs, nsubs_counter);
            EXPECT_FALSE(counter);
        }

        // axis 1
        {
            const std::int64_t rsubs_list1[][4]{
                {0, 0, 0, 0},
                {0, 0, 0, 1},
                {0, 0, 1, 0},
                {0, 0, 1, 1},
                {0, 0, 2, 0},
                {0, 0, 2, 1},
                {1, 0, 0, 0},
                {1, 0, 0, 1},
                {1, 0, 1, 0},
                {1, 0, 1, 1},
                {1, 0, 2, 0},
                {1, 0, 2, 1} };
            computoc::Array<int>::Subscriptor counter({ ndims, dims }, 1);
            std::int64_t nsubs_counter{ 0 };
            for (; counter; counter++) {
                const std::int64_t* subs{ counter.subs().p() };
                const std::int64_t* rsubs{ rsubs_list1[nsubs_counter] };

                EXPECT_EQ(rsubs[0], subs[0]);
                EXPECT_EQ(rsubs[1], subs[1]);
                EXPECT_EQ(rsubs[2], subs[2]);
                EXPECT_EQ(rsubs[3], subs[3]);

                ++nsubs_counter;
            }
            EXPECT_EQ(nsubs, nsubs_counter);
            EXPECT_FALSE(counter);
        }

        // axis 2
        {
            const std::int64_t rsubs_list2[][4]{
                {0, 0, 0, 0},
                {0, 0, 1, 0},
                {0, 0, 2, 0},
                {0, 0, 0, 1},
                {0, 0, 1, 1},
                {0, 0, 2, 1},
                {1, 0, 0, 0},
                {1, 0, 1, 0},
                {1, 0, 2, 0},
                {1, 0, 0, 1},
                {1, 0, 1, 1},
                {1, 0, 2, 1} };
            computoc::Array<int>::Subscriptor counter({ ndims, dims }, 2);
            std::int64_t nsubs_counter{ 0 };
            for (; counter; counter++) {
                const std::int64_t* subs{ counter.subs().p() };
                const std::int64_t* rsubs{ rsubs_list2[nsubs_counter] };

                EXPECT_EQ(rsubs[0], subs[0]);
                EXPECT_EQ(rsubs[1], subs[1]);
                EXPECT_EQ(rsubs[2], subs[2]);
                EXPECT_EQ(rsubs[3], subs[3]);

                ++nsubs_counter;
            }
            EXPECT_EQ(nsubs, nsubs_counter);
            EXPECT_FALSE(counter);
        }

        // axis 3
        {
            const std::int64_t rsubs_list3[][4]{
                {0, 0, 0, 0},
                {0, 0, 0, 1},
                {0, 0, 1, 0},
                {0, 0, 1, 1},
                {0, 0, 2, 0},
                {0, 0, 2, 1},
                {1, 0, 0, 0},
                {1, 0, 0, 1},
                {1, 0, 1, 0},
                {1, 0, 1, 1},
                {1, 0, 2, 0},
                {1, 0, 2, 1} };
            computoc::Array<int>::Subscriptor counter({ ndims, dims }, 3);
            std::int64_t nsubs_counter{ 0 };
            for (; counter; counter++) {
                const std::int64_t* subs{ counter.subs().p() };
                const std::int64_t* rsubs{ rsubs_list3[nsubs_counter] };

                EXPECT_EQ(rsubs[0], subs[0]);
                EXPECT_EQ(rsubs[1], subs[1]);
                EXPECT_EQ(rsubs[2], subs[2]);
                EXPECT_EQ(rsubs[3], subs[3]);

                ++nsubs_counter;
            }
            EXPECT_EQ(nsubs, nsubs_counter);
            EXPECT_FALSE(counter);
        }

        // specific order
        {
            const std::int64_t ordered_subs_list[][4]{
                {0, 0, 0, 0},
                {0, 0, 0, 1},
                {0, 1, 0, 0},
                {0, 1, 0, 1},
                {1, 0, 0, 0},
                {1, 0, 0, 1},
                {1, 1, 0, 0},
                {1, 1, 0, 1},
                {2, 0, 0, 0},
                {2, 0, 0, 1},
                {2, 1, 0, 0},
                {2, 1, 0, 1},
                {3, 0, 0, 0},
                {3, 0, 0, 1},
                {3, 1, 0, 0},
                {3, 1, 0, 1},
                {0, 0, 1, 0},
                {0, 0, 1, 1},
                {0, 1, 1, 0},
                {0, 1, 1, 1},
                {1, 0, 1, 0},
                {1, 0, 1, 1},
                {1, 1, 1, 0},
                {1, 1, 1, 1},
                {2, 0, 1, 0},
                {2, 0, 1, 1},
                {2, 1, 1, 0},
                {2, 1, 1, 1},
                {3, 0, 1, 0},
                {3, 0, 1, 1},
                {3, 1, 1, 0},
                {3, 1, 1, 1},
                {0, 0, 2, 0},
                {0, 0, 2, 1},
                {0, 1, 2, 0},
                {0, 1, 2, 1},
                {1, 0, 2, 0},
                {1, 0, 2, 1},
                {1, 1, 2, 0},
                {1, 1, 2, 1},
                {2, 0, 2, 0},
                {2, 0, 2, 1},
                {2, 1, 2, 0},
                {2, 1, 2, 1},
                {3, 0, 2, 0},
                {3, 0, 2, 1},
                {3, 1, 2, 0},
                {3, 1, 2, 1} };
            const std::int64_t ordered_ndims{ 4 };
            const std::int64_t ordered_dims[]{ 4, 2, 3, 2 };
            const std::int64_t order[]{ 2, 0, 1, 3 };

            // full count
            {
                computoc::Array<int>::Subscriptor counter({ ordered_ndims, ordered_dims }, { ordered_ndims, order }, { 0, nullptr });
                std::int64_t nsubs_counter{ 0 };
                for (; counter; counter++) {
                    const std::int64_t* subs{ counter.subs().p() };
                    const std::int64_t* rsubs{ ordered_subs_list[nsubs_counter] };

                    EXPECT_EQ(rsubs[0], subs[0]);
                    EXPECT_EQ(rsubs[1], subs[1]);
                    EXPECT_EQ(rsubs[2], subs[2]);
                    EXPECT_EQ(rsubs[3], subs[3]);

                    ++nsubs_counter;
                }
                EXPECT_EQ(48, nsubs_counter);
                EXPECT_FALSE(counter);
            }

            // partial count
            {
                const std::int64_t from[]{ 3, 0, 2, 0 };
                computoc::Array<int>::Subscriptor counter({ ordered_ndims, from }, { ordered_ndims, ordered_dims }, { ordered_ndims, order }, { 0, nullptr });
                std::int64_t nsubs_counter{ 44 };
                for (; counter; counter++) {
                    const std::int64_t* subs{ counter.subs().p() };
                    const std::int64_t* rsubs{ ordered_subs_list[nsubs_counter] };

                    EXPECT_EQ(rsubs[0], subs[0]);
                    EXPECT_EQ(rsubs[1], subs[1]);
                    EXPECT_EQ(rsubs[2], subs[2]);
                    EXPECT_EQ(rsubs[3], subs[3]);

                    ++nsubs_counter;
                }
                EXPECT_EQ(48, nsubs_counter);
                EXPECT_FALSE(counter);
            }
        }
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

    EXPECT_THROW((Integer_array{ {0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1}, data }), std::invalid_argument);

    EXPECT_THROW((Integer_array{ {1, 0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 0, 1}, data }), std::invalid_argument);

    EXPECT_THROW((Integer_array{ {0, 0, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1, 0}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 0, 1}, data }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1, 1}, data }), std::invalid_argument);
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

    EXPECT_THROW((Integer_array{ {0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1}, value }), std::invalid_argument);

    EXPECT_THROW((Integer_array{ {1, 0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {1, 0, 1}, value }), std::invalid_argument);

    EXPECT_THROW((Integer_array{ {0, 0, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1, 0}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 0, 1}, value }), std::invalid_argument);
    EXPECT_THROW((Integer_array{ {0, 1, 1}, value }), std::invalid_argument);
}

TEST(Array_test, can_return_its_header_and_data)
{
    using Integer_array = computoc::Array<int>;

    Integer_array earr{};
    const Integer_array::Header& ehdr{ earr.header() };

    EXPECT_EQ(0, ehdr.dims().s());
    EXPECT_EQ(0, ehdr.count());
    EXPECT_TRUE(ehdr.dims().empty());
    EXPECT_TRUE(ehdr.strides().empty());
    EXPECT_EQ(0, ehdr.offset());
    EXPECT_FALSE(ehdr.is_partial());
    EXPECT_FALSE(earr.data());

    const int value{ 0 };
    Integer_array arr{ {3, 1, 2}, value };
    const Integer_array::Header& hdr{ arr.header() };

    EXPECT_EQ(3, hdr.dims().s());
    EXPECT_EQ(6, hdr.count());
    EXPECT_EQ(3, hdr.dims().p()[0]); EXPECT_EQ(1, hdr.dims().p()[1]); EXPECT_EQ(2, hdr.dims().p()[2]);
    EXPECT_EQ(2, hdr.strides().p()[0]); EXPECT_EQ(2, hdr.strides().p()[1]); EXPECT_EQ(1, hdr.strides().p()[2]);
    EXPECT_EQ(0, hdr.offset());
    EXPECT_FALSE(hdr.is_partial());
    EXPECT_TRUE(arr.data());
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
    const std::int64_t* dims1d{ arr1d.header().dims().p() };
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
    const std::int64_t* dims2d{ arr2d.header().dims().p() };
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
    const std::int64_t* dims3d{ arr3d.header().dims().p() };
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
    Integer_array arr{ {4, dims}, data };

    const int rdata[] = {
        11,
        14,

        29,
        32
    };
    const std::int64_t rdims[]{ 2, 2, 1 };
    Integer_array rarr{ {3, rdims}, rdata };

    Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };

    for (std::int64_t k = 0; k < rdims[0]; ++k) {
        for (std::int64_t i = 0; i < rdims[1]; ++i) {
            for (std::int64_t j = 0; j < rdims[2]; ++j) {
                EXPECT_EQ(rarr({ k, i, j }), sarr({ k, 0, i, j }));
            }
        }
    }
}

TEST(Array_test, element_wise_transformation)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    const double odata[]{
        0.5, 1.0,
        1.5, 2.0,
        2.5, 3.0 };
    computoc::Array oarr{ {3, dims}, odata };

    EXPECT_EQ(oarr, computoc::transform(iarr, [](int n) {return n * 0.5; }));
}

TEST(Array_test, element_wise_binary_operation)
{
    EXPECT_THROW(computoc::binary(computoc::Array<int>({ 3, 1, 2 }), computoc::Array<double>({ 6 }), [](int, double) {return 0.0; }), std::invalid_argument);

    std::int64_t dims[]{ 3, 1, 2 };

    const int idata1[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr1{ {3, dims}, idata1 };

    const double idata2[]{
        0.5, 1.0,
        1.5, 2.0,
        2.5, 3.0 };
    computoc::Array iarr2{ {3, dims}, idata2 };

    computoc::Array oarr1{ {3, dims}, 0.5 };

    EXPECT_EQ(oarr1, computoc::binary(iarr1, iarr2, [](int a, double b) { return b / a; }));

    const int odata2[] = {
        0, 1,
        2, 3,
        4, 5 };
    computoc::Array oarr2{ {3, dims}, odata2 };

    EXPECT_EQ(oarr2, computoc::binary(iarr1, 1, [](int a, int b) { return a - b; }));

    const int odata3[] = {
        0, -1,
        -2, -3,
        -4, -5 };
    computoc::Array oarr3{ {3, dims}, odata3 };

    EXPECT_EQ(oarr3, computoc::binary(1, iarr1, [](int a, int b) { return a - b; }));
}

TEST(Array_test, reduce_elements)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    EXPECT_EQ((1.0 / 2 / 3 / 4 / 5 / 6), computoc::reduce(iarr, [](int value, double previous) {return previous / value; }));

    std::int64_t dims2[]{ 3, 1 };
    const double rdata2[]{
        3.0,
        7.0,
        11.0
    };
    computoc::Array rarr2{ {2, dims2}, rdata2 };
    EXPECT_EQ(rarr2, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 2));

    std::int64_t dims1[]{ 3, 2 };
    const double rdata1[]{
        1.0, 2.0,
        3.0, 4.0,
        5.0, 6.0 };
    computoc::Array rarr1{ {2, dims1}, rdata1 };
    EXPECT_EQ(rarr1, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 1));

    std::int64_t dims0[]{ 1, 2 };
    const double rdata0[]{
        9.0, 12.0 };
    computoc::Array rarr0{ {2, dims0}, rdata0 };
    EXPECT_EQ(rarr0, computoc::reduce(iarr, [](int value, double previous) {return previous + value; }, 0));

    computoc::Array iarr1d{ {6}, idata };
    const double data1d[]{ 21.0 };
    computoc::Array rarr1d{ {1}, data1d };
    EXPECT_EQ(rarr1d, computoc::reduce(iarr1d, [](int value, double previous) {return previous + value; }, 0));

    EXPECT_THROW(computoc::reduce(iarr, [](int, double) { return 0; }, 3), std::invalid_argument);

    // complex array reduction
    {
        auto sum = [](int value, double previous) {
            return previous + value;
        };

        EXPECT_EQ(rarr1d, computoc::reduce(computoc::reduce(computoc::reduce(iarr, sum, 2), sum, 1), sum, 0));
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

    EXPECT_EQ(rarr, computoc::all(arr, 0));
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

    EXPECT_EQ(rarr, computoc::any(arr, 0));
}

TEST(Array_test, filter_elements_by_condition)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 0,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    const int rdata0[]{ 1, 2, 3, 0, 5, 6 };
    computoc::Array rarr0{ {6}, rdata0 };
    EXPECT_EQ(rarr0, computoc::filter(iarr, [](int) {return 1; }));

    const int rdata1[]{ 1, 2, 3, 5, 6 };
    computoc::Array rarr1{ {5}, rdata1 };
    EXPECT_EQ(rarr1, computoc::filter(iarr, [](int a) { return a; }));

    const double rdata2[]{ 2.0, 0.0, 6.0 };
    computoc::Array rarr2{ {3}, rdata2 };
    EXPECT_EQ(rarr2, computoc::filter(iarr, [](int a) { return a % 2 == 0; }));

    EXPECT_EQ(computoc::Array<int>{}, computoc::filter(iarr, [](int a) { return a > 6; }));
    EXPECT_EQ(computoc::Array<int>{}, computoc::filter(computoc::Array<int>{}, [](int) {return 1; }));
}

TEST(Array_test, filter_elements_by_maks)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    EXPECT_THROW(computoc::filter(iarr, computoc::Array<int>{}), std::invalid_argument);

    const int imask_data0[]{
        1, 0,
        0, 1,
        0, 1 };
    computoc::Array imask0{ {3, dims}, imask_data0 };
    const int rdata0[]{ 1, 4, 6 };
    computoc::Array rarr0{ {3}, rdata0 };
    EXPECT_EQ(rarr0, computoc::filter(iarr, imask0));

    const int imask_data1[]{
        0, 0,
        0, 0,
        0, 0 };
    computoc::Array imask1{ {3, dims}, imask_data1 };
    EXPECT_EQ(computoc::Array<int>{}, computoc::filter(iarr, imask1));

    const int imask_data2[]{
        1, 1,
        1, 1,
        1, 1 };
    computoc::Array imask2{ {3, dims}, imask_data2 };
    const int rdata2[]{ 1, 2, 3, 4, 5, 6 };
    computoc::Array rarr2{ {6}, rdata2 };
    EXPECT_EQ(rarr2, computoc::filter(iarr, imask2));

    EXPECT_EQ(computoc::Array<int>{}, computoc::filter(computoc::Array<int>{}, imask0));
}

TEST(Array_test, select_elements_indices_by_condition)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 0,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    const std::int64_t rdata0[]{ 0, 1, 2, 3, 4, 5 };
    computoc::Array rarr0{ {6}, rdata0 };
    EXPECT_EQ(rarr0, computoc::find(iarr, [](int) {return 1; }));

    const std::int64_t rdata1[]{ 0, 1, 2, 4, 5 };
    computoc::Array rarr1{ {5}, rdata1 };
    EXPECT_EQ(rarr1, computoc::find(iarr, [](int a) { return a; }));

    const std::int64_t rdata2[]{ 1, 3, 5 };
    computoc::Array rarr2{ {3}, rdata2 };
    EXPECT_EQ(rarr2, computoc::find(iarr, [](int a) { return a % 2 == 0; }));

    EXPECT_EQ(computoc::Array<std::int64_t>{}, computoc::find(iarr, [](int a) { return a > 6; }));
    EXPECT_EQ(computoc::Array<std::int64_t>{}, computoc::find(computoc::Array<int>{}, [](int) {return 1; }));

    // subarray
    const std::int64_t rdatas[]{ 2 };
    computoc::Array rarrs{ {1}, rdatas };
    EXPECT_EQ(rarrs, computoc::find(iarr({ {1, 1} }), [](int a) { return a; }));
}

TEST(Array_test, select_elements_indices_by_maks)
{
    std::int64_t dims[]{ 3, 1, 2 };

    const int idata[]{
        1, 2,
        3, 4,
        5, 6 };
    computoc::Array iarr{ {3, dims}, idata };

    EXPECT_THROW(computoc::find(iarr, computoc::Array<int>{}), std::invalid_argument);

    const int imask_data0[]{
        1, 0,
        0, 1,
        0, 1 };
    computoc::Array imask0{ {3, dims}, imask_data0 };
    const std::int64_t rdata0[]{ 0, 3, 5 };
    computoc::Array rarr0{ {3}, rdata0 };
    EXPECT_EQ(rarr0, computoc::find(iarr, imask0));

    const int imask_data1[]{
        0, 0,
        0, 0,
        0, 0 };
    computoc::Array imask1{ {3, dims}, imask_data1 };
    EXPECT_EQ(computoc::Array<std::int64_t>{}, computoc::find(iarr, imask1));

    const int imask_data2[]{
        1, 1,
        1, 1,
        1, 1 };
    computoc::Array imask2{ {3, dims}, imask_data2 };
    const std::int64_t rdata2[]{ 0, 1, 2, 3, 4, 5 };
    computoc::Array rarr2{ {6}, rdata2 };
    EXPECT_EQ(rarr2, computoc::find(iarr, imask2));

    EXPECT_EQ(computoc::Array<std::int64_t>{}, computoc::find(computoc::Array<std::int64_t>{}, imask0));
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
    computoc::Array iarr{ {4, idims}, idata };

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
    computoc::Array rarr{ {4, rdims}, rdata };

    EXPECT_EQ(rarr, computoc::transpose(iarr, { 2, 0, 1, 3 }));

    EXPECT_THROW(computoc::transpose(iarr, { 2, 0, 1, 3, 2 }), std::invalid_argument);
    EXPECT_THROW(computoc::transpose(iarr, { 2, 0, 1, 4 }), std::out_of_range);
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
    
    EXPECT_EQ(rarr, computoc::equal(arr1, arr2));
    EXPECT_THROW(computoc::equal(arr1, Integer_array{ {1} }), std::invalid_argument);
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

    EXPECT_EQ(rarr, computoc::not_equal(arr1, arr2));
    EXPECT_THROW(computoc::not_equal(arr1, Integer_array{ {1} }), std::invalid_argument);
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

    EXPECT_EQ(rarr, arr1 > arr2);
    EXPECT_THROW(arr1 > Integer_array{ {1} }, std::invalid_argument);
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

    EXPECT_EQ(rarr, arr1 >= arr2);
    EXPECT_THROW(arr1 >= Integer_array{ {1} }, std::invalid_argument);
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

    EXPECT_EQ(rarr, arr1 < arr2);
    EXPECT_THROW(arr1 < Integer_array{ {1} }, std::invalid_argument);
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

    EXPECT_EQ(rarr, arr1 <= arr2);
    EXPECT_THROW(arr1 <= Integer_array{ {1} }, std::invalid_argument);
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

    EXPECT_EQ(rarr, computoc::close(arr1, arr2, 2));
    EXPECT_THROW(computoc::close(arr1, Integer_array{ {1} }), std::invalid_argument);
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

    EXPECT_EQ(rarr1, arr1 + arr2);
    arr1 += arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 + Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 += Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        11, 12,
        13, 14,
        15, 16 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 + 10);
    EXPECT_EQ(rarr2, 10 + arr2);
    arr2 += 10;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 - arr2);
    arr1 -= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 - Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 -= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0, 1,
        2, 3,
        4, 5 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 - 1);

    const int rdata3[] = {
        0, -1,
        -2, -3,
        -4, -5 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_EQ(rarr3, 1 - arr2);
    arr2 -= 1;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 * arr2);
    arr1 *= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 * Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 *= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        10, 20,
        30, 40,
        50, 60 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 * 10);
    EXPECT_EQ(rarr2, 10 * arr2);
    arr2 *= 10;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 / arr2);
    arr1 /= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 / Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 /= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0, 1,
        1, 2,
        2, 3 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 / 2);

    const int rdata3[] = {
        2, 1,
        0, 0,
        0, 0 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_EQ(rarr3, 2 / arr2);
    arr2 /= 2;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 % arr2);
    arr1 %= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 % Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 %= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        1, 0,
        1, 0,
        1, 0 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 % 2);

    const int rdata3[] = {
        0, 0,
        2, 2,
        2, 2 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_EQ(rarr3, 2 % arr2);
    arr2 %= 2;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 ^ arr2);
    arr1 ^= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 ^ Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 ^= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0b111, 0b110,
        0b101, 0b111,
        0b011, 0b111 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 ^ 0b111);
    EXPECT_EQ(rarr2, 0b111 ^ arr2);
    arr2 ^= 0b111;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 & arr2);
    arr1 &= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 & Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 &= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0b000, 0b001,
        0b010, 0b000,
        0b100, 0b000 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 & 0b111);
    EXPECT_EQ(rarr2, 0b111 & arr2);
    arr2 &= 0b111;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 | arr2);
    arr1 |= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 | Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 |= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0b111, 0b111,
        0b111, 0b111,
        0b111, 0b111 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 | 0b111);
    EXPECT_EQ(rarr2, 0b111 | arr2);
    arr2 |= 0b111;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 << arr2);
    arr1 <<= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 << Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 <<= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0, 4,
        8, 12,
        16, 20 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 << 2);

    const int rdata3[] = {
        2, 4,
        8, 16,
        32, 64 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_EQ(rarr3, 2 << arr2);
    arr2 <<= 2;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr1, arr1 >> arr2);
    arr1 >>= arr2;
    EXPECT_EQ(rarr1, arr1);

    EXPECT_THROW(arr1 >> Integer_array{ {1} }, std::invalid_argument);
    EXPECT_THROW(arr1 >>= Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0, 0,
        0, 0,
        1, 1 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 >> 2);

    const int rdata3[] = {
        2, 1,
        0, 0,
        0, 0 };
    Integer_array rarr3{ {3, 1, 2}, rdata3 };

    EXPECT_EQ(rarr3, 2 >> arr2);
    arr2 >>= 2;
    EXPECT_EQ(rarr2, arr2);
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

    EXPECT_EQ(rarr, ~arr);
    EXPECT_EQ(Integer_array{}, ~Integer_array{});
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

    EXPECT_EQ(rarr, !arr);
    EXPECT_EQ(Integer_array{}, !Integer_array{});
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

    EXPECT_EQ(rarr1, arr1 && arr2);

    EXPECT_THROW(arr1 && Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        0, 1,
        1, 0,
        1, 0 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 && 1);
    EXPECT_EQ(rarr2, 1 && arr2);
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

    EXPECT_EQ(rarr1, arr1 || arr2);

    EXPECT_THROW(arr1 || Integer_array{ {1} }, std::invalid_argument);

    const int rdata2[] = {
        1, 1,
        1, 1,
        1, 1 };
    Integer_array rarr2{ {3, 1, 2}, rdata2 };

    EXPECT_EQ(rarr2, arr2 || 1);
    EXPECT_EQ(rarr2, 1 || arr2);
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

    EXPECT_EQ(rarr1, old);
    EXPECT_EQ(rarr2, arr);
    EXPECT_EQ(Integer_array{}, ++Integer_array{});
    EXPECT_EQ(arr, ++(Integer_array{ { 3, 1, 2 }, data }));
    EXPECT_EQ((Integer_array{ { 3, 1, 2 }, data }), (Integer_array{ { 3, 1, 2 }, data })++);
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

    EXPECT_EQ(rarr1, old);
    EXPECT_EQ(rarr2, arr);
    EXPECT_EQ(Integer_array{}, --Integer_array{});
    EXPECT_EQ(arr, --(Integer_array{ { 3, 1, 2 }, data }));
    EXPECT_EQ((Integer_array{ { 3, 1, 2 }, data }), (Integer_array{ { 3, 1, 2 }, data })--);
}

TEST(Array_test, can_be_compared_with_another_array)
{
    using Integer_array = computoc::Array<int>;

    const int data1[] = {
        1, 2,
        3, 4,
        5, 6 };
    std::int64_t dims1[]{ 3, 1, 2 };
    Integer_array arr1{ {3, dims1}, data1 };
    Integer_array arr2{ {3, dims1}, data1 };

    EXPECT_EQ(arr1, arr2);

    std::int64_t dims2[]{ 3, 2 };
    Integer_array arr3{ {2, dims2}, data1 };

    EXPECT_NE(arr1, arr3);

    const int data2[] = {
        1, 2,
        3, 4,
        5, 5 };
    Integer_array arr4{ {3, dims1}, data2 };
    Integer_array arr5{ {2, dims2}, data2 };

    EXPECT_NE(arr1, arr4);
    EXPECT_NE(arr1, arr5);

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
        Integer_array arr{ {4, dims}, data };

        const int rdata[] = {
            11,
            14,

            29,
            32
        };
        const std::int64_t rdims[]{ 2, 1, 2, 1 };
        Integer_array rarr{ {4, rdims}, rdata };

        Integer_array sarr{ arr({{0, 1}, {1, 1}, {0, 1}, {1, 2, 2}}) };
        EXPECT_EQ(rarr, sarr);
    }

    // different ND array types
    {
        const double data1d[] = {
            1.0, 2.0,
            3.0, 4.0,
            5.0, 6.0 };
        std::int64_t dims1d[]{ 3, 1, 2 };
        computoc::Array<double> arr1d{ {3, dims1d}, data1d };

        EXPECT_EQ(arr1, arr1d);
        
        arr1d({ 0, 0, 0 }) = 1.001;
        EXPECT_NE(arr1, arr1d);
    }

    // empty arrays
    {
        EXPECT_EQ(Integer_array{}, Integer_array{});
        EXPECT_EQ(Integer_array{}, Integer_array({}));
        EXPECT_EQ(Integer_array{}, Integer_array({}, 0));
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
    Integer_array arr{ {3, dims}, data };

    // empty ranges
    {
        std::initializer_list<computoc::Interval<std::int64_t>> ranges{};
        Integer_array rarr{ arr(ranges) };
        EXPECT_EQ(arr, rarr);
        EXPECT_EQ(arr.data(), rarr.data());
    }

    // illegal ranges
    {
        EXPECT_EQ(Integer_array{}, arr({ {0, 0, 0} }));
        EXPECT_EQ(Integer_array{}, arr({ {2, 1, 1} }));
    }

    // empty array
    {
        EXPECT_EQ(Integer_array{}, Integer_array{}(std::initializer_list<computoc::Interval<std::int64_t>>{}));
        EXPECT_EQ(Integer_array{}, Integer_array{}({ {0,1}, {0,4,2} }));
    }

    // ranges in dims
    {
        // nranges == ndims
        const int tdata1[] = {
            1,
            5 };
        const std::int64_t tdims1[] = { 2, 1, 1 };
        Integer_array tarr1{ {3, tdims1}, tdata1 };
        Integer_array sarr1{ arr({{0, 2,2}, {0}, {0}}) };
        EXPECT_EQ(tarr1, sarr1);
        EXPECT_EQ(arr.data(), sarr1.data());

        // nranges < ndims
        const int tdata2[] = {
            3, 4 };
        const std::int64_t tdims2[] = { 1, 1, 2 };
        Integer_array tarr2{ {3, tdims2}, tdata2 };
        Integer_array sarr2{ arr({{1, 2, 2}}) };
        EXPECT_EQ(tarr2, sarr2);
        EXPECT_EQ(arr.data(), sarr2.data());

        // nranges > ndims - ignore extra ranges
        Integer_array sarr3{ arr({{0, 2, 2}, {0}, {0}, {100, 100, 5}}) };
        EXPECT_EQ(sarr1, sarr3);
        EXPECT_EQ(arr.data(), sarr3.data());

        // out of range and negative indices
        Integer_array sarr4{ arr({{-1, 3, -2}, {1}, {-2}}) };
        EXPECT_EQ(sarr1, sarr4);
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
        EXPECT_EQ(Integer_array{}, arr);
    }

    // normal array
    {
        const int data[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array arr{ {3, dims}, data };

        const int tdata[] = {
            100, 100,
            100, 100,
            100, 100 };
        Integer_array tarr{ {3, dims}, tdata };

        arr = 100;
        EXPECT_EQ(tarr, arr);
    }

    // subarray
    {
        const int data[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array arr{ {3, dims}, data };

        const int tdata[] = {
            1, 50,
            3, 100,
            5, 100 };
        Integer_array tarr{ {3, dims}, tdata };

        arr({ {1,2}, {0}, {1} }) = 100;
        // assignment of different type
        arr({ {0,0}, {0}, {1} }) = 50.5;
        EXPECT_EQ(tarr, arr);
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
    Integer_array arr{ {3, dims}, data };

    Integer_array carr1{ arr };
    carr1({ 2, 0, 0 }) = 0;
    const int rdata1[] = {
        1, 2,
        3, 4,
        0, 6 };
    Integer_array rarr1{ {3, dims}, rdata1 };
    EXPECT_EQ(rarr1, carr1);

    Integer_array carr2{};
    carr2 = carr1;
    carr1({ 0, 0, 0 }) = 0;
    const int rdata2[] = {
        0, 2,
        3, 4,
        0, 6 };
    Integer_array rarr2{ {3, dims}, rdata2 };
    EXPECT_EQ(rarr2, carr2);

    carr2({ {0, 1}, {0, 0}, {0, 1} }) = carr1;
    EXPECT_EQ(rarr2, carr2);

    // slice copying by assignment (rvalue)
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        Integer_array starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 5, 2} }) = starr3;
        EXPECT_EQ(tarr3, rarr3);
    }

    // slice copying by assignment (lvalue)
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        Integer_array starr3{ tarr3({ {0, 5, 2} }) };
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        rarr3 = starr3;
        EXPECT_NE(tarr3, rarr3);
    }

    // different template arguments
    {
        const int idata[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array iarr{ {3, dims}, idata };

        const double ddata[] = {
            1.1, 2.1,
            3.1, 4.1,
            5.1, 6.1 };
        computoc::Array<double> darr{ {3, dims}, ddata };

        Integer_array cdarr1{ darr };
        EXPECT_EQ(iarr, cdarr1);

        Integer_array cdarr2{};
        cdarr2 = darr;
        EXPECT_EQ(iarr, cdarr2);
    }

    // slice copying by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 5, 2} }) = starr3;
        EXPECT_EQ(tarr3, rarr3);
    }

    // slice copying by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        rarr3({ {0, 3, 2} }) = starr3;
        EXPECT_NE(tarr3, rarr3);
    }

    // slice copying by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        computoc::Array<double> starr3{ tarr3({ {0, 5, 2} }) };
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        rarr3 = starr3;
        EXPECT_NE(tarr3, rarr3);
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
    Integer_array sarr{ {3, dims}, data };

    Integer_array arr{ {3, dims}, data };
    Integer_array carr1{ std::move(arr) };
    EXPECT_EQ(sarr, carr1);
    EXPECT_TRUE(empty(arr));

    Integer_array carr2{};
    carr2 = std::move(carr1);
    EXPECT_EQ(sarr, carr2);
    EXPECT_TRUE(empty(carr1));

    Integer_array sarr2{ {3, dims}, data };
    carr2({ {0, 1}, {0, 0}, {0, 1} }) = std::move(sarr2);
    EXPECT_TRUE(empty(sarr2));
    EXPECT_EQ(sarr, carr2);

    // slice moving by assignment
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        rarr3({ {0, 5, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_EQ(tarr3, rarr3);
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        Integer_array srarr3{ rarr3({ {0, 5, 2} }) };
        srarr3 = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_NE(tarr3, rarr3);
        EXPECT_FALSE(empty(tarr3));
    }

    // different template arguments
    {
        const int idata[] = {
            1, 2,
            3, 4,
            5, 6 };
        const std::int64_t dims[]{ 3, 1, 2 };
        Integer_array iarr{ {3, dims}, idata };

        const double ddata[] = {
            1.1, 2.1,
            3.1, 4.1,
            5.1, 6.1 };
        computoc::Array<double> darr{ {3, dims}, ddata };

        Integer_array cdarr1{ std::move(darr) };
        EXPECT_EQ(iarr, cdarr1);
        EXPECT_TRUE(empty(darr));

        computoc::Array<double> cdarr2{};
        cdarr2 = std::move(cdarr1);
        EXPECT_EQ((Integer_array{ {3, dims}, idata }), cdarr2);
        EXPECT_TRUE(empty(cdarr1));
    }

    // slice moving by assignment (rvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const double rdata3[] = { 0, 2, 0, 4, 0, 6 };
        computoc::Array<double> rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        rarr3({ {0, 5, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_EQ(tarr3, rarr3);
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (rvalue) - different template arguments and different dimensions
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const double rdata3[] = { 0, 2, 0, 4, 0, 6 };
        computoc::Array<double> rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        rarr3({ {0, 3, 2} }) = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_NE(tarr3, rarr3);
        EXPECT_FALSE(empty(tarr3));
    }

    // slice moving by assignment (lvalue) - different template arguments
    {
        const int tdata3[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array tarr3{ {6}, tdata3 };

        const int rdata3[] = { 0, 2, 0, 4, 0, 6 };
        Integer_array rarr3{ {6}, rdata3 };

        EXPECT_NE(tarr3, rarr3);
        computoc::Array<double> srarr3{ rarr3({ {0, 5, 2} }) };
        srarr3 = std::move(tarr3({ {0, 5, 2} }));
        EXPECT_NE(tarr3, rarr3);
        EXPECT_FALSE(empty(tarr3));
    }
}

TEST(Array_test, clone)
{
    using Integer_array = computoc::Array<int>;

    Integer_array empty_arr{};
    Integer_array cempty_arr{ computoc::clone(empty_arr) };
    EXPECT_EQ(empty_arr, cempty_arr);

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array sarr{ {3, dims}, data };

    Integer_array carr{ computoc::clone(sarr) };
    EXPECT_EQ(carr, sarr);
    carr({ 0, 0, 0 }) = 0;
    EXPECT_NE(carr, sarr);

    Integer_array csubarr{ computoc::clone(sarr({{1, 1}, {0, 0}, {0, 0}})) };
    EXPECT_EQ(sarr({ {1, 1}, {0, 0}, {0, 0} }), csubarr);
    csubarr({ 0, 0, 0 }) = 5;
    EXPECT_NE(sarr({ {1, 1}, {0, 0}, {0, 0} }), csubarr);
}

TEST(Array_test, copy)
{
    using Integer_array = computoc::Array<int>;

    { // backward cases - copy from other array to created array
        Integer_array empty_arr{};
        Integer_array cempty_arr{};
        computoc::copy(Integer_array{}, cempty_arr);
        EXPECT_EQ(empty_arr, cempty_arr);

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
        EXPECT_NE(arr1, arr2);
        computoc::copy(arr2, arr1);
        EXPECT_EQ(arr1, arr2);

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        Integer_array arr3{ {3, 1, 2}, data3 };
        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_EQ(arr3, arr2);

        computoc::copy(arr2, arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_EQ(arr3, arr2);
    }

    { // forward cases - copy from created array to other array
        Integer_array empty_arr{};
        Integer_array cempty_arr{};
        computoc::copy(cempty_arr, empty_arr);
        EXPECT_EQ(empty_arr, cempty_arr);

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
        EXPECT_NE(arr1, arr2);
        computoc::copy(arr2, arr1);
        EXPECT_EQ(arr1, arr2);

        const int data3[] = {
            10, 12,
            6, 8,
            10, 12 };
        Integer_array arr3{ {3, 1, 2}, data3 };
        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 0}, {0, 0}, {0, 1} }));
        EXPECT_EQ(arr3, arr2);

        computoc::copy(arr2({ {2, 2}, {0, 0}, {0, 1} }), arr2({ {0, 1}, {0, 0}, {0, 1} }));
        EXPECT_EQ(arr3, arr2);

        computoc::copy(arr3({ {0, 0}, {0, 0}, {0, 1} }), arr2);
        EXPECT_EQ(arr3({ {0, 0}, {0, 0}, {0, 1} }), arr2);
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
        EXPECT_NE(arr1, arr2d);
        computoc::copy(arr2d, arr1);
        EXPECT_NE(arr1, arr2d);
    }

    // copy to different dimensions and same count
    {
        const int data[] = { 1, 2, 3, 4, 5, 6 };
        Integer_array arr{ {6}, data };

        Integer_array tarr{ {3, 1, 2}, data };

        Integer_array rarr{ {3, 1, 2}, 0 };
        EXPECT_NE(tarr, rarr);
        computoc::copy(arr, rarr);
        EXPECT_EQ(tarr, rarr);
    }
}

TEST(Array_test, reshape)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = {
        1, 2,
        3, 4,
        5, 6 };
    const std::int64_t dims[]{ 3, 1, 2 };
    Integer_array arr{ {3, dims}, data };

    {
        EXPECT_THROW(computoc::reshape(arr, {}), std::invalid_argument);
    }

    {
        EXPECT_EQ(Integer_array{}, computoc::reshape(Integer_array{}, {}));
    }

    {
        const int tdata[] = { 1, 2, 3, 4, 5, 6 };
        const std::int64_t tdims[]{ 6 };
        Integer_array tarr{ {1, tdims}, tdata };

        Integer_array rarr{ computoc::reshape(arr, { 6 }) };
        EXPECT_EQ(tarr, rarr);
        EXPECT_EQ(arr.data(), rarr.data());
    }

    {
        Integer_array rarr{ computoc::reshape(arr, { 3, 1, 2 }) };
        EXPECT_EQ(arr, rarr);
        EXPECT_EQ(arr.data(), rarr.data());
    }

    {
        const int tdata[] = { 1, 5 };
        const std::int64_t tdims[]{ 1, 2 };
        Integer_array tarr{ {2, tdims}, tdata };

        Integer_array rarr{ computoc::reshape(arr({{0, 2, 2}, {}, {}}), {1, 2}) };
        EXPECT_EQ(tarr, rarr);
        EXPECT_NE(arr.data(), rarr.data());
    }
}

TEST(Array_test, resize)
{
    using Integer_array = computoc::Array<int>;

    const int data[] = { 1, 2, 3, 4, 5, 6 };
    const std::int64_t dims[]{ 6 };
    Integer_array arr{ {1, dims}, data };

    {
        EXPECT_EQ(Integer_array{}, computoc::resize(arr, {}));
    }

    {
        Integer_array rarr{ computoc::resize(Integer_array{}, {6}) };
        EXPECT_NE(arr, rarr);
        EXPECT_EQ(arr.header().dims().s(), rarr.header().dims().s());
        EXPECT_EQ(6, rarr.header().dims().p()[0]);
        EXPECT_NE(arr.data(), rarr.data());
    }

    {
        Integer_array rarr{ computoc::resize(arr, {6}) };
        EXPECT_EQ(arr, rarr);
        EXPECT_NE(arr.data(), rarr.data());
    }

    {
        const int tdata[] = { 1, 2 };
        const std::int64_t tdims[]{ 2 };
        Integer_array tarr{ {1, tdims}, tdata };

        Integer_array rarr{ computoc::resize(arr, {2}) };
        EXPECT_EQ(tarr, rarr);
        EXPECT_NE(tarr.data(), rarr.data());
    }

    {
        const int tdata[] = { 
            1, 2,
            3, 4,
            5, 6};
        const std::int64_t tdims[]{ 3, 1, 2 };
        Integer_array tarr{ {3, tdims}, tdata };

        Integer_array rarr{ computoc::resize(arr, {3, 1, 2}) };
        EXPECT_EQ(tarr, rarr);
        EXPECT_NE(tarr.data(), rarr.data());
    }

    {
        Integer_array rarr{ computoc::resize(arr, {10}) };
        EXPECT_NE(arr, rarr);
        EXPECT_EQ(arr, rarr({ {0, 5} }));
        EXPECT_NE(arr.data(), rarr.data());
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
        EXPECT_EQ(rarr, computoc::append(arr1, arr2));

        EXPECT_EQ(arr1, computoc::append(arr1, Integer_array{}));
        EXPECT_EQ(arr2, computoc::append(Integer_array{}, arr2));
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
        EXPECT_EQ(rarr1, computoc::append(arr1, arr2, 0));

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
        EXPECT_EQ(rarr2, computoc::append(arr1, arr2, 1));

        const int rdata3[] = {
            1,  2,  3, 13, 14, 15,
            4,  5,  6, 16, 17, 18,
            
            7,  8,  9, 19, 20, 21,
            10, 11, 12, 22, 23, 24 };
            
        Integer_array rarr3{ { 2, 2, 6 }, rdata3 };
        EXPECT_EQ(rarr3, computoc::append(arr1, arr2, 2));

        EXPECT_EQ(arr1, computoc::append(arr1, Integer_array{}, 0));
        EXPECT_EQ(arr2, computoc::append(Integer_array{}, arr2, 0));

        EXPECT_THROW(computoc::append(arr1, arr2, 3), std::out_of_range);
        const int invalid_data1[] = { 1 };
        Integer_array invalid_arr1{ {1}, invalid_data1 };
        EXPECT_THROW(computoc::append(invalid_arr1, arr2, 0), std::invalid_argument);
        EXPECT_THROW(computoc::append(arr1, rarr2, 0), std::invalid_argument);
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
        Integer_array rarr{ {11}, rdata1 };
        EXPECT_EQ(rarr, computoc::insert(arr1, arr2, 3));
        EXPECT_THROW(computoc::insert(arr1, arr2, 7), std::out_of_range);

        EXPECT_EQ(arr1, computoc::insert(arr1, Integer_array{}, 0));
        EXPECT_EQ(arr2, computoc::insert(Integer_array{}, arr2, 0));
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
        EXPECT_EQ(rarr1, computoc::insert(arr1, arr2, 1, 0));
        EXPECT_THROW(computoc::insert(arr1, arr2, 3, 0), std::out_of_range);

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
        EXPECT_EQ(rarr2, computoc::insert(arr1, arr2, 1, 1));
        EXPECT_THROW(computoc::insert(arr1, arr2, 3, 1), std::out_of_range);

        const int rdata3[] = {
            1, 13, 14, 15,  2,  3,
            4, 16, 17, 18, 5,  6,

            7, 19, 20, 21, 8,  9,
            10, 22, 23, 24, 11, 12 };
        Integer_array rarr3{ { 2, 2, 6 }, rdata3 };
        EXPECT_EQ(rarr3, computoc::insert(arr1, arr2, 1, 2));
        EXPECT_THROW(computoc::insert(arr1, arr2, 4, 1), std::out_of_range);
        
        EXPECT_EQ(arr1, computoc::insert(arr1, Integer_array{}, 1, 0));
        EXPECT_EQ(arr2, computoc::insert(Integer_array{}, arr2, 1, 0));

        EXPECT_THROW(computoc::insert(arr1, arr2, 1, 3), std::out_of_range);
        const int invalid_data1[] = { 1 };
        Integer_array invalid_arr1{ {1}, invalid_data1 };
        EXPECT_THROW(computoc::insert(invalid_arr1, arr2, 1, 0), std::invalid_argument);
        EXPECT_THROW(computoc::insert(arr1, rarr2, 1, 0), std::invalid_argument);;
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
        Integer_array rarr{ {4}, rdata1 };
        EXPECT_EQ(rarr, computoc::remove(arr1, 3, 2));
        EXPECT_THROW(computoc::remove(arr1, 3, 4), std::out_of_range);

        EXPECT_EQ(Integer_array{}, computoc::remove(Integer_array{}, 3, 2));
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
        EXPECT_EQ(rarr1, computoc::remove(arr1, 0, 1, 0));
        EXPECT_THROW(computoc::remove(arr1, 0, 3, 0), std::out_of_range);

        const int rdata2[] = {
            1, 2, 3,

            7, 8, 9 };
        Integer_array rarr2{ { 2, 1, 3 }, rdata2 };
        EXPECT_EQ(rarr2, computoc::remove(arr1, 1, 1, 1));
        EXPECT_THROW(computoc::remove(arr1, 1, 2, 1), std::out_of_range);

        const int rdata3[] = {
            3,
            6,

            9,
            12 };
        Integer_array rarr3{ { 2, 2, 1 }, rdata3 };
        EXPECT_EQ(rarr3, computoc::remove(arr1, 0, 2, 2));
        EXPECT_THROW(computoc::remove(arr1, 2, 2, 2), std::out_of_range);

        EXPECT_THROW(computoc::remove(arr1, 0, 1, 3), std::out_of_range);
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
    Integer_array arr{ {5, dims}, reinterpret_cast<const int*>(data) };

    const int sdata1[1][1][1][2][1]{ { {{{47},{53}}} } };
    const std::int64_t sdims1[]{ 1, 1, 1, 2, 1 };
    Integer_array sarr1{ {5, sdims1}, reinterpret_cast<const int*>(sdata1) };

    EXPECT_EQ(sarr1, arr({ {1, 1}, {0, 0}, {1, 1}, {0, 2, 2}, {1, 2, 2} }));

    const int sdata2[1][1][1][1][1]{ { {{{53}}} } };
    const std::int64_t sdims2[]{ 1, 1, 1, 1, 1 };
    Integer_array sarr2{ {5, sdims2}, reinterpret_cast<const int*>(sdata2) };

    EXPECT_EQ(sarr2, sarr1({ {0, 0}, {0, 0}, {0, 0}, {1, 1}, {0, 0} }));
}
