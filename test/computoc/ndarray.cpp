#include <gtest/gtest.h>

#include <computoc/ndarray.h>


TEST(ND_array_header, initiazlization)
{
    using namespace computoc;

    //            N1 N2 N3   N4 N5
    //            <- <- <-  -> -> 
    const int data[2][2][2][2][3]{
        {{{{1, 2, 3},
        {4, 5, 6}},

        {{7, 8, 9},
        {10, 11, 12}}},


        {{{13, 14, 15},
        {16, 17, 18}},

        {{19, 20, 21},
        {22, 23, 24}}}},



        {{{{25, 26, 27},
        {28, 29, 30}},

        {{31, 32, 33},
        {34, 35, 36}}},


        {{{37, 38, 39},
        {40, 41, 42}},

        {{43, 44, 45},
        {46, 47, 48}}}}
    };


    ND_array<int> arr{ {2, 2, 2, 2, 3}, reinterpret_cast<const int*>(data) };
    const int* datap = reinterpret_cast<const int*>(data);

    for (std::size_t i1 = 0; i1 < 2; ++i1) {
        for (std::size_t i2 = 0; i2 < 2; ++i2) {
            for (std::size_t i3 = 0; i3 < 2; ++i3) {
                for (std::size_t i4 = 0; i4 < 2; ++i4) {
                    for (std::size_t i5 = 0; i5 < 3; ++i5) {
                        EXPECT_EQ(*datap, arr({ i1, i2, i3, i4, i5 }));
                        ++datap;
                    }
                }
            }
        }
    }

    ND_array<int> subarray1{ arr({ {1,1,1}, {0,1,2}, {0,0,1}, {0,1,1}, {1,2,2} }) };
    EXPECT_EQ(26, subarray1({ 0, 0, 0, 0, 0 }));
    EXPECT_EQ(29, subarray1({ 0, 0, 0, 1, 0 }));

    ND_array<int> subarray2{ subarray1({{0,0,1}, {0,0,1}, {0,0,1}, {1,1,2}, {0,0,1}}) };
    EXPECT_EQ(29, subarray2({ 0, 0, 0, 0, 0 }));
}
