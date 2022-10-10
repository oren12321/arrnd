#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstddef>
#include <initializer_list>

#include <memoc/allocators.h>
#include <memoc/buffers.h>
#include <memoc/pointers.h>

namespace computoc {
    namespace details {

        /*

        N-dimensional array:
        ====================
        Range:
        ------
        R(i) = {Rs(i)<start index>, Re(i)<end index>, Rt(i)<step>}

        Dimensions:
        -----------
        Dims = {N(1), N(2), ..., N(n-1)<rows>, N(n)<columns>}

        Dims to R:
        ----------
        R(i) = {0, N(i)-1, 1} for each i in {1, ..., n}

        Subscript(i) to R(i):
        ---------------------
        R(i) = {Sub(i), Sub(i), 1}

        Strides:
        --------
        S = {S(1), S(2), ..., S(n)<column stride>}

        R to S:
        -------
        S(n) = Rt(n),  S'(n) = 1
        For each i in {n, ..., 2}:
            S'(i) = S'(i+1) * (Re(i+1) + 1)
            S(i) = S'(i+1) * (Re(i+1) + 1) * Rt(i)

        Offsets:
        --------
        O(i) = Rs(i) for each i in {1, ..., n}
        Total offset: Toffset = Toffset' + O(1)*S(1) + O(2)*S(2) + ... + O(n)*S(n) - 1
        Toffset' is the previous subarray offset, otherwise is equal to 0

        R to Dims:
        ----------
        Dims(i) = ceil((Re(i) - Rs(i) + 1) / Rt(i)) for each i in {1, ..., n}

        Subs to Index:
        --------------
        Index = Subs(1)*S(1) + Subs(2)*S(2) + ... + Subs(n)*S(n) + offset

        
        Example:
        ========
        Dims = {2, 2, 2, 2, 3}
        n = 5
        Data =
        {
            {{{{1, 2, 3},
            {4, 5, 6}},

            {{7, 8, 9},
            {10, 11, 12}}},


            {{{13, 14, 15},
            {16, 17, 18}},

            {{19, 20, 21},
            {22, 23, 24}}}},



            {{{{25, <26>, 27},
            {28, <29>, 30}},

            {{31, 32, 33},
            {34, 35, 36}}},


            {{{37, 38, 39},
            {40, 41, 42}},

            {{43, 44, 45},
            {46, 47, 48}}}}
        };
        Ranges from Dims: R = {{0,1,1}, {0,1,1}, {0,1,1}, {0,1,1}, {0,2,1}}
        Strides: S' = {24, 12, 6, 3, 1}, S = {24, 12, 6, 3, 1}
                 offset = 0
        --------
        S(5) = 1, S'(5) = 1
        S'(4) = S'(5) * (Re(5) + 1) = 1 * (2 + 1) = 3
        S(4) = S'(5) * (Re(5) + 1) * Rt(4) = 1 * (2 + 1) * 1 = 3
        S'(3) = S'(4) * (Re(4) + 1) = 3 * (1 + 1) = 6
        S(3) = S'(4) * (Re(4) + 1) * Rt(3) = 3 * (1 + 1) * 1 = 6
        S'(2) = S'(3) * (Re(3) + 1) = 6 * (1 + 1) = 12
        S(2) = S'(3) * (Re(3) + 1) * Rt(2) = 6 * (1 + 1) * 1 = 12
        S'(1) = S'(2) * (Re(2) + 1) = 12 * (1 + 1) = 24
        S(1) = S'(2) * (Re(2) + 1) * Rt(1) = 12 * (1 + 1) * 1 = 24
        
        Ranges of subarray 1: Rsub1 = {{1,1,1}, {0,1,2}, {0,0,1}, {0,1,1}, {1,2,2}}
                              D = {1, 1, 1, 2, 1}
        Strides: S' = {12, 6, 6, 3, 1}, S = {24, 12, 6, 3, 2}
                 O = {1, 0, 0, 0, 1}
                 offset1 = 1*24 + 0*12 + 0*6 + 0*3 + 1*2 - 1 = 25
                 Ind example:
                     {0, 0, 0, 0, 0} => 25 + 0 = 25
                     {0, 0, 0, 1, 0} => 25 + 3 = 28
        --------
        S(5) = 2, S'(5) = 1
        S'(4) = S'(5) * (Rsub1e(5) + 1) = 1 * (2 + 1) = 3
        S(4) = S'(5) * (Rsub1e(5) + 1) * Rsub1t(4) = 1 * (2 + 1) * 1 = 3
        S'(3) = S'(4) * (Rsub1e(4) + 1) = 3 * (1 + 1) = 6
        S(3) = S'(4) * (Rsub1e(4) + 1) * Rsub1t(3) = 3 * (1 + 1) * 1 = 6
        S'(2) = S'(3) * (Rsub1e(3) + 1) = 6 * (0 + 1) = 6
        S(2) = S'(3) * (Rsub1e(3) + 1) * Rsub1t(2) = 6 * (0 + 1) * 2 = 12
        S'(1) = S'(2) * (Rsub1e(2) + 1) = 6 * (1 + 1) = 12
        S(1) = S'(2) * (Rsub1e(2) + 1) * Rsub1t(1) = 12 * (1 + 1) * 1 = 24

        Ranges of subarray 2: Rsub2 = {{0,0,1}, {0,0,1}, {0,0,1}, {1,1,2}, {0,0,1}}
                              D = {1, 1, 1, 1, 1}
        Strides: S' = {2, 2, 2, 1, 1}, S = {2, 2, 2, 2, 1}
                 O = {1, 0, 0, 1, 0}
                 offset2 = offset1 + 1*2 + 0*2 + 0*2 + 1*2 + 0*1 - 1 = 25 + 4 - 1 = 28
                 Ind example:
                     {0, 0, 0, 0, 0} => 28 + 0 = 28
        S(5) = 1, S'(5) = 1
        S'(4) = S'(5) * (Rsub2e(5) + 1) = 1 * (0 + 1) = 1
        S(4) = S'(5) * (Rsub2e(5) + 1) * Rsub2t(4) = 1 * (0 + 1) * 2 = 2
        S'(3) = S'(4) * (Rsub2e(4) + 1) = 1 * (1 + 1) = 2
        S(3) = S'(4) * (Rsub2e(4) + 1) * Rsub2t(3) = 1 * (1 + 1) * 1 = 2
        S'(2) = S'(3) * (Rsub2e(3) + 1) = 2 * (0 + 1) = 2
        S(2) = S'(3) * (Rsub2e(3) + 1) * Rsub2t(2) = 2 * (0 + 1) * 1 = 2
        S'(1) = S'(2) * (Rsub2e(2) + 1) = 2 * (0 + 1) = 2
        S(1) = S'(2) * (Rsub2e(2) + 1) * Rsub2t(1) = 2 * (0 + 1) * 1 = 2
        */

        using ND_dims = std::initializer_list<std::size_t>;

        struct ND_range {
            std::size_t start_index{ 0 }, stop_index{ 0 }, step{ 1 };
        };
        using ND_ranges = std::initializer_list<ND_range>;

        ND_range dim2range(std::size_t dim)
        {
            return { 0, dim - 1, 1 };
        }

        struct ND_sub {
            std::size_t start_index{ 0 };
            std::size_t stop_index{ 0 };
            std::size_t step{ 1 };
        };

        using ND_dims = std::initializer_list<std::size_t>;
        using ND_strides = std::initializer_list<std::size_t>;
        using ND_subs = std::initializer_list<std::size_t>;

        using ND_array_allocator = memoc::Malloc_allocator;

        using ND_header_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<(3 + 3) * sizeof(std::size_t)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <memoc::Buffer<std::size_t> Internal_buffer = ND_header_buffer>
        class ND_array_header {
        public:
            ND_array_header() = default;

            ND_array_header(ND_dims dims, std::size_t offset = 0, bool is_subarray = false)
                : size_info_(dims.size() * 2), ndims_(dims.size()), offset_(offset), is_subarray_(is_subarray)
            {
                std::size_t* dimsp = size_info_.data().p;
                std::size_t* stridesp = dimsp + ndims_;

                for (std::size_t i = 0; i < ndims_; ++i) {
                    dimsp[i] = dims.begin()[i];
                }

                count_ = 1;
                for (std::size_t i = 0; i < ndims_; ++i) {
                    count_ *= dimsp[i];
                }

                std::size_t current_stride{ 1 };
                for (std::size_t i = ndims_; i >= 1; --i) {
                    stridesp[i - 1] = current_stride;
                    current_stride *= dimsp[i - 1];
                }
            }

            ND_array_header(ND_array_header<Internal_buffer>&& other) = default;
            ND_array_header<Internal_buffer>& operator=(ND_array_header<Internal_buffer>&& other) = default;

            ND_array_header(const ND_array_header<Internal_buffer>& other) = default;
            ND_array_header<Internal_buffer>& operator=(const ND_array_header<Internal_buffer>& other) = default;

            virtual ~ND_array_header() = default;

            std::size_t ndims() const
            {
                return ndims_;
            }

            std::size_t count() const
            {
                return count_;
            }

            const std::size_t* dims() const
            {
                return size_info_.data().p;
            }

            const std::size_t* strides() const
            {
                return size_info_.data().p + ndims_;
            }

            std::size_t offset() const
            {
                return offset_;
            }

            bool is_subarray() const
            {
                return is_subarray_;
            }

        private:
            Internal_buffer size_info_{};
            std::size_t ndims_{ 0 };
            std::size_t count_{ 0 };
            std::size_t offset_{ 0 };
            bool is_subarray_{ false };
        };

        std::size_t subs2ind(std::size_t ndims, const std::size_t* subs, const std::size_t* strides, std::size_t offset = 0)
        {
            std::size_t ind{ offset };
            for (std::size_t i = 0; i < ndims; ++i) {
                ind += subs[i] * strides[i];
            }
            return ind;
        }

        using ND_array_allocator = memoc::Malloc_allocator;

        template <typename T>
        using ND_array_buffer = memoc::Typed_buffer<T, memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * sizeof(T)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <typename T, memoc::Buffer<T> Internal_data_buffer = ND_array_buffer<T>, memoc::Allocator Internal_allocator = ND_array_allocator, memoc::Buffer<std::size_t> Internal_header_buffer = ND_header_buffer>
        class ND_array {
        public:
            ND_array() = default;

            ND_array(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>&& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>&& other) = default;

            ND_array(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& other) = default;

            virtual ~ND_array() = default;

            ND_array(ND_dims dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count(), data))
            {
            }

            ND_array(ND_dims dims, const T& value)
                : hdr_(dims), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count()))
            {
                for (std::size_t i = 0; i < buffsp_->data().s; ++i) {
                    buffsp_->data().p[i] = value;
                }
            }

            const ND_array_header<Internal_header_buffer>& header() const
            {
                return hdr_;
            }

            const T& operator()(ND_subs inds) const
            {
                return buffsp_->data().p[subs2ind(hdr_.ndims(), inds.begin(), hdr_.strides(), hdr_.offset())];
            }

            T& operator()(ND_subs inds)
            {
                return buffsp_->data().p[subs2ind(hdr_.ndims(), inds.begin(), hdr_.strides(), hdr_.offset())];
            }

            //ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer> operator()(ND_subs subs, )

        private:
            ND_array_header<Internal_header_buffer> hdr_{};
            memoc::Shared_ptr<Internal_data_buffer, Internal_allocator> buffsp_{ nullptr };
        };
    }

    using details::ND_array;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
