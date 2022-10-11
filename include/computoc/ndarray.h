#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>


#include <computoc/errors.h>
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
        For each i in {n-1, ..., 1}:
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

        */

        using ND_dims = std::initializer_list<std::size_t>;

        struct ND_range {
            std::size_t start{ 0 }, stop{ 0 }, step{ 1 };
        };
        using ND_ranges = std::initializer_list<ND_range>;

        void dims2ranges(std::size_t ndims, const std::size_t* dims, ND_range* ranges)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                ranges[i] = { 0, dims[i] - 1, 1 };
            }
        }

        void ranges2dims(std::size_t ndims, const ND_range* ranges, std::size_t* dims)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                dims[i] = static_cast<std::size_t>(std::ceil((ranges[i].stop - ranges[i].start + 1.0) / ranges[i].step));
            }
        }

        void ranges2strides(std::size_t ndims, const ND_range* ranges, std::size_t* strides)
        {
            std::size_t current_stride_prime{ 1 }, current_stride{ ranges[ndims - 1].step };
            strides[ndims - 1] = current_stride;
            for (std::size_t i = ndims - 1; i >= 1; --i) {
                std::size_t prev_stride_prime{ current_stride_prime };
                current_stride_prime = prev_stride_prime * (ranges[i].stop + 1);
                current_stride = prev_stride_prime * (ranges[i].stop + 1) * ranges[i - 1].step;
                strides[i - 1] = current_stride;
            }
        }

        void ranges2strides(std::size_t ndims, const ND_range* ranges, const std::size_t* prev_strides, std::size_t* strides)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                strides[i] = prev_strides[i] * ranges[i].step;
            }
        }

        std::size_t ranges2offset(std::size_t ndims, const ND_range* ranges, const std::size_t* strides, std::size_t current_offset = 0)
        {
            std::size_t offset{ current_offset };
            for (std::size_t i = 0; i < ndims; ++i) {
                offset += ranges[i].start * strides[i];
            }
            //return (offset == 0 ? 0 : offset - 1);
            return offset;
        }

        std::size_t subs2index(std::size_t ndims, const std::size_t* subs, const std::size_t* strides, std::size_t offset = 0)
        {
            std::size_t ind{ offset };
            for (std::size_t i = 0; i < ndims; ++i) {
                ind += subs[i] * strides[i];
            }
            return ind;
        }

        /*
        
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
        using ND_strides = std::initializer_list<std::size_t>;
        using ND_subs = std::initializer_list<std::size_t>;

        using ND_array_allocator = memoc::Malloc_allocator;

        using ND_header_buffer = memoc::Typed_buffer<std::uint8_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (2 * sizeof(std::size_t) + sizeof(ND_range))>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <memoc::Buffer<std::uint8_t> Internal_buffer = ND_header_buffer>
        class ND_array_header {
        public:
            ND_array_header() = default;

            ND_array_header(std::size_t ndims, const ND_range* ranges, const std::size_t* strides, std::size_t offset, bool is_subarray)
                : size_info_(ndims * (2 * sizeof(std::size_t) + sizeof(ND_range))), ndims_(ndims), is_subarray_(is_subarray)
            {
                COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                ND_range* rangesp = reinterpret_cast<ND_range*>(size_info_.data().p + 2 * ndims_ * sizeof(std::size_t));
                for (std::size_t i = 0; i < ndims_; ++i) {
                    rangesp[i] = ranges[i];
                }

                std::size_t* dimsp = reinterpret_cast<std::size_t*>(size_info_.data().p);
                ranges2dims(ndims_, rangesp, dimsp);

                count_ = 1;
                for (std::size_t i = 0; i < ndims_; ++i) {
                    count_ *= dimsp[i];
                }
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                std::size_t* stridesp = reinterpret_cast<std::size_t*>(size_info_.data().p + ndims_ * sizeof(std::size_t));

                ranges2strides(ndims_, rangesp, strides, stridesp);
                offset_ = ranges2offset(ndims_, rangesp, strides, offset);
            }

            ND_array_header(std::size_t ndims, const std::size_t* dims)
                : size_info_(ndims* (2 * sizeof(std::size_t) + sizeof(ND_range))), ndims_(ndims)
            {
                COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                std::size_t* dimsp = reinterpret_cast<std::size_t*>(size_info_.data().p);
                for (std::size_t i = 0; i < ndims_; ++i) {
                    dimsp[i] = dims[i];
                }

                count_ = 1;
                for (std::size_t i = 0; i < ndims_; ++i) {
                    count_ *= dimsp[i];
                }
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                ND_range* rangesp = reinterpret_cast<ND_range*>(size_info_.data().p + 2 * ndims_ * sizeof(std::size_t));
                dims2ranges(ndims_, dims, rangesp);

                std::size_t* stridesp = reinterpret_cast<std::size_t*>(size_info_.data().p + ndims_ * sizeof(std::size_t));
                ranges2strides(ndims_, rangesp, stridesp);
                offset_ = ranges2offset(ndims_, rangesp, stridesp, 0);
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
                return reinterpret_cast<std::size_t*>(size_info_.data().p);
            }

            const std::size_t* strides() const
            {
                return reinterpret_cast<std::size_t*>(size_info_.data().p + ndims_ * sizeof(std::size_t));
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

        using ND_array_allocator = memoc::Malloc_allocator;

        template <typename T>
        using ND_array_buffer = memoc::Typed_buffer<T, memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * sizeof(T)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <typename T, memoc::Buffer<T> Internal_data_buffer = ND_array_buffer<T>, memoc::Allocator Internal_allocator = ND_array_allocator, memoc::Buffer<std::uint8_t> Internal_header_buffer = ND_header_buffer>
        class ND_array {
        public:
            ND_array() = default;

            ND_array(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>&& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>&& other) = default;

            ND_array(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer>& other) = default;

            virtual ~ND_array() = default;

            ND_array(ND_dims dims, const T* data = nullptr)
                : hdr_(dims.size(), dims.begin()), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count(), data))
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
                return buffsp_->data().p[subs2index(hdr_.ndims(), inds.begin(), hdr_.strides(), hdr_.offset())];
            }

            T& operator()(ND_subs inds)
            {
                return buffsp_->data().p[subs2index(hdr_.ndims(), inds.begin(), hdr_.strides(), hdr_.offset())];
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer> operator()(ND_ranges ranges)
            {
                ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer> slice{};
                slice.hdr_ = ND_array_header<Internal_header_buffer>{ ranges.size(), ranges.begin(), hdr_.strides(), hdr_.offset(), true };
                slice.buffsp_ = buffsp_;

                return slice;
            }

        private:
            ND_array_header<Internal_header_buffer> hdr_{};
            memoc::Shared_ptr<Internal_data_buffer, Internal_allocator> buffsp_{ nullptr };
        };
    }

    using details::ND_array;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
