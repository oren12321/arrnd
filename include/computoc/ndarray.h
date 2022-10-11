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
        * N-dimensional array definitions:
        * ================================
        * 
        * N - number of dimensions
        * All sizes of defined groups are proportional to N.
        * 
        * Dimensions:
        * -----------
        * D = {n(1), n(2), ..., n(N)}
        * The dimensions order is from the larges to the smallest (i.e. columns).
        * 
        * Ranges:
        * -------
        * R = {(Rs(1),Re(1),Rt(1)), (Rs(2),Re(2),Rt(2)), ..., (Rs(N),Re(N),Rt(N))}
        * s.t. Rs - start index
        *      Re - stop index
        *      Rt - step
        * 
        * Strides:
        * --------
        * S = {s(1), s(2), ..., s(N)}
        * s.t. s(i) - the elements count between two consecutive elements of the i(th) dimension.
        * 
        * Offset:
        * -------
        * The start index of the current array or sub-array.
        * Has value of 0 for the base array.
        * 
        * Subscripts:
        * -----------
        * I = {I(1), I(2), ..., I(N)}
        * s.t. I is a group of subscripts of specific value in the array.
        */

        using ND_dim = std::size_t;
        struct ND_range {
            std::size_t start{ 0 }, stop{ 0 }, step{ 1 };
        };
        using ND_stride = std::size_t;
        using ND_offset = std::size_t;
        using ND_subscript = std::size_t;
        using ND_index = std::size_t;

        /*
        * N-dimensional array indexing:
        * =============================
        * 
        * Dimensions to strides:
        * ----------------------
        * Relates to the original array dimensions for the base strides calculation.
        * s(N) = 1
        * s(i) = f(S,D) = s(i+1) * n(i+1) - for every i in {1, 2, ..., N-1}
        * 
        * Ranges to strides:
        * ------------------
        * Relates to calculation of sub-array strides.
        * s(i) = f(S',Rt) = s'(i) * Rt(i) for every i in {1, 2, ..., N}
        * s.t. S' are the previously calculted strides.
        * 
        * Ranges to dimensions:
        * ---------------------
        * n(i) = f(R) = ceil((Re(i) - Rs(i) + 1) / Rt(i)) for every i in {1, 2, ..., N}
        * 
        * Offset:
        * -------
        * offset = f(offset', S',Rs) = offset' + dot(S',Rs)
        * s.t. offset' - previously calculated offset
        *      S'      - vector of previously calculated strides
        *      Rs      - vector of ranges start indices
        * 
        * Index:
        * ------
        * index = f(offset, S, I) = offset + dot(S,I)
        * 
        * Number of elements in array:
        * ----------------------------
        * count = f(D) = n(1) * n(2) * ... n(N)
        */

        void dims2strides(std::size_t ndims, const ND_dim* dims, ND_stride* strides)
        {
            if (ndims > 0) {
                strides[ndims - 1] = 1;
                for (std::size_t i = ndims - 1; i >= 1; --i) {
                    strides[i - 1] = strides[i] * dims[i];
                }
            }
        }

        void ranges2strides(std::size_t ndims, const ND_stride* previous_strides, const ND_range* ranges, ND_stride* strides)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                strides[i] = previous_strides[i] * ranges[i].step;
            }
        }

        void ranges2dims(std::size_t ndims, const ND_range* ranges, ND_dim* dims)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                dims[i] = static_cast<std::size_t>(std::ceil((ranges[i].stop - ranges[i].start + 1.0) / ranges[i].step));
            }
        }

        void ranges2offset(std::size_t ndims, ND_offset previous_offset, const ND_stride* previous_strides, const ND_range* ranges, ND_offset* offset)
        {
            *offset = previous_offset;
            for (std::size_t i = 0; i < ndims; ++i) {
                *offset += previous_strides[i] * ranges[i].start;
            }
        }

        void subs2ind(std::size_t ndims, ND_offset offset, const ND_stride* strides, const ND_subscript* subs, ND_index* ind)
        {
            *ind = offset;
            for (std::size_t i = 0; i < ndims; ++i) {
                *ind += strides[i] * subs[i];
            }
        }

        void dims2count(std::size_t ndims, const ND_dim* dims, std::size_t* count)
        {
            *count = 1;
            for (std::size_t i = 0; i < ndims; ++i) {
                *count *= dims[i];
            }
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

        using ND_array_allocator = memoc::Malloc_allocator;

        using ND_header_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (sizeof(ND_dim) + sizeof(ND_stride))>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <memoc::Buffer<std::size_t> Internal_buffer = ND_header_buffer>
        class ND_array_header {
        public:
            ND_array_header() = default;

            ND_array_header(std::size_t ndims, const ND_range* ranges, const ND_stride* strides, ND_offset offset, bool is_subarray)
                : ndims_(ndims), size_info_(ndims * (3 * (sizeof(ND_dim) + sizeof(ND_stride)))), is_subarray_(is_subarray)
            {
                COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                std::size_t* dimsp = size_info_.data().p;
                ranges2dims(ndims_, ranges, dimsp);

                dims2count(ndims_, dimsp, &count_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                std::size_t* stridesp = size_info_.data().p + ndims_;
                ranges2strides(ndims_, strides, ranges, stridesp);

                ranges2offset(ndims_, offset, strides, ranges, &offset_);
            }

            ND_array_header(std::size_t ndims, const ND_dim* dims)
                : ndims_(ndims), size_info_(ndims* (2 * sizeof(std::size_t) + sizeof(ND_range)))
            {
                COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                std::size_t* dimsp = size_info_.data().p;
                for (std::size_t i = 0; i < ndims_; ++i) {
                    dimsp[i] = dims[i];
                }

                dims2count(ndims_, dimsp, &count_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                std::size_t* stridesp = size_info_.data().p + ndims_;
                dims2strides(ndims_, dimsp, stridesp);
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
            std::size_t ndims_{ 0 };
            Internal_buffer size_info_{};
            std::size_t count_{ 0 };
            ND_offset offset_{ 0 };
            bool is_subarray_{ false };
        };

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

            ND_array(std::initializer_list<ND_dim> dims, const T* data = nullptr)
                : hdr_(dims.size(), dims.begin()), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count(), data))
            {
            }

            ND_array(std::initializer_list<ND_dim> dims, const T& value)
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

            const T& operator()(std::initializer_list<ND_subscript> subs) const
            {
                ND_index ind{ 0 };
                subs2ind(hdr_.ndims(), hdr_.offset(), hdr_.strides(), subs.begin(), &ind);
                return buffsp_->data().p[ind];
            }

            T& operator()(std::initializer_list<ND_subscript> subs)
            {
                ND_index ind{ 0 };
                subs2ind(hdr_.ndims(), hdr_.offset(), hdr_.strides(), subs.begin(), &ind);
                return buffsp_->data().p[ind];
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer> operator()(std::initializer_list<ND_range> ranges)
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
