#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>


#include <computoc/errors.h>
#include <computoc/utils.h>
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

        struct ND_range {
            std::size_t start{ 0 }, stop{ start }, step{ 1 };
        };

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
        * 
        * Check if subscripts inside array dimensions:
        * ---------------------------------------
        * are_inside = f(D,I) = I(1)<D(1) and I(2)<D(2) and ... and I(N)<D(N)
        * 
        * Check if ranges are legal:
        * --------------------------
        * are_legal = f(R) = Rs(1)<=Re(1) and Rs(2)<=Re(2) and ... and Rs(N)<=Re(N)
        * 
        * Check if ranges inside array dimensions:
        * ----------------------------------------
        * are_inside = f(D,R) = Re(1)<D(1) and Re(2)<D(2) and ... and Re(N)<D(N)
        * 
        * Check if two dimensions are equal:
        * ----------------------------------
        * are_equal = f(D1,D2) = D1(1)=D2(1) and D1(2)=D2(2) and ... and D1(N)=D2(N)
        */

        inline void dims2strides(std::size_t ndims, const std::size_t* dims, std::size_t* strides)
        {
            if (ndims > 0) {
                strides[ndims - 1] = 1;
                for (std::size_t i = ndims - 1; i >= 1; --i) {
                    strides[i - 1] = strides[i] * dims[i];
                }
            }
        }

        inline void ranges2strides(std::size_t ndims, const std::size_t* previous_strides, const ND_range* ranges, std::size_t* strides)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                strides[i] = previous_strides[i] * ranges[i].step;
            }
        }

        inline void ranges2dims(std::size_t ndims, const ND_range* ranges, std::size_t* dims)
        {
            for (std::size_t i = 0; i < ndims; ++i) {
                dims[i] = static_cast<std::size_t>(std::ceil((ranges[i].stop - ranges[i].start + 1.0) / ranges[i].step));
            }
        }

        inline std::size_t ranges2offset(std::size_t ndims, std::size_t previous_offset, const std::size_t* previous_strides, const ND_range* ranges)
        {
            std::size_t offset{ previous_offset };
            for (std::size_t i = 0; i < ndims; ++i) {
                offset += previous_strides[i] * ranges[i].start;
            }
            return offset;
        }

        inline std::size_t subs2ind(std::size_t ndims, std::size_t offset, const std::size_t* strides, const std::size_t* subs)
        {
            std::size_t ind{ offset };
            for (std::size_t i = 0; i < ndims; ++i) {
                ind += strides[i] * subs[i];
            }
            return ind;
        }

        inline std::size_t dims2count(std::size_t ndims, const std::size_t* dims)
        {
            std::size_t count{ 1 };
            for (std::size_t i = 0; i < ndims; ++i) {
                count *= dims[i];
            }
            return count;
        }

        inline bool subs_in_dims(std::size_t ndims, const std::size_t* dims, const std::size_t* subs)
        {
            bool result{ true };
            for (std::size_t i = 0; i < ndims && result; ++i) {
                result &= (subs[i] < dims[i]);
            }
            return result;
        }

        inline bool legal_ranges(std::size_t ndims, const ND_range* ranges)
        {
            bool result{ true };
            for (std::size_t i = 0; i < ndims && result; ++i) {
                result &= (ranges[i].start <= ranges[i].stop);
            }
            return result;
        }

        inline bool ranges_in_dims(std::size_t ndims, const std::size_t* dims, const ND_range* ranges)
        {
            bool result{ true };
            for (std::size_t i = 0; i < ndims && result; ++i) {
                result &= (ranges[i].stop < dims[i]);
            }
            return result;
        }

        inline bool equal_dims(std::size_t ndims1, const std::size_t* dims1, std::size_t ndims2, const std::size_t* dims2)
        {
            bool result{ ndims1 == ndims2 };
            std::size_t ndims{ ndims1 };
            for (std::size_t i = 0; i < ndims && result; ++i) {
                result &= (dims1[i] == dims2[i]);
            }
            return result;
        }

        /*
        Example:
        ========
        D = {2, 2, 2, 2, 3}
        N = 5
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
        }
        S = {24, 12, 6, 3, 1}
        offset = 0

        Subarray 1:
        -----------
        R = {{1,1,1}, {0,1,2}, {0,0,1}, {0,1,1}, {1,2,2}}
        D = {1, 1, 1, 2, 1}
        N = 5
        Data =
        {
            {{{{26},
               {29}}}}
        }
        S = {24, 24, 6, 3, 2}
        offset = 25

        Subarray 2 (from subarray 1):
        -----------------------------
        R = {{0,0,1}, {0,0,1}, {0,0,1}, {1,1,2}, {0,0,1}}
        D = {1, 1, 1, 1, 1}
        N = 5
        Data =
        {
            {{{{29}}}}
        }
        S = {24, 24, 6, 6, 2}
        offset = 28
        */

        using ND_array_allocator = memoc::Malloc_allocator;

        using ND_header_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (sizeof(std::size_t) + sizeof(std::size_t))>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <typename T>
        using ND_array_buffer = memoc::Typed_buffer<T, memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * sizeof(T)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        using ND_subscriptor_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * sizeof(std::size_t)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <typename T, memoc::Buffer<T> Internal_data_buffer = ND_array_buffer<T>, memoc::Allocator Internal_allocator = ND_array_allocator, memoc::Buffer<std::size_t> Internal_header_buffer = ND_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer = ND_subscriptor_buffer>
        class ND_array {
        public:
            class Header {
            public:
                Header() = default;

                Header(std::size_t ndims, const ND_range* ranges, const std::size_t* strides, std::size_t offset, bool is_subarray)
                    : ndims_(ndims), size_info_(ndims*2), is_subarray_(is_subarray)
                {
                    COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                    COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                    std::size_t* dimsp = size_info_.data().p;
                    ranges2dims(ndims_, ranges, dimsp);

                    count_ = dims2count(ndims_, dimsp);
                    COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                    std::size_t* stridesp = size_info_.data().p + ndims_;
                    ranges2strides(ndims_, strides, ranges, stridesp);

                    offset_ = ranges2offset(ndims_, offset, strides, ranges);
                }

                Header(std::size_t ndims, const std::size_t* dims)
                    : ndims_(ndims), size_info_(ndims*2)
                {
                    COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                    COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                    std::size_t* dimsp = size_info_.data().p;
                    for (std::size_t i = 0; i < ndims_; ++i) {
                        dimsp[i] = dims[i];
                    }

                    count_ = dims2count(ndims_, dimsp);
                    COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                    std::size_t* stridesp = size_info_.data().p + ndims_;
                    dims2strides(ndims_, dimsp, stridesp);
                }

                Header(Header&& other) = default;
                Header& operator=(Header&& other) = default;

                Header(const Header& other) = default;
                Header& operator=(const Header& other) = default;

                virtual ~Header() = default;

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
                Internal_header_buffer size_info_{};
                std::size_t count_{ 0 };
                std::size_t offset_{ 0 };
                bool is_subarray_{ false };
            };


            class ND_subscriptor
            {
            public:
                ND_subscriptor(std::size_t ndims, const std::size_t* dims)
                    : ndims_(ndims), buff_(ndims), to_(dims)
                {
                    COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "failed to allocate subsscriptor buffer");
                    subs_ = buff_.data().p;
                    reset();
                }

                void reset()
                {
                    for (std::size_t i = 0; i < ndims_; ++i) {
                        subs_[i] = 0;
                    }
                }

                ND_subscriptor& operator++() {
                    bool should_process_sub{ true };
                    for (std::size_t i = ndims_; i >= 1 && should_process_sub; --i)
                    {
                        should_process_sub = (++subs_[i - 1] == to_[i - 1]);
                        if (should_process_sub)
                        {
                            if (i > 1)
                            {
                                subs_[i - 1] = 0;
                            }
                        }
                    }
                    return *this;
                }

                operator bool()
                {
                    return subs_[0] != to_[0];
                }

                std::size_t ndims() const
                {
                    return ndims_;
                }

                const std::size_t* subs() const
                {
                    return subs_;
                }

            private:
                std::size_t ndims_{ 0 };
                Internal_subscriptor_buffer buff_{};
                std::size_t* subs_{ nullptr };
                const std::size_t* to_{ nullptr };
            };


            ND_array() = default;

            ND_array(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>&& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>&& other)
            {
                COMPUTOC_THROW_IF_FALSE(!hdr_.is_subarray(), std::runtime_error, "move assignment to subarray is undefined");

                if (this == &other) {
                    return *this;
                }

                hdr_ = std::move(other.hdr_);
                buffsp_ = std::move(other.buffsp_);

                return *this;
            }

            ND_array(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& other)
            {
                COMPUTOC_THROW_IF_FALSE(!hdr_.is_subarray(), std::runtime_error, "copy assignment to subarray is undefined");

                if (this == &other) {
                    return *this;
                }

                hdr_ = other.hdr_;
                buffsp_ = other.buffsp_;

                return *this;
            }

            virtual ~ND_array() = default;

            ND_array(std::size_t ndims, const std::size_t* dims, const T* data = nullptr)
                : hdr_(ndims, dims), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count(), data))
            {
            }
            ND_array(std::initializer_list<std::size_t> dims, const T* data = nullptr)
                : ND_array(dims.size(), dims.begin(), data)
            {
            }

            ND_array(std::size_t ndims, const std::size_t* dims, const T& value)
                : hdr_(ndims, dims), buffsp_(memoc::make_shared<Internal_data_buffer, Internal_allocator>(hdr_.count()))
            {
                for (std::size_t i = 0; i < buffsp_->data().s; ++i) {
                    buffsp_->data().p[i] = value;
                }
            }
            ND_array(std::initializer_list<std::size_t> dims, const T& value)
                : ND_array(dims.size(), dims.begin(), value)
            {
            }

            const Header& header() const
            {
                return hdr_;
            }

            T* data() const
            {
                return (buffsp_ ? buffsp_->data().p : nullptr);
            }

            const T& operator()(std::size_t nsubs, const std::size_t* subs) const
            {
                COMPUTOC_THROW_IF_FALSE(hdr_.ndims() == nsubs, std::invalid_argument, "number of subscripts different from number of dimensions");
                COMPUTOC_THROW_IF_FALSE(subs_in_dims(hdr_.ndims(), hdr_.dims(), subs), std::out_of_range, "out of range subscripts");
                return buffsp_->data().p[subs2ind(hdr_.ndims(), hdr_.offset(), hdr_.strides(), subs)];
            }
            const T& operator()(std::initializer_list<std::size_t> subs) const
            {
                return (*this)(subs.size(), subs.begin());
            }

            T& operator()(std::size_t nsubs, const std::size_t* subs)
            {
                COMPUTOC_THROW_IF_FALSE(hdr_.ndims() == nsubs, std::invalid_argument, "number of subscripts different from number of dimensions");
                COMPUTOC_THROW_IF_FALSE(subs_in_dims(hdr_.ndims(), hdr_.dims(), subs), std::out_of_range, "out of range subscripts");
                return buffsp_->data().p[subs2ind(hdr_.ndims(), hdr_.offset(), hdr_.strides(), subs)];
            }
            T& operator()(std::initializer_list<std::size_t> subs)
            {
                return (*this)(subs.size(), subs.begin());
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> operator()(std::size_t nranges, const ND_range* ranges)
            {
                COMPUTOC_THROW_IF_FALSE(hdr_.ndims() == nranges, std::invalid_argument, "number of ranges different from number of dimensions");
                COMPUTOC_THROW_IF_FALSE(legal_ranges(hdr_.ndims(), ranges), std::invalid_argument, "ranges are not legal");
                COMPUTOC_THROW_IF_FALSE(ranges_in_dims(hdr_.ndims(), hdr_.dims(), ranges), std::out_of_range, "out of range ranges");

                ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> slice{};
                slice.hdr_ = Header{ nranges, ranges, hdr_.strides(), hdr_.offset(), true };
                slice.buffsp_ = buffsp_;

                return slice;
            }
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> operator()(std::initializer_list<ND_range> ranges)
            {
                return (*this)(ranges.size(), ranges.begin());
            }

            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend bool operator==(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& lhs, const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& rhs);

            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o> copy_to(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& src, ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& dst);
            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o> copy_to(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& src, ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>&& dst);

            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o> copy_of(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& arr);

            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o> reshaped(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& arr, std::size_t new_ndims, const std::size_t* new_dims);

            template <typename T_o, memoc::Buffer<T_o> Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, memoc::Buffer<std::size_t> Internal_header_buffer_o, memoc::Buffer<std::size_t> Internal_subscriptor_buffer_o>
            friend ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o> resized(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Internal_header_buffer_o, Internal_subscriptor_buffer_o>& arr, std::size_t new_ndims, const std::size_t* new_dims);

        private:
            Header hdr_{};
            memoc::Shared_ptr<Internal_data_buffer, Internal_allocator> buffsp_{ nullptr };
        };

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline bool operator==(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& lhs, const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& rhs)
        {
            if (!equal_dims(lhs.hdr_.ndims(), lhs.hdr_.dims(), rhs.hdr_.ndims(), rhs.hdr_.dims())) {
                return false;
            }

            if (lhs.hdr_.count() != rhs.hdr_.count()) {
                return false;
            }

            // empty arrays - equal dimensions and zero count for both input arrays
            if (lhs.hdr_.count() == 0) {
                return true;
            }

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::ND_subscriptor ndstor{ lhs.hdr_.ndims(), lhs.hdr_.dims() };

            while (ndstor) {
                const std::size_t* subs{ ndstor.subs() };
                if (!is_equal(lhs(ndstor.ndims(), subs), rhs(ndstor.ndims(), subs))) {
                    return false;
                }
                ++ndstor;
            }

            return true;
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> copy_to(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& src, ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& dst)
        {
            if (is_empty(src)) {
                dst = ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>{};
                return dst;
            }

            if (!equal_dims(src.hdr_.ndims(), src.hdr_.dims(), dst.hdr_.ndims(), dst.hdr_.dims())) {
                COMPUTOC_THROW_IF_FALSE(!dst.hdr_.is_subarray(), std::runtime_error, "unable to reallocate subarray");
                dst.hdr_ = typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Header( src.hdr_.ndims(), src.hdr_.dims() );
                dst.buffsp_ = memoc::make_shared<Internal_data_buffer, Internal_allocator>(src.hdr_.count());
            }

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::ND_subscriptor ndstor{ src.hdr_.ndims(), src.hdr_.dims() };

            while (ndstor) {
                const std::size_t* subs{ ndstor.subs() };
                dst(ndstor.ndims(), subs) = src(ndstor.ndims(), subs);
                ++ndstor;
            }

            return dst;
        }
        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> copy_to(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& src, ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>&& dst)
        {
            return copy_to(src, dst);
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> copy_of(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr)
        {
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> clone{};

            if (is_empty(arr)) {
                return clone;
            }

            clone.hdr_ = typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Header( arr.hdr_.ndims(), arr.hdr_.dims() );
            if (arr.buffsp_) {
                clone.buffsp_ = memoc::make_shared<Internal_data_buffer, Internal_allocator>(arr.hdr_.count());

                typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::ND_subscriptor ndstor{ arr.hdr_.ndims(), arr.hdr_.dims() };

                while (ndstor) {
                    const std::size_t* subs{ ndstor.subs() };
                    clone(ndstor.ndims(), subs) = arr(ndstor.ndims(), subs);
                    ++ndstor;
                }
            }
            return clone;
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> reshaped(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr, std::size_t new_ndims, const std::size_t* new_dims)
        {
            COMPUTOC_THROW_IF_FALSE(!arr.hdr_.is_subarray(), std::runtime_error, "reshaped is undefined for subarray");
            COMPUTOC_THROW_IF_FALSE(arr.buffsp_, std::runtime_error, "array should not be empty");
            COMPUTOC_THROW_IF_FALSE(dims2count(new_ndims, new_dims) == arr.hdr_.count(), std::invalid_argument, "reshaped array should have the same amount of cells as the original");

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> res{ arr };
            res.hdr_ = typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Header( new_ndims, new_dims );

            return res;
        }
        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> reshaped(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr, std::initializer_list<std::size_t> new_dims)
        {
            return reshaped(arr, new_dims.size(), new_dims.begin());
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> resized(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr, std::size_t new_ndims, const std::size_t* new_dims)
        {
            COMPUTOC_THROW_IF_FALSE(!arr.hdr_.is_subarray(), std::runtime_error, "resize for subarray is undefined");
            COMPUTOC_THROW_IF_FALSE(arr.buffsp_, std::runtime_error, "array should not be empty");

            if (equal_dims(arr.hdr_.ndims(), arr.hdr_.dims(), new_ndims, new_dims)) {
                return arr;
            }

            std::size_t new_count{ dims2count(new_ndims, new_dims) };

            if (new_count == arr.hdr_.count()) {
                return reshaped(arr, new_ndims, new_dims);
            }

            if (new_count < arr.hdr_.count()) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>{new_ndims, new_dims, arr.buffsp_->data().p};
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> res{ new_ndims, new_dims };
            for (std::size_t i = 0; i < arr.buffsp_->data().s; ++i) {
                res.buffsp_->data().p[i] = arr.buffsp_->data().p[i];
            }
            return res;
        }
        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> resized(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr, std::initializer_list<std::size_t> new_dims)
        {
            return resized(arr, new_dims.size(), new_dims.begin());
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline bool is_empty(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr)
        {
            return !arr.data();
        }
    }

    using details::ND_range;
    using details::ND_array;
    using details::copy_to;
    using details::copy_of;
    using details::reshaped;
    using details::resized;
    using details::is_empty;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
