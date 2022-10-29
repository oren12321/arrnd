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

        inline void ranges2dims(std::size_t ndims, const std::size_t* previous_dims, std::size_t nranges, const ND_range* ranges, std::size_t* dims)
        {
            // Assumption: size of dims is the bigger from ndims and nranges.
            for (std::size_t i = 0; i < nranges; ++i) {
                dims[i] = static_cast<std::size_t>(std::ceil((ranges[i].stop - ranges[i].start + 1.0) / ranges[i].step));
            }
            if (ndims > nranges) {
                for (std::size_t i = nranges; i < ndims; ++i) {
                    dims[i] = previous_dims[i];
                }
            }
        }

        inline std::size_t ranges2offset(std::size_t ndims, std::size_t previous_offset, const std::size_t* previous_strides, std::size_t nranges, const ND_range* ranges)
        {
            std::size_t offset{ previous_offset };
            // Assumption: ndims >= nranges
            for (std::size_t i = 0; i < nranges; ++i) {
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
            if (ndims == 0) {
                return 0;
            }
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
                result &= (ranges[i].start <= ranges[i].stop && ranges[i].step > 0);
            }
            return result;
        }

        inline bool ranges_in_dims(std::size_t ndims, const std::size_t* dims, std::size_t nranges, const ND_range* ranges)
        {
            if (ndims == 0) {
                return false;
            }
            if (nranges == 0) {
                return true; // empty group of ranges considered to be inside group of dimensions
            }
            if (nranges > ndims) {
                return false;
            }
            bool result{ true };
            for (std::size_t i = 0; i < nranges && result; ++i) {
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

        using ND_header_allocator = memoc::Malloc_allocator;

        using ND_header_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (sizeof(std::size_t) + sizeof(std::size_t))>,
            memoc::Allocated_buffer<ND_header_allocator, true>>>;

        template <memoc::Buffer<std::size_t> Internal_buffer = ND_header_buffer>
        class ND_header {
        public:
            ND_header() = default;

            ND_header(std::size_t ndims, const std::size_t* dims, std::size_t nranges, const ND_range* ranges, const std::size_t* strides, std::size_t offset, bool is_partial)
                : ndims_(ndims >= nranges ? ndims : nranges), size_info_(ndims_ * 2), is_partial_(is_partial)
            {
                COMPUTOC_THROW_IF_FALSE(ndims_ > 0, std::invalid_argument, "number of dimensions should be > 0");
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                std::size_t* dimsp = size_info_.data().p;
                ranges2dims(ndims_, dims, nranges, ranges, dimsp);

                count_ = dims2count(ndims_, dimsp);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                std::size_t* stridesp = size_info_.data().p + ndims_;
                ranges2strides(ndims > nranges ? nranges : ndims, strides, ranges, stridesp);
                if (ndims > nranges) {
                    std::size_t remained = ndims - nranges;
                    dims2strides(remained, dims + nranges, stridesp + nranges);
                }

                offset_ = nranges > ndims ? 0 : ranges2offset(ndims_, offset, strides, nranges, ranges);
            }

            ND_header(std::size_t ndims, const std::size_t* dims)
                : ndims_(ndims), size_info_(ndims * 2)
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

            ND_header(ND_header&& other)
                : ndims_(other.ndims_), size_info_(std::move(other.size_info_)), count_(other.count_), offset_(other.offset_), is_partial_(other.is_partial_)
            {
                other.ndims_ = other.count_ = other.offset_ = 0;
                other.is_partial_ = false;
            }
            ND_header& operator=(ND_header&& other)
            {
                if (&other == this) {
                    return *this;
                }

                ndims_ = other.ndims_;
                size_info_ = std::move(other.size_info_);
                count_ = other.count_;
                offset_ = other.offset_;
                is_partial_ = other.is_partial_;

                other.ndims_ = other.count_ = other.offset_ = 0;
                other.is_partial_ = false;

                return *this;
            }

            ND_header(const ND_header& other) = default;
            ND_header& operator=(const ND_header& other) = default;

            virtual ~ND_header() = default;

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

            bool is_partial() const
            {
                return is_partial_;
            }

        private:
            std::size_t ndims_{ 0 };
            Internal_buffer size_info_{};
            std::size_t count_{ 0 };
            std::size_t offset_{ 0 };
            bool is_partial_{ false };
        };


        using ND_subscriptor_allocator = memoc::Malloc_allocator;

        using ND_subscriptor_buffer = memoc::Typed_buffer<std::size_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * sizeof(std::size_t)>,
            memoc::Allocated_buffer<ND_subscriptor_allocator, true>>>;


        template <memoc::Buffer<std::size_t> Internal_buffer = ND_subscriptor_buffer>
        class ND_subscriptor
        {
        public:
            ND_subscriptor(std::size_t nsubs, const std::size_t* from, const std::size_t* to)
                : nsubs_(nsubs), buff_(nsubs, from), subs_(buff_.data().p), from_(from), to_(to)
            {
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
            }
            ND_subscriptor(std::initializer_list<std::size_t> from, std::initializer_list<std::size_t> to)
                : ND_subscriptor(from.size(), from.begin(), to.begin())
            {
                COMPUTOC_THROW_IF_FALSE(from.size() && to.size(), std::invalid_argument, "'from' and/or 'to' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(from.size() == to.size(), std::invalid_argument, "'froms' and 'to' subscripts size are not equal");
            }

            ND_subscriptor(std::size_t nsubs, const std::size_t* to)
                :nsubs_(nsubs), buff_(nsubs), subs_(buff_.data().p), to_(to)
            {
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                reset();
            }
            ND_subscriptor(std::initializer_list<std::size_t> to)
                : ND_subscriptor(to.size(), to.begin())
            {
                COMPUTOC_THROW_IF_FALSE(to.size(), std::invalid_argument, "'to' subscripts size is zero");
            }

            void reset()
            {
                for (std::size_t i = 0; i < nsubs_; ++i) {
                    subs_[i] = 0;
                }
            }

            ND_subscriptor& operator++()
            {
                bool should_process_sub{ true };
                for (std::size_t i = nsubs_; i >= 1 && should_process_sub; --i) {
                    if ((should_process_sub = (++subs_[i - 1] == to_[i - 1])) && i > 1) {
                        subs_[i - 1] = from_ ? from_[i - 1] : 0;
                    }
                }
                return *this;
            }

            ND_subscriptor operator++(int)
            {
                ND_subscriptor temp{ *this };
                ++(*this);
                return temp;
            }

            operator bool()
            {
                return subs_[0] != to_[0];
            }

            std::size_t nsubs() const
            {
                return nsubs_;
            }

            const std::size_t* subs() const
            {
                return subs_;
            }

        private:
            std::size_t nsubs_{ 0 };
            Internal_buffer buff_{};
            std::size_t* subs_{ nullptr };
            const std::size_t* from_{ nullptr };
            const std::size_t* to_{ nullptr };
        };


        using ND_array_allocator = memoc::Malloc_allocator;

        template <typename T>
        using ND_array_buffer = memoc::Typed_buffer<T, memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * sizeof(T)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>>;

        template <typename T, memoc::Buffer<T> Internal_data_buffer = ND_array_buffer<T>, memoc::Allocator Internal_allocator = ND_array_allocator, memoc::Buffer<std::size_t> Internal_header_buffer = ND_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer = ND_subscriptor_buffer>
        class ND_array {
        public:
            using Header = ND_header<Internal_header_buffer>;
            using Subscriptor = ND_subscriptor<Internal_subscriptor_buffer>;

            ND_array() = default;

            ND_array(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>&& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>&& other) = default;

            ND_array(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& other) = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& other) = default;

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& operator=(const T& value)
            {
                if (is_empty(*this)) {
                    return *this;
                }
                ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor ndstor{ hdr_.ndims(), hdr_.dims() };
                while (ndstor) {
                    (*this)(ndstor.nsubs(), ndstor.subs()) = value;
                    ++ndstor;
                }
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
                /*
                * Slicing algorithm:
                * - empty group of ranges -> original input array
                * - illegal ranges -> throw exception
                * - more ranges than dimensions -> throw exception
                * - empty array -> empty array
                * - ranges not inside dimensions -> throw exception
                * - return subarray
                */
                COMPUTOC_THROW_IF_FALSE(legal_ranges(nranges, ranges), std::invalid_argument, "illegal ranges");

                if (nranges == 0 || is_empty(*this)) {
                    return (*this);
                }

                COMPUTOC_THROW_IF_FALSE(nranges <= hdr_.ndims(), std::invalid_argument, "more ranges than dimensions");
                COMPUTOC_THROW_IF_FALSE(ranges_in_dims(hdr_.ndims(), hdr_.dims(), nranges, ranges), std::invalid_argument, "ranges invalid for dimensions");

                ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> slice{};
                slice.hdr_ = Header{ hdr_.ndims(), hdr_.dims(), nranges, ranges, hdr_.strides(), hdr_.offset(), true };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> operator()(std::initializer_list<ND_range> ranges)
            {
                return (*this)(ranges.size(), ranges.begin());
            }

            template <
                typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
                typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
            friend bool operator==(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& lhs, const ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>& rhs);

            template <
                typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
                typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
            friend ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2> copy_to(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>& dst);
            template <
                typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
                typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
            friend ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2> copy_to(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>&& dst);

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

        template <
            typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
            typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
        inline bool operator==(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& lhs, const ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>& rhs)
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

            typename ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>::Subscriptor ndstor{ lhs.hdr_.ndims(), lhs.hdr_.dims() };

            while (ndstor) {
                const std::size_t* subs{ ndstor.subs() };
                if (!is_equal(lhs(ndstor.nsubs(), subs), rhs(ndstor.nsubs(), subs))) {
                    return false;
                }
                ++ndstor;
            }

            return true;
        }

        template <
            typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
            typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
        inline ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2> copy_to(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>& dst)
        {
            /*
            * Algorithm:
            * - empty array     -> empty array
            * - not equal count -> create new buffer
            * - copy elements
            */

            if (is_empty(src)) {
                dst = ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>{};
                return dst;
            }

            if (src.hdr_.count() != dst.hdr_.count()) {
                dst.hdr_ = typename ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>::Header( src.hdr_.ndims(), src.hdr_.dims() );
                dst.buffsp_ = memoc::make_shared<Internal_data_buffer2, Internal_allocator2>(src.hdr_.count());
            }

            typename ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>::Subscriptor src_ndstor{ src.hdr_.ndims(), src.hdr_.dims() };
            typename ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>::Subscriptor dst_ndstor{ dst.hdr_.ndims(), dst.hdr_.dims() };

            while (src_ndstor && dst_ndstor) {
                dst(dst_ndstor.nsubs(), dst_ndstor.subs()) = src(src_ndstor.nsubs(), src_ndstor.subs());
                ++src_ndstor;
                ++dst_ndstor;
            }

            return dst;
        }
        template <
            typename T1, memoc::Buffer<T1> Internal_data_buffer1, memoc::Allocator Internal_allocator1, memoc::Buffer<std::size_t> Internal_header_buffer1, memoc::Buffer<std::size_t> Internal_subscriptor_buffer1,
            typename T2, memoc::Buffer<T2> Internal_data_buffer2, memoc::Allocator Internal_allocator2, memoc::Buffer<std::size_t> Internal_header_buffer2, memoc::Buffer<std::size_t> Internal_subscriptor_buffer2>
        inline ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2> copy_to(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Internal_header_buffer1, Internal_subscriptor_buffer1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Internal_header_buffer2, Internal_subscriptor_buffer2>&& dst)
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

                typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor ndstor{ arr.hdr_.ndims(), arr.hdr_.dims() };

                while (ndstor) {
                    const std::size_t* subs{ ndstor.subs() };
                    clone(ndstor.nsubs(), subs) = arr(ndstor.nsubs(), subs);
                    ++ndstor;
                }
            }
            return clone;
        }

        template <typename T, memoc::Buffer<T> Internal_data_buffer, memoc::Allocator Internal_allocator, memoc::Buffer<std::size_t> Internal_header_buffer, memoc::Buffer<std::size_t> Internal_subscriptor_buffer>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> reshaped(const ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>& arr, std::size_t new_ndims, const std::size_t* new_dims)
        {
            /*
            * Reshaping algorithm:
            * - different number of elements -> throw an exception
            * - empty array -> empty array
            * - equal dimensions -> ref to input array
            * - subarray -> new array with new size and copied elements from input array (reshape on subarray isn't always defined)
            * - not subarray -> reference to input array with modified header
            */
            COMPUTOC_THROW_IF_FALSE(arr.hdr_.count() == dims2count(new_ndims, new_dims), std::invalid_argument, "different number of elements between original and rehsaped arrays");

            if (is_empty(arr)) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>{};
            }

            if (equal_dims(arr.hdr_.ndims(), arr.hdr_.dims(), new_ndims, new_dims)) {
                return arr;
            }

            if (arr.hdr_.is_partial()) {
                ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> res{ new_ndims, new_dims };

                typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor prev_ndstor{ arr.hdr_.ndims(), arr.hdr_.dims() };
                typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor new_ndstor{ new_ndims, new_dims };

                while (prev_ndstor && new_ndstor) {
                    res(new_ndstor.nsubs(), new_ndstor.subs()) = arr(prev_ndstor.nsubs(), prev_ndstor.subs());
                    ++prev_ndstor;
                    ++new_ndstor;
                }

                return res;
            }

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
            /*
            * Resizing algorithm:
            * - return new array of the new size containing the original array data or part of it.
            */
            if (new_ndims == 0) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>{};
            }

            if (is_empty(arr)) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>{new_ndims, new_dims};
            }

            if (equal_dims(arr.hdr_.ndims(), arr.hdr_.dims(), new_ndims, new_dims)) {
                return copy_of(arr);
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer> res{ new_ndims, new_dims };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor prev_ndstor{ arr.hdr_.ndims(), arr.hdr_.dims() };
            typename ND_array<T, Internal_data_buffer, Internal_allocator, Internal_header_buffer, Internal_subscriptor_buffer>::Subscriptor new_ndstor{ new_ndims, new_dims };

            while (prev_ndstor && new_ndstor) {
                res(new_ndstor.nsubs(), new_ndstor.subs()) = arr(prev_ndstor.nsubs(), prev_ndstor.subs());
                ++prev_ndstor;
                ++new_ndstor;
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
