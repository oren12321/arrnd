#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstdint>
#include <initializer_list>
#include <stdexcept>


#include <computoc/errors.h>
#include <computoc/utils.h>
#include <computoc/math.h>
#include <memoc/allocators.h>
#include <memoc/buffers.h>
#include <memoc/pointers.h>
#include <memoc/blocks.h>

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
        * Check if two dimensions are equal:
        * ----------------------------------
        * are_equal = f(D1,D2) = D1(1)=D2(1) and D1(2)=D2(2) and ... and D1(N)=D2(N)
        * 
        * Check if ranges are valid/legal:
        * --------------------------------
        * are_legal - f(R) = Rt(1)>0 and Rt(2)>0 and ... and Rt(N)>0
        */

        /**
        * @note If dimensions contain zero or negative dimension value than the number of elements will be 0.
        */
        inline std::int64_t numel(const Params<std::int64_t>& dims) noexcept
        {
            if (dims.empty()) {
                return 0;
            }

            std::int64_t res{ 1 };
            for (std::int64_t i = 0; i < dims.s(); ++i) {
                if (dims[i] <= 0) {
                    return 0;
                }
                res *= dims[i];
            }
            return res;
        }

        /**
        * @param[out] strides An already allocated memory for computed strides.
        * @return Number of computed strides
        */
        inline std::int64_t compute_strides(const Params<std::int64_t>& dims, Params<std::int64_t> strides) noexcept
        {
            std::int64_t num_strides{ dims.s() > strides.s() ? strides.s() : dims.s() };
            if (num_strides <= 0) {
                return 0;
            }

            strides[num_strides - 1] = 1;
            for (std::int64_t i = num_strides - 2; i >= 0; --i) {
                strides[i] = strides[i + 1] * dims[i + 1];
            }
            return num_strides;
        }

        inline void dims2strides(const Params<std::int64_t>& dims, Params<std::int64_t> strides) noexcept
        {
            if (dims.empty() || strides.empty()) {
                return;
            }

            if (dims.s() != strides.s()) {
                return;
            }

            strides.p()[dims.s() - 1] = 1;
            for (std::int64_t i = dims.s() - 1; i >= 1; --i) {
                strides.p()[i - 1] = strides.p()[i] * dims.p()[i];
            }
        }

        /**
        * @param[out] strides An already allocated memory for computed strides.
        * @return Number of computed strides
        */
        inline std::int64_t compute_strides(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& previous_strides, const Params<Interval<std::int64_t>>& intervals, Params<std::int64_t> strides) noexcept
        {
            std::int64_t nstrides{ previous_strides.s() > strides.s() ? strides.s() : previous_strides.s() };
            if (nstrides <= 0) {
                return 0;
            }

            // compute strides with interval step
            for (std::int64_t i = 0; i < intervals.s(); ++i) {
                strides[i] = previous_strides[i] * forward(intervals[i]).step;
            }

            // compute strides from previous dimensions
            if (intervals.s() < previous_dims.s() && nstrides >= previous_dims.s()) {
                strides[previous_dims.s() - 1] = 1;
                for (std::int64_t i = previous_dims.s() - 2; i >= intervals.s(); --i) {
                    strides[i] = strides[i + 1] * previous_dims[i + 1];
                }
            }

            return nstrides;
        }

        inline void ranges2strides(const Params<std::int64_t>& previous_strides, const Params<Interval<std::int64_t>>& ranges, Params<std::int64_t> strides) noexcept
        {
            if (previous_strides.empty() || ranges.empty() || strides.empty()) {
                return;
            }

            if (previous_strides.s() != strides.s()) {
                return;
            }

            std::int64_t last{ ranges.s() < previous_strides.s() ? ranges.s() : previous_strides.s() };

            for (std::int64_t i = 0; i < last; ++i) {
                strides.p()[i] = previous_strides.p()[i] * forward(ranges.p()[i]).step;
            }
        }

        inline void ranges2dims(const Params<std::int64_t>& previous_dims, const Params<Interval<std::int64_t>>& ranges, Params<std::int64_t> dims) noexcept
        {
            if (previous_dims.empty() || dims.empty()) {
                return;
            }

            if (previous_dims.s() != dims.s()) {
                return;
            }

            std::int64_t middle{ previous_dims.s() >= ranges.s() ? ranges.s() : previous_dims.s() };

            for (std::int64_t i = 0; i < middle; ++i) {
                Interval<std::int64_t> r{ forward(modulo(ranges.p()[i], previous_dims.p()[i])) };
                dims.p()[i] = static_cast<std::int64_t>(std::ceil((r.stop - r.start + 1.0) / r.step));
            }

            for (std::int64_t i = middle; i < previous_dims.s(); ++i) {
                dims.p()[i] = previous_dims.p()[i];
            }
        }

        /**
        * @param[out] dims An already allocated memory for computed dimensions.
        * @return Number of computed dimensions
        * @note Previous dimensions are used in case of small number of intervals.
        */
        inline std::int64_t compute_dims(const Params<std::int64_t>& previous_dims, const Params<Interval<std::int64_t>>& intervals, Params<std::int64_t> dims) noexcept
        {
            std::int64_t ndims{ previous_dims.s() > dims.s() ? dims.s() : previous_dims.s() };
            if (ndims <= 0) {
                return 0;
            }

            std::int64_t num_computed_dims{ ndims > intervals.s() ? intervals.s() : ndims };

            for (std::int64_t i = 0; i < num_computed_dims; ++i) {
                Interval<std::int64_t> interval{ forward(modulo(intervals[i], previous_dims[i])) };
                if (interval.start > interval.stop || interval.step <= 0) {
                    return 0;
                }
                dims[i] = static_cast<std::int64_t>(std::ceil((interval.stop - interval.start + 1.0) / interval.step));
            }

            for (std::int64_t i = num_computed_dims; i < ndims; ++i) {
                dims[i] = previous_dims[i];
            }

            return ndims;
        }

        [[nodiscard]] inline std::int64_t compute_offset(const Params<std::int64_t>& previous_dims, std::int64_t previous_offset, const Params<std::int64_t>& previous_strides, const Params<Interval<std::int64_t>>& intervals) noexcept
        {
            std::int64_t offset{ previous_offset };

            if (previous_dims.empty() || previous_strides.empty() || intervals.empty()) {
                return offset;
            }

            std::int64_t num_computations{ previous_dims.s() > previous_strides.s() ? previous_strides.s() : previous_dims.s() };
            num_computations = (num_computations > intervals.s() ? intervals.s() : num_computations);

            for (std::int64_t i = 0; i < num_computations; ++i) {
                offset += previous_strides[i] * forward(modulo(intervals[i], previous_dims[i])).start;
            }
            return offset;
        }

        inline std::int64_t ranges2offset(const Params<std::int64_t>& previous_dims, std::int64_t previous_offset, const Params<std::int64_t>& previous_strides, const Params<Interval<std::int64_t>>& ranges) noexcept
        {
            std::int64_t offset{ previous_offset };

            if (previous_strides.empty() || ranges.empty()) {
                return offset;
            }

            std::int64_t last{ ranges.s() < previous_strides.s() ? ranges.s() : previous_strides.s() };

            for (std::int64_t i = 0; i < last; ++i) {
                Interval<std::int64_t> r{ forward(modulo(ranges.p()[i], previous_dims.p()[i])) };
                offset += previous_strides.p()[i] * r.start;
            }
            return offset;
        }

        inline std::int64_t subs2ind(std::int64_t offset, const Params<std::int64_t>& strides, const Params<std::int64_t>& dims, const Params<std::int64_t>& subs) noexcept
        {
            std::int64_t ind{ offset };

            if (strides.empty() || dims.empty() || subs.empty()) {
                return ind;
            }

            std::int64_t zero_nsubs{ strides.s() - subs.s() };
            if (zero_nsubs < 0) { // ignore extra subscripts
                zero_nsubs = 0;
            }
            for (std::int64_t i = zero_nsubs; i < strides.s(); ++i) {
                ind += strides.p()[i] * modulo(subs.p()[i - zero_nsubs], dims.p()[i]);
            }
            return ind;
        }

        inline std::int64_t dims2count(const Params<std::int64_t>& dims) noexcept
        {
            if (dims.empty()) {
                return 0;
            }

            std::int64_t count{ 1 };
            for (std::int64_t i = 0; i < dims.s(); ++i) {
                count *= dims.p()[i];
            }
            return count;
        }

        inline bool valid_ranges(const Params<std::int64_t>& dims, const Params<Interval<std::int64_t>>& ranges) noexcept
        {
            bool result{ true };
            std::int64_t last{ ranges.s() < dims.s() ? ranges.s() : dims.s() };

            for (std::int64_t i = 0; i < last && result; ++i) {
                Interval<std::int64_t> r{ forward(modulo(ranges.p()[i], dims.p()[i])) };

                result &= (r.start <= r.stop && r.step > 0);
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

        using Array_default_internals_buffer = memoc::Typed_buffer<std::int64_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (MEMOC_SSIZEOF(std::int64_t) + MEMOC_SSIZEOF(std::int64_t))>,
            memoc::Allocated_buffer<memoc::Malloc_allocator, true>>>;

        template <memoc::Buffer<std::int64_t> Internal_buffer = Array_default_internals_buffer>
        class Array_header {
        public:
            Array_header() = default;

            Array_header(const Params<std::int64_t>& dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                buff_ = Internal_buffer(dims.s() * 2);
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "buffer allocation failed");

                dims_ = { dims.s(), buff_.data().p() };
                copy(dims, dims_);

                strides_ = { dims.s(), buff_.data().p() + dims.s() };
                compute_strides(dims, strides_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& previous_strides, std::int64_t previous_offset, const Params<Interval<std::int64_t>>& intervals)
                : is_partial_(true)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                Internal_buffer buff(previous_dims.s() * 2);
                COMPUTOC_THROW_IF_FALSE(buff.usable(), std::runtime_error, "buffer allocation failed");

                Params<std::int64_t> dims{ previous_dims.s(), buff.data().p() };
                if (compute_dims(previous_dims, intervals, dims) <= 0) {
                    return;
                }

                buff_ = std::move(buff);
                
                dims_ = { previous_dims.s(), buff_.data().p() };

                count_ = numel(dims_);

                strides_ = { previous_dims.s(), buff_.data().p() + previous_dims.s() };
                compute_strides(previous_dims, previous_strides, intervals, strides_);

                offset_ = compute_offset(previous_dims, previous_offset, previous_strides, intervals);
            }

            Array_header(const Params<std::int64_t>& previous_dims, std::int64_t omitted_axis)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                std::int64_t axis{ modulo(omitted_axis, previous_dims.s()) };
                std::int64_t ndims{ previous_dims.s() > 1 ? previous_dims.s() - 1 : 1 };

                buff_ = Internal_buffer(ndims * 2);
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "buffer allocation failed");

                dims_ = { ndims, buff_.data().p() };
                if (previous_dims.s() > 1) {
                    for (std::int64_t i = 0; i < axis; ++i) {
                        dims_[i] = previous_dims[i];
                    }
                    for (std::int64_t i = axis + 1; i < previous_dims.s(); ++i) {
                        dims_[i - 1] = previous_dims[i];
                    }
                }
                else {
                    dims_.p()[0] = 1;
                }

                strides_ = { ndims, buff_.data().p() + ndims };
                compute_strides(dims_, strides_);

                count_ = dims2count(dims_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& new_order)
                : buff_(previous_dims.s() * 2)
            {
                if (previous_dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "failed to allocate header buffer");
                COMPUTOC_THROW_IF_FALSE(previous_dims.s() == new_order.s(), std::invalid_argument, "new_order and dimensions sizes are different");

                dims_ = { previous_dims.s(), buff_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(new_order.p()[i] < dims_.s(), std::out_of_range, "new_order index not in dimensions range");
                    dims_.p()[i] = previous_dims.p()[new_order.p()[i]];
                }

                strides_ = { previous_dims.s(), buff_.data().p() + previous_dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            Array_header(const Params<std::int64_t>& dims, const Params<std::int64_t>& appended_dims, std::int64_t axis)
                : buff_(dims.s() * 2)
            {
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), buff_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    if (axis == i) {
                        dims_.p()[i] = dims.p()[i] + appended_dims.p()[i];
                    }
                    else {
                        dims_.p()[i] = dims.p()[i];
                    }
                }

                strides_ = { dims.s(), buff_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            Array_header(const Params<std::int64_t>& dims, std::int64_t count, std::int64_t axis)
                : buff_(dims.s() * 2)
            {
                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), buff_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    if (axis == i) {
                        dims_.p()[i] = dims.p()[i] + count;
                    }
                    else {
                        dims_.p()[i] = dims.p()[i];
                    }
                }

                strides_ = { dims.s(), buff_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            Array_header(Array_header&& other) noexcept
                : buff_(std::move(other.buff_)), count_(other.count_), offset_(other.offset_), is_partial_(other.is_partial_)
            {
                dims_ = { other.dims_.s(), buff_.data().p() };
                strides_ = { other.strides_.s(), buff_.data().p() + other.dims_.s() };

                other.dims_.clear();
                other.strides_.clear();
                other.count_ = other.offset_ = 0;
                other.is_partial_ = false;
            }
            Array_header& operator=(Array_header&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = std::move(other.buff_);
                count_ = other.count_;
                offset_ = other.offset_;
                is_partial_ = other.is_partial_;

                dims_ = { other.dims_.s(), buff_.data().p() };
                strides_ = { other.strides_.s(), buff_.data().p() + other.dims_.s() };

                other.dims_.clear();
                other.strides_.clear();
                other.count_ = other.offset_ = 0;
                other.is_partial_ = false;

                return *this;
            }

            Array_header(const Array_header& other) noexcept
                : buff_(other.buff_), count_(other.count_), offset_(other.offset_), is_partial_(other.is_partial_)
            {
                dims_ = { other.dims_.s(), buff_.data().p() };
                strides_ = { other.strides_.s(), buff_.data().p() + other.dims_.s() };
            }
            Array_header& operator=(const Array_header& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                count_ = other.count_;
                offset_ = other.offset_;
                is_partial_ = other.is_partial_;

                dims_ = { other.dims_.s(), buff_.data().p() };
                strides_ = { other.strides_.s(), buff_.data().p() + other.dims_.s() };

                return *this;
            }

            virtual ~Array_header() = default;

            std::int64_t count() const noexcept
            {
                return count_;
            }

            const Params<std::int64_t> dims() const noexcept
            {
                return dims_;
            }

            const Params<std::int64_t> strides() const noexcept
            {
                return strides_;
            }

            std::int64_t offset() const noexcept
            {
                return offset_;
            }

            bool is_partial() const noexcept
            {
                return is_partial_;
            }

            bool empty() const noexcept
            {
                return !buff_.usable() && count_ <= 0;
            }

        private:
            Params<std::int64_t> dims_{};
            Params<std::int64_t> strides_{};
            Internal_buffer buff_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            bool is_partial_{ false };
        };


        template <memoc::Buffer<std::int64_t> Internal_buffer = Array_default_internals_buffer>
        class Array_subscripts_iterator
        {
        public:
            Array_subscripts_iterator(const Params<std::int64_t>& start, const Params<std::int64_t>& minimum_excluded, const Params<std::int64_t>& maximum_excluded, std::int64_t axis)
            {
                std::int64_t bounds_size{ minimum_excluded.s() > maximum_excluded.s() ? minimum_excluded.s() : maximum_excluded.s() };
                nsubs_ = start.s() > bounds_size ? start.s() : bounds_size;

                if (nsubs_ > 0) {
                    buff_ = Internal_buffer(nsubs_ * 4);
                    COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "buffer allocation failed");

                    axis_ = modulo(axis, nsubs_);

                    subs_ = { nsubs_, buff_.data().p() };
                    start_ = { nsubs_, buff_.data().p() + nsubs_ };
                    if (start.empty()) {
                        set(subs_, std::int64_t{ 0 }, nsubs_);
                        set(start_, std::int64_t{ 0 }, nsubs_);
                    }
                    else {
                        copy(start, subs_, nsubs_);
                        copy(start, start_, nsubs_);
                    }

                    minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                    if (!minimum_excluded.empty()) {
                        copy(minimum_excluded, minimum_excluded_, nsubs_);
                    }
                    else if(!start.empty()) {
                        copy(start, minimum_excluded_, nsubs_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        set(minimum_excluded_, std::int64_t{ -1 }, nsubs_);
                    }

                    maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                    if (maximum_excluded.empty()) {
                        set(maximum_excluded_, std::int64_t{ 1 }, nsubs_);
                    }
                    else {
                        copy(maximum_excluded, maximum_excluded_, nsubs_);
                    }

                    major_axis_ = find_major_axis();
                }
            }

            Array_subscripts_iterator(const Params<std::int64_t>& start, const Params<std::int64_t>& minimum_excluded, const Params<std::int64_t>& maximum_excluded, const Params<std::int64_t>& order)
            {
                std::int64_t bounds_size{ minimum_excluded.s() > maximum_excluded.s() ? minimum_excluded.s() : maximum_excluded.s() };
                nsubs_ = start.s() > bounds_size ? start.s() : bounds_size;

                if (nsubs_ > 0) {
                    if (!order.empty()) {
                        buff_ = Internal_buffer(nsubs_ * 4 + order.s());
                    }
                    else {
                        buff_ = Internal_buffer(nsubs_ * 4);
                        axis_ = nsubs_ - 1;
                    }
                    COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "buffer allocation failed");

                    subs_ = { nsubs_, buff_.data().p() };
                    start_ = { nsubs_, buff_.data().p() + nsubs_ };
                    if (start.empty()) {
                        set(subs_, std::int64_t{ 0 }, nsubs_);
                        set(start_, std::int64_t{ 0 }, nsubs_);
                    }
                    else {
                        copy(start, subs_, nsubs_);
                        copy(start, start_, nsubs_);
                    }

                    minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                    if (!minimum_excluded.empty()) {
                        copy(minimum_excluded, minimum_excluded_, nsubs_);
                    }
                    else if (!start.empty()) {
                        copy(start, minimum_excluded_, nsubs_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        set(minimum_excluded_, std::int64_t{ -1 }, nsubs_);
                    }

                    maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                    if (maximum_excluded.empty()) {
                        set(maximum_excluded_, std::int64_t{ 1 }, nsubs_);
                    }
                    else {
                        copy(maximum_excluded, maximum_excluded_, nsubs_);
                    }

                    if (!order.empty()) {
                        order_ = { order.s(), buff_.data().p() + 4 * nsubs_ };
                        copy(order, order_, order.s());
                        for (std::int64_t i = 0; i < order.s(); ++i) {
                            order_[i] = modulo(order_[i], nsubs_);
                        }
                    }

                    major_axis_ = find_major_axis();
                }
            }

            Array_subscripts_iterator(const Params<std::int64_t>& from, const Params<std::int64_t>& to, std::int64_t axis)
                : Array_subscripts_iterator(from, {}, to, axis)
            {
            }
            Array_subscripts_iterator(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::int64_t axis)
                : Array_subscripts_iterator(Params<std::int64_t>(std::ssize(from), from.begin()), Params<std::int64_t>(std::ssize(to), to.begin()), axis)
            {
            }

            Array_subscripts_iterator(const Params<std::int64_t>& from, const Params<std::int64_t>& to, const Params<std::int64_t>& order = {})
                : Array_subscripts_iterator(from, {}, to, order)
            {
            }
            Array_subscripts_iterator(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::initializer_list<std::int64_t> order = {})
                : Array_subscripts_iterator(Params<std::int64_t>(std::ssize(from), from.begin()), Params<std::int64_t>(std::ssize(to), to.begin()), Params<std::int64_t>(std::ssize(order), order.begin()))
            {
            }

            Array_subscripts_iterator() = default;

            Array_subscripts_iterator(const Array_subscripts_iterator<Internal_buffer>& other) noexcept
                : buff_(other.buff_), nsubs_(other.nsubs_), axis_(other.axis_), major_axis_(other.major_axis_)
            {
                subs_ = { nsubs_, buff_.data().p() };
                start_ = { nsubs_, buff_.data().p() + nsubs_ };
                minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                if (!other.order_.empty()) {
                    order_ = { other.order_.s(), buff_.data().p() + 4 * nsubs_ };
                }
            }
            Array_subscripts_iterator& operator=(const Array_subscripts_iterator<Internal_buffer>& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                nsubs_ = other.nsubs_;
                axis_ = other.axis_;
                subs_ = { nsubs_, buff_.data().p() };
                start_ = { nsubs_, buff_.data().p() + nsubs_ };
                minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                if (!other.order_.empty()) {
                    order_ = { other.order_.s(), buff_.data().p() + 4 * nsubs_ };
                }
                major_axis_ = other.major_axis_;

                return *this;
            }

            Array_subscripts_iterator(Array_subscripts_iterator<Internal_buffer>&& other) noexcept
                : buff_(std::move(other.buff_)), nsubs_(other.nsubs_), axis_(other.axis_), major_axis_(other.major_axis_)
            {
                subs_ = { nsubs_, buff_.data().p() };
                start_ = { nsubs_, buff_.data().p() + nsubs_ };
                minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                if (!other.order_.empty()) {
                    order_ = { other.order_.s(), buff_.data().p() + 4 * nsubs_ };
                }

                other.nsubs_ = 0;
                other.axis_ = 0;
                other.subs_.clear();
                other.start_.clear();
                other.minimum_excluded_.clear();
                other.maximum_excluded_.clear();
                other.major_axis_ = 0;
            }
            Array_subscripts_iterator& operator=(Array_subscripts_iterator&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = std::move(other.buff_);
                nsubs_ = other.nsubs_;
                axis_ = other.axis_;
                subs_ = { nsubs_, buff_.data().p() };
                start_ = { nsubs_, buff_.data().p() + nsubs_ };
                minimum_excluded_ = { nsubs_, buff_.data().p() + 2 * nsubs_ };
                maximum_excluded_ = { nsubs_, buff_.data().p() + 3 * nsubs_ };
                if (!other.order_.empty()) {
                    order_ = { other.order_.s(), buff_.data().p() + 4 * nsubs_ };
                }
                major_axis_ = other.major_axis_;

                other.nsubs_ = 0;
                other.axis_ = 0;
                other.subs_.clear();
                other.start_.clear();
                other.minimum_excluded_.clear();
                other.maximum_excluded_.clear();
                other.major_axis_ = 0;

                return *this;
            }

            virtual ~Array_subscripts_iterator() = default;

            void reset() noexcept
            {
                copy(start_, subs_, nsubs_);
            }

            Array_subscripts_iterator& operator++() noexcept
            {
                if (!order_.empty()) {
                    bool should_process_sub{ true };
                    for (int64_t i = order_.s() - 1; i >= 0 && should_process_sub; --i) {
                        should_process_sub = increment_subscript(order_[i], order_[0]);
                    }
                }
                else {
                    bool should_process_sub{ true };

                    should_process_sub = increment_subscript(axis_, major_axis_);
                    for (std::int64_t i = subs_.s() - 1; i > axis_ && should_process_sub; --i) {
                        should_process_sub = increment_subscript(i, major_axis_);
                    }
                    for (std::int64_t i = axis_ - 1; i >= 0 && should_process_sub; --i) {
                        should_process_sub = increment_subscript(i, major_axis_);
                    }
                }

                return *this;
            }

            Array_subscripts_iterator operator++(int) noexcept
            {
                Array_subscripts_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_subscripts_iterator operator+=(std::int64_t value) noexcept
            {
                for (std::int64_t i = 0; i < value; ++i) {
                    ++(*this);
                }
                return *this;
            }

            [[nodiscard]] Array_subscripts_iterator operator+(std::int64_t value) const noexcept
            {
                Array_subscripts_iterator temp{ *this };
                temp += value;
                return temp;
            }

            Array_subscripts_iterator& operator--() noexcept
            {
                if (!order_.empty()) {
                    bool should_process_sub{ true };
                    for (int64_t i = order_.s() - 1; i >= 0 && should_process_sub; --i) {
                        should_process_sub = decrement_subscript(order_[i], order_[0]);
                    }
                }
                else {
                    bool should_process_sub{ true };

                    should_process_sub = decrement_subscript(axis_, major_axis_);
                    for (std::int64_t i = subs_.s() - 1; i > axis_ && should_process_sub; --i) {
                        should_process_sub = decrement_subscript(i, major_axis_);
                    }
                    for (std::int64_t i = axis_ - 1; i >= 0 && should_process_sub; --i) {
                        should_process_sub = decrement_subscript(i, major_axis_);
                    }
                }

                return *this;
            }

            Array_subscripts_iterator operator--(int) noexcept
            {
                Array_subscripts_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_subscripts_iterator operator-=(std::int64_t value) noexcept
            {
                for (std::int64_t i = 0; i < value; ++i) {
                    --(*this);
                }
                return *this;
            }

            [[nodiscard]] Array_subscripts_iterator operator-(std::int64_t value) const noexcept
            {
                Array_subscripts_iterator temp{ *this };
                temp -= value;
                return temp;
            }

            explicit [[nodiscard]] operator bool() const noexcept
            {
                if (!order_.empty()) {
                    return (subs_[order_[0]] < maximum_excluded_[order_[0]]) && (subs_[order_[0]] > minimum_excluded_[order_[0]]);
                }

                return (subs_[major_axis_] < maximum_excluded_[major_axis_]) && (subs_[major_axis_] > minimum_excluded_[major_axis_]);
            }

            [[nodiscard]] const Params<std::int64_t>& operator*() const noexcept
            {
                return subs_;
            }

        private:
            [[nodiscard]] bool increment_subscript(std::int64_t i, std::int64_t major_axis) noexcept
            {
                bool should_continute_process_subscripts{ true };
                subs_[i] += (subs_[i] < maximum_excluded_[i]);
                if ((should_continute_process_subscripts = (subs_[i] == maximum_excluded_[i])) && i != major_axis) {
                    subs_[i] = minimum_excluded_[i] + 1 != 0 ? minimum_excluded_[i] + 1 : 0;
                }
                return should_continute_process_subscripts;
            }

            [[nodiscard]] bool decrement_subscript(std::int64_t i, std::int64_t major_axis) noexcept
            {
                bool should_continute_process_subscripts{ true };
                subs_[i] -= (subs_[i] > minimum_excluded_[i]);
                if ((should_continute_process_subscripts = (subs_[i] == minimum_excluded_[i])) && i != major_axis) {
                    subs_[i] = maximum_excluded_[i] != 0 ? maximum_excluded_[i] - 1 : 0;
                }
                return should_continute_process_subscripts;
            }

            [[nodiscard]] std::int64_t find_major_axis() const noexcept
            {
                std::int64_t major_axis{ axis_ > 0 ? std::int64_t{0} : (subs_.s() > 1) };
                if (minimum_excluded_[major_axis] == -1 && maximum_excluded_[major_axis] == 0) {
                    bool found{ false };
                    for (std::int64_t i = major_axis + 1; i < nsubs_ && !found; ++i) {
                        if (maximum_excluded_[i] != 0) {
                            major_axis = i;
                            found = true;
                        }
                    }
                    if (!found) {
                        major_axis = 0;
                    }
                }
                return major_axis;
            }

            Internal_buffer buff_{};

            std::int64_t nsubs_{ 0 };

            Params<std::int64_t> subs_{};
            Params<std::int64_t> start_{};
            Params<std::int64_t> minimum_excluded_{};
            Params<std::int64_t> maximum_excluded_{};

            std::int64_t axis_{ 0 };
            Params<std::int64_t> order_{};

            std::int64_t major_axis_{ 0 };
        };

        using Array_default_data_reference_allocator = memoc::Malloc_allocator;

        using Array_default_data_buffer = memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * MEMOC_SSIZEOF(std::int64_t)>,
            memoc::Allocated_buffer<memoc::Malloc_allocator, true>>;

        template <typename T, memoc::Buffer Data_buffer = Array_default_data_buffer, memoc::Allocator Data_reference_allocator = Array_default_data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer = Array_default_internals_buffer>
        class Array {
        public:
            using Header = Array_header<Internals_buffer>;
            using Subscripts_iterator = Array_subscripts_iterator<Internals_buffer>;

            Array() = default;

            Array(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other) = default;
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array(Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)
            {
                copy(other, *this);

                Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{ std::move(other) };
            }
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other) & = default;
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                if (hdr_.is_partial() && hdr_.dims() == other.hdr_.dims()) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = std::move(other.hdr_);
                buffsp_ = std::move(other.buffsp_);

                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)&
            {
                copy(other, *this);
                Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{std::move(other)};
                return *this;
            }

            Array(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other) = default;
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array(const Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)
            {
                copy(other, *this);
            }
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other) & = default;
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                if (hdr_.is_partial() && hdr_.dims() == other.hdr_.dims()) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = other.hdr_;
                buffsp_ = other.buffsp_;

                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)&
            {
                copy(other, *this);
                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const Array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                return *this;
            }

            template <typename U>
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }
                Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, Params<std::int64_t>(hdr_.dims()) };
                while (ndstor) {
                    (*this)(*ndstor) = value;
                    ++ndstor;
                }
                return *this;
            }

            virtual ~Array() = default;

            Array(const Params<std::int64_t>& dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count(), data))
            {
            }
            Array(const Params<std::int64_t>& dims, std::initializer_list<T> data)
                : Array(dims, data.begin())
            {
            }
            Array(std::initializer_list<std::int64_t> dims, const T* data = nullptr)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data)
            {
            }
            Array(std::initializer_list<std::int64_t> dims, std::initializer_list<T> data)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data.begin())
            {
            }
            template <typename U>
            Array(const Params<std::int64_t>& dims, const U* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = data[i];
                }
            }
            template <typename U>
            Array(const Params<std::int64_t>& dims, std::initializer_list<U> data)
                : Array(dims, data.begin())
            {
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, const U* data = nullptr)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data)
            {
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, std::initializer_list<U> data = nullptr)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data.begin())
            {
            }


            Array(const Params<std::int64_t>& dims, const T& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            Array(std::initializer_list<std::int64_t> dims, const T& value)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
            {
            }
            template <typename U>
            Array(const Params<std::int64_t>& dims, const U& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, const U& value)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
            {
            }

            const Header& header() const noexcept
            {
                return hdr_;
            }

            Header& header() noexcept
            {
                return hdr_;
            }

            T* data() const noexcept
            {
                return (buffsp_ ? buffsp_->data().p() : nullptr);
            }

            const T& operator()(const Params<std::int64_t>& subs) const noexcept
            {
                return buffsp_->data().p()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            const T& operator()(std::initializer_list<std::int64_t> subs) const noexcept
            {
                return (*this)(Params<std::int64_t>{ std::ssize(subs), subs.begin() });
            }

            T& operator()(const Params<std::int64_t>& subs) noexcept
            {
                return buffsp_->data().p()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            T& operator()(std::initializer_list<std::int64_t> subs) noexcept
            {
                return (*this)(Params<std::int64_t>{ std::ssize(subs), subs.begin() });
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> operator()(const Params<Interval<std::int64_t>>& ranges) const
            {
                /*
                * Slicing algorithm:
                * - empty ranges group -> input array
                * - ranges should be legal
                * - input empty array -> input array
                * - ranges should be inside dimensions
                * - -> slice
                */
                if (ranges.empty() || empty(*this)) {
                    return (*this);
                }

                Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> slice{};
                slice.hdr_ = Header{ hdr_.dims(), hdr_.strides(), hdr_.offset(), ranges };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> operator()(std::initializer_list<Interval<std::int64_t>> ranges) const
            {
                return (*this)(Params<Interval<std::int64_t>>{std::ssize(ranges), ranges.begin()});
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> operator()(const Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>& indices) const noexcept
            {
                Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ indices.header().dims() };

                Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, indices.header().dims() };
                while (ndstor) {
                    res(*ndstor) = buffsp_->data().p()[indices(*ndstor)];
                    ++ndstor;
                }

                return res;
            }

        private:
            Header hdr_{};
            memoc::Shared_ptr<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator> buffsp_{ nullptr };
        };

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>    
        inline auto transform(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
            -> Array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            if (empty(arr)) {
                return Array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            Array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().dims() };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, arr.header().dims() };

            while (ndstor) {
                res(*ndstor) = func(arr(*ndstor));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto reduce(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
            -> decltype(func(arr.data()[0], arr.data()[0]))
        {
            if (empty(arr)) {
                return decltype(func(arr.data()[0], arr.data()[0])){};
            }

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, arr.header().dims() };

            decltype(func(arr.data()[0], arr.data()[0])) res{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(*ndstor)) };
            ++ndstor;

            while (ndstor) {
                res = func(arr(*ndstor), res);
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto reduce(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func, std::int64_t axis)
            -> Array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            if (empty(arr)) {
                return Array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(axis < arr.header().dims().s(), std::invalid_argument, "axis is invalid for number of dimensions");

            Array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ {arr.header().count() / arr.header().dims().p()[axis]} };

            res.header() = typename Array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>::Header{ arr.header().dims(), axis };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims(), axis };
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            const std::int64_t reduction_iteration_cycle{ arr.header().dims().p()[axis] };

            while (arr_ndstor && res_ndstor) {
                decltype(func(arr.data()[0], arr.data()[0])) res_element{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(*arr_ndstor)) };
                ++arr_ndstor;
                for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++arr_ndstor) {
                    res_element = func(arr(*arr_ndstor), res_element);
                }
                res(*res_ndstor) = res_element;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) && static_cast<bool>(previous); });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> all(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) && static_cast<bool>(previous); }, axis);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool any(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) || static_cast<bool>(previous); });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> any(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) || static_cast<bool>(previous); }, axis);
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto transform(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
            -> Array<decltype(func(lhs.data()[0], rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            COMPUTOC_THROW_IF_FALSE(lhs.header().dims() == rhs.header().dims(), std::invalid_argument, "different input array dimensions");

            Array<decltype(func(lhs.data()[0], rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().dims() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, lhs.header().dims() };

            while (ndstor) {
                res(*ndstor) = func(lhs(*ndstor), rhs(*ndstor));
                ++ndstor;
            }

            return res;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto transform(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs, Func func)
            -> Array<decltype(func(lhs.data()[0], rhs)), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            Array<decltype(func(lhs.data()[0], rhs)), Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().dims() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, lhs.header().dims() };

            while (ndstor) {
                res(*ndstor) = func(lhs(*ndstor), rhs);
                ++ndstor;
            }

            return res;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto transform(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
            -> Array<decltype(func(lhs, rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            Array<decltype(func(lhs, rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ rhs.header().dims() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, rhs.header().dims() };

            while (ndstor) {
                res(*ndstor) = func(lhs, rhs(*ndstor));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> filter(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
        {
            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims() };
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(*arr_ndstor))) {
                    res(*res_ndstor) = arr(*arr_ndstor);
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> filter(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& mask)
        {
            if (empty(arr)) {
                return Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims() };
            typename Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator mask_ndstor{ {}, mask.header().dims() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && mask_ndstor && res_ndstor) {
                if (mask(*mask_ndstor)) {
                    res(*res_ndstor) = arr(*arr_ndstor);
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
                ++mask_ndstor;
            }

            if (res_count == 0) {
                return Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> find(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims() };
            typename Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(*arr_ndstor))) {
                    res(*res_ndstor) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), *arr_ndstor);
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> find(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& mask)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims() };
            typename Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator mask_ndstor{ {}, mask.header().dims() };

            typename Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && mask_ndstor && res_ndstor) {
                if (mask(*mask_ndstor)) {
                    res(*res_ndstor) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), *arr_ndstor);
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
                ++mask_ndstor;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> transpose(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& order)
        {
            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };
            res.header() = typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header{ arr.header().dims(), order };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arr_ndstor{ {}, arr.header().dims(), order };
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator res_ndstor{ {}, res.header().dims() };

            while (arr_ndstor && res_ndstor) {
                res(*res_ndstor) = arr(*arr_ndstor);
                ++arr_ndstor;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> transpose(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> order)
        {
            return transpose(arr, Params<std::int64_t>{std::ssize(order), order.begin()});
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator==(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator==(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator==(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator!=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator!=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator!=(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> close(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> close(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> close(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>=(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<=(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<=(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator+=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator+=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator-=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator-=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator*=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator*=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator/=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator/=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator%=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator%=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator^=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator^=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator&=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator&=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator|=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator|=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
            -> Array<decltype(lhs << rhs.data()[0]), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator<<=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator<<=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator>>=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator>>=(Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
            return lhs;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator~(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return transform(arr, [](const T& a) { return ~a; });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator!(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return transform(arr, [](const T& a) { return !a; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator++(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            if (empty(arr)) {
                return arr;
            }
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, arr.header().dims() };
            while (ndstor) {
                ++arr(*ndstor);
                ++ndstor;
            }
            return arr;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr)
        {
            return operator++(arr);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, int)
        {
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> old = clone(arr);
            operator++(arr);
            return old;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator--(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            if (empty(arr)) {
                return arr;
            }
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, arr.header().dims() };
            while (ndstor) {
                --arr(*ndstor);
                ++ndstor;
            }
            return arr;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr)
        {
            return operator--(arr);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, int)
        {
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> old = clone(arr);
            operator--(arr);
            return old;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_match(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
        {
            if (lhs.header().dims() != rhs.header().dims()) {
                return false;
            }

            if (lhs.header().count() != rhs.header().count()) {
                return false;
            }

            // empty arrays - equal dimensions and zero count for both input arrays
            if (lhs.header().count() == 0) {
                return true;
            }

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, lhs.header().dims() };

            while (ndstor) {
                if (!func(lhs(*ndstor), rhs(*ndstor))) {
                    return false;
                }
                ++ndstor;
            }

            return true;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_match(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs, Func func)
        {
            if (empty(lhs)) {
                return true;
            }

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, lhs.header().dims() };

            while (ndstor) {
                if (!func(lhs(*ndstor), rhs)) {
                    return false;
                }
                ++ndstor;
            }

            return true;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_match(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
        {
            if (empty(rhs)) {
                return true;
            }

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, rhs.header().dims() };

            while (ndstor) {
                if (!func(lhs, rhs(*ndstor))) {
                    return false;
                }
                ++ndstor;
            }

            return true;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_equal(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_equal(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_equal(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_close(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_close(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all_close(const T1& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <
            typename T1, memoc::Buffer Data_buffer1, memoc::Allocator Data_reference_allocator1, memoc::Buffer<std::int64_t> Internals_buffer1,
            typename T2, memoc::Buffer Data_buffer2, memoc::Allocator Data_reference_allocator2, memoc::Buffer<std::int64_t> Internals_buffer2>
        inline void copy(const Array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>& src, Array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>& dst)
        {
            /*
            * Algorithm:
            * - empty array     -> empty array
            * - not equal count -> create new buffer
            * - copy elements
            */

            if (empty(src)) {
                dst = Array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>{};
                return;
            }

            if (src.header().count() != dst.header().count()) {
                dst = Array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>(src.header().dims());
            }

            typename Array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>::Subscripts_iterator src_ndstor{ {}, src.header().dims() };
            typename Array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>::Subscripts_iterator dst_ndstor{ {}, dst.header().dims() };

            while (src_ndstor && dst_ndstor) {
                dst(*dst_ndstor) = src(*src_ndstor);
                ++src_ndstor;
                ++dst_ndstor;
            }
        }
        template <
            typename T1, memoc::Buffer Data_buffer1, memoc::Allocator Data_reference_allocator1, memoc::Buffer<std::int64_t> Internals_buffer1,
            typename T2, memoc::Buffer Data_buffer2, memoc::Allocator Data_reference_allocator2, memoc::Buffer<std::int64_t> Internals_buffer2>
        inline void copy(const Array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>& src, Array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> clone(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> clone{};

            if (empty(arr)) {
                return clone;
            }

            clone = Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{ arr.header().dims() };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator ndstor{ {}, arr.header().dims() };

            while (ndstor) {
                clone(*ndstor) = arr(*ndstor);
                ++ndstor;
            }
            return clone;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> reshape(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& new_dims)
        {
            /*
            * Reshaping algorithm:
            * - empty array -> empty array
            * - different number of elements -> throw an exception
            * - equal dimensions -> ref to input array
            * - subarray -> new array with new size and copied elements from input array (reshape on subarray isn't always defined)
            * - not subarray -> reference to input array with modified header
            */
            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().count() == dims2count(new_dims), std::invalid_argument, "different number of elements between original and rehsaped arrays");

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().is_partial()) {
                Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ new_dims };

                typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator prev_ndstor({}, arr.header().dims());
                typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator new_ndstor({}, new_dims);

                while (prev_ndstor && new_ndstor) {
                    res(*new_ndstor) = arr(*prev_ndstor);
                    ++prev_ndstor;
                    ++new_ndstor;
                }

                return res;
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr };
            res.header() = typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(new_dims);

            return res;
        }
        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> reshape(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> resize(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& new_dims)
        {
            /*
            * Resizing algorithm:
            * - return new array of the new size containing the original array data or part of it.
            */
            if (new_dims.empty()) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>(new_dims);
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ new_dims };

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator prev_ndstor({}, arr.header().dims());
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator new_ndstor({}, new_dims);

            while (prev_ndstor && new_ndstor) {
                res(*new_ndstor) = arr(*prev_ndstor);
                ++prev_ndstor;
                ++new_ndstor;
            }

            return res;
        }
        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> resize(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool empty(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr) noexcept
        {
            return !arr.data() && arr.header().empty();
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> append(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            COMPUTOC_THROW_IF_FALSE(lhs.header().dims().s() == rhs.header().dims().s(), std::invalid_argument, "different number of dimensions");
            COMPUTOC_THROW_IF_FALSE(axis < lhs.header().dims().s(), std::out_of_range, "axis out of dimensions range");
            for (std::int64_t i = 0; i < lhs.header().dims().s(); ++i) {
                if (i != axis) {
                    COMPUTOC_THROW_IF_FALSE(lhs.header().dims().p()[i] == rhs.header().dims().p()[i], std::invalid_argument, "different dimension value");
                }
            }

            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().count() + rhs.header().count() };
            res.header() = typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(lhs.header().dims(), rhs.header().dims(), axis);

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator lhsndstor({}, lhs.header().dims());
            typename Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator rhsndstor({}, rhs.header().dims());
            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator resndstor({}, res.header().dims());

            while (resndstor) {
                if (lhsndstor && (*resndstor).p()[axis] < lhs.header().dims().p()[axis] || (*resndstor).p()[axis] >= lhs.header().dims().p()[axis] + rhs.header().dims().p()[axis]) {
                    res(*resndstor) = lhs(*lhsndstor);
                    ++lhsndstor;
                }
                else if (rhsndstor && (*resndstor).p()[axis] >= lhs.header().dims().p()[axis] && (*resndstor).p()[axis] < lhs.header().dims().p()[axis] + rhs.header().dims().p()[axis]) {
                    res(*resndstor) = rhs(*rhsndstor);
                    ++rhsndstor;
                }
                ++resndstor;
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> append(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ resize(lhs, {lhs.header().count() + rhs.header().count()}) };
            Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer> rrhs{ reshape(rhs, {rhs.header().count()}) };
            for (std::int64_t i = lhs.header().count(); i < res.header().count(); ++i) {
                res({ i }) = rhs({ i - lhs.header().count() });
            }
            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> insert(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t ind, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            COMPUTOC_THROW_IF_FALSE(lhs.header().dims().s() == rhs.header().dims().s(), std::invalid_argument, "different number of dimensions");
            COMPUTOC_THROW_IF_FALSE(axis < lhs.header().dims().s(), std::out_of_range, "axis out of dimensions range");
            for (std::int64_t i = 0; i < lhs.header().dims().s(); ++i) {
                if (i != axis) {
                    COMPUTOC_THROW_IF_FALSE(lhs.header().dims().p()[i] == rhs.header().dims().p()[i], std::invalid_argument, "different dimension value");
                }
            }
            COMPUTOC_THROW_IF_FALSE(ind <= lhs.header().dims().p()[axis], std::out_of_range, "index not in array dimension range");

            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().count() + rhs.header().count() };
            res.header() = typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(lhs.header().dims(), rhs.header().dims(), axis);

            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator lhsndstor({}, lhs.header().dims());
            typename Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator rhsndstor({}, rhs.header().dims());
            typename Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator resndstor({}, res.header().dims());

            while (resndstor) {
                if (lhsndstor && (*resndstor).p()[axis] < ind || (*resndstor).p()[axis] >= ind + rhs.header().dims().p()[axis]) {
                    res(*resndstor) = lhs(*lhsndstor);
                    ++lhsndstor;
                }
                else if (rhsndstor && (*resndstor).p()[axis] >= ind && (*resndstor).p()[axis] < ind + rhs.header().dims().p()[axis]) {
                    res(*resndstor) = rhs(*rhsndstor);
                    ++rhsndstor;
                }
                ++resndstor;
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> insert(const Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t ind)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            COMPUTOC_THROW_IF_FALSE(ind <= lhs.header().count(), std::out_of_range, "index not in array dimension range");

            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ {lhs.header().count() + rhs.header().count()} };
            Array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> rlhs{ reshape(lhs, {lhs.header().count()}) };
            Array<T2, Data_buffer, Data_reference_allocator, Internals_buffer> rrhs{ reshape(rhs, {rhs.header().count()}) };
            for (std::int64_t i = 0; i < ind; ++i) {
                res({ i }) = rlhs({ i });
            }
            for (std::int64_t i = 0; i < rhs.header().count(); ++i) {
                res({ ind + i }) = rrhs({ i });
            }
            for (std::int64_t i = 0; i < lhs.header().count() - ind; ++i) {
                res({ ind + rhs.header().count() + i }) = rlhs({ ind + i });
            }
            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> remove(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(axis < arr.header().dims().s(), std::out_of_range, "axis out of dimensions range");
            COMPUTOC_THROW_IF_FALSE(ind + count <= arr.header().dims().p()[axis], std::out_of_range, "index plus count not in array dimension range");

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() - (arr.header().count() / arr.header().dims().p()[axis]) * count  };
            res.header() = typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(arr.header().dims(), -count, axis);

            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator arrndstor({}, arr.header().dims());
            typename Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscripts_iterator resndstor({}, res.header().dims());

            while (arrndstor) {
                if (resndstor && (*arrndstor).p()[axis] < ind || (*arrndstor).p()[axis] >= ind + count) {
                    res(*resndstor) = arr(*arrndstor);
                    ++resndstor;
                }
                ++arrndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> remove(const Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t ind, std::int64_t count)
        {
            if (empty(arr)) {
                return Array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(ind + count < arr.header().count(), std::out_of_range, "index plus count are not in array dimension range");

            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ {arr.header().count() - count} };
            Array<T, Data_buffer, Data_reference_allocator, Internals_buffer> rarr{ reshape(arr, {arr.header().count()}) };
            for (std::int64_t i = 0; i < ind; ++i) {
                res({ i }) = rarr({ i });
            }
            for (std::int64_t i = ind + count; i < arr.header().count(); ++i) {
                res({ ind + i - count + 1 }) = rarr({ i });
            }
            return res;
        }
    }

    using details::Array;
    using details::Array_subscripts_iterator;
    using details::all_match;
    using details::transform;
    using details::reduce;
    using details::all;
    using details::any;
    using details::filter;
    using details::find;
    using details::transpose;
    using details::close;
    using details::all_equal;
    using details::all_close;
    using details::copy;
    using details::clone;
    using details::reshape;
    using details::resize;
    using details::empty;
    using details::append;
    using details::insert;
    using details::remove;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
