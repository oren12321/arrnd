#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstdint>
#include <initializer_list>
#include <stdexcept>


#include <computoc/errors.h>
#include <computoc/utils.h>
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

        struct ND_range {
            ND_range(std::int64_t nstart, std::int64_t nstop, std::int64_t nstep) noexcept
            {
                if (nstep >= 0) {
                    start = nstart;
                    stop = nstop;
                    step = nstep;
                }
                else {
                    start = nstop;
                    stop = nstart;
                    step = -nstep;
                }
            }

            ND_range(std::int64_t nstart, std::int64_t nstop) noexcept
                : ND_range(nstart, nstop, 1) {}

            ND_range(std::int64_t nstart) noexcept
                : ND_range(nstart, nstart, 1) {}

            ND_range() = default;
            ND_range(const ND_range&) = default;
            ND_range& operator=(const ND_range&) = default;
            ND_range(ND_range&) = default;
            ND_range& operator=(ND_range&) = default;

            std::int64_t start{ 0 };
            std::int64_t stop{ 0 };
            std::int64_t step{ 1 };
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
        * Check if two dimensions are equal:
        * ----------------------------------
        * are_equal = f(D1,D2) = D1(1)=D2(1) and D1(2)=D2(2) and ... and D1(N)=D2(N)
        * 
        * Check if ranges are valid/legal:
        * --------------------------------
        * are_legal - f(R) = Rt(1)>0 and Rt(2)>0 and ... and Rt(N)>0
        */

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

        inline void ranges2strides(const Params<std::int64_t>& previous_strides, const Params<ND_range>& ranges, Params<std::int64_t> strides) noexcept
        {
            if (previous_strides.empty() || ranges.empty() || strides.empty()) {
                return;
            }

            if (previous_strides.s() != strides.s()) {
                return;
            }

            std::int64_t last{ ranges.s() < previous_strides.s() ? ranges.s() : previous_strides.s() };

            for (std::int64_t i = 0; i < last; ++i) {
                strides.p()[i] = previous_strides.p()[i] * ranges.p()[i].step;
            }
        }

        inline void ranges2dims(const Params<std::int64_t>& previous_dims, const Params<ND_range>& ranges, Params<std::int64_t> dims) noexcept
        {
            if (previous_dims.empty() || dims.empty()) {
                return;
            }

            if (previous_dims.s() != dims.s()) {
                return;
            }

            std::int64_t middle{ previous_dims.s() >= ranges.s() ? ranges.s() : previous_dims.s() };

            for (std::int64_t i = 0; i < middle; ++i) {
                dims.p()[i] = static_cast<std::int64_t>(std::ceil((modulo(ranges.p()[i].stop, previous_dims.p()[i]) - modulo(ranges.p()[i].start, previous_dims.p()[i]) + 1.0) / ranges.p()[i].step));
            }

            for (std::int64_t i = middle; i < previous_dims.s(); ++i) {
                dims.p()[i] = previous_dims.p()[i];
            }
        }

        inline std::int64_t ranges2offset(const Params<std::int64_t>& previous_dims, std::int64_t previous_offset, const Params<std::int64_t>& previous_strides, const Params<ND_range>& ranges) noexcept
        {
            std::int64_t offset{ previous_offset };

            if (previous_strides.empty() || ranges.empty()) {
                return offset;
            }

            std::int64_t last{ ranges.s() < previous_strides.s() ? ranges.s() : previous_strides.s() };

            for (std::int64_t i = 0; i < last; ++i) {
                offset += previous_strides.p()[i] * modulo(ranges.p()[i].start, previous_dims.p()[i]);
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

        inline bool valid_ranges(const Params<std::int64_t>& dims, const Params<ND_range>& ranges) noexcept
        {
            bool result{ true };
            std::int64_t last{ ranges.s() < dims.s() ? ranges.s() : dims.s() };

            for (std::int64_t i = 0; i < last && result; ++i) {
                std::int64_t dim{ dims.p()[i] };
                ND_range range{ modulo(ranges.p()[i].start, dim), modulo(ranges.p()[i].stop, dim), ranges.p()[i].step };

                result &= (range.start <= range.stop && range.step > 0);
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

        using ND_array_default_internals_buffer = memoc::Typed_buffer<std::int64_t, memoc::Fallback_buffer<
            memoc::Stack_buffer<3 * (MEMOC_SSIZEOF(std::int64_t) + MEMOC_SSIZEOF(std::int64_t))>,
            memoc::Allocated_buffer<memoc::Malloc_allocator, true>>>;

        template <memoc::Buffer<std::int64_t> Internal_buffer = ND_array_default_internals_buffer>
        class ND_header {
        public:
            ND_header() = default;

            ND_header(const Params<std::int64_t>& dims)
                : size_info_(dims.s() * 2)
            {
                if (dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), size_info_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    dims_.p()[i] = dims.p()[i];
                }

                strides_ = { dims.s(), size_info_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& previous_strides, std::int64_t previous_offset, const Params<ND_range>& derived_ranges)
                : is_partial_(true)
            {
                if (previous_dims.empty()) {
                    return;
                }

                if (!valid_ranges(previous_dims, derived_ranges)) {
                    return;
                }

                std::int64_t new_ndims = previous_dims.s();

                size_info_ = Internal_buffer(new_ndims * 2);
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { new_ndims, size_info_.data().p() };
                ranges2dims(previous_dims, derived_ranges, dims_);

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                strides_ = { new_ndims, size_info_.data().p() + new_ndims };
                ranges2strides(previous_strides, derived_ranges, strides_);
                if (previous_dims.s() > derived_ranges.s()) {
                    Params<std::int64_t> remained_dims{ previous_dims.s() - derived_ranges.s(), previous_dims.p() + derived_ranges.s() };
                    Params<std::int64_t> remained_strides{ previous_dims.s() - derived_ranges.s(), strides_.p() + derived_ranges.s() };
                    dims2strides(remained_dims, remained_strides);
                }

                offset_ = ranges2offset(previous_dims, previous_offset, previous_strides, derived_ranges);
            }

            ND_header(const Params<std::int64_t>& previous_dims, std::int64_t omitted_axis)
            {
                if (previous_dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(omitted_axis < previous_dims.s(), std::invalid_argument, "axis is invalid for number of dimensions");

                std::int64_t new_ndims{previous_dims.s() > 1 ? previous_dims.s() - 1 : 1};

                size_info_ = Internal_buffer(new_ndims * 2);
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { new_ndims, size_info_.data().p() };
                if (previous_dims.s() > 1) {
                    std::int64_t res_dim_index{ 0 };
                    for (std::int64_t i = 0; i < omitted_axis; ++i, res_dim_index += 1) {
                        dims_.p()[res_dim_index] = previous_dims.p()[i];
                    }
                    for (std::int64_t i = omitted_axis + 1; i < previous_dims.s(); ++i, res_dim_index += 1) {
                        dims_.p()[res_dim_index] = previous_dims.p()[i];
                    }
                }
                else {
                    dims_.p()[0] = 1;
                }

                strides_ = { new_ndims, size_info_.data().p() + new_ndims };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& new_order)
                : size_info_(previous_dims.s() * 2)
            {
                if (previous_dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");
                COMPUTOC_THROW_IF_FALSE(previous_dims.s() == new_order.s(), std::invalid_argument, "new_order and dimensions sizes are different");

                dims_ = { previous_dims.s(), size_info_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(new_order.p()[i] < dims_.s(), std::out_of_range, "new_order index not in dimensions range");
                    dims_.p()[i] = previous_dims.p()[new_order.p()[i]];
                }

                strides_ = { previous_dims.s(), size_info_.data().p() + previous_dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(const Params<std::int64_t>& dims, const Params<std::int64_t>& appended_dims, std::int64_t axis)
                : size_info_(dims.s() * 2)
            {
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), size_info_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    if (axis == i) {
                        dims_.p()[i] = dims.p()[i] + appended_dims.p()[i];
                    }
                    else {
                        dims_.p()[i] = dims.p()[i];
                    }
                }

                strides_ = { dims.s(), size_info_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(const Params<std::int64_t>& dims, std::int64_t count, std::int64_t axis)
                : size_info_(dims.s() * 2)
            {
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), size_info_.data().p() };
                for (std::int64_t i = 0; i < dims_.s(); ++i) {
                    if (axis == i) {
                        dims_.p()[i] = dims.p()[i] + count;
                    }
                    else {
                        dims_.p()[i] = dims.p()[i];
                    }
                }

                strides_ = { dims.s(), size_info_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(ND_header&& other) noexcept
                : size_info_(std::move(other.size_info_)), count_(other.count_), offset_(other.offset_), is_partial_(other.is_partial_)
            {
                dims_ = { other.dims_.s(), size_info_.data().p() };
                strides_ = { other.strides_.s(), size_info_.data().p() + other.dims_.s() };

                other.dims_.clear();
                other.strides_.clear();
                other.count_ = other.offset_ = 0;
                other.is_partial_ = false;
            }
            ND_header& operator=(ND_header&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                size_info_ = std::move(other.size_info_);
                count_ = other.count_;
                offset_ = other.offset_;
                is_partial_ = other.is_partial_;

                dims_ = { other.dims_.s(), size_info_.data().p() };
                strides_ = { other.strides_.s(), size_info_.data().p() + other.dims_.s() };

                other.dims_.clear();
                other.strides_.clear();
                other.count_ = other.offset_ = 0;
                other.is_partial_ = false;

                return *this;
            }

            ND_header(const ND_header& other) noexcept
                : size_info_(other.size_info_), count_(other.count_), offset_(other.offset_), is_partial_(other.is_partial_)
            {
                dims_ = { other.dims_.s(), size_info_.data().p() };
                strides_ = { other.strides_.s(), size_info_.data().p() + other.dims_.s() };
            }
            ND_header& operator=(const ND_header& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                size_info_ = other.size_info_;
                count_ = other.count_;
                offset_ = other.offset_;
                is_partial_ = other.is_partial_;

                dims_ = { other.dims_.s(), size_info_.data().p() };
                strides_ = { other.strides_.s(), size_info_.data().p() + other.dims_.s() };

                return *this;
            }

            virtual ~ND_header() = default;

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

        private:
            Params<std::int64_t> dims_{};
            Params<std::int64_t> strides_{};
            Internal_buffer size_info_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            bool is_partial_{ false };
        };


        template <memoc::Buffer<std::int64_t> Internal_buffer = ND_array_default_internals_buffer>
        class ND_subscriptor
        {
        public:
            ND_subscriptor(const Params<std::int64_t>& from, const Params<std::int64_t>& to, std::int64_t axis)
                : buff_(from.s(), from.p()), subs_(buff_.data()), from_(from.p()), to_(to.p()), axis_(axis)
            {
                COMPUTOC_THROW_IF_FALSE(!from.empty() && !to.empty(), std::invalid_argument, "'from' and/or 'to' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(from.s() == to.s(), std::invalid_argument, "'froms' and 'to' subscripts size are not equal");

                COMPUTOC_THROW_IF_FALSE(axis < to.s(), std::invalid_argument, "'axis' size is bigger or equal to 'to' size");

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
            }
            ND_subscriptor(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::int64_t axis)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(from), from.begin()), Params<std::int64_t>(std::ssize(to), to.begin()), axis)
            {
            }

            ND_subscriptor(const Params<std::int64_t>& to, std::int64_t axis)
                : buff_(to.s()), subs_(buff_.data()), to_(to.p()), axis_(axis)
            {
                COMPUTOC_THROW_IF_FALSE(!to.empty(), std::invalid_argument, "'to' subscripts size is zero");

                COMPUTOC_THROW_IF_FALSE(axis < to.s(), std::invalid_argument, "'axis' size is bigger or equal to 'to' size");

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                reset();
            }
            ND_subscriptor(std::initializer_list<std::int64_t> to, std::int64_t axis)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(to), to.begin()), axis)
            {
            }

            ND_subscriptor(const Params<std::int64_t>& from, const Params<std::int64_t>& to, const Params<std::int64_t>& order, Params<std::int64_t> dummy)
                : buff_(from.s() + order.s()), from_(from.p()), to_(to.p())
            {
                COMPUTOC_THROW_IF_FALSE(!from.empty() && !to.empty() && !order.empty(), std::invalid_argument, "'from', 'to' and/or 'order' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(from.s() == to.s() && to.s() == order.s(), std::invalid_argument, "'froms', 'to' and 'order' subscripts size are not equal");

                for (std::int64_t i = 0; i < order.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(order.p()[i] < order.s(), std::out_of_range, "out of range order value");
                }

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                subs_ = { from.s(), buff_.data().p() };
                for (std::int64_t i = 0; i < subs_.s(); ++i) {
                    subs_.p()[i] = from.p()[i];
                }
                order_ = { order.s(), buff_.data().p() + from.s() };
                for (std::int64_t i = 0; i < order_.s(); ++i) {
                    order_.p()[i] = order.p()[i];
                }
            }
            ND_subscriptor(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::initializer_list<std::int64_t> order, Params<std::int64_t> dummy)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(from), from.begin()), Params<std::int64_t>(std::ssize(to), to.begin()), Params<std::int64_t>(std::ssize(order), order.begin()))
            {
            }

            ND_subscriptor(const Params<std::int64_t>& to, const Params<std::int64_t>& order, Params<std::int64_t> dummy)
                : buff_(to.s() + order.s()), to_(to.p())
            {
                COMPUTOC_THROW_IF_FALSE(!to.empty() && !order.empty(), std::invalid_argument, "'to' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(to.s() == order.s(), std::invalid_argument, "'to' and 'order' subscripts size are not equal");

                for (std::int64_t i = 0; i < order.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(order.p()[i] < order.s(), std::out_of_range, "out of range order value");
                }

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                subs_ = { to.s(), buff_.data().p() };
                order_ = { order.s(), buff_.data().p() + to.s() };
                for (std::int64_t i = 0; i < order_.s(); ++i) {
                    order_.p()[i] = order.p()[i];
                }
                reset();
            }
            ND_subscriptor(std::initializer_list<std::int64_t> to, std::initializer_list<std::int64_t> order, Params<std::int64_t> dummy)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(to), to.begin()), Params<std::int64_t>(std::ssize(order), order.begin()))
            {
            }

            ND_subscriptor(const Params<std::int64_t>& from, const Params<std::int64_t>& to)
                : ND_subscriptor(from, to, to.s() - 1)
            {
            }
            ND_subscriptor(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(from), from.begin()), Params<std::int64_t>(std::ssize(to), to.begin()))
            {
            }

            ND_subscriptor(const Params<std::int64_t>& to)
                : ND_subscriptor(to, to.s() - 1)
            {
            }
            ND_subscriptor(std::initializer_list<std::int64_t> to)
                : ND_subscriptor(Params<std::int64_t>(std::ssize(to), to.begin()))
            {
            }

            ND_subscriptor() = default;

            ND_subscriptor(const ND_subscriptor<Internal_buffer>& other) noexcept
                : buff_(other.buff_), from_(other.from_), to_(other.to_), axis_(other.axis_)
            {
                subs_ = { other.subs_.s(), buff_.data().p() };
                order_ = { other.order_.s(), buff_.data().p() + other.subs_.s() };
            }
            ND_subscriptor& operator=(const ND_subscriptor<Internal_buffer>& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                from_ = other.from_;
                to_ = other.to_;
                subs_ = { other.subs_.s(), buff_.data().p() };
                order_ = { other.order_.s(), buff_.data().p() + other.subs_.s() };
                axis_ = other.axis_;
            }

            ND_subscriptor(ND_subscriptor<Internal_buffer>&& other) noexcept
                : buff_(std::move(other.buff_)), from_(other.from_), to_(other.to_), axis_(other.axis_)
            {
                subs_ = { other.subs_.s(), buff_.data().p() };
                order_ = { other.order_.s(), buff_.data().p() + other.subs_.s() };

                other.from_ = other.to_ = nullptr;
                other.subs_.clear();
                other.order_.clear();
                other.axis_ = 0;
            }
            ND_subscriptor& operator=(ND_subscriptor&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = std::move(other.buff_);
                from_ = other.from_;
                to_ = other.to_;
                subs_ = { other.subs_.s(), buff_.data().p() };
                order_ = { other.order_.s(), buff_.data().p() + other.subs_.s() };
                axis_ = other.axis_;

                other.from_ = other.to_ = nullptr;
                other.subs_.clear();
                other.order_.clear();
                other.axis_ = 0;
            }

            virtual ~ND_subscriptor() = default;

            void reset() noexcept
            {
                for (std::int64_t i = 0; i < subs_.s(); ++i) {
                    subs_.p()[i] = from_ ? from_[i] : 0;
                }
            }

            ND_subscriptor& operator++() noexcept
            {
                if (!order_.empty())
                {
                    bool should_process_sub{ true };

                    for (int64_t i = order_.s(); i >= 1 && should_process_sub; --i) {
                        if (subs_.p()[order_.p()[i - 1]] < to_[order_.p()[i - 1]]) {
                            ++subs_.p()[order_.p()[i - 1]];
                        }
                        if ((should_process_sub = (subs_.p()[order_.p()[i - 1]] == to_[order_.p()[i - 1]])) && order_.p()[i - 1] != order_.p()[0]) {
                            subs_.p()[order_.p()[i - 1]] = from_ ? from_[i-1] : 0;
                        }
                    }

                    return *this;
                }


                bool should_process_sub{ true };
                const std::int64_t stop_axis{ axis_ > 0 ? std::int64_t{0} : (subs_.s() > 1 ? std::int64_t{1} : std::int64_t{0}) };

                if (subs_.p()[axis_] < to_[axis_]) {
                    ++subs_.p()[axis_];
                }
                if ((should_process_sub = (subs_.p()[axis_] == to_[axis_])) && axis_ != stop_axis) {
                    subs_.p()[axis_] = from_ ? from_[axis_] : 0;
                }

                for (std::int64_t i = subs_.s(); i >= 1 && should_process_sub; --i) {
                    if (axis_ != i - 1 && subs_.p()[i - 1] < to_[i - 1]) {
                        ++subs_.p()[i - 1];
                    }
                    if (axis_ != i - 1 && (should_process_sub = (subs_.p()[i - 1] == to_[i - 1])) && i != stop_axis + 1) {
                        subs_.p()[i - 1] = from_ ? from_[i-1] : 0;
                    }
                }
                return *this;
            }

            ND_subscriptor operator++(int) noexcept
            {
                ND_subscriptor temp{ *this };
                ++(*this);
                return temp;
            }

            operator bool() const noexcept
            {
                if (!order_.empty()) {
                    return subs_.p()[order_.p()[0]] != to_[order_.p()[0]];
                }

                const std::int64_t stop_axis{ axis_ > 0 ? std::int64_t{0} : (subs_.s() > 1 ? std::int64_t{1} : std::int64_t{0}) };
                return subs_.p()[stop_axis] != to_[stop_axis];
            }

            const Params<std::int64_t>& subs() const noexcept
            {
                return subs_;
            }

        private:
            Internal_buffer buff_{};
            Params<std::int64_t> subs_{};
            const std::int64_t* from_{ nullptr };
            const std::int64_t* to_{ nullptr };
            Params<std::int64_t> order_{};
            std::int64_t axis_{ 0 };
        };

        using ND_array_default_data_reference_allocator = memoc::Malloc_allocator;

        using ND_array_default_data_buffer = memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * MEMOC_SSIZEOF(std::int64_t)>,
            memoc::Allocated_buffer<memoc::Malloc_allocator, true>>;

        template <typename T, memoc::Buffer Data_buffer = ND_array_default_data_buffer, memoc::Allocator Data_reference_allocator = ND_array_default_data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer = ND_array_default_internals_buffer>
        class ND_array {
        public:
            using Header = ND_header<Internals_buffer>;
            using Subscriptor = ND_subscriptor<Internals_buffer>;

            ND_array() = default;

            ND_array(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other) = default;
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            ND_array(ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)
            {
                copy(other, *this);

                ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{ std::move(other) };
            }
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other) & = default;
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& other)&&
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
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)&
            {
                copy(other, *this);
                ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>&& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o> dummy{std::move(other)};
                return *this;
            }

            ND_array(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other) = default;
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            ND_array(const ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)
            {
                copy(other, *this);
            }
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other) & = default;
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& other)&&
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
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)&
            {
                copy(other, *this);
                return *this;
            }
            template< typename T_o, memoc::Buffer Data_buffer_o, memoc::Allocator Data_reference_allocator_o, memoc::Buffer<std::int64_t> Internals_buffer_o>
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const ND_array<T_o, Data_buffer_o, Data_reference_allocator_o, Internals_buffer_o>& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                return *this;
            }

            template <typename U>
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }
                ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ Params<std::int64_t>(hdr_.dims()) };
                while (ndstor) {
                    (*this)(ndstor.subs()) = value;
                    ++ndstor;
                }
                return *this;
            }

            virtual ~ND_array() = default;

            ND_array(const Params<std::int64_t>& dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count(), data))
            {
            }
            ND_array(std::initializer_list<std::int64_t> dims, const T* data = nullptr)
                : ND_array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data)
            {
            }
            template <typename U>
            ND_array(const Params<std::int64_t>& dims, const U* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = data[i];
                }
            }
            template <typename U>
            ND_array(std::initializer_list<std::int64_t> dims, const U* data = nullptr)
                : ND_array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, data)
            {
            }


            ND_array(const Params<std::int64_t>& dims, const T& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            ND_array(std::initializer_list<std::int64_t> dims, const T& value)
                : ND_array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
            {
            }
            template <typename U>
            ND_array(const Params<std::int64_t>& dims, const U& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator>(hdr_.count()))
            {
                for (std::int64_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            template <typename U>
            ND_array(std::initializer_list<std::int64_t> dims, const U& value)
                : ND_array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
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

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> operator()(const Params<ND_range>& ranges) const
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

                ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> slice{};
                slice.hdr_ = Header{ hdr_.dims(), hdr_.strides(), hdr_.offset(), ranges };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> operator()(std::initializer_list<ND_range> ranges) const
            {
                return (*this)(Params<ND_range>{std::ssize(ranges), ranges.begin()});
            }

        private:
            Header hdr_{};
            memoc::Shared_ptr<memoc::Typed_buffer<T, Data_buffer>, Data_reference_allocator> buffsp_{ nullptr };
        };

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>    
        inline auto transform(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
            -> ND_array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            if (empty(arr)) {
                return ND_array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            ND_array<decltype(func(arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().dims() };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ arr.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(arr(ndstor.subs()));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto reduce(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
            -> decltype(func(arr.data()[0], arr.data()[0]))
        {
            if (empty(arr)) {
                return decltype(func(arr.data()[0], arr.data()[0])){};
            }

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ arr.header().dims() };

            decltype(func(arr.data()[0], arr.data()[0])) res{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(ndstor.subs())) };
            ++ndstor;

            while (ndstor) {
                res = func(arr(ndstor.subs()), res);
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto reduce(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func, std::int64_t axis)
            -> ND_array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            if (empty(arr)) {
                return ND_array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(axis < arr.header().dims().s(), std::invalid_argument, "axis is invalid for number of dimensions");

            ND_array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ {arr.header().count() / arr.header().dims().p()[axis]} };

            res.header() = typename ND_array<decltype(func(arr.data()[0], arr.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>::Header{ arr.header().dims(), axis };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims(), axis };
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            const std::int64_t reduction_iteration_cycle{ arr.header().dims().p()[axis] };

            while (arr_ndstor && res_ndstor) {
                decltype(func(arr.data()[0], arr.data()[0])) res_element{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(arr_ndstor.subs())) };
                ++arr_ndstor;
                for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++arr_ndstor) {
                    res_element = func(arr(arr_ndstor.subs()), res_element);
                }
                res(res_ndstor.subs()) = res_element;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool all(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) && static_cast<bool>(previous); });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> all(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) && static_cast<bool>(previous); }, axis);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool any(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) || static_cast<bool>(previous); });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> any(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& value, const T& previous) { return static_cast<bool>(value) || static_cast<bool>(previous); }, axis);
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto binary(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
            -> ND_array<decltype(func(lhs.data()[0], rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            COMPUTOC_THROW_IF_FALSE(lhs.header().dims() == rhs.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<decltype(func(lhs.data()[0], rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().dims() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ lhs.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(lhs(ndstor.subs()), rhs(ndstor.subs()));
                ++ndstor;
            }

            return res;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto binary(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs, Func func)
            -> ND_array<decltype(func(lhs.data()[0], rhs)), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            ND_array<decltype(func(lhs.data()[0], rhs)), Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().dims() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ lhs.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(lhs(ndstor.subs()), rhs);
                ++ndstor;
            }

            return res;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto binary(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, Func func)
            -> ND_array<decltype(func(lhs, rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            ND_array<decltype(func(lhs, rhs.data()[0])), Data_buffer, Data_reference_allocator, Internals_buffer> res{ rhs.header().dims() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ rhs.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(lhs, rhs(ndstor.subs()));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> filter(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
        {
            if (empty(arr)) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(arr_ndstor.subs()))) {
                    res(res_ndstor.subs()) = arr(arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> filter(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& mask)
        {
            if (empty(arr)) {
                return ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor mask_ndstor{ mask.header().dims() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && mask_ndstor && res_ndstor) {
                if (mask(mask_ndstor.subs())) {
                    res(res_ndstor.subs()) = arr(arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
                ++mask_ndstor;
            }

            if (res_count == 0) {
                return ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> find(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, Func func)
        {
            if (empty(arr)) {
                return ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(arr_ndstor.subs()))) {
                    res(res_ndstor.subs()) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <
            typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> find(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& mask)
        {
            if (empty(arr)) {
                return ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor mask_ndstor{ mask.header().dims() };

            typename ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            std::int64_t res_count{ 0 };

            while (arr_ndstor && mask_ndstor && res_ndstor) {
                if (mask(mask_ndstor.subs())) {
                    res(res_ndstor.subs()) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
                ++mask_ndstor;
            }

            if (res_count == 0) {
                return ND_array<std::int64_t, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> transpose(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& order)
        {
            if (empty(arr)) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() };
            res.header() = typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header{ arr.header().dims(), order };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arr_ndstor{ arr.header().dims(), order, {0, nullptr} };
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor res_ndstor{ res.header().dims() };

            while (arr_ndstor && res_ndstor) {
                res(res_ndstor.subs()) = arr(arr_ndstor.subs());
                ++arr_ndstor;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> transpose(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> order)
        {
            return transpose(arr, Params<std::int64_t>{std::ssize(order), order.begin()});
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> equal(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> not_equal(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> close(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>())
        {
            return binary(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator>=(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<bool, Data_buffer, Data_reference_allocator, Internals_buffer> operator<=(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator+(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator+=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator+=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator-(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator-=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator-=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator*(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator*=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator*=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator/(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator/=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator/=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator%(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator%=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator%=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator^(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator^=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator^=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator&=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator&=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator|(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator|=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator|=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator<<(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
            -> ND_array<decltype(lhs << rhs.data()[0]), Data_buffer, Data_reference_allocator, Internals_buffer>
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator<<=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator<<=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator>>(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator>>=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
            return lhs;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator>>=(ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            lhs = binary(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
            return lhs;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator~(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return transform(arr, [](const T& a) { return ~a; });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator!(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            return transform(arr, [](const T& a) { return !a; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator&&(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const T2& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator||(const T1& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            return binary(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator++(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            if (empty(arr)) {
                return arr;
            }
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ arr.header().dims() };
            while (ndstor) {
                ++arr(ndstor.subs());
                ++ndstor;
            }
            return arr;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr)
        {
            return operator++(arr);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, int)
        {
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> old = clone(arr);
            operator++(arr);
            return old;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator++(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto& operator--(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            if (empty(arr)) {
                return arr;
            }
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ arr.header().dims() };
            while (ndstor) {
                --arr(ndstor.subs());
                ++ndstor;
            }
            return arr;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr)
        {
            return operator--(arr);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, int)
        {
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> old = clone(arr);
            operator--(arr);
            return old;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline auto operator--(ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <
            typename T1, memoc::Buffer Data_buffer1, memoc::Allocator Data_reference_allocator1, memoc::Buffer<std::int64_t> Internals_buffer1,
            typename T2, memoc::Buffer Data_buffer2, memoc::Allocator Data_reference_allocator2, memoc::Buffer<std::int64_t> Internals_buffer2>
        inline bool operator==(const ND_array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>& lhs, const ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>& rhs)
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

            typename ND_array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>::Subscriptor lhs_ndstor{ lhs.header().dims() };
            typename ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>::Subscriptor rhs_ndstor{ rhs.header().dims() };

            while (lhs_ndstor && rhs_ndstor) {
                if (lhs(lhs_ndstor.subs()) != rhs(rhs_ndstor.subs())) {
                    return false;
                }
                ++lhs_ndstor;
                ++rhs_ndstor;
            }

            return true;
        }

        template <
            typename T1, memoc::Buffer Data_buffer1, memoc::Allocator Data_reference_allocator1, memoc::Buffer<std::int64_t> Internals_buffer1,
            typename T2, memoc::Buffer Data_buffer2, memoc::Allocator Data_reference_allocator2, memoc::Buffer<std::int64_t> Internals_buffer2>
        inline void copy(const ND_array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>& src, ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>& dst)
        {
            /*
            * Algorithm:
            * - empty array     -> empty array
            * - not equal count -> create new buffer
            * - copy elements
            */

            if (empty(src)) {
                dst = ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>{};
                return;
            }

            if (src.header().count() != dst.header().count()) {
                dst = ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>(src.header().dims());
            }

            typename ND_array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>::Subscriptor src_ndstor{ src.header().dims() };
            typename ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>::Subscriptor dst_ndstor{ dst.header().dims() };

            while (src_ndstor && dst_ndstor) {
                dst(dst_ndstor.subs()) = src(src_ndstor.subs());
                ++src_ndstor;
                ++dst_ndstor;
            }
        }
        template <
            typename T1, memoc::Buffer Data_buffer1, memoc::Allocator Data_reference_allocator1, memoc::Buffer<std::int64_t> Internals_buffer1,
            typename T2, memoc::Buffer Data_buffer2, memoc::Allocator Data_reference_allocator2, memoc::Buffer<std::int64_t> Internals_buffer2>
        inline void copy(const ND_array<T1, Data_buffer1, Data_reference_allocator1, Internals_buffer1>& src, ND_array<T2, Data_buffer2, Data_reference_allocator2, Internals_buffer2>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> clone(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr)
        {
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> clone{};

            if (empty(arr)) {
                return clone;
            }

            clone = ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{ arr.header().dims() };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor ndstor{ arr.header().dims() };

            while (ndstor) {
                clone(ndstor.subs()) = arr(ndstor.subs());
                ++ndstor;
            }
            return clone;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> reshape(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& new_dims)
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
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().count() == dims2count(new_dims), std::invalid_argument, "different number of elements between original and rehsaped arrays");

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().is_partial()) {
                ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ new_dims };

                typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor prev_ndstor(arr.header().dims());
                typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor new_ndstor(new_dims);

                while (prev_ndstor && new_ndstor) {
                    res(new_ndstor.subs()) = arr(prev_ndstor.subs());
                    ++prev_ndstor;
                    ++new_ndstor;
                }

                return res;
            }

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr };
            res.header() = typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(new_dims);

            return res;
        }
        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> reshape(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> resize(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, const Params<std::int64_t>& new_dims)
        {
            /*
            * Resizing algorithm:
            * - return new array of the new size containing the original array data or part of it.
            */
            if (new_dims.empty()) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            if (empty(arr)) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>(new_dims);
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ new_dims };

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor prev_ndstor(arr.header().dims());
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor new_ndstor(new_dims);

            while (prev_ndstor && new_ndstor) {
                res(new_ndstor.subs()) = arr(prev_ndstor.subs());
                ++prev_ndstor;
                ++new_ndstor;
            }

            return res;
        }
        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> resize(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline bool empty(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr) noexcept
        {
            return !arr.data() || (arr.header().count() == 0);
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> append(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t axis)
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

            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().count() + rhs.header().count() };
            res.header() = typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(lhs.header().dims(), rhs.header().dims(), axis);

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor lhsndstor(lhs.header().dims());
            typename ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor rhsndstor(rhs.header().dims());
            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor resndstor(res.header().dims());

            while (resndstor) {
                if (lhsndstor && resndstor.subs().p()[axis] < lhs.header().dims().p()[axis] || resndstor.subs().p()[axis] >= lhs.header().dims().p()[axis] + rhs.header().dims().p()[axis]) {
                    res(resndstor.subs()) = lhs(lhsndstor.subs());
                    ++lhsndstor;
                }
                else if (rhsndstor && resndstor.subs().p()[axis] >= lhs.header().dims().p()[axis] && resndstor.subs().p()[axis] < lhs.header().dims().p()[axis] + rhs.header().dims().p()[axis]) {
                    res(resndstor.subs()) = rhs(rhsndstor.subs());
                    ++rhsndstor;
                }
                ++resndstor;
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> append(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ resize(lhs, {lhs.header().count() + rhs.header().count()}) };
            ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer> rrhs{ reshape(rhs, {rhs.header().count()}) };
            for (std::int64_t i = lhs.header().count(); i < res.header().count(); ++i) {
                res({ i }) = rhs({ i - lhs.header().count() });
            }
            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> insert(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t ind, std::int64_t axis)
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

            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ lhs.header().count() + rhs.header().count() };
            res.header() = typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(lhs.header().dims(), rhs.header().dims(), axis);

            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor lhsndstor(lhs.header().dims());
            typename ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor rhsndstor(rhs.header().dims());
            typename ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor resndstor(res.header().dims());

            while (resndstor) {
                if (lhsndstor && resndstor.subs().p()[axis] < ind || resndstor.subs().p()[axis] >= ind + rhs.header().dims().p()[axis]) {
                    res(resndstor.subs()) = lhs(lhsndstor.subs());
                    ++lhsndstor;
                }
                else if (rhsndstor && resndstor.subs().p()[axis] >= ind && resndstor.subs().p()[axis] < ind + rhs.header().dims().p()[axis]) {
                    res(resndstor.subs()) = rhs(rhsndstor.subs());
                    ++rhsndstor;
                }
                ++resndstor;
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> insert(const ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer>& lhs, const ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer>& rhs, std::int64_t ind)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            COMPUTOC_THROW_IF_FALSE(ind <= lhs.header().count(), std::out_of_range, "index not in array dimension range");

            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> res{ {lhs.header().count() + rhs.header().count()} };
            ND_array<T1, Data_buffer, Data_reference_allocator, Internals_buffer> rlhs{ reshape(lhs, {lhs.header().count()}) };
            ND_array<T2, Data_buffer, Data_reference_allocator, Internals_buffer> rrhs{ reshape(rhs, {rhs.header().count()}) };
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
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> remove(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            if (empty(arr)) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(axis < arr.header().dims().s(), std::out_of_range, "axis out of dimensions range");
            COMPUTOC_THROW_IF_FALSE(ind + count <= arr.header().dims().p()[axis], std::out_of_range, "index plus count not in array dimension range");

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ arr.header().count() - (arr.header().count() / arr.header().dims().p()[axis]) * count  };
            res.header() = typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Header(arr.header().dims(), -count, axis);

            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor arrndstor(arr.header().dims());
            typename ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>::Subscriptor resndstor(res.header().dims());

            while (arrndstor) {
                if (resndstor && arrndstor.subs().p()[axis] < ind || arrndstor.subs().p()[axis] >= ind + count) {
                    res(resndstor.subs()) = arr(arrndstor.subs());
                    ++resndstor;
                }
                ++arrndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Data_buffer, memoc::Allocator Data_reference_allocator, memoc::Buffer<std::int64_t> Internals_buffer>
        inline ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> remove(const ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>& arr, std::int64_t ind, std::int64_t count)
        {
            if (empty(arr)) {
                return ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer>{};
            }

            COMPUTOC_THROW_IF_FALSE(ind + count < arr.header().count(), std::out_of_range, "index plus count are not in array dimension range");

            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> res{ {arr.header().count() - count} };
            ND_array<T, Data_buffer, Data_reference_allocator, Internals_buffer> rarr{ reshape(arr, {arr.header().count()}) };
            for (std::int64_t i = 0; i < ind; ++i) {
                res({ i }) = rarr({ i });
            }
            for (std::int64_t i = ind + count; i < arr.header().count(); ++i) {
                res({ ind + i - count + 1 }) = rarr({ i });
            }
            return res;
        }
    }

    using details::ND_range;
    using details::ND_array;
    using details::transform;
    using details::binary;
    using details::reduce;
    using details::all;
    using details::any;
    using details::filter;
    using details::find;
    using details::transpose;
    using details::equal;
    using details::not_equal;
    using details::close;
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
