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

        inline void dims2strides(const Params<std::size_t>& dims, Params<std::size_t> strides) noexcept
        {
            if (dims.empty() || strides.empty()) {
                return;
            }

            if (dims.s() != strides.s()) {
                return;
            }

            strides.p()[dims.s() - 1] = 1;
            for (std::size_t i = dims.s() - 1; i >= 1; --i) {
                strides.p()[i - 1] = strides.p()[i] * dims.p()[i];
            }
        }

        inline void ranges2strides(const Params<std::size_t>& previous_strides, const Params<ND_range>& ranges, Params<std::size_t> strides) noexcept
        {
            if (previous_strides.empty() || ranges.empty() || strides.empty()) {
                return;
            }

            if (previous_strides.s() != strides.s()) {
                return;
            }

            if (ranges.s() > strides.s()) {
                return;
            }

            for (std::size_t i = 0; i < ranges.s(); ++i) {
                strides.p()[i] = previous_strides.p()[i] * ranges.p()[i].step;
            }
        }

        inline void ranges2dims(const Params<std::size_t>& previous_dims, const Params<ND_range>& ranges, Params<std::size_t> dims) noexcept
        {
            if (previous_dims.empty() || dims.empty()) {
                return;
            }

            if (previous_dims.s() != dims.s()) {
                return;
            }

            if (ranges.s() > dims.s()) {
                return;
            }

            for (std::size_t i = 0; i < ranges.s(); ++i) {
                dims.p()[i] = static_cast<std::size_t>(std::ceil((ranges.p()[i].stop - ranges.p()[i].start + 1.0) / ranges.p()[i].step));
            }

            for (std::size_t i = ranges.s(); i < previous_dims.s(); ++i) {
                dims.p()[i] = previous_dims.p()[i];
            }
        }

        inline std::size_t ranges2offset(std::size_t previous_offset, const Params<std::size_t>& previous_strides, const Params<ND_range>& ranges) noexcept
        {
            std::size_t offset{ previous_offset };

            if (previous_strides.empty() || ranges.empty()) {
                return offset;
            }

            if (ranges.s() > previous_strides.s()) {
                return offset;
            }

            for (std::size_t i = 0; i < ranges.s(); ++i) {
                offset += previous_strides.p()[i] * ranges.p()[i].start;
            }
            return offset;
        }

        inline std::size_t subs2ind(std::size_t offset, const Params<std::size_t>& strides, const Params<std::size_t>& subs) noexcept
        {
            std::size_t ind{ offset };

            if (strides.empty() || subs.empty()) {
                return ind;
            }

            if (subs.s() > strides.s()) {
                return ind;
            }

            std::size_t zero_nsubs{ strides.s() - subs.s() };
            for (std::size_t i = zero_nsubs; i < strides.s(); ++i) {
                ind += strides.p()[i] * subs.p()[i - zero_nsubs];
            }
            return ind;
        }

        inline std::size_t dims2count(const Params<std::size_t>& dims) noexcept
        {
            if (dims.empty()) {
                return 0;
            }

            std::size_t count{ 1 };
            for (std::size_t i = 0; i < dims.s(); ++i) {
                count *= dims.p()[i];
            }
            return count;
        }

        inline bool subs_in_dims(const Params<std::size_t>& dims, const Params<std::size_t>& subs) noexcept
        {
            if (subs.empty() || dims.empty()) {
                return false;
            }

            if (subs.s() > dims.s()) {
                return false;
            }

            bool result{ true };
            std::size_t zero_nsubs{ dims.s() - subs.s() };
            for (std::size_t i = zero_nsubs; i < dims.s() && result; ++i) {
                result &= (subs.p()[i - zero_nsubs] < dims.p()[i]);
            }
            return result;
        }

        inline bool legal_ranges(const Params<ND_range>& ranges) noexcept
        {
            if (ranges.empty()) {
                return false;
            }

            bool result{ true };
            for (std::size_t i = 0; i < ranges.s() && result; ++i) {
                result &= (ranges.p()[i].start <= ranges.p()[i].stop && ranges.p()[i].step > 0);
            }
            return result;
        }

        inline bool ranges_in_dims(const Params<std::size_t>& dims, const Params<ND_range>& ranges) noexcept
        {
            if (ranges.empty() || dims.empty()) {
                return false;
            }

            if (ranges.s() > dims.s()) {
                return false;
            }

            bool result{ true };
            for (std::size_t i = 0; i < ranges.s() && result; ++i) {
                result &= (ranges.p()[i].stop < dims.p()[i]);
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

            ND_header(const Params<std::size_t>& dims)
                : size_info_(dims.s() * 2)
            {
                if (dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { dims.s(), size_info_.data().p() };
                for (std::size_t i = 0; i < dims_.s(); ++i) {
                    dims_.p()[i] = dims.p()[i];
                }

                strides_ = { dims.s(), size_info_.data().p() + dims.s() };

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::invalid_argument, "all dimensions should be > 0");

                dims2strides(dims_, strides_);
            }

            ND_header(const Params<std::size_t>& previous_dims, const Params<std::size_t>& previous_strides, std::size_t previous_offset, const Params<ND_range>& derived_ranges)
                : is_partial_(true)
            {
                if (previous_dims.empty()) {
                    return;
                }

                std::size_t new_ndims = previous_dims.s() >= derived_ranges.s() ? previous_dims.s() : derived_ranges.s();

                size_info_ = Internal_buffer(new_ndims * 2);
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { new_ndims, size_info_.data().p() };
                ranges2dims(previous_dims, derived_ranges, dims_);

                count_ = dims2count(dims_);
                COMPUTOC_THROW_IF_FALSE(count_ > 0, std::runtime_error, "all dimensions should be > 0");

                strides_ = { new_ndims, size_info_.data().p() + new_ndims };
                ranges2strides(previous_strides, derived_ranges, strides_);
                if (previous_dims.s() > derived_ranges.s()) {
                    Params<std::size_t> remained_dims{ previous_dims.s() - derived_ranges.s(), previous_dims.p() + derived_ranges.s() };
                    Params<std::size_t> remained_strides{ previous_dims.s() - derived_ranges.s(), strides_.p() + derived_ranges.s() };
                    dims2strides(remained_dims, remained_strides);
                }

                offset_ = ranges2offset(previous_offset, previous_strides, derived_ranges);
            }

            ND_header(const Params<std::size_t>& previous_dims, std::size_t omitted_axis)
            {
                if (previous_dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(omitted_axis < previous_dims.s(), std::invalid_argument, "axis is invalid for number of dimensions");

                std::size_t new_ndims{previous_dims.s() > 1 ? previous_dims.s() - 1 : 1};

                size_info_ = Internal_buffer(new_ndims * 2);
                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");

                dims_ = { new_ndims, size_info_.data().p() };
                if (previous_dims.s() > 1) {
                    std::size_t res_dim_index{ 0 };
                    for (std::size_t i = 0; i < omitted_axis; ++i, res_dim_index += 1) {
                        dims_.p()[res_dim_index] = previous_dims.p()[i];
                    }
                    for (std::size_t i = omitted_axis + 1; i < previous_dims.s(); ++i, res_dim_index += 1) {
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

            ND_header(const Params<std::size_t>& previous_dims, const Params<std::size_t>& new_order)
                : size_info_(previous_dims.s() * 2)
            {
                if (previous_dims.empty()) {
                    return;
                }

                COMPUTOC_THROW_IF_FALSE(size_info_.usable(), std::runtime_error, "failed to allocate header buffer");
                COMPUTOC_THROW_IF_FALSE(previous_dims.s() == new_order.s(), std::invalid_argument, "new_order and dimensions sizes are different");

                dims_ = { previous_dims.s(), size_info_.data().p() };
                for (std::size_t i = 0; i < dims_.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(new_order.p()[i] < dims_.s(), std::out_of_range, "new_order index not in dimensions range");
                    dims_.p()[i] = previous_dims.p()[new_order.p()[i]];
                }

                strides_ = { previous_dims.s(), size_info_.data().p() + previous_dims.s() };

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

            std::size_t count() const noexcept
            {
                return count_;
            }

            const Params<std::size_t> dims() const noexcept
            {
                return dims_;
            }

            const Params<std::size_t> strides() const noexcept
            {
                return strides_;
            }

            std::size_t offset() const noexcept
            {
                return offset_;
            }

            bool is_partial() const noexcept
            {
                return is_partial_;
            }

        private:
            Params<std::size_t> dims_{};
            Params<std::size_t> strides_{};
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
            ND_subscriptor(const Params<std::size_t>& from, const Params<std::size_t>& to, std::size_t axis)
                : buff_(from.s(), from.p()), subs_(buff_.data()), from_(from.p()), to_(to.p()), axis_(axis)
            {
                COMPUTOC_THROW_IF_FALSE(!from.empty() && !to.empty(), std::invalid_argument, "'from' and/or 'to' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(from.s() == to.s(), std::invalid_argument, "'froms' and 'to' subscripts size are not equal");

                COMPUTOC_THROW_IF_FALSE(axis < to.s(), std::invalid_argument, "'axis' size is bigger or equal to 'to' size");

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
            }
            ND_subscriptor(std::initializer_list<std::size_t> from, std::initializer_list<std::size_t> to, std::size_t axis)
                : ND_subscriptor(Params<std::size_t>(from.size(), from.begin()), Params<std::size_t>(to.size(), to.begin()), axis)
            {
            }

            ND_subscriptor(const Params<std::size_t>& to, std::size_t axis)
                : buff_(to.s()), subs_(buff_.data()), to_(to.p()), axis_(axis)
            {
                COMPUTOC_THROW_IF_FALSE(!to.empty(), std::invalid_argument, "'to' subscripts size is zero");

                COMPUTOC_THROW_IF_FALSE(axis < to.s(), std::invalid_argument, "'axis' size is bigger or equal to 'to' size");

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                reset();
            }
            ND_subscriptor(std::initializer_list<std::size_t> to, std::size_t axis)
                : ND_subscriptor(Params<std::size_t>(to.size(), to.begin()), axis)
            {
            }

            ND_subscriptor(const Params<std::size_t>& from, const Params<std::size_t>& to, const Params<std::size_t>& order, Params<std::size_t> dummy)
                : buff_(from.s(), from.p()), subs_(buff_.data()), from_(from.p()), to_(to.p()), order_(order.s(), order.p())
            {
                COMPUTOC_THROW_IF_FALSE(!from.empty() && !to.empty() && !order.empty(), std::invalid_argument, "'from', 'to' and/or 'order' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(from.s() == to.s() && to.s() == order.s(), std::invalid_argument, "'froms', 'to' and 'order' subscripts size are not equal");

                for (std::size_t i = 0; i < order.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(order.p()[i] < order.s(), std::out_of_range, "out of range order value");
                }

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                COMPUTOC_THROW_IF_FALSE(order_.usable(), std::runtime_error, "order buffer allocation failed");
            }
            ND_subscriptor(std::initializer_list<std::size_t> from, std::initializer_list<std::size_t> to, std::initializer_list<std::size_t> order, Params<std::size_t> dummy)
                : ND_subscriptor(Params<std::size_t>(from.size(), from.begin()), Params<std::size_t>(to.size(), to.begin()), Params<std::size_t>(order.size(), order.begin()))
            {
            }

            ND_subscriptor(const Params<std::size_t>& to, const Params<std::size_t>& order, Params<std::size_t> dummy)
                : buff_(to.s()), subs_(buff_.data()), to_(to.p()), order_(order.s(), order.p())
            {
                COMPUTOC_THROW_IF_FALSE(!to.empty() && !order.empty(), std::invalid_argument, "'to' subscripts size is zero");
                COMPUTOC_THROW_IF_FALSE(to.s() == order.s(), std::invalid_argument, "'to' and 'order' subscripts size are not equal");

                for (std::size_t i = 0; i < order.s(); ++i) {
                    COMPUTOC_THROW_IF_FALSE(order.p()[i] < order.s(), std::out_of_range, "out of range order value");
                }

                COMPUTOC_THROW_IF_FALSE(buff_.usable(), std::runtime_error, "subscriptor buffer allocation failed");
                COMPUTOC_THROW_IF_FALSE(order_.usable(), std::runtime_error, "order buffer allocation failed");
                reset();
            }
            ND_subscriptor(std::initializer_list<std::size_t> to, std::initializer_list<std::size_t> order, Params<std::size_t> dummy)
                : ND_subscriptor(Params<std::size_t>(to.size(), to.begin()), Params<std::size_t>(order.size(), order.begin()))
            {
            }

            ND_subscriptor(const Params<std::size_t>& from, const Params<std::size_t>& to)
                : ND_subscriptor(from, to, to.s() - 1)
            {
            }
            ND_subscriptor(std::initializer_list<std::size_t> from, std::initializer_list<std::size_t> to)
                : ND_subscriptor(Params<std::size_t>(from.size(), from.begin()), Params<std::size_t>(to.size(), to.begin()))
            {
            }

            ND_subscriptor(const Params<std::size_t>& to)
                : ND_subscriptor(to, to.s() - 1)
            {
            }
            ND_subscriptor(std::initializer_list<std::size_t> to)
                : ND_subscriptor(Params<std::size_t>(to.size(), to.begin()))
            {
            }

            ND_subscriptor() = default;

            ND_subscriptor(const ND_subscriptor<Internal_buffer>& other) noexcept
                : buff_(other.buff_), from_(other.from_), to_(other.to_), axis_(other.axis_), order_(other.order_)
            {
                subs_ = buff_.data();
            }
            ND_subscriptor& operator=(const ND_subscriptor<Internal_buffer>& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                from_ = other.from_;
                to_ = other.to_;
                subs_ = buff_.data();
                axis_ = other.axis_;
                order_ = other.order_;
            }

            ND_subscriptor(ND_subscriptor<Internal_buffer>&& other) noexcept
                : buff_(std::move(other.buff_)), from_(other.from_), to_(other.to_), axis_(other.axis_), order_(std::move(other.order_))
            {
                subs_ = buff_.data();

                other.from_ = other.to_ = nullptr;
                other.subs_.clear();
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
                subs_ = buff_.data();
                axis_ = other.axis_;
                order_ = std::move(other.order_);

                other.from_ = other.to_ = nullptr;
                other.subs_.clear();
                other.axis_ = 0;
            }

            virtual ~ND_subscriptor() = default;

            void reset() noexcept
            {
                for (std::size_t i = 0; i < subs_.s(); ++i) {
                    subs_.p()[i] = from_ ? from_[i] : 0;
                }
            }

            ND_subscriptor& operator++() noexcept
            {
                if (order_.usable())
                {
                    bool should_process_sub{ true };

                    for (size_t i = order_.data().s(); i >=1 && should_process_sub; --i) {
                        if ((should_process_sub = (++subs_.p()[order_.data().p()[i-1]] == to_[order_.data().p()[i-1]])) && order_.data().p()[i-1] != order_.data().p()[0]) {
                            subs_.p()[order_.data().p()[i-1]] = 0;
                        }
                    }

                    return *this;
                }


                bool should_process_sub{ true };
                const std::size_t stop_axis{ axis_ > 0 ? std::size_t{0} : (subs_.s() > 1 ? std::size_t{1} : std::size_t{0}) };

                if ((should_process_sub = (++subs_.p()[axis_] == to_[axis_])) && axis_ != stop_axis) {
                    subs_.p()[axis_] = 0;
                }

                for (std::size_t i = subs_.s(); i >= 1 && should_process_sub; --i) {
                    if (axis_ != i - 1 && (should_process_sub = (++subs_.p()[i - 1] == to_[i - 1])) && i != stop_axis + 1) {
                        subs_.p()[i - 1] = 0;
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
                if (order_.usable()) {
                    return subs_.p()[order_.data().p()[0]] != to_[order_.data().p()[0]];
                }

                const std::size_t stop_axis{ axis_ > 0 ? std::size_t{0} : (subs_.s() > 1 ? std::size_t{1} : std::size_t{0}) };
                return subs_.p()[stop_axis] != to_[stop_axis];
            }

            const Params<std::size_t>& subs() const noexcept
            {
                return subs_;
            }

        private:
            Internal_buffer buff_{};
            Params<std::size_t> subs_{};
            const std::size_t* from_{ nullptr };
            const std::size_t* to_{ nullptr };
            std::size_t axis_{ 0 };
            Internal_buffer order_{};
        };


        using ND_array_allocator = memoc::Malloc_allocator;

        using ND_array_buffer = memoc::Fallback_buffer<
            memoc::Stack_buffer<9 * sizeof(std::size_t)>,
            memoc::Allocated_buffer<ND_array_allocator, true>>;

        template <typename T, memoc::Buffer Internal_data_buffer = ND_array_buffer, memoc::Allocator Internal_allocator = ND_array_allocator, typename Interanl_header = ND_header<>, typename Internal_subscriptor = ND_subscriptor<>>
        class ND_array {
        public:
            using Header = Interanl_header;
            using Subscriptor = Internal_subscriptor;

            ND_array() = default;

            ND_array(ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>&& other) = default;
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array(ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>&& other)
            {
                copy(other, *this);

                ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o> dummy{ std::move(other) };
            }
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>&& other) & = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>&& other)&&
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
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>&& other)&
            {
                copy(other, *this);
                ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>&& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o> dummy{std::move(other)};
                return *this;
            }

            ND_array(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& other) = default;
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>& other)
            {
                copy(other, *this);
            }
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& other) & = default;
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& other)&&
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
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>& other)&
            {
                copy(other, *this);
                return *this;
            }
            template< typename T_o, memoc::Buffer Internal_data_buffer_o, memoc::Allocator Internal_allocator_o, typename Interanl_header_o, typename Internal_subscriptor_o>
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(const ND_array<T_o, Internal_data_buffer_o, Internal_allocator_o, Interanl_header_o, Internal_subscriptor_o>& other)&&
            {
                if (hdr_.is_partial() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                return *this;
            }

            template <typename U>
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }
                ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor ndstor{ Params<std::size_t>(hdr_.dims()) };
                while (ndstor) {
                    (*this)(ndstor.subs()) = value;
                    ++ndstor;
                }
                return *this;
            }

            virtual ~ND_array() = default;

            ND_array(const Params<std::size_t>& dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Internal_data_buffer>, Internal_allocator>(hdr_.count(), data))
            {
            }
            ND_array(std::initializer_list<std::size_t> dims, const T* data = nullptr)
                : ND_array(Params<std::size_t>{dims.size(), dims.begin()}, data)
            {
            }
            template <typename U>
            ND_array(const Params<std::size_t>& dims, const U* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Internal_data_buffer>, Internal_allocator>(hdr_.count()))
            {
                for (std::size_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = data[i];
                }
            }
            template <typename U>
            ND_array(std::initializer_list<std::size_t> dims, const U* data = nullptr)
                : ND_array(Params<std::size_t>{dims.size(), dims.begin()}, data)
            {
            }


            ND_array(const Params<std::size_t>& dims, const T& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Internal_data_buffer>, Internal_allocator>(hdr_.count()))
            {
                for (std::size_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            ND_array(std::initializer_list<std::size_t> dims, const T& value)
                : ND_array(Params<std::size_t>{dims.size(), dims.begin()}, value)
            {
            }
            template <typename U>
            ND_array(const Params<std::size_t>& dims, const U& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Typed_buffer<T, Internal_data_buffer>, Internal_allocator>(hdr_.count()))
            {
                for (std::size_t i = 0; i < buffsp_->data().s(); ++i) {
                    buffsp_->data().p()[i] = value;
                }
            }
            template <typename U>
            ND_array(std::initializer_list<std::size_t> dims, const U& value)
                : ND_array(Params<std::size_t>{dims.size(), dims.begin()}, value)
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

            const T& operator()(const Params<std::size_t>& subs) const
            {
                COMPUTOC_THROW_IF_FALSE(subs_in_dims(hdr_.dims(), subs), std::out_of_range, "out of range subscripts");
                return buffsp_->data().p()[subs2ind(hdr_.offset(), hdr_.strides(), subs)];
            }
            const T& operator()(std::initializer_list<std::size_t> subs) const
            {
                return (*this)(Params<std::size_t>{ subs.size(), subs.begin() });
            }

            T& operator()(const Params<std::size_t>& subs)
            {
                COMPUTOC_THROW_IF_FALSE(subs_in_dims(hdr_.dims(), subs), std::out_of_range, "out of range subscripts");
                return buffsp_->data().p()[subs2ind(hdr_.offset(), hdr_.strides(), subs)];
            }
            T& operator()(std::initializer_list<std::size_t> subs)
            {
                return (*this)(Params<std::size_t>{ subs.size(), subs.begin() });
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> operator()(const Params<ND_range>& ranges) const
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

                COMPUTOC_THROW_IF_FALSE(legal_ranges(ranges), std::invalid_argument, "illegal ranges");
                COMPUTOC_THROW_IF_FALSE(ranges_in_dims(hdr_.dims(), ranges), std::out_of_range, "ranges invalid for dimensions");

                ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> slice{};
                slice.hdr_ = Header{ hdr_.dims(), hdr_.strides(), hdr_.offset(), ranges };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> operator()(std::initializer_list<ND_range> ranges) const
            {
                return (*this)(Params<ND_range>{ranges.size(), ranges.begin()});
            }

        private:
            Header hdr_{};
            memoc::Shared_ptr<memoc::Typed_buffer<T, Internal_data_buffer>, Internal_allocator> buffsp_{ nullptr };
        };

        template <typename T, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>    
        inline auto transform(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, Func func)
            -> ND_array<decltype(func(arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>
        {
            if (empty(arr)) {
                return ND_array<decltype(func(arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            ND_array<decltype(func(arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().dims() };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor ndstor{ arr.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(arr(ndstor.subs()));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline auto reduce(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, Func func)
            -> decltype(func(arr.data()[0], arr.data()[0]))
        {
            if (empty(arr)) {
                return decltype(func(arr.data()[0], arr.data()[0])){};
            }

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor ndstor{ arr.header().dims() };

            decltype(func(arr.data()[0], arr.data()[0])) res{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(ndstor.subs())) };
            ++ndstor;

            while (ndstor) {
                res = func(arr(ndstor.subs()), res);
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline auto reduce(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, Func func, std::size_t axis)
            -> ND_array<decltype(func(arr.data()[0], arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>
        {
            if (empty(arr)) {
                return ND_array<decltype(func(arr.data()[0], arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            COMPUTOC_THROW_IF_FALSE(axis < arr.header().dims().s(), std::invalid_argument, "axis is invalid for number of dimensions");

            ND_array<decltype(func(arr.data()[0], arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ {arr.header().count() / arr.header().dims().p()[axis]} };

            res.header() = typename ND_array<decltype(func(arr.data()[0], arr.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Header{ arr.header().dims(), axis };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims(), axis };
            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            const std::size_t reduction_iteration_cycle{ arr.header().dims().p()[axis] };

            while (arr_ndstor && res_ndstor) {
                decltype(func(arr.data()[0], arr.data()[0])) res_element{ static_cast<decltype(func(arr.data()[0], arr.data()[0]))>(arr(arr_ndstor.subs())) };
                ++arr_ndstor;
                for (std::size_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++arr_ndstor) {
                    res_element = func(arr(arr_ndstor.subs()), res_element);
                }
                res(res_ndstor.subs()) = res_element;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T1, typename T2, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline auto binary(const ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& lhs, const ND_array<T2, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& rhs, Func func)
            -> ND_array<decltype(func(lhs.data()[0], rhs.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>
        {
            COMPUTOC_THROW_IF_FALSE(lhs.header().dims() == rhs.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<decltype(func(lhs.data()[0], rhs.data()[0])), Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ lhs.header().dims() };

            typename ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor ndstor{ lhs.header().dims() };

            while (ndstor) {
                res(ndstor.subs()) = func(lhs(ndstor.subs()), rhs(ndstor.subs()));
                ++ndstor;
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> filter(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, Func func)
        {
            if (empty(arr)) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().count() };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            std::size_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(arr_ndstor.subs()))) {
                    res(res_ndstor.subs()) = arr(arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> filter(const ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, const ND_array<T2, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& mask)
        {
            if (empty(arr)) {
                return ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().count() };

            typename ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T2, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor mask_ndstor{ mask.header().dims() };

            typename ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            std::size_t res_count{ 0 };

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
                return ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Func, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> find(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, Func func)
        {
            if (empty(arr)) {
                return ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().count() };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            std::size_t res_count{ 0 };

            while (arr_ndstor && res_ndstor) {
                if (func(arr(arr_ndstor.subs()))) {
                    res(res_ndstor.subs()) = subs2ind(arr.header().offset(), arr.header().strides(), arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
            }

            if (res_count == 0) {
                return ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <
            typename T1, typename T2, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> find(const ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, const ND_array<T2, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& mask)
        {
            if (empty(arr)) {
                return ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().dims() == mask.header().dims(), std::invalid_argument, "different input array dimensions");

            ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().count() };

            typename ND_array<T1, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims() };
            typename ND_array<T2, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor mask_ndstor{ mask.header().dims() };

            typename ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            std::size_t res_count{ 0 };

            while (arr_ndstor && mask_ndstor && res_ndstor) {
                if (mask(mask_ndstor.subs())) {
                    res(res_ndstor.subs()) = subs2ind(arr.header().offset(), arr.header().strides(), arr_ndstor.subs());
                    ++res_count;
                    ++res_ndstor;
                }
                ++arr_ndstor;
                ++mask_ndstor;
            }

            if (res_count == 0) {
                return ND_array<std::size_t, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> transpose(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, const Params<std::size_t>& order)
        {
            if (empty(arr)) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr.header().count() };
            res.header() = typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Header{ arr.header().dims(), order };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor arr_ndstor{ arr.header().dims(), order, {0, nullptr} };
            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor res_ndstor{ res.header().dims() };

            while (arr_ndstor && res_ndstor) {
                res(res_ndstor.subs()) = arr(arr_ndstor.subs());
                ++arr_ndstor;
                ++res_ndstor;
            }

            return res;
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> transpose(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, std::initializer_list<std::size_t> order)
        {
            return transpose(arr, Params<std::size_t>{order.size(), order.begin()});
        }

        template <
            typename T1, memoc::Buffer Internal_data_buffer1, memoc::Allocator Internal_allocator1, typename Interanl_header1, typename Internal_subscriptor1,
            typename T2, memoc::Buffer Internal_data_buffer2, memoc::Allocator Internal_allocator2, typename Interanl_header2, typename Internal_subscriptor2>
        inline bool operator==(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Interanl_header1, Internal_subscriptor1>& lhs, const ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>& rhs)
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

            typename ND_array<T1, Internal_data_buffer1, Internal_allocator1, Interanl_header1, Internal_subscriptor1>::Subscriptor lhs_ndstor{ lhs.header().dims() };
            typename ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>::Subscriptor rhs_ndstor{ rhs.header().dims() };

            while (lhs_ndstor && rhs_ndstor) {
                if (!equal(lhs(lhs_ndstor.subs()), rhs(rhs_ndstor.subs()))) {
                    return false;
                }
                ++lhs_ndstor;
                ++rhs_ndstor;
            }

            return true;
        }

        template <
            typename T1, memoc::Buffer Internal_data_buffer1, memoc::Allocator Internal_allocator1, typename Interanl_header1, typename Internal_subscriptor1,
            typename T2, memoc::Buffer Internal_data_buffer2, memoc::Allocator Internal_allocator2, typename Interanl_header2, typename Internal_subscriptor2>
        inline void copy(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Interanl_header1, Internal_subscriptor1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>& dst)
        {
            /*
            * Algorithm:
            * - empty array     -> empty array
            * - not equal count -> create new buffer
            * - copy elements
            */

            if (empty(src)) {
                dst = ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>{};
                return;
            }

            if (src.header().count() != dst.header().count()) {
                dst = ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>(src.header().dims());
            }

            typename ND_array<T1, Internal_data_buffer1, Internal_allocator1, Interanl_header1, Internal_subscriptor1>::Subscriptor src_ndstor{ src.header().dims() };
            typename ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>::Subscriptor dst_ndstor{ dst.header().dims() };

            while (src_ndstor && dst_ndstor) {
                dst(dst_ndstor.subs()) = src(src_ndstor.subs());
                ++src_ndstor;
                ++dst_ndstor;
            }
        }
        template <
            typename T1, memoc::Buffer Internal_data_buffer1, memoc::Allocator Internal_allocator1, typename Interanl_header1, typename Internal_subscriptor1,
            typename T2, memoc::Buffer Internal_data_buffer2, memoc::Allocator Internal_allocator2, typename Interanl_header2, typename Internal_subscriptor2>
        inline void copy(const ND_array<T1, Internal_data_buffer1, Internal_allocator1, Interanl_header1, Internal_subscriptor1>& src, ND_array<T2, Internal_data_buffer2, Internal_allocator2, Interanl_header2, Internal_subscriptor2>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> clone(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr)
        {
            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> clone{};

            if (empty(arr)) {
                return clone;
            }

            clone = ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{ arr.header().dims() };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor ndstor{ arr.header().dims() };

            while (ndstor) {
                clone(ndstor.subs()) = arr(ndstor.subs());
                ++ndstor;
            }
            return clone;
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> reshape(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, const Params<std::size_t>& new_dims)
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
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            COMPUTOC_THROW_IF_FALSE(arr.header().count() == dims2count(new_dims), std::invalid_argument, "different number of elements between original and rehsaped arrays");

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().is_partial()) {
                ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ new_dims };

                typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor prev_ndstor(arr.header().dims());
                typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor new_ndstor(new_dims);

                while (prev_ndstor && new_ndstor) {
                    res(new_ndstor.subs()) = arr(prev_ndstor.subs());
                    ++prev_ndstor;
                    ++new_ndstor;
                }

                return res;
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ arr };
            res.header() = typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Header(new_dims);

            return res;
        }
        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> reshape(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, std::initializer_list<std::size_t> new_dims)
        {
            return reshape(arr, Params<std::size_t>(new_dims.size(), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> resize(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, const Params<std::size_t>& new_dims)
        {
            /*
            * Resizing algorithm:
            * - return new array of the new size containing the original array data or part of it.
            */
            if (new_dims.empty()) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>{};
            }

            if (empty(arr)) {
                return ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>(new_dims);
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> res{ new_dims };

            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor prev_ndstor(arr.header().dims());
            typename ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>::Subscriptor new_ndstor(new_dims);

            while (prev_ndstor && new_ndstor) {
                res(new_ndstor.subs()) = arr(prev_ndstor.subs());
                ++prev_ndstor;
                ++new_ndstor;
            }

            return res;
        }
        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor> resize(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr, std::initializer_list<std::size_t> new_dims)
        {
            return resize(arr, Params<std::size_t>(new_dims.size(), new_dims.begin()));
        }

        template <typename T, memoc::Buffer Internal_data_buffer, memoc::Allocator Internal_allocator, typename Interanl_header, typename Internal_subscriptor>
        inline bool empty(const ND_array<T, Internal_data_buffer, Internal_allocator, Interanl_header, Internal_subscriptor>& arr) noexcept
        {
            return !arr.data() || (arr.header().count() == 0);
        }
    }

    using details::ND_range;
    using details::Params;
    using details::ND_array;
    using details::transform;
    using details::binary;
    using details::reduce;
    using details::filter;
    using details::find;
    using details::transpose;
    using details::copy;
    using details::clone;
    using details::reshape;
    using details::resize;
    using details::empty;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
