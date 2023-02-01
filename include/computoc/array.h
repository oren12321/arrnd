#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstdint>
#include <initializer_list>
#include <stdexcept>


#include <erroc/errors.h>
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
        [[nodiscard]] inline std::int64_t numel(const Params<std::int64_t>& dims) noexcept
        {
            if (empty(dims)) {
                return 0;
            }

            std::int64_t res{ 1 };
            for (std::int64_t i = 0; i < size(dims); ++i) {
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
            std::int64_t num_strides{ size(dims) > size(strides) ? size(strides) : size(dims) };
            if (num_strides <= 0) {
                return 0;
            }

            strides[num_strides - 1] = 1;
            for (std::int64_t i = num_strides - 2; i >= 0; --i) {
                strides[i] = strides[i + 1] * dims[i + 1];
            }
            return num_strides;
        }

        /**
        * @param[out] strides An already allocated memory for computed strides.
        * @return Number of computed strides
        * @note When number of interval is smaller than number of strides, the other strides computed from previous dimensions.
        */
        inline std::int64_t compute_strides(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& previous_strides, const Params<Interval<std::int64_t>>& intervals, Params<std::int64_t> strides) noexcept
        {
            std::int64_t nstrides{ size(previous_strides) > size(strides) ? size(strides) : size(previous_strides) };
            if (nstrides <= 0) {
                return 0;
            }

            std::int64_t ncomp_from_intervals{ nstrides > size(intervals) ? size(intervals) : nstrides };

            // compute strides with interval step
            for (std::int64_t i = 0; i < ncomp_from_intervals; ++i) {
                strides[i] = previous_strides[i] * forward(intervals[i]).step;
            }

            // compute strides from previous dimensions
            if (size(intervals) < size(previous_dims) && nstrides >= size(previous_dims)) {
                strides[size(previous_dims) - 1] = 1;
                for (std::int64_t i = size(previous_dims) - 2; i >= size(intervals); --i) {
                    strides[i] = strides[i + 1] * previous_dims[i + 1];
                }
            }

            return nstrides;
        }

        /**
        * @param[out] dims An already allocated memory for computed dimensions.
        * @return Number of computed dimensions
        * @note Previous dimensions are used in case of small number of intervals.
        */
        inline std::int64_t compute_dims(const Params<std::int64_t>& previous_dims, const Params<Interval<std::int64_t>>& intervals, Params<std::int64_t> dims) noexcept
        {
            std::int64_t ndims{ size(previous_dims) > size(dims) ? size(dims) : size(previous_dims) };
            if (ndims <= 0) {
                return 0;
            }

            std::int64_t num_computed_dims{ ndims > size(intervals) ? size(intervals) : ndims };

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

            if (empty(previous_dims) || empty(previous_strides) || empty(intervals)) {
                return offset;
            }

            std::int64_t num_computations{ size(previous_dims) > size(previous_strides) ? size(previous_strides) : size(previous_dims) };
            num_computations = (num_computations > size(intervals) ? size(intervals) : num_computations);

            for (std::int64_t i = 0; i < num_computations; ++i) {
                offset += previous_strides[i] * forward(modulo(intervals[i], previous_dims[i])).start;
            }
            return offset;
        }

        /**
        * @note Extra subscripts are ignored. If number of subscripts are less than number of strides/dimensions, they are considered as the less significant subscripts.
        */
        [[nodiscard]] inline std::int64_t subs2ind(std::int64_t offset, const Params<std::int64_t>& strides, const Params<std::int64_t>& dims, const Params<std::int64_t>& subs) noexcept
        {
            std::int64_t ind{ offset };

            if (empty(strides) || empty(dims) || empty(subs)) {
                return ind;
            }

            std::int64_t num_used_subs{ size(strides) > size(dims) ? size(dims) : size(strides) };
            num_used_subs = (num_used_subs > size(subs) ? size(subs) : num_used_subs);

            std::int64_t num_ignored_subs{size(strides) - num_used_subs};
            if (num_ignored_subs < 0) { // ignore extra subscripts
                num_ignored_subs = 0;
            }

            for (std::int64_t i = num_ignored_subs; i < size(strides); ++i) {
                ind += strides[i] * modulo(subs[i - num_ignored_subs], dims[i]);
            }

            return ind;
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

        using Array_default_internals_allocator = memoc::Fallback_allocator<
            memoc::Stack_allocator<>,
            memoc::Malloc_allocator>;

        template <memoc::Allocator Internal_allocator = Array_default_internals_allocator>
        class Array_header {
        public:
            Array_header() = default;

            Array_header(const Params<std::int64_t>& dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                buff_ = memoc::create_buffer<std::int64_t, Internal_allocator>(size(dims) * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff_), std::runtime_error, "buffer allocation failed");

                dims_ = { size(dims), memoc::data(buff_) };
                copy(dims, dims_);

                strides_ = { size(dims), memoc::data(buff_) + size(dims) };
                compute_strides(dims, strides_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& previous_strides, std::int64_t previous_offset, const Params<Interval<std::int64_t>>& intervals)
                : is_subarray_(true)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                memoc::Buffer<std::int64_t, Internal_allocator> buff = memoc::create_buffer<std::int64_t, Internal_allocator>(size(previous_dims) * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff), std::runtime_error, "buffer allocation failed");

                Params<std::int64_t> dims{ size(previous_dims), memoc::data(buff) };
                if (compute_dims(previous_dims, intervals, dims) <= 0) {
                    return;
                }

                buff_ = std::move(buff);
                
                dims_ = { size(previous_dims), memoc::data(buff_) };

                count_ = numel(dims_);

                strides_ = { size(previous_dims), memoc::data(buff_) + size(previous_dims) };
                compute_strides(previous_dims, previous_strides, intervals, strides_);

                offset_ = compute_offset(previous_dims, previous_offset, previous_strides, intervals);
            }

            Array_header(const Params<std::int64_t>& previous_dims, std::int64_t omitted_axis)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                std::int64_t axis{ modulo(omitted_axis, size(previous_dims)) };
                std::int64_t ndims{ size(previous_dims) > 1 ? size(previous_dims) - 1 : 1 };

                buff_ = memoc::create_buffer<std::int64_t, Internal_allocator>(ndims * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff_), std::runtime_error, "buffer allocation failed");

                dims_ = { ndims, memoc::data(buff_) };
                if (size(previous_dims) > 1) {
                    for (std::int64_t i = 0; i < axis; ++i) {
                        dims_[i] = previous_dims[i];
                    }
                    for (std::int64_t i = axis + 1; i < size(previous_dims); ++i) {
                        dims_[i - 1] = previous_dims[i];
                    }
                }
                else {
                    dims_[0] = 1;
                }

                strides_ = { ndims, memoc::data(buff_) + ndims };
                compute_strides(dims_, strides_);

                count_ = numel(dims_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& new_order)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                if (memoc::empty(new_order)) {
                    return;
                }

                memoc::Buffer<std::int64_t, Internal_allocator> buff = memoc::create_buffer<std::int64_t, Internal_allocator>(size(previous_dims) * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff), std::runtime_error, "buffer allocation failed");

                Params<std::int64_t> dims{ size(previous_dims), memoc::data(buff) };
                for (std::int64_t i = 0; i < size(previous_dims); ++i) {
                    dims[i] = previous_dims[modulo(new_order[i], previous_dims[i])];
                }

                if (numel(previous_dims) != numel(dims)) {
                    return;
                }

                buff_ = std::move(buff);

                dims_ = { size(previous_dims), memoc::data(buff_) };

                strides_ = { size(previous_dims), memoc::data(buff_) + size(previous_dims) };
                compute_strides(dims_, strides_);

                count_ = numel(dims_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, std::int64_t count, std::int64_t axis)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                memoc::Buffer<std::int64_t, Internal_allocator> buff = memoc::create_buffer<std::int64_t, Internal_allocator>(size(previous_dims) * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff), std::runtime_error, "buffer allocation failed");

                Params<std::int64_t> dims{ size(previous_dims), memoc::data(buff) };
                std::int64_t fixed_axis{ modulo(axis, size(previous_dims)) };
                for (std::int64_t i = 0; i < size(previous_dims); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_dims[i] : previous_dims[i] + count;
                }
                
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                buff_ = std::move(buff);

                dims_ = { size(previous_dims), memoc::data(buff_) };

                strides_ = { size(previous_dims), memoc::data(buff_) + size(previous_dims) };
                compute_strides(dims_, strides_);
            }

            Array_header(const Params<std::int64_t>& previous_dims, const Params<std::int64_t>& appended_dims, std::int64_t axis)
            {
                if (size(previous_dims) != size(appended_dims)) {
                    return;
                }

                if (numel(previous_dims) <= 0) {
                    return;
                }

                if (numel(appended_dims) <= 0) {
                    return;
                }

                std::int64_t fixed_axis{ modulo(axis, size(previous_dims)) };

                bool are_dims_valid_for_append{ true };
                for (std::int64_t i = 0; i < size(previous_dims); ++i) {
                    if (i != fixed_axis && previous_dims[i] != appended_dims[i]) {
                        are_dims_valid_for_append = false;
                    }
                }
                if (!are_dims_valid_for_append) {
                    return;
                }

                memoc::Buffer<std::int64_t, Internal_allocator> buff = memoc::create_buffer<std::int64_t, Internal_allocator>(size(previous_dims) * 2).value();
                //ERROC_EXPECT(!memoc::empty(buff), std::runtime_error, "buffer allocation failed");

                Params<std::int64_t> dims{ size(previous_dims), memoc::data(buff) };
                for (std::int64_t i = 0; i < size(previous_dims); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_dims[i] : previous_dims[i] + appended_dims[fixed_axis];
                }

                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                buff_ = std::move(buff);

                dims_ = { size(previous_dims), memoc::data(buff_) };

                strides_ = { size(previous_dims), memoc::data(buff_) + size(previous_dims) };
                compute_strides(dims_, strides_);
            }

            Array_header(Array_header&& other) noexcept
                : buff_(std::move(other.buff_)), count_(other.count_), offset_(other.offset_), is_subarray_(other.is_subarray_)
            {
                dims_ = { size(other.dims_), memoc::data(buff_) };
                strides_ = { size(other.strides_), memoc::data(buff_) + size(other.dims_) };

                other.dims_ = {};
                other.strides_ = {};
                other.count_ = other.offset_ = 0;
                other.is_subarray_ = false;
            }
            Array_header& operator=(Array_header&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = std::move(other.buff_);
                count_ = other.count_;
                offset_ = other.offset_;
                is_subarray_ = other.is_subarray_;

                dims_ = { size(other.dims_), memoc::data(buff_) };
                strides_ = { size(other.strides_), memoc::data(buff_) + size(other.dims_) };

                other.dims_ = {};
                other.strides_ = {};
                other.count_ = other.offset_ = 0;
                other.is_subarray_ = false;

                return *this;
            }

            Array_header(const Array_header& other) noexcept
                : buff_(other.buff_), count_(other.count_), offset_(other.offset_), is_subarray_(other.is_subarray_)
            {
                dims_ = { size(other.dims_), memoc::data(buff_) };
                strides_ = { size(other.strides_), memoc::data(buff_) + size(other.dims_) };
            }
            Array_header& operator=(const Array_header& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                count_ = other.count_;
                offset_ = other.offset_;
                is_subarray_ = other.is_subarray_;

                dims_ = { size(other.dims_), memoc::data(buff_) };
                strides_ = { size(other.strides_), memoc::data(buff_) + size(other.dims_) };

                return *this;
            }

            virtual ~Array_header() = default;

            [[nodiscard]] std::int64_t count() const noexcept
            {
                return count_;
            }

            [[nodiscard]] const Params<std::int64_t>& dims() const noexcept
            {
                return dims_;
            }

            [[nodiscard]] const Params<std::int64_t>& strides() const noexcept
            {
                return strides_;
            }

            [[nodiscard]] std::int64_t offset() const noexcept
            {
                return offset_;
            }

            [[nodiscard]] bool is_subarray() const noexcept
            {
                return is_subarray_;
            }

            [[nodiscard]] bool empty() const noexcept
            {
                return memoc::empty(buff_);
            }

        private:
            Params<std::int64_t> dims_{};
            Params<std::int64_t> strides_{};
            memoc::Buffer<std::int64_t, Internal_allocator> buff_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            bool is_subarray_{ false };
        };


        template <memoc::Allocator Internal_allocator = Array_default_internals_allocator>
        class Array_subscripts_iterator
        {
        public:
            Array_subscripts_iterator(const Params<std::int64_t>& start, const Params<std::int64_t>& minimum_excluded, const Params<std::int64_t>& maximum_excluded, std::int64_t axis)
            {
                std::int64_t bounds_size{ minimum_excluded.size() > maximum_excluded.size() ? minimum_excluded.size() : maximum_excluded.size() };
                nsubs_ = start.size() > bounds_size ? start.size() : bounds_size;

                if (nsubs_ > 0) {
                    buff_ = memoc::create_buffer<std::int64_t, Internal_allocator>(nsubs_ * 4).value();
                    //ERROC_EXPECT(!memoc::empty(buff_), std::runtime_error, "buffer allocation failed");

                    axis_ = modulo(axis, nsubs_);

                    bsubs_ = { nsubs_, buff_.data() };
                    subs_ = bsubs_.data();
                    start_ = subs_ + nsubs_;
                    if (start.empty()) {
                        memoc::set(bsubs_, std::int64_t{ 0 }, nsubs_);
                        memoc::set(Params<std::int64_t>(nsubs_, start_), std::int64_t{ 0 }, nsubs_);
                    }
                    else {
                        memoc::copy(start, bsubs_, nsubs_);
                        memoc::copy(start, Params<std::int64_t>(nsubs_, start_), nsubs_);
                    }

                    minimum_excluded_ = start_ + nsubs_;
                    if (!minimum_excluded.empty()) {
                        memoc::copy(minimum_excluded, Params<std::int64_t>(nsubs_, minimum_excluded_), nsubs_);
                    }
                    else if(!empty(start)) {
                        memoc::copy(start, Params<std::int64_t>(nsubs_, minimum_excluded_), nsubs_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        memoc::set(Params<std::int64_t>(nsubs_, minimum_excluded_), std::int64_t{ -1 }, nsubs_);
                    }

                    maximum_excluded_ = minimum_excluded_ + nsubs_;
                    if (maximum_excluded.empty()) {
                        memoc::set(Params<std::int64_t>(nsubs_, maximum_excluded_), std::int64_t{ 1 }, nsubs_);
                    }
                    else {
                        memoc::copy(maximum_excluded, Params<std::int64_t>(nsubs_, maximum_excluded_), nsubs_);
                    }

                    major_axis_ = find_major_axis();
                    min_at_major_ = minimum_excluded_[major_axis_];
                    max_at_major_ = maximum_excluded_[major_axis_];
                }
            }

            Array_subscripts_iterator(const Params<std::int64_t>& start, const Params<std::int64_t>& minimum_excluded, const Params<std::int64_t>& maximum_excluded, const Params<std::int64_t>& order)
            {
                std::int64_t bounds_size{ minimum_excluded.size() > maximum_excluded.size() ? minimum_excluded.size() : maximum_excluded.size() };
                nsubs_ = start.size() > bounds_size ? start.size() : bounds_size;

                if (nsubs_ > 0) {
                    if (size(order) >= nsubs_) {
                        buff_ = memoc::create_buffer<std::int64_t, Internal_allocator>(nsubs_ * 5).value();
                    }
                    else {
                        buff_ = memoc::create_buffer<std::int64_t, Internal_allocator>(nsubs_ * 4).value();
                        axis_ = nsubs_ - 1;
                    }
                    //ERROC_EXPECT(!memoc::empty(buff_), std::runtime_error, "buffer allocation failed");

                    bsubs_ = { nsubs_, buff_.data() };
                    subs_ = bsubs_.data();
                    start_ = subs_ + nsubs_;
                    if (start.empty()) {
                        memoc::set(bsubs_, std::int64_t{ 0 }, nsubs_);
                        memoc::set(Params<std::int64_t>(nsubs_, start_), std::int64_t{ 0 }, nsubs_);
                    }
                    else {
                        memoc::copy(start, bsubs_, nsubs_);
                        memoc::copy(start, Params<std::int64_t>(nsubs_, start_), nsubs_);
                    }

                    minimum_excluded_ = start_ + nsubs_;
                    if (!minimum_excluded.empty()) {
                        memoc::copy(minimum_excluded, Params<std::int64_t>(nsubs_, minimum_excluded_), nsubs_);
                    }
                    else if (!empty(start)) {
                        memoc::copy(start, Params<std::int64_t>(nsubs_, minimum_excluded_), nsubs_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        memoc::set(Params<std::int64_t>(nsubs_, minimum_excluded_), std::int64_t{ -1 }, nsubs_);
                    }

                    maximum_excluded_ = minimum_excluded_ + nsubs_;
                    if (maximum_excluded.empty()) {
                        memoc::set(Params<std::int64_t>(nsubs_, maximum_excluded_), std::int64_t{ 1 }, nsubs_);
                    }
                    else {
                        memoc::copy(maximum_excluded, Params<std::int64_t>(nsubs_, maximum_excluded_), nsubs_);
                    }

                    major_axis_ = find_major_axis();

                    if (order.size() >= nsubs_) {
                        order_ = maximum_excluded_ + nsubs_;
                        memoc::copy(order, Params<std::int64_t>(nsubs_, order_), nsubs_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            order_[i] = modulo(order_[i], nsubs_);
                        }
                        min_at_major_ = minimum_excluded_[order[0]];
                        max_at_major_ = maximum_excluded_[order[0]];
                    }
                    else {
                        min_at_major_ = minimum_excluded_[major_axis_];
                        max_at_major_ = maximum_excluded_[major_axis_];
                    }
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

            Array_subscripts_iterator(const Array_subscripts_iterator<Internal_allocator>& other) noexcept
                : buff_(other.buff_), nsubs_(other.nsubs_), axis_(other.axis_), major_axis_(other.major_axis_), min_at_major_(other.min_at_major_), max_at_major_(other.max_at_major_)
            {
                bsubs_ = { nsubs_, memoc::data(buff_) };
                subs_ = bsubs_.data();
                start_ = subs_ + nsubs_;
                minimum_excluded_ = start_ + nsubs_;
                maximum_excluded_ = minimum_excluded_ + nsubs_;
                if (other.order_) {
                    order_ = maximum_excluded_ + nsubs_;
                }
            }
            Array_subscripts_iterator<Internal_allocator>& operator=(const Array_subscripts_iterator<Internal_allocator>& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = other.buff_;
                nsubs_ = other.nsubs_;
                axis_ = other.axis_;
                bsubs_ = { nsubs_, memoc::data(buff_) };
                subs_ = bsubs_.data();
                start_ = subs_ + nsubs_;
                minimum_excluded_ = start_ + nsubs_;
                maximum_excluded_ = minimum_excluded_ + nsubs_;
                if (other.order_) {
                    order_ = maximum_excluded_ + nsubs_;
                }
                major_axis_ = other.major_axis_;
                min_at_major_ = other.min_at_major_;
                max_at_major_ = other.max_at_major_;

                return *this;
            }

            Array_subscripts_iterator(Array_subscripts_iterator<Internal_allocator>&& other) noexcept
                : buff_(std::move(other.buff_)), nsubs_(other.nsubs_), axis_(other.axis_), major_axis_(other.major_axis_), min_at_major_(other.min_at_major_), max_at_major_(other.max_at_major_)
            {
                bsubs_ = { nsubs_, memoc::data(buff_) };
                subs_ = bsubs_.data();
                start_ = subs_ + nsubs_;
                minimum_excluded_ = start_ + nsubs_;
                maximum_excluded_ = minimum_excluded_ + nsubs_;
                if (other.order_) {
                    order_ = maximum_excluded_ + nsubs_;
                }

                other.nsubs_ = 0;
                other.axis_ = 0;
                other.bsubs_ = {};
                other.subs_ = nullptr;
                other.start_ = nullptr;
                other.minimum_excluded_ = nullptr;
                other.maximum_excluded_ = nullptr;
                other.order_ = nullptr;
                other.major_axis_ = 0;
                other.min_at_major_ = 0;
                other.max_at_major_ = 0;
            }
            Array_subscripts_iterator<Internal_allocator>& operator=(Array_subscripts_iterator<Internal_allocator>&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                buff_ = std::move(other.buff_);
                nsubs_ = other.nsubs_;
                axis_ = other.axis_;
                bsubs_ = { nsubs_, memoc::data(buff_) };
                subs_ = bsubs_.data();
                start_ = subs_ + nsubs_;
                minimum_excluded_ = start_ + nsubs_;
                maximum_excluded_ = minimum_excluded_ + nsubs_;
                if (other.order_) {
                    order_ = maximum_excluded_ + nsubs_;
                }
                major_axis_ = other.major_axis_;
                min_at_major_ = other.min_at_major_;
                max_at_major_ = other.max_at_major_;

                other.nsubs_ = 0;
                other.axis_ = 0;
                other.bsubs_ = {};
                other.subs_ = nullptr;
                other.start_ = nullptr;
                other.minimum_excluded_ = nullptr;
                other.maximum_excluded_ = nullptr;
                other.order_ = nullptr;
                other.major_axis_ = 0;
                other.min_at_major_ = 0;
                other.max_at_major_ = 0;

                return *this;
            }

            virtual ~Array_subscripts_iterator() = default;

            void reset() noexcept
            {
                memoc::copy(Params<std::int64_t>(nsubs_, start_), bsubs_, nsubs_);
            }

            Array_subscripts_iterator<Internal_allocator>& operator++() noexcept
            {
#define _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__INCREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(ind, major) \
    ++subs_[ind]; \
    if (ind == major || subs_[ind] < maximum_excluded_[ind]) { \
        return *this; \
    } \
    subs_[ind] = minimum_excluded_[ind] + 1;

                if (order_) {
                    std::int64_t major_ordered{ order_[0] };
                    for (int64_t i = nsubs_ - 1; i >= 0; --i) {
                        std::int64_t ordered_i{ order_[i] };
                        _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__INCREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(ordered_i, major_ordered);
                    }
                    return *this;
                }

                _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__INCREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(axis_, major_axis_);
                for (std::int64_t i = nsubs_ - 1; i > axis_; --i) {
                    _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__INCREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(i, major_axis_);
                }
                for (std::int64_t i = axis_ - 1; i >= 0; --i) {
                    _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__INCREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(i, major_axis_);
                }

                return *this;
            }

            Array_subscripts_iterator<Internal_allocator> operator++(int) noexcept
            {
                Array_subscripts_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_subscripts_iterator<Internal_allocator>& operator+=(std::int64_t value) noexcept
            {
                for (std::int64_t i = 0; i < value; ++i) {
                    ++(*this);
                }
                return *this;
            }

            [[nodiscard]] Array_subscripts_iterator<Internal_allocator> operator+(std::int64_t value) const noexcept
            {
                Array_subscripts_iterator temp{ *this };
                temp += value;
                return temp;
            }

            Array_subscripts_iterator<Internal_allocator>& operator--() noexcept
            {
#define _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__DECREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(ind, major) \
    --subs_[ind]; \
    if (ind == major || subs_[ind] > minimum_excluded_[ind]) { \
        return *this; \
    } \
    subs_[ind] = maximum_excluded_[ind] == 0 ? 0 : maximum_excluded_[ind] - 1;

                if (order_) {
                    std::int64_t major_ordered{ order_[0] };
                    for (int64_t i = nsubs_ - 1; i >= 0; --i) {
                        std::int64_t ordered_i{ order_[i] };
                        _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__DECREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(ordered_i, major_ordered);
                    }
                    return *this;
                }

                _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__DECREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(axis_, major_axis_);
                subs_[axis_] = maximum_excluded_[axis_] == 0 ? 0 : maximum_excluded_[axis_] - 1;
                for (std::int64_t i = nsubs_ - 1; i > axis_; --i) {
                    _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__DECREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(i, major_axis_);
                }
                for (std::int64_t i = axis_ - 1; i >= 0; --i) {
                    _COMPUTOC_ARRAY_SUBSCRIPTS_ITERATOR__DECREMENT_SUBSCRIPT_AND_RETURN_IF_REQUIRED(i, major_axis_);
                }

                return *this;
            }

            Array_subscripts_iterator<Internal_allocator> operator--(int) noexcept
            {
                Array_subscripts_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_subscripts_iterator<Internal_allocator>& operator-=(std::int64_t value) noexcept
            {
                for (std::int64_t i = 0; i < value; ++i) {
                    --(*this);
                }
                return *this;
            }

            [[nodiscard]] Array_subscripts_iterator<Internal_allocator> operator-(std::int64_t value) const noexcept
            {
                Array_subscripts_iterator temp{ *this };
                temp -= value;
                return temp;
            }

            [[nodiscard]] explicit operator bool() const noexcept
            {
                if (order_) {
                    return (subs_[order_[0]] < max_at_major_) && (subs_[order_[0]] > min_at_major_);
                }

                return (subs_[major_axis_] < max_at_major_) && (subs_[major_axis_] > min_at_major_);
            }

            [[nodiscard]] const Params<std::int64_t>& operator*() const noexcept
            {
                return bsubs_;
            }

        private:
            [[nodiscard]] std::int64_t find_major_axis() const noexcept
            {
                std::int64_t major_axis{ axis_ > 0 ? std::int64_t{0} : (nsubs_ > 1) };
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

            memoc::Buffer<std::int64_t, Internal_allocator> buff_{};

            std::int64_t nsubs_{ 0 };

            Params<std::int64_t> bsubs_{};
            std::int64_t* subs_{ nullptr };
            std::int64_t* start_{ nullptr };
            std::int64_t* minimum_excluded_{ nullptr };
            std::int64_t* maximum_excluded_{ nullptr };

            std::int64_t axis_{ 0 };
            std::int64_t* order_{ nullptr };

            std::int64_t major_axis_{ 0 };
            std::int64_t min_at_major_{ 0 };
            std::int64_t max_at_major_{ 0 };
        };

        using Array_default_data_reference_allocator = memoc::Malloc_allocator;

        using Array_default_data_allocator = memoc::Fallback_allocator<
            memoc::Stack_allocator<>,
            memoc::Malloc_allocator>;

        template <typename T, memoc::Allocator Data_allocator = Array_default_data_allocator, memoc::Allocator Data_reference_allocator = Array_default_data_reference_allocator, memoc::Allocator Internals_allocator = Array_default_internals_allocator>
        class Array {
        public:
            using Header = Array_header<Internals_allocator>;
            using Subscripts_iterator = Array_subscripts_iterator<Internals_allocator>;

            Array() = default;

            Array(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& other) = default;
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array(Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>&& other)
                : Array(other.header().dims())
            {
                copy(other, *this);

                Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
            }
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& other) & = default;
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                if (hdr_.is_subarray() && hdr_.dims() == other.hdr_.dims()) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = std::move(other.hdr_);
                buffsp_ = std::move(other.buffsp_);

                return *this;
            }
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>&& other)&
            {
                *this = Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>(other.header().dims());
                copy(other, *this);
                Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>&& other)&&
            {
                if (hdr_.is_subarray() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o> dummy{std::move(other)};
                return *this;
            }

            Array(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& other) = default;
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array(const Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>& other)
                : Array(other.header().dims())
            {
                copy(other, *this);
            }
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& other) & = default;
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                if (hdr_.is_subarray() && hdr_.dims() == other.hdr_.dims()) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = other.hdr_;
                buffsp_ = other.buffsp_;

                return *this;
            }
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(const Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>& other)&
            {
                *this = Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>(other.header().dims());
                copy(other, *this);
                return *this;
            }
            template< typename T_o, memoc::Allocator Data_allocator_o, memoc::Allocator Data_reference_allocator_o, memoc::Allocator Internals_allocator_o>
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(const Array<T_o, Data_allocator_o, Data_reference_allocator_o, Internals_allocator_o>& other)&&
            {
                if (hdr_.is_subarray() && hdr_.dims() == other.header().dims()) {
                    copy(other, *this);
                }
                return *this;
            }

            template <typename U>
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }

                for (Subscripts_iterator iter({}, hdr_.dims()); iter; ++iter) {
                    (*this)(*iter) = value;
                }

                return *this;
            }

            virtual ~Array() = default;

            Array(const Params<std::int64_t>& dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Buffer<T, Data_reference_allocator>, Data_reference_allocator>(hdr_.count(), data))
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
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Buffer<T, Data_reference_allocator>, Data_reference_allocator>(hdr_.count()))
            {
                memoc::copy(Params<U>{ hdr_.count(), data }, memoc::block(*buffsp_));
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
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Buffer<T, Data_reference_allocator>, Data_reference_allocator>(hdr_.count()))
            {
                memoc::set(memoc::block(*buffsp_), value);
            }
            Array(std::initializer_list<std::int64_t> dims, const T& value)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
            {
            }
            template <typename U>
            Array(const Params<std::int64_t>& dims, const U& value)
                : hdr_(dims), buffsp_(memoc::make_shared<memoc::Buffer<T, Data_reference_allocator>, Data_reference_allocator>(hdr_.count()))
            {
                memoc::set(memoc::block(*buffsp_), value);
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, const U& value)
                : Array(Params<std::int64_t>{std::ssize(dims), dims.begin()}, value)
            {
            }

            [[nodiscard]] const Header& header() const noexcept
            {
                return hdr_;
            }

            [[nodiscard]] Header& header() noexcept
            {
                return hdr_;
            }

            [[nodiscard]] memoc::Block<T> block() const noexcept
            {
                return (buffsp_ ? memoc::block(*buffsp_) : memoc::Block<T>(0, nullptr));
            }

            [[nodiscard]] T* data() const noexcept
            {
                return block().data();
            }

            [[nodiscard]] const T& operator()(const Params<std::int64_t>& subs) const noexcept
            {
                return memoc::data(*buffsp_)[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] const T& operator()(std::initializer_list<std::int64_t> subs) const noexcept
            {
                return (*this)(Params<std::int64_t>{ std::ssize(subs), subs.begin() });
            }

            [[nodiscard]] T& operator()(const Params<std::int64_t>& subs) noexcept
            {
                return memoc::data(*buffsp_)[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] T& operator()(std::initializer_list<std::int64_t> subs) noexcept
            {
                return (*this)(Params<std::int64_t>{ std::ssize(subs), subs.begin() });
            }

            [[nodiscard]] Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> operator()(const Params<Interval<std::int64_t>>& ranges) const
            {
                if (empty(ranges) || empty(*this)) {
                    return (*this);
                }

                Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> slice{};
                slice.hdr_ = Header{ hdr_.dims(), hdr_.strides(), hdr_.offset(), ranges };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            [[nodiscard]] Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> operator()(std::initializer_list<Interval<std::int64_t>> ranges) const
            {
                return (*this)(Params<Interval<std::int64_t>>{std::ssize(ranges), ranges.begin()});
            }

            [[nodiscard]] Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> operator()(const Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>& indices) const noexcept
            {
                Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res(indices.header().dims());

                for (Subscripts_iterator iter({}, indices.header().dims()); iter; ++iter) {
                    res(*iter) = memoc::data(*buffsp_)[indices(*iter)];
                }

                return res;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& transform(const Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>& other, Binary_op&& op)
            {
                if (header().dims() != other.header().dims()) {
                    return *this;
                }

                for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, header().dims()); iter; ++iter) {
                    (*this)(*iter) = op((*this)(*iter), other(*iter));
                }

                return *this;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& transform(const T_o& other, Binary_op&& op)
            {
                for (typename Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, header().dims()); iter; ++iter) {
                    (*this)(*iter) = op((*this)(*iter), other);
                }

                return *this;
            }

        private:
            Header hdr_{};
            memoc::Shared_ptr<memoc::Buffer<T, Data_reference_allocator>, Data_reference_allocator> buffsp_{ nullptr };
        };

        /**
        * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
        */
        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline void copy(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& src, Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& dst)
        {
            if (empty(src) || empty(dst)) {
                return;
            }

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator src_iter({}, src.header().dims()); src_iter; ++src_iter) {
                dst(*src_iter) = src(*src_iter);
            }
        }
        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline void copy(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& src, Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> clone(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> clone(arr.header().dims());

            for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                clone(*iter) = arr(*iter);
            }

            return clone;
        }

        /**
        * @note Returning a reference to the input array, except in case of resulted empty array or an input subarray.
        */
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> reshape(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Params<std::int64_t>& new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().count() != numel(new_dims)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (arr.header().is_subarray()) {
                Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res(new_dims);

                typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
                typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, new_dims);

                while (arr_iter && res_iter) {
                    res(*res_iter) = arr(*arr_iter);
                    ++arr_iter;
                    ++res_iter;
                }

                return res;
            }

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(new_dims);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res(arr);
            res.header() = std::move(new_header);

            return res;
        }
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> reshape(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> resize(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Params<std::int64_t>& new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>(new_dims);
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            if (numel(new_dims) <= 0) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res(new_dims);

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, new_dims);

            while (arr_iter && res_iter) {
                res(*res_iter) = arr(*arr_iter);
                ++arr_iter;
                ++res_iter;
            }

            return res;
        }
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> resize(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, Params<std::int64_t>(std::ssize(new_dims), new_dims.begin()));
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> append(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> res(resize(lhs, { lhs.header().count() + rhs.header().count() }));

            Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

            for (std::int64_t i = lhs.header().count(); i < res.header().count(); ++i) {
                res({ i }) = rhs({ i - lhs.header().count() });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> append(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(lhs.header().dims(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>{};
            }

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator lhs_iter({}, lhs.header().dims());
            typename Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator rhs_iter({}, rhs.header().dims());
            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t fixed_axis{ modulo(axis, size(lhs.header().dims())) };

            while (res_iter) {
                if (lhs_iter && ((*res_iter)[fixed_axis] < lhs.header().dims()[fixed_axis] || (*res_iter)[fixed_axis] >= lhs.header().dims()[fixed_axis] + rhs.header().dims()[fixed_axis])) {
                    res(*res_iter) = lhs(*lhs_iter);
                    ++lhs_iter;
                }
                else if (rhs_iter && ((*res_iter)[fixed_axis] >= lhs.header().dims()[fixed_axis] && (*res_iter)[fixed_axis] < lhs.header().dims()[fixed_axis] + rhs.header().dims()[fixed_axis])) {
                    res(*res_iter) = rhs(*rhs_iter);
                    ++rhs_iter;
                }
                ++res_iter;
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> insert(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, std::int64_t ind)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> rlhs(reshape(lhs, { lhs.header().count() }));
            Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

            std::int64_t fixed_ind{ modulo(ind, lhs.header().count() + 1) };

            for (std::int64_t i = 0; i < fixed_ind; ++i) {
                res({ i }) = rlhs({ i });
            }
            for (std::int64_t i = 0; i < rhs.header().count(); ++i) {
                res({ fixed_ind + i }) = rrhs({ i });
            }
            for (std::int64_t i = 0; i < lhs.header().count() - fixed_ind; ++i) {
                res({ fixed_ind + rhs.header().count() + i }) = rlhs({ fixed_ind + i });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> insert(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, std::int64_t ind, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(lhs.header().dims(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator lhs_iter({}, lhs.header().dims());
            typename Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator rhs_iter({}, rhs.header().dims());
            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t fixed_axis{ modulo(axis, size(lhs.header().dims())) };
            std::int64_t fixed_ind{ modulo(ind, lhs.header().dims()[fixed_axis]) };

            while (res_iter) {
                if (lhs_iter && ((*res_iter)[fixed_axis] < fixed_ind || (*res_iter)[fixed_axis] >= fixed_ind + rhs.header().dims()[fixed_axis])) {
                    res(*res_iter) = lhs(*lhs_iter);
                    ++lhs_iter;
                }
                else if (rhs_iter && ((*res_iter)[fixed_axis] >= fixed_ind && (*res_iter)[fixed_axis] < fixed_ind + rhs.header().dims()[fixed_axis])) {
                    res(*res_iter) = rhs(*rhs_iter);
                    ++rhs_iter;
                }
                ++res_iter;
            }

            return res;
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> remove(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            std::int64_t fixed_ind{ modulo(ind, arr.header().count()) };
            std::int64_t fixed_count{ fixed_ind + count < arr.header().count() ? count : (arr.header().count() - fixed_ind) };

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() - fixed_count });
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> rarr(reshape(arr, { arr.header().count() }));

            for (std::int64_t i = 0; i < fixed_ind; ++i) {
                res({ i }) = rarr({ i });
            }
            for (std::int64_t i = fixed_ind + fixed_count; i < arr.header().count(); ++i) {
                res({ fixed_ind + i - fixed_count + 1 }) = rarr({ i });
            }

            return res;
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> remove(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            std::int64_t fixed_axis{ modulo(axis, size(arr.header().dims())) };
            std::int64_t fixed_ind{ modulo(ind, arr.header().dims()[fixed_axis]) };
            std::int64_t fixed_count{ fixed_ind + count <= arr.header().dims()[fixed_axis] ? count : (arr.header().dims()[fixed_axis] - fixed_ind) };

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(arr.header().dims(), -fixed_count, fixed_axis);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() - (arr.header().count() / arr.header().dims()[fixed_axis]) * fixed_count });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            while (arr_iter) {
                if (res_iter && ((*arr_iter)[fixed_axis] < fixed_ind || (*arr_iter)[fixed_axis] >= fixed_ind + fixed_count)) {
                    res(*res_iter) = arr(*arr_iter);
                    ++res_iter;
                }
                ++arr_iter;
            }

            return res;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool empty(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr) noexcept
        {
            return (memoc::empty(arr.block()) || arr.header().is_subarray()) && arr.header().empty();
        }

        template <typename T, typename Unary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>    
        [[nodiscard]] inline auto transform(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, Unary_op&& op)
            -> Array<decltype(op(arr.data()[0])), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            using T_o = decltype(op(arr.data()[0]));

            if (empty(arr)) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res(arr.header().dims());

            for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                res(*iter) = op(arr(*iter));
            }

            return res;
        }

        template <typename T, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, Binary_op&& op)
            -> decltype(op(arr.data()[0], arr.data()[0]))
        {
            using T_o = decltype(op(arr.data()[0], arr.data()[0]));

            if (empty(arr)) {
                return T_o{};
            }

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter{ {}, arr.header().dims() };

            T_o res{ static_cast<T_o>(arr(*iter)) };
            ++iter;

            while (iter) {
                res = op(res, arr(*iter));
                ++iter;
            }

            return res;
        }

        template <typename T, typename T_o, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const T_o& init_value, Binary_op&& op)
            -> decltype(op(init_value, arr.data()[0]))
        {
            if (empty(arr)) {
                return init_value;
            }

            T_o res{ init_value };
            for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter{ {}, arr.header().dims() }; iter; ++iter) {
                res = op(res, arr(*iter));
            }

            return res;
        }

        template <typename T, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, Binary_op&& op, std::int64_t axis)
            -> Array<decltype(op(arr.data()[0], arr.data()[0])), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            using T_o = decltype(op(arr.data()[0], arr.data()[0]));

            if (empty(arr)) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            typename Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(arr.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), axis);
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            const std::int64_t reduction_iteration_cycle{ arr.header().dims()[modulo(axis, size(arr.header().dims()))] };

            while (arr_iter && res_iter) {
                T_o res_element{ static_cast<T_o>(arr(*arr_iter)) };
                ++arr_iter;
                for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++arr_iter) {
                    res_element = op(res_element, arr(*arr_iter));
                }
                res(*res_iter) = res_element;
                ++res_iter;
            }

            return res;
        }

        template <typename T, typename T_o, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>& init_values, Binary_op&& op, std::int64_t axis)
            -> Array<decltype(op(init_values.data()[0], arr.data()[0])), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            if (empty(arr)) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            const std::int64_t fixed_axis{ modulo(axis, size(arr.header().dims())) };

            if (size(init_values.header().dims()) != 1 && init_values.header().dims()[fixed_axis] != arr.header().dims()[fixed_axis]) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            typename Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(arr.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), axis);
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator init_iter({}, init_values.header().dims());

            const std::int64_t reduction_iteration_cycle{ arr.header().dims()[fixed_axis] };

            while (arr_iter && res_iter && init_iter) {
                T_o res_element{ init_values(*init_iter) };
                for (std::int64_t i = 0; i < reduction_iteration_cycle; ++i, ++arr_iter) {
                    res_element = op(res_element, arr(*arr_iter));
                }
                res(*res_iter) = std::move(res_element);
                ++res_iter;
                ++init_iter;
            }

            return res;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<bool>(a) && static_cast<bool>(b); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> all(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<bool>(a) && static_cast<bool>(b); }, axis);
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool any(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<bool>(a) || static_cast<bool>(b); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> any(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<bool>(a) || static_cast<bool>(b); }, axis);
        }

        template <typename T1, typename T2, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs.data()[0], rhs.data()[0])), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs.data()[0], rhs.data()[0]));

            if (lhs.header().dims() != rhs.header().dims()) {
                return Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res(lhs.header().dims());

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs(*iter), rhs(*iter));
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs.data()[0], rhs)), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs.data()[0], rhs));

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res(lhs.header().dims());

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs(*iter), rhs);
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto transform(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs, rhs.data()[0])), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs, rhs.data()[0]));

            Array<T_o, Data_allocator, Data_reference_allocator, Internals_allocator> res(rhs.header().dims());

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, rhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs, rhs(*iter));
            }

            return res;
        }

        template <typename T, typename Unary_pred, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> filter(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t res_count{ 0 };

            while (arr_iter && res_iter) {
                if (pred(arr(*arr_iter))) {
                    res(*res_iter) = arr(*arr_iter);
                    ++res_count;
                    ++res_iter;
                }
                ++arr_iter;
            }

            if (res_count == 0) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> filter(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (arr.header().dims() != mask.header().dims()) {
                return Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator mask_iter({}, mask.header().dims());

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t res_count{ 0 };

            while (arr_iter && mask_iter && res_iter) {
                if (mask(*mask_iter)) {
                    res(*res_iter) = arr(*arr_iter);
                    ++res_count;
                    ++res_iter;
                }
                ++arr_iter;
                ++mask_iter;
            }

            if (res_count == 0) {
                return Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Unary_pred, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator> find(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t res_count{ 0 };

            while (arr_iter && res_iter) {
                if (pred(arr(*arr_iter))) {
                    res(*res_iter) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), *arr_iter);
                    ++res_count;
                    ++res_iter;
                }
                ++arr_iter;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator> find(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (arr.header().dims() != mask.header().dims()) {
                return Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator mask_iter({}, mask.header().dims());

            typename Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t res_count{ 0 };

            while (arr_iter && mask_iter && res_iter) {
                if (mask(*mask_iter)) {
                    res(*res_iter) = subs2ind(arr.header().offset(), arr.header().strides(), arr.header().dims(), *arr_iter);
                    ++res_count;
                    ++res_iter;
                }
                ++arr_iter;
                ++mask_iter;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> transpose(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, const Params<std::int64_t>& order)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Header new_header(arr.header().dims(), order);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> res({ arr.header().count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), order);
            typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            while (arr_iter && res_iter) {
                res(*res_iter) = arr(*arr_iter);
                ++arr_iter;
                ++res_iter;
            }

            return res;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> transpose(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> order)
        {
            return transpose(arr, Params<std::int64_t>(std::ssize(order), order.begin()));
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator==(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator==(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator==(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator!=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator!=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator!=(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> close(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> close(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> close(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator>=(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<=(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_allocator, Data_reference_allocator, Internals_allocator> operator<=(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator+(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator+=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator+=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator-(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator-=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator-=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator*(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator*=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator*=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator/(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator/=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator/=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto operator%(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator%(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator%(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator%=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator%=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator^(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator^=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator^=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator&=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator&=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator|(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator|=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator|=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator<<(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
            -> Array<decltype(lhs << rhs.data()[0]), Data_allocator, Data_reference_allocator, Internals_allocator>
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator>>(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator~(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return ~a; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator!(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return !a; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return +a; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return -a; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto abs(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return abs(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto acos(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acos(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto acosh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acosh(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto asin(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asin(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto asinh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asinh(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto atan(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atan(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto atanh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atanh(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto cos(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cos(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto cosh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cosh(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto exp(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return exp(a); });
        }
        
        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto log(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto log10(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log10(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto pow(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return pow(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto sin(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sin(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto sinh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sinh(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto sqrt(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sqrt(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto tan(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tan(a); });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto tanh(const Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tanh(a); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator&&(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator||(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator++(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                ++arr(*iter);
            }
            return arr;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& arr)
        {
            return operator++(arr);
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto operator++(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> old = clone(arr);
            operator++(arr);
            return old;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto& operator--(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                --arr(*iter);
            }
            return arr;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& arr)
        {
            return operator--(arr);
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        inline auto operator--(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_allocator, Data_reference_allocator, Internals_allocator> old = clone(arr);
            operator--(arr);
            return old;
        }

        template <typename T, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_allocator, Data_reference_allocator, Internals_allocator>&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <typename T1, typename T2, typename Binary_pred, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, Binary_pred pred)
        {
            if (empty(lhs) && empty(rhs)) {
                return true;
            }

            if (empty(lhs) || empty(rhs)) {
                return false;
            }

            if (lhs.header().dims() != rhs.header().dims()) {
                return false;
            }

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                if (!pred(lhs(*iter), rhs(*iter))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_pred pred)
        {
            if (empty(lhs)) {
                return true;
            }

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                if (!pred(lhs(*iter), rhs)) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_match(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, Binary_pred pred)
        {
            if (empty(rhs)) {
                return true;
            }

            for (typename Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>::Subscripts_iterator iter({}, rhs.header().dims()); iter; ++iter) {
                if (!pred(lhs, rhs(*iter))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_equal(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_allocator, Data_reference_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, memoc::Allocator Data_allocator, memoc::Allocator Data_reference_allocator, memoc::Allocator Internals_allocator>
        [[nodiscard]] inline bool all_close(const T1& lhs, const Array<T2, Data_allocator, Data_reference_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }
    }

    using details::Array;
    using details::Array_subscripts_iterator;

    using details::copy;
    using details::clone;
    using details::reshape;
    using details::resize;
    using details::append;
    using details::insert;
    using details::remove;

    using details::empty;
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


    using details::abs;
    using details::acos;
    using details::acosh;
    using details::asin;
    using details::asinh;
    using details::atan;
    using details::atanh;
    using details::cos;
    using details::cosh;
    using details::exp;
    using details::log;
    using details::log10;
    using details::pow;
    using details::sin;
    using details::sinh;
    using details::sqrt;
    using details::tan;
    using details::tanh;
}

#endif // COMPUTOC_TYPES_NDARRAY_H
