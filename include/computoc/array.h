#ifndef COMPUTOC_TYPES_NDARRAY_H
#define COMPUTOC_TYPES_NDARRAY_H

#include <cstdint>
#include <memory>
#include <initializer_list>
#include <stdexcept>
#include <span>
#include <limits>
#include <algorithm>
#include <numeric>

#include <computoc/utils.h>
#include <computoc/math.h>

namespace computoc {
    namespace details {

        template <typename T>
        using Replace_with_char_if_bool = std::conditional_t<std::is_same_v<bool, T>, char, T>;

        inline constexpr std::uint32_t dynamic_sequence = std::numeric_limits<std::uint32_t>::max();

        template <typename T, std::int64_t N = dynamic_sequence, template<typename> typename Allocator = std::allocator>
        requires (!std::is_same_v<bool, T> && N > 0)
        using simple_sequence = std::conditional_t<N == dynamic_sequence, std::vector<T, Allocator<T>>, std::array<T, N>>;

        template <typename T, template<typename> typename Allocator = std::allocator>
        requires (!std::is_same_v<bool, T>)
        using simple_vector = std::vector<T, Allocator<T>>;

        template <typename T, typename U>
        [[nodiscard]] inline bool operator==(const std::span<T>& lhs, const std::span<U>& rhs) {
            return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
        }

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
        [[nodiscard]] inline std::int64_t numel(std::span<const std::int64_t> dims) noexcept
        {
            if (dims.empty()) {
                return 0;
            }

            std::int64_t res{ 1 };
            for (std::int64_t i = 0; i < std::ssize(dims); ++i) {
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
        inline std::int64_t compute_strides(std::span<const std::int64_t> dims, std::span<std::int64_t> strides) noexcept
        {
            std::int64_t num_strides{ std::ssize(dims) > std::ssize(strides) ? std::ssize(strides) : std::ssize(dims) };
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
        inline std::int64_t compute_strides(std::span<const std::int64_t> previous_dims, std::span<const std::int64_t> previous_strides, std::span<const Interval<std::int64_t>> intervals, std::span<std::int64_t> strides) noexcept
        {
            std::int64_t nstrides{ std::ssize(previous_strides) > std::ssize(strides) ? std::ssize(strides) : std::ssize(previous_strides) };
            if (nstrides <= 0) {
                return 0;
            }

            std::int64_t ncomp_from_intervals{ nstrides > std::ssize(intervals) ? std::ssize(intervals) : nstrides };

            // compute strides with interval step
            for (std::int64_t i = 0; i < ncomp_from_intervals; ++i) {
                strides[i] = previous_strides[i] * forward(intervals[i]).step;
            }

            // compute strides from previous dimensions
            if (intervals.size() < previous_dims.size() && nstrides >= previous_dims.size()) {
                strides[previous_dims.size() - 1] = 1;
                for (std::int64_t i = std::ssize(previous_dims) - 2; i >= std::ssize(intervals); --i) {
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
        inline std::int64_t compute_dims(std::span<const std::int64_t> previous_dims, std::span<const Interval<std::int64_t>> intervals, std::span<std::int64_t> dims) noexcept
        {
            std::int64_t ndims{ std::ssize(previous_dims) > std::ssize(dims) ? std::ssize(dims) : std::ssize(previous_dims) };
            if (ndims <= 0) {
                return 0;
            }

            std::int64_t num_computed_dims{ ndims > std::ssize(intervals) ? std::ssize(intervals) : ndims };

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

        [[nodiscard]] inline std::int64_t compute_offset(std::span<const std::int64_t> previous_dims, std::int64_t previous_offset, std::span<const std::int64_t> previous_strides, std::span<const Interval<std::int64_t>> intervals) noexcept
        {
            std::int64_t offset{ previous_offset };

            if (previous_dims.empty() || previous_strides.empty() || intervals.empty()) {
                return offset;
            }

            std::int64_t num_computations{ std::ssize(previous_dims) > std::ssize(previous_strides) ? std::ssize(previous_strides) : std::ssize(previous_dims) };
            num_computations = (num_computations > std::ssize(intervals) ? std::ssize(intervals) : num_computations);

            for (std::int64_t i = 0; i < num_computations; ++i) {
                offset += previous_strides[i] * forward(modulo(intervals[i], previous_dims[i])).start;
            }
            return offset;
        }

        /**
        * @note Extra subscripts are ignored. If number of subscripts are less than number of strides/dimensions, they are considered as the less significant subscripts.
        */
        [[nodiscard]] inline std::int64_t subs2ind(std::int64_t offset, std::span<const std::int64_t> strides, std::span<const std::int64_t> dims, std::span<std::int64_t> subs) noexcept
        {
            std::int64_t ind{ offset };

            if (strides.empty() || dims.empty() || subs.empty()) {
                return ind;
            }

            std::int64_t num_used_subs{ std::ssize(strides) > std::ssize(dims) ? std::ssize(dims) : std::ssize(strides) };
            num_used_subs = (num_used_subs > std::ssize(subs) ? std::ssize(subs) : num_used_subs);

            std::int64_t num_ignored_subs{ std::ssize(strides) - num_used_subs};
            if (num_ignored_subs < 0) { // ignore extra subscripts
                num_ignored_subs = 0;
            }

            for (std::int64_t i = num_ignored_subs; i < std::ssize(strides); ++i) {
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

        template <template<typename> typename Internal_allocator = std::allocator>
        class Array_header {
        public:
            Array_header() = default;

            Array_header(std::span<const std::int64_t> dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = simple_vector<std::int64_t, Internal_allocator>(dims.begin(), dims.end());

                strides_ = simple_vector<std::int64_t, Internal_allocator>(dims.size());
                compute_strides(dims, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(std::span<const std::int64_t> previous_dims, std::span<const std::int64_t> previous_strides, std::int64_t previous_offset, std::span<const Interval<std::int64_t>> intervals)
                : is_subarray_(true)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                simple_vector<std::int64_t, Internal_allocator> dims = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());

                if (compute_dims(previous_dims, intervals, dims) <= 0) {
                    return;
                }

                dims_ = std::move(dims);
                
                count_ = numel(dims_);

                strides_ = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());
                compute_strides(previous_dims, previous_strides, intervals, strides_);

                offset_ = compute_offset(previous_dims, previous_offset, previous_strides, intervals);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(std::span<const std::int64_t> previous_dims, std::int64_t omitted_axis)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                std::int64_t axis{ modulo(omitted_axis, std::ssize(previous_dims)) };
                std::int64_t ndims{ std::ssize(previous_dims) > 1 ? std::ssize(previous_dims) - 1 : 1 };

                dims_ = simple_vector<std::int64_t, Internal_allocator>(ndims);

                if (previous_dims.size() > 1) {
                    for (std::int64_t i = 0; i < axis; ++i) {
                        dims_[i] = previous_dims[i];
                    }
                    for (std::int64_t i = axis + 1; i < previous_dims.size(); ++i) {
                        dims_[i - 1] = previous_dims[i];
                    }
                }
                else {
                    dims_[0] = 1;
                }

                strides_ = simple_vector<std::int64_t, Internal_allocator>(ndims);
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(std::span<const std::int64_t> previous_dims, std::span<const std::int64_t> new_order)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                if (new_order.empty()) {
                    return;
                }

                simple_vector<std::int64_t, Internal_allocator> dims = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());

                for (std::int64_t i = 0; i < std::ssize(previous_dims); ++i) {
                    dims[i] = previous_dims[modulo(new_order[i], previous_dims[i])];
                }

                if (numel(previous_dims) != numel(dims)) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(std::span<const std::int64_t> previous_dims, std::int64_t count, std::int64_t axis)
            {
                if (numel(previous_dims) <= 0) {
                    return;
                }

                simple_vector<std::int64_t, Internal_allocator> dims = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_dims)) };
                for (std::int64_t i = 0; i < previous_dims.size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_dims[i] : previous_dims[i] + count;
                }
                
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(std::span<const std::int64_t> previous_dims, std::span<const std::int64_t> appended_dims, std::int64_t axis)
            {
                if (previous_dims.size() != appended_dims.size()) {
                    return;
                }

                if (numel(previous_dims) <= 0) {
                    return;
                }

                if (numel(appended_dims) <= 0) {
                    return;
                }

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_dims)) };

                bool are_dims_valid_for_append{ true };
                for (std::int64_t i = 0; i < previous_dims.size(); ++i) {
                    if (i != fixed_axis && previous_dims[i] != appended_dims[i]) {
                        are_dims_valid_for_append = false;
                    }
                }
                if (!are_dims_valid_for_append) {
                    return;
                }

                simple_vector<std::int64_t, Internal_allocator> dims = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());

                for (std::int64_t i = 0; i < previous_dims.size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_dims[i] : previous_dims[i] + appended_dims[fixed_axis];
                }

                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Internal_allocator>(previous_dims.size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(Array_header&& other) noexcept
                : dims_(std::move(other.dims_)), strides_(std::move(other.strides_)), count_(other.count_), offset_(other.offset_), is_subarray_(other.is_subarray_), last_index_(other.last_index_)
            {
                other.count_ = other.offset_ = 0;
                other.is_subarray_ = false;
                other.last_index_ = 0;
            }
            Array_header& operator=(Array_header&& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                dims_ = std::move(other.dims_);
                strides_ = std::move(other.strides_);
                count_ = other.count_;
                offset_ = other.offset_;
                is_subarray_ = other.is_subarray_;
                last_index_ = other.last_index_;

                other.count_ = other.offset_ = 0;
                other.is_subarray_ = false;
                other.last_index_ = 0;

                return *this;
            }

            Array_header(const Array_header& other) noexcept
                : dims_(other.dims_), strides_(other.strides_), count_(other.count_), offset_(other.offset_), is_subarray_(other.is_subarray_), last_index_(other.last_index_)
            {
            }
            Array_header& operator=(const Array_header& other) noexcept
            {
                if (&other == this) {
                    return *this;
                }

                dims_ = other.dims_;
                strides_ = other.strides_;
                count_ = other.count_;
                offset_ = other.offset_;
                is_subarray_ = other.is_subarray_;
                last_index_ = other.last_index_;

                return *this;
            }

            virtual ~Array_header() = default;

            [[nodiscard]] std::int64_t count() const noexcept
            {
                return count_;
            }

            [[nodiscard]] std::span<const std::int64_t> dims() const noexcept
            {
                return std::span<const std::int64_t>(dims_.data(), dims_.size());
            }

            [[nodiscard]] std::span<const std::int64_t> strides() const noexcept
            {
                return std::span<const std::int64_t>(strides_.data(), strides_.size());
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
                return dims_.empty();
            }

            [[nodiscard]] std::int64_t last_index() const noexcept
            {
                return last_index_;
            }

        private:
            simple_vector<std::int64_t, Internal_allocator> dims_{};
            simple_vector<std::int64_t, Internal_allocator> strides_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            std::int64_t last_index_{ 0 };
            bool is_subarray_{ false };
        };


        template <template<typename> typename Internal_allocator = std::allocator>
        class Array_subscripts_iterator
        {
        public:
            Array_subscripts_iterator(std::span<const std::int64_t> start, std::span<const std::int64_t> minimum_excluded, std::span<const std::int64_t> maximum_excluded, std::int64_t axis)
            {
                std::int64_t bounds_size{ std::ssize(minimum_excluded) > std::ssize(maximum_excluded) ? std::ssize(minimum_excluded) : std::ssize(maximum_excluded) };
                nsubs_ = std::ssize(start) > bounds_size ? std::ssize(start) : bounds_size;

                if (nsubs_ > 0) {
                    buff_ = simple_vector<std::int64_t, Internal_allocator>(nsubs_ * 4);

                    axis_ = modulo(axis, nsubs_);

                    bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
                    subs_ = bsubs_.data();
                    start_ = subs_ + nsubs_;
                    if (start.empty()) {
                        std::fill(subs_, subs_ + nsubs_, 0);
                        std::fill(start_, start_ + nsubs_, 0);
                    }
                    else {
                        std::copy(start.data(), start.data() + nsubs_, subs_);
                        std::copy(start.data(), start.data() + nsubs_, start_);
                    }

                    minimum_excluded_ = start_ + nsubs_;
                    if (!minimum_excluded.empty()) {
                        std::copy(minimum_excluded.data(), minimum_excluded.data() + nsubs_, minimum_excluded_);
                    }
                    else if(!start.empty()) {
                        std::copy(start.data(), start.data() + nsubs_, minimum_excluded_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        std::fill(minimum_excluded_, minimum_excluded_ + nsubs_, -1);
                    }

                    maximum_excluded_ = minimum_excluded_ + nsubs_;
                    if (maximum_excluded.empty()) {
                        std::fill(maximum_excluded_, maximum_excluded_ + nsubs_, 1);
                    }
                    else {
                        std::copy(maximum_excluded.data(), maximum_excluded.data() + nsubs_, maximum_excluded_);
                    }

                    major_axis_ = find_major_axis();
                    min_at_major_ = minimum_excluded_[major_axis_];
                    max_at_major_ = maximum_excluded_[major_axis_];
                }
            }

            Array_subscripts_iterator(std::span<const std::int64_t> start, std::span<const std::int64_t> minimum_excluded, std::span<const std::int64_t> maximum_excluded, std::span<const std::int64_t> order)
            {
                std::int64_t bounds_size{ std::ssize(minimum_excluded) > std::ssize(maximum_excluded) ? std::ssize(minimum_excluded) : std::ssize(maximum_excluded) };
                nsubs_ = std::ssize(start) > bounds_size ? std::ssize(start) : bounds_size;

                if (nsubs_ > 0) {
                    if (order.size() >= nsubs_) {
                        buff_ = simple_vector<std::int64_t, Internal_allocator>(nsubs_ * 5);
                    }
                    else {
                        buff_ = simple_vector<std::int64_t, Internal_allocator>(nsubs_ * 4);
                        axis_ = nsubs_ - 1;
                    }


                    bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
                    subs_ = bsubs_.data();
                    start_ = subs_ + nsubs_;
                    if (start.empty()) {
                        std::fill(subs_, subs_ + nsubs_, 0);
                        std::fill(start_, start_ + nsubs_, 0);
                    }
                    else {
                        std::copy(start.data(), start.data() + nsubs_, subs_);
                        std::copy(start.data(), start.data() + nsubs_, start_);
                    }

                    minimum_excluded_ = start_ + nsubs_;
                    if (!minimum_excluded.empty()) {
                        std::copy(minimum_excluded.data(), minimum_excluded.data() + nsubs_, minimum_excluded_);
                    }
                    else if (!start.empty()) {
                        std::copy(start.data(), start.data() + nsubs_, minimum_excluded_);
                        for (std::int64_t i = 0; i < nsubs_; ++i) {
                            minimum_excluded_[i] -= 1;
                        }
                    }
                    else {
                        std::fill(minimum_excluded_, minimum_excluded_ + nsubs_, -1);
                    }

                    maximum_excluded_ = minimum_excluded_ + nsubs_;
                    if (maximum_excluded.empty()) {
                        std::fill(maximum_excluded_, maximum_excluded_ + nsubs_, 1);
                    }
                    else {
                        std::copy(maximum_excluded.data(), maximum_excluded.data() + nsubs_, maximum_excluded_);
                    }

                    major_axis_ = find_major_axis();

                    if (order.size() >= nsubs_) {
                        order_ = maximum_excluded_ + nsubs_;
                        std::copy(order.data(), order.data() + nsubs_, order_);
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

            Array_subscripts_iterator(std::span<const std::int64_t> from, std::span<const std::int64_t> to, std::int64_t axis)
                : Array_subscripts_iterator(from, {},to, axis)
            {
            }
            Array_subscripts_iterator(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::int64_t axis)
                : Array_subscripts_iterator(std::span<const std::int64_t>(from.begin(), from.size()), std::span<const std::int64_t>(to.begin(), to.size()), axis)
            {
            }

            Array_subscripts_iterator(const std::span<const std::int64_t> from, std::span<const std::int64_t> to, std::span<const std::int64_t> order = std::span<const std::int64_t>())
                : Array_subscripts_iterator(from, {}, to, order)
            {
            }
            Array_subscripts_iterator(std::initializer_list<std::int64_t> from, std::initializer_list<std::int64_t> to, std::initializer_list<std::int64_t> order = {})
                : Array_subscripts_iterator(std::span<const std::int64_t>(from.begin(), from.size()), std::span<const std::int64_t>(to.begin(), to.size()), std::span<const std::int64_t>(order.begin(), order.size()))
            {
            }

            Array_subscripts_iterator() = default;

            Array_subscripts_iterator(const Array_subscripts_iterator<Internal_allocator>& other) noexcept
                : buff_(other.buff_), nsubs_(other.nsubs_), axis_(other.axis_), major_axis_(other.major_axis_), min_at_major_(other.min_at_major_), max_at_major_(other.max_at_major_)
            {
                bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
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
                bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
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
                bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
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
                bsubs_ = { buff_.data(), static_cast<std::size_t>(nsubs_) };
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
                std::copy(start_, start_ + nsubs_, bsubs_.data());
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

            [[nodiscard]] const std::span<std::int64_t>& operator*() const noexcept
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

            simple_vector<std::int64_t, Internal_allocator> buff_{};

            std::int64_t nsubs_{ 0 };

            std::span<std::int64_t> bsubs_{};
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




        template <template<typename> typename Internal_allocator = std::allocator>
        class Array_indices_generator final
        {
        public:
            Array_indices_generator(const Array_header<Internal_allocator>& hdr, bool backward = false)
                : dims_(hdr.dims().begin(), hdr.dims().end()), strides_(hdr.strides().begin(), hdr.strides().end()), indices_(hdr.dims().size()), max_iterations_(hdr.count())
                , current_index_(hdr.offset())
            {
                if (backward) {
                    num_iterations_ = max_iterations_ - 1;
                    std::transform(dims_.begin(), dims_.end(), indices_.begin(), [](auto a) { return a - 1; });
                    current_index_ = hdr.last_index();
                }
            }

            Array_indices_generator(const Array_header<Internal_allocator>& hdr, std::int64_t axis, bool backward = false)
                : dims_(reorder(hdr.dims(), axis)), strides_(reorder(hdr.strides(), axis)), indices_(hdr.dims().size()), max_iterations_(hdr.count())
                , current_index_(hdr.offset())
            {
                if (backward) {
                    num_iterations_ = max_iterations_ - 1;
                    std::transform(dims_.begin(), dims_.end(), indices_.begin(), [](auto a) { return a - 1; });
                    current_index_ = hdr.last_index();
                }
            }

            Array_indices_generator(const Array_header<Internal_allocator>& hdr, std::span<const std::int64_t> order, bool backward = false)
                : dims_(reorder(hdr.dims(), order)), strides_(reorder(hdr.strides(), order)), indices_(hdr.dims().size()), max_iterations_(hdr.count())
                , current_index_(hdr.offset())
            {
                if (backward) {
                    num_iterations_ = max_iterations_ - 1;
                    std::transform(dims_.begin(), dims_.end(), indices_.begin(), [](auto a) { return a - 1; });
                    current_index_ = hdr.last_index();
                }
            }

            Array_indices_generator() = default;

            Array_indices_generator(const Array_indices_generator<Internal_allocator>& other) = default;
            Array_indices_generator<Internal_allocator>& operator=(const Array_indices_generator<Internal_allocator>& other) = default;

            Array_indices_generator(Array_indices_generator<Internal_allocator>&& other) noexcept = default;
            Array_indices_generator<Internal_allocator>& operator=(Array_indices_generator<Internal_allocator>&& other) noexcept = default;

            ~Array_indices_generator() = default;

            Array_indices_generator<Internal_allocator>& operator++() noexcept
            {
                ++num_iterations_;

                for (std::int64_t i = std::ssize(indices_) - 1; i >= 0; --i) {
                    ++indices_[i];
                    current_index_ += strides_[i];
                    if (indices_[i] < dims_[i]) {
                        return *this;
                    }
                    if (i != 0)
                    {
                        current_index_ -= indices_[i] * strides_[i];
                        indices_[i] = 0;
                    }
                }
                return *this;
            }

            Array_indices_generator<Internal_allocator> operator++(int) noexcept
            {
                Array_indices_generator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_indices_generator<Internal_allocator>& operator--() noexcept
            {
                --num_iterations_;

                for (std::int64_t i = std::ssize(indices_) - 1; i >= 0; --i) {
                    --indices_[i];
                    current_index_ -= strides_[i];
                    if (indices_[i] > -1) {
                        return *this;
                    }
                    if (i != 0)
                    {
                        indices_[i] = dims_[i] - 1;
                        current_index_ += (indices_[i] + 1) * strides_[i];
                    }
                }
                return *this;
            }

            Array_indices_generator<Internal_allocator> operator--(int) noexcept
            {
                Array_indices_generator temp{ *this };
                --(*this);
                return temp;
            }

            [[nodiscard]] explicit operator bool() const noexcept
            {
                return num_iterations_ < max_iterations_ && num_iterations_ > -1;
            }

            [[nodiscard]] std::int64_t operator*() const noexcept
            {
                return current_index_;
            }

            [[nodiscard]] std::int64_t num_iterations() const noexcept
            {
                return num_iterations_;
            }

        private:
            static simple_vector<std::int64_t, Internal_allocator> reorder(std::span<const std::int64_t> vec, std::int64_t axis)
            {
                // create ordered indices according to input axis parameter
                simple_vector<std::int64_t, Internal_allocator> new_ordered_indices(vec.size());
                new_ordered_indices[0] = axis;
                std::int64_t pos = 1;
                for (std::int64_t i = 0; i < vec.size(); ++i) {
                    if (i != axis) {
                        new_ordered_indices[pos++] = i;
                    }
                }

                return reorder(vec, new_ordered_indices);
            }

            static simple_vector<std::int64_t, Internal_allocator> reorder(std::span<const std::int64_t> vec, std::span<const std::int64_t> indices)
            {
                std::size_t size = std::min(vec.size(), indices.size());
                simple_vector<std::int64_t, Internal_allocator> res(size);
                for (std::int64_t i = 0; i < size; ++i) {
                    res[i] = vec[indices[i]];
                }
                return res;
            }

            simple_vector<std::int64_t, Internal_allocator> dims_;
            simple_vector<std::int64_t, Internal_allocator> strides_;

            simple_vector<std::int64_t, Internal_allocator> indices_;
            std::int64_t current_index_ = 0;

            std::int64_t max_iterations_ = 0;
            std::int64_t num_iterations_ = 0;
        };


        template <typename T, template<typename> typename Internal_allocator = std::allocator>
        class Array_iterator final
        {
        public:
            using value_type = T;
            using difference_type = std::ptrdiff_t;

            Array_iterator(T* data, const Array_indices_generator<Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_iterator(T* data)
                : data_(data)
            {
            }

            Array_iterator() = default;

            Array_iterator(const Array_iterator<T, Internal_allocator>& other) = default;
            Array_iterator<T, Internal_allocator>& operator=(const Array_iterator<T, Internal_allocator>& other) = default;

            Array_iterator(Array_iterator<T, Internal_allocator>&& other) noexcept = default;
            Array_iterator<T, Internal_allocator>& operator=(Array_iterator<T, Internal_allocator>&& other) noexcept = default;

            ~Array_iterator() = default;

            Array_iterator<T, Internal_allocator>& operator++() noexcept
            {
                auto diff = *gen_;
                diff = *(++gen_) - diff;
                data_ += diff;
                return *this;
            }

            Array_iterator<T, Internal_allocator> operator++(int) noexcept
            {
                Array_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_iterator<T, Internal_allocator>& operator--() noexcept
            {
                auto diff = *gen_;
                diff -= *(--gen_);
                data_ -= diff;
                return *this;
            }

            Array_iterator<T, Internal_allocator> operator--(int) noexcept
            {
                Array_iterator temp{ *this };
                --(*this);
                return temp;
            }

            [[nodiscard]] T& operator*() const noexcept
            {
                return *data_;
            }

            [[nodiscard]] bool operator==(const Array_iterator<T, Internal_allocator>& iter) const
            {
                return data_ == iter.data_;
            }

        private:
            Array_indices_generator<Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, template<typename> typename Internal_allocator = std::allocator>
        class Array_const_iterator final
        {
        public:
            Array_const_iterator(T* data, const Array_indices_generator<Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_const_iterator(T* data)
                : data_(data)
            {
            }

            Array_const_iterator() = default;

            Array_const_iterator(const Array_const_iterator<T, Internal_allocator>& other) = default;
            Array_const_iterator<T, Internal_allocator>& operator=(const Array_const_iterator<T, Internal_allocator>& other) = default;

            Array_const_iterator(Array_const_iterator<T, Internal_allocator>&& other) noexcept = default;
            Array_const_iterator<T, Internal_allocator>& operator=(Array_const_iterator<T, Internal_allocator>&& other) noexcept = default;

            ~Array_const_iterator() = default;

            Array_const_iterator<T, Internal_allocator>& operator++() noexcept
            {
                data_ -= *gen_;
                ++gen_;
                data_ += *gen_;
                return *this;
            }

            Array_const_iterator<T, Internal_allocator> operator++(int) noexcept
            {
                Array_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_const_iterator<T, Internal_allocator>& operator--() noexcept
            {
                data_ += *gen_;
                --gen_;
                data_ -= *gen_;
                return *this;
            }

            Array_const_iterator<T, Internal_allocator> operator--(int) noexcept
            {
                Array_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            [[nodiscard]] const T& operator*() const noexcept
            {
                return *data_;
            }

            [[nodiscard]] bool operator==(const Array_const_iterator<T, Internal_allocator>& iter) const
            {
                return data_ == iter.data_;
            }

        private:
            Array_indices_generator<Internal_allocator> gen_;
            T* data_ = nullptr;
        };



        template <typename T, template<typename> typename Internal_allocator = std::allocator>
        class Array_reverse_iterator final
        {
        public:
            using value_type = T;
            using difference_type = std::ptrdiff_t;

            Array_reverse_iterator(T* data, const Array_indices_generator<Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_reverse_iterator(T* data)
                : data_(data)
            {
            }

            Array_reverse_iterator() = default;

            Array_reverse_iterator(const Array_reverse_iterator<T, Internal_allocator>& other) = default;
            Array_reverse_iterator<T, Internal_allocator>& operator=(const Array_reverse_iterator<T, Internal_allocator>& other) = default;

            Array_reverse_iterator(Array_reverse_iterator<T, Internal_allocator>&& other) noexcept = default;
            Array_reverse_iterator<T, Internal_allocator>& operator=(Array_reverse_iterator<T, Internal_allocator>&& other) noexcept = default;

            ~Array_reverse_iterator() = default;

            Array_reverse_iterator<T, Internal_allocator>& operator++() noexcept
            {
                auto diff = *gen_;
                diff -= *(--gen_);
                data_ -= diff;
                return *this;
            }

            Array_reverse_iterator<T, Internal_allocator> operator++(int) noexcept
            {
                Array_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_reverse_iterator<T, Internal_allocator>& operator--() noexcept
            {
                auto diff = *gen_;
                diff = *(++gen_) - diff;
                data_ += diff;
                return *this;
            }

            Array_reverse_iterator<T, Internal_allocator> operator--(int) noexcept
            {
                Array_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            [[nodiscard]] T& operator*() const noexcept
            {
                return *data_;
            }

            [[nodiscard]] bool operator==(const Array_reverse_iterator<T, Internal_allocator>& iter) const
            {
                return data_ == iter.data_;
            }

        private:
            Array_indices_generator<Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, template<typename> typename Internal_allocator = std::allocator>
        class Array_const_reverse_iterator final
        {
        public:
            Array_const_reverse_iterator(T* data, const Array_indices_generator<Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_const_reverse_iterator(T* data)
                : data_(data)
            {
            }

            Array_const_reverse_iterator() = default;

            Array_const_reverse_iterator(const Array_const_reverse_iterator<T, Internal_allocator>& other) = default;
            Array_const_reverse_iterator<T, Internal_allocator>& operator=(const Array_const_reverse_iterator<T, Internal_allocator>& other) = default;

            Array_const_reverse_iterator(Array_const_reverse_iterator<T, Internal_allocator>&& other) noexcept = default;
            Array_const_reverse_iterator<T, Internal_allocator>& operator=(Array_const_reverse_iterator<T, Internal_allocator>&& other) noexcept = default;

            ~Array_const_reverse_iterator() = default;

            Array_const_reverse_iterator<T, Internal_allocator>& operator++() noexcept
            {
                auto diff = *gen_;
                diff -= *(--gen_);
                data_ -= diff;
                return *this;
            }

            Array_const_reverse_iterator<T, Internal_allocator> operator++(int) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_const_reverse_iterator<T, Internal_allocator>& operator--() noexcept
            {
                auto diff = *gen_;
                diff = *(++gen_) - diff;
                data_ += diff;
                return *this;
            }

            Array_const_reverse_iterator<T, Internal_allocator> operator--(int) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            [[nodiscard]] const T& operator*() const noexcept
            {
                return *data_;
            }

            [[nodiscard]] bool operator==(const Array_const_reverse_iterator<T, Internal_allocator>& iter) const
            {
                return data_ == iter.data_;
            }

        private:
            Array_indices_generator<Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, template<typename> typename Data_allocator = std::allocator, template<typename> typename Internals_allocator = std::allocator>
        class Array {
        public:
            using Header = Array_header<Internals_allocator>;
            using Subscripts_iterator = Array_subscripts_iterator<Internals_allocator>;

            Array() = default;

            Array(Array<T, Data_allocator, Internals_allocator>&& other) = default;
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array(Array<T_o, Data_allocator_o, Internals_allocator_o>&& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);

                Array<T_o, Data_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
            }
            Array<T, Data_allocator, Internals_allocator>& operator=(Array<T, Data_allocator, Internals_allocator>&& other) & = default;
            Array<T, Data_allocator, Internals_allocator>& operator=(Array<T, Data_allocator, Internals_allocator>&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                if (hdr_.is_subarray() && std::equal(hdr_.dims().begin(), hdr_.dims().end(), other.hdr_.dims().begin(), other.hdr_.dims().end())) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = std::move(other.hdr_);
                buffsp_ = std::move(other.buffsp_);

                return *this;
            }
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_allocator, Internals_allocator>& operator=(Array<T_o, Data_allocator_o, Internals_allocator_o>&& other)&
            {
                *this = Array<T, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                Array<T_o, Data_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_allocator, Internals_allocator>& operator=(Array<T_o, Data_allocator_o, Internals_allocator_o>&& other)&&
            {
                if (hdr_.is_subarray() && std::equal(hdr_.dims().begin(), hdr_.dims().end(), other.header().dims().begin(), other.header().dims().end())) {
                    copy(other, *this);
                }
                Array<T_o, Data_allocator_o, Internals_allocator_o> dummy{std::move(other)};
                return *this;
            }

            Array(const Array<T, Data_allocator, Internals_allocator>& other) = default;
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array(const Array<T_o, Data_allocator_o, Internals_allocator_o>& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);
            }
            Array<T, Data_allocator, Internals_allocator>& operator=(const Array<T, Data_allocator, Internals_allocator>& other) & = default;
            Array<T, Data_allocator, Internals_allocator>& operator=(const Array<T, Data_allocator, Internals_allocator>& other)&&
            {
                if (&other == this) {
                    return *this;
                }
                
                if (hdr_.is_subarray() && std::equal(hdr_.dims().begin(), hdr_.dims().end(), other.hdr_.dims().begin(), other.hdr_.dims().end())) {
                    copy(other, *this);
                    return *this;
                }

                hdr_ = other.hdr_;
                buffsp_ = other.buffsp_;

                return *this;
            }
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_allocator, Internals_allocator>& operator=(const Array<T_o, Data_allocator_o, Internals_allocator_o>& other)&
            {
                *this = Array<T, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                return *this;
            }
            template< typename T_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_allocator, Internals_allocator>& operator=(const Array<T_o, Data_allocator_o, Internals_allocator_o>& other)&&
            {
                if (hdr_.is_subarray() && std::equal(hdr_.dims().begin(), hdr_.dims().end(), other.header().dims().begin(), other.header().dims().end())) {
                    copy(other, *this);
                }
                return *this;
            }

            template <typename U>
            Array<T, Data_allocator, Internals_allocator>& operator=(const U& value)
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

            Array(std::span<const std::int64_t> dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_allocator>>(Internals_allocator<simple_vector<T, Data_allocator>>(), hdr_.count()))
            {
                if (data) {
                    std::copy(data, data + hdr_.count(), buffsp_->data());
                }
            }
            Array(std::span<const std::int64_t> dims, std::initializer_list<T> data)
                : Array(dims, data.begin())
            {
            }
            Array(std::initializer_list<std::int64_t> dims, const T* data = nullptr)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, data)
            {
            }
            Array(std::initializer_list<std::int64_t> dims, std::initializer_list<T> data)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, data.begin())
            {
            }
            template <typename U>
            Array(std::span<const std::int64_t> dims, const U* data = nullptr)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_allocator>>(Internals_allocator < simple_vector<T, Data_allocator>>(), hdr_.count()))
            {
                std::copy(data, data + hdr_.count(), buffsp_->data());
            }
            template <typename U>
            Array(std::span<const std::int64_t> dims, std::initializer_list<U> data)
                : Array(dims, data.begin())
            {
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, const U* data = nullptr)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, data)
            {
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, std::initializer_list<U> data = nullptr)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, data.begin())
            {
            }


            Array(std::span<const std::int64_t> dims, const T& value)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_allocator>>(Internals_allocator < simple_vector<T, Data_allocator>>(), hdr_.count()))
            {
                std::fill(buffsp_->data(), buffsp_->data() + buffsp_->size(), value);
            }
            Array(std::initializer_list<std::int64_t> dims, const T& value)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, value)
            {
            }
            template <typename U>
            Array(std::span<const std::int64_t> dims, const U& value)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_allocator>>(Internals_allocator < simple_vector<T, Data_allocator>>(), hdr_.count()))
            {
                std::fill(buffsp_->data(), buffsp_->data() + buffsp_->size(), value);
            }
            template <typename U>
            Array(std::initializer_list<std::int64_t> dims, const U& value)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, value)
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

            [[nodiscard]] std::span<T> block() const noexcept
            {
                return buffsp_ ? std::span(buffsp_->data(), buffsp_->size()) : std::span<T>();
            }

            [[nodiscard]] T* data() const noexcept
            {
                return block().data();
            }

            [[nodiscard]] const T& operator()(std::span<std::int64_t> subs) const noexcept
            {
                return buffsp_->data()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] const T& operator()(std::initializer_list<std::int64_t> subs) const noexcept
            {
                return (*this)(std::span<std::int64_t>{ const_cast<std::int64_t*>(subs.begin()), subs.size() });
            }

            [[nodiscard]] T& operator()(std::span<std::int64_t> subs) noexcept
            {
                return buffsp_->data()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] T& operator()(std::initializer_list<std::int64_t> subs) noexcept
            {
                return (*this)(std::span<std::int64_t>{ const_cast<std::int64_t*>(subs.begin()), subs.size() });
            }

            [[nodiscard]] Array<T, Data_allocator, Internals_allocator> operator()(std::span<const Interval<std::int64_t>> ranges) const
            {
                if (ranges.empty() || empty(*this)) {
                    return (*this);
                }

                Array<T, Data_allocator, Internals_allocator> slice{};
                slice.hdr_ = Header{ hdr_.dims(), hdr_.strides(), hdr_.offset(), ranges };
                slice.buffsp_ = buffsp_;
                return slice;
            }
            [[nodiscard]] Array<T, Data_allocator, Internals_allocator> operator()(std::initializer_list<Interval<std::int64_t>> ranges) const
            {
                return (*this)(std::span<const Interval<std::int64_t>>{ranges.begin(), ranges.size()});
            }

            [[nodiscard]] Array<T, Data_allocator, Internals_allocator> operator()(const Array<std::int64_t, Data_allocator, Internals_allocator>& indices) const noexcept
            {
                Array<T, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(indices.header().dims().data(), indices.header().dims().size()));

                for (Subscripts_iterator iter({}, indices.header().dims()); iter; ++iter) {
                    res(*iter) = buffsp_->data()[indices(*iter)];
                }

                return res;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_allocator, Internals_allocator>& transform(const Array<T_o, Data_allocator, Internals_allocator>& other, Binary_op&& op)
            {
                if (!std::equal(header().dims().begin(), header().dims().end(), other.header().dims().begin(), other.header().dims().end())) {
                    return *this;
                }

                for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, header().dims()); iter; ++iter) {
                    (*this)(*iter) = op((*this)(*iter), other(*iter));
                }

                return *this;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_allocator, Internals_allocator>& transform(const T_o& other, Binary_op&& op)
            {
                for (typename Array<T_o, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, header().dims()); iter; ++iter) {
                    (*this)(*iter) = op((*this)(*iter), other);
                }

                return *this;
            }

            Array_iterator<T, Internals_allocator> begin()
            {
                return Array_iterator<T, Internals_allocator>(buffsp_->data() + hdr_.offset(), Array_indices_generator<Internals_allocator>(hdr_));
            }

            Array_iterator<T, Internals_allocator> end()
            {
                return Array_iterator<T, Internals_allocator>(buffsp_->data() + *(++Array_indices_generator<Internals_allocator>(hdr_, true)));
            }


            Array_const_iterator<T, Internals_allocator> cbegin() const
            {
                return Array_const_iterator<T, Internals_allocator>(buffsp_->data() + hdr_.offset(), Array_indices_generator<Internals_allocator>(hdr_));
            }

            Array_const_iterator<T, Internals_allocator> cend() const
            {
                return Array_const_iterator<T, Internals_allocator>(buffsp_->data() + *(++Array_indices_generator<Internals_allocator>(hdr_, true)));
            }


            auto rbegin()
            {
                return Array_reverse_iterator<T, Internals_allocator>(buffsp_->data() + hdr_.last_index(), Array_indices_generator<Internals_allocator>(hdr_, true));
            }

            auto rend()
            {
                return Array_reverse_iterator<T, Internals_allocator>(&(buffsp_->data()[*(--Array_indices_generator<Internals_allocator>(hdr_))]));
            }

            auto crbegin() const
            {
                return Array_const_reverse_iterator<T, Internals_allocator>(buffsp_->data() + hdr_.last_index(), Array_indices_generator<Internals_allocator>(hdr_, true));
            }

            auto crend() const
            {
                return Array_const_reverse_iterator<T, Internals_allocator>(&(buffsp_->data()[*(--Array_indices_generator<Internals_allocator>(hdr_))]));
            }

        private:
            Header hdr_{};
            std::shared_ptr<simple_vector<T, Data_allocator>> buffsp_{ nullptr };
        };

        /**
        * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
        */
        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline void copy(const Array<T1, Data_allocator, Internals_allocator>& src, Array<T2, Data_allocator, Internals_allocator>& dst)
        {
            if (empty(src) || empty(dst)) {
                return;
            }

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator src_iter({}, src.header().dims()); src_iter; ++src_iter) {
                dst(*src_iter) = src(*src_iter);
            }
        }
        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline void copy(const Array<T1, Data_allocator, Internals_allocator>& src, Array<T2, Data_allocator, Internals_allocator>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> clone(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> clone(std::span<const std::int64_t>(arr.header().dims().data(), arr.header().dims().size()));

            for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                clone(*iter) = arr(*iter);
            }

            return clone;
        }

        /**
        * @note Returning a reference to the input array, except in case of resulted empty array or an input subarray.
        */
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> reshape(const Array<T, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().count() != numel(new_dims)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            if (arr.header().is_subarray()) {
                Array<T, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

                typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
                typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, new_dims);

                while (arr_iter && res_iter) {
                    res(*res_iter) = arr(*arr_iter);
                    ++arr_iter;
                    ++res_iter;
                }

                return res;
            }

            typename Array<T, Data_allocator, Internals_allocator>::Header new_header(new_dims);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> res(arr);
            res.header() = std::move(new_header);

            return res;
        }
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> reshape(const Array<T, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> resize(const Array<T, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            if (numel(new_dims) <= 0) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, new_dims);

            while (arr_iter && res_iter) {
                res(*res_iter) = arr(*arr_iter);
                ++arr_iter;
                ++res_iter;
            }

            return res;
        }
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> resize(const Array<T, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Internals_allocator> append(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_allocator, Internals_allocator> res(resize(lhs, { lhs.header().count() + rhs.header().count() }));

            Array<T2, Data_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

            for (std::int64_t i = lhs.header().count(); i < res.header().count(); ++i) {
                res({ i }) = rhs({ i - lhs.header().count() });
            }

            return res;
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Internals_allocator> append(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_allocator, Internals_allocator>::Header new_header(lhs.header().dims(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_allocator, Internals_allocator>{};
            }

            Array<T1, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator lhs_iter({}, lhs.header().dims());
            typename Array<T2, Data_allocator, Internals_allocator>::Subscripts_iterator rhs_iter({}, rhs.header().dims());
            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t fixed_axis{ modulo(axis, std::ssize(lhs.header().dims())) };

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

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Internals_allocator> insert(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, std::int64_t ind)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });

            Array<T1, Data_allocator, Internals_allocator> rlhs(reshape(lhs, { lhs.header().count() }));
            Array<T2, Data_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

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

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Internals_allocator> insert(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, std::int64_t ind, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_allocator, Internals_allocator>::Header new_header(lhs.header().dims(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_allocator, Internals_allocator>();
            }

            Array<T1, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator lhs_iter({}, lhs.header().dims());
            typename Array<T2, Data_allocator, Internals_allocator>::Subscripts_iterator rhs_iter({}, rhs.header().dims());
            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            std::int64_t fixed_axis{ modulo(axis, std::ssize(lhs.header().dims())) };
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
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> remove(const Array<T, Data_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            std::int64_t fixed_ind{ modulo(ind, arr.header().count()) };
            std::int64_t fixed_count{ fixed_ind + count < arr.header().count() ? count : (arr.header().count() - fixed_ind) };

            Array<T, Data_allocator, Internals_allocator> res({ arr.header().count() - fixed_count });
            Array<T, Data_allocator, Internals_allocator> rarr(reshape(arr, { arr.header().count() }));

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
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> remove(const Array<T, Data_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            std::int64_t fixed_axis{ modulo(axis, std::ssize(arr.header().dims())) };
            std::int64_t fixed_ind{ modulo(ind, arr.header().dims()[fixed_axis]) };
            std::int64_t fixed_count{ fixed_ind + count <= arr.header().dims()[fixed_axis] ? count : (arr.header().dims()[fixed_axis] - fixed_ind) };

            typename Array<T, Data_allocator, Internals_allocator>::Header new_header(arr.header().dims(), -fixed_count, fixed_axis);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> res({ arr.header().count() - (arr.header().count() / arr.header().dims()[fixed_axis]) * fixed_count });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            while (arr_iter) {
                if (res_iter && ((*arr_iter)[fixed_axis] < fixed_ind || (*arr_iter)[fixed_axis] >= fixed_ind + fixed_count)) {
                    res(*res_iter) = arr(*arr_iter);
                    ++res_iter;
                }
                ++arr_iter;
            }

            return res;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool empty(const Array<T, Data_allocator, Internals_allocator>& arr) noexcept
        {
            return (arr.block().empty() || arr.header().is_subarray()) && arr.header().empty();
        }

        template <typename T, typename Unary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>    
        [[nodiscard]] inline auto transform(const Array<T, Data_allocator, Internals_allocator>& arr, Unary_op&& op)
            -> Array<Replace_with_char_if_bool<decltype(op(arr.data()[0]))>, Data_allocator, Internals_allocator>
        {
            using T_o = Replace_with_char_if_bool<decltype(op(arr.data()[0]))>;

            if (empty(arr)) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(arr.header().dims().data(), arr.header().dims().size()));

            for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                res(*iter) = op(arr(*iter));
            }

            return res;
        }

        template <typename T, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Internals_allocator>& arr, Binary_op&& op)
            -> decltype(op(arr.data()[0], arr.data()[0]))
        {
            using T_o = decltype(op(arr.data()[0], arr.data()[0]));

            if (empty(arr)) {
                return T_o{};
            }

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter{ {}, arr.header().dims() };

            T_o res{ static_cast<T_o>(arr(*iter)) };
            ++iter;

            while (iter) {
                res = op(res, arr(*iter));
                ++iter;
            }

            return res;
        }

        template <typename T, typename T_o, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Internals_allocator>& arr, const T_o& init_value, Binary_op&& op)
            -> decltype(op(init_value, arr.data()[0]))
        {
            if (empty(arr)) {
                return init_value;
            }

            T_o res{ init_value };
            for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter{ {}, arr.header().dims() }; iter; ++iter) {
                res = op(res, arr(*iter));
            }

            return res;
        }

        template <typename T, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Internals_allocator>& arr, Binary_op&& op, std::int64_t axis)
            -> Array<Replace_with_char_if_bool<decltype(op(arr.data()[0], arr.data()[0]))>, Data_allocator, Internals_allocator>
        {
            using T_o = Replace_with_char_if_bool<decltype(op(arr.data()[0], arr.data()[0]))>;

            if (empty(arr)) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            typename Array<T_o, Data_allocator, Internals_allocator>::Header new_header(arr.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), axis);
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            const std::int64_t reduction_iteration_cycle{ arr.header().dims()[modulo(axis, std::ssize(arr.header().dims()))] };

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

        template <typename T, typename T_o, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_allocator, Internals_allocator>& arr, const Array<T_o, Data_allocator, Internals_allocator>& init_values, Binary_op&& op, std::int64_t axis)
            -> Array<Replace_with_char_if_bool<decltype(op(init_values.data()[0], arr.data()[0]))>, Data_allocator, Internals_allocator>
        {
            if (empty(arr)) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            const std::int64_t fixed_axis{ modulo(axis, std::ssize(arr.header().dims())) };

            if (init_values.header().dims().size() != 1 && init_values.header().dims()[fixed_axis] != arr.header().dims()[fixed_axis]) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            typename Array<T_o, Data_allocator, Internals_allocator>::Header new_header(arr.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), axis);
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator init_iter({}, init_values.header().dims());

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

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<char>(a) && static_cast<char>(b); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> all(const Array<T, Data_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<char>(a) && static_cast<char>(b); }, axis);
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool any(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<char>(a) || static_cast<char>(b); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> any(const Array<T, Data_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return static_cast<char>(a) || static_cast<char>(b); }, axis);
        }

        template <typename T1, typename T2, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<Replace_with_char_if_bool<decltype(op(lhs.data()[0], rhs.data()[0]))>, Data_allocator, Internals_allocator>
        {
            using T_o = Replace_with_char_if_bool<decltype(op(lhs.data()[0], rhs.data()[0]))>;
            
            if (!std::equal(lhs.header().dims().begin(), lhs.header().dims().end(), rhs.header().dims().begin(), rhs.header().dims().end())) {
                return Array<T_o, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(lhs.header().dims().data(), lhs.header().dims().size()));

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs(*iter), rhs(*iter));
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_op&& op)
            -> Array<Replace_with_char_if_bool<decltype(op(lhs.data()[0], rhs))>, Data_allocator, Internals_allocator>
        {
            using T_o = Replace_with_char_if_bool<decltype(op(lhs.data()[0], rhs))>;

            Array<T_o, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(lhs.header().dims().data(), lhs.header().dims().size()));

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs(*iter), rhs);
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<Replace_with_char_if_bool<decltype(op(lhs, rhs.data()[0]))>, Data_allocator, Internals_allocator>
        {
            using T_o = Replace_with_char_if_bool<decltype(op(lhs, rhs.data()[0]))>;

            Array<T_o, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(rhs.header().dims().data(), rhs.header().dims().size()));

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, rhs.header().dims()); iter; ++iter) {
                res(*iter) = op(lhs, rhs(*iter));
            }

            return res;
        }

        template <typename T, typename Unary_pred, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> filter(const Array<T, Data_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

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
                return Array<T, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_allocator, Internals_allocator> filter(const Array<T1, Data_allocator, Internals_allocator>& arr, const Array<T2, Data_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<T1, Data_allocator, Internals_allocator>();
            }

            if (!std::equal(arr.header().dims().begin(), arr.header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                return Array<T1, Data_allocator, Internals_allocator>();
            }

            Array<T1, Data_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T2, Data_allocator, Internals_allocator>::Subscripts_iterator mask_iter({}, mask.header().dims());

            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

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
                return Array<T1, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Unary_pred, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_allocator, Internals_allocator> find(const Array<T, Data_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<std::int64_t, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

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
                return Array<std::int64_t, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_allocator, Internals_allocator> find(const Array<T1, Data_allocator, Internals_allocator>& arr, const Array<T2, Data_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_allocator, Internals_allocator>();
            }

            if (!std::equal(arr.header().dims().begin(), arr.header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                return Array<std::int64_t, Data_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_allocator, Internals_allocator> res({ arr.header().count() });

            typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims());
            typename Array<T2, Data_allocator, Internals_allocator>::Subscripts_iterator mask_iter({}, mask.header().dims());

            typename Array<std::int64_t, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

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
                return Array<std::int64_t, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> transpose(const Array<T, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> order)
        {
            if (empty(arr)) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            typename Array<T, Data_allocator, Internals_allocator>::Header new_header(arr.header().dims(), order);
            if (new_header.empty()) {
                return Array<T, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_allocator, Internals_allocator> res({ arr.header().count() });
            res.header() = std::move(new_header);

            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator arr_iter({}, arr.header().dims(), order);
            typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator res_iter({}, res.header().dims());

            while (arr_iter && res_iter) {
                res(*res_iter) = arr(*arr_iter);
                ++arr_iter;
                ++res_iter;
            }

            return res;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_allocator, Internals_allocator> transpose(const Array<T, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> order)
        {
            return transpose(arr, std::span<const std::int64_t>(order.begin(), order.size() ));
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator==(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator==(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator==(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator!=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator!=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator!=(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> close(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> close(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> close(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator>=(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<=(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<char, Data_allocator, Internals_allocator> operator<=(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator+=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator+=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator-=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator-=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator*=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator*=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator/=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator/=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator%(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator%(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator%(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator%=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator%=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator^=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator^=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator&=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator&=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator|=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator|=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
            -> Array<Replace_with_char_if_bool<decltype(lhs << rhs.data()[0])>, Data_allocator, Internals_allocator>
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator~(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return ~a; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator!(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return !a; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return +a; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return -a; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto abs(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return abs(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto acos(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acos(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto acosh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acosh(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto asin(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asin(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto asinh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asinh(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto atan(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atan(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto atanh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atanh(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto cos(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cos(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto cosh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cosh(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto exp(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return exp(a); });
        }
        
        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto log(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto log10(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log10(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto pow(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return pow(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sin(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sin(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sinh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sinh(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sqrt(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sqrt(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto tan(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tan(a); });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto tanh(const Array<T, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tanh(a); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator++(Array<T, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                ++arr(*iter);
            }
            return arr;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_allocator, Internals_allocator>&& arr)
        {
            return operator++(arr);
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator++(Array<T, Data_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_allocator, Internals_allocator> old = clone(arr);
            operator++(arr);
            return old;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_allocator, Internals_allocator>&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator--(Array<T, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename Array<T, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, arr.header().dims()); iter; ++iter) {
                --arr(*iter);
            }
            return arr;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_allocator, Internals_allocator>&& arr)
        {
            return operator--(arr);
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator--(Array<T, Data_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_allocator, Internals_allocator> old = clone(arr);
            operator--(arr);
            return old;
        }

        template <typename T, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_allocator, Internals_allocator>&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <typename T1, typename T2, typename Binary_pred, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, Binary_pred pred)
        {
            if (empty(lhs) && empty(rhs)) {
                return true;
            }

            if (empty(lhs) || empty(rhs)) {
                return false;
            }

            if (!std::equal(lhs.header().dims().begin(), lhs.header().dims().end(), rhs.header().dims().begin(), rhs.header().dims().end())) {
                return false;
            }

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                if (!pred(lhs(*iter), rhs(*iter))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_pred pred)
        {
            if (empty(lhs)) {
                return true;
            }

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, lhs.header().dims()); iter; ++iter) {
                if (!pred(lhs(*iter), rhs)) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, Binary_pred pred)
        {
            if (empty(rhs)) {
                return true;
            }

            for (typename Array<T1, Data_allocator, Internals_allocator>::Subscripts_iterator iter({}, rhs.header().dims()); iter; ++iter) {
                if (!pred(lhs, rhs(*iter))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const T1& lhs, const Array<T2, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
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
