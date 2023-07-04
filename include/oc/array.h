#ifndef OC_ARRAY_H
#define OC_ARRAY_H

#include <cstdint>
#include <memory>
#include <initializer_list>
#include <stdexcept>
#include <span>
#include <limits>
#include <algorithm>
#include <numeric>
#include <variant>
#include <sstream>
#include <cmath>

namespace oc {

    namespace details {

        template <typename T>
        requires (!std::is_reference_v<T>)
            class lightweight_allocator {
            public:
                using value_type = T;
                using pointer = T*;
                using const_pointer = const T*;
                using reference = T&;
                using const_reference = const T&;
                using size_type = std::int64_t;
                using difference_type = std::int64_t;

                constexpr lightweight_allocator() = default;
                constexpr lightweight_allocator(const lightweight_allocator& other) = default;
                constexpr lightweight_allocator& operator=(const lightweight_allocator& other) = default;
                constexpr lightweight_allocator(lightweight_allocator&& other) = default;
                constexpr lightweight_allocator& operator=(lightweight_allocator&& other) = default;
                constexpr ~lightweight_allocator() = default;

                template <typename U>
                requires (!std::is_reference_v<U>)
                    constexpr lightweight_allocator(const lightweight_allocator<U>&) noexcept {}

                [[nodiscard]] constexpr pointer allocate(size_type n)
                {
                    return n == 0 ? nullptr : reinterpret_cast<pointer>(operator new[](n * sizeof(value_type)));
                }

                constexpr void deallocate(pointer p, size_type n) noexcept
                {
                    if (p && n > 0) {
                        operator delete[](p, n * sizeof(value_type));
                    }
                }
        };

        template <typename T, template<typename> typename Allocator = lightweight_allocator>
        requires (std::is_copy_constructible_v<T>&& std::is_copy_assignable_v<T>)
            class simple_dynamic_vector final {
            public:
                using value_type = T;
                using allocator_type = Allocator<T>;
                using size_type = std::int64_t;
                using difference_type = std::int64_t;
                using reference = T&;
                using const_reference = const T&;
                using pointer = T*;
                using const_pointer = const T*;
                using iterator = T*;
                using const_iterator = const T*;

                template <typename U>
                using replaced_type = simple_dynamic_vector<U, Allocator>;

                constexpr simple_dynamic_vector(size_type size = 0, const_pointer data = nullptr)
                    : size_(size), capacity_(size)
                {
                    data_ptr_ = alloc_.allocate(capacity_);
                    if (data) {
                        std::uninitialized_copy_n(data, size_, data_ptr_);
                    }
                    else if constexpr (!std::is_fundamental_v<T>) {
                        std::uninitialized_default_construct_n(data_ptr_, size_);
                    }
                }

                template <typename InputIt>
                constexpr simple_dynamic_vector(InputIt first, InputIt last)
                {
                    size_ = capacity_ = last - first;
                    data_ptr_ = alloc_.allocate(capacity_);
                    std::uninitialized_copy_n(first, size_, data_ptr_);
                }

                constexpr simple_dynamic_vector(const simple_dynamic_vector& other)
                    : alloc_(other.alloc_), size_(other.size_), capacity_(other.capacity_)
                {
                    data_ptr_ = alloc_.allocate(capacity_);
                    std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);
                }

                constexpr simple_dynamic_vector operator=(const simple_dynamic_vector& other)
                {
                    if (this == &other) {
                        return *this;
                    }

                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_, size_);
                    }
                    alloc_.deallocate(data_ptr_, capacity_);

                    alloc_ = other.alloc_;
                    size_ = other.size_;
                    capacity_ = other.capacity_;

                    data_ptr_ = alloc_.allocate(capacity_);
                    std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);

                    return *this;
                }

                constexpr simple_dynamic_vector(simple_dynamic_vector&& other) noexcept
                    : alloc_(std::move(other.alloc_)), size_(other.size_), capacity_(other.capacity_)
                {
                    data_ptr_ = other.data_ptr_;

                    other.data_ptr_ = nullptr;
                    other.size_ = 0;
                }

                constexpr simple_dynamic_vector operator=(simple_dynamic_vector&& other) noexcept
                {
                    if (this == &other) {
                        return *this;
                    }

                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_, size_);
                    }
                    alloc_.deallocate(data_ptr_, capacity_);

                    alloc_ = std::move(other.alloc_);
                    size_ = other.size_;
                    capacity_ = other.capacity_;

                    data_ptr_ = other.data_ptr_;

                    other.data_ptr_ = nullptr;
                    other.size_ = 0;

                    return *this;
                }

                constexpr ~simple_dynamic_vector() noexcept
                {
                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_, size_);
                    }
                    alloc_.deallocate(data_ptr_, capacity_);
                }

                [[nodiscard]] constexpr bool empty() const noexcept
                {
                    return size_ == 0 || !data_ptr_;
                }

                [[nodiscard]] constexpr size_type size() const noexcept
                {
                    return size_;
                }

                [[nodiscard]] constexpr size_type capacity() const noexcept
                {
                    return capacity_;
                }

                [[nodiscard]] constexpr pointer data() const noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr reference operator[](size_type index) noexcept
                {
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    return data_ptr_[index];
                }

                constexpr void resize(size_type new_size)
                {
                    if (new_size < size_) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_ + new_size, size_ - new_size);
                        }
                        size_ = new_size;
                    }
                    //else if (new_size == size_) { /* do nothing */ }
                    else if (new_size > size_) {
                        size_type new_capacity = new_size;
                        pointer new_data_ptr = alloc_.allocate(new_capacity);
                        std::uninitialized_move_n(data_ptr_, size_, new_data_ptr);
                        std::uninitialized_default_construct_n(new_data_ptr + size_, new_size - size_);

                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_, size_);
                        }
                        alloc_.deallocate(data_ptr_, capacity_);

                        data_ptr_ = new_data_ptr;
                        size_ = new_size;
                        capacity_ = new_capacity;
                    }
                }

                constexpr void reserve(size_type new_capacity)
                {
                    // if (new_capacity <= capacity_) do nothing
                    if (new_capacity > capacity_) {
                        pointer new_data_ptr = alloc_.allocate(new_capacity);
                        std::uninitialized_move_n(data_ptr_, size_, new_data_ptr);

                        alloc_.deallocate(data_ptr_, capacity_);
                        data_ptr_ = new_data_ptr;
                        capacity_ = new_capacity;
                    }
                }

                constexpr void expand(size_type count)
                {
                    if (size_ + count < capacity_) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::uninitialized_default_construct_n(data_ptr_ + size_, count);
                        }
                        size_ += count;
                    }
                    else if (size_ + count >= capacity_) {
                        size_type new_capacity = static_cast<size_type>(1.5 * (size_ + count));
                        size_type new_size = size_ + count;
                        pointer data_ptr = alloc_.allocate(new_capacity);
                        std::uninitialized_move_n(data_ptr_, size_, data_ptr);
                        std::uninitialized_default_construct_n(data_ptr + size_, count);

                        alloc_.deallocate(data_ptr_, capacity_);
                        data_ptr_ = data_ptr;
                        capacity_ = new_capacity;
                        size_ = new_size;
                    }
                }

                constexpr void shrink(size_type count)
                {
                    if (count > size_) {
                        throw std::length_error("count > size_");
                    }

                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_ + size_ - count, count);
                    }
                    size_ -= count;
                }

                constexpr void shrink_to_fit()
                {
                    if (capacity_ > size_) {
                        pointer data_ptr = alloc_.allocate(size_);
                        std::uninitialized_move_n(data_ptr_, size_, data_ptr);

                        alloc_.deallocate(data_ptr_, capacity_);
                        data_ptr_ = data_ptr;
                        capacity_ = size_;
                    }
                }

                [[nodiscard]] constexpr pointer begin() noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr pointer end() noexcept
                {
                    return data_ptr_ + size_;
                }

                [[nodiscard]] constexpr const_reference back() const noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr reference back() noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr const_reference front() const noexcept
                {
                    return data_ptr_[0];
                }

                [[nodiscard]] constexpr reference front() noexcept
                {
                    return data_ptr_[0];
                }

            private:
                pointer data_ptr_;

                size_type size_;
                size_type capacity_;

                allocator_type alloc_;
        };



        template <typename T, std::int64_t Capacity>
        requires (std::is_copy_constructible_v<T>&& std::is_copy_assignable_v<T>)
            class simple_static_vector final {
            public:
                using value_type = T;
                using size_type = std::int64_t;
                using difference_type = std::int64_t;
                using reference = T&;
                using const_reference = const T&;
                using pointer = T*;
                using const_pointer = const T*;
                using iterator = T*;
                using const_iterator = const T*;

                template <typename U>
                using replaced_type = simple_static_vector<U, Capacity>;

                constexpr simple_static_vector(size_type size = 0, const_pointer data = nullptr)
                    : size_(size)
                {
                    if (size_ > Capacity) {
                        throw std::length_error("size_ > Capacity");
                    }
                    if (data) {
                        std::copy(data, data + size_, data_ptr_);
                    }
                }

                template <typename InputIt>
                constexpr simple_static_vector(InputIt first, InputIt last)
                    : simple_static_vector(last - first, &(*first)) {}

                constexpr simple_static_vector(const simple_static_vector& other)
                    : size_(other.size_)
                {
                    std::copy(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);
                }

                constexpr simple_static_vector operator=(const simple_static_vector& other)
                {
                    if (this == &other) {
                        return *this;
                    }

                    size_ = other.size_;

                    std::copy(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);

                    return *this;
                }

                constexpr simple_static_vector(simple_static_vector&& other) noexcept
                    : size_(other.size_)
                {
                    std::move(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);

                    other.size_ = 0;
                }

                constexpr simple_static_vector operator=(simple_static_vector&& other) noexcept
                {
                    if (this == &other) {
                        return *this;
                    }

                    size_ = other.size_;

                    std::move(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);

                    other.size_ = 0;

                    return *this;
                }

                [[nodiscard]] constexpr bool empty() const noexcept
                {
                    return size_ == 0;
                }

                [[nodiscard]] constexpr size_type size() const noexcept
                {
                    return size_;
                }

                [[nodiscard]] constexpr size_type capacity() const noexcept
                {
                    return Capacity;
                }

                [[nodiscard]] constexpr pointer data() const noexcept
                {
                    return const_cast<pointer>(data_ptr_);
                }

                [[nodiscard]] constexpr reference operator[](size_type index) noexcept
                {
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    return data_ptr_[index];
                }

                constexpr void resize(size_type new_size)
                {
                    if (new_size > Capacity) {
                        throw std::length_error("new_size > Capacity");
                    }
                    if (new_size < size_) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_ + new_size, size_ - new_size);
                        }
                        size_ = new_size;
                    }
                    //else if (new_size == size_) { /* do nothing */ }
                    else if (new_size > size_) {
                        size_ = new_size;
                    }
                }

                constexpr void reserve(size_type new_capacity)
                {
                    // noop
                }

                constexpr void expand(size_type count)
                {
                    if (size_ + count > Capacity) {
                        throw std::length_error("size_ + count > Capacity");
                    }
                    size_ += count;
                }

                constexpr void shrink(size_type count)
                {
                    if (count > size_) {
                        throw std::length_error("count > size_");
                    }

                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_ + size_ - count, count);
                    }
                    size_ -= count;
                }

                constexpr void shrink_to_fit()
                {
                    // noop
                }

                [[nodiscard]] constexpr pointer begin() noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr pointer end() noexcept
                {
                    return data_ptr_ + size_;
                }

                [[nodiscard]] constexpr const_reference back() const noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr reference back() noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr const_reference front() const noexcept
                {
                    return data_ptr_[0];
                }

                [[nodiscard]] constexpr reference front() noexcept
                {
                    return data_ptr_[0];
                }

            private:
                value_type data_ptr_[Capacity];

                size_type size_;
        };
    }


    namespace details {
        template <typename T>
        [[nodiscard]] inline constexpr T default_atol() noexcept
        {
            return T{};
        }

        template <std::integral T>
        [[nodiscard]] inline constexpr T default_atol() noexcept
        {
            return T{ 0 };
        }

        template <std::floating_point T>
        [[nodiscard]] inline constexpr T default_atol() noexcept
        {
            return T{ 1e-8 };
        }

        template <typename T>
        [[nodiscard]] inline constexpr T default_rtol() noexcept
        {
            return T{};
        }

        template <std::integral T>
        [[nodiscard]] inline constexpr T default_rtol() noexcept
        {
            return T{ 0 };
        }

        template <std::floating_point T>
        [[nodiscard]] inline constexpr T default_rtol() noexcept
        {
            return T{ 1e-5 };
        }

        template <typename T1, typename T2>
        [[nodiscard]] inline constexpr bool close(const T1& a, const T2& b, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ()) noexcept
        {
            const decltype(a - b) reps{ rtol * (abs(a) > abs(b) ? abs(a) : abs(b)) };
            return abs(a - b) <= (atol > reps ? atol : reps);
        }

        template <std::integral T1, std::integral T2>
        [[nodiscard]] inline constexpr auto modulo(const T1& value, const T2& modulus) noexcept -> decltype((value% modulus) + modulus)
        {
            return ((value % modulus) + modulus) % modulus;
        }
    }

    using details::default_atol;
    using details::default_rtol;

    using details::close;
    using details::modulo;



    namespace details {
        template <std::integral T = std::int64_t>
        struct Interval {
            Interval(const T& nstart, const T& nstop, const T& nstep) noexcept
                : start(nstart), stop(nstop), step(nstep) {}

            Interval(const T& nstart, const T& nstop) noexcept
                : Interval(nstart, nstop, 1) {}

            Interval(const T& nstart) noexcept
                : Interval(nstart, nstart, 1) {}

            Interval() = default;
            Interval(const Interval&) = default;
            Interval& operator=(const Interval&) = default;
            Interval(Interval&) = default;
            Interval& operator=(Interval&) = default;

            T start{ 0 };
            T stop{ 0 };
            T step{ 1 };
        };

        template <std::integral T>
        [[nodiscard]] inline Interval<T> reverse(const Interval<T>& i) noexcept
        {
            return { i.stop, i.start, -i.step };
        }

        template <std::integral T>
        [[nodiscard]] inline Interval<T> modulo(const Interval<T>& i, const T& modulus) noexcept
        {
            return { modulo(i.start, modulus), modulo(i.stop, modulus), i.step };
        }

        template <std::integral T>
        [[nodiscard]] inline Interval<T> forward(const Interval<T>& i) noexcept
        {
            return i.step < T{ 0 } ? reverse(i) : i;
        }
    }

    using details::Interval;

    using details::modulo;

    using details::reverse;
    using details::forward;




    namespace details {

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

        template <typename Storage = simple_dynamic_vector<std::int64_t>>
        class arrnd_header {
        public:
            using storage_type = Storage;

            arrnd_header() = default;

            arrnd_header(std::span<const std::int64_t> dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = storage_type(dims.begin(), dims.end());

                strides_ = storage_type(dims.size());
                compute_strides(dims, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            arrnd_header(const arrnd_header& previous_hdr, std::span<const Interval<std::int64_t>> intervals)
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                if (compute_dims(previous_hdr.dims(), intervals, dims) <= 0) {
                    return;
                }

                dims_ = std::move(dims);
                
                count_ = numel(dims_);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(previous_hdr.dims(), previous_hdr.strides(), intervals, strides_);

                offset_ = compute_offset(previous_hdr.dims(), previous_hdr.offset(), previous_hdr.strides(), intervals);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });

                is_subarray_ = previous_hdr.is_subarray() || !std::equal(previous_hdr.dims().begin(), previous_hdr.dims().end(), dims_.begin());
            }

            arrnd_header(const arrnd_header& previous_hdr, std::int64_t omitted_axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                std::int64_t axis{ modulo(omitted_axis, std::ssize(previous_hdr.dims())) };
                std::int64_t ndims{ std::ssize(previous_hdr.dims()) > 1 ? std::ssize(previous_hdr.dims()) - 1 : 1 };

                dims_ = storage_type(ndims);

                if (previous_hdr.dims().size() > 1) {
                    for (std::int64_t i = 0; i < axis; ++i) {
                        dims_[i] = previous_hdr.dims()[i];
                    }
                    for (std::int64_t i = axis + 1; i < previous_hdr.dims().size(); ++i) {
                        dims_[i - 1] = previous_hdr.dims()[i];
                    }
                }
                else {
                    dims_[0] = 1;
                }

                strides_ = storage_type(ndims);
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            arrnd_header(const arrnd_header& previous_hdr, std::span<const std::int64_t> new_order)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                if (new_order.empty()) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                for (std::int64_t i = 0; i < std::ssize(previous_hdr.dims()); ++i) {
                    dims[i] = previous_hdr.dims()[modulo(new_order[i], std::ssize(previous_hdr.dims()))];
                }

                if (numel(previous_hdr.dims()) != numel(dims)) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            arrnd_header(const arrnd_header& previous_hdr, std::int64_t count, std::int64_t axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_hdr.dims())) };
                for (std::int64_t i = 0; i < previous_hdr.dims().size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + count;
                }
                
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            arrnd_header(const arrnd_header& previous_hdr, std::span<const std::int64_t> appended_dims, std::int64_t axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (previous_hdr.dims().size() != appended_dims.size()) {
                    return;
                }

                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                if (numel(appended_dims) <= 0) {
                    return;
                }

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_hdr.dims())) };

                bool are_dims_valid_for_append{ true };
                for (std::int64_t i = 0; i < previous_hdr.dims().size(); ++i) {
                    if (i != fixed_axis && previous_hdr.dims()[i] != appended_dims[i]) {
                        are_dims_valid_for_append = false;
                    }
                }
                if (!are_dims_valid_for_append) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                for (std::int64_t i = 0; i < previous_hdr.dims().size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + appended_dims[fixed_axis];
                }

                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            arrnd_header(arrnd_header&& other) = default;
            arrnd_header& operator=(arrnd_header&& other) = default;

            arrnd_header(const arrnd_header& other) = default;
            arrnd_header& operator=(const arrnd_header& other) = default;

            virtual ~arrnd_header() = default;

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
            storage_type dims_{};
            storage_type strides_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            std::int64_t last_index_{ 0 };
            bool is_subarray_{ false };
        };


        template <typename Storage = simple_dynamic_vector<std::int64_t>, typename Header = arrnd_header<>>
        class arrnd_general_indexer final
        {
        public:
            using storage_type = Storage;
            using header_type = Header;

            constexpr arrnd_general_indexer(const header_type& hdr, bool backward = false)
                : arrnd_general_indexer(hdr, std::span<const std::int64_t>{}, backward)
            {
            }

            constexpr arrnd_general_indexer(const header_type& hdr, std::int64_t axis, bool backward = false)
                : arrnd_general_indexer(hdr, order_from_major_axis(hdr.dims().size(), axis), backward)
            {
            }

            constexpr arrnd_general_indexer(const header_type& hdr, std::span<const std::int64_t> order, bool backward = false)
                : dims_(hdr.dims().begin(), hdr.dims().end()), strides_(hdr.strides().begin(), hdr.strides().end())
            {
                if (!order.empty()) {
                    dims_ = reorder(dims_, order);
                    strides_ = reorder(strides_, order);
                }
                std::tie(dims_, strides_) = reduce_dimensions(dims_, strides_);

                first_index_ = hdr.offset();
                last_index_ = hdr.last_index();
                last_first_diff_ = last_index_ - first_index_;

                ndims_ = dims_.size();

                if (ndims_ > 0) {
                    first_dim_ = dims_[ndims_ - 1];
                    first_stride_ = strides_[ndims_ - 1];
                    first_ind_ = backward ? first_dim_ - 1 : 0;
                }

                if (ndims_ > 1) {
                    second_dim_ = dims_[ndims_ - 2];
                    second_stride_ = strides_[ndims_ - 2];
                    second_ind_ = backward ? second_dim_ - 1 : 0;
                }

                if (ndims_ > 2) {
                    third_dim_ = dims_[ndims_ - 3];
                    third_stride_ = strides_[ndims_ - 3];
                    third_ind_ = backward ? third_dim_ - 1 : 0;
                }

                if (ndims_ > 3) {
                    indices_.resize(ndims_ - 3);
                    for (std::int64_t i = 0; i < indices_.size(); ++i) {
                        indices_[i] = backward ? dims_[i] - 1 : 0;
                    }
                }

                current_index_ = backward ? last_index_ : first_index_;
            }

            constexpr arrnd_general_indexer() = default;

            constexpr arrnd_general_indexer(const arrnd_general_indexer& other) = default;
            constexpr arrnd_general_indexer& operator=(const arrnd_general_indexer& other) = default;

            constexpr arrnd_general_indexer(arrnd_general_indexer&& other) noexcept = default;
            constexpr arrnd_general_indexer& operator=(arrnd_general_indexer&& other) noexcept = default;

            constexpr ~arrnd_general_indexer() = default;

            constexpr arrnd_general_indexer& operator++() noexcept
            {
                if (current_index_ < first_index_) {
                    current_index_ = first_index_;
                    return *this;
                }
                if (current_index_ >= last_index_) {
                    current_index_ = last_index_ + 1;
                    return *this;
                }
                ++first_ind_;
                current_index_ += first_stride_;
                if (first_ind_ < first_dim_) {
                    return *this;
                }
                current_index_ -= first_ind_ * first_stride_;
                first_ind_ = 0;
                if (ndims_ > 1) {
                    ++second_ind_;
                    current_index_ += second_stride_;
                    if (second_ind_ < second_dim_) {
                        return *this;
                    }
                    current_index_ -= second_ind_ * second_stride_;
                    second_ind_ = 0;
                }
                if (ndims_ > 2) {
                    ++third_ind_;
                    current_index_ += third_stride_;
                    if (third_ind_ < third_dim_) {
                        return *this;
                    }
                    current_index_ -= third_ind_ * third_stride_;
                    third_ind_ = 0;
                }
                for (std::int64_t i = ndims_ - 4; i >= 0; --i) {
                    ++indices_[i];
                    current_index_ += strides_[i];
                    if (indices_[i] < dims_[i]) {
                        return *this;
                    }
                    current_index_ -= indices_[i] * strides_[i];
                    indices_[i] = 0;
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator++(int) noexcept
            {
                arrnd_general_indexer<storage_type, header_type> temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_general_indexer& operator+=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            arrnd_general_indexer operator+(std::int64_t count) noexcept
            {
                arrnd_general_indexer<storage_type, header_type> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_general_indexer& operator--() noexcept
            {
                if (current_index_ <= first_index_) {
                    current_index_ = first_index_ - 1;
                    return *this;
                }
                if (current_index_ == last_index_ + 1) {
                    current_index_ = last_index_;
                    return *this;
                }
                --first_ind_;
                current_index_ -= first_stride_;
                if (first_ind_ > -1) {
                    return *this;
                }
                first_ind_ = first_dim_ - 1;
                current_index_ += (first_ind_ + 1) * first_stride_;
                if (ndims_ > 1) {
                    --second_ind_;
                    current_index_ -= second_stride_;
                    if (second_ind_ > -1) {
                        return *this;
                    }
                    second_ind_ = second_dim_ - 1;
                    current_index_ += (second_ind_ + 1) * second_stride_;
                }
                if (ndims_ > 2) {
                    --third_ind_;
                    current_index_ -= third_stride_;
                    if (third_ind_ > -1) {
                        return *this;
                    }
                    third_ind_ = third_dim_ - 1;
                    current_index_ += (third_ind_ + 1) * third_stride_;
                }
                for (std::int64_t i = ndims_ - 4; i >= 0; --i) {
                    --indices_[i];
                    current_index_ -= strides_[i];
                    if (indices_[i] > -1) {
                        return *this;
                    }
                    indices_[i] = dims_[i] - 1;
                    current_index_ += (indices_[i] + 1) * strides_[i];
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator--(int) noexcept
            {
                arrnd_general_indexer<storage_type, header_type> temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_general_indexer& operator-=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator-(std::int64_t count) noexcept
            {
                arrnd_general_indexer<storage_type, header_type> temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return static_cast<std::uint64_t>(current_index_ - first_index_) <= last_first_diff_;
            }

            [[nodiscard]] constexpr std::int64_t operator*() const noexcept
            {
                return current_index_;
            }

        private:
            constexpr static storage_type order_from_major_axis(std::int64_t order_size, std::int64_t axis)
            {
                storage_type new_ordered_indices(order_size);
                std::iota(new_ordered_indices.begin(), new_ordered_indices.end(), static_cast<std::int64_t>(0));
                new_ordered_indices[0] = axis;
                std::int64_t pos = 1;
                for (std::int64_t i = 0; i < order_size; ++i) {
                    if (i != axis) {
                        new_ordered_indices[pos++] = i;
                    }
                }
                return new_ordered_indices;
            }

            constexpr static storage_type reorder(std::span<const std::int64_t> vec, std::span<const std::int64_t> indices)
            {
                std::size_t size = std::min(vec.size(), indices.size());
                storage_type res(size);
                for (std::int64_t i = 0; i < size; ++i) {
                    res[i] = vec[indices[i]];
                }
                return res;
            }

            constexpr static std::tuple<
                storage_type, storage_type>
                reduce_dimensions(std::span<const std::int64_t> dims, std::span<const std::int64_t> strides)
            {
                std::tuple<
                    storage_type,
                    storage_type> reds(dims.size(), dims.size());

                auto& [rdims, rstrides] = reds;

                std::int64_t rndims = 0;

                std::int64_t ri = 0;
                for (std::int64_t i = 0; i < dims.size(); ++i) {
                    if (dims[i] > 1) {
                        rdims[ri] = dims[i];
                        rstrides[ri] = strides[i];
                        ++ri;
                        ++rndims;
                    }
                }

                for (std::int64_t i = rndims - 1; i >= 1; --i) {
                    if (rdims[i] == rstrides[i - 1]) {
                        rdims[i - 1] *= rdims[i];
                        rstrides[i - 1] = rstrides[i];
                        --rndims;
                    }
                }

                if (rndims != dims.size()) {
                    rdims.resize(rndims);
                    rstrides.resize(rndims);
                }

                return reds;
            }

            storage_type dims_;
            storage_type strides_;
            std::int64_t first_index_;
            std::int64_t last_index_;
            std::int64_t last_first_diff_;
            std::int64_t ndims_;

            std::int64_t first_stride_;
            std::int64_t first_dim_;
            std::int64_t first_ind_;

            std::int64_t second_stride_;
            std::int64_t second_dim_;
            std::int64_t second_ind_;

            std::int64_t third_stride_;
            std::int64_t third_dim_;
            std::int64_t third_ind_;

            storage_type indices_;
            std::int64_t current_index_;
        };




        template <typename Header = arrnd_header<>>
        class arrnd_fast_indexer final
        {
        public:
            using header_type = Header;

            constexpr arrnd_fast_indexer(const header_type& hdr, bool backward = false)
                : arrnd_fast_indexer(hdr, 0, backward)
            {
            }

            constexpr arrnd_fast_indexer(const header_type& hdr, std::int64_t axis, bool backward = false)
            {
                // data

                last_index_ = hdr.last_index();

                num_super_groups_ = hdr.dims()[axis];
                step_size_between_super_groups_ = hdr.strides()[axis];

                num_groups_in_super_group_ =
                    std::accumulate(hdr.dims().begin(), hdr.dims().begin() + axis + 1, 1, std::multiplies<>{}) / num_super_groups_;
                group_size_ = hdr.strides()[axis];
                step_size_inside_group_ = hdr.strides().back();
                step_size_between_groups_ = num_super_groups_ * step_size_between_super_groups_;

                // accumulators
                if (!backward) {
                    current_index_ = 0;

                    super_groups_counter_ = 0;

                    group_indices_counter_ = 0;
                    groups_counter_ = 0;

                    super_group_start_index_ = 0;

                    group_start_index_ = 0;
                }
                else {
                    group_indices_counter_ = group_size_ - 1;
                    groups_counter_ = num_groups_in_super_group_ - 1;
                    super_groups_counter_ = num_super_groups_ - 1;

                    super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
                    group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                    current_index_ = last_index_;
                }
            }

            constexpr arrnd_fast_indexer() = default;

            constexpr arrnd_fast_indexer(const arrnd_fast_indexer& other) = default;
            constexpr arrnd_fast_indexer& operator=(const arrnd_fast_indexer& other) = default;

            constexpr arrnd_fast_indexer(arrnd_fast_indexer&& other) noexcept = default;
            constexpr arrnd_fast_indexer& operator=(arrnd_fast_indexer&& other) noexcept = default;

            constexpr ~arrnd_fast_indexer() = default;

            constexpr arrnd_fast_indexer& operator++() noexcept
            {
                // the algorithm is done by three functions composition:
                // - index
                // - group
                // - super group

                // index function

                if (current_index_ > last_index_) {
                    return *this;
                }

                ++group_indices_counter_;
                current_index_ += step_size_inside_group_;

                if (group_indices_counter_ < group_size_) {
                    return *this;
                }

                // group function

                group_indices_counter_ = 0;

                ++groups_counter_;
                group_start_index_ += step_size_between_groups_;

                current_index_ = group_start_index_;

                if (groups_counter_ < num_groups_in_super_group_) {
                    return *this;
                }

                // super group function

                groups_counter_ = 0;

                ++super_groups_counter_;
                super_group_start_index_ += step_size_between_super_groups_;

                group_start_index_ = super_group_start_index_;

                current_index_ = group_start_index_;

                if (super_groups_counter_ < num_super_groups_) {
                    return *this;
                }

                group_indices_counter_ = group_size_;
                groups_counter_ = num_groups_in_super_group_ - 1;
                super_groups_counter_ = num_super_groups_ - 1;

                super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
                group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                current_index_ = last_index_ + 1;

                return *this;
            }

            constexpr arrnd_fast_indexer operator++(int) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_fast_indexer& operator+=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            constexpr arrnd_fast_indexer operator+(std::int64_t count) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_fast_indexer& operator--() noexcept
            {
                // the algorithm is done by inverese of three functions composition:
                // - super group
                // - group
                // - index

                if (current_index_ < 0) {
                    return *this;
                }

                // index function

                --group_indices_counter_;
                current_index_ -= step_size_inside_group_;

                if (group_indices_counter_ >= 0) {
                    return *this;
                }

                // group function

                group_indices_counter_ = group_size_ - 1;

                --groups_counter_;
                group_start_index_ -= step_size_between_groups_;

                current_index_ = group_start_index_ + (group_size_-1) * step_size_inside_group_;

                if (groups_counter_ >= 0) {
                    return *this;
                }

                // super group function

                groups_counter_ = num_groups_in_super_group_ - 1;

                --super_groups_counter_;
                super_group_start_index_ -= step_size_between_super_groups_;

                group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                current_index_ = group_start_index_ + (group_size_-1) * step_size_inside_group_;

                if (super_groups_counter_ >= 0) {
                    return *this;
                }

                group_indices_counter_ = -1;
                groups_counter_ = 0;
                super_groups_counter_ = 0;

                super_group_start_index_ = 0;
                group_start_index_ = 0;

                current_index_ = -1;

                return *this;
            }

            constexpr arrnd_fast_indexer operator--(int) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_fast_indexer& operator-=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr arrnd_fast_indexer operator-(std::int64_t count) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return static_cast<std::uint64_t>(current_index_) <= last_index_;
            }

            [[nodiscard]] constexpr std::int64_t operator*() const noexcept
            {
                return current_index_;
            }

        private:
            std::int64_t current_index_ = 0;

            // data

            std::int64_t last_index_ = 0;

            std::int64_t num_super_groups_ = 0;
            std::int64_t step_size_between_super_groups_ = 0;

            std::int64_t num_groups_in_super_group_ = 0;
            std::int64_t group_size_ = 0;
            std::int64_t step_size_inside_group_ = 0;
            std::int64_t step_size_between_groups_ = 0;
            
            // counters

            std::int64_t super_groups_counter_ = 0;

            std::int64_t group_indices_counter_ = 0;
            std::int64_t groups_counter_ = 0;

            std::int64_t super_group_start_index_ = 0;

            std::int64_t group_start_index_ = 0;
        };

        template <typename T, typename Indexer = arrnd_general_indexer<>>
        class arrnd_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = T;
            using pointer = T*;
            using reference = T&;

            using indexer_type = Indexer;

            constexpr arrnd_iterator(pointer data, const indexer_type& gen)
                : gen_(gen), data_(data)
            {
            }

            constexpr arrnd_iterator() = default;

            constexpr arrnd_iterator(const arrnd_iterator& other) = default;
            constexpr arrnd_iterator& operator=(const arrnd_iterator& other) = default;

            constexpr arrnd_iterator(arrnd_iterator&& other) noexcept = default;
            constexpr arrnd_iterator& operator=(arrnd_iterator&& other) noexcept = default;

            constexpr ~arrnd_iterator() = default;

            constexpr arrnd_iterator& operator++() noexcept
            {
                ++gen_;
                return *this;
            }

            constexpr arrnd_iterator operator++(int) noexcept
            {
                arrnd_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_iterator& operator+=(difference_type count) noexcept
            {
                gen_ += count;
                return *this;
            }

            constexpr arrnd_iterator operator+(difference_type count) noexcept
            {
                arrnd_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_iterator& operator--() noexcept
            {
                gen_--;
                return *this;
            }

            constexpr arrnd_iterator operator--(int) noexcept
            {
                arrnd_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_iterator& operator-=(difference_type count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            constexpr arrnd_iterator operator-(difference_type count) noexcept
            {
                arrnd_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_iterator& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };




        template <typename T, typename Indexer = arrnd_general_indexer<>>
        class arrnd_const_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = T;
            using pointer = T*;
            using reference = T&;

            using indexer_type = Indexer;

            constexpr arrnd_const_iterator(pointer data, const indexer_type& gen)
                : gen_(gen), data_(data)
            {
            }

            constexpr arrnd_const_iterator() = default;

            constexpr arrnd_const_iterator(const arrnd_const_iterator& other) = default;
            constexpr arrnd_const_iterator& operator=(const arrnd_const_iterator& other) = default;

            constexpr arrnd_const_iterator(arrnd_const_iterator&& other) noexcept = default;
            constexpr arrnd_const_iterator& operator=(arrnd_const_iterator&& other) noexcept = default;

            constexpr ~arrnd_const_iterator() = default;

            constexpr arrnd_const_iterator& operator++() noexcept
            {
                ++gen_;
                return *this;
            }

            constexpr arrnd_const_iterator operator++(int) noexcept
            {
                arrnd_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_const_iterator& operator+=(difference_type count) noexcept
            {
                gen_ += count;
                return *this;
            }

            constexpr arrnd_const_iterator operator+(difference_type count) noexcept
            {
                arrnd_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_const_iterator& operator--() noexcept
            {
                --gen_;
                return *this;
            }

            constexpr arrnd_const_iterator operator--(int) noexcept
            {
                arrnd_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_const_iterator& operator-=(difference_type count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            constexpr arrnd_const_iterator operator-(difference_type count) noexcept
            {
                arrnd_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_const_iterator& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };



        template <typename T, typename Indexer = arrnd_general_indexer<>>
        class arrnd_reverse_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = T;
            using pointer = T*;
            using reference = T&;

            using indexer_type = Indexer;

            constexpr arrnd_reverse_iterator(pointer data, const indexer_type& gen)
                : gen_(gen), data_(data)
            {
            }

            constexpr arrnd_reverse_iterator() = default;

            constexpr arrnd_reverse_iterator(const arrnd_reverse_iterator& other) = default;
            constexpr arrnd_reverse_iterator& operator=(const arrnd_reverse_iterator& other) = default;

            constexpr arrnd_reverse_iterator(arrnd_reverse_iterator&& other) noexcept = default;
            constexpr arrnd_reverse_iterator& operator=(arrnd_reverse_iterator&& other) noexcept = default;

            constexpr ~arrnd_reverse_iterator() = default;

            constexpr arrnd_reverse_iterator& operator++() noexcept
            {
                --gen_;
                return *this;
            }

            constexpr arrnd_reverse_iterator operator++(int) noexcept
            {
                arrnd_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_reverse_iterator& operator+=(difference_type count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            constexpr arrnd_reverse_iterator operator+(difference_type count) noexcept
            {
                arrnd_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_reverse_iterator& operator--() noexcept
            {
                ++gen_;
                return *this;
            }

            constexpr arrnd_reverse_iterator operator--(int) noexcept
            {
                arrnd_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_reverse_iterator& operator-=(difference_type count) noexcept
            {
                gen_ += count;
                return *this;
            }

            constexpr arrnd_reverse_iterator operator-(difference_type count) noexcept
            {
                arrnd_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_reverse_iterator& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };




        template <typename T, typename Indexer = arrnd_general_indexer<>>
        class arrnd_const_reverse_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = T;
            using pointer = T*;
            using reference = T&;

            using indexer_type = Indexer;

            constexpr arrnd_const_reverse_iterator(pointer data, const indexer_type& gen)
                : gen_(gen), data_(data)
            {
            }

            constexpr arrnd_const_reverse_iterator() = default;

            constexpr arrnd_const_reverse_iterator(const arrnd_const_reverse_iterator& other) = default;
            constexpr arrnd_const_reverse_iterator& operator=(const arrnd_const_reverse_iterator& other) = default;

            constexpr arrnd_const_reverse_iterator(arrnd_const_reverse_iterator&& other) noexcept = default;
            constexpr arrnd_const_reverse_iterator& operator=(arrnd_const_reverse_iterator&& other) noexcept = default;

            constexpr ~arrnd_const_reverse_iterator() = default;

            constexpr arrnd_const_reverse_iterator& operator++() noexcept
            {
                --gen_;
                return *this;
            }

            constexpr arrnd_const_reverse_iterator operator++(int) noexcept
            {
                arrnd_const_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_const_reverse_iterator& operator+=(difference_type count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            constexpr arrnd_const_reverse_iterator operator+(difference_type count) noexcept
            {
                arrnd_const_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_const_reverse_iterator& operator--() noexcept
            {
                ++gen_;
                return *this;
            }

            constexpr arrnd_const_reverse_iterator operator--(int) noexcept
            {
                arrnd_const_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_const_reverse_iterator& operator-=(difference_type count) noexcept
            {
                gen_ += count;
                return *this;
            }

            constexpr arrnd_const_reverse_iterator operator-(difference_type count) noexcept
            {
                arrnd_const_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_const_reverse_iterator& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };


        struct CustomArrayTag {};

        template <typename T>
        concept CustomArray = std::is_same_v<typename T::Tag, CustomArrayTag>;


        template <typename T, typename StorageType = simple_dynamic_vector<T>, template<typename> typename SharedRefAllocType = lightweight_allocator, typename HeaderType = arrnd_header<>, typename IndexerType = arrnd_general_indexer<>>
        class Array {
        public:
            using Tag = CustomArrayTag;

            using ValueType = T;
            using Header = HeaderType;
            using Indexer = IndexerType;
            using Storage = StorageType;
            template <typename T>
            using SharedRefAlloc = SharedRefAllocType<T>;

            using ThisArrayType = Array<T, StorageType, SharedRefAllocType, HeaderType, IndexerType>;
            template <typename U>
            using RetypedArray = Array<U, typename StorageType::template replaced_type<U>, SharedRefAllocType, HeaderType, IndexerType>;

            Array() = default;

            Array(Array&& other) = default;
            template<CustomArray CA>
            Array(CA&& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);

                CA dummy{ std::move(other) };
            }
            Array& operator=(Array&& other) & = default;
            Array& operator=(Array&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy(other, *this);
                Array dummy{ std::move(other) };
                return *this;
            }
            template<CustomArray CA>
            Array& operator=(CA&& other)&
            {
                *this = ThisArrayType(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                CA dummy{ std::move(other) };
                return *this;
            }
            template<CustomArray CA>
            Array<T, StorageType, SharedRefAllocType, HeaderType, IndexerType>& operator=(CA&& other)&&
            {
                copy(other, *this);
                CA dummy{ std::move(other) };
                return *this;
            }

            Array(const Array& other) = default;
            template<CustomArray CA>
            Array(const CA& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);
            }
            Array& operator=(const Array& other) & = default;
            Array& operator=(const Array& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy(other, *this);
                return *this;
            }
            template<CustomArray CA>
            Array& operator=(const CA& other)&
            {
                *this = ThisArrayType(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                return *this;
            }
            template<CustomArray CA>
            Array& operator=(const CA& other)&&
            {
                copy(other, *this);
                return *this;
            }

            template <typename U>
            Array& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }

                for (IndexerType gen(hdr_); gen; ++gen) {
                    (*this)(*gen) = value;
                }

                return *this;
            }

            virtual ~Array() = default;

            Array(std::span<const std::int64_t> dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(std::allocate_shared<StorageType>(SharedRefAllocType<StorageType>(), hdr_.count()))
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
                : hdr_(dims), buffsp_(std::allocate_shared<StorageType>(SharedRefAllocType < StorageType>(), hdr_.count()))
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
                : hdr_(dims), buffsp_(std::allocate_shared<StorageType>(SharedRefAllocType < StorageType>(), hdr_.count()))
            {
                std::fill(buffsp_->data(), buffsp_->data() + buffsp_->size(), value);
            }
            Array(std::initializer_list<std::int64_t> dims, const T& value)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, value)
            {
            }
            template <typename U>
            Array(std::span<const std::int64_t> dims, const U& value)
                : hdr_(dims), buffsp_(std::allocate_shared<StorageType>(SharedRefAllocType < StorageType>(), hdr_.count()))
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

            [[nodiscard]] T* data() const noexcept
            {
                return buffsp_ ? buffsp_->data() : nullptr;
            }

            [[nodiscard]] const T& operator()(std::int64_t index) const noexcept
            {
                return buffsp_->data()[modulo(index, hdr_.last_index() + 1)];
            }
            [[nodiscard]] T& operator()(std::int64_t index) noexcept
            {
                return buffsp_->data()[modulo(index, hdr_.last_index() + 1)];
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

            [[nodiscard]] Array operator()(std::span<const Interval<std::int64_t>> ranges) const
            {
                if (ranges.empty() || empty(*this)) {
                    return (*this);
                }

                ThisArrayType slice{};
                slice.hdr_ = Header{ hdr_, ranges };
                slice.buffsp_ = slice.hdr_.empty() ? nullptr : buffsp_;
                return slice;
            }
            [[nodiscard]] Array operator()(std::initializer_list<Interval<std::int64_t>> ranges) const
            {
                return (*this)(std::span<const Interval<std::int64_t>>{ranges.begin(), ranges.size()});
            }

            [[nodiscard]] Array operator()(const RetypedArray<std::int64_t>& indices) const noexcept
            {
                ThisArrayType res(std::span<const std::int64_t>(indices.header().dims().data(), indices.header().dims().size()));

                for (IndexerType gen(indices.header()); gen; ++gen) {
                    res(*gen) = buffsp_->data()[indices(*gen)];
                }

                return res;
            }


            /**
            * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
            */
            template <CustomArray CA>
            auto& copy_from(const CA& src)
            {
                if (empty(*this) || empty(src)) {
                    return *this;
                }

                typename CA::Indexer src_gen(src.header());
                IndexerType dst_gen(header());

                for (; src_gen && dst_gen; ++src_gen, ++dst_gen) {
                    (*this)(*dst_gen) = src(*src_gen);
                }

                return *this;
            }

            [[nodiscard]] auto clone() const
            {
                if (empty(*this)) {
                    return ThisArrayType();
                }

                ThisArrayType clone(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                IndexerType gen(header());
                IndexerType clone_gen(clone.header());

                for (; gen && clone_gen; ++gen, ++clone_gen) {
                    clone(*clone_gen) = (*this)(*gen);
                }

                return clone;
            }


            /**
            * @note Returning a reference to the input array and no allocation performed. If empty array, subarray, or different dimensions resizing.
            */
            [[nodiscard]] auto reshape(std::span<const std::int64_t> new_dims) const
            {
                if (header().count() != numel(new_dims) || header().is_subarray()) {
                    return resize(new_dims);
                }

                if (header().dims() == new_dims) {
                    return *this;
                }

                typename ThisArrayType::Header new_header(new_dims);
                if (new_header.empty()) {
                    return ThisArrayType();
                }

                ThisArrayType res(*this);
                res.header() = std::move(new_header);

                return res;
            }
            [[nodiscard]] auto reshape(std::initializer_list<std::int64_t> new_dims) const
            {
                return reshape(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
            }

            /*
            * @note Other references to this array buffer are not modified. Resize is not performed if dims and new_dims are equal.
            */
            [[nodiscard]] auto resize(std::span<const std::int64_t> new_dims) const
            {
                if (empty(*this)) {
                    return ThisArrayType(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));
                }

                if (header().dims() == new_dims) {
                    return *this;
                }

                if (numel(new_dims) <= 0) {
                    return ThisArrayType();
                }

                ThisArrayType res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

                IndexerType gen(header());
                IndexerType res_gen(res.header());

                while (gen && res_gen) {
                    res(*res_gen) = (*this)(*gen);
                    ++gen;
                    ++res_gen;
                }

                return res;
            }
            [[nodiscard]] auto resize(std::initializer_list<std::int64_t> new_dims) const
            {
                return resize(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
            }


            template <CustomArray CA>
            [[nodiscard]] auto append(const CA& arr) const
            {
                if (empty(*this)) {
                    ThisArrayType res(arr);
                    return res.clone();
                }

                if (empty(arr)) {
                    return *this;
                }

                ThisArrayType res(resize({ header().count() + arr.header().count() }));

                CA rarr(arr.reshape({ arr.header().count() }));

                for (std::int64_t i = header().count(); i < res.header().count(); ++i) {
                    res({ i }) = arr({ i - header().count() });
                }

                return res;
            }

            template <CustomArray CA>
            [[nodiscard]] auto append(const CA& arr, std::int64_t axis) const
            {
                if (empty(*this)) {
                    ThisArrayType res(arr);
                    return res.clone();
                }

                if (empty(arr)) {
                    return *this;
                }

                HeaderType new_header(header(), arr.header().dims(), axis);
                if (new_header.empty()) {
                    return ThisArrayType{};
                }

                ThisArrayType res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                IndexerType gen(header(), fixed_axis);
                typename CA::Indexer arr_gen(arr.header(), fixed_axis);
                IndexerType res_gen(res.header(), fixed_axis);

                for (; gen && res_gen; ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }
                for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                    res.data()[*res_gen] = arr.data()[*arr_gen];
                }

                return res;
            }

            template <CustomArray CA>
            [[nodiscard]] auto insert(const CA& arr, std::int64_t ind) const
            {
                if (empty(*this)) {
                    ThisArrayType res(arr);
                    return res.clone();
                }

                if (empty(arr)) {
                    return *this;
                }

                ThisArrayType res({ header().count() + arr.header().count() });

                ThisArrayType rlhs(reshape({ header().count() }));
                CA rarr(arr.reshape({ arr.header().count() }));

                std::int64_t fixed_ind{ modulo(ind, header().count() + 1) };

                for (std::int64_t i = 0; i < fixed_ind; ++i) {
                    res({ i }) = rlhs({ i });
                }
                for (std::int64_t i = 0; i < arr.header().count(); ++i) {
                    res({ fixed_ind + i }) = rarr({ i });
                }
                for (std::int64_t i = 0; i < header().count() - fixed_ind; ++i) {
                    res({ fixed_ind + arr.header().count() + i }) = rlhs({ fixed_ind + i });
                }

                return res;
            }

            template <CustomArray CA>
            [[nodiscard]] auto insert(const CA& arr, std::int64_t ind, std::int64_t axis) const
            {
                if (empty(*this)) {
                    ThisArrayType res(arr);
                    return res.clone();
                }

                if (empty(arr)) {
                    return *this;
                }

                HeaderType new_header(header(), arr.header().dims(), axis);
                if (new_header.empty()) {
                    return ThisArrayType();
                }

                ThisArrayType res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                IndexerType gen(header(), fixed_axis);
                typename CA::Indexer arr_gen(arr.header(), fixed_axis);
                IndexerType res_gen(res.header(), fixed_axis);

                std::int64_t fixed_ind{ modulo(ind, header().dims()[fixed_axis]) };
                std::int64_t cycle = fixed_ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), 1, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

                for (; gen && res_gen && cycle; --cycle, ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }
                for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                    res.data()[*res_gen] = arr.data()[*arr_gen];
                }
                for (; gen && res_gen; ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }

                return res;
            }

            /**
            * @note All elements starting from ind are being removed in case that count value is too big.
            */
            [[nodiscard]] auto remove(std::int64_t ind, std::int64_t count) const
            {
                if (empty(*this)) {
                    return *this;
                }

                std::int64_t fixed_ind{ modulo(ind, header().count()) };
                std::int64_t fixed_count{ fixed_ind + count < header().count() ? count : (header().count() - fixed_ind) };

                ThisArrayType res({ header().count() - fixed_count });
                ThisArrayType rarr(reshape({ header().count() }));

                for (std::int64_t i = 0; i < fixed_ind; ++i) {
                    res({ i }) = rarr({ i });
                }
                for (std::int64_t i = fixed_ind + fixed_count; i < header().count(); ++i) {
                    res({ i - fixed_count }) = rarr({ i });
                }

                return res;
            }

            /**
            * @note All elements starting from ind are being removed in case that count value is too big.
            */
            [[nodiscard]] auto remove(std::int64_t ind, std::int64_t count, std::int64_t axis) const
            {
                if (empty(*this)) {
                    return *this;
                }

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };
                std::int64_t fixed_ind{ modulo(ind, header().dims()[fixed_axis]) };
                std::int64_t fixed_count{ fixed_ind + count <= header().dims()[fixed_axis] ? count : (header().dims()[fixed_axis] - fixed_ind) };

                HeaderType new_header(header(), -fixed_count, fixed_axis);
                if (new_header.empty()) {
                    return ThisArrayType();
                }

                ThisArrayType res({ header().count() - (header().count() / header().dims()[fixed_axis]) * fixed_count });
                res.header() = std::move(new_header);

                IndexerType gen(header(), fixed_axis);
                IndexerType res_gen(res.header(), fixed_axis);

                std::int64_t cycle = fixed_ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), 1, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

                std::int64_t removals = header().count() - res.header().count();

                for (; gen && res_gen && cycle; --cycle, ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }
                for (; gen && removals; --removals, ++gen) {
                    //data()[*gen] = arr.data()[*arr_gen];
                }
                for (; gen && res_gen; ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }

                return res;
            }


            template <typename Unary_op>
            [[nodiscard]] auto transform(Unary_op&& op) const
            {
                using U = decltype(op(data()[0]));

                if (empty(*this)) {
                    return RetypedArray<U>();
                }

                RetypedArray<U> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                for (IndexerType gen(header()); gen; ++gen) {
                    res(*gen) = op((*this)(*gen));
                }

                return res;
            }

            template <CustomArray CA, typename Binary_op>
            [[nodiscard]] auto transform(const CA& arr, Binary_op&& op) const
            {
                using U = decltype(op(data()[0], arr.data()[0]));

                if (!std::equal(header().dims().begin(), header().dims().end(), arr.header().dims().begin(), arr.header().dims().end())) {
                    return RetypedArray<U>();
                }

                RetypedArray<U> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                IndexerType gen(header());
                typename RetypedArray<U>::Indexer arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    res(*gen) = op((*this)(*gen), arr(*arr_gen));
                }

                return res;
            }

            template <typename V, typename Binary_op>
            [[nodiscard]] auto transform(const V& value, Binary_op&& op) const
            {
                using U = decltype(op(data()[0], value));

                RetypedArray<U> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                for (IndexerType gen(header()); gen; ++gen) {
                    res(*gen) = op((*this)(*gen), value);
                }

                return res;
            }


            template <typename Unary_op>
            auto& apply(Unary_op&& op)
            {
                if (empty(*this)) {
                    return *this;
                }

                for (IndexerType gen(header()); gen; ++gen) {
                    (*this)(*gen) = op((*this)(*gen));
                }

                return *this;
            }

            template <CustomArray CA, typename Binary_op>
            auto& apply(const CA& arr, Binary_op&& op)
            {
                if (!std::equal(header().dims().begin(), header().dims().end(), arr.header().dims().begin(), arr.header().dims().end())) {
                    return *this;
                }

                IndexerType gen(header());
                typename CA::Indexer arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    (*this)(*gen) = op((*this)(*gen), arr(*arr_gen));
                }

                return *this;
            }

            template <typename V, typename Binary_op>
            auto& apply(const V& value, Binary_op&& op)
            {
                for (IndexerType gen(header()); gen; ++gen) {
                    (*this)(*gen) = op((*this)(*gen), value);
                }

                return *this;
            }



            template <typename Binary_op>
            [[nodiscard]] auto reduce(Binary_op&& op) const
            {
                using U = decltype(op(data()[0], data()[0]));

                if (empty(*this)) {
                    return U{};
                }

                IndexerType gen{ header() };

                U res{ static_cast<U>((*this)(*gen)) };
                ++gen;

                while (gen) {
                    res = op(res, (*this)(*gen));
                    ++gen;
                }

                return res;
            }

            template <typename U, typename Binary_op>
            [[nodiscard]] auto reduce(const U& init_value, Binary_op&& op) const
            {
                if (empty(*this)) {
                    return init_value;
                }

                U res{ init_value };
                for (IndexerType gen{ header() }; gen; ++gen) {
                    res = op(res, (*this)(*gen));
                }

                return res;
            }

            template <typename Binary_op>
            [[nodiscard]] auto reduce(Binary_op&& op, std::int64_t axis) const
            {
                using U = decltype(op(data()[0], data()[0]));

                if (empty(*this)) {
                    return RetypedArray<U>();
                }

                const std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                typename RetypedArray<U>::Header new_header(header(), fixed_axis);
                if (new_header.empty()) {
                    return RetypedArray<U>();
                }

                RetypedArray<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                IndexerType gen(header(), std::ssize(header().dims()) - fixed_axis - 1);
                IndexerType res_gen(res.header());

                const std::int64_t reduction_iteration_cycle{ header().dims()[fixed_axis] };

                while (gen && res_gen) {
                    U res_element{ static_cast<U>((*this)(*gen)) };
                    ++gen;
                    for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++gen) {
                        res_element = op(res_element, (*this)(*gen));
                    }
                    res(*res_gen) = res_element;
                    ++res_gen;
                }

                return res;
            }

            template <CustomArray CA, typename Binary_op>
            [[nodiscard]] auto reduce(const CA& init_values, Binary_op&& op, std::int64_t axis) const
            {
                using U = decltype(op(init_values.data()[0], data()[0]));

                if (empty(*this)) {
                    return RetypedArray<U>();
                }

                const std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                if (init_values.header().dims().size() != 1 && init_values.header().dims()[fixed_axis] != header().dims()[fixed_axis]) {
                    return RetypedArray<U>();
                }

                typename RetypedArray<U>::Header new_header(header(), axis);
                if (new_header.empty()) {
                    return RetypedArray<U>();
                }

                RetypedArray<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                IndexerType gen(header(), std::ssize(header().dims()) - fixed_axis - 1);
                IndexerType res_gen(res.header());
                typename CA::Indexer init_gen(init_values.header());

                const std::int64_t reduction_iteration_cycle{ header().dims()[fixed_axis] };

                while (gen && res_gen && init_gen) {
                    U res_element{ init_values(*init_gen) };
                    for (std::int64_t i = 0; i < reduction_iteration_cycle; ++i, ++gen) {
                        res_element = op(res_element, (*this)(*gen));
                    }
                    res(*res_gen) = std::move(res_element);
                    ++res_gen;
                    ++init_gen;
                }

                return res;
            }


            template <typename Unary_pred>
            [[nodiscard]] auto filter(Unary_pred pred) const
            {
                if (empty(*this)) {
                    return ThisArrayType();
                }

                ThisArrayType res({ header().count() });

                IndexerType gen(header());
                IndexerType res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && res_gen) {
                    if (pred((*this)(*gen))) {
                        res(*res_gen) = (*this)(*gen);
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                }

                if (res_count == 0) {
                    return ThisArrayType();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <CustomArray CA>
            [[nodiscard]] auto filter(const CA& mask) const
            {
                if (empty(*this)) {
                    return ThisArrayType();
                }

                if (!std::equal(header().dims().begin(), header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                    return ThisArrayType();
                }

                ThisArrayType res({ header().count() });

                IndexerType gen(header());
                typename CA::Indexer mask_gen(mask.header());

                IndexerType res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && mask_gen && res_gen) {
                    if (mask(*mask_gen)) {
                        res(*res_gen) = (*this)(*gen);
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                    ++mask_gen;
                }

                if (res_count == 0) {
                    return ThisArrayType();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <typename Unary_pred>
            [[nodiscard]] auto find(Unary_pred pred) const
            {
                if (empty(*this)) {
                    return RetypedArray<std::int64_t>();
                }

                RetypedArray<std::int64_t> res({ header().count() });

                IndexerType gen(header());
                typename RetypedArray<std::int64_t>::Indexer res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && res_gen) {
                    if (pred((*this)(*gen))) {
                        res(*res_gen) = *gen;
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                }

                if (res_count == 0) {
                    return RetypedArray<std::int64_t>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <CustomArray CA>
            [[nodiscard]] auto find(const CA& mask) const
            {
                if (empty(*this)) {
                    return RetypedArray<std::int64_t>();
                }

                if (!std::equal(header().dims().begin(), header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                    return RetypedArray<std::int64_t>();
                }

                RetypedArray<std::int64_t> res({ header().count() });

                IndexerType gen(header());
                typename CA::Indexer mask_gen(mask.header());

                typename RetypedArray<std::int64_t>::Indexer res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && mask_gen && res_gen) {
                    if (mask(*mask_gen)) {
                        res(*res_gen) = *gen;
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                    ++mask_gen;
                }

                if (res_count == 0) {
                    return RetypedArray<std::int64_t>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }


            [[nodiscard]] auto transpose(std::span<const std::int64_t> order) const
            {
                if (empty(*this)) {
                    return ThisArrayType();
                }

                HeaderType new_header(header(), order);
                if (new_header.empty()) {
                    return ThisArrayType();
                }

                ThisArrayType res({ header().count() });
                res.header() = std::move(new_header);

                IndexerType gen(header(), order);
                IndexerType res_gen(res.header());

                while (gen && res_gen) {
                    res(*res_gen) = (*this)(*gen);
                    ++gen;
                    ++res_gen;
                }

                return res;
            }

            [[nodiscard]] auto transpose(std::initializer_list<std::int64_t> order) const
            {
                return transpose(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            template <CustomArray CA, typename Binary_pred>
            [[nodiscard]] bool all_match(const CA& arr, Binary_pred pred) const
            {
                if (empty(*this) && empty(arr)) {
                    return true;
                }

                if (empty(*this) || empty(arr)) {
                    return false;
                }

                if (!std::equal(header().dims().begin(), header().dims().end(), arr.header().dims().begin(), arr.header().dims().end())) {
                    return false;
                }

                IndexerType gen(header());
                typename CA::Indexer arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    if (!pred((*this)(*gen), arr(*arr_gen))) {
                        return false;
                    }
                }

                return true;
            }

            template <typename U, typename Binary_pred>
            [[nodiscard]] bool all_match(const U& value, Binary_pred pred) const
            {
                if (empty(*this)) {
                    return true;
                }

                for (IndexerType gen(header()); gen; ++gen) {
                    if (!pred((*this)(*gen), value)) {
                        return false;
                    }
                }

                return true;
            }

            template <typename Unary_pred>
            [[nodiscard]] bool all_match(Unary_pred pred) const
            {
                if (empty(*this)) {
                    return true;
                }

                for (IndexerType gen(header()); gen; ++gen) {
                    if (!pred((*this)(*gen))) {
                        return false;
                    }
                }

                return true;
            }

            template <CustomArray CA, typename Binary_pred>
            [[nodiscard]] bool any_match(const CA& arr, Binary_pred pred) const
            {
                if (empty(*this) && empty(arr)) {
                    return true;
                }

                if (empty(*this) || empty(arr)) {
                    return false;
                }

                if (!std::equal(header().dims().begin(), header().dims().end(), arr.header().dims().begin(), arr.header().dims().end())) {
                    return false;
                }

                IndexerType gen(header());
                typename CA::Indexer arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    if (pred((*this)(*gen), arr(*arr_gen))) {
                        return true;
                    }
                }

                return false;
            }

            template <typename U, typename Binary_pred>
            [[nodiscard]] bool any_match(const U& value, Binary_pred pred) const
            {
                if (empty(*this)) {
                    return true;
                }

                for (IndexerType gen(header()); gen; ++gen) {
                    if (pred((*this)(*gen), value)) {
                        return true;
                    }
                }

                return false;
            }

            template <typename Unary_pred>
            [[nodiscard]] bool any_match(Unary_pred pred) const
            {
                if (empty(*this)) {
                    return true;
                }

                for (IndexerType gen(header()); gen; ++gen) {
                    if (pred((*this)(*gen))) {
                        return true;
                    }
                }

                return false;
            }


            [[nodiscard]] bool all() const
            {
                return all_match([](const T& value) { return static_cast<bool>(value); });
            }

            [[nodiscard]] auto all(std::int64_t axis) const
            {
                return reduce([](const T& a, const T& b) { return a && b; }, axis);
            }

            [[nodiscard]] bool any() const
            {
                return any_match([](const T& value) { return static_cast<bool>(value); });
            }

            [[nodiscard]] auto any(std::int64_t axis) const
            {
                return reduce([](const T& a, const T& b) { return a || b; }, axis);
            }


            template <CustomArray CA>
            [[nodiscard]] RetypedArray<bool> close(const CA& arr, const decltype(T{} - typename CA::ValueType{})& atol = default_atol<decltype(T{} - typename CA::ValueType{}) > (), const decltype(T{} - typename CA::ValueType{})& rtol = default_rtol<decltype(T{} - typename CA::ValueType{}) > ()) const
            {
                return transform(arr, [&atol, &rtol](const T& a, const typename CA::ValueType& b) { return oc::details::close(a, b, atol, rtol); });
            }

            template <typename U>
            [[nodiscard]] RetypedArray<bool> close(const U& value, const decltype(T{} - U{})& atol = default_atol<decltype(T{} - U{}) > (), const decltype(T{} - U{})& rtol = default_rtol<decltype(T{} - U{}) > ()) const
            {
                return transform(value, [&atol, &rtol](const T& a, const U& b) { return oc::details::close(a, b, atol, rtol); });
            }


            template <CustomArray CA>
            [[nodiscard]] bool all_equal(const CA& arr) const
            {
                return all_match(arr, [](const T& a, const typename CA::ValueType& b) { return a == b; });
            }

            template <typename U>
            [[nodiscard]] bool all_equal(const U& value) const
            {
                return all_match(value, [](const T& a, const U& b) { return a == b; });
            }

            template <CustomArray CA>
            [[nodiscard]] bool all_close(const CA& arr, const decltype(T{} - typename CA::ValueType{})& atol = default_atol<decltype(T{} - typename CA::ValueType{}) > (), const decltype(T{} - typename CA::ValueType{})& rtol = default_rtol<decltype(T{} - typename CA::ValueType{}) > ()) const
            {
                return all_match(arr, [&atol, &rtol](const T& a, const typename CA::ValueType& b) { return oc::details::close(a, b, atol, rtol); });
            }

            template <typename U>
            [[nodiscard]] bool all_close(const U& value, const decltype(T{} - U{})& atol = default_atol<decltype(T{} - U{}) > (), const decltype(T{} - U{})& rtol = default_rtol<decltype(T{} - U{}) > ()) const
            {
                return all_match(value, [&atol, &rtol](const T& a, const U& b) { return oc::details::close(a, b, atol, rtol); });
            }

            auto begin(std::int64_t axis = 0)
            {
                return arrnd_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis));
            }

            auto end(std::int64_t axis = 0)
            {
                return arrnd_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis, true) + 1);
            }


            auto cbegin(std::int64_t axis = 0) const
            {
                return arrnd_const_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis));
            }

            auto cend(std::int64_t axis = 0) const
            {
                return arrnd_const_iterator<T, IndexerType>(buffsp_->data() , IndexerType(hdr_, axis, true) + 1);
            }


            auto rbegin(std::int64_t axis = 0)
            {
                return arrnd_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis, true));
            }

            auto rend(std::int64_t axis = 0)
            {
                return arrnd_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis) - 1);
            }

            auto crbegin(std::int64_t axis = 0) const
            {
                return arrnd_const_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis, true));
            }

            auto crend(std::int64_t axis = 0) const
            {
                return arrnd_const_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, axis) - 1);
            }


            auto begin(std::span<const std::int64_t> order)
            {
                return arrnd_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order));
            }

            auto end(std::span<const std::int64_t> order)
            {
                return arrnd_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order, true) + 1);
            }


            auto cbegin(std::span<const std::int64_t> order) const
            {
                return arrnd_const_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order));
            }

            auto cend(std::span<const std::int64_t> order) const
            {
                return arrnd_const_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order, true) + 1);
            }


            auto rbegin(std::span<const std::int64_t> order)
            {
                return arrnd_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order, true));
            }

            auto rend(std::span<const std::int64_t> order)
            {
                return arrnd_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order) - 1);
            }

            auto crbegin(std::span<const std::int64_t> order) const
            {
                return arrnd_const_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order, true));
            }

            auto crend(std::span<const std::int64_t> order) const
            {
                return arrnd_const_reverse_iterator<T, IndexerType>(buffsp_->data(), IndexerType(hdr_, order) - 1);
            }



            auto begin(std::initializer_list<std::int64_t> order)
            {
                return begin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            auto end(std::initializer_list<std::int64_t> order)
            {
                return end(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            auto cbegin(std::initializer_list<std::int64_t> order) const
            {
                return cbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            auto cend(std::initializer_list<std::int64_t> order) const
            {
                return cend(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            auto rbegin(std::initializer_list<std::int64_t> order)
            {
                return rbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            auto rend(std::initializer_list<std::int64_t> order)
            {
                return rend(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            auto crbegin(std::initializer_list<std::int64_t> order) const
            {
                return crbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            auto crend(std::initializer_list<std::int64_t> order) const
            {
                return crend(std::span<const std::int64_t>(order.begin(), order.size()));
            }



            
            [[nodiscard]] inline auto abs()
            {
                return transform([](const T& a) { return abs(a); });
            }

            
            [[nodiscard]] inline auto acos()
            {
                return transform([](const T& a) { return acos(a); });
            }

            
            [[nodiscard]] inline auto acosh()
            {
                return transform([](const T& a) { return acosh(a); });
            }

            
            [[nodiscard]] inline auto asin()
            {
                return transform([](const T& a) { return asin(a); });
            }

            
            [[nodiscard]] inline auto asinh()
            {
                return transform([](const T& a) { return asinh(a); });
            }

            
            [[nodiscard]] inline auto atan()
            {
                return transform([](const T& a) { return atan(a); });
            }

            
            [[nodiscard]] inline auto atanh()
            {
                return transform([](const T& a) { return atanh(a); });
            }

            
            [[nodiscard]] inline auto cos()
            {
                return transform([](const T& a) { return cos(a); });
            }

            
            [[nodiscard]] inline auto cosh()
            {
                return transform([](const T& a) { return cosh(a); });
            }

            
            [[nodiscard]] inline auto exp()
            {
                return transform([](const T& a) { return exp(a); });
            }

            
            [[nodiscard]] inline auto log()
            {
                return transform([](const T& a) { return log(a); });
            }

            
            [[nodiscard]] inline auto log10()
            {
                return transform([](const T& a) { return log10(a); });
            }

            
            [[nodiscard]] inline auto pow()
            {
                return transform([](const T& a) { return pow(a); });
            }

            
            [[nodiscard]] inline auto sin()
            {
                return transform([](const T& a) { return sin(a); });
            }

            
            [[nodiscard]] inline auto sinh()
            {
                return transform([](const T& a) { return sinh(a); });
            }

            
            [[nodiscard]] inline auto sqrt()
            {
                return transform([](const T& a) { return sqrt(a); });
            }

            
            [[nodiscard]] inline auto tan()
            {
                return transform([](const T& a) { return tan(a); });
            }

            
            [[nodiscard]] inline auto tanh()
            {
                return transform([](const T& a) { return tanh(a); });
            }



        private:
            Header hdr_{};
            std::shared_ptr<StorageType> buffsp_{ nullptr };
        };

        /**
        * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
        */
        template <CustomArray CA1, CustomArray CA2>
        inline void copy(const CA1& src, CA2& dst)
        {
            dst.copy_from(src);
        }
        template <CustomArray CA1, CustomArray CA2>
        inline void copy(const CA1& src, CA2&& dst)
        {
            copy(src, dst);
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto clone(const CA& arr)
        {
            return arr.clone();
        }

        /**
        * @note Returning a reference to the input array, except in case of resulted empty array or an input subarray.
        */
        template <CustomArray CA>
        [[nodiscard]] inline auto reshape(const CA& arr, std::span<const std::int64_t> new_dims)
        {
            return arr.reshape(new_dims);
        }
        template <CustomArray CA>
        [[nodiscard]] inline auto reshape(const CA& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto resize(const CA& arr, std::span<const std::int64_t> new_dims)
        {
            return arr.resize(new_dims);
        }
        template <CustomArray CA>
        [[nodiscard]] inline auto resize(const CA& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline CA1 append(const CA1& lhs, const CA2& rhs)
        {
            return lhs.append(rhs);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline CA1 append(const CA1& lhs, const CA2& rhs, std::int64_t axis)
        {
            return lhs.append(rhs, axis);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline CA1 insert(const CA1& lhs, const CA2& rhs, std::int64_t ind)
        {
            return lhs.insert(rhs, ind);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline CA1 insert(const CA1& lhs, const CA2& rhs, std::int64_t ind, std::int64_t axis)
        {
            return lhs.insert(rhs, ind, axis);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <CustomArray CA>
        [[nodiscard]] inline auto remove(const CA& arr, std::int64_t ind, std::int64_t count)
        {
            return arr.remove(ind, count);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <CustomArray CA>
        [[nodiscard]] inline auto remove(const CA& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            return arr.remove(ind, count, axis);
        }

        template <CustomArray CA>
        [[nodiscard]] inline bool empty(const CA& arr) noexcept
        {
            return !arr.data() && arr.header().empty();
        }

        template <CustomArray CA, typename Unary_op>    
        [[nodiscard]] inline auto transform(const CA& arr, Unary_op&& op)
        {
            return arr.transform(op);
        }

        template <CustomArray CA, typename Binary_op>
        requires std::is_invocable_v<Binary_op, typename CA::ValueType, typename CA::ValueType>
        [[nodiscard]] inline auto reduce(const CA& arr, Binary_op&& op)
        {
            return arr.reduce(op);
        }

        template <CustomArray CA, typename T, typename Binary_op>
        requires std::is_invocable_v<Binary_op, T, typename CA::ValueType>
        [[nodiscard]] inline auto reduce(const CA& arr, const T& init_value, Binary_op&& op)
        {
            return arr.reduce(init_value, op);
        }

        template <CustomArray CA, typename Binary_op>
        requires std::is_invocable_v<Binary_op, typename CA::ValueType, typename CA::ValueType>
        [[nodiscard]] inline auto reduce(const CA& arr, Binary_op&& op, std::int64_t axis)
        {
            return arr.reduce(op, axis);
        }

        template <CustomArray CA1, CustomArray CA2, typename Binary_op>
        requires std::is_invocable_v<Binary_op, typename CA2::ValueType, typename CA1::ValueType>
        [[nodiscard]] inline auto reduce(const CA1& arr, const CA2& init_values, Binary_op&& op, std::int64_t axis)
        {
            return arr.reduce(init_values, op, axis);
        }

        template <CustomArray CA>
        [[nodiscard]] inline bool all(const CA& arr)
        {
            return arr.all();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto all(const CA& arr, std::int64_t axis)
        {
            return arr.all(axis);
        }

        template <CustomArray CA>
        [[nodiscard]] inline bool any(const CA& arr)
        {
            return arr.any();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto any(const CA& arr, std::int64_t axis)
        {
            return arr.any(axis);
        }

        template <CustomArray CA1, CustomArray CA2, typename Binary_op>
        [[nodiscard]] inline auto transform(const CA1& lhs, const CA2& rhs, Binary_op&& op)
        {
            return lhs.transform(rhs, op);
        }

        template <CustomArray CA, typename T, typename Binary_op>
        [[nodiscard]] inline auto transform(const CA& lhs, const T& rhs, Binary_op&& op)
        {
            return lhs.transform(rhs, op);
        }

        template <typename T, CustomArray CA, typename Binary_op>
        [[nodiscard]] inline auto transform(const T& lhs, const CA& rhs, Binary_op&& op)
        {
            return rhs.transform([&lhs, &op](const typename CA::ValueType& value) { return op(lhs, value); });
        }

        template <CustomArray CA, typename Unary_pred>
        [[nodiscard]] inline auto filter(const CA& arr, Unary_pred pred)
        {
            return arr.filter(pred);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline CA1 filter(const CA1& arr, const CA2& mask)
        {
            return arr.filter(mask);
        }

        template <CustomArray CA, typename Unary_pred>
        [[nodiscard]] inline auto find(const CA& arr, Unary_pred pred)
        {
            return arr.find(pred);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto find(const CA1& arr, const CA2& mask)
        {
            return arr.find(mask);
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto transpose(const CA& arr, std::span<const std::int64_t> order)
        {
            return arr.traspose(order);
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto transpose(const CA& arr, std::initializer_list<std::int64_t> order)
        {
            return arr.transpose(order);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator==(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a == b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator==(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a == b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator==(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a == b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator!=(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a != b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator!=(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a != b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator!=(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a != b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto close(const CA1& lhs, const CA2& rhs, const decltype(typename CA1::ValueType{} - typename CA2::ValueType{})& atol = default_atol<decltype(typename CA1::ValueType{} - typename CA2::ValueType{}) > (), const decltype(typename CA1::ValueType{} - typename CA2::ValueType{})& rtol = default_rtol<decltype(typename CA1::ValueType{} - typename CA2::ValueType{}) > ())
        {
            return lhs.close(rhs, atol, rtol);
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto close(const CA& lhs, const T& rhs, const decltype(typename CA::ValueType{} - T{})& atol = default_atol<decltype(typename CA::ValueType{} - T{}) > (), const decltype(typename CA::ValueType{} - T{})& rtol = default_rtol<decltype(typename CA::ValueType{} - T{}) > ())
        {
            return lhs.close(rhs, atol, rtol);
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto close(const T& lhs, const CA& rhs, const decltype(T{} - typename CA::ValueType{})& atol = default_atol<decltype(T{} - typename CA::ValueType{}) > (), const decltype(T{} - typename CA::ValueType{})& rtol = default_rtol<decltype(T{} - typename CA::ValueType{}) > ())
        {
            return rhs.close(lhs, atol, rtol);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator>(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a > b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator>(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a > b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator>(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a > b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator>=(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a >= b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator>=(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a >= b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator>=(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a >= b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator<(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a < b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator<(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a < b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator<(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a < b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator<=(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a <= b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator<=(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a <= b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator<=(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a <= b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator+(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a + b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator+(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a + b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator+(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a + b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator+=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a + b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator+=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a + b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator-(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a - b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator-(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a - b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator-(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a - b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator-=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a - b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator-=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a - b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator*(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a * b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator*(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a * b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator*(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a * b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator*=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a * b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator*=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a * b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator/(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a / b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator/(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a / b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator/(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a / b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator/=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a / b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator/=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a / b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto operator%(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a % b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator%(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a % b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator%(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a % b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator%=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a % b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator%=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a % b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator^(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a ^ b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator^(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a ^ b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator^(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a ^ b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator^=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a ^ b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator^=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a ^ b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator&(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a & b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator&(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a & b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator&(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a & b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator&=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a & b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator&=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a & b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator|(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a | b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator|(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a | b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator|(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a | b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator|=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a | b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator|=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a | b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator<<(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a << b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator<<(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a << b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator<<(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a << b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator<<=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a << b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator<<=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a << b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator>>(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a >> b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator>>(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a >> b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator>>(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a >> b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        inline auto& operator>>=(CA1& lhs, const CA2& rhs)
        {
            return lhs.apply(rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a >> b; });
        }

        template <CustomArray CA, typename T>
        inline auto& operator>>=(CA& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename CA::ValueType& a, const T& b) { return a >> b; });
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator~(const CA& arr)
        {
            return transform(arr, [](const typename CA::ValueType& a) { return ~a; });
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator!(const CA& arr)
        {
            return transform(arr, [](const typename CA::ValueType& a) { return !a; });
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator+(const CA& arr)
        {
            return transform(arr, [](const typename CA::ValueType& a) { return +a; });
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator-(const CA& arr)
        {
            return transform(arr, [](const typename CA::ValueType& a) { return -a; });
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto abs(const CA& arr)
        {
            return arr.abs();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto acos(const CA& arr)
        {
            return arr.acos();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto acosh(const CA& arr)
        {
            return arr.acosh();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto asin(const CA& arr)
        {
            return arr.asin();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto asinh(const CA& arr)
        {
            return arr.asinh();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto atan(const CA& arr)
        {
            return arr.atan();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto atanh(const CA& arr)
        {
            return arr.atanh();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto cos(const CA& arr)
        {
            return arr.cos();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto cosh(const CA& arr)
        {
            return arr.cosh();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto exp(const CA& arr)
        {
            return arr.exp();
        }
        
        template <CustomArray CA>
        [[nodiscard]] inline auto log(const CA& arr)
        {
            return arr.log();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto log10(const CA& arr)
        {
            return arr.log10();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto pow(const CA& arr)
        {
            return arr.pow();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto sin(const CA& arr)
        {
            return arr.sin();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto sinh(const CA& arr)
        {
            return arr.sinh();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto sqrt(const CA& arr)
        {
            return arr.sqrt();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto tan(const CA& arr)
        {
            return arr.tan();
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto tanh(const CA& arr)
        {
            return arr.tanh();
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator&&(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a && b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator&&(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a && b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator&&(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a && b; });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline auto operator||(const CA1& lhs, const CA2& rhs)
        {
            return transform(lhs, rhs, [](const typename CA1::ValueType& a, const typename CA2::ValueType& b) { return a || b; });
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline auto operator||(const CA& lhs, const T& rhs)
        {
            return transform(lhs, rhs, [](const typename CA::ValueType& a, const T& b) { return a || b; });
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline auto operator||(const T& lhs, const CA& rhs)
        {
            return transform(lhs, rhs, [](const T& a, const typename CA::ValueType& b) { return a || b; });
        }

        template <CustomArray CA>
        inline auto& operator++(CA& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename CA::Indexer gen(arr.header()); gen; ++gen) {
                ++arr(*gen);
            }
            return arr;
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator++(CA&& arr)
        {
            return operator++(arr);
        }

        template <CustomArray CA>
        inline auto operator++(CA& arr, int)
        {
            CA old = clone(arr);
            operator++(arr);
            return old;
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator++(CA&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <CustomArray CA>
        inline auto& operator--(CA& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename CA::Indexer gen(arr.header()); gen; ++gen) {
                --arr(*gen);
            }
            return arr;
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator--(CA&& arr)
        {
            return operator--(arr);
        }

        template <CustomArray CA>
        inline auto operator--(CA& arr, int)
        {
            CA old = clone(arr);
            operator--(arr);
            return old;
        }

        template <CustomArray CA>
        [[nodiscard]] inline auto operator--(CA&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <CustomArray CA1, CustomArray CA2, typename Binary_pred>
        [[nodiscard]] inline bool all_match(const CA1& lhs, const CA2& rhs, Binary_pred pred)
        {
            return lhs.all_match(rhs, pred);
        }

        template <CustomArray CA, typename T, typename Binary_pred>
        [[nodiscard]] inline bool all_match(const CA& lhs, const T& rhs, Binary_pred pred)
        {
            return lhs.all_match(rhs, pred);
        }

        template <typename T, CustomArray CA, typename Binary_pred>
        [[nodiscard]] inline bool all_match(const T& lhs, const CA& rhs, Binary_pred pred)
        {
            return rhs.all_match([&lhs, &pred](const typename CA::ValueType& value) { return pred(lhs, value); });
        }


        template <CustomArray CA1, CustomArray CA2, typename Binary_pred>
        [[nodiscard]] inline bool any_match(const CA1& lhs, const CA2& rhs, Binary_pred pred)
        {
            return lhs.any_match(rhs, pred);
        }

        template <CustomArray CA, typename T, typename Binary_pred>
        [[nodiscard]] inline bool any_match(const CA& lhs, const T& rhs, Binary_pred pred)
        {
            return lhs.any_match(rhs, pred);
        }

        template <typename T, CustomArray CA, typename Binary_pred>
        [[nodiscard]] inline bool any_match(const T& lhs, const CA& rhs, Binary_pred pred)
        {
            return rhs.any_match([&lhs, &pred](const typename CA::ValueType& value) { return pred(lhs, value); });
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline bool all_equal(const CA1& lhs, const CA2& rhs)
        {
            return lhs.all_equal(rhs);
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline bool all_equal(const CA& lhs, const T& rhs)
        {
            return lhs.all_equal(rhs);
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline bool all_equal(const T& lhs, const CA& rhs)
        {
            return rhs.all_equal(lhs);
        }

        template <CustomArray CA1, CustomArray CA2>
        [[nodiscard]] inline bool all_close(const CA1& lhs, const CA2& rhs, const decltype(typename CA1::ValueType{} - typename CA2::ValueType{})& atol = default_atol<decltype(typename CA1::ValueType{} - typename CA2::ValueType{}) > (), const decltype(typename CA1::ValueType{} - typename CA2::ValueType{})& rtol = default_rtol<decltype(typename CA1::ValueType{} - typename CA2::ValueType{}) > ())
        {
            return lhs.all_close(rhs, atol, rtol);
        }

        template <CustomArray CA, typename T>
        [[nodiscard]] inline bool all_close(const CA& lhs, const T& rhs, const decltype(typename CA::ValueType{} - T{})& atol = default_atol<decltype(typename CA::ValueType{} - T{}) > (), const decltype(typename CA::ValueType{} - T{})& rtol = default_rtol<decltype(typename CA::ValueType{} - T{}) > ())
        {
            return lhs.all_close(rhs, atol, rtol);
        }

        template <typename T, CustomArray CA>
        [[nodiscard]] inline bool all_close(const T& lhs, const CA& rhs, const decltype(T{} - typename CA::ValueType{})& atol = default_atol<decltype(T{} - typename CA::ValueType{}) > (), const decltype(T{} - typename CA::ValueType{})& rtol = default_rtol<decltype(T{} - typename CA::ValueType{}) > ())
        {
            return rhs.all_close(lhs, atol, rtol);
        }
    }

    using details::Array;

    using details::copy;
    using details::clone;
    using details::reshape;
    using details::resize;
    using details::append;
    using details::insert;
    using details::remove;

    using details::empty;
    using details::all_match;
    using details::any_match;
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

#endif // OC_ARRAY_H
