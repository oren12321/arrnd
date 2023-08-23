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
#include <ostream>
#include <cassert>
#include <iterator>

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
                    assert(n > 0 && "non positive allocation size");
                    auto p = reinterpret_cast<pointer>(operator new[](n * sizeof(value_type)));
                    if (!p) {
                        throw std::bad_alloc{};
                    }
                    return p;
                }

                constexpr void deallocate(pointer p, size_type n) noexcept
                {
                    assert(p && "nullptr deallocation");
                    assert(n > 0 && "non positive size deallocation");
                    operator delete[](p, n * sizeof(value_type));
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
                using reverse_iterator = std::reverse_iterator<pointer>;
                using const_reverse_iterator = std::reverse_iterator<const_pointer>;

                template <typename U>
                using replaced_type = simple_dynamic_vector<U, Allocator>;

                constexpr simple_dynamic_vector(size_type size = 0, const_pointer data = nullptr)
                    : size_(size), capacity_(size)
                {
                    assert(size_ >= 0 && "negative vector size");
                    if (size > 0) {
                        data_ptr_ = alloc_.allocate(capacity_);
                        if (data) {
                            std::uninitialized_copy_n(data, size_, data_ptr_);
                        }
                        else if constexpr (!std::is_fundamental_v<T>) {
                            std::uninitialized_default_construct_n(data_ptr_, size_);
                        }
                    }
                }

                template <typename InputIt>
                constexpr simple_dynamic_vector(InputIt first, InputIt last)
                    : simple_dynamic_vector(std::distance(first, last), &(*first)) {}

                constexpr simple_dynamic_vector(const simple_dynamic_vector& other)
                    : alloc_(other.alloc_), size_(other.size_), capacity_(other.capacity_)
                {
                    if (!other.empty()) {
                        data_ptr_ = alloc_.allocate(capacity_);
                        std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);
                    }
                }

                constexpr simple_dynamic_vector operator=(const simple_dynamic_vector& other)
                {
                    if (this == &other) {
                        return *this;
                    }

                    if (!empty()) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_, size_);
                        }
                        alloc_.deallocate(data_ptr_, capacity_);
                    }

                    alloc_ = other.alloc_;
                    size_ = other.size_;
                    capacity_ = other.capacity_;

                    if (!other.empty()) {
                        data_ptr_ = alloc_.allocate(capacity_);
                        if (data_ptr_) {
                            std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);
                        }
                    }

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

                    if (!empty()) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_, size_);
                        }
                        alloc_.deallocate(data_ptr_, capacity_);
                    }

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
                    if (!empty()) {
                        if constexpr (!std::is_fundamental_v<T>) {
                            std::destroy_n(data_ptr_, size_);
                        }
                        alloc_.deallocate(data_ptr_, capacity_);
                    }
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
                    assert(index >= 0 && "negative index");
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    assert(index >= 0 && "negative index");
                    return data_ptr_[index];
                }

                constexpr void resize(size_type new_size)
                {
                    assert(new_size >= 0 && "negative resize");
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
                        if (new_data_ptr) {
                            std::uninitialized_move_n(data_ptr_, size_, new_data_ptr);
                            std::uninitialized_default_construct_n(new_data_ptr + size_, new_size - size_);
                        }

                        if (!empty()) {
                            if constexpr (!std::is_fundamental_v<T>) {
                                std::destroy_n(data_ptr_, size_);
                            }
                            alloc_.deallocate(data_ptr_, capacity_);
                        }

                        data_ptr_ = new_data_ptr;
                        size_ = new_size;
                        capacity_ = new_capacity;
                    }
                }

                constexpr void reserve(size_type new_capacity)
                {
                    assert(new_capacity >= 0 && "negative reserve");
                    // if (new_capacity <= capacity_) do nothing
                    if (new_capacity > capacity_) {
                        pointer new_data_ptr = alloc_.allocate(new_capacity);
                        if (new_data_ptr) {
                            std::uninitialized_move_n(data_ptr_, size_, new_data_ptr);
                        }

                        alloc_.deallocate(data_ptr_, capacity_);
                        data_ptr_ = new_data_ptr;
                        capacity_ = new_capacity;
                    }
                }

                constexpr void expand(size_type count)
                {
                    assert(count >= 0 && "negative expand size");
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
                        if (data_ptr) {
                            std::uninitialized_move_n(data_ptr_, size_, data_ptr);
                            std::uninitialized_default_construct_n(data_ptr + size_, count);
                        }

                        alloc_.deallocate(data_ptr_, capacity_);
                        data_ptr_ = data_ptr;
                        capacity_ = new_capacity;
                        size_ = new_size;
                    }
                }

                constexpr void shrink(size_type count)
                {
                    assert(count <= size_ && "shrink input bigger than vector size");
                    if constexpr (!std::is_fundamental_v<T>) {
                        std::destroy_n(data_ptr_ + size_ - count, count);
                    }
                    size_ -= count;
                }

                constexpr void shrink_to_fit()
                {
                    if (capacity_ > size_) {
                        pointer data_ptr = alloc_.allocate(size_);
                        if (data_ptr) {
                            std::uninitialized_move_n(data_ptr_, size_, data_ptr);
                        }

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

                [[nodiscard]] constexpr const_pointer cbegin() const noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr const_pointer cend() const noexcept
                {
                    return data_ptr_ + size_;
                }

                [[nodsicard]] constexpr std::reverse_iterator<pointer> rbegin() noexcept
                {
                    return std::make_reverse_iterator(end());
                }

                [[nodsicard]] constexpr std::reverse_iterator<pointer> rend() noexcept
                {
                    return std::make_reverse_iterator(begin());
                }

                [[nodsicard]] constexpr std::reverse_iterator<const_pointer> crbegin() const noexcept
                {
                    return std::make_reverse_iterator(cend());
                }

                [[nodsicard]] constexpr std::reverse_iterator<const_pointer> crend() const noexcept
                {
                    return std::make_reverse_iterator(cbegin());
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
                allocator_type alloc_;

                size_type size_ = 0;
                size_type capacity_ = 0;

                pointer data_ptr_ = nullptr;
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
                using reverse_iterator = std::reverse_iterator<pointer>;
                using const_reverse_iterator = std::reverse_iterator<const_pointer>;

                template <typename U>
                using replaced_type = simple_static_vector<U, Capacity>;

                constexpr simple_static_vector(size_type size = 0, const_pointer data = nullptr)
                    : size_(size)
                {
                    assert(size_ <= Capacity && "invalid vector size");
                    if (data) {
                        std::copy(data, std::next(data, size), data_ptr_);
                    }
                }

                template <typename InputIt>
                constexpr simple_static_vector(InputIt first, InputIt last)
                    : simple_static_vector(std::distance(first, last), &(*first)) {}

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
                    assert(index >= 0 && "invalid index");
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    assert(index >= 0 && "invalid index");
                    return data_ptr_[index];
                }

                constexpr void resize(size_type new_size)
                {
                    assert(new_size <= Capacity && "invalid resize input");
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
                    assert(size_ + count <= Capacity && "invalid exapnd size");
                    size_ += count;
                }

                constexpr void shrink(size_type count)
                {
                    assert(count <= size_ && "invalid shrink size");
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

                [[nodiscard]] constexpr const_pointer cbegin() const noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr const_pointer cend() const noexcept
                {
                    return data_ptr_ + size_;
                }

                [[nodsicard]] constexpr std::reverse_iterator<pointer> rbegin() noexcept
                {
                    return std::make_reverse_iterator(end());
                }

                [[nodsicard]] constexpr std::reverse_iterator<pointer> rend() noexcept
                {
                    return std::make_reverse_iterator(begin());
                }

                [[nodsicard]] constexpr std::reverse_iterator<const_pointer> crbegin() const noexcept
                {
                    return std::make_reverse_iterator(cend());
                }

                [[nodsicard]] constexpr std::reverse_iterator<const_pointer> crend() const noexcept
                {
                    return std::make_reverse_iterator(cbegin());
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

                size_type size_ = 0;
        };
    }

    namespace details {
        struct arrnd_tag {};

        template <typename T>
        concept arrnd_complient = std::is_same_v<typename T::tag, arrnd_tag>;
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

        template <typename T1, typename T2> requires (!arrnd_complient<T1> && !arrnd_complient<T2>)
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
        struct interval {
            constexpr interval(const T& nstart, const T& nstop, const T& nstep) noexcept
                : start(nstart), stop(nstop), step(nstep) {}

            constexpr interval(const T& nstart, const T& nstop) noexcept
                : interval(nstart, nstop, 1) {}

            constexpr interval(const T& nstart) noexcept
                : interval(nstart, nstart, 1) {}

            constexpr interval() = default;
            constexpr interval(const interval&) = default;
            constexpr interval& operator=(const interval&) = default;
            constexpr interval(interval&) = default;
            constexpr interval& operator=(interval&) = default;

            T start{ 0 };
            T stop{ 0 };
            T step{ 1 };

            [[nodicsard]] static constexpr interval full() noexcept {
                return interval{ std::numeric_limits<T>::min(), std::numeric_limits<T>::max() };
            }

            [[nodiscard]] static constexpr interval from(T nstart) {
                return interval{ nstart, std::numeric_limits<T>::max() };
            }

            [[nodicsard]] static constexpr interval to(T nstop) noexcept {
                return interval{ std::numeric_limits<T>::min(), nstop };
            }

            [[nodicsard]] static constexpr interval at(T npos) noexcept {
                return interval{ npos, npos };
            }

            [[nodiscard]] static constexpr interval between(T nstart, T nstop, T nstep = 1) noexcept {
                return interval{ nstart, nstop, nstep };
            }
        };

        template <std::integral T>
        [[nodiscard]] inline constexpr interval<T> reverse(const interval<T>& i) noexcept
        {
            return { i.stop, i.start, -i.step };
        }

        template <std::integral T>
        [[nodiscard]] inline constexpr interval<T> modulo(const interval<T>& i, const T& modulus) noexcept
        {
            T nstart = i.start == std::numeric_limits<T>::min() ? 0 : i.start;
            T nstop = i.stop == std::numeric_limits<T>::max() ? modulus - 1 : i.stop;

            return { modulo(nstart, modulus), modulo(nstop, modulus), i.step };
        }

        template <std::integral T>
        [[nodiscard]] inline constexpr interval<T> forward(const interval<T>& i) noexcept
        {
            return i.step < T{ 0 } ? reverse(i) : i;
        }
    }

    using details::interval;

    using details::modulo;

    using details::reverse;
    using details::forward;




    namespace details {

        template <typename T, typename U>
        [[nodiscard]] inline constexpr bool operator==(const std::span<T>& lhs, const std::span<U>& rhs) {
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
        [[nodiscard]] inline constexpr std::int64_t numel(std::span<const std::int64_t> dims) noexcept
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
        inline constexpr std::int64_t compute_strides(std::span<const std::int64_t> dims, std::span<std::int64_t> strides) noexcept
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
        * @note When number of ranges is smaller than number of strides, the other strides computed from previous dimensions.
        */
        inline constexpr std::int64_t compute_strides(std::span<const std::int64_t> previous_dims, std::span<const std::int64_t> previous_strides, std::span<const interval<std::int64_t>> ranges, std::span<std::int64_t> strides) noexcept
        {
            std::int64_t nstrides{ std::ssize(previous_strides) > std::ssize(strides) ? std::ssize(strides) : std::ssize(previous_strides) };
            if (nstrides <= 0) {
                return 0;
            }

            std::int64_t ncomp_from_ranges{ nstrides > std::ssize(ranges) ? std::ssize(ranges) : nstrides };

            // compute strides with range step
            for (std::int64_t i = 0; i < ncomp_from_ranges; ++i) {
                strides[i] = previous_strides[i] * forward(ranges[i]).step;
            }

            // set strides from previous strides
            if (std::ssize(previous_strides) > ncomp_from_ranges) {
                for (std::int64_t i = ncomp_from_ranges; i < std::ssize(previous_strides); ++i) {
                    strides[i] = previous_strides[i];
                }
            }

            std::int64_t nstrides_calc =
                (std::ssize(previous_strides) > ncomp_from_ranges ? std::ssize(previous_strides) : ncomp_from_ranges);

            // compute strides from previous dimensions
            if (nstrides_calc < std::ssize(previous_dims) && nstrides >= std::ssize(previous_dims)) {
                strides[std::ssize(previous_dims) - 1] = 1;
                for (std::int64_t i = std::ssize(previous_dims) - 2; i >= nstrides_calc - 1; --i) {
                    strides[i] = strides[i + 1] * previous_dims[i + 1];
                }
            }

            return nstrides;
        }

        /**
        * @param[out] dims An already allocated memory for computed dimensions.
        * @return Number of computed dimensions
        * @note Previous dimensions are used in case of small number of ranges.
        */
        inline constexpr std::int64_t compute_dims(std::span<const std::int64_t> previous_dims, std::span<const interval<std::int64_t>> ranges, std::span<std::int64_t> dims) noexcept
        {
            std::int64_t ndims{ std::ssize(previous_dims) > std::ssize(dims) ? std::ssize(dims) : std::ssize(previous_dims) };
            if (ndims <= 0) {
                return 0;
            }

            std::int64_t num_computed_dims{ ndims > std::ssize(ranges) ? std::ssize(ranges) : ndims };

            for (std::int64_t i = 0; i < num_computed_dims; ++i) {
                interval<std::int64_t> range{ forward(modulo(ranges[i], previous_dims[i])) };
                if (range.start > range.stop || range.step <= 0) {
                    return 0;
                }
                dims[i] = static_cast<std::int64_t>(std::ceil((range.stop - range.start + 1.0) / range.step));
            }

            for (std::int64_t i = num_computed_dims; i < ndims; ++i) {
                dims[i] = previous_dims[i];
            }

            return ndims;
        }

        [[nodiscard]] inline constexpr std::int64_t compute_offset(std::span<const std::int64_t> previous_dims, std::int64_t previous_offset, std::span<const std::int64_t> previous_strides, std::span<const interval<std::int64_t>> ranges) noexcept
        {
            std::int64_t offset{ previous_offset };

            if (previous_dims.empty() || previous_strides.empty() || ranges.empty()) {
                return offset;
            }

            std::int64_t num_computations{ std::ssize(previous_dims) > std::ssize(previous_strides) ? std::ssize(previous_strides) : std::ssize(previous_dims) };
            num_computations = (num_computations > std::ssize(ranges) ? std::ssize(ranges) : num_computations);

            for (std::int64_t i = 0; i < num_computations; ++i) {
                offset += previous_strides[i] * forward(modulo(ranges[i], previous_dims[i])).start;
            }
            return offset;
        }

        /**
        * @note Extra subscripts are ignored. If number of subscripts are less than number of strides/dimensions, they are considered as the less significant subscripts.
        */
        [[nodiscard]] inline constexpr std::int64_t subs2ind(std::int64_t offset, std::span<const std::int64_t> strides, std::span<const std::int64_t> dims, std::span<std::int64_t> subs) noexcept
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

        template <typename Iter>
        using iter_value_type = typename std::iterator_traits<Iter>::value_type;

        template <typename Storage = simple_dynamic_vector<std::int64_t>>
        class arrnd_header {
        public:
            using storage_type = Storage;

            constexpr arrnd_header() = default;

            template <typename InputIt>
            constexpr arrnd_header(InputIt first_dim, InputIt last_dim)
            {
                assert(first_dim <= last_dim);
                assert(std::all_of(first_dim, last_dim, [](auto d) { return d >= 0; }));

                if (first_dim == last_dim) {
                    return;
                }

                count_ = std::reduce(first_dim, last_dim, iter_value_type<InputIt>{1}, std::multiplies<>{});
                if (count_ == 0) {
                    return;
                }

                dims_ = storage_type(first_dim, last_dim);

                strides_ = storage_type(dims_.size());
                std::exclusive_scan(dims_.crbegin(), dims_.crend(), strides_.rbegin(), typename storage_type::value_type{ 1 }, std::multiplies<>{});

                last_index_ = std::inner_product(dims_.cbegin(), dims_.cend(), strides_.cbegin(), typename storage_type::value_type{ 0 },
                    std::plus<>{}, [](auto d, auto s) { return (d - 1) * s; });
            }

            constexpr arrnd_header(std::span<const std::int64_t> dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = storage_type(dims.begin(), dims.end());

                strides_ = storage_type(dims.size());
                compute_strides(dims, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, std::span<const interval<std::int64_t>> ranges)
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                if (compute_dims(previous_hdr.dims(), ranges, dims) <= 0) {
                    return;
                }

                dims_ = std::move(dims);
                
                count_ = numel(dims_);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(previous_hdr.dims(), previous_hdr.strides(), ranges, strides_);

                offset_ = compute_offset(previous_hdr.dims(), previous_hdr.offset(), previous_hdr.strides(), ranges);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });

                is_subarray_ = previous_hdr.is_subarray() || !std::equal(previous_hdr.dims().begin(), previous_hdr.dims().end(), dims_.begin());
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, interval<std::int64_t> range)
                : arrnd_header(previous_hdr, std::span<interval<std::int64_t>>(&range, 1))
            {
                if (empty()) {
                    return;
                }

                auto fixed_ival = modulo(range, dims_[0]);

                if (dims_[0] == 1) {
                    offset_ += fixed_ival.start * strides_[0];
                    dims_ = storage_type(dims_.cbegin() + 1, dims_.cend());
                    strides_ = storage_type(strides_.cbegin() + 1, strides_.cend());
                    last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                        [](auto a, auto b) { return a + b; },
                        [](auto a, auto b) { return (a - 1) * b; });
                    is_subarray_ = true;
                }
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, std::int64_t omitted_axis)
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
                    for (std::int64_t i = axis + 1; i < std::ssize(previous_hdr.dims()); ++i) {
                        dims_[i - 1] = previous_hdr.dims()[i];
                    }
                }
                else {
                    dims_[0] = 1;
                }

                strides_ = storage_type(ndims);
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, std::span<const std::int64_t> new_order)
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

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, std::int64_t count, std::int64_t axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_hdr.dims())) };
                for (std::int64_t i = 0; i < std::ssize(previous_hdr.dims()); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + count;
                }
                
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            constexpr arrnd_header(const arrnd_header& previous_hdr, std::span<const std::int64_t> appended_dims, std::int64_t axis)
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
                for (std::int64_t i = 0; i < std::ssize(previous_hdr.dims()); ++i) {
                    if (i != fixed_axis && previous_hdr.dims()[i] != appended_dims[i]) {
                        are_dims_valid_for_append = false;
                    }
                }
                if (!are_dims_valid_for_append) {
                    return;
                }

                storage_type dims = storage_type(previous_hdr.dims().size());

                for (std::int64_t i = 0; i < std::ssize(previous_hdr.dims()); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + appended_dims[fixed_axis];
                }

                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = storage_type(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), std::int64_t{ 0 },
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            constexpr arrnd_header(arrnd_header&& other) = default;
            constexpr arrnd_header& operator=(arrnd_header&& other) = default;

            constexpr arrnd_header(const arrnd_header& other) = default;
            constexpr arrnd_header& operator=(const arrnd_header& other) = default;

            virtual constexpr ~arrnd_header() = default;

            [[nodiscard]] constexpr std::int64_t count() const noexcept
            {
                return count_;
            }

            [[nodiscard]] constexpr std::span<const std::int64_t> dims() const noexcept
            {
                return std::span<const std::int64_t>(dims_.data(), dims_.size());
            }

            [[nodiscard]] constexpr std::span<const std::int64_t> strides() const noexcept
            {
                return std::span<const std::int64_t>(strides_.data(), strides_.size());
            }

            [[nodiscard]] constexpr std::int64_t offset() const noexcept
            {
                return offset_;
            }

            [[nodiscard]] constexpr bool is_subarray() const noexcept
            {
                return is_subarray_;
            }

            [[nodiscard]] constexpr bool empty() const noexcept
            {
                return dims_.empty();
            }

            [[nodiscard]] constexpr std::int64_t last_index() const noexcept
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
                last_first_diff_ = static_cast<std::uint64_t>(last_index_ - first_index_);

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
                std::int64_t size = std::min(std::ssize(vec), std::ssize(indices));
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
                for (std::int64_t i = 0; i < std::ssize(dims); ++i) {
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

                if (rndims != std::ssize(dims)) {
                    rdims.resize(rndims);
                    rstrides.resize(rndims);
                }

                return reds;
            }

            storage_type dims_;
            storage_type strides_;
            std::int64_t first_index_;
            std::int64_t last_index_;
            std::uint64_t last_first_diff_;
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
                    std::accumulate(hdr.dims().begin(), hdr.dims().begin() + axis + 1, std::int64_t{ 1 }, std::multiplies<>{}) / num_super_groups_;
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
                return static_cast<std::uint64_t>(current_index_) <= static_cast<std::uint64_t>(last_index_);
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

        template <typename Arrnd>
        class arrnd_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd::value_type;
            using pointer = Arrnd::value_type*;
            using reference = Arrnd::value_type&;

            using indexer_type = Arrnd::indexer_type;

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




        template <typename Arrnd>
        class arrnd_const_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd::value_type;
            using pointer = Arrnd::value_type*;
            using reference = Arrnd::value_type&;

            using indexer_type = Arrnd::indexer_type;

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



        template <typename Arrnd>
        class arrnd_reverse_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd::value_type;
            using pointer = Arrnd::value_type*;
            using reference = Arrnd::value_type&;

            using indexer_type = Arrnd::indexer_type;

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




        template <typename Arrnd>
        class arrnd_const_reverse_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd::value_type;
            using pointer = Arrnd::value_type*;
            using reference = Arrnd::value_type&;

            using indexer_type = Arrnd::indexer_type;

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


        template <typename Arrnd>
        class arrnd_axis_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd;
            using reference = Arrnd&;

            using storage_type = Arrnd::header_type::storage_type::template replaced_type<interval<std::int64_t>>;

            constexpr arrnd_axis_iterator(const value_type& arrnd_ref, std::int64_t fixed_axis, std::int64_t start_index)
                : arrnd_ref_(arrnd_ref)
            {
                fixed_axis_ = fixed_axis;
                if (fixed_axis < 0) {
                    fixed_axis_ = 0;
                }
                else if (fixed_axis >= std::ssize(arrnd_ref.header().dims())) {
                    fixed_axis_ = std::ssize(arrnd_ref.header().dims()) - 1;
                }

                current_index_ = start_index;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                else if (current_index_ > arrnd_ref.header().dims()[fixed_axis_]) {
                    current_index_ = arrnd_ref.header().dims()[fixed_axis_];
                }

                last_index_ = arrnd_ref.header().dims()[fixed_axis_];

                ranges_ = storage_type(std::ssize(arrnd_ref.header().dims()));
                for (std::int64_t i = 0; i < std::ssize(arrnd_ref.header().dims()); ++i) {
                    ranges_[i] = { 0, arrnd_ref.header().dims()[i] - 1 };
                }
                ranges_[fixed_axis_] = { current_index_, current_index_ };
                if (current_index_ >= 0 && current_index_ < arrnd_ref.header().dims()[fixed_axis_]) {
                    slice_ = arrnd_ref[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                }
            }

            constexpr arrnd_axis_iterator() = default;

            constexpr arrnd_axis_iterator(const arrnd_axis_iterator& other) = default;
            constexpr arrnd_axis_iterator& operator=(const arrnd_axis_iterator& other) = default;

            constexpr arrnd_axis_iterator(arrnd_axis_iterator&& other) noexcept = default;
            constexpr arrnd_axis_iterator& operator=(arrnd_axis_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_iterator() = default;

            constexpr arrnd_axis_iterator& operator++()
            {
                ++current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_iterator operator++(int) noexcept
            {
                arrnd_axis_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_iterator& operator+=(difference_type count)
            {
                current_index_ += count;
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                if (current_index_ >= last_index_) {
                    return *this;
                }
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_iterator operator+(difference_type count)
            {
                arrnd_axis_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_iterator& operator--()
            {
                --current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_iterator operator--(int)
            {
                arrnd_axis_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_iterator& operator-=(difference_type count)
            {
                current_index_ -= count;
                if (current_index_ < 0) {
                    current_index_  = -1;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                if (current_index_ < 0) {
                    return *this;
                }
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_iterator operator-(difference_type count)
            {
                arrnd_axis_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() noexcept
            {
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_iterator& iter) const noexcept
            {
                return current_index_ == iter.current_index_;
            }

        private:
            value_type arrnd_ref_;
            std::int64_t fixed_axis_;
            std::int64_t current_index_;
            std::int64_t last_index_;
            storage_type ranges_;

            value_type slice_;
        };

        template <typename Arrnd>
        class arrnd_axis_const_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd;
            using const_reference = const Arrnd&;

            using storage_type = Arrnd::header_type::storage_type::template replaced_type<interval<std::int64_t>>;

            constexpr arrnd_axis_const_iterator(const value_type& arrnd_ref, std::int64_t fixed_axis, std::int64_t start_index)
                : arrnd_ref_(arrnd_ref)
            {
                fixed_axis_ = fixed_axis;
                if (fixed_axis < 0) {
                    fixed_axis_ = 0;
                }
                else if (fixed_axis >= std::ssize(arrnd_ref.header().dims())) {
                    fixed_axis_ = std::ssize(arrnd_ref.header().dims()) - 1;
                }

                current_index_ = start_index;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                else if (current_index_ > arrnd_ref.header().dims()[fixed_axis_]) {
                    current_index_ = arrnd_ref.header().dims()[fixed_axis_];
                }

                last_index_ = arrnd_ref.header().dims()[fixed_axis_];

                ranges_ = storage_type(std::ssize(arrnd_ref.header().dims()));
                for (std::int64_t i = 0; i < std::ssize(arrnd_ref.header().dims()); ++i) {
                    ranges_[i] = { 0, arrnd_ref.header().dims()[i] - 1 };
                }
                ranges_[fixed_axis_] = { current_index_, current_index_ };
                if (current_index_ >= 0 && current_index_ < arrnd_ref.header().dims()[fixed_axis_]) {
                    slice_ = arrnd_ref[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                }
            }

            constexpr arrnd_axis_const_iterator() = default;

            constexpr arrnd_axis_const_iterator(const arrnd_axis_const_iterator& other) = default;
            constexpr arrnd_axis_const_iterator& operator=(const arrnd_axis_const_iterator& other) = default;

            constexpr arrnd_axis_const_iterator(arrnd_axis_const_iterator&& other) noexcept = default;
            constexpr arrnd_axis_const_iterator& operator=(arrnd_axis_const_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_const_iterator() = default;

            constexpr arrnd_axis_const_iterator& operator++()
            {
                ++current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator++(int)
            {
                arrnd_axis_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator+=(difference_type count)
            {
                current_index_ += count;
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator+(difference_type count)
            {
                arrnd_axis_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator--()
            {
                --current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator--(int)
            {
                arrnd_axis_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator-=(difference_type count)
            {
                current_index_ -= count;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator-(difference_type count)
            {
                arrnd_axis_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const_reference operator*() const noexcept
            {
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_const_iterator& iter) const noexcept
            {
                return current_index_ == iter.current_index_;
            }

        private:
            value_type arrnd_ref_;
            std::int64_t fixed_axis_;
            std::int64_t current_index_;
            std::int64_t last_index_;
            storage_type ranges_;

            value_type slice_;
        };



        template <typename Arrnd>
        class arrnd_axis_reverse_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd;
            using reference = Arrnd&;

            using storage_type = Arrnd::header_type::storage_type::template replaced_type<interval<std::int64_t>>;

            constexpr arrnd_axis_reverse_iterator(const value_type& arrnd_ref, std::int64_t fixed_axis, std::int64_t start_index)
                : arrnd_ref_(arrnd_ref)
            {
                fixed_axis_ = fixed_axis;
                if (fixed_axis < 0) {
                    fixed_axis_ = 0;
                }
                else if (fixed_axis >= std::ssize(arrnd_ref.header().dims())) {
                    fixed_axis_ = std::ssize(arrnd_ref.header().dims()) - 1;
                }

                current_index_ = start_index;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                else if (current_index_ > arrnd_ref.header().dims()[fixed_axis_]) {
                    current_index_ = arrnd_ref.header().dims()[fixed_axis_];
                }

                last_index_ = arrnd_ref.header().dims()[fixed_axis_];

                ranges_ = storage_type(std::ssize(arrnd_ref.header().dims()));
                for (std::int64_t i = 0; i < std::ssize(arrnd_ref.header().dims()); ++i) {
                    ranges_[i] = { 0, arrnd_ref.header().dims()[i] - 1 };
                }
                ranges_[fixed_axis_] = { current_index_, current_index_ };
                if (current_index_ >= 0 && current_index_ < arrnd_ref.header().dims()[fixed_axis_]) {
                    slice_ = arrnd_ref[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                }
            }

            constexpr arrnd_axis_reverse_iterator() = default;

            constexpr arrnd_axis_reverse_iterator(const arrnd_axis_reverse_iterator& other) = default;
            constexpr arrnd_axis_reverse_iterator& operator=(const arrnd_axis_reverse_iterator& other) = default;

            constexpr arrnd_axis_reverse_iterator(arrnd_axis_reverse_iterator&& other) noexcept = default;
            constexpr arrnd_axis_reverse_iterator& operator=(arrnd_axis_reverse_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_reverse_iterator() = default;

            constexpr arrnd_axis_reverse_iterator& operator--()
            {
                ++current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator--(int) noexcept
            {
                arrnd_axis_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator-=(difference_type count)
            {
                current_index_ += count;
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                if (current_index_ >= last_index_) {
                    return *this;
                }
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator-(difference_type count)
            {
                arrnd_axis_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator++()
            {
                --current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator++(int)
            {
                arrnd_axis_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator+=(difference_type count)
            {
                current_index_ -= count;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                if (current_index_ < 0) {
                    return *this;
                }
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator+(difference_type count)
            {
                arrnd_axis_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() noexcept
            {
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_iterator& iter) const noexcept
            {
                return current_index_ == iter.current_index_;
            }

        private:
            value_type arrnd_ref_;
            std::int64_t fixed_axis_;
            std::int64_t current_index_;
            std::int64_t last_index_;
            storage_type ranges_;

            value_type slice_;
        };

        template <typename Arrnd>
        class arrnd_axis_reverse_const_iterator final
        {
        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type = std::int64_t;
            using value_type = Arrnd;
            using const_reference = const Arrnd&;

            using storage_type = Arrnd::header_type::storage_type::template replaced_type<interval<std::int64_t>>;

            constexpr arrnd_axis_reverse_const_iterator(const value_type& arrnd_ref, std::int64_t fixed_axis, std::int64_t start_index)
                : arrnd_ref_(arrnd_ref)
            {
                fixed_axis_ = fixed_axis;
                if (fixed_axis < 0) {
                    fixed_axis_ = 0;
                }
                else if (fixed_axis >= std::ssize(arrnd_ref.header().dims())) {
                    fixed_axis_ = std::ssize(arrnd_ref.header().dims()) - 1;
                }

                current_index_ = start_index;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                else if (current_index_ > arrnd_ref.header().dims()[fixed_axis_]) {
                    current_index_ = arrnd_ref.header().dims()[fixed_axis_];
                }

                last_index_ = arrnd_ref.header().dims()[fixed_axis_];

                ranges_ = storage_type(std::ssize(arrnd_ref.header().dims()));
                for (std::int64_t i = 0; i < std::ssize(arrnd_ref.header().dims()); ++i) {
                    ranges_[i] = { 0, arrnd_ref.header().dims()[i] - 1 };
                }
                ranges_[fixed_axis_] = { current_index_, current_index_ };
                if (current_index_ >= 0 && current_index_ < arrnd_ref.header().dims()[fixed_axis_]) {
                    slice_ = arrnd_ref[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                }
            }

            constexpr arrnd_axis_reverse_const_iterator() = default;

            constexpr arrnd_axis_reverse_const_iterator(const arrnd_axis_reverse_const_iterator& other) = default;
            constexpr arrnd_axis_reverse_const_iterator& operator=(const arrnd_axis_reverse_const_iterator& other) = default;

            constexpr arrnd_axis_reverse_const_iterator(arrnd_axis_reverse_const_iterator&& other) noexcept = default;
            constexpr arrnd_axis_reverse_const_iterator& operator=(arrnd_axis_reverse_const_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_reverse_const_iterator() = default;

            constexpr arrnd_axis_reverse_const_iterator& operator--()
            {
                ++current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator--(int)
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator-=(difference_type count)
            {
                current_index_ += count;
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator-(difference_type count)
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator++()
            {
                --current_index_;
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator++(int)
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator+=(difference_type count)
            {
                current_index_ -= count;
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                ranges_[fixed_axis_] = interval<std::int64_t>{ current_index_ , current_index_ };
                slice_ = arrnd_ref_[std::span<interval<std::int64_t>>(ranges_.data(), ranges_.size())];
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator+(difference_type count)
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const_reference operator*() const noexcept
            {
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_const_iterator& iter) const noexcept
            {
                return current_index_ == iter.current_index_;
            }

        private:
            value_type arrnd_ref_;
            std::int64_t fixed_axis_;
            std::int64_t current_index_;
            std::int64_t last_index_;
            storage_type ranges_;

            value_type slice_;
        };



        template <typename T, typename Storage = simple_dynamic_vector<T>, template<typename> typename SharedRefAllocator = lightweight_allocator, typename Header = arrnd_header<>, typename Indexer = arrnd_general_indexer<>>
        class arrnd {
        public:
            using value_type = T;
            using size_type = std::int64_t;
            using difference_type = std::int64_t;
            using reference = T&;
            using const_reference = const T&;
            using pointer = T*;
            using const_pointer = const T*;

            using tag = arrnd_tag;

            using storage_type = Storage;
            template <typename U>
            using shared_ref_allocator_type = SharedRefAllocator<T>;
            using header_type = Header;
            using indexer_type = Indexer;

            using this_type = arrnd<T, Storage, SharedRefAllocator, Header, Indexer>;
            template <typename U>
            using replaced_type = arrnd<U, typename Storage::template replaced_type<U>, SharedRefAllocator, Header, Indexer>;

            template <typename U>
            using shared_ref = U;
            template <typename U>
            using maybe_shared_ref = U;

            using iterator = arrnd_iterator<this_type>;
            using const_iterator = arrnd_const_iterator<this_type>;
            using reverse_iterator = arrnd_reverse_iterator<this_type>;
            using const_reverse_iterator = arrnd_const_reverse_iterator<this_type>;

            using subarray_iterator = arrnd_axis_iterator<this_type>;
            using const_subarray_iterator = arrnd_axis_const_iterator<this_type>;
            using reverse_subarray_iterator = arrnd_axis_reverse_iterator<this_type>;
            using const_reverse_subarray_iterator = arrnd_axis_reverse_const_iterator<this_type>;

            constexpr arrnd() = default;

            constexpr arrnd(arrnd&& other) = default;
            template<arrnd_complient ArCo>
            constexpr arrnd(ArCo&& other)
                : arrnd(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy_from(other);

                ArCo dummy{ std::move(other) };
            }
            constexpr arrnd& operator=(arrnd&& other) & = default;
            constexpr arrnd& operator=(arrnd&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy_from(other);
                arrnd dummy{ std::move(other) };
                return *this;
            }
            template<arrnd_complient ArCo>
            constexpr arrnd& operator=(ArCo&& other)&
            {
                *this = this_type(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy_from(other);
                ArCo dummy{ std::move(other) };
                return *this;
            }
            template<arrnd_complient ArCo>
            constexpr arrnd& operator=(ArCo&& other)&&
            {
                copy_from(other);
                ArCo dummy{ std::move(other) };
                return *this;
            }

            constexpr arrnd(const arrnd& other) = default;
            template<arrnd_complient ArCo>
            constexpr arrnd(const ArCo& other)
                : arrnd(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy_from(other);
            }
            constexpr arrnd& operator=(const arrnd& other) & = default;
            constexpr arrnd& operator=(const arrnd& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy_from(other);
                return *this;
            }
            template<arrnd_complient ArCo>
            constexpr arrnd& operator=(const ArCo& other)&
            {
                *this = this_type(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy_from(other);
                return *this;
            }
            template<arrnd_complient ArCo>
            constexpr arrnd& operator=(const ArCo& other)&&
            {
                copy_from(other);
                return *this;
            }

            template <typename U>
            constexpr arrnd& operator=(const U& value)
            {
                if (empty()) {
                    return *this;
                }

                for (indexer_type gen(hdr_); gen; ++gen) {
                    (*this)[*gen] = value;
                }

                return *this;
            }

            virtual constexpr ~arrnd() = default;


            template <typename InputDimsIt, typename InputDataIt>
            requires (std::input_iterator<InputDimsIt> && std::input_iterator<InputDataIt>)
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, InputDataIt first_data)
                : hdr_(first_dim, last_dim), buffsp_(std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                std::copy(first_data, first_data + hdr_.count(), buffsp_->data());
            }
            template <typename InputDataIt>
            requires std::input_iterator<InputDataIt>
            explicit constexpr arrnd(std::span<const size_type> dims, InputDataIt first_data)
                : arrnd(dims.begin(), dims.end(), first_data)
            {
            }
            template <typename InputDataIt>
            requires std::input_iterator<InputDataIt>
            explicit constexpr arrnd(std::initializer_list<size_type> dims, InputDataIt first_data)
                : arrnd(dims.begin(), dims.end(), first_data)
            {
            }
            template <typename InputDimsIt, typename U>
            requires (std::input_iterator<InputDimsIt> && !(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, std::initializer_list<U> data)
                : arrnd(first_dim, last_dim, data.begin())
            {
            }
            template <typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(std::span<const size_type> dims, std::initializer_list<U> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }
            template <typename U>
            requires !(std::is_pointer_v<U> || std::is_array_v<U>)
            explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<U> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const_pointer first_data)
                : hdr_(first_dim, last_dim), buffsp_(std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count(), first_data))
            {
            }
            explicit constexpr arrnd(std::span<const size_type> dims, const_pointer first_data)
                : arrnd(dims.begin(), dims.end(), first_data)
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims, const_pointer first_data)
                : arrnd(dims.begin(), dims.end(), first_data)
            {
            }
            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, std::initializer_list<T> data)
                : arrnd(first_dim, last_dim, data.begin())
            {
            }
            explicit constexpr arrnd(std::span<const size_type> dims, std::initializer_list<T> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<T> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim)
                : hdr_(first_dim, last_dim), buffsp_(std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
            }
            explicit constexpr arrnd(std::span<const size_type> dims)
                : arrnd(dims.begin(), dims.end())
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims)
                : arrnd(dims.begin(), dims.end())
            {
            }

            template <typename InputDimsIt, typename U>
            requires (std::input_iterator<InputDimsIt> && !(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const U& value)
                : hdr_(first_dim, last_dim), buffsp_(std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                std::fill(buffsp_->begin(), buffsp_->end(), value);
            }
            template <typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(std::span<const size_type> dims, const U& value)
                : arrnd(dims.begin(), dims.end(), value)
            {
            }
            template <typename U>
            requires !(std::is_pointer_v<U> || std::is_array_v<U>)
            explicit constexpr arrnd(std::initializer_list<size_type> dims, const U& value)
                : arrnd(dims.begin(), dims.end(), value)
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const_reference value)
                : hdr_(first_dim, last_dim), buffsp_(std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                std::fill(buffsp_->begin(), buffsp_->end(), value);
            }
            explicit constexpr arrnd(std::span<const size_type> dims, const_reference value)
                : arrnd(dims.begin(), dims.end(), value)
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims, const_reference value)
                : arrnd(dims.begin(), dims.end(), value)
            {
            }

            [[nodiscard]] constexpr const header_type& header() const noexcept
            {
                return hdr_;
            }

            [[nodiscard]] constexpr header_type& header() noexcept
            {
                return hdr_;
            }

            [[nodiscard]] constexpr pointer data() const noexcept
            {
                return buffsp_ ? buffsp_->data() : nullptr;
            }

            [[nodiscard]] constexpr auto size() const noexcept
            {
                return hdr_.size();
            }

            [[nodiscard]] constexpr auto dims() const noexcept
            {
                return hdr_.dims();
            }

            [[nodiscard]] constexpr const_reference operator[](std::int64_t index) const noexcept
            {
                return buffsp_->data()[modulo(index, hdr_.last_index() + 1)];
            }
            [[nodiscard]] constexpr reference operator[](std::int64_t index) noexcept
            {
                return buffsp_->data()[modulo(index, hdr_.last_index() + 1)];
            }

            [[nodiscard]] constexpr const_reference operator[](std::span<std::int64_t> subs) const noexcept
            {
                return buffsp_->data()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] constexpr const_reference operator[](std::initializer_list<std::int64_t> subs) const noexcept
            {
                return (*this)[std::span<std::int64_t>{ const_cast<std::int64_t*>(subs.begin()), subs.size() }];
            }

            [[nodiscard]] constexpr reference operator[](std::span<std::int64_t> subs) noexcept
            {
                return buffsp_->data()[subs2ind(hdr_.offset(), hdr_.strides(), hdr_.dims(), subs)];
            }
            [[nodiscard]] constexpr reference operator[](std::initializer_list<std::int64_t> subs) noexcept
            {
                return (*this)[std::span<std::int64_t>{ const_cast<std::int64_t*>(subs.begin()), subs.size() }];
            }

            [[nodiscard]] constexpr shared_ref<this_type> operator[](std::span<const interval<std::int64_t>> ranges) const
            {
                if (ranges.empty() || empty()) {
                    return *this;
                }

                this_type slice{};
                slice.hdr_ = header_type{ hdr_, ranges };
                slice.buffsp_ = slice.hdr_.empty() ? nullptr : buffsp_;
                return slice;
            }
            [[nodiscard]] constexpr shared_ref<this_type> operator[](std::initializer_list<interval<std::int64_t>> ranges) const
            {
                return (*this)[std::span<const interval<std::int64_t>>{ranges.begin(), ranges.size()}];
            }

            [[nodiscard]] constexpr shared_ref<this_type> operator[](interval<std::int64_t> range) const
            {
                if (empty() || std::ssize(header().dims()) == 1) {
                    return *this;
                }

                this_type slice{};
                slice.hdr_ = header_type{ hdr_, range };
                slice.buffsp_ = slice.hdr_.empty() ? nullptr : buffsp_;
                return slice;
            }

            template <arrnd_complient ArCo> requires std::is_integral_v<typename ArCo::value_type>
            [[nodiscard]] constexpr this_type operator[](const ArCo& indices) const noexcept
            {
                this_type res(std::span<const std::int64_t>(indices.header().dims().data(), indices.header().dims().size()));

                indexer_type res_gen(res.header());
                typename ArCo::indexer_type ind_gen(indices.header());

                for (; res_gen && ind_gen; ++res_gen, ++ind_gen) {
                    res[*res_gen] = (*this)[indices[*ind_gen]];
                }

                return res;
            }

            [[nodiscard]] constexpr bool empty() const noexcept
            {
                return !data() && header().empty();
            }

            /**
            * @note copy this array to dst partially or full depending on the arrays size
            */
            template <arrnd_complient ArCo>
            constexpr const this_type& copy_to(ArCo& dst) const
            {
                if (empty() || dst.empty()) {
                    return *this;
                }

                indexer_type gen(header());
                typename ArCo::indexer_type dst_gen(dst.header());

                for (; gen && dst_gen; ++gen, ++dst_gen) {
                    dst[*dst_gen] = (*this)[*gen];
                }
                
                return *this;
            }

            template <arrnd_complient ArCo1, arrnd_complient ArCo2> requires std::is_integral_v<typename ArCo2::value_type>
            constexpr const this_type& copy_to(ArCo1& dst, const ArCo2& indices) const
            {
                if (empty() || dst.empty() || indices.empty()) {
                    return *this;
                }

                indexer_type gen(header());
                typename ArCo2::indexer_type ind_gen(indices.header());

                for (;gen && ind_gen; ++gen, ++ind_gen) {
                    dst[indices[*ind_gen]] = (*this)[*gen];
                }

                return *this;
            }

            template <arrnd_complient ArCo>
            constexpr const this_type& copy_to(ArCo& dst, std::span<const interval<std::int64_t>> ranges) const
            {
                auto slice = dst[ranges];
                copy_to(slice);
                return *this;
            }
            template <arrnd_complient ArCo>
            constexpr const this_type& copy_to(ArCo& dst, std::initializer_list<interval<std::int64_t>> ranges) const
            {
                return copy_to(dst, std::span<const interval<std::int64_t>>{ranges.begin(), ranges.size()});
            }

            /**
            * @note if buffer reallocation not required use copy_to function
            */
            template <arrnd_complient ArCo>
            constexpr const this_type& set_to(ArCo& dst) const
            {
                if (empty()) {
                    dst = ArCo{};
                    return *this;
                }

                if (dst.empty()) {
                    dst = ArCo{ header().dims() };
                    return copy_to(dst);
                }

                if (header().count() == dst.header().count()) {
                    if (header().dims() != dst.header().dims()) {
                        dst.header() = header_type{ header().dims() };
                    }
                    return copy_to(dst);
                }

                dst = ArCo{ header().dims() };
                return copy_to(dst);
            }

            template <arrnd_complient ArCo>
            constexpr this_type& copy_from(const ArCo& src)
            {
                src.copy_to(*this);
                return *this;
            }

            template <arrnd_complient ArCo1, arrnd_complient ArCo2> requires std::is_integral_v<typename ArCo2::value_type>
            constexpr this_type& copy_from(const ArCo1& src, const ArCo2& indices)
            {
                src.copy_to(*this, indices);
                return *this;
            }

            template <arrnd_complient ArCo>
            constexpr this_type& copy_from(const ArCo& src, std::span<const interval<std::int64_t>> ranges)
            {
                src.copy_to(*this, ranges);
                return *this;
            }
            template <arrnd_complient ArCo>
            constexpr this_type& copy_from(const ArCo& src, std::initializer_list<interval<std::int64_t>> ranges)
            {
                return copy_from(src, std::span<const interval<std::int64_t>>{ranges.begin(), ranges.size()});
            }

            template <arrnd_complient ArCo>
            constexpr this_type& set_from(const ArCo& src)
            {
                src.set_to(*this);
                return *this;
            }

            [[nodiscard]] constexpr this_type clone() const
            {
                if (empty()) {
                    return this_type();
                }

                this_type clone(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                indexer_type gen(header());
                indexer_type clone_gen(clone.header());

                for (; gen && clone_gen; ++gen, ++clone_gen) {
                    clone[*clone_gen] = (*this)[*gen];
                }

                return clone;
            }


            /**
            * @note Returning a reference to the input array and no allocation performed. If empty array, subarray, or different dimensions resizing.
            */
            [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::span<const std::int64_t> new_dims) const
            {
                if (header().count() != numel(new_dims) || header().is_subarray()) {
                    return resize(new_dims);
                }

                if (header().dims() == new_dims) {
                    return *this;
                }

                typename this_type::header_type new_header(new_dims);
                if (new_header.empty()) {
                    return this_type();
                }

                this_type res(*this);
                res.header() = std::move(new_header);

                return res;
            }
            [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<std::int64_t> new_dims) const
            {
                return reshape(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
            }

            /*
            * @note Other references to this array buffer are not modified. Resize is not performed if dims and new_dims are equal.
            */
            [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::span<const std::int64_t> new_dims) const
            {
                if (empty()) {
                    return this_type(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));
                }

                if (header().dims() == new_dims) {
                    return *this;
                }

                if (numel(new_dims) <= 0) {
                    return this_type();
                }

                this_type res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

                indexer_type gen(header());
                indexer_type res_gen(res.header());

                while (gen && res_gen) {
                    res[*res_gen] = (*this)[*gen];
                    ++gen;
                    ++res_gen;
                }

                return res;
            }
            [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<std::int64_t> new_dims) const
            {
                return resize(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
            }


            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                this_type res(resize({ header().count() + arr.header().count() }));

                ArCo rarr(arr.reshape({ arr.header().count() }));

                for (std::int64_t i = header().count(); i < res.header().count(); ++i) {
                    res[{ i }] = arr[{ i - header().count() }];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, std::int64_t axis) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                header_type new_header(header(), arr.header().dims(), axis);
                if (new_header.empty()) {
                    return this_type{};
                }

                this_type res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                indexer_type gen(header(), fixed_axis);
                typename ArCo::indexer_type arr_gen(arr.header(), fixed_axis);
                indexer_type res_gen(res.header(), fixed_axis);

                for (; gen && res_gen; ++gen, ++res_gen) {
                    res.data()[*res_gen] = data()[*gen];
                }
                for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                    res.data()[*res_gen] = arr.data()[*arr_gen];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, std::int64_t ind) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                this_type res({ header().count() + arr.header().count() });

                this_type rlhs(reshape({ header().count() }));
                ArCo rarr(arr.reshape({ arr.header().count() }));

                std::int64_t fixed_ind{ modulo(ind, header().count() + 1) };

                for (std::int64_t i = 0; i < fixed_ind; ++i) {
                    res[{ i }] = rlhs[{ i }];
                }
                for (std::int64_t i = 0; i < arr.header().count(); ++i) {
                    res[{ fixed_ind + i }] = rarr[{ i }];
                }
                for (std::int64_t i = 0; i < header().count() - fixed_ind; ++i) {
                    res[{ fixed_ind + arr.header().count() + i }] = rlhs[{ fixed_ind + i }];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, std::int64_t ind, std::int64_t axis) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                header_type new_header(header(), arr.header().dims(), axis);
                if (new_header.empty()) {
                    return this_type();
                }

                this_type res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                indexer_type gen(header(), fixed_axis);
                typename ArCo::indexer_type arr_gen(arr.header(), fixed_axis);
                indexer_type res_gen(res.header(), fixed_axis);

                std::int64_t fixed_ind{ modulo(ind, header().dims()[fixed_axis]) };
                std::int64_t cycle = fixed_ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), std::int64_t{ 1 }, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

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
            [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(std::int64_t ind, std::int64_t count) const
            {
                if (empty()) {
                    return *this;
                }

                std::int64_t fixed_ind{ modulo(ind, header().count()) };
                std::int64_t fixed_count{ fixed_ind + count < header().count() ? count : (header().count() - fixed_ind) };

                this_type res({ header().count() - fixed_count });
                this_type rarr(reshape({ header().count() }));

                for (std::int64_t i = 0; i < fixed_ind; ++i) {
                    res[{ i }] = rarr[{ i }];
                }
                for (std::int64_t i = fixed_ind + fixed_count; i < header().count(); ++i) {
                    res[{ i - fixed_count }] = rarr[{ i }];
                }

                return res;
            }

            /**
            * @note All elements starting from ind are being removed in case that count value is too big.
            */
            [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(std::int64_t ind, std::int64_t count, std::int64_t axis) const
            {
                if (empty()) {
                    return *this;
                }

                std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };
                std::int64_t fixed_ind{ modulo(ind, header().dims()[fixed_axis]) };
                std::int64_t fixed_count{ fixed_ind + count <= header().dims()[fixed_axis] ? count : (header().dims()[fixed_axis] - fixed_ind) };

                header_type new_header(header(), -fixed_count, fixed_axis);
                if (new_header.empty()) {
                    return this_type();
                }

                this_type res({ header().count() - (header().count() / header().dims()[fixed_axis]) * fixed_count });
                res.header() = std::move(new_header);

                indexer_type gen(header(), fixed_axis);
                indexer_type res_gen(res.header(), fixed_axis);

                std::int64_t cycle = fixed_ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), std::int64_t{ 1 }, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

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


            template <typename Unary_op> requires std::is_invocable_v<Unary_op, T>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Unary_op, T>> transform(Unary_op&& op) const
            {
                using U = std::invoke_result_t<Unary_op, T>;

                if (empty()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                for (indexer_type gen(header()); gen; ++gen) {
                    res[*gen] = op((*this)[*gen]);
                }

                return res;
            }

            template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, T, typename ArCo::value_type>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, T, typename ArCo::value_type>> transform(const ArCo& arr, Binary_op&& op) const
            {
                using U = std::invoke_result_t<Binary_op, T, typename ArCo::value_type>;

                if (header().dims() != arr.header().dims()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                indexer_type gen(header());
                typename replaced_type<U>::indexer_type arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    res[*gen] = op((*this)[*gen], arr[*arr_gen]);
                }

                return res;
            }

            template <typename V, typename Binary_op> requires std::is_invocable_v<Binary_op, T, V>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, T, V>> transform(const V& value, Binary_op&& op) const
            {
                replaced_type<std::invoke_result_t<Binary_op, T, V>> res(std::span<const std::int64_t>(header().dims().data(), header().dims().size()));

                for (indexer_type gen(header()); gen; ++gen) {
                    res[*gen] = op((*this)[*gen], value);
                }

                return res;
            }


            template <typename Unary_op> requires std::is_invocable_v<Unary_op, T>
            constexpr this_type& apply(Unary_op&& op)
            {
                if (empty()) {
                    return *this;
                }

                for (indexer_type gen(header()); gen; ++gen) {
                    (*this)[*gen] = op((*this)[*gen]);
                }

                return *this;
            }

            template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, T, typename ArCo::value_type>
            constexpr this_type& apply(const ArCo& arr, Binary_op&& op)
            {
                if (header().dims() != arr.header().dims()) {
                    return *this;
                }

                indexer_type gen(header());
                typename ArCo::indexer_type arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    (*this)[*gen] = op((*this)[*gen], arr[*arr_gen]);
                }

                return *this;
            }

            template <typename V, typename Binary_op> requires std::is_invocable_v<Binary_op, T, V>
            constexpr this_type& apply(const V& value, Binary_op&& op)
            {
                for (indexer_type gen(header()); gen; ++gen) {
                    (*this)[*gen] = op((*this)[*gen], value);
                }

                return *this;
            }



            template <typename Binary_op> requires std::is_invocable_v<Binary_op, T, T>
            [[nodiscard]] constexpr std::invoke_result_t<Binary_op, T, T> reduce(Binary_op&& op) const
            {
                using U = std::invoke_result_t<Binary_op, T, T>;

                if (empty()) {
                    return U{};
                }

                indexer_type gen{ header() };

                U res{ static_cast<U>((*this)[*gen]) };
                ++gen;

                while (gen) {
                    res = op(res, (*this)[*gen]);
                    ++gen;
                }

                return res;
            }

            template <typename U, typename Binary_op> requires std::is_invocable_v<Binary_op, U, T>
            [[nodiscard]] constexpr std::invoke_result_t<Binary_op, U, T> reduce(const U& init_value, Binary_op&& op) const
            {
                if (empty()) {
                    return init_value;
                }

                std::invoke_result_t<Binary_op, U, T> res{ init_value };
                for (indexer_type gen{ header() }; gen; ++gen) {
                    res = op(res, (*this)[*gen]);
                }

                return res;
            }

            template <typename Binary_op> requires std::is_invocable_v<Binary_op, T, T>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, T, T>> reduce(Binary_op&& op, std::int64_t axis) const
            {
                using U = std::invoke_result_t<Binary_op, T, T>;

                if (empty()) {
                    return replaced_type<U>();
                }

                const std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                typename replaced_type<U>::header_type new_header(header(), fixed_axis);
                if (new_header.empty()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), std::ssize(header().dims()) - fixed_axis - 1);
                indexer_type res_gen(res.header());

                const std::int64_t reduction_iteration_cycle{ header().dims()[fixed_axis] };

                while (gen && res_gen) {
                    U res_element{ static_cast<U>((*this)[*gen]) };
                    ++gen;
                    for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++gen) {
                        res_element = op(res_element, (*this)[*gen]);
                    }
                    res[*res_gen] = res_element;
                    ++res_gen;
                }

                return res;
            }

            template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, T>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, typename ArCo::value_type, T>> reduce(const ArCo& init_values, Binary_op&& op, std::int64_t axis) const
            {
                using U = std::invoke_result_t<Binary_op, typename ArCo::value_type, T>;

                if (empty()) {
                    return replaced_type<U>();
                }

                const std::int64_t fixed_axis{ modulo(axis, std::ssize(header().dims())) };

                if (init_values.header().dims().size() != 1 && init_values.header().dims()[fixed_axis] != header().dims()[fixed_axis]) {
                    return replaced_type<U>();
                }

                typename replaced_type<U>::header_type new_header(header(), axis);
                if (new_header.empty()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), std::ssize(header().dims()) - fixed_axis - 1);
                indexer_type res_gen(res.header());
                typename ArCo::indexer_type init_gen(init_values.header());

                const std::int64_t reduction_iteration_cycle{ header().dims()[fixed_axis] };

                while (gen && res_gen && init_gen) {
                    U res_element{ init_values[*init_gen] };
                    for (std::int64_t i = 0; i < reduction_iteration_cycle; ++i, ++gen) {
                        res_element = op(res_element, (*this)[*gen]);
                    }
                    res[*res_gen] = std::move(res_element);
                    ++res_gen;
                    ++init_gen;
                }

                return res;
            }


            template <typename Unary_pred> requires std::is_invocable_v<Unary_pred, T>
            [[nodiscard]] constexpr this_type filter(Unary_pred pred) const
            {
                if (empty()) {
                    return this_type();
                }

                this_type res({ header().count() });

                indexer_type gen(header());
                indexer_type res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && res_gen) {
                    if (pred((*this)[*gen])) {
                        res[*res_gen] = (*this)[*gen];
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                }

                if (res_count == 0) {
                    return this_type();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr this_type filter(const ArCo& mask) const
            {
                if (empty()) {
                    return this_type();
                }

                if (header().dims() != mask.header().dims()) {
                    return this_type();
                }

                this_type res({ header().count() });

                indexer_type gen(header());
                typename ArCo::indexer_type mask_gen(mask.header());

                indexer_type res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && mask_gen && res_gen) {
                    if (mask[*mask_gen]) {
                        res[*res_gen] = (*this)[*gen];
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                    ++mask_gen;
                }

                if (res_count == 0) {
                    return this_type();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <typename Unary_pred> requires std::is_invocable_v<Unary_pred, T>
            [[nodiscard]] constexpr replaced_type<std::int64_t> find(Unary_pred pred) const
            {
                if (empty()) {
                    return replaced_type<std::int64_t>();
                }

                replaced_type<std::int64_t> res({ header().count() });

                indexer_type gen(header());
                typename replaced_type<std::int64_t>::indexer_type res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && res_gen) {
                    if (pred((*this)[*gen])) {
                        res[*res_gen] = *gen;
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                }

                if (res_count == 0) {
                    return replaced_type<std::int64_t>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr replaced_type<std::int64_t> find(const ArCo& mask) const
            {
                if (empty()) {
                    return replaced_type<std::int64_t>();
                }

                if (header().dims() != mask.header().dims()) {
                    return replaced_type<std::int64_t>();
                }

                replaced_type<std::int64_t> res({ header().count() });

                indexer_type gen(header());
                typename ArCo::indexer_type mask_gen(mask.header());

                typename replaced_type<std::int64_t>::indexer_type res_gen(res.header());

                std::int64_t res_count{ 0 };

                while (gen && mask_gen && res_gen) {
                    if (mask[*mask_gen]) {
                        res[*res_gen] = *gen;
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                    ++mask_gen;
                }

                if (res_count == 0) {
                    return replaced_type<std::int64_t>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }


            [[nodiscard]] constexpr this_type transpose(std::span<const std::int64_t> order) const
            {
                if (empty()) {
                    return this_type();
                }

                header_type new_header(header(), order);
                if (new_header.empty()) {
                    return this_type();
                }

                this_type res({ header().count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), order);
                indexer_type res_gen(res.header());

                while (gen && res_gen) {
                    res[*res_gen] = (*this)[*gen];
                    ++gen;
                    ++res_gen;
                }

                return res;
            }

            [[nodiscard]] constexpr this_type transpose(std::initializer_list<std::int64_t> order) const
            {
                return transpose(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            template <arrnd_complient ArCo, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, typename ArCo::value_type>
            [[nodiscard]] constexpr bool all_match(const ArCo& arr, Binary_pred pred) const
            {
                if (empty() && arr.empty()) {
                    return true;
                }

                if (empty() || arr.empty()) {
                    return false;
                }

                if (header().dims() != arr.header().dims()) {
                    return false;
                }

                indexer_type gen(header());
                typename ArCo::indexer_type arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    if (!pred((*this)[*gen], arr[*arr_gen])) {
                        return false;
                    }
                }

                return true;
            }

            template <typename U, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, U>
            [[nodiscard]] constexpr bool all_match(const U& value, Binary_pred pred) const
            {
                if (empty()) {
                    return true;
                }

                for (indexer_type gen(header()); gen; ++gen) {
                    if (!pred((*this)[*gen], value)) {
                        return false;
                    }
                }

                return true;
            }

            template <typename Unary_pred> requires std::is_invocable_v<Unary_pred, T>
            [[nodiscard]] constexpr bool all_match(Unary_pred pred) const
            {
                if (empty()) {
                    return true;
                }

                for (indexer_type gen(header()); gen; ++gen) {
                    if (!pred((*this)[*gen])) {
                        return false;
                    }
                }

                return true;
            }

            template <arrnd_complient ArCo, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, typename ArCo::value_type>
            [[nodiscard]] constexpr bool any_match(const ArCo& arr, Binary_pred pred) const
            {
                if (empty() && arr.empty()) {
                    return true;
                }

                if (empty() || arr.empty()) {
                    return false;
                }

                if (header().dims() != arr.header().dims()) {
                    return false;
                }

                indexer_type gen(header());
                typename ArCo::indexer_type arr_gen(arr.header());

                for (; gen && arr_gen; ++gen, ++arr_gen) {
                    if (pred((*this)[*gen], arr[*arr_gen])) {
                        return true;
                    }
                }

                return false;
            }

            template <typename U, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, U>
            [[nodiscard]] constexpr bool any_match(const U& value, Binary_pred pred) const
            {
                if (empty()) {
                    return true;
                }

                for (indexer_type gen(header()); gen; ++gen) {
                    if (pred((*this)[*gen], value)) {
                        return true;
                    }
                }

                return false;
            }

            template <typename Unary_pred> requires std::is_invocable_v<Unary_pred, T>
            [[nodiscard]] constexpr bool any_match(Unary_pred pred) const
            {
                if (empty()) {
                    return true;
                }

                for (indexer_type gen(header()); gen; ++gen) {
                    if (pred((*this)[*gen])) {
                        return true;
                    }
                }

                return false;
            }


            [[nodiscard]] constexpr bool all() const
            {
                return all_match([](const value_type& value) { return static_cast<bool>(value); });
            }

            [[nodiscard]] constexpr replaced_type<bool> all(std::int64_t axis) const
            {
                return reduce([](const value_type& a, const value_type& b) { return a && b; }, axis);
            }

            [[nodiscard]] constexpr bool any() const
            {
                return any_match([](const value_type& value) { return static_cast<bool>(value); });
            }

            [[nodiscard]] constexpr replaced_type<bool> any(std::int64_t axis) const
            {
                return reduce([](const value_type& a, const value_type& b) { return a || b; }, axis);
            }


            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr replaced_type<bool> close(const ArCo& arr, const decltype(value_type{} - typename ArCo::value_type{})& atol = default_atol<decltype(value_type{} - typename ArCo::value_type{}) > (), const decltype(value_type{} - typename ArCo::value_type{})& rtol = default_rtol<decltype(value_type{} - typename ArCo::value_type{}) > ()) const
            {
                return transform(arr, [&atol, &rtol](const value_type& a, const typename ArCo::value_type& b) { return oc::details::close(a, b, atol, rtol); });
            }

            template <typename U> requires (!arrnd_complient<U>)
            [[nodiscard]] constexpr replaced_type<bool> close(const U& value, const decltype(value_type{} - U{})& atol = default_atol<decltype(value_type{} - U{}) > (), const decltype(value_type{} - U{})& rtol = default_rtol<decltype(value_type{} - U{}) > ()) const
            {
                return transform(value, [&atol, &rtol](const value_type& a, const U& b) { return oc::details::close(a, b, atol, rtol); });
            }


            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr bool all_equal(const ArCo& arr) const
            {
                return all_match(arr, [](const value_type& a, const typename ArCo::value_type& b) { return a == b; });
            }

            template <typename U>
            [[nodiscard]] constexpr bool all_equal(const U& value) const
            {
                return all_match(value, [](const value_type& a, const U& b) { return a == b; });
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr bool all_close(const ArCo& arr, const decltype(value_type{} - typename ArCo::value_type{})& atol = default_atol<decltype(value_type{} - typename ArCo::value_type{}) > (), const decltype(value_type{} - typename ArCo::value_type{})& rtol = default_rtol<decltype(value_type{} - typename ArCo::value_type{}) > ()) const
            {
                return all_match(arr, [&atol, &rtol](const value_type& a, const typename ArCo::value_type& b) { return oc::details::close(a, b, atol, rtol); });
            }

            template <typename U> requires (!arrnd_complient<U>)
            [[nodiscard]] constexpr bool all_close(const U& value, const decltype(value_type{} - U{})& atol = default_atol<decltype(value_type{} - U{}) > (), const decltype(value_type{} - U{})& rtol = default_rtol<decltype(value_type{} - U{}) > ()) const
            {
                return all_match(value, [&atol, &rtol](const value_type& a, const U& b) { return oc::details::close(a, b, atol, rtol); });
            }

            constexpr auto begin(std::int64_t axis = 0)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, axis));
            }

            constexpr auto end(std::int64_t axis = 0)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, axis, true) + 1);
            }


            constexpr auto cbegin(std::int64_t axis = 0) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, axis));
            }

            constexpr auto cend(std::int64_t axis = 0) const
            {
                return const_iterator(buffsp_->data() , indexer_type(hdr_, axis, true) + 1);
            }


            constexpr auto rbegin(std::int64_t axis = 0)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, true));
            }

            constexpr auto rend(std::int64_t axis = 0)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis) - 1);
            }

            constexpr auto crbegin(std::int64_t axis = 0) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, true));
            }

            constexpr auto crend(std::int64_t axis = 0) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis) - 1);
            }


            constexpr auto begin(std::span<const std::int64_t> order)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, order));
            }

            constexpr auto end(std::span<const std::int64_t> order)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, order, true) + 1);
            }


            constexpr auto cbegin(std::span<const std::int64_t> order) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, order));
            }

            constexpr auto cend(std::span<const std::int64_t> order) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, order, true) + 1);
            }


            constexpr auto rbegin(std::span<const std::int64_t> order)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, order, true));
            }

            constexpr auto rend(std::span<const std::int64_t> order)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, order) - 1);
            }

            constexpr auto crbegin(std::span<const std::int64_t> order) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, order, true));
            }

            constexpr auto crend(std::span<const std::int64_t> order) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, order) - 1);
            }



            constexpr auto begin(std::initializer_list<std::int64_t> order)
            {
                return begin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            constexpr auto end(std::initializer_list<std::int64_t> order)
            {
                return end(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            constexpr auto cbegin(std::initializer_list<std::int64_t> order) const
            {
                return cbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            constexpr auto cend(std::initializer_list<std::int64_t> order) const
            {
                return cend(std::span<const std::int64_t>(order.begin(), order.size()));
            }


            constexpr auto rbegin(std::initializer_list<std::int64_t> order)
            {
                return rbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            constexpr auto rend(std::initializer_list<std::int64_t> order)
            {
                return rend(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            constexpr auto crbegin(std::initializer_list<std::int64_t> order) const
            {
                return crbegin(std::span<const std::int64_t>(order.begin(), order.size()));
            }

            constexpr auto crend(std::initializer_list<std::int64_t> order) const
            {
                return crend(std::span<const std::int64_t>(order.begin(), order.size()));
            }



            constexpr auto begin_subarray(std::int64_t fixed_axis = 0)
            {
                return subarray_iterator(*this, fixed_axis, 0);
            }

            constexpr auto end_subarray(std::int64_t fixed_axis = 0)
            {
                return subarray_iterator(*this, fixed_axis, hdr_.dims()[fixed_axis]);
            }


            constexpr auto cbegin_subarray(std::int64_t fixed_axis = 0) const
            {
                return const_subarray_iterator(*this, fixed_axis, 0);
            }

            constexpr auto cend_subarray(std::int64_t fixed_axis = 0) const
            {
                return const_subarray_iterator(*this, fixed_axis, hdr_.dims()[fixed_axis]);
            }


            constexpr auto rbegin_subarray(std::int64_t fixed_axis = 0)
            {
                return reverse_subarray_iterator(*this, fixed_axis, hdr_.dims()[fixed_axis] - 1);
            }

            constexpr auto rend_subarray(std::int64_t fixed_axis = 0)
            {
                return reverse_subarray_iterator(*this, fixed_axis, -1);
            }

            constexpr auto crbegin_subarray(std::int64_t fixed_axis = 0) const
            {
                return const_reverse_subarray_iterator(*this, fixed_axis, hdr_.dims()[fixed_axis] - 1);
            }

            constexpr auto crend_subarray(std::int64_t fixed_axis = 0) const
            {
                return const_reverse_subarray_iterator(*this, fixed_axis, -1);
            }



            [[nodiscard]] constexpr auto abs()
            {
                return transform([](const value_type& a) { return ::abs(a); });
            }

            
            [[nodiscard]] constexpr auto acos()
            {
                return transform([](const value_type& a) { return ::acos(a); });
            }

            
            [[nodiscard]] constexpr auto acosh()
            {
                return transform([](const value_type& a) { return ::acosh(a); });
            }

            
            [[nodiscard]] constexpr auto asin()
            {
                return transform([](const value_type& a) { return ::asin(a); });
            }

            
            [[nodiscard]] constexpr auto asinh()
            {
                return transform([](const value_type& a) { return ::asinh(a); });
            }

            
            [[nodiscard]] constexpr auto atan()
            {
                return transform([](const value_type& a) { return ::atan(a); });
            }

            
            [[nodiscard]] constexpr auto atanh()
            {
                return transform([](const value_type& a) { return ::atanh(a); });
            }

            
            [[nodiscard]] constexpr auto cos()
            {
                return transform([](const value_type& a) { return ::cos(a); });
            }

            
            [[nodiscard]] constexpr auto cosh()
            {
                return transform([](const value_type& a) { return ::cosh(a); });
            }

            
            [[nodiscard]] constexpr auto exp()
            {
                return transform([](const value_type& a) { return ::exp(a); });
            }

            
            [[nodiscard]] constexpr auto log()
            {
                return transform([](const value_type& a) { return ::log(a); });
            }

            
            [[nodiscard]] constexpr auto log10()
            {
                return transform([](const value_type& a) { return ::log10(a); });
            }

            
            [[nodiscard]] constexpr auto pow()
            {
                return transform([](const value_type& a) { return ::pow(a); });
            }

            
            [[nodiscard]] constexpr auto sin()
            {
                return transform([](const value_type& a) { return ::sin(a); });
            }

            
            [[nodiscard]] constexpr auto sinh()
            {
                return transform([](const value_type& a) { return ::sinh(a); });
            }

            
            [[nodiscard]] constexpr auto sqrt()
            {
                return transform([](const value_type& a) { return ::sqrt(a); });
            }

            
            [[nodiscard]] constexpr auto tan()
            {
                return transform([](const value_type& a) { return ::tan(a); });
            }

            
            [[nodiscard]] constexpr auto tanh()
            {
                return transform([](const value_type& a) { return ::tanh(a); });
            }



        private:
            header_type hdr_{};
            std::shared_ptr<storage_type> buffsp_{ nullptr };
        };

        /**
        * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
        */
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst)
        {
            return dst.copy_from(src);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst)
        {
            return dst.copy_from(src);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, arrnd_complient ArCo3>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, const ArCo3& indices)
        {
            return dst.copy_from(src, indices);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2, arrnd_complient ArCo3>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const ArCo3& indices)
        {
            return dst.copy_from(src, indices);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, std::span<const interval<std::int64_t>> ranges)
        {
            return dst.copy_from(src, ranges);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::span<const interval<std::int64_t>> ranges)
        {
            return dst.copy_from(src, ranges);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, std::initializer_list<interval<std::int64_t>> ranges)
        {
            return dst.copy_from(src, ranges);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::initializer_list<interval<std::int64_t>> ranges)
        {
            return dst.copy_from(src, ranges);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& set(const ArCo1& src, ArCo2& dst)
        {
            return dst.set_from(src);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& set(const ArCo1& src, ArCo2&& dst)
        {
            return dst.set_from(src);
        }


        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto clone(const ArCo& arr)
        {
            return arr.clone();
        }

        /**
        * @note Returning a reference to the input array, except in case of resulted empty array or an input subarray.
        */
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, std::span<const std::int64_t> new_dims)
        {
            return arr.reshape(new_dims);
        }
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return arr.reshape(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto resize(const ArCo& arr, std::span<const std::int64_t> new_dims)
        {
            return arr.resize(new_dims);
        }
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto resize(const ArCo& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return arr.resize(std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.append(rhs);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, std::int64_t axis)
        {
            return lhs.append(rhs, axis);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, std::int64_t ind)
        {
            return lhs.insert(rhs, ind);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, std::int64_t ind, std::int64_t axis)
        {
            return lhs.insert(rhs, ind, axis);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto remove(const ArCo& arr, std::int64_t ind, std::int64_t count)
        {
            return arr.remove(ind, count);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto remove(const ArCo& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            return arr.remove(ind, count, axis);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool empty(const ArCo& arr) noexcept
        {
            return arr.empty();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto size(const ArCo& arr) noexcept
        {
            return arr.size();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto dims(const ArCo& arr) noexcept
        {
            return arr.dims();
        }

        template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, Binary_op&& op)
        {
            return arr.reduce(op);
        }

        template <arrnd_complient ArCo, typename T, typename Binary_op> requires std::is_invocable_v<Binary_op, T, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, const T& init_value, Binary_op&& op)
        {
            return arr.reduce(init_value, op);
        }

        template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, Binary_op&& op, std::int64_t axis)
        {
            return arr.reduce(op, axis);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo2::value_type, typename ArCo1::value_type>
        [[nodiscard]] inline constexpr auto reduce(const ArCo1& arr, const ArCo2& init_values, Binary_op&& op, std::int64_t axis)
        {
            return arr.reduce(init_values, op, axis);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool all(const ArCo& arr)
        {
            return arr.all();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto all(const ArCo& arr, std::int64_t axis)
        {
            return arr.all(axis);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool any(const ArCo& arr)
        {
            return arr.any();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto any(const ArCo& arr, std::int64_t axis)
        {
            return arr.any(axis);
        }

        template <arrnd_complient ArCo, typename Unary_op> requires std::is_invocable_v<Unary_op, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto transform(const ArCo& arr, Unary_op&& op)
        {
            return arr.transform(op);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo1::value_type, typename ArCo2::value_type>
        [[nodiscard]] inline constexpr auto transform(const ArCo1& lhs, const ArCo2& rhs, Binary_op&& op)
        {
            return lhs.transform(rhs, op);
        }

        template <arrnd_complient ArCo, typename T, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, T>
        [[nodiscard]] inline constexpr auto transform(const ArCo& lhs, const T& rhs, Binary_op&& op)
        {
            return lhs.transform(rhs, op);
        }

        template <typename T, arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, T, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto transform(const T& lhs, const ArCo& rhs, Binary_op&& op)
        {
            return rhs.transform([&lhs, &op](const typename ArCo::value_type& value) { return op(lhs, value); });
        }

        template <arrnd_complient ArCo, typename Unary_op> requires std::is_invocable_v<Unary_op, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto& apply(ArCo& arr, Unary_op&& op)
        {
            return arr.apply(op);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo1::value_type, typename ArCo2::value_type>
        [[nodiscard]] inline constexpr auto& apply(ArCo1& lhs, const ArCo2& rhs, Binary_op&& op)
        {
            return lhs.apply(rhs, op);
        }

        template <arrnd_complient ArCo, typename T, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, T>
        [[nodiscard]] inline constexpr auto& apply(ArCo& lhs, const T& rhs, Binary_op&& op)
        {
            return lhs.apply(rhs, op);
        }

        template <typename T, arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, T, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto& apply(const T& lhs, ArCo& rhs, Binary_op&& op)
        {
            return rhs.apply([&lhs, &op](const typename ArCo::value_type& value) { return op(lhs, value); });
        }

        template <arrnd_complient ArCo, typename Unary_pred> requires std::is_invocable_v<Unary_pred, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto filter(const ArCo& arr, Unary_pred pred)
        {
            return arr.filter(pred);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr ArCo1 filter(const ArCo1& arr, const ArCo2& mask)
        {
            return arr.filter(mask);
        }

        template <arrnd_complient ArCo, typename Unary_pred> requires std::is_invocable_v<Unary_pred, typename ArCo::value_type>
        [[nodiscard]] inline constexpr auto find(const ArCo& arr, Unary_pred pred)
        {
            return arr.find(pred);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto find(const ArCo1& arr, const ArCo2& mask)
        {
            return arr.find(mask);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, std::span<const std::int64_t> order)
        {
            return arr.transpose(order);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, std::initializer_list<std::int64_t> order)
        {
            return arr.transpose(order);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator==(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a == b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator==(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a == b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator==(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a == b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator!=(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a != b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator!=(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a != b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator!=(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a != b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto close(const ArCo1& lhs, const ArCo2& rhs, const decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{})& atol = default_atol<decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{}) > (), const decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{})& rtol = default_rtol<decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{}) > ())
        {
            return lhs.close(rhs, atol, rtol);
        }

        template <arrnd_complient ArCo, typename T> requires (!arrnd_complient<T>)
        [[nodiscard]] inline constexpr auto close(const ArCo& lhs, const T& rhs, const decltype(typename ArCo::value_type{} - T{})& atol = default_atol<decltype(typename ArCo::value_type{} - T{}) > (), const decltype(typename ArCo::value_type{} - T{})& rtol = default_rtol<decltype(typename ArCo::value_type{} - T{}) > ())
        {
            return lhs.close(rhs, atol, rtol);
        }

        template <typename T, arrnd_complient ArCo> requires (!arrnd_complient<T>)
        [[nodiscard]] inline constexpr auto close(const T& lhs, const ArCo& rhs, const decltype(T{} - typename ArCo::value_type{})& atol = default_atol<decltype(T{} - typename ArCo::value_type{}) > (), const decltype(T{} - typename ArCo::value_type{})& rtol = default_rtol<decltype(T{} - typename ArCo::value_type{}) > ())
        {
            return rhs.close(lhs, atol, rtol);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator>(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a > b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator>(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a > b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator>(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a > b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator>=(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a >= b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator>=(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a >= b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator>=(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a >= b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator<(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a < b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator<(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a < b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator<(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a < b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator<=(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a <= b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator<=(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a <= b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator<=(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a <= b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator+(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a + b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator+(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a + b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator+(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a + b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator+=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a + b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator+=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a + b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator-(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a - b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator-(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a - b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator-(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a - b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator-=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a - b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator-=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a - b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator*(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a * b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator*(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a * b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator*(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a * b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator*=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a * b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator*=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a * b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator/(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a / b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator/(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a / b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator/(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a / b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator/=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a / b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator/=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a / b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto operator%(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a % b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator%(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a % b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator%(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a % b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator%=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a % b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator%=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a % b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator^(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a ^ b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator^(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a ^ b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator^(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a ^ b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator^=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a ^ b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator^=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a ^ b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator&(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a & b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator&(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a & b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator&(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a & b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator&=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a & b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator&=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a & b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator|(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a | b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator|(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a | b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator|(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a | b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator|=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a | b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator|=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a | b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator<<(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a << b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator<<(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a << b; });
        }

        template <typename T, arrnd_complient ArCo> requires (!std::derived_from<T, std::ios_base> && !arrnd_complient<T>)
        [[nodiscard]] inline constexpr auto operator<<(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a << b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator<<=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a << b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator<<=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a << b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator>>(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a >> b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator>>(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a >> b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator>>(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a >> b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& operator>>=(ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a >> b; });
        }

        template <arrnd_complient ArCo, typename T>
        inline constexpr auto& operator>>=(ArCo& lhs, const T& rhs)
        {
            return lhs.apply(rhs, [](const typename ArCo::value_type& a, const T& b) { return a >> b; });
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator~(const ArCo& arr)
        {
            return arr.transform([](const typename ArCo::value_type& a) { return ~a; });
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator!(const ArCo& arr)
        {
            return arr.transform([](const typename ArCo::value_type& a) { return !a; });
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator+(const ArCo& arr)
        {
            return arr.transform([](const typename ArCo::value_type& a) { return +a; });
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator-(const ArCo& arr)
        {
            return arr.transform([](const typename ArCo::value_type& a) { return -a; });
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto abs(const ArCo& arr)
        {
            return arr.abs();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto acos(const ArCo& arr)
        {
            return arr.acos();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto acosh(const ArCo& arr)
        {
            return arr.acosh();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto asin(const ArCo& arr)
        {
            return arr.asin();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto asinh(const ArCo& arr)
        {
            return arr.asinh();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto atan(const ArCo& arr)
        {
            return arr.atan();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto atanh(const ArCo& arr)
        {
            return arr.atanh();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto cos(const ArCo& arr)
        {
            return arr.cos();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto cosh(const ArCo& arr)
        {
            return arr.cosh();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto exp(const ArCo& arr)
        {
            return arr.exp();
        }
        
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto log(const ArCo& arr)
        {
            return arr.log();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto log10(const ArCo& arr)
        {
            return arr.log10();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto pow(const ArCo& arr)
        {
            return arr.pow();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto sin(const ArCo& arr)
        {
            return arr.sin();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto sinh(const ArCo& arr)
        {
            return arr.sinh();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto sqrt(const ArCo& arr)
        {
            return arr.sqrt();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto tan(const ArCo& arr)
        {
            return arr.tan();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto tanh(const ArCo& arr)
        {
            return arr.tanh();
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator&&(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a && b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator&&(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a && b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator&&(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a && b; });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto operator||(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo1::value_type& a, const typename ArCo2::value_type& b) { return a || b; });
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr auto operator||(const ArCo& lhs, const T& rhs)
        {
            return lhs.transform(rhs, [](const typename ArCo::value_type& a, const T& b) { return a || b; });
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator||(const T& lhs, const ArCo& rhs)
        {
            return rhs.transform(lhs, [](const typename ArCo::value_type& b, const T& a) { return a || b; });
        }

        template <arrnd_complient ArCo>
        inline constexpr auto& operator++(ArCo& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename ArCo::indexer_type gen(arr.header()); gen; ++gen) {
                ++arr[*gen];
            }
            return arr;
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator++(ArCo&& arr)
        {
            return operator++(arr);
        }

        template <arrnd_complient ArCo>
        inline constexpr auto operator++(ArCo& arr, int)
        {
            ArCo old = clone(arr);
            operator++(arr);
            return old;
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator++(ArCo&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <arrnd_complient ArCo>
        inline constexpr auto& operator--(ArCo& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (typename ArCo::indexer_type gen(arr.header()); gen; ++gen) {
                --arr[*gen];
            }
            return arr;
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator--(ArCo&& arr)
        {
            return operator--(arr);
        }

        template <arrnd_complient ArCo>
        inline constexpr auto operator--(ArCo& arr, int)
        {
            ArCo old = clone(arr);
            operator--(arr);
            return old;
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto operator--(ArCo&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_pred> requires std::is_invocable_v<Binary_pred, typename ArCo1::value_type, typename ArCo2::value_type>
        [[nodiscard]] inline constexpr bool all_match(const ArCo1& lhs, const ArCo2& rhs, Binary_pred pred)
        {
            return lhs.all_match(rhs, pred);
        }

        template <arrnd_complient ArCo, typename T, typename Binary_pred> requires std::is_invocable_v<Binary_pred, typename ArCo::value_type, T>
        [[nodiscard]] inline constexpr bool all_match(const ArCo& lhs, const T& rhs, Binary_pred pred)
        {
            return lhs.all_match(rhs, pred);
        }

        template <typename T, arrnd_complient ArCo, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, typename ArCo::value_type>
        [[nodiscard]] inline constexpr bool all_match(const T& lhs, const ArCo& rhs, Binary_pred pred)
        {
            return rhs.all_match([&lhs, &pred](const typename ArCo::value_type& value) { return pred(lhs, value); });
        }


        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_pred> requires std::is_invocable_v<Binary_pred, typename ArCo1::value_type, typename ArCo2::value_type>
        [[nodiscard]] inline constexpr bool any_match(const ArCo1& lhs, const ArCo2& rhs, Binary_pred pred)
        {
            return lhs.any_match(rhs, pred);
        }

        template <arrnd_complient ArCo, typename T, typename Binary_pred> requires std::is_invocable_v<Binary_pred, typename ArCo::value_type, T>
        [[nodiscard]] inline constexpr bool any_match(const ArCo& lhs, const T& rhs, Binary_pred pred)
        {
            return lhs.any_match(rhs, pred);
        }

        template <typename T, arrnd_complient ArCo, typename Binary_pred> requires std::is_invocable_v<Binary_pred, T, typename ArCo::value_type>
        [[nodiscard]] inline constexpr bool any_match(const T& lhs, const ArCo& rhs, Binary_pred pred)
        {
            return rhs.any_match([&lhs, &pred](const typename ArCo::value_type& value) { return pred(lhs, value); });
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr bool all_equal(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.all_equal(rhs);
        }

        template <arrnd_complient ArCo, typename T>
        [[nodiscard]] inline constexpr bool all_equal(const ArCo& lhs, const T& rhs)
        {
            return lhs.all_equal(rhs);
        }

        template <typename T, arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool all_equal(const T& lhs, const ArCo& rhs)
        {
            return rhs.all_equal(lhs);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr bool all_close(const ArCo1& lhs, const ArCo2& rhs, const decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{})& atol = default_atol<decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{}) > (), const decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{})& rtol = default_rtol<decltype(typename ArCo1::value_type{} - typename ArCo2::value_type{}) > ())
        {
            return lhs.all_close(rhs, atol, rtol);
        }

        template <arrnd_complient ArCo, typename T> requires (!arrnd_complient<T>)
        [[nodiscard]] inline constexpr bool all_close(const ArCo& lhs, const T& rhs, const decltype(typename ArCo::value_type{} - T{})& atol = default_atol<decltype(typename ArCo::value_type{} - T{}) > (), const decltype(typename ArCo::value_type{} - T{})& rtol = default_rtol<decltype(typename ArCo::value_type{} - T{}) > ())
        {
            return lhs.all_close(rhs, atol, rtol);
        }

        template <typename T, arrnd_complient ArCo> requires (!arrnd_complient<T>)
        [[nodiscard]] inline constexpr bool all_close(const T& lhs, const ArCo& rhs, const decltype(T{} - typename ArCo::value_type{})& atol = default_atol<decltype(T{} - typename ArCo::value_type{}) > (), const decltype(T{} - typename ArCo::value_type{})& rtol = default_rtol<decltype(T{} - typename ArCo::value_type{}) > ())
        {
            return rhs.all_close(lhs, atol, rtol);
        }

        template <arrnd_complient ArCo>
        std::ostream& ostream_operator_recursive(std::ostream& os, const ArCo& arco, std::int64_t nvectical_spaces)
        {
            if (empty(arco)) {
                os << "[]";
                return os;
            }

            if (std::ssize(arco.header().dims()) > 1) {
                os << '[';
                for (std::int64_t i = 0; i < arco.header().dims()[0]; ++i) {
                    if (i > 0) {
                        for (std::int64_t i = 0; i < nvectical_spaces - (std::ssize(arco.header().dims()) - 1) + 1; ++i) {
                            os << ' ';
                        }
                    }
                    ostream_operator_recursive(os, arco[interval<std::int64_t>{i, i}], nvectical_spaces);
                    if (i < arco.header().dims()[0] - 1) {
                        os << '\n';
                    }
                }
                os << ']';
                return os;
            }

            os << '[';
            typename ArCo::indexer_type gen(arco.header());
            os << arco[*gen];
            ++gen;
            for (; gen; ++gen) {
                os << ' ' << arco[*gen];
            }
            os << ']';
            return os;
        }

        template <arrnd_complient ArCo>
        inline constexpr std::ostream& operator<<(std::ostream& os, const ArCo& arco)
        {
            std::int64_t nvectical_spaces = std::ssize(arco.header().dims()) - 1;
            return ostream_operator_recursive(os, arco, nvectical_spaces);
        }
    }

    using details::arrnd_complient;

    using details::arrnd;

    using details::arrnd_header;
    
    using details::arrnd_general_indexer;
    using details::arrnd_fast_indexer;

    using details::arrnd_iterator;
    using details::arrnd_const_iterator;
    using details::arrnd_reverse_iterator;
    using details::arrnd_const_reverse_iterator;

    using details::arrnd_axis_iterator;
    using details::arrnd_axis_const_iterator;
    using details::arrnd_axis_reverse_iterator;
    using details::arrnd_axis_reverse_const_iterator;

    using details::copy;
    using details::clone;
    using details::reshape;
    using details::resize;
    using details::append;
    using details::insert;
    using details::remove;

    using details::empty;
    using details::size;
    using details::dims;
    using details::all_match;
    using details::any_match;
    using details::transform;
    using details::apply;
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
