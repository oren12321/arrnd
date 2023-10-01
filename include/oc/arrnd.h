#ifndef OC_ARRAY_H
#define OC_ARRAY_H

#include <cstdint>
#include <memory>
#include <initializer_list>
#include <stdexcept>
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
                    other.capacity_ = 0;
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
                    other.capacity_ = 0;

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
                    assert(index >= 0 && index < size_ && "invalid index");
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    assert(index >= 0 && index < size_ && "invalid index");
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

                [[nodiscard]] constexpr std::reverse_iterator<pointer> rbegin() noexcept
                {
                    return std::make_reverse_iterator(end());
                }

                [[nodiscard]] constexpr std::reverse_iterator<pointer> rend() noexcept
                {
                    return std::make_reverse_iterator(begin());
                }

                [[nodiscard]] constexpr std::reverse_iterator<const_pointer> crbegin() const noexcept
                {
                    return std::make_reverse_iterator(cend());
                }

                [[nodiscard]] constexpr std::reverse_iterator<const_pointer> crend() const noexcept
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

        template <typename T, template<typename> typename Allocator = lightweight_allocator>
        [[nodiscard]] inline constexpr bool operator==(const simple_dynamic_vector<T, Allocator>& lhs, const simple_dynamic_vector<T, Allocator>& rhs)
        {
            return std::equal(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend());
        }


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
                    assert(index >= 0 && index < size_ && "invalid index");
                    return data_ptr_[index];
                }

                [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
                {
                    assert(index >= 0 && index < size_ && "invalid index");
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

                [[nodiscard]] constexpr std::reverse_iterator<pointer> rbegin() noexcept
                {
                    return std::make_reverse_iterator(end());
                }

                [[nodiscard]] constexpr std::reverse_iterator<pointer> rend() noexcept
                {
                    return std::make_reverse_iterator(begin());
                }

                [[nodiscard]] constexpr std::reverse_iterator<const_pointer> crbegin() const noexcept
                {
                    return std::make_reverse_iterator(cend());
                }

                [[nodiscard]] constexpr std::reverse_iterator<const_pointer> crend() const noexcept
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

        template <typename T, std::int64_t Capacity>
        [[nodiscard]] inline constexpr bool operator==(const simple_static_vector<T, Capacity>& lhs, const simple_static_vector<T, Capacity>& rhs)
        {
            return std::equal(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend());
        }
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
            constexpr interval(T nstart, T nstop, T nstep) noexcept
                : start(nstart), stop(nstop), step(nstep) {}

            constexpr interval(T nstart, T nstop) noexcept
                : interval(nstart, nstop, 1) {}

            constexpr interval(T nstart) noexcept
                : interval(nstart, nstart, 1) {}

            constexpr interval() = default;
            constexpr interval(const interval&) = default;
            constexpr interval& operator=(const interval&) = default;
            constexpr interval(interval&) = default;
            constexpr interval& operator=(interval&) = default;

            T start{ 0 };
            T stop{ 0 };
            T step{ 1 };

            [[nodiscard]] static constexpr interval full(T nsize) noexcept {
                return interval{ 0, nsize - 1 };
            }

            [[nodiscard]] static constexpr interval from(T nstart, T ncount) {
                return interval{ nstart, nstart + ncount };
            }

            [[nodiscard]] static constexpr interval to(T nstop) noexcept {
                return interval{ 0, nstop };
            }

            [[nodiscard]] static constexpr interval at(T npos) noexcept {
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

        template <std::integral T>
        [[nodiscard]] inline constexpr bool operator==(const interval<T>& lhs, const interval<T>& rhs) noexcept
        {
            return lhs.start == rhs.start && lhs.stop == rhs.stop && lhs.step == rhs.step;
        }
    }

    using details::interval;

    using details::modulo;

    using details::reverse;
    using details::forward;




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
        using iterator_value_type = typename std::iterator_traits<Iter>::value_type;

        template <typename Cont, typename T>
        concept iterable_of_type = requires(const Cont & c) {
            std::begin(c);
            std::end(c);
            { std::remove_cvref_t<decltype(*std::begin(c))>() } -> std::same_as<T>;
        };

        template <typename T>
        concept random_access_type = std::random_access_iterator<typename T::iterator>;

        template <random_access_type Storage = simple_dynamic_vector<std::int64_t>>
        class arrnd_header {
        public:
            using storage_type = Storage;
            using value_type = Storage::value_type;
            using size_type = Storage::size_type;

            constexpr arrnd_header() = default;

            template <typename InputIt> requires std::is_same_v<value_type, iterator_value_type<InputIt>>
            constexpr arrnd_header(InputIt first_dim, InputIt last_dim)
            {
                assert(first_dim <= last_dim);
                assert(std::all_of(first_dim, last_dim, [](auto d) { return d >= 0; }));

                if (first_dim == last_dim) {
                    return;
                }

                count_ = std::reduce(first_dim, last_dim, iterator_value_type<InputIt>{1}, std::multiplies<>{});
                if (count_ == 0) {
                    return;
                }

                dims_ = storage_type(first_dim, last_dim);

                strides_ = storage_type(dims_.size());
                std::exclusive_scan(dims_.crbegin(), dims_.crend(), strides_.rbegin(), value_type{ 1 }, std::multiplies<>{});

                last_index_ = count_ - 1;
            }

            template <iterable_of_type<value_type> Cont>
            constexpr arrnd_header(const Cont& dims)
                : arrnd_header(std::begin(dims), std::end(dims))
            {
            }

            constexpr arrnd_header(std::initializer_list<value_type> dims)
                : arrnd_header(dims.begin(), dims.end())
            {
            }

            template <typename InputIt> requires std::is_same_v<interval<value_type>, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr arrnd_header subheader(InputIt first_range, InputIt last_range) const
            {
                assert(first_range <= last_range);

                if (empty()) {
                    return *this;
                }

                size_type nranges = std::min(std::distance(first_range, last_range), dims_.size());

                auto valid_ranges = [&]() {
                    return std::inner_product(first_range, std::next(first_range, nranges), dims_.cbegin(), true,
                        std::logical_and<>{},
                        [](const auto& r, auto d) { return (r.start <= r.stop && r.step >= 1) && (r.start >= 0 && r.stop < d); });
                };
                assert(valid_ranges());

                if (first_range == last_range) {
                    return *this;
                }

                arrnd_header res{};

                res.dims_ = storage_type(dims_.size());
                std::transform(first_range, std::next(first_range, nranges), res.dims_.begin(),
                    [](const auto& r) { return static_cast<value_type>(std::ceil((r.stop - r.start + 1.0) / r.step)); });
                std::copy(std::next(dims_.cbegin(), nranges), dims_.cend(), std::next(res.dims_.begin(), nranges));

                if (std::equal(res.dims_.cbegin(), res.dims_.cend(), dims_.cbegin(), dims_.cend())) {
                    return *this;
                }

                res.count_ = std::reduce(res.dims_.cbegin(), res.dims_.cend(), value_type{1}, std::multiplies<>{});

                res.strides_ = storage_type(res.dims_.size());
                std::transform(strides_.cbegin(), std::next(strides_.cbegin(), nranges), first_range, res.strides_.begin(),
                    [](auto s, const auto& r) { return s * r.step; });
                std::copy(std::next(strides_.cbegin(), nranges), strides_.cend(), std::next(res.strides_.begin(), nranges));

                res.offset_ = offset_ + std::transform_reduce(strides_.cbegin(), std::next(strides_.cbegin(), nranges), first_range,
                    value_type{ 0 }, std::plus<>{},
                    [](auto s, const auto& r) { return s * r.start; });

                res.last_index_ = res.offset_ + std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(), value_type{ 0 },
                    std::plus<>{}, [](auto d, auto s) { return (d - 1) * s; });

                res.is_subarray_ = true;

                return res;
            }

            template <iterable_of_type<interval<value_type>> Cont>
            [[nodiscard]] constexpr arrnd_header subheader(const Cont& ranges) const
            {
                return subheader(std::begin(ranges), std::end(ranges));
            }

            [[nodiscard]] constexpr arrnd_header subheader(std::initializer_list<interval<value_type>> ranges) const
            {
                return subheader(ranges.begin(), ranges.end());
            }

            [[nodiscard]] constexpr arrnd_header subheader(interval<value_type> range) const
            {
                std::initializer_list<interval<value_type>> ranges = { range };

                auto res = subheader(ranges.begin(), ranges.end());
                if (res.empty() || res.dims_.front() != 1) {
                    return res;
                }

                res.dims_ = storage_type(std::next(res.dims_.cbegin(), 1), res.dims_.cend());
                res.strides_ = storage_type(std::next(res.strides_.cbegin(), 1), res.strides_.cend());
                res.last_index_ = res.offset_ + std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(), value_type{ 0 },
                    std::plus<>{}, [](auto d, auto s) { return (d - 1) * s; });
                res.is_subarray_ = true;

                return res;
            }

            [[nodiscard]] constexpr arrnd_header subheader(size_type omitted_axis) const
            {
                assert(omitted_axis >= 0 && omitted_axis < dims_.size());

                if (empty()) {
                    return *this;
                }

                storage_type new_dims(dims_.size() > 1 ? dims_.size() - 1 : 1);

                if (dims_.size() == 1) {
                    new_dims.front() = 1;
                    return arrnd_header(new_dims.cbegin(), new_dims.cend());
                }

                std::copy(dims_.cbegin(), std::next(dims_.cbegin(), omitted_axis), new_dims.begin());
                std::copy(std::next(dims_.cbegin(), omitted_axis + 1), dims_.cend(), std::next(new_dims.begin(), omitted_axis));

                return arrnd_header(new_dims.cbegin(), new_dims.cend());
            }

            [[nodiscard]] constexpr arrnd_header subheader(value_type count, size_type axis) const
            {
                assert(axis >= 0 && axis < dims_.size());
                assert(count >= -*std::next(dims_.cbegin(), axis));

                if (empty()) {
                    return *this;
                }

                storage_type new_dims(dims_);
                *std::next(new_dims.begin(), axis) += count;

                return arrnd_header(new_dims.cbegin(), new_dims.cend());
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr arrnd_header reorder(InputIt first_order, InputIt last_order) const
            {
                assert(std::distance(first_order, last_order) == dims_.size());
                assert(std::all_of(first_order, last_order, [&](auto order) { return order >= 0 && order < dims_.size(); }));

                if (empty() || dims_.size() == 1) {
                    return *this;
                }

                arrnd_header res(*this);

                for (size_type i = 0; i < dims_.size(); ++i) {
                    *std::next(res.dims_.begin(), i) = *std::next(dims_.cbegin(), *std::next(first_order, i));
                    *std::next(res.strides_.begin(), i) = *std::next(strides_.cbegin(), *std::next(first_order, i));
                }
                res.is_axis_reordered_ = true;

                return res;
            }

            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr arrnd_header reorder(const Cont& order) const
            {
                return reorder(std::begin(order), std::end(order));
            }

            [[nodiscard]] constexpr arrnd_header reorder(std::initializer_list<size_type> order) const
            {
                return reorder(order.begin(), order.end());
            }

            [[nodiscard]] constexpr arrnd_header reorder(size_type main_axis) const
            {
                assert(main_axis >= 0 && main_axis < dims_.size());

                if (empty() || dims_.size() == 1 || main_axis == 0) {
                    return *this;
                }

                arrnd_header res(*this);

                value_type main_dim = *std::next(dims_.cbegin(), main_axis);
                value_type main_stride = *std::next(strides_.cbegin(), main_axis);

                size_type j = 1;

                for (size_type i = 0; i < main_axis; ++i) {
                    *std::next(res.dims_.begin(), j) = *std::next(dims_.cbegin(), i);
                    *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                    ++j;
                }

                for (size_type i = main_axis + 1; i < dims_.size(); ++i) {
                    *std::next(res.dims_.begin(), j) = *std::next(dims_.cbegin(), i);
                    *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                    ++j;
                }

                res.dims_.front() = main_dim;
                res.strides_.front() = main_stride;

                res.is_axis_reordered_ = true;

                return res;
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr value_type subs2ind(InputIt first_sub, InputIt last_sub) const
            {
                assert(first_sub <= last_sub);

                size_type nsubs = std::distance(first_sub, last_sub);
                assert(nsubs > 0 && nsubs <= dims_.size());

                auto valid_subs = [&]() {
                    return std::inner_product(first_sub, last_sub, std::next(dims_.cbegin(), dims_.size() - nsubs), true,
                        std::logical_and<>{},
                        [](auto s, auto d) { return (s >= 0 && s < d); });
                };
                assert(valid_subs());

                return offset_ + std::transform_reduce(first_sub, last_sub, std::next(strides_.cbegin(), strides_.size() - nsubs),
                    value_type{ 0 }, std::plus<>{}, std::multiplies<>{});
            }

            template <iterable_of_type<value_type> Cont>
            [[nodiscard]] constexpr value_type subs2ind(const Cont& subs) const
            {
                return sub2ind(std::begin(subs), std::end(subs));
            }

            [[nodiscard]] constexpr value_type subs2ind(std::initializer_list<value_type> subs) const
            {
                return sub2ind(subs.begin(), subs.end());
            }

            constexpr arrnd_header(arrnd_header&& other) = default;
            constexpr arrnd_header& operator=(arrnd_header&& other) = default;

            constexpr arrnd_header(const arrnd_header& other) = default;
            constexpr arrnd_header& operator=(const arrnd_header& other) = default;

            virtual constexpr ~arrnd_header() = default;

            [[nodiscard]] constexpr value_type count() const noexcept
            {
                return count_;
            }

            [[nodiscard]] constexpr const storage_type& dims() const noexcept
            {
                return dims_;
            }

            [[nodiscard]] constexpr storage_type& dims() noexcept
            {
                return dims_;
            }

            [[nodiscard]] constexpr const storage_type& strides() const noexcept
            {
                return strides_;
            }

            [[nodiscard]] constexpr storage_type& strides() noexcept
            {
                return strides_;
            }

            [[nodiscard]] constexpr value_type offset() const noexcept
            {
                return offset_;
            }

            [[nodiscard]] constexpr bool is_subarray() const noexcept
            {
                return is_subarray_;
            }

            [[nodiscard]] constexpr bool is_axis_reordered() const noexcept
            {
                return is_axis_reordered_;
            }

            [[nodiscard]] constexpr bool empty() const noexcept
            {
                return dims_.empty();
            }

            [[nodiscard]] constexpr value_type last_index() const noexcept
            {
                return last_index_;
            }

        private:
            storage_type dims_{};
            storage_type strides_{};
            value_type count_{ 0 };
            value_type offset_{ 0 };
            value_type last_index_{ 0 };
            bool is_subarray_{ false };
            bool is_axis_reordered_{ false };
        };


        template <typename Header = arrnd_header<>>
        class arrnd_general_indexer final
        {
        public:
            using storage_type = Header::storage_type;
            using header_type = Header;
            using size_type = Header::size_type;
            using value_type = Header::value_type;

            constexpr arrnd_general_indexer(const header_type& hdr, bool backward = false)
                : hdr_(hdr)
            {
                setup(backward);
            }

            constexpr arrnd_general_indexer(const header_type& hdr, size_type axis, bool backward = false)
                : hdr_(hdr.reorder(axis))
            {
                setup(backward);
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr arrnd_general_indexer(const header_type& hdr, InputIt first_order, InputIt last_order, bool backward = false)
                : hdr_(hdr.reorder(first_order, last_order))
            {
                setup(backward);
            }

            template <iterable_of_type<size_type> Cont>
            constexpr arrnd_general_indexer(const header_type& hdr, const Cont& order, bool backward = false)
                : arrnd_general_indexer(hdr, std::begin(order), std::end(order), backward)
            {
            }

            constexpr arrnd_general_indexer(const header_type& hdr, std::initializer_list<size_type> order, bool backward = false)
                : arrnd_general_indexer(hdr, order.begin(), order.end(), backward)
            {
            }

            constexpr arrnd_general_indexer() = default;

            constexpr arrnd_general_indexer(const arrnd_general_indexer& other) = default;
            constexpr arrnd_general_indexer& operator=(const arrnd_general_indexer& other) = default;

            constexpr arrnd_general_indexer(arrnd_general_indexer&& other) noexcept = default;
            constexpr arrnd_general_indexer& operator=(arrnd_general_indexer&& other) noexcept = default;

            constexpr ~arrnd_general_indexer() = default;

            constexpr arrnd_general_indexer& operator++() noexcept
            {
                if (current_index_ < hdr_.offset()) {
                    current_index_ = hdr_.offset();
                    return *this;
                }
                if (current_index_ >= hdr_.last_index()) {
                    current_index_ = hdr_.last_index() + 1;
                    return *this;
                }
                ++rel_pos_;
                ++first_ind_;
                current_index_ += first_stride_;
                if (first_ind_ < first_dim_) {
                    return *this;
                }
                current_index_ -= first_ind_ * first_stride_;
                first_ind_ = 0;
                if (hdr_.dims().size() > 1) {
                    ++second_ind_;
                    current_index_ += second_stride_;
                    if (second_ind_ < second_dim_) {
                        return *this;
                    }
                    current_index_ -= second_ind_ * second_stride_;
                    second_ind_ = 0;
                }
                if (hdr_.dims().size() > 2) {
                    ++third_ind_;
                    current_index_ += third_stride_;
                    if (third_ind_ < third_dim_) {
                        return *this;
                    }
                    current_index_ -= third_ind_ * third_stride_;
                    third_ind_ = 0;
                }
                for (size_type i = hdr_.dims().size() - 4; i >= 0; --i) {
                    ++(*std::next(indices_.begin(), i));
                    current_index_ += *std::next(hdr_.strides().cbegin(), i);
                    if (*std::next(indices_.cbegin(), i) < *std::next(hdr_.dims().cbegin(), i)) {
                        return *this;
                    }
                    current_index_ -= *std::next(indices_.cbegin(), i) * *std::next(hdr_.strides().cbegin(), i);
                    *std::next(indices_.begin(), i) = 0;
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator++(int) noexcept
            {
                arrnd_general_indexer<header_type> temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_general_indexer& operator+=(size_type count) noexcept
            {
                for (size_type i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            arrnd_general_indexer operator+(size_type count) const noexcept
            {
                arrnd_general_indexer<header_type> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_general_indexer& operator--() noexcept
            {
                if (current_index_ <= hdr_.offset()) {
                    current_index_ = hdr_.offset() - 1;
                    return *this;
                }
                if (current_index_ == hdr_.last_index() + 1) {
                    current_index_ = hdr_.last_index();
                    return *this;
                }
                --rel_pos_;
                --first_ind_;
                current_index_ -= first_stride_;
                if (first_ind_ > -1) {
                    return *this;
                }
                first_ind_ = first_dim_ - 1;
                current_index_ += (first_ind_ + 1) * first_stride_;
                if (hdr_.dims().size() > 1) {
                    --second_ind_;
                    current_index_ -= second_stride_;
                    if (second_ind_ > -1) {
                        return *this;
                    }
                    second_ind_ = second_dim_ - 1;
                    current_index_ += (second_ind_ + 1) * second_stride_;
                }
                if (hdr_.dims().size() > 2) {
                    --third_ind_;
                    current_index_ -= third_stride_;
                    if (third_ind_ > -1) {
                        return *this;
                    }
                    third_ind_ = third_dim_ - 1;
                    current_index_ += (third_ind_ + 1) * third_stride_;
                }
                for (size_type i = hdr_.dims().size() - 4; i >= 0; --i) {
                    --(*std::next(indices_.begin(), i));
                    current_index_ -= *std::next(hdr_.strides().cbegin(), i);
                    if (*std::next(indices_.cbegin(), i) > -1) {
                        return *this;
                    }
                    *std::next(indices_.begin(), i) = *std::next(hdr_.dims().cbegin(), i) - 1;
                    current_index_ += (*std::next(indices_.cbegin(), i) + 1) * *std::next(hdr_.strides().cbegin(), i);
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator--(int) noexcept
            {
                arrnd_general_indexer<header_type> temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_general_indexer& operator-=(size_type count) noexcept
            {
                for (size_type i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr arrnd_general_indexer operator-(size_type count) const noexcept
            {
                arrnd_general_indexer<header_type> temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return static_cast<std::make_unsigned_t<value_type>>(current_index_ - hdr_.offset()) <= last_first_diff_
                    && !hdr_.empty();
            }

            [[nodiscard]] constexpr value_type operator*() const noexcept
            {
                return current_index_;
            }

            [[nodiscard]] constexpr value_type operator[](size_type index) const noexcept
            {
                assert(index >= 0 && index < hdr_.count());

                size_type advance_count = index - rel_pos_;
                if (advance_count > 0) {
                    return ((*this) + advance_count).current_index_;
                }
                else if (advance_count < 0) {
                    return ((*this) - (-advance_count)).current_index_;
                }
                return current_index_;
            }

        private:
            constexpr void setup(bool backward)
            {
                last_first_diff_ = static_cast<std::make_unsigned_t<value_type>>(hdr_.last_index() - hdr_.offset());

                if (hdr_.dims().size() > 0) {
                    first_dim_ = hdr_.dims().back();
                    first_stride_ = hdr_.strides().back();
                    first_ind_ = backward ? first_dim_ - 1 : 0;
                }

                if (hdr_.dims().size() > 1) {
                    second_dim_ = *std::next(hdr_.dims().cbegin(), hdr_.dims().size() - 2);
                    second_stride_ = *std::next(hdr_.strides().cbegin(), hdr_.dims().size() - 2);
                    second_ind_ = backward ? second_dim_ - 1 : 0;
                }

                if (hdr_.dims().size() > 2) {
                    third_dim_ = *std::next(hdr_.dims().cbegin(), hdr_.dims().size() - 3);
                    third_stride_ = *std::next(hdr_.strides().cbegin(), hdr_.dims().size() - 3);
                    third_ind_ = backward ? third_dim_ - 1 : 0;
                }

                if (hdr_.dims().size() > 3) {
                    indices_.resize(hdr_.dims().size() - 3);
                    for (size_type i = 0; i < indices_.size(); ++i) {
                        *std::next(indices_.begin(), i) = backward ? *std::next(hdr_.dims().cbegin(), i) - 1 : 0;
                    }
                }

                current_index_ = backward ? hdr_.last_index() : hdr_.offset();

                rel_pos_ = backward ? hdr_.count() - 1 : 0;
            }

            header_type hdr_;

            std::make_unsigned_t<value_type> last_first_diff_;

            value_type first_stride_;
            value_type first_dim_;
            value_type first_ind_;

            value_type second_stride_;
            value_type second_dim_;
            value_type second_ind_;

            value_type third_stride_;
            value_type third_dim_;
            value_type third_ind_;

            storage_type indices_;
            value_type current_index_;

            size_type rel_pos_ = 0;
        };




        template <typename Header = arrnd_header<>>
        class arrnd_fast_indexer final
        {
        public:
            using header_type = Header;
            using size_type = Header::size_type;
            using value_type = Header::value_type;

            constexpr arrnd_fast_indexer(const header_type& hdr, bool backward = false)
                : arrnd_fast_indexer(hdr, 0, backward)
            {
            }

            constexpr arrnd_fast_indexer(const header_type& hdr, size_type axis, bool backward = false)
            {
                assert(!hdr.is_subarray() && !hdr.is_axis_reordered());
                assert(axis >= 0 && axis < hdr.dims().size());

                // data

                last_index_ = hdr.last_index();

                num_super_groups_ = *std::next(hdr.dims().cbegin(), axis);
                step_size_between_super_groups_ = *std::next(hdr.strides().cbegin(), axis);

                num_groups_in_super_group_ =
                    std::accumulate(hdr.dims().cbegin(), std::next(hdr.dims().cbegin(), axis + 1), value_type{ 1 }, std::multiplies<>{}) / num_super_groups_;
                group_size_ = *std::next(hdr.strides().cbegin(), axis);
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

                    rel_pos_ = 0;
                }
                else {
                    group_indices_counter_ = group_size_ - 1;
                    groups_counter_ = num_groups_in_super_group_ - 1;
                    super_groups_counter_ = num_super_groups_ - 1;

                    super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
                    group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                    current_index_ = last_index_;

                    rel_pos_ = hdr.count() - 1;
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
                    ++rel_pos_;
                    return *this;
                }

                // group function

                group_indices_counter_ = 0;

                ++groups_counter_;
                group_start_index_ += step_size_between_groups_;

                current_index_ = group_start_index_;

                if (groups_counter_ < num_groups_in_super_group_) {
                    ++rel_pos_;
                    return *this;
                }

                // super group function

                groups_counter_ = 0;

                ++super_groups_counter_;
                super_group_start_index_ += step_size_between_super_groups_;

                group_start_index_ = super_group_start_index_;

                current_index_ = group_start_index_;

                if (super_groups_counter_ < num_super_groups_) {
                    ++rel_pos_;
                    return *this;
                }

                group_indices_counter_ = group_size_;
                groups_counter_ = num_groups_in_super_group_ - 1;
                super_groups_counter_ = num_super_groups_ - 1;

                super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
                group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                current_index_ = last_index_ + 1;

                ++rel_pos_;
                return *this;
            }

            constexpr arrnd_fast_indexer operator++(int) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_fast_indexer& operator+=(size_type count) noexcept
            {
                for (size_type i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            constexpr arrnd_fast_indexer operator+(size_type count) const noexcept
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
                    --rel_pos_;
                    return *this;
                }

                // group function

                group_indices_counter_ = group_size_ - 1;

                --groups_counter_;
                group_start_index_ -= step_size_between_groups_;

                current_index_ = group_start_index_ + (group_size_-1) * step_size_inside_group_;

                if (groups_counter_ >= 0) {
                    --rel_pos_;
                    return *this;
                }

                // super group function

                groups_counter_ = num_groups_in_super_group_ - 1;

                --super_groups_counter_;
                super_group_start_index_ -= step_size_between_super_groups_;

                group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

                current_index_ = group_start_index_ + (group_size_-1) * step_size_inside_group_;

                if (super_groups_counter_ >= 0) {
                    --rel_pos_;
                    return *this;
                }

                group_indices_counter_ = -1;
                groups_counter_ = 0;
                super_groups_counter_ = 0;

                super_group_start_index_ = 0;
                group_start_index_ = 0;

                current_index_ = -1;

                --rel_pos_;
                return *this;
            }

            constexpr arrnd_fast_indexer operator--(int) noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_fast_indexer& operator-=(size_type count) noexcept
            {
                for (size_type i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr arrnd_fast_indexer operator-(size_type count) const noexcept
            {
                arrnd_fast_indexer<header_type> temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return static_cast<std::make_unsigned_t<value_type>>(current_index_) <= static_cast<std::make_unsigned_t<value_type>>(last_index_);
            }

            [[nodiscard]] constexpr value_type operator*() const noexcept
            {
                return current_index_;
            }

            [[nodiscard]] constexpr value_type operator[](size_type index) const noexcept
            {
                assert(index >= 0 && index < num_super_groups_* num_groups_in_super_group_* group_size_);

                size_type advance_count = index - rel_pos_;
                if (advance_count > 0) {
                    return ((*this) + advance_count).current_index_;
                }
                else if (advance_count < 0) {
                    return ((*this) - (-advance_count)).current_index_;
                }
                return current_index_;
            }

        private:
            value_type current_index_ = 0;

            // data

            value_type last_index_ = 0;

            size_type num_super_groups_ = 0;
            value_type step_size_between_super_groups_ = 0;

            size_type num_groups_in_super_group_ = 0;
            size_type group_size_ = 0;
            value_type step_size_inside_group_ = 0;
            value_type step_size_between_groups_ = 0;
            
            // counters

            size_type super_groups_counter_ = 0;

            size_type group_indices_counter_ = 0;
            size_type groups_counter_ = 0;

            value_type super_group_start_index_ = 0;

            value_type group_start_index_ = 0;

            size_type rel_pos_ = 0;
        };

        template <typename Arrnd>
        class arrnd_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
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

            constexpr arrnd_iterator operator+(difference_type count) const noexcept
            {
                arrnd_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_iterator& operator--() noexcept
            {
                --gen_;
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

            constexpr arrnd_iterator operator-(difference_type count) const noexcept
            {
                arrnd_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_iterator& iter) const noexcept
            {
                return *gen_ == *(iter.gen_);
            }

            [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
            {
                return data_[gen_[index]];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_iterator& other) const noexcept
            {
                return *gen_ - *(other.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };




        template <typename Arrnd>
        class arrnd_const_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
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

            constexpr arrnd_const_iterator operator+(difference_type count) const noexcept
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

            constexpr arrnd_const_iterator operator-(difference_type count) const noexcept
            {
                arrnd_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const reference operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_const_iterator& iter) const noexcept
            {
                return *gen_ == *(iter.gen_);
            }

            [[nodiscard]] constexpr const reference operator[](difference_type index) noexcept
            {
                return data_[gen_[index]];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_const_iterator& other) const noexcept
            {
                return *gen_ - *(other.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };



        template <typename Arrnd>
        class arrnd_reverse_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
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

            constexpr arrnd_reverse_iterator operator+(difference_type count) const noexcept
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

            constexpr arrnd_reverse_iterator operator-(difference_type count) const noexcept
            {
                arrnd_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_reverse_iterator& iter) const noexcept
            {
                return *gen_ == *(iter.gen_);
            }

            [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
            {
                return data_[gen_[index]];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_reverse_iterator& other) const noexcept
            {
                return *gen_ - *(other.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };




        template <typename Arrnd>
        class arrnd_const_reverse_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
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

            constexpr arrnd_const_reverse_iterator operator+(difference_type count) const noexcept
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

            constexpr arrnd_const_reverse_iterator operator-(difference_type count) const noexcept
            {
                arrnd_const_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const reference operator*() noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_const_reverse_iterator& iter) const noexcept
            {
                return *gen_ == *(iter.gen_);
            }

            [[nodiscard]] constexpr const reference operator[](difference_type index) noexcept
            {
                return data_[gen_[index]];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_const_reverse_iterator& other) const noexcept
            {
                return *gen_ - *(other.gen_);
            }

        private:
            indexer_type gen_;
            pointer data_ = nullptr;
        };


        template <typename Header = arrnd_header<>>
        class arrnd_fixed_axis_ranger final
        {
        public:
            using header_type = Header;
            using size_type = Header::size_type;
            using value_type = Header::value_type;

            using storage_type = Header::storage_type::template replaced_type<interval<value_type>>;

            constexpr arrnd_fixed_axis_ranger(const header_type& hdr, size_type fixed_axis = 0, bool backward = false)
                : fixed_axis_(fixed_axis)
            {
                assert(fixed_axis >= 0 && fixed_axis < hdr.dims().size());

                current_index_ = backward ? *std::next(hdr.dims().cbegin(), fixed_axis) - 1 : 0;

                last_index_ = *std::next(hdr.dims().cbegin(), fixed_axis);

                ranges_ = storage_type(hdr.dims().size());
                for (size_type i = 0; i < hdr.dims().size(); ++i) {
                    ranges_[i] = { 0, *std::next(hdr.dims().cbegin(), i) - 1 };
                }
                ranges_[fixed_axis_] = { current_index_, current_index_ };
            }

            constexpr arrnd_fixed_axis_ranger() = default;

            constexpr arrnd_fixed_axis_ranger(const arrnd_fixed_axis_ranger& other) = default;
            constexpr arrnd_fixed_axis_ranger& operator=(const arrnd_fixed_axis_ranger& other) = default;

            constexpr arrnd_fixed_axis_ranger(arrnd_fixed_axis_ranger&& other) noexcept = default;
            constexpr arrnd_fixed_axis_ranger& operator=(arrnd_fixed_axis_ranger&& other) noexcept = default;

            constexpr ~arrnd_fixed_axis_ranger() = default;

            constexpr arrnd_fixed_axis_ranger& operator++() noexcept
            {
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                ++current_index_;
                ranges_[fixed_axis_] = interval<value_type>{ current_index_ , current_index_ };
                return *this;
            }

            constexpr arrnd_fixed_axis_ranger operator++(int) noexcept
            {
                arrnd_fixed_axis_ranger<header_type> temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_fixed_axis_ranger& operator+=(size_type count) noexcept
            {
                if (current_index_ > last_index_) {
                    current_index_ = last_index_;
                }
                current_index_ += count;
                ranges_[fixed_axis_] = interval<value_type>{ current_index_ , current_index_ };
                if (current_index_ >= last_index_) {
                    return *this;
                }
                return *this;
            }

            constexpr arrnd_fixed_axis_ranger operator+(size_type count) const noexcept
            {
                arrnd_fixed_axis_ranger<header_type> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_fixed_axis_ranger& operator--() noexcept
            {
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                --current_index_;
                ranges_[fixed_axis_] = interval<value_type>{ current_index_ , current_index_ };
                return *this;
            }

            constexpr arrnd_fixed_axis_ranger operator--(int) noexcept
            {
                arrnd_fixed_axis_ranger<header_type> temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_fixed_axis_ranger& operator-=(size_type count) noexcept
            {
                if (current_index_ < 0) {
                    current_index_ = -1;
                }
                current_index_ -= count;
                ranges_[fixed_axis_] = interval<value_type>{ current_index_ , current_index_ };
                if (current_index_ < 0) {
                    return *this;
                }
                return *this;
            }

            constexpr arrnd_fixed_axis_ranger operator-(size_type count) const noexcept
            {
                arrnd_fixed_axis_ranger<header_type> temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return current_index_ >= 0 && current_index_ < last_index_;
            }

            [[nodiscard]] constexpr const storage_type& operator*() const noexcept
            {
                return ranges_;
            }

            [[nodiscard]] constexpr storage_type operator[](size_type index) const noexcept
            {
                assert(index >= 0 && index <= last_index_);

                size_type advance_count = index - current_index_;
                if (advance_count > 0) {
                    return ((*this) + advance_count).ranges_;
                }
                else if (advance_count < 0) {
                    return ((*this) - (-advance_count)).ranges_;
                }
                return ranges_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_fixed_axis_ranger& far) const noexcept
            {
                return current_index_ == far.current_index_;
            }

            [[nodiscard]] constexpr size_type fixed_axis() const noexcept
            {
                return fixed_axis_;
            }

        private:
            size_type fixed_axis_;
            value_type current_index_;
            value_type last_index_;
            storage_type ranges_;
        };


        template <typename Arrnd>
        class arrnd_axis_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
            using value_type = Arrnd;
            using reference = Arrnd&;

            using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

            constexpr arrnd_axis_iterator(const value_type& arrnd_ref, const ranger_type& far)
                : arrnd_ref_(arrnd_ref), far_(far)
            {
                if (far) {
                    slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
                }
            }

            constexpr arrnd_axis_iterator() = default;

            constexpr arrnd_axis_iterator(const arrnd_axis_iterator& other) = default;
            constexpr arrnd_axis_iterator& operator=(const arrnd_axis_iterator& other) = default;

            constexpr arrnd_axis_iterator(arrnd_axis_iterator&& other) noexcept = default;
            constexpr arrnd_axis_iterator& operator=(arrnd_axis_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_iterator() = default;

            constexpr arrnd_axis_iterator& operator++() noexcept
            {
                ++far_;
                return *this;
            }

            constexpr arrnd_axis_iterator operator++(int) noexcept
            {
                arrnd_axis_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_iterator& operator+=(difference_type count) noexcept
            {
                far_ += count;
                return *this;
            }

            constexpr arrnd_axis_iterator operator+(difference_type count) const
            {
                arrnd_axis_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_iterator& operator--() noexcept
            {
                --far_;
                return *this;
            }

            constexpr arrnd_axis_iterator operator--(int) noexcept
            {
                arrnd_axis_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_iterator& operator-=(difference_type count) noexcept
            {
                far_ -= count;
                return *this;
            }

            constexpr arrnd_axis_iterator operator-(difference_type count) const noexcept
            {
                arrnd_axis_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() noexcept
            {
                slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_iterator& iter) const noexcept
            {
                return far_ == iter.far_;
            }

            [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
            {
                auto ranges = far_[index];
                return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_iterator& other) const noexcept
            {
                return (*far_)[far_.fixed_axis()].start - (*other.far_)[far_.fixed_axis()].start;
            }

        private:
            value_type arrnd_ref_;
            ranger_type far_;

            value_type slice_;
        };

        template <typename Arrnd>
        class arrnd_axis_const_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
            using value_type = Arrnd;
            using const_reference = const Arrnd&;

            using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

            constexpr arrnd_axis_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
                : arrnd_ref_(arrnd_ref), far_(far)
            {
                if (far) {
                    slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
                }
            }

            constexpr arrnd_axis_const_iterator() = default;

            constexpr arrnd_axis_const_iterator(const arrnd_axis_const_iterator& other) = default;
            constexpr arrnd_axis_const_iterator& operator=(const arrnd_axis_const_iterator& other) = default;

            constexpr arrnd_axis_const_iterator(arrnd_axis_const_iterator&& other) noexcept = default;
            constexpr arrnd_axis_const_iterator& operator=(arrnd_axis_const_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_const_iterator() = default;

            constexpr arrnd_axis_const_iterator& operator++() noexcept
            {
                ++far_;
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator++(int) noexcept
            {
                arrnd_axis_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator+=(difference_type count) noexcept
            {
                far_ += count;
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator+(difference_type count) const noexcept
            {
                arrnd_axis_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator--() noexcept
            {
                --far_;
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator--(int) noexcept
            {
                arrnd_axis_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_const_iterator& operator-=(difference_type count) noexcept
            {
                far_ -= count;
                return *this;
            }

            constexpr arrnd_axis_const_iterator operator-(difference_type count) const noexcept
            {
                arrnd_axis_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const_reference operator*() noexcept
            {
                slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_const_iterator& iter) const noexcept
            {
                return far_ == iter.far_;
            }

            [[nodiscard]] constexpr const_reference operator[](difference_type index) noexcept
            {
                auto ranges = far_[index];
                return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_const_iterator& other) const noexcept
            {
                return (*far_)[far_.fixed_axis()].start - (*other.far_)[far_.fixed_axis()].start;
            }

        private:
            value_type arrnd_ref_;
            ranger_type far_;

            value_type slice_;
        };



        template <typename Arrnd>
        class arrnd_axis_reverse_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
            using value_type = Arrnd;
            using reference = Arrnd&;

            using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

            constexpr arrnd_axis_reverse_iterator(const value_type& arrnd_ref, const ranger_type& far)
                : arrnd_ref_(arrnd_ref), far_(far)
            {
                if (far) {
                    slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
                }
            }

            constexpr arrnd_axis_reverse_iterator() = default;

            constexpr arrnd_axis_reverse_iterator(const arrnd_axis_reverse_iterator& other) = default;
            constexpr arrnd_axis_reverse_iterator& operator=(const arrnd_axis_reverse_iterator& other) = default;

            constexpr arrnd_axis_reverse_iterator(arrnd_axis_reverse_iterator&& other) noexcept = default;
            constexpr arrnd_axis_reverse_iterator& operator=(arrnd_axis_reverse_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_reverse_iterator() = default;

            constexpr arrnd_axis_reverse_iterator& operator--() noexcept
            {
                ++far_;
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator--(int) noexcept
            {
                arrnd_axis_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator-=(difference_type count) noexcept
            {
                far_ += count;
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator-(difference_type count) const noexcept
            {
                arrnd_axis_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator++() noexcept
            {
                --far_;
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator++(int) noexcept
            {
                arrnd_axis_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_iterator& operator+=(difference_type count) noexcept
            {
                far_ -= count;
                return *this;
            }

            constexpr arrnd_axis_reverse_iterator operator+(difference_type count) const noexcept
            {
                arrnd_axis_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr reference operator*() noexcept
            {
                slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_iterator& iter) const noexcept
            {
                return far_ == iter.far_;
            }

            [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
            {
                auto ranges = far_[index];
                return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_reverse_iterator& other) const noexcept
            {
                return (*far_)[far_.fixed_axis()].start - (*other.far_)[far_.fixed_axis()].start;
            }

        private:
            value_type arrnd_ref_;
            ranger_type far_;

            value_type slice_;
        };

        template <typename Arrnd>
        class arrnd_axis_reverse_const_iterator final
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using difference_type = Arrnd::size_type;
            using value_type = Arrnd;
            using const_reference = const Arrnd&;

            using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

            constexpr arrnd_axis_reverse_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
                : arrnd_ref_(arrnd_ref), far_(far)
            {
                if (far) {
                    slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
                }
            }

            constexpr arrnd_axis_reverse_const_iterator() = default;

            constexpr arrnd_axis_reverse_const_iterator(const arrnd_axis_reverse_const_iterator& other) = default;
            constexpr arrnd_axis_reverse_const_iterator& operator=(const arrnd_axis_reverse_const_iterator& other) = default;

            constexpr arrnd_axis_reverse_const_iterator(arrnd_axis_reverse_const_iterator&& other) noexcept = default;
            constexpr arrnd_axis_reverse_const_iterator& operator=(arrnd_axis_reverse_const_iterator&& other) noexcept = default;

            constexpr ~arrnd_axis_reverse_const_iterator() = default;

            constexpr arrnd_axis_reverse_const_iterator& operator--() noexcept
            {
                ++far_;
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator--(int) noexcept
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator-=(difference_type count) noexcept
            {
                far_ += count;
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator-(difference_type count) const noexcept
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator++() noexcept
            {
                --far_;
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator++(int) noexcept
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr arrnd_axis_reverse_const_iterator& operator+=(difference_type count) noexcept
            {
                far_ -= count;
                return *this;
            }

            constexpr arrnd_axis_reverse_const_iterator operator+(difference_type count) const noexcept
            {
                arrnd_axis_reverse_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr const_reference operator*() noexcept
            {
                slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
                return slice_;
            }

            [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_const_iterator& iter) const noexcept
            {
                return far_ == iter.far_;
            }

            [[nodiscard]] constexpr const_reference operator[](difference_type index) noexcept
            {
                auto ranges = far_[index];
                return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
            }

            [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_reverse_const_iterator& other) const noexcept
            {
                return (*far_)[far_.fixed_axis()].start - (*other.far_)[far_.fixed_axis()].start;
            }

        private:
            value_type arrnd_ref_;
            ranger_type far_;

            value_type slice_;
        };



        template <typename Arrnd>
        class arrnd_back_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_back_insert_iterator() noexcept = default;

            explicit arrnd_back_insert_iterator(Arrnd& cont) noexcept
                : cont_(std::addressof(cont))
            {
            }

            arrnd_back_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->append(cont);
                return *this;
            }

            arrnd_back_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->append(std::move(cont));
                return *this;
            }

            [[nodiscard]] arrnd_back_insert_iterator& operator*() noexcept
            {
                return *this;
            }

            arrnd_back_insert_iterator& operator++() noexcept
            {
                return *this;
            }

            arrnd_back_insert_iterator operator++(int) noexcept
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_back_insert_iterator<Arrnd> arrnd_back_inserter(Arrnd& cont) noexcept
        {
            return arrnd_back_insert_iterator<Arrnd>(cont);
        }

        template <typename Arrnd>
        class arrnd_front_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_front_insert_iterator() noexcept = default;

            explicit arrnd_front_insert_iterator(Arrnd& cont)
                : cont_(std::addressof(cont))
            {
            }

            arrnd_front_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->insert(cont, 0);
                return *this;
            }

            arrnd_front_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->insert(std::move(cont), 0);
                return *this;
            }

            [[nodiscard]] arrnd_front_insert_iterator& operator*()
            {
                return *this;
            }

            arrnd_front_insert_iterator& operator++()
            {
                return *this;
            }

            arrnd_front_insert_iterator operator++(int)
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_front_insert_iterator<Arrnd> arrnd_front_inserter(Arrnd& cont)
        {
            return arrnd_front_insert_iterator<Arrnd>(cont);
        }

        template <typename Arrnd>
        class arrnd_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_insert_iterator() noexcept = default;

            explicit arrnd_insert_iterator(Arrnd& cont, typename Arrnd::size_type ind = 0)
                : cont_(std::addressof(cont)), ind_(ind)
            {
            }

            arrnd_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->insert(cont, ind_);
                ind_ += cont.header().count();
                return *this;
            }

            arrnd_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->insert(std::move(cont), ind_);
                ind_ += cont.header().count();
                return *this;
            }

            [[nodiscard]] arrnd_insert_iterator& operator*()
            {
                return *this;
            }

            arrnd_insert_iterator& operator++()
            {
                return *this;
            }

            arrnd_insert_iterator operator++(int)
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
            typename Arrnd::size_type ind_;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_insert_iterator<Arrnd> arrnd_inserter(Arrnd& cont, typename Arrnd::size_type ind = 0)
        {
            return arrnd_insert_iterator<Arrnd>(cont, ind);
        }


        template <typename Arrnd>
        class arrnd_axis_back_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_axis_back_insert_iterator() noexcept = default;

            explicit arrnd_axis_back_insert_iterator(Arrnd& cont, typename Arrnd::size_type axis = 0) noexcept
                : cont_(std::addressof(cont)), axis_(axis)
            {
            }

            arrnd_axis_back_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->append(cont, axis_);
                return *this;
            }

            arrnd_axis_back_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->append(std::move(cont), axis_);
                return *this;
            }

            [[nodiscard]] arrnd_axis_back_insert_iterator& operator*() noexcept
            {
                return *this;
            }

            arrnd_axis_back_insert_iterator& operator++() noexcept
            {
                return *this;
            }

            arrnd_axis_back_insert_iterator operator++(int) noexcept
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
            typename Arrnd::size_type axis_;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_axis_back_insert_iterator<Arrnd> arrnd_axis_back_inserter(Arrnd& cont, typename Arrnd::size_type axis = 0) noexcept
        {
            return arrnd_axis_back_insert_iterator<Arrnd>(cont, axis);
        }

        template <typename Arrnd>
        class arrnd_axis_front_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_axis_front_insert_iterator() noexcept = default;

            explicit arrnd_axis_front_insert_iterator(Arrnd& cont, typename Arrnd::size_type axis = 0)
                : cont_(std::addressof(cont)), axis_(axis)
            {
            }

            arrnd_axis_front_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->insert(cont, 0, axis_);
                return *this;
            }

            arrnd_axis_front_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->insert(std::move(cont), 0, axis_);
                return *this;
            }

            [[nodiscard]] arrnd_axis_front_insert_iterator& operator*()
            {
                return *this;
            }

            arrnd_axis_front_insert_iterator& operator++()
            {
                return *this;
            }

            arrnd_axis_front_insert_iterator operator++(int)
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
            typename Arrnd::size_type axis_;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_axis_front_insert_iterator<Arrnd> arrnd_axis_front_inserter(Arrnd& cont, typename Arrnd::size_type axis = 0)
        {
            return arrnd_axis_front_insert_iterator<Arrnd>(cont, axis);
        }

        template <typename Arrnd>
        class arrnd_axis_insert_iterator {
        public:
            using iterator_category = std::output_iterator_tag;

            constexpr arrnd_axis_insert_iterator() noexcept = default;

            explicit arrnd_axis_insert_iterator(Arrnd& cont, typename Arrnd::size_type ind = 0, typename Arrnd::size_type axis = 0)
                : cont_(std::addressof(cont)), ind_(ind), axis_(axis)
            {
            }

            arrnd_axis_insert_iterator& operator=(const Arrnd& cont)
            {
                *cont_ = cont_->insert(cont, ind_, axis_);
                ind_ += cont.header().dims()[axis_];
                return *this;
            }

            arrnd_axis_insert_iterator& operator=(Arrnd&& cont)
            {
                *cont_ = cont_->insert(std::move(cont), ind_, axis_);
                ind_ += cont.header().dims()[axis_];
                return *this;
            }

            [[nodiscard]] arrnd_axis_insert_iterator& operator*()
            {
                return *this;
            }

            arrnd_axis_insert_iterator& operator++()
            {
                return *this;
            }

            arrnd_axis_insert_iterator operator++(int)
            {
                return *this;
            }

        protected:
            Arrnd* cont_ = nullptr;
            typename Arrnd::size_type ind_;
            typename Arrnd::size_type axis_;
        };

        template <typename Arrnd>
        [[nodiscard]] inline constexpr arrnd_axis_insert_iterator<Arrnd> arrnd_axis_inserter(Arrnd& cont, typename Arrnd::size_type ind = 0, typename Arrnd::size_type axis = 0)
        {
            return arrnd_axis_insert_iterator<Arrnd>(cont, ind, axis);
        }


        template <typename T, random_access_type Storage = simple_dynamic_vector<T>, template<typename> typename SharedRefAllocator = lightweight_allocator, typename Header = arrnd_header<>, template<typename> typename Indexer = arrnd_general_indexer>
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
            using indexer_type = Indexer<Header>;
            using ranger_type = arrnd_fixed_axis_ranger<Header>;

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
                : arrnd(other.header().dims().cbegin(), other.header().dims().cend())
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
                *this = this_type(other.header().dims().cbegin(), other.header().dims().cend());
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
                : arrnd(other.header().dims().cbegin(), other.header().dims().cend())
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
                *this = this_type(other.header().dims().cbegin(), other.header().dims().cend());
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
                : hdr_(first_dim, last_dim), buffsp_(hdr_.empty() ? nullptr : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                std::copy(first_data, std::next(first_data, hdr_.count()), buffsp_->data());
            }
            template <iterable_of_type<size_type> Cont, typename InputDataIt>
            requires std::input_iterator<InputDataIt>
            explicit constexpr arrnd(const Cont& dims, InputDataIt first_data)
                : arrnd(std::begin(dims), std::end(dims), first_data)
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
            template <iterable_of_type<size_type> Cont, typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(const Cont& dims, std::initializer_list<U> data)
                : arrnd(std::begin(dims), std::end(dims), data.begin())
            {
            }
            template <typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<U> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const_pointer first_data)
                : hdr_(first_dim, last_dim), buffsp_(hdr_.empty() ? nullptr : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count(), first_data))
            {
            }
            template <iterable_of_type<size_type> Cont>
            explicit constexpr arrnd(const Cont& dims, const_pointer first_data)
                : arrnd(std::begin(dims), std::end(dims), first_data)
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
            template <iterable_of_type<size_type> Cont>
            explicit constexpr arrnd(const Cont& dims, std::initializer_list<T> data)
                : arrnd(std::begin(dims), std::end(dims), data.begin())
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<T> data)
                : arrnd(dims.begin(), dims.end(), data.begin())
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim)
                : hdr_(first_dim, last_dim), buffsp_(hdr_.empty() ? nullptr : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
            }
            template <iterable_of_type<size_type> Cont>
            explicit constexpr arrnd(const Cont& dims)
                : arrnd(std::begin(dims), std::end(dims))
            {
            }
            explicit constexpr arrnd(std::initializer_list<size_type> dims)
                : arrnd(dims.begin(), dims.end())
            {
            }

            template <typename InputDimsIt, typename U>
            requires (std::input_iterator<InputDimsIt> && !(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const U& value)
                : hdr_(first_dim, last_dim), buffsp_(hdr_.empty() ? nullptr : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                if (buffsp_) {
                    std::fill(buffsp_->begin(), buffsp_->end(), value);
                }
            }
            template <iterable_of_type<size_type> Cont, typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(const Cont& dims, const U& value)
                : arrnd(std::begin(dims), std::end(dims), value)
            {
            }
            template <typename U>
            requires (!(std::is_pointer_v<U> || std::is_array_v<U>))
            explicit constexpr arrnd(std::initializer_list<size_type> dims, const U& value)
                : arrnd(dims.begin(), dims.end(), value)
            {
            }

            template <typename InputDimsIt>
            requires std::input_iterator<InputDimsIt>
            explicit constexpr arrnd(InputDimsIt first_dim, InputDimsIt last_dim, const_reference value)
                : hdr_(first_dim, last_dim), buffsp_(hdr_.empty() ? nullptr : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.count()))
            {
                if (buffsp_) {
                    std::fill(buffsp_->begin(), buffsp_->end(), value);
                }
            }
            template <iterable_of_type<size_type> Cont>
            explicit constexpr arrnd(const Cont& dims, const_reference value)
                : arrnd(std::begin(dims), std::end(dims), value)
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

            [[nodiscard]] constexpr const auto& storage() const noexcept
            {
                return buffsp_;
            }

            [[nodiscard]] constexpr auto& storage() noexcept
            {
                return buffsp_;
            }

            [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
            {
                assert(index >= hdr_.offset() && index <= hdr_.last_index());
                return buffsp_->data()[index];
            }
            [[nodiscard]] constexpr reference operator[](size_type index) noexcept
            {
                assert(index >= hdr_.offset() && index <= hdr_.last_index());
                return buffsp_->data()[index];
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr const_reference operator[](std::pair<InputIt, InputIt> subs) const noexcept
            {
                return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
            }
            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr const_reference operator[](const Cont& subs) const noexcept
            {
                return buffsp_->data()[std::make_pair(std::begin(subs), std::end(subs))];
            }
            [[nodiscard]] constexpr const_reference operator[](std::initializer_list<size_type> subs) const noexcept
            {
                return (*this)[std::make_pair(subs.begin(), subs.end())];
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr reference operator[](std::pair<InputIt, InputIt> subs) noexcept
            {
                return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
            }
            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr reference operator[](const Cont& subs) noexcept
            {
                return buffsp_->data()[std::make_pair(std::begin(subs), std::end(subs))];
            }
            [[nodiscard]] constexpr reference operator[](std::initializer_list<size_type> subs) noexcept
            {
                return (*this)[std::make_pair(subs.begin(), subs.end())];
            }

            template <typename InputIt> requires std::is_same_v<interval<size_type>, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr shared_ref<this_type> operator[](std::pair<InputIt, InputIt> ranges) const
            {
                this_type slice{};
                slice.hdr_ = hdr_.subheader(ranges.first, ranges.second);
                slice.buffsp_ = buffsp_;
                return slice;
            }
            template <iterable_of_type<interval<size_type>> Cont>
            [[nodiscard]] constexpr shared_ref<this_type> operator[](const Cont& ranges) const
            {
                return (*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
            }
            [[nodiscard]] constexpr shared_ref<this_type> operator[](std::initializer_list<interval<size_type>> ranges) const
            {
                return (*this)[std::make_pair(ranges.begin(), ranges.end())];
            }

            [[nodiscard]] constexpr shared_ref<this_type> operator[](interval<size_type> range) const
            {
                this_type slice{};
                slice.hdr_ = hdr_.subheader(range);
                slice.buffsp_ = buffsp_;
                return slice;
            }

            template <arrnd_complient ArCo> requires std::is_integral_v<typename ArCo::value_type>
            [[nodiscard]] constexpr this_type operator[](const ArCo& indices) const noexcept
            {
                this_type res(indices.header().dims().cbegin(), indices.header().dims().cend());

                indexer_type res_gen(res.header());
                typename ArCo::indexer_type ind_gen(indices.header());

                for (; res_gen && ind_gen; ++res_gen, ++ind_gen) {
                    res[*res_gen] = (*this)[indices[*ind_gen]];
                }

                return res;
            }

            [[nodiscard]] constexpr bool empty() const noexcept
            {
                return hdr_.empty() && (hdr_.is_subarray() || !buffsp_);
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

            template <arrnd_complient ArCo, typename InputIt> requires std::is_same_v<interval<size_type>, iterator_value_type<InputIt>>
            constexpr const this_type& copy_to(ArCo& dst, InputIt first_range, InputIt last_range) const
            {
                auto slice = dst[std::make_pair(first_range, last_range)];
                copy_to(slice);
                return *this;
            }
            template <arrnd_complient ArCo, iterable_of_type<interval<size_type>> Cont>
            constexpr const this_type& copy_to(ArCo& dst, const Cont& ranges) const
            {
                return copy_to(dst, std::begin(ranges), std::end(ranges));
            }
            template <arrnd_complient ArCo>
            constexpr const this_type& copy_to(ArCo& dst, std::initializer_list<interval<size_type>> ranges) const
            {
                return copy_to(dst, ranges.begin(), ranges.end());
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
                    dst = ArCo{ header().dims().cbegin(), header().dims().cend() };
                    return copy_to(dst);
                }

                if (header().count() == dst.header().count()) {
                    if (header().dims() != dst.header().dims()) {
                        dst.header() = header_type{ header().dims().cbegin(), header().dims().cend() };
                    }
                    return copy_to(dst);
                }

                dst = ArCo{ header().dims().cbegin(), header().dims().cend() };
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

            template <arrnd_complient ArCo, typename InputIt> requires std::is_same_v<interval<size_type>, iterator_value_type<InputIt>>
            constexpr this_type& copy_from(const ArCo& src, InputIt first_range, InputIt last_range)
            {
                src.copy_to(*this, first_range, last_range);
                return *this;
            }
            template <arrnd_complient ArCo, iterable_of_type<interval<size_type>> Cont>
            constexpr this_type& copy_from(const ArCo& src, const Cont& ranges)
            {
                return copy_from(src, std::begin(ranges), std::end(ranges));
            }
            template <arrnd_complient ArCo>
            constexpr this_type& copy_from(const ArCo& src, std::initializer_list<interval<size_type>> ranges)
            {
                return copy_from(src, ranges.begin(), ranges.end());
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

                this_type clone(header().dims().cbegin(), header().dims().cend());

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
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(InputIt first_new_dim, InputIt last_new_dim) const
            {
                if (hdr_.is_subarray()) {
                    return resize(first_new_dim, last_new_dim);
                }

                typename this_type::header_type new_header(first_new_dim, last_new_dim);
                if (hdr_.count() != new_header.count()) {
                    return resize(first_new_dim, last_new_dim);
                }

                if (std::equal(header().dims().cbegin(), header().dims().cend(), first_new_dim, last_new_dim)) {
                    return *this;
                }

                this_type res(*this);
                res.header() = std::move(new_header);

                return res;
            }
            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const Cont& new_dims) const
            {
                return reshape(std::begin(new_dims), std::end(new_dims));
            }
            [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<size_type> new_dims) const
            {
                return reshape(new_dims.begin(), new_dims.end());
            }

            /*
            * @note Other references to this array buffer are not modified. Resize is not performed if dims and new_dims are equal.
            */
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(InputIt first_new_dim, InputIt last_new_dim) const
            {
                if (std::equal(header().dims().cbegin(), header().dims().cend(), first_new_dim, last_new_dim)) {
                    return *this;
                }

                if (empty()) {
                    return this_type(first_new_dim, last_new_dim);
                }

                this_type res(first_new_dim, last_new_dim);

                indexer_type gen(header());
                indexer_type res_gen(res.header());

                while (gen && res_gen) {
                    res[*res_gen] = (*this)[*gen];
                    ++gen;
                    ++res_gen;
                }

                return res;
            }
            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const Cont& new_dims) const
            {
                return resize(std::begin(new_dims), std::end(new_dims));
            }
            [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<size_type> new_dims) const
            {
                return resize(new_dims.begin(), new_dims.end());
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

                indexer_type res_gen(res.header());
                typename ArCo::indexer_type arr_gen(arr.header());

                res_gen += hdr_.count();
                for (; res_gen && arr_gen; ++res_gen, ++arr_gen) {
                    res[*res_gen] = arr[*arr_gen];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, size_type axis) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                header_type new_header(header().subheader(arr.header().dims()[axis], axis));
                if (new_header.empty()) {
                    return this_type{};
                }

                this_type res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), axis);
                typename ArCo::indexer_type arr_gen(arr.header(), axis);
                indexer_type res_gen(res.header(), axis);

                auto ptr = storage()->data();
                auto res_ptr = res.storage()->data();
                auto arr_ptr = arr.storage()->data();

                for (; gen && res_gen; ++gen, ++res_gen) {
                    res_ptr[*res_gen] = ptr[*gen];
                }
                for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                    res_ptr[*res_gen] = arr_ptr[*arr_gen];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                assert(ind >= 0 && ind <= hdr_.count());

                this_type res({ header().count() + arr.header().count() });

                indexer_type gen(header());
                indexer_type res_gen(res.header());
                typename ArCo::indexer_type arr_gen(arr.header());

                for (size_type i = 0; i < ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                    res[*res_gen] = (*this)[*gen];
                }
                for (size_type i = 0; i < arr.header().count() && arr_gen && res_gen; ++i, ++arr_gen, ++res_gen) {
                    res[*res_gen] = arr[*arr_gen];
                }
                for (size_type i = 0; i < header().count() - ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                    res[*res_gen] = (*this)[*gen];
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind, size_type axis) const
            {
                if (empty()) {
                    this_type res(arr);
                    return res.clone();
                }

                if (arr.empty()) {
                    return *this;
                }

                header_type new_header(header().subheader(arr.header().dims()[axis], axis));
                if (new_header.empty()) {
                    return this_type();
                }

                assert(ind >= 0 && ind < hdr_.dims()[axis]);

                this_type res({ header().count() + arr.header().count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), axis);
                typename ArCo::indexer_type arr_gen(arr.header(), axis);
                indexer_type res_gen(res.header(), axis);

                size_type cycle = ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), size_type{ 1 }, std::multiplies<>{}) / res.header().dims()[axis]);

                auto ptr = storage()->data();
                auto res_ptr = res.storage()->data();
                auto arr_ptr = arr.storage()->data();

                for (; gen && res_gen && cycle; --cycle, ++gen, ++res_gen) {
                    res_ptr[*res_gen] = ptr[*gen];
                }
                for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                    res_ptr[*res_gen] = arr_ptr[*arr_gen];
                }
                for (; gen && res_gen; ++gen, ++res_gen) {
                    res_ptr[*res_gen] = ptr[*gen];
                }

                return res;
            }

            /**
            * @note All elements starting from ind are being removed in case that count value is too big.
            */
            [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count) const
            {
                if (empty()) {
                    return *this;
                }

                assert(ind >= 0 && ind < hdr_.count());
                assert(ind + count <= hdr_.count());

                this_type res({ header().count() - count });

                indexer_type gen(header());
                indexer_type res_gen(res.header());

                for (size_type i = 0; i < ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                    res[*res_gen] = (*this)[*gen];
                }
                gen += count;
                for (size_type i = ind + count; i < header().count() && gen && res_gen; ++i, ++gen, ++res_gen) {
                    res[*res_gen] = (*this)[*gen];
                }

                return res;
            }

            /**
            * @note All elements starting from ind are being removed in case that count value is too big.
            */
            [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count, size_type axis) const
            {
                if (empty()) {
                    return *this;
                }

                header_type new_header(header().subheader(-count, axis));

                assert(ind >= 0 && ind < hdr_.dims()[axis]);
                assert(ind + count <= hdr_.dims()[axis]);

                if (new_header.empty()) {
                    return this_type();
                }

                this_type res({ header().count() - (header().count() / header().dims()[axis]) * count });
                res.header() = std::move(new_header);

                indexer_type gen(header(), axis);
                indexer_type res_gen(res.header(), axis);

                size_type cycle = ind *
                    (std::accumulate(res.header().dims().begin(), res.header().dims().end(), size_type{ 1 }, std::multiplies<>{}) / res.header().dims()[axis]);

                size_type removals = header().count() - res.header().count();

                auto ptr = storage()->data();
                auto res_ptr = res.storage()->data();

                for (; gen && res_gen && cycle; --cycle, ++gen, ++res_gen) {
                    res_ptr[*res_gen] = ptr[*gen];
                }
                for (; gen && removals; --removals, ++gen) {
                    //ptr[*gen] = arr_ptr[*arr_gen];
                }
                for (; gen && res_gen; ++gen, ++res_gen) {
                    res_ptr[*res_gen] = ptr[*gen];
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

                replaced_type<U> res(header().dims().cbegin(), header().dims().cend());

                indexer_type gen(header());
                typename replaced_type<U>::indexer_type res_gen(res.header());

                for (; gen && res_gen; ++gen, ++res_gen) {
                    res[*res_gen] = op((*this)[*gen]);
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

                replaced_type<U> res(hdr_.dims().cbegin(), hdr_.dims().cend());

                indexer_type gen(header());
                typename replaced_type<U>::indexer_type arr_gen(arr.header());
                typename replaced_type<U>::indexer_type res_gen(res.header());

                for (; gen && arr_gen && res_gen; ++gen, ++arr_gen, ++res_gen) {
                    res[*res_gen] = op((*this)[*gen], arr[*arr_gen]);
                }

                return res;
            }

            template <typename V, typename Binary_op> requires std::is_invocable_v<Binary_op, T, V>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, T, V>> transform(const V& value, Binary_op&& op) const
            {
                replaced_type<std::invoke_result_t<Binary_op, T, V>> res(hdr_.dims().cbegin(), hdr_.dims().cend());

                indexer_type gen(header());
                typename replaced_type<std::invoke_result_t<Binary_op, T, V>>::indexer_type res_gen(res.header());

                for (; gen && res_gen; ++gen, ++res_gen) {
                    res[*res_gen] = op((*this)[*gen], value);
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
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, T, T>> reduce(Binary_op&& op, size_type axis) const
            {
                using U = std::invoke_result_t<Binary_op, T, T>;

                if (empty()) {
                    return replaced_type<U>();
                }

                typename replaced_type<U>::header_type new_header(header().subheader(axis));
                if (new_header.empty()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), std::ssize(header().dims()) - axis - 1);
                indexer_type res_gen(res.header());

                const size_type reduction_iteration_cycle{ header().dims()[axis] };

                while (gen && res_gen) {
                    U res_element{ static_cast<U>((*this)[*gen]) };
                    ++gen;
                    for (size_type i = 0; i < reduction_iteration_cycle - 1; ++i, ++gen) {
                        res_element = op(res_element, (*this)[*gen]);
                    }
                    res[*res_gen] = res_element;
                    ++res_gen;
                }

                return res;
            }

            template <arrnd_complient ArCo, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo::value_type, T>
            [[nodiscard]] constexpr replaced_type<std::invoke_result_t<Binary_op, typename ArCo::value_type, T>> reduce(const ArCo& init_values, Binary_op&& op, size_type axis) const
            {
                using U = std::invoke_result_t<Binary_op, typename ArCo::value_type, T>;

                if (empty()) {
                    return replaced_type<U>();
                }

                typename replaced_type<U>::header_type new_header(header().subheader(axis));

                assert(init_values.header().dims().size() == 1 && init_values.header().dims()[0] == hdr_.count() / hdr_.dims()[axis]);

                if (new_header.empty()) {
                    return replaced_type<U>();
                }

                replaced_type<U> res({ new_header.count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), std::ssize(header().dims()) - axis - 1);
                indexer_type res_gen(res.header());
                typename ArCo::indexer_type init_gen(init_values.header());

                const size_type reduction_iteration_cycle{ header().dims()[axis] };

                while (gen && res_gen && init_gen) {
                    U res_element{ init_values[*init_gen] };
                    for (size_type i = 0; i < reduction_iteration_cycle; ++i, ++gen) {
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

                size_type res_count{ 0 };

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

                assert(header().dims() == mask.header().dims());

                this_type res({ header().count() });

                indexer_type gen(header());
                typename ArCo::indexer_type mask_gen(mask.header());

                indexer_type res_gen(res.header());

                size_type res_count{ 0 };

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
            [[nodiscard]] constexpr replaced_type<size_type> find(Unary_pred pred) const
            {
                if (empty()) {
                    return replaced_type<size_type>();
                }

                replaced_type<size_type> res({ header().count() });

                indexer_type gen(header());
                typename replaced_type<size_type>::indexer_type res_gen(res.header());

                size_type res_count{ 0 };

                while (gen && res_gen) {
                    if (pred((*this)[*gen])) {
                        res[*res_gen] = *gen;
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                }

                if (res_count == 0) {
                    return replaced_type<size_type>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <arrnd_complient ArCo>
            [[nodiscard]] constexpr replaced_type<size_type> find(const ArCo& mask) const
            {
                if (empty()) {
                    return replaced_type<size_type>();
                }

                assert(header().dims() == mask.header().dims());

                replaced_type<size_type> res({ header().count() });

                indexer_type gen(header());
                typename ArCo::indexer_type mask_gen(mask.header());

                typename replaced_type<size_type>::indexer_type res_gen(res.header());

                size_type res_count{ 0 };

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
                    return replaced_type<size_type>();
                }

                if (res_count < header().count()) {
                    return res.resize({ res_count });
                }

                return res;
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            [[nodiscard]] constexpr this_type transpose(InputIt first_order, InputIt last_order) const
            {
                if (empty()) {
                    return this_type();
                }

                header_type new_header(header().reorder(first_order, last_order));
                if (new_header.empty()) {
                    return this_type();
                }

                this_type res({ header().count() });
                res.header() = std::move(new_header);

                indexer_type gen(header(), first_order, last_order);
                indexer_type res_gen(res.header());

                while (gen && res_gen) {
                    res[*res_gen] = (*this)[*gen];
                    ++gen;
                    ++res_gen;
                }

                return res;
            }
            template <iterable_of_type<size_type> Cont>
            [[nodiscard]] constexpr this_type transpose(const Cont& order) const
            {
                return transpose(std::begin(order), std::end(order));
            }
            [[nodiscard]] constexpr this_type transpose(std::initializer_list<size_type> order) const
            {
                return transpose(order.begin(), order.end());
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

            [[nodiscard]] constexpr replaced_type<bool> all(size_type axis) const
            {
                return reduce([](const value_type& a, const value_type& b) { return a && b; }, axis);
            }

            [[nodiscard]] constexpr bool any() const
            {
                return any_match([](const value_type& value) { return static_cast<bool>(value); });
            }

            [[nodiscard]] constexpr replaced_type<bool> any(size_type axis) const
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

            constexpr auto begin(size_type axis = 0)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, axis));
            }
            constexpr auto end(size_type axis = 0)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, axis, true) + 1);
            }
            constexpr auto cbegin(size_type axis = 0) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, axis));
            }
            constexpr auto cend(size_type axis = 0) const
            {
                return const_iterator(buffsp_->data() , indexer_type(hdr_, axis, true) + 1);
            }
            constexpr auto rbegin(size_type axis = 0)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, true));
            }
            constexpr auto rend(size_type axis = 0)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis) - 1);
            }
            constexpr auto crbegin(size_type axis = 0) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, true));
            }
            constexpr auto crend(size_type axis = 0) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis) - 1);
            }

            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto begin(InputIt first_order, InputIt last_order)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto end(InputIt first_order, InputIt last_order)
            {
                return iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order, true) + 1);
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto cbegin(InputIt first_order, InputIt last_order) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto cend(InputIt first_order, InputIt last_order) const
            {
                return const_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order, true) + 1);
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto rbegin(InputIt first_order, InputIt last_order)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order, true));
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto rend(InputIt first_order, InputIt last_order)
            {
                return reverse_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order) - 1);
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto crbegin(InputIt first_order, InputIt last_order) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order, true));
            }
            template <typename InputIt> requires std::is_same_v<size_type, iterator_value_type<InputIt>>
            constexpr auto crend(InputIt first_order, InputIt last_order) const
            {
                return const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order) - 1);
            }

            constexpr auto begin(std::initializer_list<size_type> order)
            {
                return begin(order.begin(), order.end());
            }
            constexpr auto end(std::initializer_list<size_type> order)
            {
                return end(order.begin(), order.end());
            }
            constexpr auto cbegin(std::initializer_list<size_type> order) const
            {
                return cbegin(order.begin(), order.end());
            }
            constexpr auto cend(std::initializer_list<size_type> order) const
            {
                return cend(order.begin(), order.end());
            }
            constexpr auto rbegin(std::initializer_list<size_type> order)
            {
                return rbegin(order.begin(), order.end());
            }
            constexpr auto rend(std::initializer_list<size_type> order)
            {
                return rend(order.begin(), order.end());
            }
            constexpr auto crbegin(std::initializer_list<size_type> order) const
            {
                return crbegin(order.begin(), order.end());
            }
            constexpr auto crend(std::initializer_list<size_type> order) const
            {
                return crend(order.begin(), order.end());
            }

            template <iterable_of_type<size_type> Cont>
            constexpr auto begin(const Cont& order)
            {
                return begin(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto end(const Cont& order)
            {
                return end(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto cbegin(const Cont& order) const
            {
                return cbegin(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto cend(const Cont& order) const
            {
                return cend(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto rbegin(const Cont& order)
            {
                return rbegin(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto rend(const Cont& order)
            {
                return rend(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto crbegin(const Cont& order) const
            {
                return crbegin(std::begin(order), std::end(order));
            }
            template <iterable_of_type<size_type> Cont>
            constexpr auto crend(const Cont& order) const
            {
                return crend(std::begin(order), std::end(order));
            }

            constexpr auto begin_subarray(size_type fixed_axis = 0)
            {
                return subarray_iterator(*this, ranger_type(hdr_, fixed_axis));
            }
            constexpr auto end_subarray(size_type fixed_axis = 0)
            {
                return subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true) + 1);
            }
            constexpr auto cbegin_subarray(size_type fixed_axis = 0) const
            {
                return const_subarray_iterator(*this, ranger_type(hdr_, fixed_axis));
            }
            constexpr auto cend_subarray(size_type fixed_axis = 0) const
            {
                return const_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true) + 1);
            }
            constexpr auto rbegin_subarray(size_type fixed_axis = 0)
            {
                return reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true));
            }
            constexpr auto rend_subarray(size_type fixed_axis = 0)
            {
                return reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis) - 1);
            }
            constexpr auto crbegin_subarray(size_type fixed_axis = 0) const
            {
                return const_reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true));
            }
            constexpr auto crend_subarray(size_type fixed_axis = 0) const
            {
                return const_reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis) - 1);
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

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename InputIt> requires std::is_same_v<interval<typename ArCo1::size_type>, iterator_value_type<InputIt>>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, InputIt first_range, InputIt last_range)
        {
            return dst.copy_from(src, first_range, last_range);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename InputIt> requires std::is_same_v<interval<typename ArCo1::size_type>, iterator_value_type<InputIt>>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, InputIt first_range, InputIt last_range)
        {
            return dst.copy_from(src, first_range, last_range);
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2, iterable_of_type<interval<typename ArCo1::size_type>> Cont>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, const Cont& ranges)
        {
            return copy(src, dst, std::begin(ranges), std::end(ranges));
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2, iterable_of_type<interval<typename ArCo1::size_type>> Cont>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const Cont& ranges)
        {
            return copy(src, dst, std::begin(ranges), std::end(ranges));
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2& dst, std::initializer_list<interval<typename ArCo1::size_type>> ranges)
        {
            return copy(src, dst, ranges.begin(), ranges.end());
        }
        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::initializer_list<interval<typename ArCo1::size_type>> ranges)
        {
            return copy(src, dst, ranges.begin(), ranges.end());
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
        template <arrnd_complient ArCo, typename InputIt> requires std::is_same_v<typename ArCo::size_type, iterator_value_type<InputIt>>
        [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, InputIt first_new_dim, InputIt last_new_dim)
        {
            return arr.reshape(first_new_dim, last_new_dim);
        }
        template <arrnd_complient ArCo, iterable_of_type<typename ArCo::size_type> Cont>
        [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const Cont& new_dims)
        {
            return reshape(arr, std::begin(new_dims), std::end(new_dims));
        }
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
        {
            return reshape(arr, new_dims.begin(), new_dims.end());
        }

        template <arrnd_complient ArCo, typename InputIt> requires std::is_same_v<typename ArCo::size_type, iterator_value_type<InputIt>>
        [[nodiscard]] inline constexpr auto resize(const ArCo& arr, InputIt first_new_dim, InputIt last_new_dim)
        {
            return arr.resize(first_new_dim, last_new_dim);
        }
        template <arrnd_complient ArCo, iterable_of_type<typename ArCo::size_type> Cont>
        [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const Cont& new_dims)
        {
            return resize(arr, std::begin(new_dims), std::end(new_dims));
        }
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto resize(const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
        {
            return resize(arr, new_dims.begin(), new_dims.end());
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
        {
            return lhs.append(rhs);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type axis)
        {
            return lhs.append(rhs, axis);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind)
        {
            return lhs.insert(rhs, ind);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2>
        [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind, typename ArCo1::size_type axis)
        {
            return lhs.insert(rhs, ind, axis);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto remove(const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count)
        {
            return arr.remove(ind, count);
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto remove(const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count, typename ArCo::size_type axis)
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
        [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, Binary_op&& op, typename ArCo::size_type axis)
        {
            return arr.reduce(op, axis);
        }

        template <arrnd_complient ArCo1, arrnd_complient ArCo2, typename Binary_op> requires std::is_invocable_v<Binary_op, typename ArCo2::value_type, typename ArCo1::value_type>
        [[nodiscard]] inline constexpr auto reduce(const ArCo1& arr, const ArCo2& init_values, Binary_op&& op, typename ArCo1::size_type axis)
        {
            return arr.reduce(init_values, op, axis);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool all(const ArCo& arr)
        {
            return arr.all();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto all(const ArCo& arr, typename ArCo::size_type axis)
        {
            return arr.all(axis);
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr bool any(const ArCo& arr)
        {
            return arr.any();
        }

        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto any(const ArCo& arr, typename ArCo::size_type axis)
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

        template <arrnd_complient ArCo, typename InputIt> requires std::is_same_v<typename ArCo::size_type, iterator_value_type<InputIt>>
        [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, InputIt first_order, InputIt last_order)
        {
            return arr.transpose(first_order, last_order);
        }
        template <arrnd_complient ArCo, iterable_of_type<typename ArCo::size_type> Cont>
        [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const Cont& order)
        {
            return transpose(arr, std::begin(order), std::end(order));
        }
        template <arrnd_complient ArCo>
        [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, std::initializer_list<typename ArCo::size_type> order)
        {
            return transpose(arr, order.begin(), order.end());
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
        std::ostream& ostream_operator_recursive(std::ostream& os, const ArCo& arco, typename ArCo::size_type nvectical_spaces)
        {
            if (empty(arco)) {
                os << "[]";
                return os;
            }

            if (std::ssize(arco.header().dims()) > 1) {
                os << '[';
                for (typename ArCo::size_type i = 0; i < arco.header().dims()[0]; ++i) {
                    if (i > 0) {
                        for (typename ArCo::size_type i = 0; i < nvectical_spaces - (std::ssize(arco.header().dims()) - 1) + 1; ++i) {
                            os << ' ';
                        }
                    }
                    ostream_operator_recursive(os, arco[interval<typename ArCo::size_type>{i, i}], nvectical_spaces);
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
            typename ArCo::size_type nvectical_spaces = std::ssize(arco.header().dims()) - 1;
            return ostream_operator_recursive(os, arco, nvectical_spaces);
        }
    }

    using details::arrnd_complient;

    using details::arrnd_back_inserter;
    using details::arrnd_front_inserter;
    using details::arrnd_inserter;

    using details::arrnd_axis_back_inserter;
    using details::arrnd_axis_front_inserter;
    using details::arrnd_axis_inserter;

    using details::arrnd;

    using details::arrnd_header;
    
    using details::arrnd_general_indexer;
    using details::arrnd_fast_indexer;

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
