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
#include <variant>
#include <sstream>

namespace computoc {
    namespace details {
        inline std::string make_error_msg(const char* failed_cond, const char* exception_type, int line, const char* func, const char* file, const std::string& desc = std::string{})
        {
            std::stringstream ss;
            ss << exception_type << " exception (at line " << line << ", " << func << "@" << file << "), assertion " << failed_cond << " failed";
            if (!desc.empty()) {
                ss << ": " << desc;
            }
            return ss.str();
        }
    }
}

#ifdef __unix__
#define _REQUIRE(condition, exception_type, ...) \
    if(!(condition)) { \
        std::string msg = computoc::details::make_error_msg(#condition, #exception_type, __LINE__, __FUNCTION__, __FILE__ __VA_OPT__(, __VA_ARGS__)); \
        throw exception_type(msg); \
    }
#elif defined(_WIN32) || defined(_WIN64)
#define _REQUIRE(condition, exception_type, ...) \
    if(!(condition)) { \
        std::string msg = computoc::details::make_error_msg(#condition, #exception_type, __LINE__, __FUNCTION__, __FILE__, __VA_ARGS__); \
        throw exception_type(msg); \
    }
#endif

namespace computoc {

    namespace details {
        using None_option = std::monostate;

        template <typename T = None_option>
        class Unexpected {
        public:
            constexpr Unexpected(const T& value = None_option{})
                : value_(value)
            {
            }
            constexpr Unexpected(T&& value) noexcept
                : value_(std::move(value))
            {
            }
            constexpr Unexpected(const Unexpected&) = default;
            constexpr Unexpected& operator=(const Unexpected&) = default;
            constexpr Unexpected(Unexpected&&) = default;
            constexpr Unexpected& operator=(Unexpected&&) = default;
            ~Unexpected() = default;

            [[nodiscard]] constexpr const T& value() const noexcept
            {
                return value_;
            }

        private:
            T value_;
        };

        template <typename T, typename E = None_option>
        //requires (!std::is_same_v<None_option, T>)
        class Expected {
        public:
            template <typename U = T>
            constexpr Expected(const U& value)
                : opts_(value)
            {
            }
            template <typename U = T>
            constexpr Expected(U&& value) noexcept
                : opts_(std::move(value))
            {
            }

            template <typename U = E>
            constexpr Expected(const Unexpected<U>& error)
                : opts_(error)
            {
            }
            template <typename U = E>
            constexpr Expected(Unexpected<U>&& error) noexcept
                : opts_(std::move(error))
            {
            }

            constexpr Expected(const Expected& other) = default;
            constexpr Expected& operator=(const Expected& other) = default;

            constexpr Expected(Expected&& other) = default;
            constexpr Expected& operator=(Expected&& other) = default;

            constexpr ~Expected() = default;

            [[nodiscard]] explicit constexpr operator bool() const noexcept
            {
                return opts_.index() == 0;
            }

            [[nodiscard]] constexpr bool has_value() const noexcept
            {
                return opts_.index() == 0;
            }

            [[nodiscard]] constexpr const T& value() const
            {
                if (opts_.index() == 1) {
                    throw std::runtime_error("value is not present");
                }
                return std::get<T>(opts_);
            }

            [[nodiscard]] constexpr const T& operator*() const
            {
                if (opts_.index() == 1) {
                    throw std::runtime_error("value is not present");
                }
                return std::get<T>(opts_);
            }

            [[nodiscard]] constexpr const T* operator->() const
            {
                if (opts_.index() == 1) {
                    throw std::runtime_error("value is not present");
                }
                return &std::get<T>(opts_);
            }

            [[nodiscard]] constexpr const E& error() const
            {
                if (opts_.index() == 0) {
                    throw std::runtime_error("error is not present");
                }
                return std::get<Unexpected<E>>(opts_).value();
            }

            template <typename U>
            [[nodiscard]] constexpr T value_or(const U& other) const
            {
                return opts_.index() == 0 ? std::get<T>(opts_) : other;
            }
            template <typename U>
            [[nodiscard]] constexpr T value_or(U&& other) const
            {
                return opts_.index() == 0 ? std::get<T>(opts_) : std::move(other);
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto and_then(Unary_op&& op) const
            {
                if (opts_.index() == 0) {
                    return op(value());
                }
                return decltype(op(value()))(Unexpected(error()));
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto and_then(Unary_op&& op) const requires std::is_void_v<decltype(op(value()))>
            {
                if (opts_.index() == 0) {
                    op(value());
                }
                return *this;
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto and_then(Unary_op&& op) const requires std::is_invocable_v<Unary_op>
            {
                if (opts_.index() == 0) {
                    return op();
                }
                return decltype(op())(Unexpected(error()));
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto or_else(Unary_op&& op) const
            {
                if (opts_.index() == 0) {
                    return decltype(op(error()))(value());
                }
                return op(error());
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto or_else(Unary_op&& op) const requires std::is_void_v<decltype(op(error()))>
            {
                if (opts_.index() == 0) {
                    return *this;
                }
                op(error());
                return *this;
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto or_else(Unary_op&& op) const requires std::is_invocable_v<Unary_op>
            {
                if (opts_.index() == 0) {
                    return decltype(op())(value());
                }
                return op();
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto transform(Unary_op&& op) const
            {
                if (opts_.index() == 0) {
                    return Expected<decltype(op(value())), E>(op(value()));
                }
                return Expected<decltype(op(value())), E>(error());
            }

            template <typename Unary_op>
            [[nodiscard]] constexpr auto transform_error(Unary_op&& op) const
            {
                if (opts_.index() == 0) {
                    return Expected<T, decltype(op(error()))>(value());
                }
                return Expected<T, decltype(op(error()))>(op(error()));
            }

        private:
            constexpr Expected() noexcept
                : Expected(None_option{})
            {
            }

            std::variant<T, Unexpected<E>> opts_;
        };

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator==(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return false;
            }

            return lhs ? (lhs.value() == rhs.value()) : (lhs.error() == rhs.error());
        }

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator!=(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return true;
            }

            return lhs ? (lhs.value() != rhs.value()) : (lhs.error() != rhs.error());
        }

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator<(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return false;
            }

            return lhs ? (lhs.value() < rhs.value()) : (lhs.error() < rhs.error());
        }

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator<=(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return false;
            }

            return lhs ? (lhs.value() <= rhs.value()) : (lhs.error() <= rhs.error());
        }

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator>(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return false;
            }

            return lhs ? (lhs.value() > rhs.value()) : (lhs.error() > rhs.error());
        }

        template <typename T1, typename E1, typename T2, typename E2>
        [[nodiscard]] inline constexpr bool operator>=(const Expected<T1, E1>& lhs, const Expected<T2, E2>& rhs)
        {
            if (static_cast<bool>(lhs) != static_cast<bool>(rhs)) {
                return false;
            }

            return lhs ? (lhs.value() >= rhs.value()) : (lhs.error() >= rhs.error());
        }
    }

    using details::Unexpected;
    using details::Expected;
    using details::None_option;


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
            /*decltype((value % modulus) + modulus) res{ value };
            while (res < 0) {
                res += modulus;
            }
            while (res >= modulus) {
                res -= modulus;
            }
            return res;*/
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

        template <typename T>
        requires (!std::is_reference_v<T>)
        class Lightweight_stl_allocator {
        public:
            using value_type = T;

            constexpr Lightweight_stl_allocator() = default;
            constexpr Lightweight_stl_allocator(const Lightweight_stl_allocator& other) = default;
            constexpr Lightweight_stl_allocator& operator=(const Lightweight_stl_allocator& other) = default;
            constexpr Lightweight_stl_allocator(Lightweight_stl_allocator&& other) = default;
            constexpr Lightweight_stl_allocator& operator=(Lightweight_stl_allocator&& other) = default;
            constexpr ~Lightweight_stl_allocator() = default;

            template <typename U>
            requires (!std::is_reference_v<U>)
                constexpr Lightweight_stl_allocator(const Lightweight_stl_allocator<U>&) noexcept {}

            [[nodiscard]] constexpr T* allocate(std::size_t n)
            {
                return n == 0 ? nullptr : reinterpret_cast<T*>(operator new[](n * sizeof(T)));
            }

            constexpr void deallocate(T* p, std::size_t n) noexcept
            {
                if (p && n > 0) {
                    operator delete[](p, n * sizeof(T));
                }
            }
        };

        template <typename T, template<typename> typename Allocator = Lightweight_stl_allocator>
        requires (std::is_copy_constructible_v<T>&& std::is_copy_assignable_v<T>)
            class simple_dynamic_vector final {
            public:
                using value_type = T;
                using size_type = std::int64_t;
                using reference = T&;
                using const_reference = const T&;
                using pointer = T*;
                using const_pointer = const T*;

                using capacity_func_type = std::function<size_type(size_type)>;

                constexpr simple_dynamic_vector(size_type size = 0, const_pointer data = nullptr, capacity_func_type capacity_func = [](size_type s) { return static_cast<size_type>(1.5 * s); })
                    : size_(size), capacity_(size), capacity_func_(capacity_func)
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
                    : alloc_(other.alloc_), size_(other.size_), capacity_(other.capacity_), capacity_func_(other.capacity_func_)
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
                    capacity_func_ = other.capacity_func_;

                    data_ptr_ = alloc_.allocate(capacity_);
                    std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);

                    return *this;
                }

                constexpr simple_dynamic_vector(simple_dynamic_vector&& other) noexcept
                    : alloc_(std::move(other.alloc_)), size_(other.size_), capacity_(other.capacity_), capacity_func_(std::move(other.capacity_func_))
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
                    capacity_func_ = std::move(other.capacity_func_);

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
                        size_type new_capacity = capacity_func_(size_ + count);
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

                [[nodiscard]] constexpr const T& back() const noexcept
                {
                    return data_ptr_[size_-1];
                }

                [[nodiscard]] constexpr T& back() noexcept
                {
                    return data_ptr_[size_-1];
                }

                [[nodiscard]] constexpr const T& front() const noexcept
                {
                    return data_ptr_[0];
                }

                [[nodiscard]] constexpr T& front() noexcept
                {
                    return data_ptr_[0];
                }

            private:
                pointer data_ptr_;

                size_type size_;
                size_type capacity_;

                Allocator<T> alloc_;

                capacity_func_type capacity_func_;
        };



        template <typename T, std::int64_t Capacity>
        requires (std::is_copy_constructible_v<T>&& std::is_copy_assignable_v<T>)
            class simple_static_vector final {
            public:
                using value_type = T;
                using size_type = std::int64_t;
                using reference = T&;
                using const_reference = const T&;
                using pointer = T*;
                using const_pointer = const T*;

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

                [[nodiscard]] constexpr pointer begin() noexcept
                {
                    return data_ptr_;
                }

                [[nodiscard]] constexpr pointer end() noexcept
                {
                    return data_ptr_ + size_;
                }

                [[nodiscard]] constexpr const T& back() const noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr T& back() noexcept
                {
                    return data_ptr_[size_ - 1];
                }

                [[nodiscard]] constexpr const T& front() const noexcept
                {
                    return data_ptr_[0];
                }

                [[nodiscard]] constexpr T& front() noexcept
                {
                    return data_ptr_[0];
                }

            private:
                value_type data_ptr_[Capacity];

                size_type size_;
        };

        //inline constexpr std::uint32_t dynamic_vector = std::numeric_limits<std::uint32_t>::max();

        //template <typename T, std::int64_t Capacity = dynamic_vector, template<typename> typename Allocator = Lightweight_stl_allocator>
        //using simple_vector = std::conditional_t<Capacity == dynamic_vector, simple_dynamic_vector<T, Allocator>, simple_static_vector<T, Capacity>>;




        inline constexpr std::uint32_t dynamic_sequence = std::numeric_limits<std::uint32_t>::max();

        template <typename T, std::int64_t N = dynamic_sequence, template<typename> typename Allocator = Lightweight_stl_allocator>
        requires (N > 0)
        using simple_vector = std::conditional_t<N == dynamic_sequence, simple_dynamic_vector<T, Allocator>, simple_static_vector<T, N>>;

        //template <typename T, template<typename> typename Allocator = Lightweight_stl_allocator>
        //using simple_vector = simple_dynamic_vector<T, Allocator>;//std::vector<T, Allocator<T>>;

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

        template <std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Array_header {
        public:
            Array_header() = default;

            Array_header(std::span<const std::int64_t> dims)
            {
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(dims.begin(), dims.end());

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(dims.size());
                compute_strides(dims, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(const Array_header<Dims_capacity, Internal_allocator>& previous_hdr, std::span<const Interval<std::int64_t>> intervals)
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());

                if (compute_dims(previous_hdr.dims(), intervals, dims) <= 0) {
                    return;
                }

                dims_ = std::move(dims);
                
                count_ = numel(dims_);

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());
                compute_strides(previous_hdr.dims(), previous_hdr.strides(), intervals, strides_);

                offset_ = compute_offset(previous_hdr.dims(), previous_hdr.offset(), previous_hdr.strides(), intervals);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });

                is_subarray_ = previous_hdr.is_subarray() || !std::equal(previous_hdr.dims().begin(), previous_hdr.dims().end(), dims_.begin());
            }

            Array_header(const Array_header<Dims_capacity, Internal_allocator>& previous_hdr, std::int64_t omitted_axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                std::int64_t axis{ modulo(omitted_axis, std::ssize(previous_hdr.dims())) };
                std::int64_t ndims{ std::ssize(previous_hdr.dims()) > 1 ? std::ssize(previous_hdr.dims()) - 1 : 1 };

                dims_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(ndims);

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

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(ndims);
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(const Array_header<Dims_capacity, Internal_allocator>& previous_hdr, std::span<const std::int64_t> new_order)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                if (new_order.empty()) {
                    return;
                }

                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());

                for (std::int64_t i = 0; i < std::ssize(previous_hdr.dims()); ++i) {
                    dims[i] = previous_hdr.dims()[modulo(new_order[i], std::ssize(previous_hdr.dims()))];
                }

                if (numel(previous_hdr.dims()) != numel(dims)) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                count_ = numel(dims_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(const Array_header<Dims_capacity, Internal_allocator>& previous_hdr, std::int64_t count, std::int64_t axis)
                : is_subarray_(previous_hdr.is_subarray())
            {
                if (numel(previous_hdr.dims()) <= 0) {
                    return;
                }

                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());

                std::int64_t fixed_axis{ modulo(axis, std::ssize(previous_hdr.dims())) };
                for (std::int64_t i = 0; i < previous_hdr.dims().size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + count;
                }
                
                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(const Array_header<Dims_capacity, Internal_allocator>& previous_hdr, std::span<const std::int64_t> appended_dims, std::int64_t axis)
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

                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());

                for (std::int64_t i = 0; i < previous_hdr.dims().size(); ++i) {
                    dims[i] = (i != fixed_axis) ? previous_hdr.dims()[i] : previous_hdr.dims()[i] + appended_dims[fixed_axis];
                }

                if ((count_ = numel(dims)) <= 0) {
                    return;
                }

                dims_ = std::move(dims);

                strides_ = simple_vector<std::int64_t, Dims_capacity, Internal_allocator>(previous_hdr.dims().size());
                compute_strides(dims_, strides_);

                last_index_ = offset_ + std::inner_product(dims_.begin(), dims_.end(), strides_.begin(), 0,
                    [](auto a, auto b) { return a + b; },
                    [](auto a, auto b) { return (a - 1) * b; });
            }

            Array_header(Array_header&& other) = default;
            Array_header& operator=(Array_header&& other) = default;

            Array_header(const Array_header& other) = default;
            Array_header& operator=(const Array_header& other) = default;

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
            simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims_{};
            simple_vector<std::int64_t, Dims_capacity, Internal_allocator> strides_{};
            std::int64_t count_{ 0 };
            std::int64_t offset_{ 0 };
            std::int64_t last_index_{ 0 };
            bool is_subarray_{ false };
        };


        template <std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Simple_array_indices_generator final
        {
        public:
            constexpr Simple_array_indices_generator(const Array_header<Dims_capacity, Internal_allocator>& hdr, bool backward = false)
                : Simple_array_indices_generator(hdr, std::span<const std::int64_t>{}, backward)
            {
            }

            constexpr Simple_array_indices_generator(const Array_header<Dims_capacity, Internal_allocator>& hdr, std::int64_t axis, bool backward = false)
                : Simple_array_indices_generator(hdr, order_from_major_axis(hdr.dims().size(), axis), backward)
            {
            }

            constexpr Simple_array_indices_generator(const Array_header<Dims_capacity, Internal_allocator>& hdr, std::span<const std::int64_t> order, bool backward = false)
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

            constexpr Simple_array_indices_generator() = default;

            constexpr Simple_array_indices_generator(const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& other) = default;
            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator=(const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& other) = default;

            constexpr Simple_array_indices_generator(Simple_array_indices_generator<Dims_capacity, Internal_allocator>&& other) noexcept = default;
            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator=(Simple_array_indices_generator<Dims_capacity, Internal_allocator>&& other) noexcept = default;

            constexpr ~Simple_array_indices_generator() = default;

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator++() noexcept
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

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Simple_array_indices_generator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            Simple_array_indices_generator<Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Simple_array_indices_generator<Dims_capacity, Internal_allocator> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator--() noexcept
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

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Simple_array_indices_generator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr Simple_array_indices_generator<Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Simple_array_indices_generator<Dims_capacity, Internal_allocator> temp{ *this };
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
            constexpr static simple_vector<std::int64_t, Dims_capacity, Internal_allocator> order_from_major_axis(std::int64_t order_size, std::int64_t axis)
            {
                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> new_ordered_indices(order_size);
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

            constexpr static simple_vector<std::int64_t, Dims_capacity, Internal_allocator> reorder(std::span<const std::int64_t> vec, std::span<const std::int64_t> indices)
            {
                std::size_t size = std::min(vec.size(), indices.size());
                simple_vector<std::int64_t, Dims_capacity, Internal_allocator> res(size);
                for (std::int64_t i = 0; i < size; ++i) {
                    res[i] = vec[indices[i]];
                }
                return res;
            }

            constexpr static std::tuple<
                simple_vector<std::int64_t, Dims_capacity, Internal_allocator>, simple_vector<std::int64_t, Dims_capacity, Internal_allocator>>
                reduce_dimensions(std::span<const std::int64_t> dims, std::span<const std::int64_t> strides)
            {
                std::tuple<
                    simple_vector<std::int64_t, Dims_capacity, Internal_allocator>,
                    simple_vector<std::int64_t, Dims_capacity, Internal_allocator>> reds(dims.size(), dims.size());

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

            simple_vector<std::int64_t, Dims_capacity, Internal_allocator> dims_;
            simple_vector<std::int64_t, Dims_capacity, Internal_allocator> strides_;
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

            simple_vector<std::int64_t, Dims_capacity, Internal_allocator> indices_;
            std::int64_t current_index_;
        };




        template <std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Fast_array_indices_generator final
        {
        public:
            constexpr Fast_array_indices_generator(const Array_header<Dims_capacity, Internal_allocator>& hdr, bool backward = false)
                : Fast_array_indices_generator(hdr, 0, backward)
            {
            }

            constexpr Fast_array_indices_generator(const Array_header<Dims_capacity, Internal_allocator>& hdr, std::int64_t axis, bool backward = false)
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

            constexpr Fast_array_indices_generator() = default;

            constexpr Fast_array_indices_generator(const Fast_array_indices_generator<Dims_capacity, Internal_allocator>& other) = default;
            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator=(const Fast_array_indices_generator<Dims_capacity, Internal_allocator>& other) = default;

            constexpr Fast_array_indices_generator(Fast_array_indices_generator<Dims_capacity, Internal_allocator>&& other) noexcept = default;
            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator=(Fast_array_indices_generator<Dims_capacity, Internal_allocator>&& other) noexcept = default;

            constexpr ~Fast_array_indices_generator() = default;

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator++() noexcept
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

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Fast_array_indices_generator temp{ *this };
                ++(*this);
                return temp;
            }

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    ++(*this);
                }
                return *this;
            }

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Fast_array_indices_generator<Dims_capacity, Internal_allocator> temp{ *this };
                temp += count;
                return temp;
            }

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator--() noexcept
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

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Fast_array_indices_generator temp{ *this };
                --(*this);
                return temp;
            }

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                for (std::int64_t i = 0; i < count; ++i) {
                    --(*this);
                }
                return *this;
            }

            constexpr Fast_array_indices_generator<Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Fast_array_indices_generator<Dims_capacity, Internal_allocator> temp{ *this };
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


        template <typename T, std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Array_iterator final
        {
        public:
            using value_type = T;
            using difference_type = std::ptrdiff_t;

            Array_iterator(T* data, const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_iterator() = default;

            Array_iterator(const Array_iterator<T, Dims_capacity, Internal_allocator>& other) = default;
            Array_iterator<T, Dims_capacity, Internal_allocator>& operator=(const Array_iterator<T, Dims_capacity, Internal_allocator>& other) = default;

            Array_iterator(Array_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;
            Array_iterator<T, Dims_capacity, Internal_allocator>& operator=(Array_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;

            ~Array_iterator() = default;

            Array_iterator<T, Dims_capacity, Internal_allocator>& operator++() noexcept
            {
                ++gen_;
                return *this;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Array_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                gen_ += count;
                return *this;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Array_iterator temp{ *this };
                temp += count;
                return temp;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator>& operator--() noexcept
            {
                gen_--;
                return *this;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Array_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            Array_iterator<T, Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Array_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] T& operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] bool operator==(const Array_iterator<T, Dims_capacity, Internal_allocator>& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            Simple_array_indices_generator<Dims_capacity, Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Array_const_iterator final
        {
        public:
            Array_const_iterator(T* data, const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_const_iterator() = default;

            Array_const_iterator(const Array_const_iterator<T, Dims_capacity, Internal_allocator>& other) = default;
            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator=(const Array_const_iterator<T, Dims_capacity, Internal_allocator>& other) = default;

            Array_const_iterator(Array_const_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;
            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator=(Array_const_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;

            ~Array_const_iterator() = default;

            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator++() noexcept
            {
                ++gen_;
                return *this;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Array_const_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                gen_ += count;
                return *this;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Array_const_iterator temp{ *this };
                temp += count;
                return temp;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator--() noexcept
            {
                --gen_;
                return *this;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Array_const_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            Array_const_iterator<T, Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Array_const_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] const T& operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] bool operator==(const Array_const_iterator<T, Dims_capacity, Internal_allocator>& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            Simple_array_indices_generator<Dims_capacity, Internal_allocator> gen_;
            T* data_ = nullptr;
        };



        template <typename T, std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Array_reverse_iterator final
        {
        public:
            using value_type = T;
            using difference_type = std::ptrdiff_t;

            Array_reverse_iterator(T* data, const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_reverse_iterator() = default;

            Array_reverse_iterator(const Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& other) = default;
            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator=(const Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& other) = default;

            Array_reverse_iterator(Array_reverse_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;
            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator=(Array_reverse_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;

            ~Array_reverse_iterator() = default;

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator++() noexcept
            {
                --gen_;
                return *this;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Array_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Array_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator--() noexcept
            {
                ++gen_;
                return *this;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Array_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                gen_ += count;
                return *this;
            }

            Array_reverse_iterator<T, Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Array_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] T& operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] bool operator==(const Array_reverse_iterator<T, Dims_capacity, Internal_allocator>& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            Simple_array_indices_generator<Dims_capacity, Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Internal_allocator = Lightweight_stl_allocator>
        class Array_const_reverse_iterator final
        {
        public:
            Array_const_reverse_iterator(T* data, const Simple_array_indices_generator<Dims_capacity, Internal_allocator>& gen)
                : gen_(gen), data_(data)
            {
            }

            Array_const_reverse_iterator() = default;

            Array_const_reverse_iterator(const Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& other) = default;
            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator=(const Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& other) = default;

            Array_const_reverse_iterator(Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;
            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator=(Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>&& other) noexcept = default;

            ~Array_const_reverse_iterator() = default;

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator++() noexcept
            {
                --gen_;
                return *this;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator> operator++(int) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                --(*this);
                return temp;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator+=(std::int64_t count) noexcept
            {
                gen_ -= count;
                return *this;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator> operator+(std::int64_t count) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                temp += count;
                return temp;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator--() noexcept
            {
                ++gen_;
                return *this;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator> operator--(int) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                ++(*this);
                return temp;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& operator-=(std::int64_t count) noexcept
            {
                gen_ += count;
                return *this;
            }

            Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator> operator-(std::int64_t count) noexcept
            {
                Array_const_reverse_iterator temp{ *this };
                temp -= count;
                return temp;
            }

            [[nodiscard]] const T& operator*() const noexcept
            {
                return data_[*gen_];
            }

            [[nodiscard]] bool operator==(const Array_const_reverse_iterator<T, Dims_capacity, Internal_allocator>& iter) const
            {
                return *gen_ == *(iter.gen_);
            }

        private:
            Simple_array_indices_generator<Dims_capacity, Internal_allocator> gen_;
            T* data_ = nullptr;
        };




        template <typename T, std::int64_t Data_capacity = dynamic_sequence, std::int64_t Dims_capacity = dynamic_sequence, template<typename> typename Data_allocator = Lightweight_stl_allocator, template<typename> typename Internals_allocator = Lightweight_stl_allocator>
        class Array {
        public:
            using Header = Array_header<Dims_capacity, Internals_allocator>;

            Array() = default;

            Array(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& other) = default;
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array(Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>&& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);

                Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
            }
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& other) & = default;
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy(other, *this);
                Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>&& other)&
            {
                *this = Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
                return *this;
            }
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>&& other)&&
            {
                copy(other, *this);
                Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o> dummy{ std::move(other) };
                return *this;
            }

            Array(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& other) = default;
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array(const Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>& other)
                : Array(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()))
            {
                copy(other, *this);
            }
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& other) & = default;
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& other)&&
            {
                if (&other == this) {
                    return *this;
                }

                copy(other, *this);
                return *this;
            }
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(const Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>& other)&
            {
                *this = Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(other.header().dims().data(), other.header().dims().size()));
                copy(other, *this);
                return *this;
            }
            template< typename T_o, std::int64_t Data_capacity_o, std::int64_t Dims_capacity_o, template<typename> typename Data_allocator_o, template<typename> typename Internals_allocator_o>
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(const Array<T_o, Data_capacity_o, Dims_capacity_o, Data_allocator_o, Internals_allocator_o>& other)&&
            {
                copy(other, *this);
                return *this;
            }

            template <typename U>
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& operator=(const U& value)
            {
                if (empty(*this)) {
                    return *this;
                }

                for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(hdr_); gen; ++gen) {
                    (*this)(*gen) = value;
                }

                return *this;
            }

            virtual ~Array() = default;

            Array(std::span<const std::int64_t> dims, const T* data = nullptr)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_capacity, Data_allocator>>(Internals_allocator<simple_vector<T, Data_capacity, Data_allocator>>(), hdr_.count()))
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
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_capacity, Data_allocator>>(Internals_allocator < simple_vector<T, Data_capacity, Data_allocator>>(), hdr_.count()))
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
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_capacity, Data_allocator>>(Internals_allocator < simple_vector<T, Data_capacity, Data_allocator>>(), hdr_.count()))
            {
                std::fill(buffsp_->data(), buffsp_->data() + buffsp_->size(), value);
            }
            Array(std::initializer_list<std::int64_t> dims, const T& value)
                : Array(std::span<const std::int64_t>{dims.begin(), dims.size()}, value)
            {
            }
            template <typename U>
            Array(std::span<const std::int64_t> dims, const U& value)
                : hdr_(dims), buffsp_(std::allocate_shared<simple_vector<T, Data_capacity, Data_allocator>>(Internals_allocator < simple_vector<T, Data_capacity, Data_allocator>>(), hdr_.count()))
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

            [[nodiscard]] Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator()(std::span<const Interval<std::int64_t>> ranges) const
            {
                if (ranges.empty() || empty(*this)) {
                    return (*this);
                }

                Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> slice{};
                slice.hdr_ = Header{ hdr_, ranges };
                slice.buffsp_ = slice.hdr_.empty() ? nullptr : buffsp_;
                return slice;
            }
            [[nodiscard]] Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator()(std::initializer_list<Interval<std::int64_t>> ranges) const
            {
                return (*this)(std::span<const Interval<std::int64_t>>{ranges.begin(), ranges.size()});
            }

            [[nodiscard]] Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator()(const Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& indices) const noexcept
            {
                Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(indices.header().dims().data(), indices.header().dims().size()));

                for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(indices.header()); gen; ++gen) {
                    res(*gen) = buffsp_->data()[indices(*gen)];
                }

                return res;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& transform(const Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& other, Binary_op&& op)
            {
                if (!std::equal(header().dims().begin(), header().dims().end(), other.header().dims().begin(), other.header().dims().end())) {
                    return *this;
                }

                for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(header()); gen; ++gen) {
                    (*this)(*gen) = op((*this)(*gen), other(*gen));
                }

                return *this;
            }

            template <typename T_o, typename Binary_op>
            [[nodiscard]] Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& transform(const T_o& other, Binary_op&& op)
            {
                for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(header()); gen; ++gen) {
                    (*this)(*gen) = op((*this)(*gen), other);
                }

                return *this;
            }

            auto begin(std::int64_t axis = 0)
            {
                return Array_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis));
            }

            auto end(std::int64_t axis = 0)
            {
                return Array_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis, true) + 1);
            }


            auto cbegin(std::int64_t axis = 0) const
            {
                return Array_const_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis));
            }

            auto cend(std::int64_t axis = 0) const
            {
                return Array_const_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data() , Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis, true) + 1);
            }


            auto rbegin(std::int64_t axis = 0)
            {
                return Array_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis, true));
            }

            auto rend(std::int64_t axis = 0)
            {
                return Array_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis) - 1);
            }

            auto crbegin(std::int64_t axis = 0) const
            {
                return Array_const_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis, true));
            }

            auto crend(std::int64_t axis = 0) const
            {
                return Array_const_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, axis) - 1);
            }


            auto begin(std::span<const std::int64_t> order)
            {
                return Array_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order));
            }

            auto end(std::span<const std::int64_t> order)
            {
                return Array_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order, true) + 1);
            }


            auto cbegin(std::span<const std::int64_t> order) const
            {
                return Array_const_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order));
            }

            auto cend(std::span<const std::int64_t> order) const
            {
                return Array_const_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order, true) + 1);
            }


            auto rbegin(std::span<const std::int64_t> order)
            {
                return Array_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order, true));
            }

            auto rend(std::span<const std::int64_t> order)
            {
                return Array_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order) - 1);
            }

            auto crbegin(std::span<const std::int64_t> order) const
            {
                return Array_const_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order, true));
            }

            auto crend(std::span<const std::int64_t> order) const
            {
                return Array_const_reverse_iterator<T, Dims_capacity, Internals_allocator>(buffsp_->data(), Simple_array_indices_generator<Dims_capacity, Internals_allocator>(hdr_, order) - 1);
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


        private:
            Header hdr_{};
            std::shared_ptr<simple_vector<T, Data_capacity, Data_allocator>> buffsp_{ nullptr };
        };

        /**
        * @note Copy is being performed even if dimensions are not match either partialy or by indices modulus.
        */
        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline void copy(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& src, Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& dst)
        {
            if (empty(src) || empty(dst)) {
                return;
            }

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> src_gen(src.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> dst_gen(dst.header());

            for (; src_gen && dst_gen; ++src_gen, ++dst_gen) {
                dst(*dst_gen) = src(*src_gen);
            }
        }
        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline void copy(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& src, Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& dst)
        {
            copy(src, dst);
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> clone(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> clone(std::span<const std::int64_t>(arr.header().dims().data(), arr.header().dims().size()));

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(arr.header()); gen; ++gen) {
                clone(*gen) = arr(*gen);
            }

            return clone;
        }

        /**
        * @note Returning a reference to the input array, except in case of resulted empty array or an input subarray.
        */
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> reshape(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (arr.header().dims() == new_dims) {
                return arr;
            }

            if (arr.header().count() != numel(new_dims)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (arr.header().is_subarray()) {
                Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

                Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
                Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

                while (arr_gen && res_gen) {
                    res(*res_gen) = arr(*arr_gen);
                    ++arr_gen;
                    ++res_gen;
                }

                return res;
            }

            typename Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(new_dims);
            if (new_header.empty()) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(arr);
            res.header() = std::move(new_header);

            return res;
        }
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> reshape(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return reshape(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> resize(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> new_dims)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));
            }

            if (arr.header().dims() == new_dims) {
                return clone(arr);
            }

            if (numel(new_dims) <= 0) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(new_dims.data(), new_dims.size()));

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            while (arr_gen && res_gen) {
                res(*res_gen) = arr(*arr_gen);
                ++arr_gen;
                ++res_gen;
            }

            return res;
        }
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> resize(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> new_dims)
        {
            return resize(arr, std::span<const std::int64_t>(new_dims.begin(), new_dims.size()));
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> append(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(resize(lhs, { lhs.header().count() + rhs.header().count() }));

            Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

            for (std::int64_t i = lhs.header().count(); i < res.header().count(); ++i) {
                res({ i }) = rhs({ i - lhs.header().count() });
            }

            return res;
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> append(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(lhs.header(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>{};
            }

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            std::int64_t fixed_axis{ modulo(axis, std::ssize(lhs.header().dims())) };

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> lhs_gen(lhs.header(), fixed_axis);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> rhs_gen(rhs.header(), fixed_axis);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header(), fixed_axis);

            for (; lhs_gen && res_gen; ++lhs_gen, ++res_gen) {
                res.data()[*res_gen] = lhs.data()[*lhs_gen];
            }
            for (; rhs_gen && res_gen; ++rhs_gen, ++res_gen) {
                res.data()[*res_gen] = rhs.data()[*rhs_gen];
            }

            return res;
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> insert(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, std::int64_t ind)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> rlhs(reshape(lhs, { lhs.header().count() }));
            Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> rrhs(reshape(rhs, { rhs.header().count() }));

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

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> insert(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, std::int64_t ind, std::int64_t axis)
        {
            if (empty(lhs)) {
                return clone(rhs);
            }

            if (empty(rhs)) {
                return clone(lhs);
            }

            typename Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(lhs.header(), rhs.header().dims(), axis);
            if (new_header.empty()) {
                return Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ lhs.header().count() + rhs.header().count() });
            res.header() = std::move(new_header);

            std::int64_t fixed_axis{ modulo(axis, std::ssize(lhs.header().dims())) };

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> lhs_gen(lhs.header(), fixed_axis);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> rhs_gen(rhs.header(), fixed_axis);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header(), fixed_axis);

            std::int64_t fixed_ind{ modulo(ind, lhs.header().dims()[fixed_axis]) };
            std::int64_t cycle = fixed_ind *
                (std::accumulate(res.header().dims().begin(), res.header().dims().end(), 1, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

            for (; lhs_gen && res_gen && cycle; --cycle, ++lhs_gen, ++res_gen) {
                res.data()[*res_gen] = lhs.data()[*lhs_gen];
            }
            for (; rhs_gen && res_gen; ++rhs_gen, ++res_gen) {
                res.data()[*res_gen] = rhs.data()[*rhs_gen];
            }
            for (; lhs_gen && res_gen; ++lhs_gen, ++res_gen) {
                res.data()[*res_gen] = lhs.data()[*lhs_gen];
            }

            return res;
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> remove(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            std::int64_t fixed_ind{ modulo(ind, arr.header().count()) };
            std::int64_t fixed_count{ fixed_ind + count < arr.header().count() ? count : (arr.header().count() - fixed_ind) };

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() - fixed_count });
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> rarr(reshape(arr, { arr.header().count() }));

            for (std::int64_t i = 0; i < fixed_ind; ++i) {
                res({ i }) = rarr({ i });
            }
            for (std::int64_t i = fixed_ind + fixed_count; i < arr.header().count(); ++i) {
                res({ i - fixed_count }) = rarr({ i });
            }

            return res;
        }

        /**
        * @note All elements starting from ind are being removed in case that count value is too big.
        */
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> remove(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::int64_t ind, std::int64_t count, std::int64_t axis)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            std::int64_t fixed_axis{ modulo(axis, std::ssize(arr.header().dims())) };
            std::int64_t fixed_ind{ modulo(ind, arr.header().dims()[fixed_axis]) };
            std::int64_t fixed_count{ fixed_ind + count <= arr.header().dims()[fixed_axis] ? count : (arr.header().dims()[fixed_axis] - fixed_ind) };

            typename Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(arr.header(), -fixed_count, fixed_axis);
            if (new_header.empty()) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() - (arr.header().count() / arr.header().dims()[fixed_axis]) * fixed_count });
            res.header() = std::move(new_header);

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header(), fixed_axis);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header(), fixed_axis);

            std::int64_t cycle = fixed_ind *
                (std::accumulate(res.header().dims().begin(), res.header().dims().end(), 1, std::multiplies<>{}) / res.header().dims()[fixed_axis]);

            std::int64_t removals = arr.header().count() - res.header().count();

            for (; arr_gen && res_gen && cycle; --cycle, ++arr_gen, ++res_gen) {
                res.data()[*res_gen] = arr.data()[*arr_gen];
            }
            for (; arr_gen && removals; --removals, ++arr_gen) {
                //arr.data()[*arr_gen] = rhs.data()[*rhs_gen];
            }
            for (; arr_gen && res_gen; ++arr_gen, ++res_gen) {
                res.data()[*res_gen] = arr.data()[*arr_gen];
            }

            return res;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool empty(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr) noexcept
        {
            return !arr.data() && arr.header().empty();
        }

        template <typename T, typename Unary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>    
        [[nodiscard]] inline auto transform(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, Unary_op&& op)
            -> Array<decltype(op(arr.data()[0])), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            using T_o = decltype(op(arr.data()[0]));

            if (empty(arr)) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(arr.header().dims().data(), arr.header().dims().size()));

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(arr.header()); gen; ++gen) {
                res(*gen) = op(arr(*gen));
            }

            return res;
        }

        template <typename T, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, Binary_op&& op)
            -> decltype(op(arr.data()[0], arr.data()[0]))
        {
            using T_o = decltype(op(arr.data()[0], arr.data()[0]));

            if (empty(arr)) {
                return T_o{};
            }

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen{ arr.header() };

            T_o res{ static_cast<T_o>(arr(*gen)) };
            ++gen;

            while (gen) {
                res = op(res, arr(*gen));
                ++gen;
            }

            return res;
        }

        template <typename T, typename T_o, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, const T_o& init_value, Binary_op&& op)
            -> decltype(op(init_value, arr.data()[0]))
        {
            if (empty(arr)) {
                return init_value;
            }

            T_o res{ init_value };
            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen{ arr.header() }; gen; ++gen) {
                res = op(res, arr(*gen));
            }

            return res;
        }

        template <typename T, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, Binary_op&& op, std::int64_t axis)
            -> Array<decltype(op(arr.data()[0], arr.data()[0])), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            using T_o = decltype(op(arr.data()[0], arr.data()[0]));

            if (empty(arr)) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            const std::int64_t fixed_axis{ modulo(axis, std::ssize(arr.header().dims())) };

            typename Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(arr.header(), fixed_axis);
            if (new_header.empty()) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header(), std::ssize(arr.header().dims()) - fixed_axis - 1);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            const std::int64_t reduction_iteration_cycle{ arr.header().dims()[fixed_axis] };

            while (arr_gen && res_gen) {
                T_o res_element{ static_cast<T_o>(arr(*arr_gen)) };
                ++arr_gen;
                for (std::int64_t i = 0; i < reduction_iteration_cycle - 1; ++i, ++arr_gen) {
                    res_element = op(res_element, arr(*arr_gen));
                }
                res(*res_gen) = res_element;
                ++res_gen;
            }

            return res;
        }

        template <typename T, typename T_o, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto reduce(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, const Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& init_values, Binary_op&& op, std::int64_t axis)
            -> Array<decltype(op(init_values.data()[0], arr.data()[0])), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            if (empty(arr)) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            const std::int64_t fixed_axis{ modulo(axis, std::ssize(arr.header().dims())) };

            if (init_values.header().dims().size() != 1 && init_values.header().dims()[fixed_axis] != arr.header().dims()[fixed_axis]) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            typename Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(arr.header(), axis);
            if (new_header.empty()) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ new_header.count() });
            res.header() = std::move(new_header);

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header(), std::ssize(arr.header().dims()) - fixed_axis - 1);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> init_gen(init_values.header());

            const std::int64_t reduction_iteration_cycle{ arr.header().dims()[fixed_axis] };

            while (arr_gen && res_gen && init_gen) {
                T_o res_element{ init_values(*init_gen) };
                for (std::int64_t i = 0; i < reduction_iteration_cycle; ++i, ++arr_gen) {
                    res_element = op(res_element, arr(*arr_gen));
                }
                res(*res_gen) = std::move(res_element);
                ++res_gen;
                ++init_gen;
            }

            return res;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return a && b; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> all(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return a && b; }, axis);
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool any(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return reduce(arr, [](const T& a, const T& b) { return a || b; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> any(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::int64_t axis)
        {
            return reduce(arr, [](const T& a, const T& b) { return a || b; }, axis);
        }

        template <typename T1, typename T2, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs.data()[0], rhs.data()[0])), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs.data()[0], rhs.data()[0]));
            
            if (!std::equal(lhs.header().dims().begin(), lhs.header().dims().end(), rhs.header().dims().begin(), rhs.header().dims().end())) {
                return Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(lhs.header().dims().data(), lhs.header().dims().size()));

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(lhs.header()); gen; ++gen) {
                res(*gen) = op(lhs(*gen), rhs(*gen));
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs.data()[0], rhs)), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs.data()[0], rhs));

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(lhs.header().dims().data(), lhs.header().dims().size()));

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(lhs.header()); gen; ++gen) {
                res(*gen) = op(lhs(*gen), rhs);
            }

            return res;
        }

        template <typename T1, typename T2, typename Binary_op, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto transform(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, Binary_op&& op)
            -> Array<decltype(op(lhs, rhs.data()[0])), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            using T_o = decltype(op(lhs, rhs.data()[0]));

            Array<T_o, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res(std::span<const std::int64_t>(rhs.header().dims().data(), rhs.header().dims().size()));

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(rhs.header()); gen; ++gen) {
                res(*gen) = op(lhs, rhs(*gen));
            }

            return res;
        }

        template <typename T, typename Unary_pred, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> filter(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() });

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            std::int64_t res_count{ 0 };

            while (arr_gen && res_gen) {
                if (pred(arr(*arr_gen))) {
                    res(*res_gen) = arr(*arr_gen);
                    ++res_count;
                    ++res_gen;
                }
                ++arr_gen;
            }

            if (res_count == 0) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> filter(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (!std::equal(arr.header().dims().begin(), arr.header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                return Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() });

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> mask_gen(mask.header());

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            std::int64_t res_count{ 0 };

            while (arr_gen && mask_gen && res_gen) {
                if (mask(*mask_gen)) {
                    res(*res_gen) = arr(*arr_gen);
                    ++res_count;
                    ++res_gen;
                }
                ++arr_gen;
                ++mask_gen;
            }

            if (res_count == 0) {
                return Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, typename Unary_pred, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> find(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, Unary_pred pred)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() });

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            std::int64_t res_count{ 0 };

            while (arr_gen && res_gen) {
                if (pred(arr(*arr_gen))) {
                    res(*res_gen) = *arr_gen;
                    ++res_count;
                    ++res_gen;
                }
                ++arr_gen;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> find(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& mask)
        {
            if (empty(arr)) {
                return Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (!std::equal(arr.header().dims().begin(), arr.header().dims().end(), mask.header().dims().begin(), mask.header().dims().end())) {
                return Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() });

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> mask_gen(mask.header());

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            std::int64_t res_count{ 0 };

            while (arr_gen && mask_gen && res_gen) {
                if (mask(*mask_gen)) {
                    res(*res_gen) = *arr_gen;
                    ++res_count;
                    ++res_gen;
                }
                ++arr_gen;
                ++mask_gen;
            }

            if (res_count == 0) {
                return Array<std::int64_t, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            if (res_count < arr.header().count()) {
                return resize(res, { res_count });
            }

            return res;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> transpose(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::span<const std::int64_t> order)
        {
            if (empty(arr)) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            typename Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>::Header new_header(arr.header(), order);
            if (new_header.empty()) {
                return Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>();
            }

            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> res({ arr.header().count() });
            res.header() = std::move(new_header);

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> arr_gen(arr.header(), order);
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> res_gen(res.header());

            while (arr_gen && res_gen) {
                res(*res_gen) = arr(*arr_gen);
                ++arr_gen;
                ++res_gen;
            }

            return res;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> transpose(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, std::initializer_list<std::int64_t> order)
        {
            return transpose(arr, std::span<const std::int64_t>(order.begin(), order.size() ));
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator==(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator==(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator==(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator!=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator!=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator!=(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a != b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> close(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> close(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> close(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return transform(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a > b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator>=(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a < b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<=(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline Array<bool, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> operator<=(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a <= b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator+=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator+=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a + b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator-=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator-=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a - b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator*(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator*=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator*=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a * b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator/(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator/=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator/=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a / b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator%(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator%(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator%(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator%=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator%=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a % b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator^(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator^=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator^=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a ^ b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator&=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator&=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a & b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator|(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator|=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator|=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a | b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator<<(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
            -> Array<decltype(lhs << rhs.data()[0]), Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator<<=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a << b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator>>(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator>>=(Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return lhs.transform(rhs, [](const T1& a, const T2& b) { return a >> b; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator~(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return ~a; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator!(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return !a; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator+(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return +a; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator-(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return -a; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto abs(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return abs(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto acos(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acos(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto acosh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return acosh(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto asin(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asin(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto asinh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return asinh(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto atan(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atan(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto atanh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return atanh(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto cos(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cos(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto cosh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return cosh(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto exp(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return exp(a); });
        }
        
        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto log(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto log10(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return log10(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto pow(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return pow(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sin(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sin(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sinh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sinh(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto sqrt(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return sqrt(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto tan(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tan(a); });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto tanh(const Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            return transform(arr, [](const T& a) { return tanh(a); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator&&(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a && b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator||(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return transform(lhs, rhs, [](const T1& a, const T2& b) { return a || b; });
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator++(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(arr.header()); gen; ++gen) {
                ++arr(*gen);
            }
            return arr;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& arr)
        {
            return operator++(arr);
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator++(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> old = clone(arr);
            operator++(arr);
            return old;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator++(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& arr, int)
        {
            return operator++(arr, int{});
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto& operator--(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr)
        {
            if (empty(arr)) {
                return arr;
            }

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(arr.header()); gen; ++gen) {
                --arr(*gen);
            }
            return arr;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& arr)
        {
            return operator--(arr);
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        inline auto operator--(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& arr, int)
        {
            Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator> old = clone(arr);
            operator--(arr);
            return old;
        }

        template <typename T, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline auto operator--(Array<T, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>&& arr, int)
        {
            return operator--(arr, int{});
        }

        template <typename T1, typename T2, typename Binary_pred, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, Binary_pred pred)
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

            Simple_array_indices_generator<Dims_capacity, Internals_allocator> lhs_gen(lhs.header());
            Simple_array_indices_generator<Dims_capacity, Internals_allocator> rhs_gen(rhs.header());

            for (; lhs_gen && rhs_gen; ++lhs_gen, ++rhs_gen) {
                if (!pred(lhs(*lhs_gen), rhs(*rhs_gen))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs, Binary_pred pred)
        {
            if (empty(lhs)) {
                return true;
            }

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(lhs.header()); gen; ++gen) {
                if (!pred(lhs(*gen), rhs)) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, typename Binary_pred, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_match(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, Binary_pred pred)
        {
            if (empty(rhs)) {
                return true;
            }

            for (Simple_array_indices_generator<Dims_capacity, Internals_allocator> gen(rhs.header()); gen; ++gen) {
                if (!pred(lhs, rhs(*gen))) {
                    return false;
                }
            }

            return true;
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_equal(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs)
        {
            return all_match(lhs, rhs, [](const T1& a, const T2& b) { return a == b; });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const Array<T1, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& lhs, const T2& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
        }

        template <typename T1, typename T2, std::int64_t Data_capacity, std::int64_t Dims_capacity, template<typename> typename Data_allocator, template<typename> typename Internals_allocator>
        [[nodiscard]] inline bool all_close(const T1& lhs, const Array<T2, Data_capacity, Dims_capacity, Data_allocator, Internals_allocator>& rhs, const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{}) > (), const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{}) > ())
        {
            return all_match(lhs, rhs, [&atol, &rtol](const T1& a, const T2& b) { return close(a, b, atol, rtol); });
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
