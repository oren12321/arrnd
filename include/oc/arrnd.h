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
#include <functional>
#include <complex>
#include <tuple>
#include <string>
#include <typeinfo>
#ifndef _MSC_VER
#include <cxxabi.h>
#endif
#include <ranges>

namespace oc {
namespace details {
    // reference: http://stackoverflow.com/a/20170989/1593077
    template <typename T, bool AddCvref = false>
    std::string type_name()
    {
        using bare_type = std::remove_cvref_t<T>;

        std::unique_ptr<char, void (*)(void*)> type_ptr(
#ifndef _MSC_VER
            abi::__cxa_demangle(typeid(bare_type).name(), nullptr, nullptr, nullptr),
#else
            nullptr,
#endif
            std::free);

        std::string type_name = (type_ptr ? type_ptr.get() : typeid(bare_type).name());
        if (AddCvref) {
            if (std::is_const_v<bare_type>)
                type_name += " const";
            if (std::is_volatile_v<bare_type>)
                type_name += " volatile";
            if (std::is_lvalue_reference_v<T>)
                type_name += "&";
            else if (std::is_rvalue_reference_v<T>)
                type_name += "&&";
        }
        return type_name;
    }
}
}

namespace oc {
namespace details {
    template <template <typename...> typename T, typename... Args>
    [[nodiscard]] static inline constexpr std::true_type is_template_type_impl(T<Args...>)
    {
        return std::true_type{};
    }
    template <template <typename...> typename T>
    [[nodiscard]] static inline constexpr std::false_type is_template_type_impl(...)
    {
        return std::false_type{};
    }
    template <template <typename...> typename T, typename U>
    using is_template_type = decltype(is_template_type_impl<T>(std::declval<typename std::decay_t<U>>()));
    template <typename U, template <typename...> typename T>
    concept template_type = is_template_type<T, U>::value;

    template <typename Iter>
    using iterator_value_type = typename std::iterator_traits<Iter>::value_type;
    template <typename Iter, typename T>
    concept iterator_of_type = std::input_iterator<Iter> && std::is_same_v<T, iterator_value_type<Iter>>;
    template <typename Iter, template <typename...> typename T>
    concept iterator_of_template_type = std::input_iterator<Iter>&& template_type<iterator_value_type<Iter>, T>;

    template <typename Cont>
    concept iterable = requires(Cont&& c) {
                           {
                               std::begin(c)
                           };
                           {
                               std::end(c)
                           };
                       }/* && !
    std::is_array_v<Cont>*/;
    template<typename Cont>
    using iterable_value_type = std::remove_reference_t<decltype(*std::begin(std::declval<Cont&>()))>;
    template <typename Cont, typename T>
    concept iterable_of_type = iterable<Cont> && requires(Cont&& c) {
                                                     {
                                                         std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                         } -> std::same_as<T>;
                                                 };
    template <typename Cont, template <typename...> typename T>
    concept iterable_of_template_type = iterable<Cont>&& requires(Cont&& c)
    {
        {
            std::remove_cvref_t<decltype(*std::begin(c))> { }
        }
        ->template_type<T>;
    };

    template <typename T>
    concept random_access_type = std::random_access_iterator<typename T::iterator>;

    template <typename T, std::size_t... Ns>
    constexpr std::size_t array_elements_count(std::index_sequence<Ns...>)
    {
        return (1 * ... * std::extent<T, Ns>{});
    }
    template <typename T>
    constexpr std::size_t array_elements_count()
    {
        return array_elements_count<T>(std::make_index_sequence<std::rank<T>{}>());
    }

    template <typename T, typename U>
    T (&array_cast(U& u))
    [array_elements_count<U>()]
    {
        auto ptr = reinterpret_cast<T*>(u);
        T(&res)[array_elements_count<U>()] = *reinterpret_cast<T(*)[array_elements_count<U>()]>(ptr);
        return res;
    }

    template <typename T, typename U>
    const T (&const_array_cast(const U& u))[array_elements_count<U>()]
    {
        auto ptr = reinterpret_cast<const T*>(u);
        const T(&res)[array_elements_count<U>()] = *reinterpret_cast<const T(*)[array_elements_count<U>()]>(ptr);
        return res;
    }
}

using details::iterator_value_type;
using details::iterator_of_type;
using details::iterable;
using details::iterable_of_type;
using details::random_access_type;

using details::array_cast;
using details::const_array_cast;
}

namespace oc {
namespace details {
    template <typename T>
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
        constexpr lightweight_allocator(const lightweight_allocator<U>&) noexcept
        { }

        [[nodiscard]] constexpr pointer allocate(size_type n)
        {
            assert(n > 0);
            auto p = reinterpret_cast<pointer>(operator new[](n * sizeof(value_type)));
            if (!p) {
                throw std::bad_alloc{};
            }
            return p;
        }

        constexpr void deallocate(pointer p, size_type n) noexcept
        {
            assert(p && n > 0);
            operator delete[](p, n * sizeof(value_type));
        }
    };
}

using details::lightweight_allocator;
}

namespace oc {
namespace details {
    template <typename T, template <typename> typename Allocator = lightweight_allocator>
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

        template <typename U = value_type>
        explicit constexpr simple_dynamic_vector(size_type size = 0, const U* data = nullptr)
            : size_(size)
        {
            assert(size >= 0);
            if (size > 0) {
                data_ptr_ = alloc_.allocate(size);
                if (data) {
                    if constexpr (std::is_copy_constructible_v<T>) {
                        std::uninitialized_copy_n(data, size, data_ptr_);
                    }
                } else if constexpr (!std::is_fundamental_v<T>) {
                    if constexpr (std::is_default_constructible_v<T>) {
                        std::uninitialized_default_construct_n(data_ptr_, size);
                    }
                }
            }
        }

        template <typename InputIt>
        explicit constexpr simple_dynamic_vector(const InputIt& first, const InputIt& last)
            : simple_dynamic_vector(std::distance(first, last), &(*first))
        { }

        constexpr simple_dynamic_vector(const simple_dynamic_vector& other)
            : alloc_(other.alloc_)
            , size_(other.size_)
        {
            if (!other.empty()) {
                data_ptr_ = alloc_.allocate(size_);
                if constexpr (std::is_copy_constructible_v<T>) {
                    std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);
                }
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
                alloc_.deallocate(data_ptr_, size_);
            }

            alloc_ = other.alloc_;
            size_ = other.size_;

            if (!other.empty()) {
                data_ptr_ = alloc_.allocate(size_);
                if (data_ptr_) {
                    if constexpr (std::is_copy_constructible_v<T>) {
                        std::uninitialized_copy_n(other.data_ptr_, other.size_, data_ptr_);
                    }
                }
            }

            return *this;
        }

        constexpr simple_dynamic_vector(simple_dynamic_vector&& other) noexcept
            : alloc_(std::move(other.alloc_))
            , size_(other.size_)
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
                alloc_.deallocate(data_ptr_, size_);
            }

            alloc_ = std::move(other.alloc_);
            size_ = other.size_;

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
                alloc_.deallocate(data_ptr_, size_);
            }
        }

        [[nodiscard]] constexpr bool empty() const noexcept
        {
            return size_ == 0 && !data_ptr_;
        }

        [[nodiscard]] constexpr size_type size() const noexcept
        {
            return size_;
        }

        [[nodiscard]] constexpr pointer data() const noexcept
        {
            return data_ptr_;
        }

        [[nodiscard]] constexpr reference operator[](size_type index) noexcept
        {
            assert(index >= 0 && index < size_);
            return data_ptr_[index];
        }

        [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
        {
            assert(index >= 0 && index < size_);
            return data_ptr_[index];
        }

        [[nodiscard]] constexpr pointer begin() noexcept
        {
            return data_ptr_;
        }

        [[nodiscard]] constexpr pointer end() noexcept
        {
            return data_ptr_ + size_;
        }

        [[nodiscard]] constexpr const_pointer begin() const noexcept
        {
            return data_ptr_;
        }

        [[nodiscard]] constexpr const_pointer end() const noexcept
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

        [[nodiscard]] constexpr reverse_iterator rbegin() noexcept
        {
            return reverse_iterator(end());
        }

        [[nodiscard]] constexpr reverse_iterator rend() noexcept
        {
            return reverse_iterator(begin());
        }

        [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept
        {
            return const_reverse_iterator(cend());
        }

        [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept
        {
            return const_reverse_iterator(cbegin());
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
        pointer data_ptr_ = nullptr;
    };

    template <typename T, template <typename> typename Allocator = lightweight_allocator>
    [[nodiscard]] inline constexpr bool operator==(
        const simple_dynamic_vector<T, Allocator>& lhs, const simple_dynamic_vector<T, Allocator>& rhs)
    {
        return std::equal(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend());
    }

    template <typename T, std::int64_t Size>
    class simple_static_vector final {
        static_assert(Size >= 0);

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
        using replaced_type = simple_static_vector<U, Size>;

        template <typename U = value_type>
        explicit constexpr simple_static_vector(size_type size = 0, const U* data = nullptr)
            : size_(size)
        {
            assert(size_ >= 0 && size_ <= Size);
            if (data) {
                if constexpr (std::is_copy_constructible_v<T>) {
                    std::copy(data, std::next(data, size_), data_ptr_);
                }
            }
        }

        template <typename InputIt>
        explicit constexpr simple_static_vector(const InputIt& first, const InputIt& last)
            : simple_static_vector(std::distance(first, last), &(*first))
        { }

        constexpr simple_static_vector(const simple_static_vector& other)
            : size_(other.size_)
        {
            if constexpr (std::is_copy_constructible_v<T>) {
                std::copy(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);
            }
        }

        constexpr simple_static_vector operator=(const simple_static_vector& other)
        {
            if (this == &other) {
                return *this;
            }

            size_ = other.size_;

            if constexpr (std::is_copy_constructible_v<T>) {
                std::copy(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);
            }

            return *this;
        }

        constexpr simple_static_vector(simple_static_vector&& other) noexcept
            : size_(other.size_)
        {
            if constexpr (std::is_move_constructible_v<T>) {
                std::move(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);
            }

            other.size_ = 0;
        }

        constexpr simple_static_vector operator=(simple_static_vector&& other) noexcept
        {
            if (this == &other) {
                return *this;
            }

            size_ = other.size_;

            if constexpr (std::is_move_constructible_v<T>) {
                std::move(other.data_ptr_, other.data_ptr_ + other.size_, data_ptr_);
            }

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

        [[nodiscard]] constexpr pointer data() const noexcept
        {
            return const_cast<pointer>(data_ptr_);
        }

        [[nodiscard]] constexpr reference operator[](size_type index) noexcept
        {
            assert(index >= 0 && index < size_);
            return data_ptr_[index];
        }

        [[nodiscard]] constexpr const_reference operator[](size_type index) const noexcept
        {
            assert(index >= 0 && index < size_);
            return data_ptr_[index];
        }

        [[nodiscard]] constexpr pointer begin() noexcept
        {
            return data_ptr_;
        }

        [[nodiscard]] constexpr pointer end() noexcept
        {
            return data_ptr_ + size_;
        }

        [[nodiscard]] constexpr const_pointer begin() const noexcept
        {
            return data_ptr_;
        }

        [[nodiscard]] constexpr const_pointer end() const noexcept
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

        [[nodiscard]] constexpr reverse_iterator rbegin() noexcept
        {
            return reverse_iterator(end());
        }

        [[nodiscard]] constexpr reverse_iterator rend() noexcept
        {
            return reverse_iterator(begin());
        }

        [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept
        {
            return const_reverse_iterator(cend());
        }

        [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept
        {
            return const_reverse_iterator(cbegin());
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
        size_type size_ = 0;
        value_type data_ptr_[Size];
    };

    template <typename T, std::int64_t Size>
    [[nodiscard]] inline constexpr bool operator==(
        const simple_static_vector<T, Size>& lhs, const simple_static_vector<T, Size>& rhs)
    {
        return std::equal(lhs.cbegin(), lhs.cend(), rhs.cbegin(), rhs.cend());
    }
}

using details::simple_dynamic_vector;
using details::simple_static_vector;
}

namespace oc {
namespace details {
    template <typename T>
    [[nodiscard]] inline constexpr bool operator<(const std::complex<T>& lhs, const std::complex<T>& rhs)
    {
        return std::abs(lhs) < std::abs(rhs);
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator<(const std::complex<T>& lhs, const T& rhs)
    {
        return std::abs(lhs) < rhs;
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator<(const T& lhs, const std::complex<T>& rhs)
    {
        return lhs < std::abs(rhs);
    }

    template <typename T>
    [[nodiscard]] inline constexpr bool operator<=(const std::complex<T>& lhs, const std::complex<T>& rhs)
    {
        return std::abs(lhs) <= std::abs(rhs);
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator<=(const std::complex<T>& lhs, const T& rhs)
    {
        return std::abs(lhs) <= rhs;
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator<=(const T& lhs, const std::complex<T>& rhs)
    {
        return lhs <= std::abs(rhs);
    }

    template <typename T>
    [[nodiscard]] inline constexpr bool operator>(const std::complex<T>& lhs, const std::complex<T>& rhs)
    {
        return std::abs(lhs) > std::abs(rhs);
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator>(const std::complex<T>& lhs, const T& rhs)
    {
        return std::abs(lhs) > rhs;
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator>(const T& lhs, const std::complex<T>& rhs)
    {
        return lhs > std::abs(rhs);
    }

    template <typename T>
    [[nodiscard]] inline constexpr bool operator>=(const std::complex<T>& lhs, const std::complex<T>& rhs)
    {
        return std::abs(lhs) >= std::abs(rhs);
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator>=(const std::complex<T>& lhs, const T& rhs)
    {
        return std::abs(lhs) >= rhs;
    }
    template <typename T>
    [[nodiscard]] inline constexpr bool operator>=(const T& lhs, const std::complex<T>& rhs)
    {
        return lhs >= std::abs(rhs);
    }
}

using details::operator<;
using details::operator<=;
using details::operator>;
using details::operator>=;
}

namespace oc {
namespace details {
    template <std::integral T>
    [[nodiscard]] inline constexpr T default_atol() noexcept
    {
        return T{0};
    }

    template </*std::floating_point*/ typename T>
    requires(std::floating_point<T> || template_type<T, std::complex>)
    [[nodiscard]] inline constexpr T default_atol() noexcept
    {
        return T{1e-8};
    }

    //template <template_type<std::complex> T>
    //[[nodiscard]] inline constexpr T default_atol() noexcept
    //{
    //    return T{1e-8};
    //}

    template <std::integral T>
    [[nodiscard]] inline constexpr T default_rtol() noexcept
    {
        return T{0};
    }

    template </*std::floating_point*/ typename T>
    requires(std::floating_point<T> || template_type<T, std::complex>)
    [[nodiscard]] inline constexpr T default_rtol() noexcept
    {
        return T{1e-5};
    }

    //template <template_type<std::complex> T>
    //[[nodiscard]] inline constexpr T default_atol() noexcept
    //{
    //    return T{1e-5};
    //}

    template <typename T1, typename T2>
    requires((std::is_arithmetic_v<T1> || template_type<T1, std::complex>)&&(
        std::is_arithmetic_v<T2> || template_type<T2, std::complex>))
    [[nodiscard]] inline constexpr bool close(const T1& a, const T2& b,
        const decltype(T1{} - T2{})& atol = default_atol<decltype(T1{} - T2{})>(),
        const decltype(T1{} - T2{})& rtol = default_rtol<decltype(T1{} - T2{})>()) noexcept
    {
        using std::abs;
        if (a == b) {
            return true;
        }
        const decltype(a - b) reps{rtol * (abs(a) > abs(b) ? abs(a) : abs(b))};
        return abs(a - b) <= (atol > reps ? atol : reps);
    }

    template <std::integral T1, std::integral T2>
    [[nodiscard]] inline constexpr auto modulo(const T1& value, const T2& modulus) noexcept
        -> decltype((value % modulus) + modulus)
    {
        return ((value % modulus) + modulus) % modulus;
    }

    template <typename T>
    [[nodiscard]] inline constexpr auto sign(const T& value)
    {
        return (T{0} < value) - (value < T{0});
    }
}

using details::default_atol;
using details::default_rtol;

using details::close;
using details::modulo;
using details::sign;
}

namespace oc {
namespace details {
    enum class interval_hint { full, from, to, none };

    /**
    * @note half open interval
    */
    template <std::signed_integral T = std::int64_t>
    class interval {
    public:
        // interval type might cause ignoring values of interval's start or stop values
        constexpr interval(T start, T stop, T step = 1, interval_hint type = interval_hint::none) noexcept
            : start_(start)
            , stop_(stop)
            , step_(step)
            , hint_(type)
        { }

        constexpr interval() = default; // interval of first element
        constexpr interval(const interval&) = default;
        constexpr interval& operator=(const interval&) = default;
        constexpr interval(interval&) = default;
        constexpr interval& operator=(interval&) = default;

        [[nodiscard]] constexpr T start() const noexcept
        {
            return start_;
        }

        [[nodiscard]] constexpr T stop() const noexcept
        {
            return stop_;
        }

        [[nodiscard]] constexpr T step() const noexcept
        {
            return step_;
        }

        [[nodiscard]] constexpr interval_hint hint() const noexcept
        {
            return hint_;
        }

        // returns normalize type from dimension, useful in case of interval types that are not none
        [[nodiscard]] constexpr interval align(T dim) const noexcept
        {
            switch (hint_) {
            case interval_hint::none:
                return *this;
            case interval_hint::full:
                return interval{0, dim, step_, interval_hint::none};
            case interval_hint::from:
                return interval{start_, dim, step_, interval_hint::none};
            case interval_hint::to:
                return interval{0, stop_, step_, interval_hint::none};
            }

            return *this;
        }

        [[nodiscard]] static constexpr interval full(T step = 1) noexcept
        {
            return interval{std::numeric_limits<T>::max(), std::numeric_limits<T>::max(), step, interval_hint::full};
        }

        [[nodiscard]] static constexpr interval from(T start, T step = 1)
        {
            return interval{start, std::numeric_limits<T>::max(), step, interval_hint::from};
        }

        [[nodiscard]] static constexpr interval to(T stop, T step = 1) noexcept
        {
            return interval{0, stop, step, interval_hint::to};
        }

        [[nodiscard]] static constexpr interval at(T pos) noexcept
        {
            return interval{pos, pos + 1};
        }

        [[nodiscard]] static constexpr interval between(T start, T stop, T step = 1) noexcept
        {
            return interval{start, stop, step};
        }

    private:
        T start_{0};
        T stop_{1};
        T step_{1};
        interval_hint hint_{interval_hint::none};
    };

    template <std::signed_integral T>
    [[nodiscard]] inline constexpr interval<T> reverse(const interval<T>& i) noexcept
    {
        return interval<T>{i.stop(), i.start(), -i.step()};
    }

    template <std::signed_integral T>
    [[nodiscard]] inline constexpr interval<T> modulo(const interval<T>& i, const T& modulus) noexcept
    {
        return interval<T>{modulo(i.start(), modulus), modulo(i.stop(), modulus), i.step()};
    }

    template <std::signed_integral T>
    [[nodiscard]] inline constexpr interval<T> forward(const interval<T>& i) noexcept
    {
        return i.step() < T{0} ? reverse(i) : i;
    }

    template <std::signed_integral T>
    [[nodiscard]] inline constexpr bool operator==(const interval<T>& lhs, const interval<T>& rhs) noexcept
    {
        return (lhs.hint() == interval_hint::none && rhs.hint() == interval_hint::none && lhs.start() == rhs.start()
                   && lhs.stop() == rhs.stop() && lhs.step() == rhs.step())
            || (lhs.hint() == interval_hint::full && rhs.hint() == interval_hint::full && lhs.step() == rhs.step())
            || (lhs.hint() == interval_hint::from && rhs.hint() == interval_hint::from && lhs.start() == rhs.start()
                && lhs.step() == rhs.step())
            || (lhs.hint() == interval_hint::to && rhs.hint() == interval_hint::to && lhs.start() == 0
                && rhs.start() == 0 && lhs.stop() == rhs.stop() && lhs.step() == rhs.step());
    }
}

using details::interval_hint;
using details::interval;

using details::modulo;
using details::reverse;
using details::forward;
}

namespace oc {
namespace details {

    template <typename Iter>
    concept signed_integral_type_iterator
        = std::input_iterator<Iter> && std::signed_integral<iterator_value_type<Iter>>;
    template <typename Iter>
    concept interval_type_iterator
        = std::input_iterator<Iter> && is_template_type<interval, iterator_value_type<Iter>>::value;

    template <typename Cont>
    concept signed_integral_type_iterable = iterable<Cont> && requires(Cont&& c) {
                                                                  {
                                                                      std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                                      } -> std::signed_integral;
                                                              };
    template <typename Cont>
    concept interval_type_iterable = iterable<Cont> && requires(Cont&& c) {
                                                           {
                                                               std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                               } -> template_type<interval>;
                                                       };

}

using details::signed_integral_type_iterator;
using details::interval_type_iterator;
using details::signed_integral_type_iterable;
using details::interval_type_iterable;
}

namespace oc {
namespace details {
    template <typename T, template <typename> typename Allocator = lightweight_allocator>
    struct dynamic_storage_info {
        using storage_type = simple_dynamic_vector<T, Allocator>;
        static constexpr std::int64_t size = std::dynamic_extent;
        template <typename U>
        using replaced_type = dynamic_storage_info<U, Allocator>;
    };

    template <typename T, std::int64_t N>
    struct static_storage_info {
        using storage_type = simple_static_vector<T, N>;
        static constexpr std::int64_t size = N;
        template <typename U>
        using replaced_type = static_storage_info<U, N>;
    };

    struct arrnd_header_tag { };
    template <typename T>
    concept arrnd_header_compliant = std::is_same_v<typename T::tag, arrnd_header_tag>;

    //template <random_access_type Storage = simple_dynamic_vector<std::int64_t>>
    template <typename StorageInfo = dynamic_storage_info<std::int64_t>>
    class arrnd_header {
    public:
        //using storage_type = Storage;
        using storage_type = typename StorageInfo::storage_type;

        //using size_type = typename Storage::size_type;
        using size_type = std::make_signed_t<typename storage_type::size_type>;
        //static_assert(std::signed_integral<size_type>);

        using interval_type = interval<size_type>;

        using tag = arrnd_header_tag;

        constexpr arrnd_header() = default;

        template <signed_integral_type_iterator InputIt>
        explicit constexpr arrnd_header(const InputIt& first_dim, const InputIt& last_dim)
        {
            if (first_dim == last_dim) {
                return;
            }

            assert(first_dim < last_dim);
            assert(std::all_of(first_dim, last_dim, [](auto d) {
                return d >= 0;
            }));

            numel_ = std::reduce(first_dim, last_dim, iterator_value_type<InputIt>{1}, std::multiplies<>{});
            if (numel_ == 0) {
                return;
            }

            dims_ = storage_type(first_dim, last_dim);

            strides_ = storage_type(std::ssize(dims_));
            std::exclusive_scan(dims_.crbegin(), dims_.crend(), strides_.rbegin(), size_type{1}, std::multiplies<>{});

            last_index_ = numel_ - 1;
        }

        template <signed_integral_type_iterable Cont>
        explicit constexpr arrnd_header(const Cont& dims)
            : arrnd_header(std::begin(dims), std::end(dims))
        { }

        explicit constexpr arrnd_header(std::initializer_list<size_type> dims)
            : arrnd_header(dims.begin(), dims.end())
        { }

        //template <std::signed_integral D, std::int64_t M>
        //explicit constexpr arrnd_header(const D (&dims)[M])
        //    : arrnd_header(std::begin(dims), std::end(dims))
        //{ }

        //template <signed_integral_type_iterator InputIt>
        //[[nodiscard]] constexpr arrnd_header expand(const InputIt& first_dim, const InputIt& last_dim) const
        //{
        //    if (first_dim == last_dim) {
        //        return *this;
        //    }

        //    assert(first_dim < last_dim);

        //    if (empty()) {
        //        return arrnd_header(first_dim, last_dim);
        //    }

        //    storage_type new_dims(std::ssize(dims_) + std::distance(first_dim, last_dim));

        //    std::copy(dims_.cbegin(), dims_.cend(), new_dims.begin());
        //    std::copy(first_dim, last_dim, std::next(new_dims.begin(), std::ssize(dims_)));

        //    return arrnd_header(new_dims.cbegin(), new_dims.cend());
        //}

        //template <signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr arrnd_header expand(const Cont& dims) const
        //{
        //    return expand(std::begin(dims), std::end(dims));
        //}

        //[[nodiscard]] constexpr arrnd_header expand(std::initializer_list<size_type> dims) const
        //{
        //    return expand(dims.begin(), dims.end());
        //}

        //template <std::signed_integral D, std::int64_t M>
        //[[nodiscard]] constexpr arrnd_header expand(const D (&dims)[M]) const
        //{
        //    return expand(std::begin(dims), std::end(dims));
        //}

        template <interval_type_iterator InputIt>
        [[nodiscard]] constexpr arrnd_header subheader(const InputIt& first_range, const InputIt& last_range) const
        {
            if (first_range == last_range) {
                return *this;
            }

            assert(first_range < last_range);

            if (empty()) {
                return *this;
            }

            size_type nranges = std::min(std::distance(first_range, last_range), std::ssize(dims_));

            auto valid_ranges = [&]() {
                return std::inner_product(first_range, std::next(first_range, nranges), dims_.cbegin(), true,
                    std::logical_and<>{}, [](const auto& r, auto d) {
                        auto nr = r.align(d);
                        return (nr.start() <= nr.stop() && nr.step() >= 1) && (nr.start() >= 0 && nr.stop() <= d);
                    });
            };
            assert(valid_ranges());

            arrnd_header res{};

            res.dims_ = storage_type(std::ssize(dims_));
            std::transform(first_range, std::next(first_range, nranges), dims_.cbegin(), res.dims_.begin(),
                [](const auto& r, auto d) {
                    auto nr = r.align(d);
                    return static_cast<size_type>(std::ceil(static_cast<double>(nr.stop() - nr.start()) / nr.step()));
                });
            std::copy(std::next(dims_.cbegin(), nranges), dims_.cend(), std::next(res.dims_.begin(), nranges));

            if (std::equal(res.dims_.cbegin(), res.dims_.cend(), dims_.cbegin(), dims_.cend())) {
                return *this;
            }

            res.numel_ = std::reduce(res.dims_.cbegin(), res.dims_.cend(), size_type{1}, std::multiplies<>{});
            if (res.numel_ == 0) { // allow empty header for zero interval
                arrnd_header empty_subheader{};
                empty_subheader.is_slice_ = true;
                res.is_reordered_ = is_reordered_;
                res.is_continuous_ = true;
                return empty_subheader;
            }

            res.strides_ = storage_type(std::ssize(res.dims_));

            res.offset_ = offset_;
            for (size_type i = 0; i < nranges; ++i) {
                auto s = *std::next(strides_.cbegin(), i);
                auto nr = (*std::next(first_range, i)).align(*std::next(dims_.cbegin(), i));
                *std::next(res.strides_.begin(), i) = s * nr.step();
                res.offset_ += s * nr.start();
            }
            std::copy(std::next(strides_.cbegin(), nranges), strides_.cend(), std::next(res.strides_.begin(), nranges));

            auto dot_prod = std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(),
                size_type{0}, std::plus<>{}, [](auto d, auto s) {
                    return (d - 1) * s;
                });

            res.last_index_ = res.offset_ + dot_prod;

            res.is_slice_ = true;

            res.is_reordered_ = is_reordered_;
            res.order_ = order_;

            res.is_continuous_ = (dot_prod + 1 == res.numel_);

            return res;
        }

        template <interval_type_iterable Cont>
        [[nodiscard]] constexpr arrnd_header subheader(const Cont& ranges) const
        {
            return subheader(std::begin(ranges), std::end(ranges));
        }

        [[nodiscard]] constexpr arrnd_header subheader(std::initializer_list<interval_type> ranges) const
        {
            return subheader(ranges.begin(), ranges.end());
        }

        //template <std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr arrnd_header subheader(const interval<U> (&ranges)[M]) const
        //{
        //    return subheader(std::begin(ranges), std::end(ranges));
        //}

        [[nodiscard]] constexpr arrnd_header subheader(interval_type range) const
        {
            std::initializer_list<interval_type> ranges = {range/*.align(dims_.front())*/};

            auto res = subheader(ranges.begin(), ranges.end());
            if (res.empty() || res.dims_.front() != 1) {
                return res;
            }

            res.dims_ = storage_type(std::next(res.dims_.cbegin(), 1), res.dims_.cend());
            res.strides_ = storage_type(std::next(res.strides_.cbegin(), 1), res.strides_.cend());
            //res.last_index_ = res.offset_
            //    + std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(), size_type{0},
            //        std::plus<>{}, [](auto d, auto s) {
            //            return (d - 1) * s;
            //        });
            //res.is_slice_ = true;

            return res;
        }

        [[nodiscard]] constexpr arrnd_header subheader(interval_type range, size_type axis) const
        {
            assert(axis < std::ssize(dims_));

            typename storage_type::template replaced_type<interval_type> ranges(axis + 1);

            std::fill(ranges.begin(), ranges.end(), interval_type::full());
            ranges[std::ssize(ranges) - 1] = range/*.align(*std::next(dims_.cbegin(), axis))*/;

            return subheader(ranges.begin(), ranges.end());
        }

        //[[nodiscard]] constexpr arrnd_header subheader(size_type omitted_axis) const
        //{
        //    assert(omitted_axis >= 0 && omitted_axis < std::ssize(dims_));

        //    if (empty()) {
        //        return *this;
        //    }

        //    storage_type new_dims(std::ssize(dims_) > 1 ? std::ssize(dims_) - 1 : 1);

        //    if (std::ssize(dims_) == 1) {
        //        new_dims.front() = 1;
        //        return arrnd_header(new_dims.cbegin(), new_dims.cend());
        //    }

        //    std::copy(dims_.cbegin(), std::next(dims_.cbegin(), omitted_axis), new_dims.begin());
        //    std::copy(
        //        std::next(dims_.cbegin(), omitted_axis + 1), dims_.cend(), std::next(new_dims.begin(), omitted_axis));

        //    return arrnd_header(new_dims.cbegin(), new_dims.cend());
        //}

        [[nodiscard]] constexpr storage_type dims_with_modified_axis(size_type removed_axis) const
        {
            assert(removed_axis >= 0 && removed_axis < std::ssize(dims_));

            storage_type new_dims(std::ssize(dims_) > 1 ? std::ssize(dims_) - 1 : 1);

            if (std::ssize(dims_) == 1) {
                new_dims.front() = 1;
                return new_dims;
            }

            std::copy(dims_.cbegin(), std::next(dims_.cbegin(), removed_axis), new_dims.begin());
            std::copy(
                std::next(dims_.cbegin(), removed_axis + 1), dims_.cend(), std::next(new_dims.begin(), removed_axis));

            return new_dims;
        }

        //[[nodiscard]] constexpr arrnd_header subheader(size_type count, size_type axis) const
        //{
        //    assert(axis >= 0 && axis < std::ssize(dims_));
        //    assert(count >= -*std::next(dims_.cbegin(), axis));

        //    if (empty()) {
        //        return *this;
        //    }

        //    storage_type new_dims(dims_);
        //    *std::next(new_dims.begin(), axis) += count;

        //    return arrnd_header(new_dims.cbegin(), new_dims.cend());
        //}

        [[nodiscard]] constexpr storage_type dims_with_modified_axis(size_type axis, size_type added_count) const
        {
            assert(axis >= 0 && axis < std::ssize(dims_));
            assert(added_count >= -*std::next(dims_.cbegin(), axis));

            storage_type new_dims(dims_);
            *std::next(new_dims.begin(), axis) += added_count;

            return new_dims;
        }

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr arrnd_header reorder(const InputIt& first_order, const InputIt& last_order) const
        {
            assert(std::distance(first_order, last_order) == std::ssize(dims_));
            assert(std::all_of(first_order, last_order, [&](auto order) {
                return order >= 0 && order < std::ssize(dims_);
            }));

            if (empty() || std::ssize(dims_) == 1) {
                return *this;
            }

            storage_type same_order(std::ssize(dims_));
            std::iota(same_order.begin(), same_order.end(), size_type{0});

            if (std::equal(same_order.cbegin(), same_order.cend(), first_order, last_order)) {
                return *this;
            }

            arrnd_header res(*this);

            res.order_ = storage_type(std::ssize(dims_));
            std::copy(first_order, last_order, res.order_.begin());

            for (size_type i = 0; i < std::ssize(dims_); ++i) {
                *std::next(res.dims_.begin(), i) = *std::next(dims_.cbegin(), *std::next(first_order, i));
                *std::next(res.strides_.begin(), i) = *std::next(strides_.cbegin(), *std::next(first_order, i));
            }

            res.is_reordered_ = true;

            return res;
        }

        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr arrnd_header reorder(const Cont& order) const
        {
            return reorder(std::begin(order), std::end(order));
        }

        [[nodiscard]] constexpr arrnd_header reorder(std::initializer_list<size_type> order) const
        {
            return reorder(order.begin(), order.end());
        }

        //template <std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr arrnd_header reorder(const U (&order)[M]) const
        //{
        //    return reorder(std::begin(order), std::end(order));
        //}

        [[nodiscard]] constexpr arrnd_header reorder(size_type main_axis) const
        {
            if (empty() || std::ssize(dims_) == 1 || main_axis == 0) {
                return *this;
            }

            assert(main_axis >= 0 && main_axis < std::ssize(dims_));

            arrnd_header res(*this);

            size_type main_dim = *std::next(dims_.cbegin(), main_axis);
            size_type main_stride = *std::next(strides_.cbegin(), main_axis);

            size_type j = 1;

            res.order_ = storage_type(std::ssize(dims_));

            for (size_type i = 0; i < main_axis; ++i) {
                *std::next(res.dims_.begin(), j) = *std::next(dims_.cbegin(), i);
                *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                *std::next(res.order_.begin(), j) = i;
                ++j;
            }

            for (size_type i = main_axis + 1; i < std::ssize(dims_); ++i) {
                *std::next(res.dims_.begin(), j) = *std::next(dims_.cbegin(), i);
                *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                *std::next(res.order_.begin(), j) = i;
                ++j;
            }

            res.dims_.front() = main_dim;
            res.strides_.front() = main_stride;
            res.order_.front() = main_axis;

            res.is_reordered_ = true;

            return res;
        }

        [[nodiscard]] constexpr arrnd_header squeeze() const
        {
            if (empty() || std::ssize(dims_) == 1) {
                return *this;
            }

            size_type ones_count = std::count(dims_.cbegin(), dims_.cend(), size_type{1});
            if (ones_count == 0) {
                return *this;
            }

            arrnd_header res(*this);

            res.dims_ = storage_type(std::ssize(dims_) - ones_count);
            std::copy_if(dims_.cbegin(), dims_.cend(), res.dims_.begin(), [](auto d) {
                return d != size_type{1};
            });

            res.strides_ = storage_type(std::ssize(strides_) - ones_count);
            size_type j = 0;
            for (size_type i = 0; i < std::ssize(strides_); ++i) {
                if (*std::next(dims_.cbegin(), i) != size_type{1}) {
                    *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                    ++j;
                }
            }

            //res.is_slice_ = true;

            return res;
        }

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr size_type subs2ind(const InputIt& first_sub, const InputIt& last_sub) const
        {
            assert(first_sub < last_sub); // at least one subscript is required

            size_type nsubs = std::distance(first_sub, last_sub);
            assert(nsubs > 0 && nsubs <= std::ssize(dims_));

            auto valid_subs = [&]() {
                return std::inner_product(first_sub, last_sub, std::next(dims_.cbegin(), std::ssize(dims_) - nsubs),
                    true, std::logical_and<>{}, [](auto s, auto d) {
                        return (s >= 0 && s < d);
                    });
            };
            assert(valid_subs());

            return offset_
                + std::transform_reduce(first_sub, last_sub, std::next(strides_.cbegin(), std::ssize(strides_) - nsubs),
                    size_type{0}, std::plus<>{}, std::multiplies<>{});
        }

        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr size_type subs2ind(const Cont& subs) const
        {
            return subs2ind(std::begin(subs), std::end(subs));
        }

        [[nodiscard]] constexpr size_type subs2ind(std::initializer_list<size_type> subs) const
        {
            return subs2ind(subs.begin(), subs.end());
        }

        [[nodiscard]] constexpr storage_type ind2subs(size_type ind) const
        {
            if (empty()) {
                return storage_type();
            }

            storage_type subs(std::ssize(dims_));
            ind -= offset_;
            for (size_type i = std::ssize(dims_) - 1; i >= 0; --i) {
                if (*std::next(dims_.cbegin(), i) > 1) {
                    *std::next(subs.begin(), i)
                        = (ind / *std::next(strides_.cbegin(), i)) % *std::next(dims_.cbegin(), i);
                } else {
                    *std::next(subs.begin(), i) = 0;
                }
            }

            return subs;
        }

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr bool is_reduced_dims_from(const InputIt& first_dim, const InputIt& last_dim) const
        {
            size_type ndims = std::distance(first_dim, last_dim);

            assert(ndims >= 0);

            if (dims_.size() != ndims) {
                return false;
            }

            return !std::equal(dims_.cbegin(), dims_.cend(), first_dim)
                && std::transform_reduce(
                    dims_.cbegin(), dims_.cend(), first_dim, true, std::logical_and<>{}, [](auto d1, auto d2) {
                        return d1 == d2 || d1 == size_type{1};
                    });
        }

        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr bool is_reduced_dims_from(const Cont& dims) const
        {
            return is_reduced_dims_from(std::begin(dims), std::end(dims));
        }

        //template <std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr bool is_reduced_dims_from(const U (&dims)[M]) const
        //{
        //    return is_reduced_dims_from(std::begin(dims), std::end(dims));
        //}

        [[nodiscard]] constexpr bool is_reduced_dims_from(std::initializer_list<size_type> dims) const
        {
            return is_reduced_dims_from(dims.begin(), dims.end());
        }


        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr storage_type complement_dims_from(const InputIt& first_dim, const InputIt& last_dim) const
        {
            assert(dims_.size() == std::distance(first_dim, last_dim));

            storage_type comp_dims(dims_.size());

            std::transform(dims_.cbegin(), dims_.cend(), first_dim, comp_dims.begin(), [](auto rd, auto cd) {
                return cd - rd + 1;
            });

            return comp_dims;
        }

        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr storage_type complement_dims_from(const Cont& dims) const
        {
            return complement_dims_from(std::begin(dims), std::end(dims));
        }

        //template <std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr storage_type complement_dims_from(const U (&dims)[M]) const
        //{
        //    return complement_dims_from(std::begin(dims), std::end(dims));
        //}

        [[nodiscard]] constexpr storage_type complement_dims_from(std::initializer_list<size_type> dims) const
        {
            return complement_dims_from(dims.begin(), dims.end());
        }


        constexpr arrnd_header(arrnd_header&& other) = default;
        constexpr arrnd_header& operator=(arrnd_header&& other) = default;

        constexpr arrnd_header(const arrnd_header& other) = default;
        constexpr arrnd_header& operator=(const arrnd_header& other) = default;

        virtual constexpr ~arrnd_header() = default;

        [[nodiscard]] constexpr size_type numel() const noexcept
        {
            return numel_;
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

        [[nodiscard]] constexpr size_type offset() const noexcept
        {
            return offset_;
        }

        [[nodiscard]] constexpr bool is_slice() const noexcept
        {
            return is_slice_;
        }

        [[nodiscard]] constexpr bool is_reordered() const noexcept
        {
            return is_reordered_;
        }

        [[nodiscard]] constexpr storage_type& order() noexcept
        {
            return order_;
        }

        [[nodiscard]] constexpr bool is_continuous() const noexcept
        {
            return is_continuous_;
        }

        [[nodiscard]] constexpr bool empty() const noexcept
        {
            return dims_.empty();
        }

        [[nodiscard]] constexpr size_type last_index() const noexcept
        {
            return last_index_;
        }

        [[nodiscard]] constexpr bool is_vector() const noexcept
        {
            return std::ssize(dims_) == 1;
        }

        [[nodiscard]] constexpr bool is_row() const noexcept
        {
            return std::ssize(dims_) == 2 && dims_.front() == 1;
        }

        [[nodiscard]] constexpr bool is_column() const noexcept
        {
            return std::ssize(dims_) == 2 && dims_.back() == 1;
        }

        [[nodiscard]] constexpr bool is_matrix() const noexcept
        {
            return std::ssize(dims_) == 2;
        }

        [[nodiscard]] constexpr bool is_scalar() const noexcept
        {
            return numel_ == 1;
        }

    private:
        storage_type dims_{};
        storage_type strides_{};
        size_type numel_{0};
        size_type offset_{0};
        size_type last_index_{0};
        bool is_slice_{false}; // not all array buffer included
        bool is_reordered_{false}; // header axis are reordered
        storage_type order_{};
        bool is_continuous_{true}; // array buffer is not continuous in memory
    };

    template <arrnd_header_compliant ArHdrCo>
    inline constexpr std::ostream& operator<<(std::ostream& os, const ArHdrCo& hdr)
    {
        if (hdr.empty()) {
            os << "empty";
            return os;
        }

        auto print_vec = [&os](const auto& vec) {
            os << '[';
            std::for_each_n(std::cbegin(vec), std::ssize(vec) - 1, [&os](const auto& e) {
                os << e << ' ';
            });
            os << *std::next(std::cbegin(vec), std::ssize(vec) - 1) << ']';
        };

        os << "numel: " << hdr.numel() << '\n';
        os << "dims: ";
        print_vec(hdr.dims());
        os << '\n';
        os << "strides: ";
        print_vec(hdr.strides());
        os << '\n';
        os << "offset: " << hdr.offset() << '\n';
        os << "last_index: " << hdr.last_index() << '\n';
        os << "flags: vector(" << hdr.is_vector() << "), matrix(" << hdr.is_matrix() << "), row(" << hdr.is_row()
           << "), column(" << hdr.is_column() << "), scalar(" << hdr.is_scalar() << "), slice(" << hdr.is_slice()
           << "), reordered(" << hdr.is_reordered() << "), continuous(" << hdr.is_continuous() << ')';

        return os;
    }
}

using details::arrnd_header_tag;
using details::arrnd_header_compliant;
using details::arrnd_header;

using details::dynamic_storage_info;
using details::static_storage_info;
}

namespace oc {
namespace details {
    enum class arrnd_iterator_start_position { begin, end, rbegin, rend };

    template <arrnd_header_compliant Header = arrnd_header<>>
    class arrnd_indexer final {
    public:
        using storage_type = typename Header::storage_type;
        using header_type = Header;
        using size_type = typename Header::size_type;

        explicit constexpr arrnd_indexer(
            const header_type& hdr, arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
            : hdr_(hdr)
        {
            setup(pos);
        }

        template <std::integral U>
        explicit constexpr arrnd_indexer(
            const header_type& hdr, U axis, arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
            : hdr_(hdr.reorder(axis))
        {
            setup(pos);
        }

        template <signed_integral_type_iterator InputIt>
        explicit constexpr arrnd_indexer(const header_type& hdr, const InputIt& first_order, const InputIt& last_order,
            arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
            : hdr_(hdr.reorder(first_order, last_order))
        {
            setup(pos);
        }

        template <signed_integral_type_iterable Cont>
        explicit constexpr arrnd_indexer(const header_type& hdr, const Cont& order,
            arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
            : arrnd_indexer(hdr, std::begin(order), std::end(order), pos)
        { }

        explicit constexpr arrnd_indexer(const header_type& hdr, std::initializer_list<size_type> order,
            arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
            : arrnd_indexer(hdr, order.begin(), order.end(), pos)
        { }

        //template <std::signed_integral U, std::int64_t M>
        //explicit constexpr arrnd_indexer(const header_type& hdr, const U (&order)[M],
        //    arrnd_iterator_start_position pos = arrnd_iterator_start_position::begin)
        //    : arrnd_indexer(hdr, std::begin(order), std::end(order), pos)
        //{ }

        constexpr arrnd_indexer() = default;

        constexpr arrnd_indexer(const arrnd_indexer& other) = default;
        constexpr arrnd_indexer& operator=(const arrnd_indexer& other) = default;

        constexpr arrnd_indexer(arrnd_indexer&& other) noexcept = default;
        constexpr arrnd_indexer& operator=(arrnd_indexer&& other) noexcept = default;

        constexpr ~arrnd_indexer() = default;

        constexpr arrnd_indexer& operator++() noexcept
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

            //for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
            //    ++firsts_[i].index;
            //    current_index_ += firsts_[i].stride;
            //    if (firsts_[i].index < firsts_[i].dim) {
            //        return *this;
            //    }
            //    current_index_ -= firsts_[i].index * firsts_[i].stride;
            //    firsts_[i].index = 0;
            //}

            for (size_type i = 0/*3*/; i < hdr_.dims().size(); ++i) {
                ++indices_[hdr_.dims().size() - 1 - i];
                current_index_ += hdr_.strides()[hdr_.dims().size() - 1 - i];
                if (indices_[hdr_.dims().size() - 1 - i] < hdr_.dims()[hdr_.dims().size() - 1 - i]) {
                    return *this;
                }
                current_index_
                    -= indices_[hdr_.dims().size() - 1 - i] * hdr_.strides()[hdr_.dims().size() - 1 - i];
                indices_[hdr_.dims().size() - 1 - i] = 0;
            }

            return *this;
        }

        constexpr arrnd_indexer operator++(int) noexcept
        {
            arrnd_indexer<header_type> temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_indexer& operator+=(size_type count) noexcept
        {
            for (size_type i = 0; i < count; ++i) {
                ++(*this);
            }
            return *this;
        }

        arrnd_indexer operator+(size_type count) const noexcept
        {
            arrnd_indexer<header_type> temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_indexer& operator--() noexcept
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

            //for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
            //    --firsts_[i].index;
            //    current_index_ -= firsts_[i].stride;
            //    if (firsts_[i].index > -1) {
            //        return *this;
            //    }
            //    firsts_[i].index = firsts_[i].dim - 1;
            //    current_index_ += (firsts_[i].index + 1) * firsts_[i].stride;
            //}

            for (size_type i = 0/*3*/; i < hdr_.dims().size(); ++i) {
                --indices_[hdr_.dims().size() - 1 - i];
                current_index_ -= hdr_.strides()[hdr_.dims().size() - 1 - i];
                if (indices_[hdr_.dims().size() - 1 - i] > -1) {
                    return *this;
                }
                indices_[hdr_.dims().size() - 1 - i] = hdr_.dims()[hdr_.dims().size() - 1 - i] - 1;
                current_index_ += (indices_[hdr_.dims().size() - 1 - i] + 1)
                    * hdr_.strides()[hdr_.dims().size() - 1 - i];
            }

            return *this;
        }

        constexpr arrnd_indexer operator--(int) noexcept
        {
            arrnd_indexer<header_type> temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_indexer& operator-=(size_type count) noexcept
        {
            for (size_type i = 0; i < count; ++i) {
                --(*this);
            }
            return *this;
        }

        constexpr arrnd_indexer operator-(size_type count) const noexcept
        {
            arrnd_indexer<header_type> temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] explicit constexpr operator bool() const noexcept
        {
            return static_cast<std::make_unsigned_t<size_type>>(current_index_ - hdr_.offset()) <= last_first_diff_
                && !hdr_.empty();
        }

        [[nodiscard]] constexpr size_type operator*() const noexcept
        {
            return current_index_;
        }

        [[nodiscard]] constexpr const storage_type& indices() const noexcept
        {
            return indices_;
        }

        [[nodiscard]] constexpr size_type operator[](size_type index) const noexcept
        {
            assert(index >= 0 && index < hdr_.numel());

            size_type advance_count = index - rel_pos_;
            if (advance_count > 0) {
                return ((*this) + advance_count).current_index_;
            }
            if (advance_count < 0) {
                return ((*this) - (-advance_count)).current_index_;
            }
            return current_index_;
        }

    private:
        constexpr void setup(arrnd_iterator_start_position pos)
        {
            last_first_diff_ = static_cast<std::make_unsigned_t<size_type>>(hdr_.last_index() - hdr_.offset());

            bool backward = (pos == arrnd_iterator_start_position::rbegin || pos == arrnd_iterator_start_position::end);

            //for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
            //    firsts_[i].dim = hdr_.dims()[hdr_.dims().size() - i - 1];
            //    firsts_[i].stride = hdr_.strides()[hdr_.dims().size() - i - 1];
            //    firsts_[i].index = backward ? firsts_[i].dim - 1 : 0;
            //}

            //if (std::ssize(hdr_.dims()) > 3) {
                indices_ = storage_type(hdr_.dims().size()/* - 3*/);
                for (size_type i = 0/*3*/; i < hdr_.dims().size(); ++i) {
                    indices_[hdr_.dims().size() - 1 - i] = backward ? hdr_.dims()[hdr_.dims().size() - 1 - i] - 1 : 0;
                }
            //}

            current_index_ = backward ? hdr_.last_index() : hdr_.offset();

            rel_pos_ = backward ? hdr_.numel() - 1 : 0;

            if (pos == arrnd_iterator_start_position::end) {
                ++(*this);
            } else if (pos == arrnd_iterator_start_position::rend) {
                --(*this);
            }
        }

        header_type hdr_;

        std::make_unsigned_t<size_type> last_first_diff_;

        //struct data_package {
        //    size_type dim;
        //    size_type stride;
        //    size_type index;
        //};

        //data_package firsts_[3];

        storage_type indices_;
        size_type current_index_;

        size_type rel_pos_ = 0;
    };

    template <arrnd_header_compliant Header = arrnd_header<>>
    class arrnd_axis_ranger final {
    public:
        using header_type = Header;
        using size_type = typename Header::size_type;
        using interval_type = typename Header::interval_type;

        using storage_type = typename Header::storage_type::template replaced_type<interval_type>;

        explicit constexpr arrnd_axis_ranger(const header_type& hdr, size_type fixed_axis = 0,
            interval_type window = interval_type(0, 0, 1), bool is_window_contained = true,
            arrnd_iterator_start_position start_pos = arrnd_iterator_start_position::begin)
            : fixed_axis_(fixed_axis)
            , left_window_size_(-window.start())
            , right_window_size_(window.stop())
            , window_step_(window.step())
            , is_window_contained_(is_window_contained)
        {
            assert(fixed_axis_ >= 0 && fixed_axis_ < hdr.dims().size());
            assert(window.start() <= 0 && window.stop() >= 0);

            fixed_axis_dim_ = *std::next(hdr.dims().cbegin(), fixed_axis_);

            assert(window.stop() - window.start() <= fixed_axis_dim_ && window.step() <= fixed_axis_dim_);

            ranges_ = storage_type(hdr.dims().size());
            for (size_type i = 0; i < hdr.dims().size(); ++i) {
                *std::next(ranges_.begin(), i) = interval_type(0, *std::next(hdr.dims().cbegin(), i));
            }

            last_index_ = fixed_axis_dim_ - 1;

            switch (start_pos) {
            case arrnd_iterator_start_position::begin:
            case arrnd_iterator_start_position::rend:
                current_index_ = is_window_contained_ ? left_window_size_ : 0;
                break;
            case arrnd_iterator_start_position::end:
            case arrnd_iterator_start_position::rbegin:
                current_index_ = is_window_contained_ ? fixed_axis_dim_ - 1 - right_window_size_ : fixed_axis_dim_ - 1;
                break;
            }

            ranges_[fixed_axis_] = compute_current_index_interval();

            if (start_pos == arrnd_iterator_start_position::end) {
                ++(*this);
            } else if (start_pos == arrnd_iterator_start_position::rend) {
                --(*this);
            }
        }

        constexpr arrnd_axis_ranger() = default;

        constexpr arrnd_axis_ranger(const arrnd_axis_ranger& other) = default;
        constexpr arrnd_axis_ranger& operator=(const arrnd_axis_ranger& other) = default;

        constexpr arrnd_axis_ranger(arrnd_axis_ranger&& other) noexcept = default;
        constexpr arrnd_axis_ranger& operator=(arrnd_axis_ranger&& other) noexcept = default;

        constexpr ~arrnd_axis_ranger() = default;

        constexpr arrnd_axis_ranger& operator++() noexcept
        {
            ++current_index_;
            ranges_[fixed_axis_] = compute_current_index_interval();
            return *this;
        }

        constexpr arrnd_axis_ranger operator++(int) noexcept
        {
            arrnd_axis_ranger<header_type> temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_axis_ranger& operator+=(size_type count) noexcept
        {
            current_index_ += count;
            ranges_[fixed_axis_] = compute_current_index_interval();
            return *this;
        }

        constexpr arrnd_axis_ranger operator+(size_type count) const noexcept
        {
            arrnd_axis_ranger<header_type> temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_axis_ranger& operator--() noexcept
        {
            --current_index_;
            ranges_[fixed_axis_] = compute_current_index_interval();
            return *this;
        }

        constexpr arrnd_axis_ranger operator--(int) noexcept
        {
            arrnd_axis_ranger<header_type> temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_axis_ranger& operator-=(size_type count) noexcept
        {
            current_index_ -= count;
            ranges_[fixed_axis_] = compute_current_index_interval();
            return *this;
        }

        constexpr arrnd_axis_ranger operator-(size_type count) const noexcept
        {
            arrnd_axis_ranger<header_type> temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] explicit constexpr operator bool() const noexcept
        {
            return is_window_contained_
                ? (current_index_ - left_window_size_ >= 0 && current_index_ + right_window_size_ <= last_index_)
                : (current_index_ >= 0 && current_index_ <= last_index_);
        }

        [[nodiscard]] constexpr const storage_type& operator*() const noexcept
        {
            return ranges_;
        }

        [[nodiscard]] constexpr storage_type operator[](size_type index) const noexcept
        {
            if (is_window_contained_) {
                assert(index - left_window_size_ >= 0 && index + right_window_size_ <= last_index_);
            } else {
                assert(index >= 0 && index <= last_index_);
            }

            size_type advance_count = index - current_index_;
            if (advance_count > 0) {
                return ((*this) + advance_count).ranges_;
            }
            if (advance_count < 0) {
                return ((*this) - (-advance_count)).ranges_;
            }
            return ranges_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_axis_ranger& far) const noexcept
        {
            return current_index_ == far.current_index_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_axis_ranger& far) const noexcept
        {
            return current_index_ < far.current_index_;
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_axis_ranger& far) const noexcept
        {
            return current_index_ <= far.current_index_;
        }

        [[nodiscard]] constexpr size_type fixed_axis() const noexcept
        {
            return fixed_axis_;
        }

        constexpr arrnd_axis_ranger& change_window(interval_type window) noexcept
        {
            assert(window.start() <= 0 && window.stop() >= 0);
            assert(window.stop() - window.start() <= fixed_axis_dim_ && window.step() <= fixed_axis_dim_);

            left_window_size_ = -window.start();
            right_window_size_ = window.stop();
            window_step_ = window.step();
            ranges_[fixed_axis_] = compute_current_index_interval();
            return *this;
        }

    private:
        // calculate current interval window according to current index
        [[nodiscard]] constexpr interval_type compute_current_index_interval() const noexcept
        {
            if (current_index_ < 0 || current_index_ > last_index_) {
                return interval_type{};
            }

            // calculate normalized forward neigh
            size_type norm_window_stop = (current_index_ + right_window_size_ <= last_index_)
                ? right_window_size_
                : last_index_ - current_index_;

            // calculate normalized backward neigh
            size_type norm_window_start
                = (current_index_ - left_window_size_ >= 0) ? left_window_size_ : current_index_;

            return interval_type{
                current_index_ - norm_window_start, current_index_ + norm_window_stop + 1, window_step_};
        }

        size_type fixed_axis_;
        size_type fixed_axis_dim_;
        size_type left_window_size_;
        size_type right_window_size_;
        size_type window_step_;
        size_type current_index_;
        size_type last_index_;
        storage_type ranges_;
        bool is_window_contained_;
    };
}

using details::arrnd_iterator_start_position;
using details::arrnd_indexer;
using details::arrnd_axis_ranger;
}

namespace oc {
namespace details {
    struct arrnd_tag { };
    template <typename T>
    concept arrnd_compliant = std::is_same_v<typename std::remove_cvref_t<T>::tag, arrnd_tag>;

    template <typename T, typename U>
    concept same_depth = T::depth ==
    U::depth;

    template <typename T, typename... Args>
    concept invocable_no_arrnd = !
    arrnd_compliant<T>&& std::is_invocable_v<T, Args...>;

    struct arrnd_returned_element_iterator_tag { };
    struct arrnd_returned_slice_iterator_tag { };

    template <typename Arrnd>
    requires std::is_same_v<typename Arrnd::tag, arrnd_tag> class arrnd_const_iterator;

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;
        using returned_type_category = arrnd_returned_element_iterator_tag;

        friend class arrnd_const_iterator<Arrnd>;

        explicit constexpr arrnd_iterator(pointer data, const indexer_type& gen)
            : gen_(gen)
            , data_(data)
        { }

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
            arrnd_iterator temp{*this};
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
            arrnd_iterator temp{*this};
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
            arrnd_iterator temp{*this};
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
            arrnd_iterator temp{*this};
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

        [[nodiscard]] constexpr bool operator<(const arrnd_iterator& iter) const noexcept
        {
            return *gen_ < *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_iterator& iter) const noexcept
        {
            return *gen_ <= *(iter.gen_);
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) const noexcept
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
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;
        using returned_type_category = arrnd_returned_element_iterator_tag;

        explicit constexpr arrnd_const_iterator(pointer data, const indexer_type& gen)
            : gen_(gen)
            , data_(data)
        { }

        constexpr arrnd_const_iterator() = default;

        constexpr arrnd_const_iterator(const arrnd_const_iterator& other) = default;
        constexpr arrnd_const_iterator& operator=(const arrnd_const_iterator& other) = default;

        constexpr arrnd_const_iterator(arrnd_const_iterator&& other) noexcept = default;
        constexpr arrnd_const_iterator& operator=(arrnd_const_iterator&& other) noexcept = default;

        constexpr ~arrnd_const_iterator() = default;

        constexpr arrnd_const_iterator(const arrnd_iterator<Arrnd>& other)
            : gen_(other.gen_)
            , data_(other.data_)
        { }
        constexpr arrnd_const_iterator& operator=(const arrnd_iterator<Arrnd>& other)
        {
            gen_ = other.gen_;
            data_ = other.data_;
            return *this;
        }

        constexpr arrnd_const_iterator(arrnd_iterator<Arrnd>&& other)
            : gen_(std::move(other.gen_))
            , data_(std::move(other.data_))
        { }
        constexpr arrnd_const_iterator& operator=(arrnd_iterator<Arrnd>&& other)
        {
            gen_ = std::move(other.gen_);
            data_ = std::move(other.data_);
            return *this;
        }

        constexpr arrnd_const_iterator& operator++() noexcept
        {
            ++gen_;
            return *this;
        }

        constexpr arrnd_const_iterator operator++(int) noexcept
        {
            arrnd_const_iterator temp{*this};
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
            arrnd_const_iterator temp{*this};
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
            arrnd_const_iterator temp{*this};
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
            arrnd_const_iterator temp{*this};
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

        [[nodiscard]] constexpr bool operator<(const arrnd_const_iterator& iter) const noexcept
        {
            return *gen_ < *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_const_iterator& iter) const noexcept
        {
            return *gen_ <= *(iter.gen_);
        }

        [[nodiscard]] constexpr const reference operator[](difference_type index) const noexcept
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
    requires std::is_same_v<typename Arrnd::tag, arrnd_tag> class arrnd_const_reverse_iterator;

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;
        using returned_type_category = arrnd_returned_element_iterator_tag;

        friend arrnd_const_reverse_iterator<Arrnd>;

        explicit constexpr arrnd_reverse_iterator(pointer data, const indexer_type& gen)
            : gen_(gen)
            , data_(data)
        { }

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
            arrnd_reverse_iterator temp{*this};
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
            arrnd_reverse_iterator temp{*this};
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
            arrnd_reverse_iterator temp{*this};
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
            arrnd_reverse_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
            return data_[*gen_];
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_reverse_iterator& iter) const noexcept
        {
            return *gen_ == *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_reverse_iterator& iter) const noexcept
        {
            return *gen_ > *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_reverse_iterator& iter) const noexcept
        {
            return *gen_ >= *(iter.gen_);
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) const noexcept
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
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_const_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;
        using returned_type_category = arrnd_returned_element_iterator_tag;

        explicit constexpr arrnd_const_reverse_iterator(pointer data, const indexer_type& gen)
            : gen_(gen)
            , data_(data)
        { }

        constexpr arrnd_const_reverse_iterator() = default;

        constexpr arrnd_const_reverse_iterator(const arrnd_const_reverse_iterator& other) = default;
        constexpr arrnd_const_reverse_iterator& operator=(const arrnd_const_reverse_iterator& other) = default;

        constexpr arrnd_const_reverse_iterator(arrnd_const_reverse_iterator&& other) noexcept = default;
        constexpr arrnd_const_reverse_iterator& operator=(arrnd_const_reverse_iterator&& other) noexcept = default;

        constexpr ~arrnd_const_reverse_iterator() = default;

        constexpr arrnd_const_reverse_iterator(const arrnd_reverse_iterator<Arrnd>& other)
            : gen_(other.gen_)
            , data_(other.data_)
        { }
        constexpr arrnd_const_reverse_iterator& operator=(const arrnd_reverse_iterator<Arrnd>& other)
        {
            gen_ = other.gen_;
            data_ = other.data_;
            return *this;
        }

        constexpr arrnd_const_reverse_iterator(arrnd_reverse_iterator<Arrnd>&& other)
            : gen_(std::move(other.gen_))
            , data_(std::move(other.data_))
        { }
        constexpr arrnd_const_reverse_iterator& operator=(arrnd_reverse_iterator<Arrnd>&& other)
        {
            gen_ = std::move(other.gen_);
            data_ = std::move(other.data_);
            return *this;
        }

        constexpr arrnd_const_reverse_iterator& operator++() noexcept
        {
            --gen_;
            return *this;
        }

        constexpr arrnd_const_reverse_iterator operator++(int) noexcept
        {
            arrnd_const_reverse_iterator temp{*this};
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
            arrnd_const_reverse_iterator temp{*this};
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
            arrnd_const_reverse_iterator temp{*this};
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
            arrnd_const_reverse_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr const reference operator*() const noexcept
        {
            return data_[*gen_];
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_const_reverse_iterator& iter) const noexcept
        {
            return *gen_ == *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_const_reverse_iterator& iter) const noexcept
        {
            return *gen_ > *(iter.gen_);
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_const_reverse_iterator& iter) const noexcept
        {
            return *gen_ >= *(iter.gen_);
        }

        [[nodiscard]] constexpr const reference operator[](difference_type index) const noexcept
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

    template <typename Arrnd>
    requires std::is_same_v<typename Arrnd::tag, arrnd_tag> class arrnd_slice_const_iterator;

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_slice_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using reference = Arrnd&;

        using ranger_type = typename Arrnd::ranger_type;
        using returned_type_category = arrnd_returned_slice_iterator_tag;

        friend arrnd_slice_const_iterator<Arrnd>;

        explicit constexpr arrnd_slice_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
        {
            if (far) {
                slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
            }
        }

        constexpr arrnd_slice_iterator() = default;

        constexpr arrnd_slice_iterator(const arrnd_slice_iterator& other) = default;
        constexpr arrnd_slice_iterator& operator=(const arrnd_slice_iterator& other) = default;

        constexpr arrnd_slice_iterator(arrnd_slice_iterator&& other) noexcept = default;
        constexpr arrnd_slice_iterator& operator=(arrnd_slice_iterator&& other) noexcept = default;

        constexpr ~arrnd_slice_iterator() = default;

        constexpr arrnd_slice_iterator& operator++() noexcept
        {
            ++far_;
            return *this;
        }

        constexpr arrnd_slice_iterator operator++(int) noexcept
        {
            arrnd_slice_iterator temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_slice_iterator& operator+=(difference_type count) noexcept
        {
            far_ += count;
            return *this;
        }

        constexpr arrnd_slice_iterator operator+(difference_type count) const
        {
            arrnd_slice_iterator temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_slice_iterator& operator--() noexcept
        {
            --far_;
            return *this;
        }

        constexpr arrnd_slice_iterator operator--(int) noexcept
        {
            arrnd_slice_iterator temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_slice_iterator& operator-=(difference_type count) noexcept
        {
            far_ -= count;
            return *this;
        }

        constexpr arrnd_slice_iterator operator-(difference_type count) const noexcept
        {
            arrnd_slice_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_slice_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_slice_iterator& iter) const noexcept
        {
            return far_ < iter.far_;
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_slice_iterator& iter) const noexcept
        {
            return far_ <= iter.far_;
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) const noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_slice_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_slice_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using const_reference = const Arrnd&;

        using ranger_type = typename Arrnd::ranger_type;
        using returned_type_category = arrnd_returned_slice_iterator_tag;

        explicit constexpr arrnd_slice_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
        {
            if (far) {
                slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
            }
        }

        constexpr arrnd_slice_const_iterator() = default;

        constexpr arrnd_slice_const_iterator(const arrnd_slice_const_iterator& other) = default;
        constexpr arrnd_slice_const_iterator& operator=(const arrnd_slice_const_iterator& other) = default;

        constexpr arrnd_slice_const_iterator(arrnd_slice_const_iterator&& other) noexcept = default;
        constexpr arrnd_slice_const_iterator& operator=(arrnd_slice_const_iterator&& other) noexcept = default;

        constexpr ~arrnd_slice_const_iterator() = default;

        constexpr arrnd_slice_const_iterator(const arrnd_slice_iterator<Arrnd>& other)
            : arrnd_ref_(other.arrnd_ref_)
            , far_(other.far_)
            , slice_(other.slice_)
        { }
        constexpr arrnd_slice_const_iterator& operator=(const arrnd_slice_iterator<Arrnd>& other)
        {
            arrnd_ref_ = other.arrnd_ref_;
            far_ = other.far_;
            slice_ = other.slice_;
            return *this;
        }

        constexpr arrnd_slice_const_iterator(arrnd_slice_iterator<Arrnd>&& other)
            : arrnd_ref_(std::move(other.arrnd_ref_))
            , far_(std::move(other.far_))
            , slice_(std::move(other.slice_))
        { }
        constexpr arrnd_slice_const_iterator& operator=(arrnd_slice_iterator<Arrnd>&& other)
        {
            arrnd_ref_ = std::move(other.arrnd_ref_);
            far_ = std::move(other.far_);
            slice_ = std::move(other.slice_);
            return *this;
        }

        constexpr arrnd_slice_const_iterator& operator++() noexcept
        {
            ++far_;
            return *this;
        }

        constexpr arrnd_slice_const_iterator operator++(int) noexcept
        {
            arrnd_slice_const_iterator temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_slice_const_iterator& operator+=(difference_type count) noexcept
        {
            far_ += count;
            return *this;
        }

        constexpr arrnd_slice_const_iterator operator+(difference_type count) const noexcept
        {
            arrnd_slice_const_iterator temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_slice_const_iterator& operator--() noexcept
        {
            --far_;
            return *this;
        }

        constexpr arrnd_slice_const_iterator operator--(int) noexcept
        {
            arrnd_slice_const_iterator temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_slice_const_iterator& operator-=(difference_type count) noexcept
        {
            far_ -= count;
            return *this;
        }

        constexpr arrnd_slice_const_iterator operator-(difference_type count) const noexcept
        {
            arrnd_slice_const_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr const_reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_slice_const_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_slice_const_iterator& iter) const noexcept
        {
            return far_ < iter.far_;
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_slice_const_iterator& iter) const noexcept
        {
            return far_ <= iter.far_;
        }

        [[nodiscard]] constexpr const_reference operator[](difference_type index) const noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_slice_const_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <typename Arrnd>
    requires std::is_same_v<typename Arrnd::tag, arrnd_tag> class arrnd_slice_reverse_const_iterator;

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_slice_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using reference = Arrnd&;

        using ranger_type = typename Arrnd::ranger_type;
        using returned_type_category = arrnd_returned_slice_iterator_tag;

        friend arrnd_slice_reverse_const_iterator<Arrnd>;

        explicit constexpr arrnd_slice_reverse_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
        {
            if (far) {
                slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
            }
        }

        constexpr arrnd_slice_reverse_iterator() = default;

        constexpr arrnd_slice_reverse_iterator(const arrnd_slice_reverse_iterator& other) = default;
        constexpr arrnd_slice_reverse_iterator& operator=(const arrnd_slice_reverse_iterator& other) = default;

        constexpr arrnd_slice_reverse_iterator(arrnd_slice_reverse_iterator&& other) noexcept = default;
        constexpr arrnd_slice_reverse_iterator& operator=(arrnd_slice_reverse_iterator&& other) noexcept = default;

        constexpr ~arrnd_slice_reverse_iterator() = default;

        constexpr arrnd_slice_reverse_iterator& operator--() noexcept
        {
            ++far_;
            return *this;
        }

        constexpr arrnd_slice_reverse_iterator operator--(int) noexcept
        {
            arrnd_slice_reverse_iterator temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_slice_reverse_iterator& operator-=(difference_type count) noexcept
        {
            far_ += count;
            return *this;
        }

        constexpr arrnd_slice_reverse_iterator operator-(difference_type count) const noexcept
        {
            arrnd_slice_reverse_iterator temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_slice_reverse_iterator& operator++() noexcept
        {
            --far_;
            return *this;
        }

        constexpr arrnd_slice_reverse_iterator operator++(int) noexcept
        {
            arrnd_slice_reverse_iterator temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_slice_reverse_iterator& operator+=(difference_type count) noexcept
        {
            far_ -= count;
            return *this;
        }

        constexpr arrnd_slice_reverse_iterator operator+(difference_type count) const noexcept
        {
            arrnd_slice_reverse_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_slice_reverse_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_slice_reverse_iterator& iter) const noexcept
        {
            return far_ > iter.far_;
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_slice_reverse_iterator& iter) const noexcept
        {
            return far_ >= iter.far_;
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) const noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_slice_reverse_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <typename Arrnd>
        requires std::is_same_v<typename Arrnd::tag, arrnd_tag>
    class arrnd_slice_reverse_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using const_reference = const Arrnd&;

        using ranger_type = typename Arrnd::ranger_type;
        using returned_type_category = arrnd_returned_slice_iterator_tag;

        explicit constexpr arrnd_slice_reverse_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
        {
            if (far) {
                slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
            }
        }

        constexpr arrnd_slice_reverse_const_iterator() = default;

        constexpr arrnd_slice_reverse_const_iterator(const arrnd_slice_reverse_const_iterator& other) = default;
        constexpr arrnd_slice_reverse_const_iterator& operator=(const arrnd_slice_reverse_const_iterator& other)
            = default;

        constexpr arrnd_slice_reverse_const_iterator(arrnd_slice_reverse_const_iterator&& other) noexcept = default;
        constexpr arrnd_slice_reverse_const_iterator& operator=(arrnd_slice_reverse_const_iterator&& other) noexcept
            = default;

        constexpr ~arrnd_slice_reverse_const_iterator() = default;

        constexpr arrnd_slice_reverse_const_iterator(const arrnd_slice_reverse_iterator<Arrnd>& other)
            : arrnd_ref_(other.arrnd_ref_)
            , far_(other.far_)
            , slice_(other.slice_)
        { }
        constexpr arrnd_slice_reverse_const_iterator& operator=(const arrnd_slice_reverse_iterator<Arrnd>& other)
        {
            arrnd_ref_ = other.arrnd_ref_;
            far_ = other.far_;
            slice_ = other.slice_;
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator(arrnd_slice_reverse_iterator<Arrnd>&& other)
            : arrnd_ref_(std::move(other.arrnd_ref_))
            , far_(std::move(other.far_))
            , slice_(std::move(other.slice_))
        { }
        constexpr arrnd_slice_reverse_const_iterator& operator=(arrnd_slice_reverse_iterator<Arrnd>&& other)
        {
            arrnd_ref_ = std::move(other.arrnd_ref_);
            far_ = std::move(other.far_);
            slice_ = std::move(other.slice_);
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator& operator--() noexcept
        {
            ++far_;
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator operator--(int) noexcept
        {
            arrnd_slice_reverse_const_iterator temp{*this};
            ++(*this);
            return temp;
        }

        constexpr arrnd_slice_reverse_const_iterator& operator-=(difference_type count) noexcept
        {
            far_ += count;
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator operator-(difference_type count) const noexcept
        {
            arrnd_slice_reverse_const_iterator temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_slice_reverse_const_iterator& operator++() noexcept
        {
            --far_;
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator operator++(int) noexcept
        {
            arrnd_slice_reverse_const_iterator temp{*this};
            --(*this);
            return temp;
        }

        constexpr arrnd_slice_reverse_const_iterator& operator+=(difference_type count) noexcept
        {
            far_ -= count;
            return *this;
        }

        constexpr arrnd_slice_reverse_const_iterator operator+(difference_type count) const noexcept
        {
            arrnd_slice_reverse_const_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr const_reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_slice_reverse_const_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_slice_reverse_const_iterator& iter) const noexcept
        {
            return far_ > iter.far_;
        }

        [[nodiscard]] constexpr bool operator<=(const arrnd_slice_reverse_const_iterator& iter) const noexcept
        {
            return far_ >= iter.far_;
        }

        [[nodiscard]] constexpr const_reference operator[](difference_type index) const noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(
            const arrnd_slice_reverse_const_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <arrnd_compliant Arrnd>
    class arrnd_back_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;

        constexpr arrnd_back_insert_iterator() noexcept = default;

        explicit arrnd_back_insert_iterator(Arrnd& cont) noexcept
            : cont_(std::addressof(cont))
        { }

        arrnd_back_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->append/*<0>*/(cont);
            return *this;
        }

        arrnd_back_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->append/*<0>*/(std::move(cont));
            return *this;
        }

        arrnd_back_insert_iterator& operator=(const typename Arrnd::value_type& value)
        {
            *cont_ = cont_->append/*<0>*/(Arrnd({1}, {value}));
            return *this;
        }

        arrnd_back_insert_iterator& operator=(typename Arrnd::value_type&& value)
        {
            *cont_ = cont_->append/*<0>*/(std::move(Arrnd({1}, {value})));
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

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_back_insert_iterator<Arrnd> arrnd_back_inserter(Arrnd& cont) noexcept
    {
        return arrnd_back_insert_iterator<Arrnd>(cont);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_front_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;

        constexpr arrnd_front_insert_iterator() noexcept = default;

        explicit arrnd_front_insert_iterator(Arrnd& cont)
            : cont_(std::addressof(cont))
        { }

        arrnd_front_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->insert/*<0>*/(cont, 0);
            return *this;
        }

        arrnd_front_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(cont), 0);
            return *this;
        }

        arrnd_front_insert_iterator& operator=(const typename Arrnd::value_type& value)
        {
            *cont_ = cont_->insert/*<0>*/(Arrnd({1}, {value}), 0);
            return *this;
        }

        arrnd_front_insert_iterator& operator=(typename Arrnd::value_type&& value)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(Arrnd({1}, {value})), 0);
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

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_front_insert_iterator<Arrnd> arrnd_front_inserter(Arrnd& cont)
    {
        return arrnd_front_insert_iterator<Arrnd>(cont);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_insert_iterator() noexcept = default;

        explicit arrnd_insert_iterator(Arrnd& cont, size_type ind = 0)
            : cont_(std::addressof(cont))
            , ind_(ind)
        { }

        arrnd_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->insert/*<0>*/(cont, ind_);
            ind_ += cont.header().numel();
            return *this;
        }

        arrnd_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(cont), ind_);
            ind_ += cont.header().numel();
            return *this;
        }

        arrnd_insert_iterator& operator=(const typename Arrnd::value_type& value)
        {
            *cont_ = cont_->insert/*<0>*/(Arrnd({1}, {value}), ind_);
            ind_ += 1;
            return *this;
        }

        arrnd_insert_iterator& operator=(typename Arrnd::value_type&& value)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(Arrnd({1}, {value})), ind_);
            ind_ += 1;
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
        size_type ind_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_insert_iterator<Arrnd> arrnd_inserter(
        Arrnd& cont, typename Arrnd::size_type ind = 0)
    {
        return arrnd_insert_iterator<Arrnd>(cont, ind);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_slice_back_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_slice_back_insert_iterator() noexcept = default;

        explicit arrnd_slice_back_insert_iterator(Arrnd& cont, size_type axis = 0) noexcept
            : cont_(std::addressof(cont))
            , axis_(axis)
        { }

        arrnd_slice_back_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->append/*<0>*/(cont, axis_);
            return *this;
        }

        arrnd_slice_back_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->append/*<0>*/(std::move(cont), axis_);
            return *this;
        }

        [[nodiscard]] arrnd_slice_back_insert_iterator& operator*() noexcept
        {
            return *this;
        }

        arrnd_slice_back_insert_iterator& operator++() noexcept
        {
            return *this;
        }

        arrnd_slice_back_insert_iterator operator++(int) noexcept
        {
            return *this;
        }

    protected:
        Arrnd* cont_ = nullptr;
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_slice_back_insert_iterator<Arrnd> arrnd_slice_back_inserter(
        Arrnd& cont, typename Arrnd::size_type axis = 0) noexcept
    {
        return arrnd_slice_back_insert_iterator<Arrnd>(cont, axis);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_slice_front_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_slice_front_insert_iterator() noexcept = default;

        explicit arrnd_slice_front_insert_iterator(Arrnd& cont, size_type axis = 0)
            : cont_(std::addressof(cont))
            , axis_(axis)
        { }

        arrnd_slice_front_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->insert/*<0>*/(cont, 0, axis_);
            return *this;
        }

        arrnd_slice_front_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(cont), 0, axis_);
            return *this;
        }

        [[nodiscard]] arrnd_slice_front_insert_iterator& operator*()
        {
            return *this;
        }

        arrnd_slice_front_insert_iterator& operator++()
        {
            return *this;
        }

        arrnd_slice_front_insert_iterator operator++(int)
        {
            return *this;
        }

    protected:
        Arrnd* cont_ = nullptr;
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_slice_front_insert_iterator<Arrnd> arrnd_slice_front_inserter(
        Arrnd& cont, typename Arrnd::size_type axis = 0)
    {
        return arrnd_slice_front_insert_iterator<Arrnd>(cont, axis);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_slice_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_slice_insert_iterator() noexcept = default;

        explicit arrnd_slice_insert_iterator(Arrnd& cont, size_type ind = 0, size_type axis = 0)
            : cont_(std::addressof(cont))
            , ind_(ind)
            , axis_(axis)
        { }

        arrnd_slice_insert_iterator& operator=(const Arrnd& cont)
        {
            *cont_ = cont_->insert/*<0>*/(cont, ind_, axis_);
            ind_ += cont.header().dims()[axis_];
            return *this;
        }

        arrnd_slice_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->insert/*<0>*/(std::move(cont), ind_, axis_);
            ind_ += cont.header().dims()[axis_];
            return *this;
        }

        [[nodiscard]] arrnd_slice_insert_iterator& operator*()
        {
            return *this;
        }

        arrnd_slice_insert_iterator& operator++()
        {
            return *this;
        }

        arrnd_slice_insert_iterator operator++(int)
        {
            return *this;
        }

    protected:
        Arrnd* cont_ = nullptr;
        size_type ind_;
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_slice_insert_iterator<Arrnd> arrnd_slice_inserter(
        Arrnd& cont, typename Arrnd::size_type ind = 0, typename Arrnd::size_type axis = 0)
    {
        return arrnd_slice_insert_iterator<Arrnd>(cont, ind, axis);
    }

    template <arrnd_compliant T>
    [[nodiscard]] inline constexpr std::int64_t calc_arrnd_depth()
    {
        return T::depth + 1;
    }
    template <typename T>
    [[nodiscard]] inline constexpr std::int64_t calc_arrnd_depth()
    {
        return 0;
    }

    template <typename T>
    concept flat_arrnd_compliant = arrnd_compliant<T> && T::is_flat;

    template <typename T, typename U>
    [[nodiscard]] inline constexpr bool is_arrnd_of_type()
    {
        return std::is_same_v<T, U>;
    }
    template <arrnd_compliant T, typename U>
    [[nodiscard]] inline constexpr bool is_arrnd_of_type()
    {
        return is_arrnd_of_type<typename T::value_type, U>();
    }

    template <typename T, typename U>
    concept arrnd_compliant_of_type = arrnd_compliant<T> && is_arrnd_of_type<T, U>();

    template <typename T, template <typename...> typename U>
    [[nodiscard]] inline constexpr bool is_arrnd_of_template_type()
    {
        return is_template_type<U, T>::value;
    }
    template <arrnd_compliant T, template <typename...> typename U>
    [[nodiscard]] inline constexpr bool is_arrnd_of_template_type()
    {
        return is_arrnd_of_template_type<typename T::value_type, U>();
    }

    template <typename T, template <typename...> typename U>
    concept arrnd_compliant_of_template_type = arrnd_compliant<T> && is_arrnd_of_template_type<T, U>();

    template <typename T, template <typename> typename Trait>
    [[nodiscard]] inline constexpr bool is_arrnd_with_trait()
    {
        return Trait<T>::value;
    }
    template <arrnd_compliant T, template <typename> typename Trait>
    [[nodiscard]] inline constexpr bool is_arrnd_with_trait()
    {
        return is_arrnd_with_trait<typename T::value_type, Trait>();
    }

    template <typename T, template <typename> typename Trait>
    concept arrnd_compliant_with_trait = arrnd_compliant<T> && is_arrnd_with_trait<T, Trait>();

    template <typename ArrndSrc, typename ArrndDst>
    concept arrnd_depths_match
        = arrnd_compliant<ArrndSrc> && arrnd_compliant<ArrndDst> && same_depth<ArrndSrc, ArrndDst>;

    template <typename T, std::int64_t Depth>
        requires(Depth >= 0 && Depth <= T::depth)
    struct arrnd_inner_impl {
        using type = arrnd_inner_impl<typename T::value_type, Depth - 1>::type;
    };
    template <typename T>
    struct arrnd_inner_impl<T, 0> {
        using type = T;
    };
    template </*arrnd_compliant*/ typename ArCo, std::int64_t Level = ArCo::depth>
        requires std::is_same_v<typename ArCo::tag, arrnd_tag>
    using arrnd_inner = arrnd_inner_impl<ArCo, Level>;
    template </*arrnd_compliant*/ typename ArCo, std::int64_t Level = ArCo::depth>
        requires std::is_same_v<typename ArCo::tag, arrnd_tag>
    using arrnd_inner_t = arrnd_inner<ArCo, Level>::type;

    template <typename T>
    struct typed {
        using type = T;
    };

    template <typename T, typename R, std::int64_t Level>
    struct last_inner_replaced_types_tuple_impl {
        using type
            = std::tuple<T, typename last_inner_replaced_types_tuple_impl<typename T::value_type, R, Level - 1>::type>;
    };
    template <typename T, typename R>
    struct last_inner_replaced_types_tuple_impl<T, R, 0> {
        using type = typename T::template replaced_type<R>;
    };
    template <typename T, typename R, std::int64_t Level>
    using last_inner_replaced_types_tuple = std::conditional_t<Level == 0,
        typed<std::tuple<typename T::template replaced_type<R>>>, last_inner_replaced_types_tuple_impl<T, R, Level>>;
    template <typename T, typename R, std::int64_t Level>
    using last_inner_replaced_types_tuple_t = typename last_inner_replaced_types_tuple<T, R, Level>::type;

    template <typename T>
    struct flat_tuple {
        using type = std::tuple<T>;
    };
    template <typename... Args>
    struct flat_tuple<std::tuple<Args...>> {
        using type = decltype(std::tuple_cat(typename flat_tuple<Args>::type{}...));
    };
    template <typename Tuple>
    using flat_tuple_t = typename flat_tuple<Tuple>::type;

    template <typename Tuple, std::int64_t Index>
    struct folded_replaced_type_tuple {
        static constexpr std::size_t tsi = std::tuple_size_v<Tuple> - 1;
        using type = typename std::tuple_element_t<tsi - Index,
            Tuple>::template replaced_type<typename folded_replaced_type_tuple<Tuple, Index - 1>::type>;
    };
    template <typename Tuple>
    struct folded_replaced_type_tuple<Tuple, 1> {
        static constexpr std::size_t tsi = std::tuple_size_v<Tuple> - 1;
        using type =
            typename std::tuple_element_t<tsi - 1, Tuple>::template replaced_type<std::tuple_element_t<tsi - 0, Tuple>>;
    };
    template <typename Tuple>
    struct folded_replaced_type_tuple<Tuple, 0> {
        static constexpr int tsi = std::tuple_size_v<Tuple> - 1;
        using type = std::tuple_element_t<tsi - 0, Tuple>;
    };
    template <typename Tuple>
    using folded_replaced_type_tuple_t = folded_replaced_type_tuple<Tuple, std::tuple_size_v<Tuple> - 1>::type;

    template <typename T, typename R, std::int64_t Level>
    struct inner_replaced_type {
        using type = folded_replaced_type_tuple_t<flat_tuple_t<last_inner_replaced_types_tuple_t<T, R, Level>>>;
    };
    template <typename T, typename R, std::int64_t Level>
    using inner_replaced_type_t = inner_replaced_type<T, R, Level>::type;

    enum class arrnd_shape_preset { vector, row, column };
    enum class arrnd_diag_type { from_matrix, to_matrix };

    // zip class

    template <typename... _Tp>
    bool variadic_or(_Tp&&... args)
    {
        return (... || args);
    }

    template <typename Tuple, std::size_t... I>
    bool any_equals(Tuple&& t1, Tuple&& t2, std::index_sequence<I...>)
    {
        return variadic_or(std::get<I>(std::forward<Tuple>(t1)) == std::get<I>(std::forward<Tuple>(t2))...);
    }

    template <typename... _Tp>
    bool variadic_and(_Tp&&... args)
    {
        return (... && args);
    }

    template <typename Tuple, std::size_t... I>
    bool all_equals(Tuple&& t1, Tuple&& t2, std::index_sequence<I...>)
    {
        return variadic_and(std::get<I>(std::forward<Tuple>(t1)) == std::get<I>(std::forward<Tuple>(t2))...);
    }

    template <typename Tuple, std::size_t... I>
    bool all_lesseq(Tuple&& t1, Tuple&& t2, std::index_sequence<I...>)
    {
        return variadic_and(std::get<I>(std::forward<Tuple>(t1)) <= std::get<I>(std::forward<Tuple>(t2))...);
    }

    template <typename Tuple, std::size_t... I>
    constexpr auto tuple_max(Tuple&& t, std::index_sequence<I...>)
    {
        return std::max({
            std::get<I>(std::forward<Tuple>(t))...,
        });
    }

    template <typename Cont, typename... Args>
    class zipped_cont {
    public:
        using cont_type = Cont;
        using iter_type = decltype(begin(std::declval<Cont&>(), Args{}...));
        using riter_type = decltype(rbegin(std::declval<Cont&>(), Args{}...));

        constexpr zipped_cont(Cont& cont, Args&&... args)
            : cont_(cont)
            , args_(std::forward_as_tuple(std::forward<Args>(args)...))
        { }

        constexpr zipped_cont(const zipped_cont&) = default;
        constexpr zipped_cont(zipped_cont&&) = default;

        constexpr zipped_cont& operator=(const zipped_cont&) = default;
        constexpr zipped_cont& operator=(zipped_cont&&) = default;

        constexpr virtual ~zipped_cont() = default;

        constexpr auto& cont() noexcept
        {
            return cont_;
        }
        constexpr const auto& cont() const noexcept
        {
            return cont_;
        }

        constexpr auto& args() noexcept
        {
            return args_;
        }
        constexpr const auto& args() const
        {
            return args_;
        }

    private:
        Cont& cont_;
        std::tuple<Args...> args_ = std::tuple<>{};
    };

    template <std::input_iterator InputIt>
    class zipped_iter {
    public:
        struct unknown { };
        using cont_type = unknown;
        using iter_type = InputIt;
        using riter_type = InputIt;

        constexpr zipped_iter(const InputIt& first, const InputIt& last)
            : first_(first)
            , last_(last)
        { }

        constexpr zipped_iter(const zipped_iter&) = default;
        constexpr zipped_iter(zipped_iter&&) = default;

        constexpr zipped_iter& operator=(const zipped_iter&) = default;
        constexpr zipped_iter& operator=(zipped_iter&&) = default;

        constexpr virtual ~zipped_iter() = default;

        constexpr auto& first() noexcept
        {
            return first_;
        }
        constexpr const auto& first() const noexcept
        {
            return first_;
        }

        constexpr auto& last() noexcept
        {
            return last_;
        }
        constexpr const auto& last() const
        {
            return last_;
        }

        constexpr auto& args() noexcept
        {
            return args_;
        }
        constexpr const auto& args() const
        {
            return args_;
        }

    private:
        InputIt first_;
        InputIt last_;
        std::tuple<> args_ = std::tuple<>{};
    };

    template <typename... ItPack>
    class zip {
    public:
        struct iterator {
            using iterator_category = std::random_access_iterator_tag;
            using value_type = std::tuple<std::iter_value_t<typename ItPack::iter_type>...>;
            using reference = std::tuple<std::iter_reference_t<typename ItPack::iter_type>...>;
            using difference_type = std::int64_t;
            //std::tuple<std::iter_difference_t<typename ItPack::iter_type>...>;
            // using pointer = std::tuple<typename std::iterator_traits<std::ranges::iterator_t<T>>::pointer...>;

            [[nodiscard]] constexpr reference operator*() const
            {
                return std::apply(
                    []<typename... Ts>(Ts&&... e) {
                        return std::forward_as_tuple(*std::forward<Ts>(e)...);
                    },
                    data_);
            }

            constexpr iterator& operator++()
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple(++std::forward<Ts>(e)...);
                    },
                    data_);
                return *this;
            }

            constexpr iterator operator++(int)
            {
                iterator temp{*this};
                ++(*this);
                return temp;
            }

            constexpr iterator& operator+=(std::int64_t count)
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple((std::forward<Ts>(e) += count)...);
                    },
                    data_);
                return *this;
            }

            [[nodiscard]] constexpr iterator operator+(std::int64_t count) const
            {
                iterator temp{*this};
                temp += count;
                return temp;
            }

            constexpr iterator& operator--()
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple(--std::forward<Ts>(e)...);
                    },
                    data_);
                return *this;
            }

            constexpr iterator operator--(int)
            {
                iterator temp{*this};
                --(*this);
                return temp;
            }

            constexpr iterator& operator-=(std::int64_t count)
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple((std::forward<Ts>(e) -= count)...);
                    },
                    data_);
                return *this;
            }

            [[nodiscard]] constexpr iterator operator-(std::int64_t count) const
            {
                iterator temp{*this};
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr auto operator!=(const iterator& iter) const
            {
                return !any_equals(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr auto operator==(const iterator& iter) const
            {
                return all_equals(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr reference operator[](std::int64_t index) const
            {
                return std::apply(
                    [index]<typename... Ts>(Ts&&... e) {
                        return std::forward_as_tuple(std::forward<Ts>(e)[index]...);
                    },
                    data_);
            }

            [[nodiscard]] constexpr bool operator<(const iterator& iter) const noexcept
            {
                return all_lesseq(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr std::int64_t operator-(const iterator& iter) const noexcept
            {
                auto impl
                    = []<typename T1, typename T2, std::size_t... I>(T1&& t1, T2&& t2, std::index_sequence<I...>) {
                          return std::forward_as_tuple(
                              (std::get<I>(std::forward<T1>(t1)) - std::get<I>(std::forward<T2>(t2)))...);
                      };

                auto diffs = impl(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
                return tuple_max(diffs, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            std::tuple<typename ItPack::iter_type...> data_;
        };

        struct reverse_iterator {
            using iterator_category = std::random_access_iterator_tag;
            using value_type = std::tuple<std::iter_value_t<typename ItPack::riter_type>...>;
            using reference = std::tuple<std::iter_reference_t<typename ItPack::riter_type>...>;
            using difference_type = std::int64_t;
            //std::tuple<std::iter_difference_t<typename ItPack::riter_type>...>;
            // using pointer = std::tuple<typename std::reverse_iterator_traits<std::ranges::reverse_iterator_t<T>>::pointer...>;

            [[nodiscard]] constexpr reference operator*() const
            {
                return std::apply(
                    []<typename... Ts>(Ts&&... e) {
                        return std::forward_as_tuple(*std::forward<Ts>(e)...);
                    },
                    data_);
            }

            constexpr reverse_iterator& operator++()
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple(++std::forward<Ts>(e)...);
                    },
                    data_);
                return *this;
            }

            constexpr reverse_iterator operator++(int)
            {
                reverse_iterator temp{*this};
                ++(*this);
                return temp;
            }

            constexpr reverse_iterator& operator+=(std::int64_t count)
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple((std::forward<Ts>(e) += count)...);
                    },
                    data_);
                return *this;
            }

            [[nodiscard]] constexpr reverse_iterator operator+(std::int64_t count) const
            {
                reverse_iterator temp{*this};
                temp += count;
                return temp;
            }

            constexpr reverse_iterator& operator--()
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple(--std::forward<Ts>(e)...);
                    },
                    data_);
                return *this;
            }

            constexpr reverse_iterator operator--(int)
            {
                reverse_iterator temp{*this};
                --(*this);
                return temp;
            }

            constexpr reverse_iterator& operator-=(std::int64_t count)
            {
                std::apply(
                    [&]<typename... Ts>(Ts&&... e) {
                        data_ = std::make_tuple((std::forward<Ts>(e) -= count)...);
                    },
                    data_);
                return *this;
            }

            [[nodiscard]] constexpr reverse_iterator operator-(std::int64_t count) const
            {
                reverse_iterator temp{*this};
                temp -= count;
                return temp;
            }

            [[nodiscard]] constexpr auto operator!=(const reverse_iterator& iter) const
            {
                return !any_equals(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr auto operator==(const reverse_iterator& iter) const
            {
                return all_equals(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr reference operator[](std::int64_t index) const
            {
                return std::apply(
                    [index]<typename... Ts>(Ts&&... e) {
                        return std::forward_as_tuple(std::forward<Ts>(e)[index]...);
                    },
                    data_);
            }

            [[nodiscard]] constexpr bool operator<(const reverse_iterator& iter) const noexcept
            {
                return all_lesseq(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            [[nodiscard]] constexpr std::int64_t operator-(const reverse_iterator& iter) const noexcept
            {
                auto impl
                    = []<typename T1, typename T2, std::size_t... I>(T1&& t1, T2&& t2, std::index_sequence<I...>) {
                          return std::forward_as_tuple(
                              (std::get<I>(std::forward<T1>(t1)) - std::get<I>(std::forward<T2>(t2)))...);
                      };

                auto diffs = impl(data_, iter.data_, std::index_sequence_for<typename ItPack::cont_type...>{});
                return tuple_max(diffs, std::index_sequence_for<typename ItPack::cont_type...>{});
            }

            std::tuple<typename ItPack::riter_type...> data_;
        };

        zip(ItPack... packs)
            : packs_(std::forward_as_tuple(packs...))
        { }

        auto begin()
        {
            auto impl = []<typename P, std::size_t... I>(P ip, std::index_sequence<I...>) {
                if constexpr (template_type<P, zipped_cont>) {
                    using std::begin;
                    return begin(ip.cont(), std::get<I>(ip.args())...);
                }
                else {
                    return ip.first();
                }
            };

            return iterator(std::apply(
                [&]<typename... Ts>(Ts&&... e) {
                    return std::make_tuple(impl(std::forward<Ts>(e),
                        std::make_index_sequence<
                            std::tuple_size_v<std::remove_cvref_t<decltype(std::forward<Ts>(e).args())>>>{})...);
                },
                packs_));
        }

        auto end()
        {
            auto impl = []<typename P, std::size_t... I>(P ip, std::index_sequence<I...>) {
                if constexpr (template_type<P, zipped_cont>) {
                    using std::end;
                    return end(ip.cont(), std::get<I>(ip.args())...);
                }
                else {
                    return ip.last();
                }
            };

            return iterator{std::apply(
                [&]<typename... Ts>(Ts&&... e) {
                    return std::make_tuple(impl(std::forward<Ts>(e),
                        std::make_index_sequence<
                            std::tuple_size_v<std::remove_cvref_t<decltype(std::forward<Ts>(e).args())>>>{})...);
                },
                packs_)};
        }

        auto rbegin()
        {
            auto impl = []<typename P, std::size_t... I>(P ip, std::index_sequence<I...>) {
                if constexpr (template_type<P, zipped_cont>) {
                    using std::rbegin;
                    return rbegin(ip.cont(), std::get<I>(ip.args())...);
                }
                else {
                    return ip.first();
                }
            };

            return reverse_iterator(std::apply(
                [&]<typename... Ts>(Ts&&... e) {
                    return std::make_tuple(impl(std::forward<Ts>(e),
                        std::make_index_sequence<
                            std::tuple_size_v<std::remove_cvref_t<decltype(std::forward<Ts>(e).args())>>>{})...);
                },
                packs_));
        }

        auto rend()
        {
            auto impl = []<typename P, std::size_t... I>(P ip, std::index_sequence<I...>) {
                if constexpr (template_type<P, zipped_cont>) {
                    using std::rend;
                    return rend(ip.cont(), std::get<I>(ip.args())...);
                }
                else {
                    return ip.last();
                }
            };

            return reverse_iterator{std::apply(
                [&]<typename... Ts>(Ts&&... e) {
                    return std::make_tuple(impl(std::forward<Ts>(e),
                        std::make_index_sequence<
                            std::tuple_size_v<std::remove_cvref_t<decltype(std::forward<Ts>(e).args())>>>{})...);
                },
                packs_)};
        }

    private:
        std::tuple<ItPack...> packs_;
    };

    // ---------

    template <arrnd_compliant Arrnd, typename Constraint>
    class arrnd_filter_proxy {
    public:
        constexpr arrnd_filter_proxy() = delete;

        explicit constexpr arrnd_filter_proxy(Arrnd arr_ref, Constraint constraint)
            : arr_ref_(arr_ref)
            , constraint_(constraint)
        { }

        [[nodiscard]] constexpr operator Arrnd() const
        {
            return arr_ref_.filter(constraint_);
        }

        [[nodiscard]] constexpr Arrnd operator()() const
        {
            return arr_ref_.filter(constraint_);
        }

        constexpr arrnd_filter_proxy(arrnd_filter_proxy&& other) = default;
        template <arrnd_compliant OtherArrnd, typename OtherConstraint>
        constexpr arrnd_filter_proxy& operator=(arrnd_filter_proxy<OtherArrnd, OtherConstraint>&& other)
        {
            // copy array elements from one proxy to another according to mask

            if (arr_ref_.empty()) {
                return *this;
            }

            // the user is responsible that the number of elements in both constraints is the same

            auto other_filtered = static_cast<OtherArrnd>(other);

            if constexpr (arrnd_compliant<Constraint>) {
                if constexpr (std::is_same_v<typename Constraint::value_type, bool>) {
                    assert(constraint_.header().dims() == arr_ref_.header().dims()
                        && "boolean constraint considered as mask");

                    typename Arrnd::indexer_type gen(arr_ref_.header());
                    typename OtherArrnd::indexer_type ogen(other_filtered.header());
                    typename Constraint::indexer_type cgen(constraint_.header());

                    for (; gen && cgen && ogen; ++gen, ++cgen) {
                        if (constraint_[*cgen]) {
                            arr_ref_[*gen] = other_filtered[*ogen];
                            ++ogen;
                        }
                    }

                } else {
                    typename Constraint::indexer_type gen(constraint_.header());
                    typename OtherArrnd::indexer_type ogen(other_filtered.header());

                    for (; gen && ogen; ++gen, ++ogen) {
                        arr_ref_[constraint_[*gen]] = other_filtered[*ogen];
                    }
                }
            } else { // might be predicator type
                typename Arrnd::indexer_type gen(arr_ref_.header());
                typename OtherArrnd::indexer_type ogen(other_filtered.header());

                for (; gen && ogen; ++gen) {
                    if (constraint_(arr_ref_[*gen])) {
                        arr_ref_[*gen] = other_filtered[*ogen];
                        ++ogen;
                    }
                }
            }

            (void)arrnd_filter_proxy<OtherArrnd, OtherConstraint>(std::move(other));

            return *this;
        }
        //constexpr arrnd_filter_proxy& operator=(arrnd_filter_proxy&& other) & = default;
        //constexpr arrnd_filter_proxy& operator=(arrnd_filter_proxy&& other) &&
        //{
        //    if (&other == this) {
        //        return *this;
        //    }

        //    copy_from(other, constraint_);
        //    (void)arrnd_filter_proxy(std::move(other));
        //    return *this;
        //}
        //template <arrnd_compliant ArCo>
        //constexpr arrnd_filter_proxy& operator=(ArCo&& other) &&
        //{
        //    arr_ref_.copy_from(other, constraint_);
        //    return *this;
        //}

        constexpr arrnd_filter_proxy(const arrnd_filter_proxy& other) = delete;
        constexpr arrnd_filter_proxy& operator=(const arrnd_filter_proxy& other) = delete;
        //constexpr arrnd_filter_proxy& operator=(const arrnd_filter_proxy& other) & = default;
        //constexpr arrnd_filter_proxy& operator=(const arrnd_filter_proxy& other) &&
        //{
        //    if (&other == this) {
        //        return *this;
        //    }

        //    copy_from(other, constraint_);
        //    return *this;
        //}

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator=(const ArCo& other) &&
        {
            arr_ref_.copy_from(other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator+=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) + other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator-=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) - other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator*=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) * other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator/=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) / other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator%=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) % other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator^=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) ^ other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator&=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) & other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator|=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) | other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator<<=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) << other, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator>>=(const ArCo& other) &&
        {
            arr_ref_.copy_from(arr_ref_.filter(constraint_) >> other, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator+=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ + value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator-=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ - value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator*=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ * value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator/=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ / value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator%=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ % value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator^=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ ^ value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator&=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ & value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator|=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ | value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator<<=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ << value, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator>>=(const U& value) &&
        {
            *this = arrnd_filter_proxy(arr_ref_ >> value, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator++() &&
        {
            auto c = arr_ref_.clone();
            *this = arrnd_filter_proxy(++c, constraint_);
            return *this;
        }

        template <arrnd_compliant ArCo>
        constexpr arrnd_filter_proxy& operator--() &&
        {
            auto c = arr_ref_.clone();
            *this = arrnd_filter_proxy(--c, constraint_);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        constexpr arrnd_filter_proxy& operator=(const U& value) &&
        {
            if (arr_ref_.empty()) {
                return *this;
            }

            if constexpr (arrnd_compliant<Constraint>) {
                if constexpr (std::is_same_v<typename Constraint::value_type, bool>) {
                    assert(constraint_.header().dims() == arr_ref_.header().dims()
                        && "boolean constraint considered as mask");

                    typename Arrnd::indexer_type gen(arr_ref_.header());
                    typename Constraint::indexer_type cgen(constraint_.header());

                    for (; gen && cgen; ++gen, ++cgen) {
                        if (constraint_[*cgen]) {
                            arr_ref_[*gen] = value;
                        }
                    }

                } else {
                    for (typename Constraint::indexer_type gen(constraint_.header()); gen; ++gen) {
                        arr_ref_[constraint_[*gen]] = value;
                    }
                }
            } else { // might be predicator type
                typename Arrnd::indexer_type gen(arr_ref_.header());

                for (; gen; ++gen) {
                    if (constraint_(arr_ref_[*gen])) {
                        arr_ref_[*gen] = value;
                    }
                }
            }

            return *this;
        }

        virtual constexpr ~arrnd_filter_proxy() = default;

    private:
        Arrnd arr_ref_;
        Constraint constraint_;
    };

    template <arrnd_compliant ArCo, signed_integral_type_iterator DimsIt>
    [[nodiscard]] inline constexpr auto zeros(DimsIt first_dim, DimsIt last_dim)
    {
        return ArCo(first_dim, last_dim, 0);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto zeros(const Cont& dims)
    {
        return zeros<ArCo>(std::begin(dims), std::end(dims));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto zeros(std::initializer_list<typename ArCo::size_type> dims)
    {
        return zeros<ArCo>(dims.begin(), dims.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto zeros(const U (&dims)[M])
    //{
    //    return zeros<ArCo>(std::begin(dims), std::end(dims));
    //}

    template <arrnd_compliant ArCo, signed_integral_type_iterator DimsIt>
    [[nodiscard]] inline constexpr auto eye(DimsIt first_dim, DimsIt last_dim)
    {
        auto ndims = std::distance(first_dim, last_dim);
        assert(ndims >= 2);

        auto eye_impl = [](typename ArCo::size_type r, typename ArCo::size_type c) {
            if (r == 0 || c == 0) {
                return ArCo();
            }
            ArCo res({r, c}, typename ArCo::value_type{0});
            assert(res.header().is_matrix());

            auto n = std::min(r, c);

            typename ArCo::size_type one_ind = 0;

            for (typename ArCo::size_type i = 0; i < n; ++i) {
                res[one_ind] = typename ArCo::value_type{1};
                one_ind += c + 1;
            }

            return res;
        };

        if (ndims == 2) {
            return eye_impl(*first_dim, *std::next(first_dim, 1));
        }

        ArCo res(first_dim, last_dim, typename ArCo::value_type{0});
        return res.template pageop<0>(2, [eye_impl](auto page) {
            return eye_impl(page.header().dims().front(), page.header().dims().back());
        });
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto eye(const Cont& dims)
    {
        return eye<ArCo>(std::begin(dims), std::end(dims));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto eye(std::initializer_list<typename ArCo::size_type> dims)
    {
        return eye<ArCo>(dims.begin(), dims.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto eye(const U (&dims)[M])
    //{
    //    return eye<ArCo>(std::begin(dims), std::end(dims));
    //}

    //template <typename T, /*random_access_type Storage = simple_dynamic_vector<T>*/typename StorageInfo = dynamic_storage_info<T>,
    //    template <typename> typename SharedRefAllocator = lightweight_allocator,
    //    arrnd_header_compliant Header = arrnd_header<>/*, template <typename> typename Indexer = arrnd_indexer,
    //    template <typename> typename Ranger = arrnd_axis_ranger*/>
    //template <typename T, /*random_access_type Storage = simple_dynamic_vector<T>*/typename StorageInfo = dynamic_storage_info<T>,
    //    arrnd_header_compliant Header = arrnd_header<>,
    //    template <typename> typename SharedRefAllocator = lightweight_allocator/*, template <typename> typename Indexer = arrnd_indexer,
    //    template <typename> typename Ranger = arrnd_axis_ranger*/>
    template <typename T, /*random_access_type Storage = simple_dynamic_vector<T>*/typename DataStorageInfo = dynamic_storage_info<T>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator/*, template <typename> typename Indexer = arrnd_indexer,
        template <typename> typename Ranger = arrnd_axis_ranger*/>
    class arrnd {
    public:
        using value_type = T;
        using size_type = /*std::int64_t*/std::make_signed_t<typename DataStorageInfo::storage_type::size_type>;
        using difference_type = /*std::int64_t*/size_type;
        using reference = T&;
        using const_reference = const T&;
        using pointer = T*;
        using const_pointer = const T*;

        using tag = arrnd_tag;

        using /*storage_info*/ data_storage_info = /*StorageInfo*/ DataStorageInfo;
        using dims_storage_info = DimsStorageInfo;

        using storage_type = /*Storage*/ typename /*StorageInfo*/ DataStorageInfo::storage_type;
        template <typename U>
        using shared_ref_allocator_type = SharedRefAllocator<U>;
        using header_type = /*Header*/ arrnd_header<DimsStorageInfo>;
        using indexer_type = /*Indexer*/ arrnd_indexer</*Header*/ header_type>;
        using ranger_type = /*Ranger*/ arrnd_axis_ranger</*Header*/ header_type>;

        using interval_type = typename header_type::interval_type;

        //using this_type = arrnd<T, /*Storage*/StorageInfo, SharedRefAllocator, Header/*, Indexer, Ranger*/>;
        //template <typename U>
        //using replaced_type = arrnd<U, /*typename Storage*/ typename StorageInfo::template replaced_type<U>,
        //    SharedRefAllocator, Header /*, Indexer, Ranger*/>;
        using this_type = arrnd<T, /*Storage*/ /*StorageInfo*/ DataStorageInfo,
            /*Header*/ /*header_type*/ DimsStorageInfo, SharedRefAllocator /*, Indexer, Ranger*/>;
        template <typename U>
        using replaced_type
            = arrnd<U, /*typename Storage*/ typename /*StorageInfo*/ DataStorageInfo::template replaced_type<U>,
                /*Header*/ /*header_type*/ DimsStorageInfo, SharedRefAllocator /*, Indexer, Ranger*/>;

        template <typename U, std::int64_t Level>
        using inner_replaced_type = inner_replaced_type_t<this_type, U, Level>;
        template <std::int64_t Level>
        using inner_this_type = arrnd_inner_t<this_type, Level>;
        template <std::int64_t Level>
        using inner_value_type = typename inner_this_type<Level>::value_type;

        template <typename U>
        using shared_ref = U;
        template <typename U>
        using maybe_shared_ref = U;

        using iterator = arrnd_iterator<this_type>;
        using const_iterator = arrnd_const_iterator<this_type>;
        using reverse_iterator = arrnd_reverse_iterator<this_type>;
        using const_reverse_iterator = arrnd_const_reverse_iterator<this_type>;

        using slice_iterator = arrnd_slice_iterator<this_type>;
        using const_slice_iterator = arrnd_slice_const_iterator<this_type>;
        using reverse_slice_iterator = arrnd_slice_reverse_iterator<this_type>;
        using const_reverse_slice_iterator = arrnd_slice_reverse_const_iterator<this_type>;

        constexpr static std::int64_t depth = calc_arrnd_depth<T>();
        constexpr static bool is_flat = depth == 0;

        template <std::int64_t Depth>
            requires(Depth >= 0)
        struct nested {
            using type = replaced_type<typename nested<Depth - 1>::type>;
        };
        template <>
        struct nested<0> {
            using type = this_type;
        };
        template <std::int64_t Depth>
        using nested_t = typename nested<Depth>::type;

        template <typename U, std::int64_t Level = this_type::depth>
        using tol_type = decltype(inner_value_type<Level>{} - U{});
        template <arrnd_compliant ArCo, std::int64_t Level = this_type::depth>
        using compliant_tol_type
            = decltype(inner_value_type<Level>{} - typename ArCo::template inner_value_type<Level>{});

        constexpr arrnd() = default;

        constexpr arrnd(arrnd&& other) = default;
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd(ArCo&& other)
        {
            set_from(other);
            (void)ArCo(std::move(other));
        }
        constexpr arrnd& operator=(arrnd&& other) & = default;
        constexpr arrnd& operator=(arrnd&& other) &&
        {
            if (&other == this) {
                return *this;
            }

            copy_from(other);
            (void)arrnd(std::move(other));
            return *this;
        }
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd& operator=(ArCo&& other) &
        {
            set_from(other);
            (void)ArCo(std::move(other));
            return *this;
        }
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd& operator=(ArCo&& other) &&
        {
            copy_from(other);
            (void)ArCo(std::move(other));
            return *this;
        }

        constexpr arrnd(const arrnd& other) = default;
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd(const ArCo& other)
        {
            set_from(other);
        }
        constexpr arrnd& operator=(const arrnd& other) & = default;
        constexpr arrnd& operator=(const arrnd& other) &&
        {
            if (&other == this) {
                return *this;
            }

            copy_from(other);
            return *this;
        }
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd& operator=(const ArCo& other) &
        {
            set_from(other);
            return *this;
        }
        template <arrnd_compliant ArCo>
            requires arrnd_depths_match<arrnd, ArCo>
        constexpr arrnd& operator=(const ArCo& other) &&
        {
            copy_from(other);
            return *this;
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
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

        template <signed_integral_type_iterator InputDimsIt, std::input_iterator InputDataIt>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim,
            const InputDataIt& first_data, const InputDataIt& last_data)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        {
            assert(last_data - first_data >= hdr_.numel());
            if (buffsp_) {
                std::copy_n(first_data, hdr_.numel(), buffsp_->data());
            }
        }
        template <signed_integral_type_iterable Cont, std::input_iterator InputDataIt>
        explicit constexpr arrnd(const Cont& dims, const InputDataIt& first_data, const InputDataIt& last_data)
            : arrnd(std::begin(dims), std::end(dims), first_data, last_data)
        { }
        template <std::input_iterator InputDataIt>
        explicit constexpr arrnd(
            std::initializer_list<size_type> dims, const InputDataIt& first_data, const InputDataIt& last_data)
            : arrnd(dims.begin(), dims.end(), first_data, last_data)
        { }
        //template <std::integral D, std::int64_t M, std::input_iterator InputDataIt>
        //explicit constexpr arrnd(const D (&dims)[M], const InputDataIt& first_data, const InputDataIt& last_data)
        //    : arrnd(std::begin(dims), std::end(dims), first_data, last_data)
        //{ }

        //template <signed_integral_type_iterator InputDimsIt>
        //explicit constexpr arrnd(
        //    const InputDimsIt& first_dim, const InputDimsIt& last_dim, std::initializer_list<value_type> data)
        //    : arrnd(first_dim, last_dim, data.begin(), data.end())
        //{ }
        //template <signed_integral_type_iterable Cont>
        //explicit constexpr arrnd(const Cont& dims, std::initializer_list<value_type> data)
        //    : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        //{ }
        //explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<value_type> data)
        //    : arrnd(dims.begin(), dims.end(), data.begin(), data.end())
        //{ }
        //template <std::integral D, std::int64_t M>
        //explicit constexpr arrnd(const D (&dims)[M], std::initializer_list<value_type> data)
        //    : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        //{ }

        template <signed_integral_type_iterator InputDimsIt, typename U>
        explicit constexpr arrnd(
            const InputDimsIt& first_dim, const InputDimsIt& last_dim, std::initializer_list<U> data)
            : arrnd(first_dim, last_dim, data.begin(), data.end())
        { }
        template <signed_integral_type_iterable Cont, typename U>
        explicit constexpr arrnd(const Cont& dims, std::initializer_list<U> data)
            : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        { }
        template <typename U>
        explicit constexpr arrnd(std::initializer_list<size_type> dims, std::initializer_list<U> data)
            : arrnd(dims.begin(), dims.end(), data.begin(), data.end())
        { }
        //template <std::integral D, std::int64_t M, typename U>
        //explicit constexpr arrnd(const D (&dims)[M], std::initializer_list<U> data)
        //    : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        //{ }

        template <signed_integral_type_iterator InputDimsIt, iterable DataCont>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const DataCont& data)
            : arrnd(first_dim, last_dim, std::begin(data), std::end(data))
        { }
        template <signed_integral_type_iterable Cont, iterable DataCont>
        explicit constexpr arrnd(const Cont& dims, const DataCont& data)
            : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        { }
        template <iterable DataCont>
        explicit constexpr arrnd(std::initializer_list<size_type> dims, const DataCont& data)
            : arrnd(dims.begin(), dims.end(), std::begin(data), std::end(data))
        { }

        //template <signed_integral_type_iterator InputDimsIt, std::int64_t N>
        //explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const value_type (&data)[N])
        //    : arrnd(first_dim, last_dim, std::begin(data), std::end(data))
        //{ }
        //template <signed_integral_type_iterable Cont, std::int64_t N>
        //explicit constexpr arrnd(const Cont& dims, const value_type (&data)[N])
        //    : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        //{ }
        //template <std::int64_t N>
        //explicit constexpr arrnd(std::initializer_list<size_type> dims, const value_type (&data)[N])
        //    : arrnd(dims.begin(), dims.end(), std::begin(data), std::end(data))
        //{ }
        //template <std::integral D, std::int64_t M, std::int64_t N>
        //explicit constexpr arrnd(const D (&dims)[M], const value_type (&data)[N])
        //    : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        //{ }

        //template <signed_integral_type_iterator InputDimsIt, typename U, std::int64_t N>
        //explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const U (&data)[N])
        //    : arrnd(first_dim, last_dim, std::begin(data), std::end(data))
        //{ }
        //template <signed_integral_type_iterable Cont, typename U, std::int64_t N>
        //explicit constexpr arrnd(const Cont& dims, const U (&data)[N])
        //    : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        //{ }
        //template <typename U, std::int64_t N>
        //explicit constexpr arrnd(std::initializer_list<size_type> dims, const U (&data)[N])
        //    : arrnd(dims.begin(), dims.end(), std::begin(data), std::end(data))
        //{ }
        //template <std::integral D, std::int64_t M, typename U, std::int64_t N>
        //explicit constexpr arrnd(const D (&dims)[M], const U (&data)[N])
        //    : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        //{ }

        template <signed_integral_type_iterator InputDimsIt>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        { }
        template <signed_integral_type_iterable Cont>
        explicit constexpr arrnd(const Cont& dims)
            : arrnd(std::begin(dims), std::end(dims))
        { }
        explicit constexpr arrnd(std::initializer_list<size_type> dims)
            : arrnd(dims.begin(), dims.end())
        { }
        //template <std::integral D, std::int64_t M>
        //explicit constexpr arrnd(const D (&dims)[M])
        //    : arrnd(std::begin(dims), std::end(dims))
        //{ }

        template <signed_integral_type_iterator InputDimsIt>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const_reference value)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        {
            if (buffsp_) {
                std::fill(buffsp_->begin(), buffsp_->end(), value);
            }
        }
        template <signed_integral_type_iterable Cont>
        explicit constexpr arrnd(const Cont& dims, const_reference value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }
        explicit constexpr arrnd(std::initializer_list<size_type> dims, const_reference value)
            : arrnd(dims.begin(), dims.end(), value)
        { }
        //template <std::integral D, std::int64_t M>
        //explicit constexpr arrnd(const D (&dims)[M], const_reference value)
        //    : arrnd(std::begin(dims), std::end(dims), value)
        //{ }

        template <signed_integral_type_iterator InputDimsIt, typename U>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const U& value)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        {
            if (buffsp_) {
                std::fill(buffsp_->begin(), buffsp_->end(), value);
            }
        }
        template <signed_integral_type_iterable Cont, typename U>
        explicit constexpr arrnd(const Cont& dims, const U& value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }
        template <typename U>
        explicit constexpr arrnd(std::initializer_list<size_type> dims, const U& value)
            : arrnd(dims.begin(), dims.end(), value)
        { }
        //template <std::integral D, std::int64_t M, typename U>
        //explicit constexpr arrnd(const D (&dims)[M], const U& value)
        //    : arrnd(std::begin(dims), std::end(dims), value)
        //{ }

        template <signed_integral_type_iterator InputDimsIt, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, Func&& func, Args&&... args)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        {
            if (buffsp_) {
                std::for_each(buffsp_->begin(), buffsp_->end(), [&func, &args...](auto& value) {
                    value = static_cast<value_type>(func(std::forward<Args>(args)...));
                });
            }
        }
        template <signed_integral_type_iterable Cont, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(const Cont& dims, Func&& func, Args&&... args)
            : arrnd(std::begin(dims), std::end(dims), std::forward<Func>(func), std::forward<Args>(args)...)
        { }
        template <typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(std::initializer_list<size_type> dims, Func&& func, Args&&... args)
            : arrnd(dims.begin(), dims.end(), std::forward<Func>(func), std::forward<Args>(args)...)
        { }
        //template <std::integral D, std::int64_t M, typename Func, typename... Args>
        //    requires(invocable_no_arrnd<Func, Args...>)
        //explicit constexpr arrnd(const D (&dims)[M], Func&& func, Args&&... args)
        //    : arrnd(std::begin(dims), std::end(dims), std::forward<Func>(func), std::forward<Args>(args)...)
        //{ }

        //explicit constexpr arrnd(const_reference value)
        //    : arrnd({1}, {value})
        //{ }
        //template <typename U>
        //explicit constexpr arrnd(const U& value)
        //    : arrnd({1}, {static_cast<value_type>(value)})
        //{ }

        template <typename... Args>
        [[nodiscard]] constexpr indexer_type indexer(Args&&... args) const
        {
            return indexer_type(hdr_, std::forward<Args>(args)...);
        }

        template <typename... Args>
        [[nodiscard]] constexpr ranger_type ranger(Args&&... args) const
        {
            return ranger_type(hdr_, std::forward<Args>(args)...);
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

        [[nodiscard]] constexpr const this_type* creator() const noexcept
        {
            return is_creator_valid_.expired() ? nullptr : creator_;
        }

        [[nodiscard]] explicit constexpr operator value_type() const noexcept
        {
            assert(hdr_.is_scalar());
            return (*this)[hdr_.offset()];
        }

        [[nodiscard]] constexpr shared_ref<this_type> as_pages() const
        {
            auto res = *this;
            res.treat_as_pages_ = true;
            return res;
        }

        [[nodiscard]] constexpr bool is_as_pages() const noexcept
        {
            return treat_as_pages_;
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

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr const_reference operator[](std::pair<InputIt, InputIt> subs) const noexcept
        {
            return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr const_reference operator[](const Cont& subs) const noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }
        [[nodiscard]] constexpr const_reference operator[](std::initializer_list<size_type> subs) const noexcept
        {
            return (*this)[std::make_pair(subs.begin(), subs.end())];
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr const_reference operator[](const U (&subs)[M]) const noexcept
        //{
        //    return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        //}

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr reference operator[](std::pair<InputIt, InputIt> subs) noexcept
        {
            return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr reference operator[](const Cont& subs) noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }
        [[nodiscard]] constexpr reference operator[](std::initializer_list<size_type> subs) noexcept
        {
            return (*this)[std::make_pair(subs.begin(), subs.end())];
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr reference operator[](const U (&subs)[M]) noexcept
        //{
        //    return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        //}

        template <interval_type_iterator InputIt>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](std::pair<InputIt, InputIt> ranges) const&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(ranges.first, ranges.second);
            slice.buffsp_ = buffsp_;
            slice.is_creator_valid_ = original_valid_creator_;
            slice.creator_ = this;
            return slice;
        }
        template <interval_type_iterator InputIt>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](std::pair<InputIt, InputIt> ranges) const&&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(ranges.first, ranges.second);
            slice.buffsp_ = buffsp_;
            return slice;
        }
        template <interval_type_iterable Cont>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](const Cont& ranges) const&
        {
            return (*this)[std::make_pair(std::cbegin(ranges), std::cend(ranges))];
        }
        template <interval_type_iterable Cont>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](const Cont& ranges) const&&
        {
            return std::move(*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
        }
        [[nodiscard]] constexpr shared_ref<this_type> operator[](
            std::initializer_list<interval_type> ranges) const&
        {
            return (*this)[std::make_pair(ranges.begin(), ranges.end())];
        }
        [[nodiscard]] constexpr shared_ref<this_type> operator[](
            std::initializer_list<interval_type> ranges) const&&
        {
            return std::move(*this)[std::make_pair(ranges.begin(), ranges.end())];
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr shared_ref<this_type> operator[](const interval<U> (&ranges)[M]) const&
        //{
        //    return (*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr shared_ref<this_type> operator[](const interval<U> (&ranges)[M]) const&&
        //{
        //    return std::move(*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
        //}

        [[nodiscard]] constexpr shared_ref<this_type> operator[](interval_type range) const&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range);
            slice.buffsp_ = buffsp_;
            slice.is_creator_valid_ = original_valid_creator_;
            slice.creator_ = this;
            return slice;
        }
        [[nodiscard]] constexpr shared_ref<this_type> operator[](interval_type range) const&&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range);
            slice.buffsp_ = buffsp_;
            return slice;
        }

        [[nodiscard]] constexpr shared_ref<this_type> operator()(interval_type range, size_type axis) const&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range, axis);
            slice.buffsp_ = buffsp_;
            slice.is_creator_valid_ = original_valid_creator_;
            slice.creator_ = this;
            return slice;
        }
        [[nodiscard]] constexpr shared_ref<this_type> operator()(interval_type range, size_type axis) const&&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range, axis);
            slice.buffsp_ = buffsp_;
            return slice;
        }

        // access relative array indices, might be slow for slices
        [[nodiscard]] constexpr const_reference operator()(size_type relative_index) const noexcept
        {
            assert(relative_index >= 0 && relative_index <= hdr_.numel());
            return hdr_.is_slice() ? buffsp_->data()[*(indexer() + relative_index)] : buffsp_->data()[relative_index];
        }
        [[nodiscard]] constexpr reference operator()(size_type relative_index) noexcept
        {
            assert(relative_index >= 0 && relative_index <= hdr_.numel());
            return hdr_.is_slice() ? buffsp_->data()[*(indexer() + relative_index)] : buffsp_->data()[relative_index];
        }

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto operator()(std::pair<InputIt, InputIt> indices) const
        {
            return std::move(*this)(replaced_type<size_type>(
                {std::distance(indices.first, indices.second)}, indices.first, indices.second));
        }
        template <signed_integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        [[nodiscard]] constexpr auto operator()(const Cont& indices) const
        {
            return (*this)(replaced_type<size_type>({std::ssize(indices)}, std::begin(indices), std::end(indices)));
        }
        /**
        * @note more strict function than filter. in case of logical type arrnd, its being treated as mask
        */
        template <arrnd_compliant ArCo>
            requires(std::integral<typename ArCo::value_type>)
        [[nodiscard]] constexpr auto operator()(const ArCo& selector) const
        {
            return arrnd_filter_proxy(*this, selector);
        }
        [[nodiscard]] constexpr auto operator()(std::initializer_list<size_type> indices) const
        {
            return (*this)(replaced_type<size_type>({std::ssize(indices)}, indices.begin(), indices.end()));
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto operator()(const U (&indices)[M]) const
        //{
        //    return (*this)(replaced_type<size_type>({M}, std::begin(indices), std::end(indices)));
        //}

        [[nodiscard]] constexpr auto operator()(arrnd_shape_preset shape) const
        {
            return reshape/*<0>*/(shape);
        }

        template <typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, value_type, Args...>
        [[nodiscard]] constexpr auto operator()(Pred&& pred, Args&&... args) const
        {
            //auto selector = std::bind(std::forward<Pred>(pred), std::placeholders::_1, std::forward<Args>(args)...);
            auto selector = [&pred, &args...](const value_type& value) {
                return pred(value, std::forward<Args>(args)...);
            };

            /*    auto selector = [&args](const value_type& vt) {
                    return
            };*/

            return arrnd_filter_proxy(*this, selector);
        }

        [[nodiscard]] constexpr bool empty() const noexcept
        {
            return hdr_.empty() && (hdr_.is_slice() || !buffsp_);
        }

        /**
        * @note no reallocation to dst
        */
        template <arrnd_compliant ArCo>
        constexpr const this_type& copy_to(ArCo&& dst) const
        {
            if (empty() || dst.empty()) {
                return *this;
            }

            indexer_type gen(hdr_);
            typename std::remove_cvref_t<ArCo>::indexer_type dst_gen(dst.header());

            for (; gen && dst_gen; ++gen, ++dst_gen) {
                if constexpr (arrnd_compliant<value_type>) {
                    (*this)[*gen].copy_to(dst[*dst_gen]); // deep copying
                } else {
                    dst[*dst_gen] = (*this)[*gen];
                }
            }

            return *this;
        }

        template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
        constexpr const this_type& copy_to(ArCo&& dst, std::pair<InputIt, InputIt> indices) const
        {
            if (empty() || dst.empty() || std::distance(indices.first, indices.second) <= 0) {
                return *this;
            }

            indexer_type gen(hdr_);
            auto inds_it = indices.first;

            for (; gen && inds_it != indices.second; ++gen, ++inds_it) {
                if constexpr (arrnd_compliant<value_type>) {
                    (*this)[*gen].copy_to(dst[*inds_it]); // deep copying
                } else {
                    dst[*inds_it] = (*this)[*gen];
                }
            }

            return *this;
        }
        template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        constexpr const this_type& copy_to(ArCo&& dst, const Cont& indices) const
        {
            return copy_to(std::forward<ArCo>(dst), std::make_pair(std::begin(indices), std::end(indices)));
        }
        template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
            requires(std::integral<typename ArCo2::value_type>)
        constexpr const this_type& copy_to(ArCo1&& dst, const ArCo2& selector) const
        {
            // in case that indices isn't a vector treat it as a mask
            if constexpr (std::is_same_v<bool, typename ArCo2::value_type>) {
                if (empty() || dst.empty() || selector.empty()) {
                    return *this;
                }

                assert(dst.header().dims() == selector.header().dims());

                indexer_type gen(hdr_);
                typename std::remove_cvref_t<ArCo1>::indexer_type dst_gen(dst.header());
                typename ArCo2::indexer_type slc_gen(selector.header());

                for (; gen && dst_gen && slc_gen; ++dst_gen, ++slc_gen) {
                    if (selector[*slc_gen]) {
                        if constexpr (arrnd_compliant<value_type>) {
                            (*this)[*gen].copy_to(dst[*dst_gen]); // deep copying
                        } else {
                            dst[*dst_gen] = (*this)[*gen];
                        }
                        ++gen;
                    }
                }

                return *this;
            } else {
                return copy_to(std::forward<ArCo1>(dst), std::make_pair(std::begin(selector), std::end(selector)));
            }
        }
        template <arrnd_compliant ArCo>
        constexpr const this_type& copy_to(ArCo&& dst, std::initializer_list<size_type> indices) const
        {
            return copy_to(std::forward<ArCo>(dst), std::make_pair(indices.begin(), indices.end()));
        }
        //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        //constexpr const this_type& copy_to(ArCo&& dst, const U (&indices)[M]) const
        //{
        //    return copy_to(std::forward<ArCo>(dst), std::make_pair(std::begin(indices), std::end(indices)));
        //}

        template <arrnd_compliant ArCo, typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, value_type, Args...>
        constexpr const this_type& copy_to(ArCo&& dst, Pred&& pred, Args&&... args) const
        {
            if (empty() || dst.empty()) {
                return *this;
            }

            indexer_type gen(hdr_);
            typename std::remove_cvref_t<ArCo>::indexer_type dst_gen(dst.header());

            for (; gen && dst_gen; ++dst_gen) {
                if (pred(dst[*dst_gen])) {
                    if constexpr (arrnd_compliant<value_type>) {
                        (*this)[*gen].copy_to(dst[*dst_gen]); // deep copying
                    } else {
                        dst[*dst_gen] = (*this)[*gen];
                    }
                    ++gen;
                }
            }

            return *this;
        }

        template <arrnd_compliant ArCo, interval_type_iterator InputIt>
        constexpr const this_type& copy_to(ArCo&& dst, const InputIt& first_range, const InputIt& last_range) const
        {
            copy_to(dst[std::make_pair(first_range, last_range)]);
            return *this;
        }
        template <arrnd_compliant ArCo, interval_type_iterable Cont>
        constexpr const this_type& copy_to(ArCo&& dst, const Cont& ranges) const
        {
            return copy_to(std::forward<ArCo>(dst), std::begin(ranges), std::end(ranges));
        }
        template <arrnd_compliant ArCo>
        constexpr const this_type& copy_to(ArCo&& dst, std::initializer_list<interval_type> ranges) const
        {
            return copy_to(std::forward<ArCo>(dst), ranges.begin(), ranges.end());
        }
        //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        //constexpr const this_type& copy_to(ArCo&& dst, const interval<U> (&ranges)[M]) const
        //{
        //    return copy_to(std::forward<ArCo>(dst), std::begin(ranges), std::end(ranges));
        //}

        template <arrnd_compliant ArCo>
        constexpr const this_type& set_to(ArCo& dst) const
        {
            if (empty()) {
                dst = ArCo{};
                return *this;
            }

            if (hdr_.numel() == dst.header().numel()) {
                if (hdr_.dims() != dst.header().dims()) {
                    dst.header() = header_type{hdr_.dims().cbegin(), hdr_.dims().cend()};
                }
                return copy_to(dst);
            }

            dst = ArCo{hdr_.dims().cbegin(), hdr_.dims().cend()};
            if constexpr (arrnd_compliant<value_type>) {
                indexer_type gen(hdr_);
                typename std::remove_cvref_t<ArCo>::indexer_type dst_gen(dst.header());

                for (; gen && dst_gen; ++gen, ++dst_gen) {
                    (*this)[*gen].set_to(dst[*dst_gen]);
                }
            }
            return copy_to(dst);
        }

        template <arrnd_compliant ArCo>
        constexpr this_type& copy_from(const ArCo& src)
        {
            src.copy_to(*this);
            return *this;
        }

        template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
        constexpr this_type& copy_from(const ArCo& src, std::pair<InputIt, InputIt> indices)
        {
            src.copy_to(*this, indices);
            return *this;
        }
        template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        constexpr this_type& copy_from(const ArCo& src, const Cont& indices)
        {
            return copy_from(src, std::make_pair(std::begin(indices), std::end(indices)));
        }
        template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
            requires(std::integral<typename ArCo2::value_type>)
        constexpr this_type& copy_from(const ArCo1& src, const ArCo2& selector)
        {
            src.copy_to(*this, selector);
            return *this;
        }
        template <arrnd_compliant ArCo, std::integral U = size_type>
        constexpr this_type& copy_from(const ArCo& src, std::initializer_list<U> indices)
        {
            return copy_from(src, std::make_pair(indices.begin(), indices.end()));
        }
        //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        //constexpr this_type& copy_from(const ArCo& src, const U (&indices)[M])
        //{
        //    return copy_from(src, std::make_pair(std::begin(indices), std::end(indices)));
        //}

        template <arrnd_compliant ArCo, typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, value_type, Args...>
        constexpr const this_type& copy_from(const ArCo& src, Pred&& pred, Args&&... args)
        {
            src.copy_to(*this, std::forward<Pred>(pred), std::forward<Args>(args)...);
            return *this;
        }

        template <arrnd_compliant ArCo, interval_type_iterator InputIt>
        constexpr this_type& copy_from(const ArCo& src, const InputIt& first_range, const InputIt& last_range)
        {
            src.copy_to(*this, first_range, last_range);
            return *this;
        }
        template <arrnd_compliant ArCo, interval_type_iterable Cont>
        constexpr this_type& copy_from(const ArCo& src, const Cont& ranges)
        {
            return copy_from(src, std::begin(ranges), std::end(ranges));
        }
        template <arrnd_compliant ArCo>
        constexpr this_type& copy_from(const ArCo& src, std::initializer_list<interval_type> ranges)
        {
            return copy_from(src, ranges.begin(), ranges.end());
        }
        //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        //constexpr this_type& copy_from(const ArCo& src, const interval<U> (&ranges)[M])
        //{
        //    return copy_from(src, std::begin(ranges), std::end(ranges));
        //}

        template <arrnd_compliant ArCo>
        constexpr this_type& set_from(const ArCo& src)
        {
            src.set_to(*this);
            return *this;
        }

        [[nodiscard]] constexpr this_type clone() const
        {
            this_type clone;
            set_to(clone);
            return clone;
        }

        template </*std::int64_t Level, */signed_integral_type_iterator InputIt>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            typename this_type::header_type new_header(first_new_dim, last_new_dim);
            assert(hdr_.numel() == new_header.numel());

            if (hdr_.dims() == new_header.dims()) {
                return *this;
            }

            if (hdr_.is_slice()) {
                return clone().reshape(first_new_dim, last_new_dim);
                //return resize<Level>(first_new_dim, last_new_dim);
            }

            this_type res(*this);
            res.hdr_ = std::move(new_header);

            return res;
        }
        //template <std::int64_t Level, signed_integral_type_iterator InputIt>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
        //    const InputIt& first_new_dim, const InputIt& last_new_dim) const
        //{
        //    if (empty()) {
        //        return *this;
        //    }

        //    this_type res(hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);

        //    for (; gen && res_gen; ++gen, ++res_gen) {
        //        res[*res_gen] = (*this)[*gen].template reshape<Level - 1, InputIt>(first_new_dim, last_new_dim);
        //    }

        //    return res;
        //}
        //template <signed_integral_type_iterator InputIt>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
        //    const InputIt& first_new_dim, const InputIt& last_new_dim) const
        //{
        //    return reshape<this_type::depth>(first_new_dim, last_new_dim);
        //}
        //template <std::int64_t Level, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const Cont& new_dims) const
        //{
        //    return reshape<Level>(std::begin(new_dims), std::end(new_dims));
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const Cont& new_dims) const
        {
            return reshape/*<this_type::depth, Cont>*/(std::begin(new_dims), std::end(new_dims));
        }
        //template <std::int64_t Level>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<size_type> new_dims) const
        //{
        //    return reshape<Level>(new_dims.begin(), new_dims.end());
        //}
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<size_type> new_dims) const
        {
            return reshape/*<this_type::depth>*/(new_dims.begin(), new_dims.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const U (&new_dims)[M]) const
        //{
        //    return reshape<Level>(std::begin(new_dims), std::end(new_dims));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const U (&new_dims)[M]) const
        //{
        //    return reshape<this_type::depth>(std::begin(new_dims), std::end(new_dims));
        //}
        //template <std::int64_t Level>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape_preset shape) const
        //{
        //    if (empty()) {
        //        return *this;
        //    }

        //    this_type res(hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);

        //    for (; gen && res_gen; ++gen, ++res_gen) {
        //        res[*res_gen] = (*this)[*gen].template reshape<Level - 1>(shape);
        //    }

        //    return res;
        //}
        //template <std::int64_t Level>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape_preset shape) const
        {
            if (empty()) {
                return *this;
            }

            switch (shape) {
            case arrnd_shape_preset::vector:
                return reshape/*<Level>*/({hdr_.numel()});
            case arrnd_shape_preset::row:
                return reshape/*<Level>*/({size_type{1}, hdr_.numel()});
            case arrnd_shape_preset::column:
                return reshape/*<Level>*/({hdr_.numel(), size_type{1}});
            default:
                assert(false && "unknown arrnd_shape_preset value");
                return this_type();
            }
        }
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape_preset shape) const
        //{
        //    return reshape<this_type::depth>(shape);
        //}

        template </*std::int64_t Level, */signed_integral_type_iterator InputIt>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            if (std::equal(hdr_.dims().cbegin(), hdr_.dims().cend(), first_new_dim, last_new_dim)) {
                return *this;
            }

            if (empty()) {
                return this_type(first_new_dim, last_new_dim);
            }

            this_type res(first_new_dim, last_new_dim);

            size_type n = std::min({std::distance(first_new_dim, last_new_dim), hdr_.dims().size()});

            typename header_type::storage_type::template replaced_type<interval_type> prev_ranges(
                hdr_.dims().size());
            std::fill(prev_ranges.begin(), prev_ranges.end(), interval_type::full());

            typename header_type::storage_type::template replaced_type<interval_type> new_ranges(
                std::distance(first_new_dim, last_new_dim));
            std::fill(new_ranges.begin(), new_ranges.end(), interval_type::full());

            size_type pi = hdr_.dims().size() - 1;
            size_type ni = std::distance(first_new_dim, last_new_dim) - 1;
            for (int i = n - 1; i >= 0; --i) {
                size_type prev_dim = *std::next(hdr_.dims().cbegin(), pi);
                size_type new_dim = *std::next(first_new_dim, ni);
                *std::next(prev_ranges.begin(), pi--) = interval_type::to(std::min({prev_dim, new_dim}));
                *std::next(new_ranges.begin(), ni--) = interval_type::to(std::min({prev_dim, new_dim}));
            }

            auto sthis = (*this)[prev_ranges];
            auto sres = res[new_ranges];

            indexer_type gen(sthis.header());
            indexer_type res_gen(sres.header());

            while (gen && res_gen) {
                sres[*res_gen] = sthis[*gen];
                ++gen;
                ++res_gen;
            }

            return res;
        }
        //template <std::int64_t Level, signed_integral_type_iterator InputIt>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(
        //    const InputIt& first_new_dim, const InputIt& last_new_dim) const
        //{
        //    if (empty()) {
        //        return *this;
        //    }

        //    this_type res(hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);

        //    for (; gen && res_gen; ++gen, ++res_gen) {
        //        res[*res_gen] = (*this)[*gen].template resize<Level - 1, InputIt>(first_new_dim, last_new_dim);
        //    }

        //    return res;
        //}
        //template <signed_integral_type_iterator InputIt>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(
        //    const InputIt& first_new_dim, const InputIt& last_new_dim) const
        //{
        //    return resize<this_type::depth>(first_new_dim, last_new_dim);
        //}
        //template <std::int64_t Level, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const Cont& new_dims) const
        //{
        //    return resize<Level>(std::begin(new_dims), std::end(new_dims));
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const Cont& new_dims) const
        {
            return resize/*<this_type::depth, Cont>*/(std::begin(new_dims), std::end(new_dims));
        }
        //template <std::int64_t Level>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<size_type> new_dims) const
        //{
        //    return resize<Level>(new_dims.begin(), new_dims.end());
        //}
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<size_type> new_dims) const
        {
            return resize/*<this_type::depth>*/(new_dims.begin(), new_dims.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const U (&new_dims)[M]) const
        //{
        //    return resize<Level>(std::begin(new_dims), std::end(new_dims));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const U (&new_dims)[M]) const
        //{
        //    return resize<this_type::depth>(std::begin(new_dims), std::end(new_dims));
        //}

        template </*std::int64_t Level, */arrnd_compliant ArCo>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr) const
        {
            return insert</*Level, */ArCo>(arr, hdr_.numel());
        }
        //template <arrnd_compliant ArCo>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr) const
        //{
        //    return append<this_type::depth, ArCo>(arr);
        //}

        template </*std::int64_t Level, */arrnd_compliant ArCo>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, size_type axis) const
        {
            size_type ind = empty() ? size_type{0} : hdr_.dims()[axis];
            return insert</*Level, */ArCo>(arr, ind, axis);
        }
        //template <arrnd_compliant ArCo>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, size_type axis) const
        //{
        //    return append<this_type::depth, ArCo>(arr, axis);
        //}

        template </*std::int64_t Level, */arrnd_compliant ArCo, arrnd_compliant... ArCos>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, ArCos&&... others) const
        {
            return append</*Level, */ArCo>(arr).template append<ArCos...>(std::forward<ArCos>(others)...);
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(Tuple&& arr_axis_tuple) const
        {
            return append/*<Level>*/(std::get<0>(arr_axis_tuple), std::get<1>(arr_axis_tuple));
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple, typename... ArCoAxisTuples>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(
            Tuple&& arr_axis_tuple, ArCoAxisTuples&&... others) const
        {
            return append/*<Level>*/(std::get<0>(arr_axis_tuple), std::get<1>(arr_axis_tuple))
                .template append<ArCoAxisTuples...>(std::forward<ArCoAxisTuples>(others)...);
        }
        //template <arrnd_compliant ArCo, arrnd_compliant... ArCos>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, ArCos&&... others) const
        //{
        //    return append<this_type::depth, ArCo, ArCos...>(arr, std::forward<ArCos>(others)...);
        //}
        //template <template_type<std::tuple> Tuple, typename... ArCoAxisTuples>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> append(
        //    Tuple&& arr_axis_tuple, ArCoAxisTuples&&... others) const
        //{
        //    return append<this_type::depth, Tuple, ArCoAxisTuples...>(
        //        std::forward<Tuple>(arr_axis_tuple), std::forward<ArCoAxisTuples>(others)...);
        //}

        template </*std::int64_t Level, */ arrnd_compliant ArCo, arrnd_compliant... ArCos>
        [[nodiscard]] constexpr auto matmul(const ArCo& arr, ArCos&&... others) const
        {
            return matmul(arr).matmul(std::forward<ArCos>(others)...);
        }
        //template <arrnd_compliant... ArCos>
        //[[nodiscard]] constexpr auto matmul(ArCos&&... others) const
        //{
        //    return matmul<this_type::depth>(std::forward<ArCos>(others)...);
        //}

        template </*std::int64_t Level, */arrnd_compliant ArCo>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind) const
        {
            if (empty()) {
                return arr./*template */reshape/*<Level>*/({arr.header().numel()}).clone();
            }

            if (arr.empty()) {
                return *this;
            }

            assert(ind >= 0 && ind <= hdr_.numel());

            this_type res({hdr_.numel() + arr.header().numel()});

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (size_type i = 0; i < ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen];
            }
            for (size_type i = 0; i < arr.header().numel() && arr_gen && res_gen; ++i, ++arr_gen, ++res_gen) {
                res[*res_gen] = arr[*arr_gen];
            }
            for (size_type i = 0; i < hdr_.numel() - ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen];
            }

            return res;
        }
        //template <std::int64_t Level, arrnd_compliant ArCo>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind) const
        //{
        //    this_type res(empty() ? arr.header().dims() : hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);
        //    typename ArCo::indexer_type arr_gen(arr.header());

        //    for (; gen && res_gen && arr_gen; ++gen, ++res_gen, ++arr_gen) {
        //        res[*res_gen] = (*this)[*gen].template insert<Level - 1, typename ArCo::value_type>(arr[*arr_gen], ind);
        //    }

        //    return res;
        //}
        //template <arrnd_compliant ArCo>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind) const
        //{
        //    return insert<this_type::depth, ArCo>(arr, ind);
        //}

        template </*std::int64_t Level, */arrnd_compliant ArCo>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind, size_type axis) const
        {
            if (empty()) {
                this_type res(arr);
                return res.clone();
            }

            if (arr.empty()) {
                return clone(); //*this;
            }

            //header_type new_header(hdr_.subheader(arr.header().dims()[axis], axis));
            //if (new_header.empty()) {
            //    return this_type();
            //}

            assert(ind >= 0 && ind <= hdr_.dims()[axis]);

            this_type res({hdr_.numel() + arr.header().numel()});
            //res.hdr_ = std::move(new_header);
            res.hdr_ = header_type(hdr_.dims_with_modified_axis(axis, *std::next(arr.header().dims().cbegin(), axis)));

            indexer_type gen(hdr_, axis);
            typename ArCo::indexer_type arr_gen(arr.header(), axis);
            indexer_type res_gen(res.hdr_, axis);

            size_type cycle = ind
                * (std::reduce(res.hdr_.dims().begin(), res.hdr_.dims().end(), size_type{1}, std::multiplies<>{})
                    / res.hdr_.dims()[axis]);

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
        //template <std::int64_t Level, arrnd_compliant ArCo>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind, size_type axis) const
        //{
        //    this_type res(empty() ? arr.header().dims() : hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);
        //    typename ArCo::indexer_type arr_gen(arr.header());

        //    for (; gen && res_gen && arr_gen; ++gen, ++res_gen, ++arr_gen) {
        //        res[*res_gen]
        //            = (*this)[*gen].template insert<Level - 1, typename ArCo::value_type>(arr[*arr_gen], ind, axis);
        //    }

        //    return res;
        //}
        //template <arrnd_compliant ArCo>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, size_type ind, size_type axis) const
        //{
        //    return insert<this_type::depth, ArCo>(arr, ind, axis);
        //}

        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
            requires(std::tuple_size_v<Tuple> == 2)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(Tuple&& arr_ind_tuple) const
        {
            return insert/*<Level>*/(std::get<0>(arr_ind_tuple), std::get<1>(arr_ind_tuple));
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple, typename... ArCoIndexTuples>
            requires(std::tuple_size_v<Tuple> == 2)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(
            Tuple&& arr_ind_tuple, ArCoIndexTuples&&... others) const
        {
            return insert/*<Level>*/(std::get<0>(arr_ind_tuple), std::get<1>(arr_ind_tuple))
                .template insert<ArCoIndexTuples...>(std::forward<ArCoIndexTuples>(others)...);
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
            requires(std::tuple_size_v<Tuple> == 3)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(Tuple&& arr_ind_axis_tuple) const
        {
            return insert/*<Level>*/(
                std::get<0>(arr_ind_axis_tuple), std::get<1>(arr_ind_axis_tuple), std::get<2>(arr_ind_axis_tuple));
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple, typename... ArCoIndexAxisTuples>
            requires(std::tuple_size_v<Tuple> == 3)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(
            Tuple&& arr_ind_axis_tuple, ArCoIndexAxisTuples&&... others) const
        {
            return insert/*<Level>*/(
                std::get<0>(arr_ind_axis_tuple), std::get<1>(arr_ind_axis_tuple), std::get<2>(arr_ind_axis_tuple))
                .template insert<ArCoIndexAxisTuples...>(std::forward<ArCoIndexAxisTuples>(others)...);
        }
        //template <typename... Tuples>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> insert(Tuples&&... tuples) const
        //{
        //    return insert<this_type::depth>(std::forward<Tuples>(tuples)...);
        //}

        //template <std::int64_t Level>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(size_type count) const
        {
            assert(count >= 0);

            auto res = *this;

            for (size_type i = 0; i < count - 1; ++i) {
                res = res./*template */append/*<Level>*/(*this);
            }

            return res;
        }
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(size_type count) const
        //{
        //    return repeat<this_type::depth>(count);
        //}


        template </*std::int64_t Level, */iterator_of_template_type<std::tuple> InputIt>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(InputIt first_tuple, InputIt last_tuple) const
        {
            assert(std::distance(first_tuple, last_tuple) >= 0);

            auto res = *this;
            auto mid = res;

            std::for_each(first_tuple, last_tuple, [&res, &mid](const auto& tuple) {
                for (size_type i = 0; i < std::get<0>(tuple) - 1; ++i) {
                    res = res./*template */append/*<Level>*/(mid, std::get<1>(tuple));
                }
                mid = res;
            });

            return res;
        }
        //template <iterator_of_template_type<std::tuple> InputIt>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(InputIt first_tuple, InputIt last_tuple) const
        //{
        //    return repeat<this_type::depth>(first_tuple, last_tuple);
        //}
        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(std::initializer_list<Tuple> count_axis_tuples) const
        {
            return repeat/*<Level>*/(count_axis_tuples.begin(), count_axis_tuples.end());
        }
        //template <template_type<std::tuple> Tuple>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(std::initializer_list<Tuple> count_axis_tuples) const
        //{
        //    return repeat<this_type::depth>(count_axis_tuples.begin(), count_axis_tuples.end());
        //}
        template </*std::int64_t Level, */iterable_of_template_type<std::tuple> Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const Cont& count_axis_tuples) const
        {
            return repeat/*<Level>*/(std::begin(count_axis_tuples), std::end(count_axis_tuples));
        }
        //template <iterable_of_template_type<std::tuple> Cont>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const Cont& count_axis_tuples) const
        //{
        //    return repeat<this_type::depth>(std::begin(count_axis_tuples), std::end(count_axis_tuples));
        //}
        //template <std::int64_t Level, template_type<std::tuple> U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const U (&count_axis_tuples)[M]) const
        //{
        //    return repeat<Level>(std::begin(count_axis_tuples), std::end(count_axis_tuples));
        //}
        //template <template_type<std::tuple> U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const U (&count_axis_tuples)[M]) const
        //{
        //    return repeat<this_type::depth>(std::begin(count_axis_tuples), std::end(count_axis_tuples));
        //}




        template </*std::int64_t Level, */signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(InputIt first_rep, InputIt last_rep) const
        {
            assert(std::distance(first_rep, last_rep) <= hdr_.dims().size());

            auto nreps = std::distance(first_rep, last_rep);

            this_type::template replaced_type<size_type> reps({nreps}, first_rep, last_rep);
            this_type::template replaced_type<size_type> axes({nreps});
            std::iota(axes.begin(), axes.end(), hdr_.dims().size() - nreps);

            auto z = zip(zipped_cont(reps), zipped_cont(axes));

            auto res = *this;
            auto mid = res;

            std::for_each(z.begin(), z.end(), [&res, &mid](const auto& tuple) {
                for (size_type i = 0; i < std::get<0>(tuple) - 1; ++i) {
                    res = res./*template */append/*<Level>*/(mid, std::get<1>(tuple));
                }
                mid = res;
            });

            return res;
        }
        //template <signed_integral_type_iterator InputIt>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(InputIt first_rep, InputIt last_rep) const
        //{
        //    return repeat<this_type::depth>(first_rep, last_rep);
        //}
        //template <std::int64_t Level>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(std::initializer_list<size_type> reps) const
        {
            return repeat/*<Level>*/(reps.begin(), reps.end());
        }
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(std::initializer_list<size_type> reps) const
        //{
        //    return repeat<this_type::depth>(reps.begin(), reps.end());
        //}
        template </*std::int64_t Level, */signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const Cont& reps) const
        {
            return repeat/*<Level>*/(std::begin(reps), std::end(reps));
        }
        //template <signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const Cont& reps) const
        //{
        //    return repeat<this_type::depth>(std::begin(reps), std::end(reps));
        //}
        //template <std::int64_t Level, std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const U (&reps)[M]) const
        //{
        //    return repeat<Level>(std::begin(reps), std::end(reps));
        //}
        //template <std::signed_integral U, std::int64_t M>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(const U (&reps)[M]) const
        //{
        //    return repeat<this_type::depth>(std::begin(reps), std::end(reps));
        //}




        //[[nodiscard]] constexpr maybe_shared_ref<this_type> repeat(std::initializer_list<size_type> axes) const
        //{
        //    auto res = *this;
        //    std::for_each(axes.begin(), axes.end(), [&](const auto& axis) {
        //        res = res.append(res, axis);
        //        });
        //    return res;
        //}

        //template <std::int64_t Level>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count) const
        {
            if (empty()) {
                return *this;
            }

            assert(ind >= 0 && ind < hdr_.numel());
            assert(ind + count <= hdr_.numel());

            this_type res({hdr_.numel() - count});

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (size_type i = 0; i < ind && gen && res_gen; ++i, ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen];
            }
            gen += count;
            for (size_type i = ind + count; i < hdr_.numel() && gen && res_gen; ++i, ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen];
            }

            return res;
        }
        //template <std::int64_t Level>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count) const
        //{
        //    if (empty()) {
        //        return *this;
        //    }

        //    this_type res(hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);

        //    for (; gen && res_gen; ++gen, ++res_gen) {
        //        res[*res_gen] = (*this)[*gen].template remove<Level - 1>(ind, count);
        //    }

        //    return res;
        //}
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count) const
        //{
        //    return remove<this_type::depth>(ind, count);
        //}

        //template <std::int64_t Level>
            //requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count, size_type axis) const
        {
            if (empty()) {
                return *this;
            }

            //header_type new_header(hdr_.subheader(-count, axis));

            assert(ind >= 0 && ind < hdr_.dims()[axis]);
            assert(ind + count <= hdr_.dims()[axis]);

            //if (new_header.empty()) {
            //    return this_type();
            //}

            this_type res({hdr_.numel() - (hdr_.numel() / hdr_.dims()[axis]) * count});
            //res.hdr_ = std::move(new_header);
            res.hdr_ = header_type(hdr_.dims_with_modified_axis(axis, -count));

            indexer_type gen(hdr_, axis);
            indexer_type res_gen(res.hdr_, axis);

            size_type cycle = ind
                * (std::reduce(res.hdr_.dims().begin(), res.hdr_.dims().end(), size_type{1}, std::multiplies<>{})
                    / res.hdr_.dims()[axis]);

            size_type removals = hdr_.numel() - res.hdr_.numel();

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
        //template <std::int64_t Level>
        //    requires(Level > 0)
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count, size_type axis) const
        //{
        //    if (empty()) {
        //        return *this;
        //    }

        //    this_type res(hdr_.dims());

        //    indexer_type gen(hdr_);
        //    indexer_type res_gen(res.hdr_);

        //    for (; gen && res_gen; ++gen, ++res_gen) {
        //        res[*res_gen] = (*this)[*gen].template remove<Level - 1>(ind, count, axis);
        //    }

        //    return res;
        //}
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> remove(size_type ind, size_type count, size_type axis) const
        //{
        //    return remove<this_type::depth>(ind, count, axis);
        //}

        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
            requires(std::tuple_size_v<Tuple> == 2)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(Tuple&& ind_count_tuple) const
        {
            return remove/*<Level>*/(std::get<0>(ind_count_tuple), std::get<1>(ind_count_tuple));
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple, typename... IndexCountTuples>
            requires(std::tuple_size_v<Tuple> == 2)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(
            Tuple&& ind_count_tuple, IndexCountTuples&&... others) const
        {
            return remove/*<Level>*/(std::get<0>(ind_count_tuple), std::get<1>(ind_count_tuple))
                .template remove<IndexCountTuples...>(std::forward<IndexCountTuples>(others)...);
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple>
            requires(std::tuple_size_v<Tuple> == 3)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(Tuple&& ind_count_axis_tuple) const
        {
            return remove/*<Level>*/(std::get<0>(ind_count_axis_tuple), std::get<1>(ind_count_axis_tuple),
                std::get<2>(ind_count_axis_tuple));
        }
        template </*std::int64_t Level, */template_type<std::tuple> Tuple, typename... IndexCountAxisTuples>
            requires(std::tuple_size_v<Tuple> == 3)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(
            Tuple&& ind_count_axis_tuple, IndexCountAxisTuples&&... others) const
        {
            return remove/*<Level>*/(
                std::get<0>(ind_count_axis_tuple), std::get<1>(ind_count_axis_tuple), std::get<2>(ind_count_axis_tuple))
                .template remove<IndexCountAxisTuples...>(std::forward<IndexCountAxisTuples>(others)...);
        }
        //template <typename... Tuples>
        //[[nodiscard]] constexpr maybe_shared_ref<this_type> remove(Tuples&&... tuples) const
        //{
        //    return remove<this_type::depth>(std::forward<Tuples>(tuples)...);
        //}

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto transform(Func&& func, Args&&... args) const
        {
            using transformed_type
                = inner_replaced_type<std::invoke_result_t<Func, inner_value_type<Level>, Args...>, Level>;

            if (empty()) {
                return transformed_type();
            }

            transformed_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            typename transformed_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template transform<Level - 1, Func, Args...>(
                    std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto transform(Func&& func, Args&&... args) const
        {
            using transformed_type
                = inner_replaced_type<std::invoke_result_t<Func, inner_value_type<Level>, Args...>, Level>;

            if (empty()) {
                return transformed_type();
            }

            transformed_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            typename transformed_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = func((*this)[*gen], std::forward<Args>(args)...);
            }

            return res;
        }

        template <typename Func, typename... Args>
            requires invocable_no_arrnd<Func, inner_value_type<this_type::depth>, Args...>
        [[nodiscard]] constexpr auto transform(Func&& func, Args&&... args) const
        {
            return transform<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_value_type<Level>, typename ArCo::value_type, Args...>)
        [[nodiscard]] constexpr auto transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            using transformed_type = inner_replaced_type<std::invoke_result_t<Func, inner_value_type<Level>,
                                                             typename ArCo::template inner_value_type<Level>, Args...>,
                Level>;

            if (empty()) {
                return transformed_type();
            }

            //if (hdr_.dims() != arr.header().dims()) {
            //    return transformed_type();
            //}
            assert(hdr_.numel() == arr.header().numel());

            transformed_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            typename transformed_type::indexer_type res_gen(res.header());
            typename transformed_type::indexer_type arr_gen(arr.header());

            for (; gen && res_gen && arr_gen; ++gen, ++res_gen, ++arr_gen) {
                res[*res_gen] = (*this)[*gen].template transform<Level - 1, typename ArCo::value_type, Func, Args...>(
                    arr[*arr_gen], std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Func, inner_value_type<Level>, typename ArCo::value_type, Args...>)
        [[nodiscard]] constexpr auto transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            using transformed_type = inner_replaced_type<
                std::invoke_result_t<Func, inner_value_type<Level>, typename ArCo::value_type, Args...>, Level>;

            if (empty()) {
                return transformed_type();
            }

            if (hdr_.is_scalar()) {
                auto val = (*this)(0);
                auto tfunc = [&](const auto& a) -> typename transformed_type::value_type {
                    return func(val, a, std::forward<Args>(args)...);
                };
                return arr.template transform<Level>(tfunc);
            }

            if (arr.header().is_scalar()) {
                auto val = arr(0);
                auto tfunc = [&](const auto& a) -> typename transformed_type::value_type {
                    return func(a, val, std::forward<Args>(args)...);
                };
                return transform<Level>(tfunc);
            }

            auto arr1 = *this;
            auto arr2 = arr;

            if (arr1.header().is_reduced_dims_from(arr2.header().dims())) {
                auto reps = arr1.header().complement_dims_from(arr2.header().dims());
                arr1 = arr1./*template */repeat/*<Level>*/(reps);
            } else if (arr2.header().is_reduced_dims_from(arr1.header().dims())) {
                auto reps = arr2.header().complement_dims_from(arr1.header().dims());
                arr2 = arr2./*template */repeat/*<Level>*/(reps);
            }

            //if (hdr_.dims() != arr.header().dims()) {
            //    return transformed_type();
            //}
            assert(arr1.header().numel() == arr2.header().numel());

            transformed_type res(arr1.header().dims().cbegin(), arr1.header().dims().cend());

            indexer_type gen(arr1.header());
            typename transformed_type::indexer_type res_gen(res.header());

            typename ArCo::indexer_type arr_gen(arr2.header());

            for (; gen && arr_gen && res_gen; ++gen, ++arr_gen, ++res_gen) {
                res[*res_gen] = func(arr1[*gen], arr2[*arr_gen], std::forward<Args>(args)...);
            }

            return res;
        }

        template <arrnd_compliant ArCo, typename Func, typename... Args>
            requires invocable_no_arrnd<Func, inner_value_type<this_type::depth>, typename ArCo::value_type, Args...>
        [[nodiscard]] constexpr auto transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            return transform<this_type::depth, ArCo, Func, Args...>(
                arr, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_value_type<Level>, Args...>)
        constexpr this_type& apply(Func&& func, Args&&... args)
        {
            if (empty()) {
                return *this;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                (*this)[*gen].template apply<Level - 1, Func, Args...>(
                    std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return *this;
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, inner_value_type<Level>, Args...>)
        constexpr this_type& apply(Func&& func, Args&&... args)
        {
            if (empty()) {
                return *this;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                (*this)[*gen] = func((*this)[*gen], std::forward<Args>(args)...);
            }

            return *this;
        }

        template <typename Func, typename... Args>
            requires invocable_no_arrnd<Func, inner_value_type<this_type::depth>, Args...>
        constexpr this_type& apply(Func&& func, Args&&... args)
        {
            return apply<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            if (empty() || arr.empty()) {
                return *this;
            }

            if (arr.header().is_scalar()) {
                auto val = arr(0);
                auto tfunc = [&](const auto& a) {
                    return func(a, val, std::forward<Args>(args)...);
                };
                return apply<Level>(tfunc);
            }

            auto carr = arr;

            if (arr.header().is_reduced_dims_from(hdr_.dims())) {
                auto reps = arr.header().complement_dims_from(hdr_.dims());
                carr = arr./*template */repeat/*<Level>*/(reps);
            }

            //if (hdr_.dims() != arr.header().dims()) {
            //    return *this;
            //}
            assert(hdr_.numel() == carr.header().numel());

            indexer_type gen(hdr_);
            typename std::remove_cvref_t<ArCo>::indexer_type arr_gen(carr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                (*this)[*gen] = func((*this)[*gen], carr[*arr_gen], std::forward<Args>(args)...);
            }

            return *this;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Func, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            if (empty() || arr.empty()) {
                return *this;
            }

            //if (hdr_.dims() != arr.header().dims()) {
            //    return *this;
            //}
            assert(hdr_.numel() == arr.header().numel());

            for (indexer_type gen(hdr_); gen; ++gen) {
                (*this)[*gen].template apply<Level - 1, typename ArCo::value_type, Func, Args...>(
                    arr, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return *this;
        }

        template <arrnd_compliant ArCo, typename Func, typename... Args>
            requires invocable_no_arrnd<Func, inner_value_type<this_type::depth>,
                typename ArCo::template inner_value_type<ArCo::depth>, Args...>
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            return apply<this_type::depth, ArCo, Func, Args...>(
                arr, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            using reduced_type = std::invoke_result_t<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>;

            if (empty()) {
                return reduced_type();
            }

            indexer_type gen(hdr_);

            reduced_type res(static_cast<reduced_type>((*this)[*gen]));
            ++gen;

            while (gen) {
                res = func(std::forward<reduced_type>(res), (*this)[*gen], std::forward<Args>(args)...);
                ++gen;
            }

            return res;
        }
        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            using reduced_type = inner_replaced_type<
                std::invoke_result_t<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>, Level - 1>;

            if (empty()) {
                return reduced_type();
            }

            reduced_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reduce<Level - 1, Func, Args...>(
                    std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, inner_value_type<this_type::depth>, inner_value_type<this_type::depth>,
                Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            return reduce<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename U, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, U, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            using folded_type = std::invoke_result_t<Func, U, inner_value_type<Level>, Args...>;

            if (empty()) {
                return init;
            }

            folded_type res(static_cast<folded_type>(init));
            for (indexer_type gen{hdr_}; gen; ++gen) {
                res = func(res, (*this)[*gen], std::forward<Args>(args)...);
            }

            return res;
        }
        template <std::int64_t Level, typename U, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, U, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            using folded_type
                = inner_replaced_type<std::invoke_result_t<Func, U, inner_value_type<Level>, Args...>, Level - 1>;

            if (empty()) {
                return folded_type();
            }

            folded_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template fold<Level - 1, U, Func, Args...>(
                    init, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename U, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, U, inner_value_type<this_type::depth>, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            return fold<this_type::depth, U, Func, Args...>(
                init, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto reduce(size_type axis, Func&& func, Args&&... args) const
        {
            using reduced_type
                = replaced_type<std::invoke_result_t<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>>;

            if (empty()) {
                return reduced_type();
            }

            //typename reduced_type::header_type new_header(hdr_.subheader(axis));
            //if (new_header.empty()) {
            //    return reduced_type();
            //}

            auto new_header = header_type(hdr_.dims_with_modified_axis(axis));
            reduced_type res({new_header.numel()});
            res.header() = std::move(new_header);

            indexer_type gen(hdr_, std::ssize(hdr_.dims()) - axis - 1);
            indexer_type res_gen(res.header());

            const size_type reduction_iteration_cycle{hdr_.dims()[axis]};

            while (gen && res_gen) {
                typename reduced_type::value_type res_element(
                    static_cast<typename reduced_type::value_type>((*this)[*gen]));
                ++gen;
                for (size_type i = 0; i < reduction_iteration_cycle - 1; ++i, ++gen) {
                    res_element = func(std::forward<typename reduced_type::value_type>(res_element), (*this)[*gen],
                        std::forward<Args>(args)...);
                }
                res[*res_gen] = res_element;
                ++res_gen;
            }

            return res;
        }
        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto reduce(size_type axis, Func&& func, Args&&... args) const
        {
            using reduced_type = inner_replaced_type<
                replaced_type<std::invoke_result_t<Func, inner_value_type<Level>, inner_value_type<Level>, Args...>>,
                Level - 1>;

            if (empty()) {
                return reduced_type();
            }

            reduced_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reduce<Level - 1, Func, Args...>(
                    axis, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, inner_value_type<this_type::depth>, inner_value_type<this_type::depth>,
                Args...>)
        [[nodiscard]] constexpr auto reduce(size_type axis, Func&& func, Args&&... args) const
        {
            return reduce<this_type::depth, Func, Args...>(axis, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Func, typename ArCo::value_type, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto fold(size_type axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            using folded_type = replaced_type<
                std::invoke_result_t<Func, typename ArCo::value_type, inner_value_type<Level>, Args...>>;

            if (empty()) {
                return folded_type();
            }

            //typename folded_type::header_type new_header(hdr_.subheader(axis));

            assert(inits.header().dims().size() == 1 && inits.header().dims()[0] == hdr_.numel() / hdr_.dims()[axis]);

            //if (new_header.empty()) {
            //    return folded_type();
            //}

            auto new_header = header_type(hdr_.dims_with_modified_axis(axis));
            folded_type res({new_header.numel()});
            res.header() = std::move(new_header);

            indexer_type gen(hdr_, std::ssize(hdr_.dims()) - axis - 1);
            indexer_type res_gen(res.header());
            typename ArCo::indexer_type init_gen(inits.header());

            const size_type reduction_iteration_cycle{hdr_.dims()[axis]};

            while (gen && res_gen && init_gen) {
                typename folded_type::value_type res_element(
                    static_cast<typename folded_type::value_type>(inits[*init_gen]));
                for (size_type i = 0; i < reduction_iteration_cycle; ++i, ++gen) {
                    res_element = func(res_element, (*this)[*gen], std::forward<Args>(args)...);
                }
                res[*res_gen] = std::move(res_element);
                ++res_gen;
                ++init_gen;
            }

            return res;
        }
        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, typename ArCo::value_type, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto fold(size_type axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            using folded_type = inner_replaced_type<
                replaced_type<std::invoke_result_t<Func, typename ArCo::value_type, inner_value_type<Level>, Args...>>,
                Level - 1>;

            if (empty()) {
                return folded_type();
            }

            folded_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template fold<Level - 1, ArCo, Func, Args...>(
                    axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <arrnd_compliant ArCo, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, typename ArCo::value_type, inner_value_type<this_type::depth>, Args...>)
        [[nodiscard]] constexpr auto fold(size_type axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            return fold<this_type::depth, ArCo, Func, Args...>(
                axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type filter(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res({hdr_.numel()});

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            size_type res_count{0};

            while (gen && res_gen) {
                if (pred((*this)[*gen], std::forward<Args>(args)...)) {
                    res[*res_gen] = (*this)[*gen];
                    ++res_count;
                    ++res_gen;
                }
                ++gen;
            }

            if (res_count == 0) {
                return this_type();
            }

            if (res_count < hdr_.numel()) {
                return res./*template */resize/*<Level>*/({res_count});
            }

            return res;
        }
        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type filter(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template filter<Level - 1, Pred, Args...>(
                    std::forward<Pred>(pred), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>, Args...>
        [[nodiscard]] constexpr this_type filter(Pred&& pred, Args&&... args) const
        {
            return filter<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type filter(const ArCo& selector) const
        {
            if (empty()) {
                return this_type();
            }

            if constexpr (std::is_same_v<bool, typename ArCo::value_type>) {
                assert(hdr_.dims() == selector.header().dims());

                this_type res({hdr_.numel()});

                indexer_type gen(hdr_);
                typename ArCo::indexer_type selector_gen(selector.header());

                indexer_type res_gen(res.hdr_);

                size_type res_count{0};

                while (gen && selector_gen && res_gen) {
                    if (selector[*selector_gen]) {
                        res[*res_gen] = (*this)[*gen];
                        ++res_count;
                        ++res_gen;
                    }
                    ++gen;
                    ++selector_gen;
                }

                if (res_count == 0) {
                    return this_type();
                }

                if (res_count < hdr_.numel()) {
                    return res./*template */resize/*<Level>*/({res_count});
                }

                return res;
            } else {
                this_type res(selector.header().dims());

                indexer_type res_gen(res.hdr_);
                typename ArCo::indexer_type slc_gen(selector.header());

                for (; res_gen && slc_gen; ++res_gen, ++slc_gen) {
                    res[*res_gen] = (*this)[selector[*slc_gen]];
                }

                return res;
            }
        }
        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type filter(const ArCo& selector) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template filter<Level - 1, ArCo>(selector);
            }

            return res;
        }
        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr this_type filter(const ArCo& selector) const
        {
            return filter<this_type::depth, ArCo>(selector);
        }

        template <std::int64_t Level, signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto filter(InputIt first_ind, InputIt last_ind) const
        {
            return filter<Level>(replaced_type<size_type>({std::distance(first_ind, last_ind)}, first_ind, last_ind));
        }
        template <std::int64_t Level, signed_integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        [[nodiscard]] constexpr auto filter(const Cont& indices) const
        {
            return filter<Level>(
                replaced_type<size_type>({std::ssize(indices)}, std::begin(indices), std::end(indices)));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto filter(std::initializer_list<size_type> indices) const
        {
            return filter<Level>(replaced_type<size_type>({std::ssize(indices)}, indices.begin(), indices.end()));
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto filter(const U (&indices)[M]) const
        //{
        //    return filter<Level>(replaced_type<size_type>({M}, std::begin(indices), std::end(indices)));
        //}

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto filter(InputIt first_ind, InputIt last_ind) const
        {
            return filter<this_type::depth>(
                replaced_type<size_type>({std::distance(first_ind, last_ind)}, first_ind, last_ind));
        }
        template <signed_integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        [[nodiscard]] constexpr auto filter(const Cont& indices) const
        {
            return filter<this_type::depth>(
                replaced_type<size_type>({std::ssize(indices)}, std::begin(indices), std::end(indices)));
        }
        [[nodiscard]] constexpr auto filter(std::initializer_list<size_type> indices) const
        {
            return filter<this_type::depth>(
                replaced_type<size_type>({std::ssize(indices)}, indices.begin(), indices.end()));
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto filter(const U (&indices)[M]) const
        //{
        //    return filter<this_type::depth>(replaced_type<size_type>({M}, std::begin(indices), std::end(indices)));
        //}

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto find(Pred&& pred, Args&&... args) const
        {
            using found_type = inner_replaced_type<size_type, Level>;

            if (empty()) {
                return found_type();
            }

            found_type res({hdr_.numel()});

            indexer_type gen(hdr_);
            typename found_type::indexer_type res_gen(res.header());

            size_type res_count{0};

            while (gen && res_gen) {
                if (pred((*this)[*gen], std::forward<Args>(args)...)) {
                    res[*res_gen] = *gen;
                    ++res_count;
                    ++res_gen;
                }
                ++gen;
            }

            if (res_count == 0) {
                return found_type();
            }

            if (res_count < hdr_.numel()) {
                return res./*template */resize/*<Level>*/({res_count});
            }

            return res;
        }
        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto find(Pred&& pred, Args&&... args) const
        {
            using found_type = inner_replaced_type<size_type, Level>;

            if (empty()) {
                return found_type();
            }

            found_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template find<Level - 1, Pred, Args...>(
                    std::forward<Pred>(pred), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>, Args...>
        [[nodiscard]] constexpr auto find(Pred&& pred, Args&&... args) const
        {
            return find<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level == 0)
        [[nodiscard]] constexpr auto find(const ArCo& mask) const
        {
            using found_type = inner_replaced_type<size_type, Level>;

            if (empty()) {
                return found_type();
            }

            assert(hdr_.dims() == mask.header().dims());

            found_type res({hdr_.numel()});

            indexer_type gen(hdr_);
            typename ArCo::indexer_type mask_gen(mask.header());

            typename found_type::indexer_type res_gen(res.header());

            size_type res_count{0};

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
                return found_type();
            }

            if (res_count < hdr_.numel()) {
                return res./*template */resize/*<Level>*/({res_count});
            }

            return res;
        }
        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level > 0)
        [[nodiscard]] constexpr auto find(const ArCo& mask) const
        {
            using found_type = inner_replaced_type<size_type, Level>;

            if (empty()) {
                return found_type();
            }

            found_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template find<Level - 1, ArCo>(mask);
            }

            return res;
        }
        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr auto find(const ArCo& mask) const
        {
            return find<this_type::depth, ArCo>(mask);
        }

        template </*std::int64_t Level, */ arrnd_compliant ArCo>
            requires(/*Level > 0*/ same_depth<this_type, ArCo> && !this_type::is_flat && !ArCo::is_flat)
        [[nodiscard]] constexpr auto matmul(const ArCo& arr) const
        {
            return transform<0>(arr, [](const auto& a, const auto& b) {
                return a.matmul(b);
            });
            /*using ret_type = inner_replaced_type<decltype(inner_value_type<Level>{} * (typename ArCo::template inner_value_type<Level>{})), Level>;

            if (empty()) {
                return ret_type();
            }

            ret_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename ret_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template matmul<Level - 1>(arr);
            }

            return res;*/
        }
        template </*std::int64_t Level, */ arrnd_compliant ArCo>
            requires(/*Level == 0*/ this_type::is_flat && ArCo::is_flat)
        [[nodiscard]] constexpr auto matmul(const ArCo& arr) const
        {
            using ret_type = replaced_type<decltype(value_type{} * (typename ArCo::value_type{}))>;

            assert(hdr_.dims().size() >= 2 && arr.header().dims().size() >= 2);

            auto impl = [](const auto& lhs, const auto& rhs) {
                assert(lhs.header().is_matrix() && rhs.header().is_matrix());
                assert(lhs.header().dims().back() == rhs.header().dims().front());

                ret_type res({lhs.header().dims().front(), rhs.header().dims().back()});

                size_type ind = 0;
                auto trhs = rhs.template transpose<0>({1, 0});
                std::for_each(lhs.cbegin(arrnd_returned_slice_iterator_tag{}),
                    lhs.cend(arrnd_returned_slice_iterator_tag{}), [&res, &trhs, &ind](const auto& row) {
                        std::for_each(trhs.cbegin(arrnd_returned_slice_iterator_tag{}),
                            trhs.cend(arrnd_returned_slice_iterator_tag{}), [&res, &ind, &row](const auto& col) {
                                res[ind++] = (row * col).template reduce<0>(std::plus<>{});
                            });
                    });

                return res;
            };

            if (hdr_.is_matrix() && arr.header().is_matrix()) {
                return impl(*this, arr);
            }

            if (arr.header().is_matrix()) {
                return pageop<0>(2, [&arr, impl](auto page) {
                    return impl(page, arr);
                });
            } else {
                size_type lhs_num_pages
                    = hdr_.numel() / ((*std::next(hdr_.dims().cbegin(), hdr_.dims().size() - 2)) * hdr_.dims().back());
                size_type rhs_num_pages = arr.header().numel()
                    / ((*std::next(arr.header().dims().cbegin(), arr.header().dims().size() - 2))
                        * arr.header().dims().back());
                assert(lhs_num_pages == rhs_num_pages);

                auto arr_pages = arr.template pages<0>(arr.header().dims().size() - 3, 0, true);
                typename decltype(arr_pages)::indexer_type arr_pages_gen(arr_pages.header());

                return pageop<0>(2, [&arr_pages, &arr_pages_gen, &impl](auto page) {
                    return impl(page, arr_pages[*(arr_pages_gen++)]);
                });
            }
            //};

            //return pageop([&arr, &impl](const auto& page) {
            //    return impl(page, arr);
            //});
        }
        //template <arrnd_compliant ArCo>
        //[[nodiscard]] constexpr auto matmul(const ArCo& arr) const
        //{
        //    return matmul<this_type::depth>(arr);
        //}

        [[nodiscard]] constexpr auto det() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.det();
            });
        }

        [[nodiscard]] constexpr auto det() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<value_type(this_type)> det_impl;

            det_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());
                size_type n = arr.header().dims().front();

                if (n == 1) {
                    return arr(0);
                }

                if (n == 2) {
                    return arr(0) * arr(3) - arr(1) * arr(2);
                }

                value_type sign{1};
                value_type d{0};

                for (size_type j = 0; j < n; ++j) {
                    value_type p{arr[{0, j}]};
                    if (p != value_type{0}) {
                        d += sign * p * det_impl(arr.exclude({0}, {0}).exclude({1}, {j}).template merge<0>().merge());
                    }
                    sign *= value_type{-1};
                }
                return d;
            };

            if (hdr_.is_matrix()) {
                return this_type({1}, det_impl(*this));
            }

            return pageop<0>(2, [det_impl](auto page) {
                return det_impl(page);
            });
        }

        [[nodiscard]] constexpr auto inverse() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.inverse();
            });
        }

        [[nodiscard]] constexpr auto inverse() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<this_type(this_type)> inverse_impl;

            inverse_impl = [&](this_type arr) {
                //std::cout << "calc:\n" << arr << "\n\n";
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                value_type d = arr.det()(0);
                assert(d != value_type{0});
                size_type n = arr.header().dims().front();

                this_type res(arr.header().dims());

                for (size_type i = 0; i < n; ++i) {
                    value_type sign = (i + 1) % 2 == 0 ? value_type{-1} : value_type{1};
                    for (size_type j = 0; j < n; ++j) {
                        /*std::cout << "det for:\n"
                                  << exclude({0}, {i}).exclude({1}, {j}) << "\n\n";*/
                        res[{i, j}]
                            = sign * arr.exclude({0}, {i}).exclude({1}, {j}).template merge<0>().merge().det()(0);
                        sign *= value_type{-1};
                    }
                }

                return (value_type{1} / d) * res.transpose({1, 0});
            };

            if (hdr_.is_matrix()) {
                return inverse_impl(*this);
            }

            return pageop<0>(2, [inverse_impl](auto page) {
                return inverse_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto lu() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.lu();
            });
        }
        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto lu() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type>(this_type)> lu_impl;

            lu_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                this_type l(arr.header().dims(), value_type{0});
                this_type u(arr.header().dims(), value_type{0});

                size_type n = arr.header().dims().front();

                for (size_type i = 0; i < n; ++i) {
                    // l
                    for (size_type k = i; k < n; ++k) {
                        value_type sum{0};
                        for (size_type j = 0; j < i; ++j) {
                            sum += l[{i, j}] * u[{j, k}];
                        }

                        u[{i, k}] = arr[{i, k}] - sum;
                    }

                    // u
                    for (size_type k = i; k < n; ++k) {
                        if (i == k) {
                            l[{i, i}] = value_type{1};
                        } else {
                            value_type sum{0};
                            for (size_type j = 0; j < i; ++j) {
                                sum += l[{k, j}] * u[{j, i}];
                            }

                            l[{k, i}] = (arr[{k, i}] - sum) / u[{i, i}];
                        }
                    }
                }

                return std::make_tuple(l, u);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, lu_impl(*this));
            }

            return pageop<0>(2, [lu_impl](auto page) {
                return lu_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto qr(/*bool permute = false*/) const
            requires(!this_type::is_flat)
        {
            return transform<0>([/*permute*/](const auto& a) {
                return a.qr(/*permute*/);
            });
        }
        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto qr(/*bool permute = false*/) const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type>(this_type)> qr_impl;

            qr_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                //assert(arr.header().dims().front() == arr.header().dims().back());

                size_type m = arr.header().dims().front();
                size_type n = arr.header().dims().back();

                this_type q = eye<this_type>({m, m});
                this_type r = arr.clone();

                using ival = interval_type;

                size_type loop_size = (m * (m < n) + n * (m >= n));

                for (size_type k = 1; k <= loop_size; ++k) {
                    auto a = r[{ival::between(k - 1, m), ival::at(k - 1)}];
                    auto alpha = std::sqrt(a.fold(value_type{0}, [](value_type acc, value_type val) {
                        return acc + val * val;
                    }));
                    if constexpr (template_type<value_type, std::complex>) {
                        using namespace std::complex_literals;
                        alpha *= std::exp(1i * std::arg(a[{0, 0}]));
                    }
                    auto e = eye<this_type>({m - k + 1, 1}) * alpha;
                    auto u = a - e;
                    auto sumsqu = u.fold(value_type{0}, [](value_type acc, value_type val) {
                        return acc + val * val;
                    });
                    this_type v{};
                    if (sumsqu != value_type{0}) {
                        v = u / std::sqrt(sumsqu);
                    } else {
                        v = u;
                    }

                    auto qk1 = eye<this_type>({k - 1, k - 1}).append(zeros<this_type>({k - 1, m - k + 1}), 1);
                    auto qk2 = zeros<this_type>({m - k + 1, k - 1})
                                   .append(eye<this_type>({m - k + 1, m - k + 1})
                                           - value_type{2} * v.matmul(v.transpose({1, 0})),
                                       1);
                    auto qk = qk1.append(qk2, 0);
                    r = qk.matmul(r);
                    q = q.matmul(qk);

                    /*std::cout << "qk:" << qk << "\n";
                    std::cout << "r:" << r << "\n";
                    std::cout << "q:" << q << "\n\n";*/
                }

                //if (permute) {
                return std::make_tuple(q, r);
                //}
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, qr_impl(*this));
            }

            return pageop<0>(2, [qr_impl](auto page) {
                return qr_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto hess() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.hess();
            });
        }
        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto hess() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type>(this_type)> hess_impl;

            hess_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                size_type n = arr.header().dims().front();

                auto q = eye<this_type>({n, n});
                auto h = arr;

                using ival = interval_type;

                for (size_type k = 0; k < n - 2; ++k) {
                    auto r = h[{ival::between(k + 1, n), ival::at(k)}];
                    auto u = zeros<this_type>({n - (k + 1), 1});

                    if constexpr (template_type<value_type, std::complex>) {
                        using std::exp;
                        using std::arg;
                        using std::pow;
                        using namespace std::complex_literals;

                        u[{0, 0}]
                            = -exp(arg(r[{0, 0}]) * 1i) * pow((r.transpose({1, 0}).matmul(r)), value_type{0.5})(0);
                    } else {
                        u[{0, 0}] = -(oc::sign(r[{0, 0}]) * (r[{0, 0}] != value_type{0}) + (r[{0, 0}] == value_type{0}))
                            * std::pow((r.transpose({1, 0}).matmul(r)(0)), value_type{0.5});
                    }

                    auto v = r - u;
                    v /= std::pow((v.transpose({1, 0}).matmul(v)(0)), value_type{0.5});

                    auto w1 = eye<this_type>({k + 1, k + 1}).append(zeros<this_type>({k + 1, n - (k + 1)}), 1);
                    auto w2 = zeros<this_type>({n - (k + 1), k + 1})
                                  .append(eye<this_type>({n - (k + 1), n - (k + 1)})
                                          - value_type{2} * (v.matmul(v.transpose({1, 0}))),
                                      1);

                    auto w = w1.append(w2, 0);

                    h = w.matmul(h, w.transpose({1, 0}));
                    q = q.matmul(w.transpose({1, 0}));
                }

                return std::make_tuple(q, h);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, hess_impl(*this));
            }

            return pageop<0>(2, [hess_impl](auto page) {
                return hess_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto schur() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.schur();
            });
        }
        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto schur() const
            requires(this_type::is_flat && !template_type<value_type, std::complex>)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type>(this_type)> schur_impl;

            schur_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                size_type n = arr.header().dims().front();

                auto [u, s] = arr.hess()(0);

                using ival = interval_type;

                auto k = n;
                value_type tol = value_type{1e-20};

                while (k > 2) {
                    //std::cout << "k=" << k << "\n";
                    if (s[{k - 1, k - 2}] == value_type{0} && s[{k - 1, k - 3}] == value_type{0}) {
                        --k;
                    } else if (s[{k - 2, k - 3}] == value_type{0} && s[{k - 1, k - 3}] == value_type{0}
                        && (k <= 3 || s[{k - 2, k - 4}] == value_type{0})) {
                        k -= 2;
                    }
                    //std::cout << "k=" << k << "\n";

                    if (k > 1) {
                        auto stok = s[{ival::to(k), ival::to(k)}];
                        //std::cout << "stok:\n" << stok << "\n";
                        auto s_k_1 = s[{k - 2, k - 2}];
                        auto s_k = s[{k - 1, k - 1}];
                        auto s_kk_1 = s[{k - 1, k - 2}];
                        auto s_k_1k = s[{k - 2, k - 1}];
                        //std::cout << "stok2:\n" << stok.matmul(stok) << "\n";
                        //std::cout << "s_k_1:\n" << s_k_1 << "\n";
                        //std::cout << "s_k:\n" << s_k << "\n";
                        //std::cout << "s_k_1k:\n" << s_k_1k << "\n";
                        //std::cout << "s_kk_1:\n" << s_kk_1 << "\n";

                        auto m = stok.matmul(stok) - (s_k_1 + s_k) * stok
                            + (s_k_1 * s_k - s_k_1k * s_kk_1) * eye<this_type>({k, k});
                        //std::cout << "m:\n" << m << "\n";
                        auto [q, r] = m.qr()(0);
                        //std::cout << "q:\n" << q << "\n";
                        //std::cout << "r:\n" << r << "\n";

                        auto q1 = q.append(zeros<this_type>({k, n - k}), 1);
                        auto q2 = zeros<this_type>({n - k, k}).append(eye<this_type>({n - k, n - k}), 1);
                        q = q1.append(q2, 0);
                        //std::cout << "q:\n" << q << "\n";

                        u = u.matmul(q);
                        s = q.transpose({1, 0}).matmul(s, q);

                        auto m1 = s.abs() < tol;

                        replaced_type<size_type> arng({n, n});
                        std::iota(arng.begin(), arng.end(), size_type{1});
                        auto m2 = arng.tril(-1) > size_type{0};

                        auto mask = m1 && m2;
                        //std::cout << "m1:\n" << m1 << "\n";
                        //std::cout << "m2:\n" << m2 << "\n";
                        //std::cout << "mask:\n" << mask << "\n";
                        {
                            auto sgen = s.indexer();
                            auto mgen = mask.indexer();

                            for (; sgen && mgen; ++sgen, ++mgen) {
                                if (mask[*mgen]) {
                                    s[*sgen] = value_type{0};
                                }
                            }
                        }

                        //std::cout << "u:\n" << u << "\n";
                        //std::cout << "s:\n" << s << "\n";
                    }
                }

                for (size_type k = 1; k <= n - 1; ++k) {
                    if (s[{k, k - 1}] != value_type{0}) {
                        auto f = s[{ival::between(k - 1, k + 1), ival::between(k - 1, k + 1)}];
                        //std::cout << "f:\n" << f << "\n";
                        auto trc = f.diag().sum();
                        value_type t{};
                        if (trc * trc - 4 * f.det() > value_type{0}) {
                            t = std::atan((-f[{0, 0}] + f[{1, 1}]
                                              + std::sqrt(std::pow(f[{0, 0}], 2) - 2 * f[{0, 0}] * f[{1, 1}]
                                                  + 4 * f[{0, 1}] * f[{1, 0}] + std::pow(f[{1, 1}], 2)))
                                / (2 * f[{0, 1}]));
                        } else {
                            t = .5 * std::atan((f[{1, 1}] - f[{0, 0}]) / (f[{0, 1}] + f[{1, 0}]));
                        }
                        auto q = this_type({2, 2}, {std::cos(t), -std::sin(t), std::sin(t), std::cos(t)});
                        //std::cout << "q:\n" << q << "\n\n";
                        auto qq = eye<this_type>({n, n});
                        qq[{ival::between(k - 1, k + 1), ival::between(k - 1, k + 1)}].copy_from(q);
                        //std::cout << "q:\n" << q << "\n\n";
                        s = qq.transpose({1, 0}).matmul(s, qq);
                        u = u.matmul(qq);
                    }
                }

                return std::make_tuple(u, s);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, schur_impl(*this));
            }

            return pageop<0>(2, [schur_impl](auto page) {
                return schur_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto schur() const
            requires(this_type::is_flat && template_type<value_type, std::complex>)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type>(this_type)> schur_impl;

            schur_impl = [&](this_type arr) {
                size_type n = arr.header().dims().front();

                using rtype = typename value_type::value_type;

                replaced_type<rtype> rarr = arr.real();
                auto [ru, rs] = rarr.schur()(0);
                this_type u = ru;
                this_type s = rs;

                //std::cout << u << "\n\n";
                //std::cout << s << "\n\n";

                using ival = interval_type;

                rtype tol = std::numeric_limits<rtype>::epsilon();

                for (size_type k = 1; k <= n - 1; ++k) {
                    if (std::abs(s[{k, k - 1}]) > tol * (std::abs(s[{k - 1, k - 1}] + std::abs(s[{k, k}])))) {
                        auto b = s[{ival::between(k - 1, k + 1), ival::between(k - 1, k + 1)}];
                        //std::cout << b << "\n\n";
                        auto mu = std::sqrt(b[{1, 0}] * b[{0, 1}]);
                        //std::cout << mu << "\n\n";
                        auto r = std::sqrt(std::conj(mu) * mu + std::conj(b[{1, 0}]) * b[{1, 0}]); // conj transpose
                        //std::cout << r << "\n\n";
                        auto c = mu / r;
                        //std::cout << c << "\n\n";
                        auto ss = b[{1, 0}] / r;
                        //std::cout << ss << "\n\n";

                        this_type g({2, 2}, {std::conj(c), -ss, ss, c});
                        //std::cout << g << "\n\n";

                        auto s1 = s[{ival::between(k - 1, k + 1), ival::between(k - 1, n)}];
                        s1.copy_from(g.transpose({1, 0}).conj().matmul(s1)); // conj transpose
                        //std::cout << s << "\n\n";

                        auto s2 = s[{ival::between(0, k + 1), ival::between(k - 1, k + 1)}];
                        s2.copy_from(s2.matmul(g));
                        //std::cout << s << "\n\n";

                        auto u1 = u[{ival::between(0, n), ival::between(k - 1, k + 1)}];
                        u1.copy_from(u1.matmul(g));
                        //std::cout << u << "\n\n";
                    }
                    s[{k, k - 1}] = 0;
                }

                return std::make_tuple(u, s);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, schur_impl(*this));
            }

            return pageop<0>(2, [schur_impl](auto page) {
                return schur_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto eig() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.eig();
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto eig() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<this_type(this_type)> qr_algorithm_impl;
            
            qr_algorithm_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                value_type tol = value_type{1e-12};
                size_type max_iters = 1000;

                auto [_, h] = arr.hess()(0);
                // for tolerance
                //h(h <= tol) = value_type{0};

                size_type n = arr.header().dims().front();

                auto qq = eye<this_type>({n, n});

                size_type k = 0;
                value_type diff;
                if constexpr (template_type<value_type, std::complex>) {
                    diff = std::numeric_limits<double>::max();
                } else {
                    diff = std::numeric_limits<value_type>::max();
                }

                while (k++ < max_iters && diff > tol) {
                    auto h_prev = h.clone();
                    auto s = h[{n - 1, n - 1}];
                    auto smult = s * eye<this_type>({n, n});

                    auto [q, r] = (h - smult).qr()(0);

                    h = r.matmul(q) + smult;
                    qq = qq.matmul(q);

                    diff = (h - h_prev).abs().max();
                }

                return h.diag()(arrnd_shape_preset::column);
            };

            std::function<std::tuple<this_type, this_type>(this_type)> eig_impl;

            eig_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                size_type n = arr.header().dims().front();

                //auto [u, s] = arr.schur()(0);
                ////std::cout << "u:\n" << u << "\n\n";
                ////std::cout << "s:\n" << s << "\n\n";

                using ival = interval_type;

                //auto lambda = s.diag()(arrnd_shape_preset::column);
                auto lambda = qr_algorithm_impl(arr);
                auto v = zeros<this_type>({n, n});

                for (size_type k = 1; k <= n; ++k) {
                    auto [q, _] = (arr - eye<this_type>({n, n}) * lambda[k - 1]).transpose({1, 0}).qr()(0);
                    //std::cout << "q(m):\n" << q << "\n\n";
                    v[{ival::full(), ival::at(k - 1)}].copy_from(q[{ival::full(), ival::at(n - 1)}]);
                    //std::cout << "v(m):\n" << v << "\n\n";
                }

                return std::make_tuple(lambda, v);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type>>({1}, eig_impl(*this));
            }

            return pageop<0>(2, [eig_impl](auto page) {
                return eig_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto svd() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.svd();
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto svd() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<std::tuple<this_type, this_type, this_type>(this_type)> svd_impl;

            svd_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());
                //assert(arr.header().dims().front() == arr.header().dims().back());

                this_type arr_t{};
                if constexpr (template_type<value_type, std::complex>) {
                    arr_t = arr.transpose().conj();
                } else {
                    arr_t = arr.transpose();
                }

                auto [l1, u] = (arr.as_pages() * arr_t.as_pages()).eig()(0);
                auto [l2, v] = (arr_t.as_pages() * arr.as_pages()).eig()(0);

                using ord_type = replaced_type<size_type>;

                auto comp = [](const auto& t1, const auto& t2) {
                    return std::get<0>(t1) > std::get<0>(t2);
                };

                ord_type ord1({l1.header().numel()});
                std::iota(ord1.begin(), ord1.end(), size_type{0});
                auto z1 = zip(zipped_cont(l1), zipped_cont(ord1));
                std::sort(z1.begin(), z1.end(), comp);
                u = u.reorder(1, ord1);

                ord_type ord2({l2.header().numel()});
                std::iota(ord2.begin(), ord2.end(), size_type{0});
                auto z2 = zip(zipped_cont(l2), zipped_cont(ord2));
                std::sort(z2.begin(), z2.end(), comp);
                v = v.reorder(1, ord2);

                auto s = zeros<this_type>(arr.header().dims());

                auto min_dim = std::min({s.header().dims().front(), s.header().dims().back()});

                auto sv = (l1(arrnd_shape_preset::vector)[{interval_type::to(min_dim)}]
                              + l2(arrnd_shape_preset::vector)[{interval_type::to(min_dim)}])
                    / value_type{2};
                sv(sv < value_type{0}) = value_type{0};

                s[{interval_type::to(min_dim), interval_type::to(min_dim)}]
                    = sv.sqrt().diag(arrnd_diag_type::to_matrix);

                if constexpr (template_type<value_type, std::complex>) {
                    v = ((arr.matmul(v)).inverse().matmul(u)).matmul(s);
                } else {
                    auto m = (arr.matmul(v) - u.matmul(s)).abs().max(1)(arrnd_shape_preset::vector) > value_type{1e-8};

                    for (size_type i = 0; i < m.header().numel(); ++i) {
                        if (m[i]) {
                            v[{interval_type::full(), interval_type::at(i)}]
                                = v[{interval_type::full(), interval_type::at(i)}] * value_type{-1};
                        }
                    }
                }

                //using ival = interval_type;

                //auto r = arr.header().dims().front();
                //auto c = arr.header().dims().back();

                //this_type l1;
                //this_type l2;
                //this_type u;
                //this_type v;

                //if constexpr (template_type<value_type, std::complex>) {
                //    auto [l1t, ut] = (arr.matmul(arr.transpose({1, 0}).conj())).eig()(0);
                //    auto [l2t, vt] = (arr.transpose({1, 0}).conj().matmul(arr)).eig()(0);
                //    l1 = l1t;
                //    l2 = l2t;
                //    u = ut;
                //    v = vt;
                //} else {
                //    auto [l1t, ut] = (arr.matmul(arr.transpose({1, 0}))).eig()(0);
                //    auto [l2t, vt] = (arr.transpose({1, 0}).matmul(arr)).eig()(0);
                //    l1 = l1t;
                //    l2 = l2t;
                //    u = ut;
                //    v = vt;
                //}

                //auto comp = [](const auto& t1, const auto& t2) {
                //    return std::get<0>(t1) > std::get<0>(t2);
                //};

                //auto sl1 = l1.clone();
                //replaced_type<size_type> sl1i({l1.header().numel()});
                //std::iota(sl1i.begin(), sl1i.end(), size_type{0});
                //auto z1 = zip(zipped_cont(sl1), zipped_cont(sl1i));
                //std::sort(z1.begin(), z1.end(), comp);

                //auto sl2 = l2.clone();
                //replaced_type<size_type> sl2i({l2.header().numel()});
                //std::iota(sl2i.begin(), sl2i.end(), size_type{0});
                //auto z2 = zip(zipped_cont(sl2), zipped_cont(sl2i));
                //std::sort(z2.begin(), z2.end(), comp);

                //u = u.reorder(1, sl1i.cbegin(), sl1i.cend());
                //v = v.reorder(1, sl2i.cbegin(), sl2i.cend());

                //auto s = zeros<this_type>({r, c});

                //auto arr_minsize = std::min({r, c});

                //auto slc1 = l1(sl1i)()[{ival::to(arr_minsize)}];
                //auto slc2 = l2(sl2i)()[{ival::to(arr_minsize)}];
                //auto sv = (slc1 + slc2) / 2;
                //sv(sv < value_type{0}) = value_type{0};
                //s[{ival::to(arr_minsize), ival::to(arr_minsize)}].copy_from(sv.sqrt().diag(arrnd_diag_type::to_matrix));

                //if constexpr (template_type<value_type, std::complex>) {
                //    v = ((arr.matmul(v)).inverse().matmul(u)).matmul(s);
                //} else {
                //    auto mask = (arr.matmul(v) - u.matmul(s)).abs().reduce(1, [](value_type m, value_type v) {
                //        return std::max({m, v});
                //    }) > value_type{1e-8};
                //    v(mask) = v * (value_type{-1});
                //}

                return std::make_tuple(u, s, v);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<std::tuple<this_type, this_type, this_type>>({1}, svd_impl(*this));
            }

            return pageop<0>(2, [svd_impl](auto page) {
                return svd_impl(page);
            });
        }

        [[nodiscard]] constexpr auto tril(size_type offset = 0) const
            requires(!this_type::is_flat)
        {
            return transform<0>([offset](const auto& a) {
                return a.tril(offset);
            });
        }

        [[nodiscard]] constexpr auto tril(size_type offset = 0) const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<this_type(this_type)> tril_impl;

            tril_impl = [&](this_type arr) {
                assert(arr.header().is_matrix());

                size_type r = arr.header().dims().front();
                size_type c = arr.header().dims().back();
                assert(offset >= -r && offset <= c);

                this_type res(arr.header().dims(), value_type{0});

                size_type current_row_size = offset <= 0 ? 1 : offset + 1;
                size_type tril_rows_count = offset >= 0 ? r : r + offset;

                size_type current_ind = (r - tril_rows_count) * c;

                while (tril_rows_count--) {
                    for (size_type i = current_ind; i < current_ind + current_row_size; ++i) {
                        res[i] = arr(i);
                    }

                    current_row_size = std::min(c, current_row_size + 1);
                    current_ind += c;
                }

                return res;
            };

            if (hdr_.is_matrix()) {
                return tril_impl(*this);
            }

            return pageop<0>(2, [tril_impl](auto page) {
                return tril_impl(page);
            });
        }

        [[nodiscard]] constexpr auto triu(size_type offset = 0) const
            requires(!this_type::is_flat)
        {
            return transform<0>([offset](const auto& a) {
                return a.triu(offset);
            });
        }

        [[nodiscard]] constexpr auto triu(size_type offset = 0) const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<this_type(this_type)> triu_impl;

            triu_impl = [&](this_type arr) {
                return arr - arr.tril(offset - 1);
                /*assert(arr.header().is_matrix());

                size_type r = arr.header().dims().front();
                size_type c = arr.header().dims().back();
                assert(offset > -r && offset < c);

                this_type res = arr.clone();

                size_type current_row_size = offset <= 0 ? 1 : offset + 1;
                size_type tril_rows_count = offset >= 0 ? r : r + offset;

                size_type current_ind = (r - tril_rows_count) * c;

                while (tril_rows_count--) {
                    for (size_type i = current_ind; i < current_ind + current_row_size; ++i) {
                        res[i] = value_type{0};
                    }

                    current_row_size = std::min(c, current_row_size + 1);
                    current_ind += c;
                }

                return res;*/
            };

            if (hdr_.is_matrix()) {
                return triu_impl(*this);
            }

            return pageop<0>(2, [triu_impl](auto page) {
                return triu_impl(page);
            });
        }

        [[nodiscard]] constexpr auto diag(arrnd_diag_type type = arrnd_diag_type::from_matrix, size_type offset = 0) const
            requires(!this_type::is_flat)
        {
            return transform<0>([type, offset](const auto& a) {
                return a.diag(type, offset);
            });
        }
        [[nodiscard]] constexpr auto diag(arrnd_diag_type type = arrnd_diag_type::from_matrix,
            size_type offset = 0) const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 1);

            std::function<this_type(this_type)> diag_from_matrix_impl;

            diag_from_matrix_impl = [&](this_type arr) {
                if (arr.empty()) {
                    return this_type();
                }

                assert(arr.header().is_matrix());

                size_type r = arr.header().dims().front();
                size_type c = arr.header().dims().back();

                assert(offset > -r && offset < c);

                size_type n = std::min(r, c);

                size_type abs_offset = static_cast<size_type>(std::abs(offset));

                size_type numel = 0;
                //abs_offset + 1 <= n ? n : n - abs_offset;
                if (r == c) {
                    numel = n - abs_offset;
                } else if (c > r) {
                    if (offset > 0) {
                        if (offset < n) {
                            numel = n;
                        } else {
                            numel = n - abs_offset;
                        }
                    } else {
                        numel = n - abs_offset;
                    }
                } else {
                    if (offset < 0) {
                        if (abs_offset < n) {
                            numel = n;
                        } else {
                            numel = n - abs_offset;
                        }
                    } else {
                        numel = n - abs_offset;
                    }
                }

                this_type res({numel});

                size_type current_ind = offset >= 0 ? offset : -offset * c;

                for (size_type i = 0; i < numel; ++i) {
                    res[i] = arr(current_ind);
                    current_ind += c + 1;
                }

                return res;
            };


            std::function<this_type(this_type)> diag_to_matrix_impl;

            diag_to_matrix_impl = [&](this_type arr) {
                if (arr.empty()) {
                    return this_type();
                }

                assert(arr.header().is_vector());

                size_type abs_offset = static_cast<size_type>(std::abs(offset));

                size_type n = arr.header().numel() + abs_offset;

                this_type res({n, n}, 0);

                size_type current_ind = offset >= 0 ? offset : -offset * n;

                indexer_type gen(arr.header());

                for (; current_ind < res.header().numel() && gen; ++gen) {
                    res[current_ind] = arr[*gen];
                    current_ind += n + 1;
                }

                return res;
            };


            if (type == arrnd_diag_type::from_matrix) {
                if (hdr_.is_matrix()) {
                    return diag_from_matrix_impl(*this);
                }

                return pageop<0>(2, [diag_from_matrix_impl](auto page) {
                    return diag_from_matrix_impl(page);
                });
            }
            else {
                if (hdr_.is_vector()) {
                    return diag_to_matrix_impl(*this);
                }

                return pageop<0>(1, [diag_to_matrix_impl](auto page) {
                    return diag_to_matrix_impl(page(arrnd_shape_preset::vector));
                });
            }
        }

        [[nodiscard]] constexpr auto is_banded(size_type lower = 0, size_type upper = 0) const
            requires(!this_type::is_flat)
        {
            return transform<0>([lower, upper](const auto& a) {
                return a.is_banded(lower, upper);
            });
        }
        [[nodiscard]] constexpr auto is_banded(size_type lower = 0, size_type upper = 0) const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<bool(this_type)> is_banded_impl;

            is_banded_impl = [&](this_type arr) {
                if (arr.empty()) {
                    return true;
                }

                assert(arr.header().is_matrix());

                auto required = arr.tril(upper) && arr.triu(-lower);
                //std::cout << required << "\n";

                auto actual = arr.transform([](const value_type& val) {
                    return !oc::close(val, value_type{0});
                });
                //std::cout << actual << "\n";

                return required.all_equal(actual);
            };

            if (hdr_.is_matrix()) {
                return replaced_type<bool>({1}, {is_banded_impl(*this)});
            }

            return pageop<0>(2, [is_banded_impl](auto page) {
                return is_banded_impl(page);
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto cholesky() const
            requires(!this_type::is_flat)
        {
            return transform<0>([](const auto& a) {
                return a.cholesky();
            });
        }

        [[deprecated("not fully cheked")]] [[nodiscard]] constexpr auto cholesky() const
            requires(this_type::is_flat)
        {
            assert(hdr_.dims().size() >= 2);

            std::function<this_type(this_type)> cholesky_impl;

            cholesky_impl = [&](this_type arr) {
                //std::cout << "calc:\n" << arr << "\n\n";
                assert(arr.header().is_matrix());
                assert(arr.header().dims().front() == arr.header().dims().back());

                size_type n = arr.header().dims().front();

                this_type l(arr.header().dims(), value_type{0});

                using ival = interval_type;

                for (size_type i = 1; i <= n; ++i) {
                    for (size_type j = 1; j <= i; ++j) {
                        if (i == j) {
                            auto sumsq = l[{ival::at(i - 1), ival::to(j - 1)}].fold(
                                value_type{0}, [](value_type acc, value_type val) {
                                    return acc + val * val;
                                });
                            l[{i - 1, j - 1}] = std::sqrt(arr[{i - 1, j - 1}] - sumsq);
                        } else {
                            if constexpr (template_type<value_type, std::complex>) {
                                l[{i - 1, j - 1}] = (arr[{i - 1, j - 1}]
                                                        - (l[{ival::at(i - 1), ival::to(j - 1)}]
                                                            * l[{ival::at(j - 1), ival::to(j - 1)}].conj())
                                                              .sum())
                                    / l[{j - 1, j - 1}];
                            } else {
                                l[{i - 1, j - 1}] = (arr[{i - 1, j - 1}]
                                                        - (l[{ival::at(i - 1), ival::to(j - 1)}]
                                                            * l[{ival::at(j - 1), ival::to(j - 1)}])
                                                              .sum())
                                    / l[{j - 1, j - 1}];
                            }
                        }
                        //std::cout << "L:\n" << l << "\n\n";
                    }
                }

                return l;
            };

            if (hdr_.is_matrix()) {
                return cholesky_impl(*this);
            }

            return pageop<0>(2, [cholesky_impl](auto page) {
                return cholesky_impl(page);
            });
        }

        template <arrnd_compliant ArCo>
            requires(same_depth<this_type, ArCo> && !this_type::is_flat && !ArCo::is_flat)
        [[nodiscard]] constexpr auto solve(const ArCo& b) const
        {
            return transform<0>(b, [](const auto& a, const auto& b) {
                return a.solve(b);
            });
        }
        template <arrnd_compliant ArCo>
            requires(this_type::is_flat && ArCo::is_flat)
        [[nodiscard]] constexpr auto solve(const ArCo& b) const
        {
            using ret_type = replaced_type<decltype(value_type{} * (typename ArCo::value_type{}))>;

            assert(hdr_.dims().size() >= 2 && b.header().dims().size() >= 2);

            auto solve_impl = [](const auto& lhs, const auto& rhs) {
                assert(lhs.header().is_matrix() && rhs.header().is_matrix());
                assert(lhs.header().dims().back() == rhs.header().dims().front());

                return lhs.inverse().matmul(rhs);
            };

            if (hdr_.is_matrix() && b.header().is_matrix()) {
                return solve_impl(*this, b);
            }

            if (b.header().is_matrix()) {
                return pageop<0>(2, [&b, solve_impl](auto page) {
                    return solve_impl(page, b);
                });
            } else {
                size_type lhs_num_pages
                    = hdr_.numel() / ((*std::next(hdr_.dims().cbegin(), hdr_.dims().size() - 2)) * hdr_.dims().back());
                size_type rhs_num_pages = b.header().numel()
                    / ((*std::next(b.header().dims().cbegin(), b.header().dims().size() - 2))
                        * b.header().dims().back());
                assert(lhs_num_pages == rhs_num_pages);

                auto b_pages = b.template pages<0>(b.header().dims().size() - 3, 0, true);
                typename decltype(b_pages)::indexer_type b_pages_gen(b_pages.header());

                return pageop<0>(2, [&b_pages, &b_pages_gen, &solve_impl](auto page) {
                    return solve_impl(page, b_pages[*(b_pages_gen++)]);
                });
            }
            //};

            //return pageop([&arr, &impl](const auto& page) {
            //    return impl(page, arr);
            //});
        }

        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type transpose(const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            header_type new_header = header_type(hdr_.dims()).reorder(first_order, last_order);
            if (new_header.empty()) {
                return this_type();
            }

            this_type res({hdr_.numel()});
            res.hdr_ = std::move(new_header);

            indexer_type gen(hdr_, first_order, last_order);
            indexer_type res_gen(res.hdr_);

            while (gen && res_gen) {
                res[*res_gen] = (*this)[*gen];
                ++gen;
                ++res_gen;
            }

            return res;
        }
        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type transpose(const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template transpose<Level - 1, InputIt>(first_order, last_order);
            }

            return res;
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr this_type transpose(const InputIt& first_order, const InputIt& last_order) const
        {
            return transpose<this_type::depth, InputIt>(first_order, last_order);
        }
        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr this_type transpose(const Cont& order) const
        {
            return transpose<Level>(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr this_type transpose(const Cont& order) const
        {
            return transpose<this_type::depth>(std::begin(order), std::end(order));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr this_type transpose(std::initializer_list<size_type> order) const
        {
            return transpose<Level>(order.begin(), order.end());
        }
        [[nodiscard]] constexpr this_type transpose(std::initializer_list<size_type> order) const
        {
            return transpose<this_type::depth>(order.begin(), order.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr this_type transpose(const U (&order)[M]) const
        //{
        //    return transpose<Level>(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr this_type transpose(const U (&order)[M]) const
        //{
        //    return transpose<this_type::depth>(std::begin(order), std::end(order));
        //}


        template <std::int64_t Level>
        requires(Level == 0) [[nodiscard]] constexpr this_type transpose() const
        {
            if (empty()) {
                return this_type();
            }

            this_type::template replaced_type<size_type> order({hdr_.dims().size()});
            std::iota(order.begin(), order.end(), size_type{0});

            if (order.header().numel() > 1) {
                std::swap(order[order.header().numel() - 1], order[order.header().numel() - 2]);
            }

            return transpose<Level>(order);
        }
        template <std::int64_t Level>
        requires(Level > 0) [[nodiscard]] constexpr this_type transpose() const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template transpose<Level - 1>();
            }

            return res;
        }
        [[nodiscard]] constexpr this_type transpose() const
        {
            return transpose<this_type::depth>();
        }


        //template <std::int64_t Depth>
        //    requires(Depth == 0)
        //[[nodiscard]] constexpr auto nest() const // deprecated
        //{
        //    assert(hdr_.dims().size() > Depth);

        //    return *this;
        //}
        //template <std::int64_t Depth>
        //    requires(Depth > 0)
        //[[nodiscard]] constexpr auto nest() const
        //{
        //    assert(hdr_.dims().size() > Depth);

        //    using nested_type = replaced_type<decltype(nest<Depth - 1>())>;

        //    nested_type res({hdr_.dims().front()});

        //    for (std::int64_t i = 0; i < hdr_.dims().front(); ++i) {
        //        res[i] = (*this)[interval<size_type>(i, i + 1)].template nest<Depth - 1>();
        //    }

        //    return res;
        //}

        template <std::int64_t Level>
            requires(Level > 0)
        [[nodiscard]] constexpr auto expand(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            using expanded_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return expanded_type();
            }

            expanded_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename expanded_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template expand<Level - 1>(
                    axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
            }

            return res;
        }

        template <std::int64_t Level>
            requires(Level == 0)
        [[nodiscard]] constexpr auto expand(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            using expanded_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return expanded_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            auto fixed_axis = axis;
            if (find_closest_axis_dim_bigger_than_one_to_the_left) {
                if (*std::next(hdr_.dims().cbegin(), fixed_axis) == 1) {
                    for (size_type i = axis - 1; i >= 0; --i) {
                        if (*std::next(hdr_.dims().cbegin(), i) > 1) {
                            fixed_axis = i;
                            break;
                        }
                    }
                }
            }

            auto axis_dim = *std::next(hdr_.dims().cbegin(), fixed_axis);

            assert(division <= axis_dim);
            auto fixed_div = division > 0 ? std::min(axis_dim, division) : axis_dim;

            // TODO: new dimensions creation should be in arrnd_header class
            typename expanded_type::header_type::storage_type new_dims(hdr_.dims().size());
            std::fill(new_dims.begin(), new_dims.end(), 1);
            *std::next(new_dims.begin(), fixed_axis) = fixed_div;

            expanded_type res(new_dims);
            typename expanded_type::indexer_type res_gen(res.header());

            //auto axis_dim_left = axis_dim;
            //auto interval_width
            //    = static_cast<size_type>(std::ceil(axis_dim / static_cast<double>(fixed_div)));
            //auto count = 0;

            auto curr_div = fixed_div;
            auto curr_axis_dim = axis_dim;
            auto curr_ival_width = static_cast<size_type>(std::ceil(curr_axis_dim / static_cast<double>(curr_div)));

            auto count = 0;

            ranger_type rgr(hdr_, fixed_axis, interval_type(0, curr_ival_width - 1), true);

            while (curr_div > 0) {
                res[*res_gen] = (*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())];

                rgr += curr_ival_width;
                ++res_gen;

                --curr_div;
                curr_axis_dim -= curr_ival_width;

                // prevent division by zero before end of division loop
                if (curr_div > 0) {
                    curr_ival_width = static_cast<size_type>(std::ceil(curr_axis_dim / static_cast<double>(curr_div)));
                    rgr.change_window(interval_type(0, curr_ival_width - 1));
                }

                ++count;
            }

            //for (; res_gen && rgr; ++res_gen, rgr += interval_width) {
            //    res[*res_gen] = (*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())];

            //    axis_dim_left -= interval_width;
            //    if (axis_dim_left > 0 && axis_dim_left < interval_width) {
            //        interval_width = axis_dim_left + 1;
            //        rgr.change_interval_width(axis_dim_left);
            //    }

            //    ++count;
            //}

            if (count != fixed_div) {
                return res./*template */resize/*<Level>*/({count});
            }
            return res;
        }

        [[nodiscard]] constexpr auto expand(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            return expand<this_type::depth>(axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
        }

        template <std::int64_t Level>
            requires(Level > 1 && !this_type::is_flat)
        [[nodiscard]] constexpr auto collapse() const
        {
            using collapsed_type = inner_replaced_type<inner_value_type<Level>, Level - 1>;

            if (empty()) {
                return collapsed_type();
            }

            collapsed_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename collapsed_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template collapse<Level - 1>();
            }

            return res;
        }

        /**
        * @note collapse should only used on valid expanded arrays
        */
        template <std::int64_t Level>
            requires(Level == 1 && !this_type::is_flat)
        [[nodiscard]] constexpr auto collapse() const
        {
            using collapsed_type = value_type;

            if (empty()) {
                return collapsed_type();
            }

            // from array creator

            bool all_nested_values_have_the_same_creator
                = std::adjacent_find(cbegin(), cend(),
                      [](const collapsed_type& vt1, const collapsed_type& vt2) {
                          return !vt1.creator() || !vt2.creator() || vt1.creator() != vt2.creator();
                      })
                == cend();

            if (all_nested_values_have_the_same_creator && (*this)(0).creator() != nullptr) {
                return *((*this)(0).creator());
            }

            // from one collapsed array

            if (hdr_.numel() == 1) {
                return (*this)(0);
            }

            // from assumed axis hint

            assert(std::count(hdr_.dims().cbegin(), hdr_.dims().cend(), 1) == hdr_.dims().size() - 1);

            auto axis_dim_it = std::find(hdr_.dims().cbegin(), hdr_.dims().cend(), hdr_.numel());
            auto assumed_axis = axis_dim_it - hdr_.dims().cbegin();

            indexer_type gen(hdr_);

            collapsed_type res = (*this)[*gen];
            ++gen;

            while (gen) {
                res = res./*template */append/*<Level - 1>*/((*this)[*gen], assumed_axis);
                ++gen;
            }

            return res;

            //bool nested_values_with_same_dims
            //    = std::adjacent_find(cbegin(), cend(), [](const collapsed_type& vt1, const collapsed_type& vt2) {
            //          return vt1.header().dims() != vt2.header().dims();
            //      }) == cend();
            //assert(nested_values_with_same_dims);

            //size_type assumed_axis = hdr_.dims().size();

            //header_type new_header = hdr_.expand((*this)(0).header().dims());
            //collapsed_type res(new_header.dims());
            //typename collapsed_type::indexer_type res_gen(res.header(), assumed_axis);

            //indexer_type gen(hdr_);

            //while (res_gen) {
            //    auto current_page = (*this)[*gen];
            //    typename collapsed_type::indexer_type page_gen(current_page.header());

            //    for (; page_gen; ++page_gen, ++res_gen) {
            //        res[*res_gen] = current_page[*page_gen];
            //    }
            //}

            //return res;
        }

        [[nodiscard]] constexpr auto collapse() const
            requires(!this_type::is_flat)
        {
            return collapse<this_type::depth>();
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_this_type<Level>, Args...>)
        constexpr auto pageop(size_type page_size, Func&& func, Args&&... args) const
        {
            constexpr bool is_void_func
                = std::is_same_v<std::invoke_result_t<Func, inner_this_type<Level>, Args...>, void>;
            using func_res_type = std::conditional_t<is_void_func, inner_this_type<Level>,
                std::invoke_result_t<Func, inner_this_type<Level>, Args...>>;
            using returned_type = std::conditional_t<arrnd_compliant<func_res_type>
                    && same_depth<func_res_type, inner_this_type<Level>>,
                inner_replaced_type<func_res_type, Level - 1>, inner_replaced_type<func_res_type, Level>>;

            if (empty()) {
                return returned_type();
            }

            returned_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename returned_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template pageop<Level - 1, Func, Args...>(
                    page_size, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Func, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr auto movop(
            size_type axis, interval_type window, bool bounded, Func&& func, Args&&... args) const
        {
            using func_res_type = std::invoke_result_t<Func, inner_this_type<Level>, Args...>;
            using movop_type = inner_replaced_type<func_res_type, Level>;

            if (empty()) {
                return movop_type();
            }

            movop_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename movop_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template movop<Level - 1, Func, Args...>(
                    axis, window, bounded, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr auto movop(
            size_type axis, interval_type window, bool bounded, Func&& func, Args&&... args) const
        {
            using func_res_type = std::invoke_result_t<Func, this_type, Args...>;
            using movop_type = replaced_type<func_res_type>;

            if (empty()) {
                return movop_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            size_type axis_dim = *std::next(hdr_.dims().cbegin(), axis);

            ranger_type rgr(hdr_, axis, window, bounded);

            size_type res_numel = bounded ? axis_dim - window.stop() + window.start() : axis_dim;

            movop_type res({res_numel});
            typename movop_type::indexer_type res_gen(res.header());

            for (; rgr && res_gen; ++rgr, ++res_gen) {
                res[*res_gen]
                    = func((*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())], std::forward<Args>(args)...);
            }

            return res;
        }

        template <typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, inner_this_type<this_type::depth>, Args...>)
        [[nodiscard]] constexpr auto movop(
            size_type axis, interval_type window, bool bounded, Func&& func, Args&&... args) const
        {
            return movop<this_type::depth>(
                axis, window, bounded, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename ReduceFunc, typename TransformFunc, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<TransformFunc, inner_this_type<Level>, Args...>
                && invocable_no_arrnd<ReduceFunc, std::invoke_result_t<TransformFunc, inner_this_type<Level>, Args...>,
                    std::invoke_result_t<TransformFunc, inner_this_type<Level>, Args...>>)
        [[nodiscard]] constexpr auto cumop(size_type axis, interval_type window, bool bounded, ReduceFunc&& rfunc,
            TransformFunc&& tfunc, Args&&... args) const
        {
            using trans_res_type = std::invoke_result_t<TransformFunc, inner_this_type<Level>, Args...>;
            using acc_res_type = std::invoke_result_t<ReduceFunc, trans_res_type, trans_res_type>;
            using cumop_type = inner_replaced_type<acc_res_type, Level>;

            if (empty()) {
                return cumop_type();
            }

            cumop_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename cumop_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template cumop<Level - 1, ReduceFunc, TransformFunc, Args...>(axis,
                    window, bounded, std::forward<ReduceFunc>(rfunc), std::forward<TransformFunc>(tfunc),
                    std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, typename ReduceFunc, typename TransformFunc, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<TransformFunc, inner_this_type<Level>, Args...>
                && invocable_no_arrnd<ReduceFunc, std::invoke_result_t<TransformFunc, inner_this_type<Level>, Args...>,
                    std::invoke_result_t<TransformFunc, inner_this_type<Level>, Args...>>)
        [[nodiscard]] constexpr auto cumop(size_type axis, interval_type window, bool bounded, ReduceFunc&& rfunc,
            TransformFunc&& tfunc, Args&&... args) const
        {
            using trans_res_type = std::invoke_result_t<TransformFunc, this_type, Args...>;
            using acc_res_type = std::invoke_result_t<ReduceFunc, trans_res_type, trans_res_type>;
            using cumop_type = replaced_type<acc_res_type>;

            if (empty()) {
                return cumop_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            size_type axis_dim = *std::next(hdr_.dims().cbegin(), axis);

            ranger_type rgr(hdr_, axis, window, bounded);

            size_type res_numel = bounded ? axis_dim - window.stop() + window.start() : axis_dim;

            cumop_type res({res_numel});
            typename cumop_type::indexer_type res_gen(res.header());

            if (res.empty()) {
                return res;
            }

            res[*res_gen] = tfunc((*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())], std::forward<Args>(args)...);
            auto prev = res[*res_gen];
            ++res_gen;
            ++rgr;

            for (; rgr && res_gen; ++rgr, ++res_gen) {
                res[*res_gen] = rfunc(
                    prev, tfunc((*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())], std::forward<Args>(args)...));
                prev = res[*res_gen];
            }

            return res;
        }

        template <typename ReduceFunc, typename TransformFunc, typename... Args>
            requires(invocable_no_arrnd<TransformFunc, inner_this_type<this_type::depth>, Args...>
                && invocable_no_arrnd<ReduceFunc,
                    std::invoke_result_t<TransformFunc, inner_this_type<this_type::depth>, Args...>,
                    std::invoke_result_t<TransformFunc, inner_this_type<this_type::depth>, Args...>>)
        [[nodiscard]] constexpr auto cumop(size_type axis, interval_type window, bool bounded, ReduceFunc&& rfunc,
            TransformFunc&& tfunc, Args&&... args) const
        {
            return cumop<this_type::depth>(axis, window, bounded, std::forward<ReduceFunc>(rfunc),
                std::forward<TransformFunc>(tfunc), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Func, this_type, Args...>)
        constexpr auto pageop(size_type page_size, Func&& func, Args&&... args) const
        {
            constexpr bool is_void_func = std::is_same_v<std::invoke_result_t<Func, this_type, Args...>, void>;
            using func_res_type
                = std::conditional_t<is_void_func, this_type, std::invoke_result_t<Func, this_type, Args...>>;
            using returned_type
                = std::conditional_t<arrnd_compliant<func_res_type> && same_depth<func_res_type, this_type>,
                    func_res_type, replaced_type<func_res_type>>;

            auto invoke_func = [&func, &args...](auto page) {
                if constexpr (arrnd_compliant<func_res_type> && same_depth<func_res_type, this_type>) {
                    if constexpr (is_void_func) {
                        func(page, std::forward<Args>(args)...);
                        return page;
                    } else {
                        return func(page, std::forward<Args>(args)...);
                    }
                } else { // in case that the returned type of func is not arrnd_compliant, then it should not be void returned type
                    return returned_type({1}, {func(page, std::forward<Args>(args)...)});
                }
            };

            if (empty()) {
                return returned_type{};
            }

            assert(hdr_.dims().size() >= page_size);

            if (hdr_.dims().size() == page_size) {
                if constexpr (is_void_func) {
                    invoke_func(*this);
                    return *this;
                } else {

                    return invoke_func(*this);
                }
            }

            auto expanded = expand<Level>(hdr_.dims().size() - (page_size + 1), 0, true);

            using trans_expanded_type = typename decltype(expanded)::template replaced_type<returned_type>;

            trans_expanded_type trans_expanded{};
            if constexpr (std::is_same_v<decltype(expanded), trans_expanded_type>) {
                trans_expanded = expanded;
            } else {
                trans_expanded = trans_expanded_type(expanded.header().dims());
            }

            typename decltype(expanded)::indexer_type exp_gen(expanded.header());
            typename trans_expanded_type::indexer_type trs_gen(trans_expanded.header());

            for (; exp_gen && trs_gen; ++exp_gen, ++trs_gen) {
                auto page = expanded[*exp_gen];

                auto trimed_page = page;
                size_type cycles_until_page = page.header().dims().size() - page_size;
                for (size_type i = 0; i < cycles_until_page; ++i) {
                    assert(trimed_page.header().dims().front() == 1);
                    trimed_page = trimed_page[interval_type::full()];
                }
                //auto page = expanded[*exp_gen];

                auto processed = invoke_func(trimed_page /*.template reshape<0>({page_dim1, page_dim2})*/);

                //processed.header() = typename decltype(page)::header_type(page.header().dims().cbegin(),
                //    std::next(page.header().dims().cbegin(), page.header().dims().size() - page_size))
                //                         .expand(processed.header().dims());

                typename decltype(page)::header_type::storage_type expanded_dims(
                    std::ssize(page.header().dims()) - page_size + std::ssize(processed.header().dims()));
                std::copy(page.header().dims().cbegin(),
                    std::next(page.header().dims().cbegin(), page.header().dims().size() - page_size), expanded_dims.begin());
                std::copy(processed.header().dims().begin(), processed.header().dims().end(),
                    std::next(expanded_dims.begin(), page.header().dims().size() - page_size));
                processed.header() = typename decltype(page)::header_type(expanded_dims);

                trans_expanded[*trs_gen] = processed;
            }

            if constexpr (is_void_func) {
                return *this;
            } else {

                return trans_expanded.template collapse<Level + 1>();
            }
        }

        template <typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, this_type, Args...>)
        constexpr auto pageop(size_type page_size, Func&& func, Args&&... args) const
        {
            return pageop<this_type::depth>(page_size, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level>
            requires(Level > 0)
        [[nodiscard]] constexpr auto pages(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            using pages_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return pages_type();
            }

            pages_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename pages_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template pages<Level - 1>(
                    axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
            }

            return res;
        }
        template <std::int64_t Level>
            requires(Level == 0)
        constexpr auto pages(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            using page_type = this_type;
            using pages_type = inner_replaced_type<page_type, Level>;

            if (empty()) {
                return pages_type();
            }

            //assert(hdr_.dims().size() >= 2);

            //if (hdr_.is_matrix()) {
            //    return pages_type({1}, {*this});
            //}

            // find axis
            // the first axis from the end bigger than one, and after the page

            pages_type pages = expand<Level>(axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);

            //auto fixed_axis = axis;
            //if (find_closest_axis_dim_bigger_than_one_to_the_left) {
            //    if (*std::next(hdr_.dims().cbegin(), fixed_axis) == 1) {
            //        for (size_type i = axis - 1; i >= 0; --i) {
            //            if (*std::next(hdr_.dims().cbegin(), i) > 1) {
            //                fixed_axis = i;
            //                break;
            //            }
            //        }
            //    }
            //}

            for (typename pages_type::indexer_type pgen(pages.header()); pgen; ++pgen) {
                //size_type cycles_until_page = pages[*pgen].header().dims().size() - 2;
                int count = axis + 1;
                while (count-- > 0 && pages[*pgen].header().dims().front() == 1) {
                    pages[*pgen] = pages[*pgen][interval_type::full()];
                }
                //for (size_type i = 0; i < cycles_until_page; ++i) {
                //    assert(pages[*pgen].header().dims().front() == 1);
                //    pages[*pgen] = pages[*pgen][interval<size_type>::full()];
                //}
            }

            return pages./*template */reshape/*<Level>*/(
                pages.header().dims().cbegin(), std::next(pages.header().dims().cbegin(), axis + 1));
        }
        [[nodiscard]] constexpr auto pages(size_type axis, size_type division = 0,
            bool find_closest_axis_dim_bigger_than_one_to_the_left = false) const
        {
            return pages<this_type::depth>(axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
        }

        template <std::int64_t Level, signed_integral_type_iterator AxesIt>
            requires(Level > 0)
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, size_type division) const
        {
            using split_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return split_type();
            }

            split_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename split_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template split<Level - 1>(first_axis, last_axis, division);
            }

            return res;
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt>
            requires(Level == 0)
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, size_type division) const
        {
            using split_type = replaced_type<this_type>;

            if (empty()) {
                return split_type();
            }

            assert(std::is_sorted(first_axis, last_axis));
            assert(std::adjacent_find(first_axis, last_axis) == last_axis);
            assert(std::distance(first_axis, last_axis) >= 0
                && std::distance(first_axis, last_axis) <= hdr_.dims().size());
            assert(std::all_of(first_axis, last_axis, [&](size_type axis) {
                return axis >= 0 && axis < hdr_.dims().size();
            }));

            assert(std::all_of(hdr_.dims().cbegin(), hdr_.dims().cend(), [division](size_type d) {
                return division > 0 && division <= d;
            }));

            // calculate dimensions according to number of slices in each dimension

            typename header_type::storage_type new_dims(hdr_.dims().size());
            if (std::distance(first_axis, last_axis) == 0) {
                std::fill(new_dims.begin(), new_dims.end(), division);
            } else {
                std::fill(new_dims.begin(), new_dims.end(), 1);
                std::for_each(first_axis, last_axis, [&](size_type axis) {
                    *std::next(new_dims.begin(), axis) = division;
                });
            }

            // --------------------------------------------------------------------

            //size_type assumed_num_slices = static_cast<size_type>(std::pow(division, hdr_.dims().size()));

            split_type slices(new_dims);
            typename split_type::indexer_type slc_gen(slices.header());

            size_type actual_num_slices = 0;

            std::function<void(this_type, size_type)> split_impl;

            split_impl
                = [&/*&slices, &slc_gen, &actual_num_slices, ind*/](this_type arr, size_type current_depth) -> void {
                if (arr.empty()) {
                    return;
                }

                if (current_depth == 0) {
                    assert(static_cast<bool>(slc_gen));

                    slices[*slc_gen] = arr;
                    ++slc_gen;
                    ++actual_num_slices;
                    return;
                }

                if (std::distance(first_axis, last_axis) > 0
                    && std::find(first_axis, last_axis, arr.header().dims().size() - current_depth) == last_axis) {
                    split_impl(arr(interval_type::full(), arr.header().dims().size() - current_depth),
                        current_depth - 1);
                    return;
                }

                size_type current_dim
                    = *std::next(arr.header().dims().cbegin(), arr.header().dims().size() - current_depth);

                auto exp = arr.template expand<0>(arr.header().dims().size() - current_depth, division);
                typename decltype(exp)::indexer_type exp_gen(exp.header());

                for (; exp_gen; ++exp_gen) {
                    split_impl(exp[*exp_gen], current_depth - 1);
                }
            };

            split_impl(*this, hdr_.dims().size());

            assert(/*assumed_num_slices >= actual_num_slices*/ actual_num_slices == slices.header().numel());

            //if (assumed_num_slices > actual_num_slices) {
            //return slices.template resize<Level>(new_dims/*{actual_num_slices}*/);
            //}
            return slices;
        }
        template <signed_integral_type_iterator AxesIt>
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, size_type division) const
        {
            return split<this_type::depth>(first_axis, last_axis, division);
        }
        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(const Cont& axes, size_type division) const
        {
            return split<Level>(std::begin(axes), std::end(axes), division);
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, size_type division) const
        {
            return split<Level>(axes.begin(), axes.end(), division);
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const U (&axes)[M], size_type division) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), division);
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(const Cont& axes, size_type division) const
        {
            return split<this_type::depth>(std::begin(axes), std::end(axes), division);
        }
        [[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, size_type division) const
        {
            return split<this_type::depth>(axes.begin(), axes.end(), division);
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const U (&axes)[M], size_type division) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), division);
        //}

        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
            requires(Level > 0)
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            using split_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return split_type();
            }

            split_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename split_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template split<Level - 1>(first_axis, last_axis, first_ind, last_ind);
            }

            return res;
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
            requires(Level == 0)
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            using split_type = replaced_type<this_type>;

            if (empty()) {
                return split_type();
            }

            assert(std::is_sorted(first_ind, last_ind));
            assert(std::adjacent_find(first_ind, last_ind) == last_ind);

            assert(std::is_sorted(first_axis, last_axis));
            assert(std::adjacent_find(first_axis, last_axis) == last_axis);
            assert(std::distance(first_axis, last_axis) >= 0
                && std::distance(first_axis, last_axis) <= hdr_.dims().size());
            assert(std::all_of(first_axis, last_axis, [&](size_type axis) {
                return axis >= 0 && axis < hdr_.dims().size();
            }));

            // calculate dimensions according to number of slices in each dimension

            typename header_type::storage_type new_dims(hdr_.dims().size());
            std::fill(new_dims.begin(), new_dims.end(), 1);
            auto calc_num_slices_at_dim = [&](auto indf, auto indl) {
                if (*indf == 0) {
                    return std::distance(indf, indl);
                }
                return std::distance(indf, indl) + 1;
            };
            if (std::distance(first_axis, last_axis) == 0) {
                std::for_each(new_dims.begin(), new_dims.end(), [&](size_type& d) {
                    d = calc_num_slices_at_dim(first_ind, last_ind);
                });
            } else {
                std::for_each(first_axis, last_axis, [&](size_type axis) {
                    *std::next(new_dims.begin(), axis) = calc_num_slices_at_dim(first_ind, last_ind);
                });
            }

            // --------------------------------------------------------------------

            /*size_type assumed_num_slices
                = static_cast<size_type>(std::pow(std::distance(first_ind, last_ind) + 1, hdr_.dims().size()));*/

            split_type slices(new_dims);
            typename split_type::indexer_type slc_gen(slices.header());

            size_type actual_num_slices = 0;

            std::function<void(this_type, size_type)> split_impl;

            split_impl
                = [&/*&slices, &slc_gen, &actual_num_slices, ind*/](this_type arr, size_type current_depth) -> void {
                if (arr.empty()) {
                    return;
                }

                if (current_depth == 0) {
                    assert(static_cast<bool>(slc_gen));

                    slices[*slc_gen] = arr;
                    ++slc_gen;
                    ++actual_num_slices;
                    return;
                }

                if (std::distance(first_axis, last_axis) > 0
                    && std::find(first_axis, last_axis, arr.header().dims().size() - current_depth) == last_axis) {
                    split_impl(arr(interval_type::full(), arr.header().dims().size() - current_depth),
                        current_depth - 1);
                    return;
                }

                size_type current_dim
                    = *std::next(arr.header().dims().cbegin(), arr.header().dims().size() - current_depth);

                assert(std::distance(first_ind, last_ind) > 0 && std::distance(first_ind, last_ind) <= current_dim);
                assert(std::all_of(first_ind, last_ind, [current_dim](size_type ind) {
                    return ind >= 0 && ind < current_dim;
                }));

                size_type prev_ind = *first_ind;
                auto ind_it = first_ind;
                ++ind_it;
                if (prev_ind > 0) {
                    split_impl(arr(interval_type{0, prev_ind}, arr.header().dims().size() - current_depth),
                        current_depth - 1);
                }
                size_type current_ind = prev_ind;
                for (; ind_it != last_ind; ++ind_it) {
                    current_ind = *ind_it;
                    if (prev_ind < current_ind) {
                        split_impl(
                            arr(interval_type{prev_ind, current_ind}, arr.header().dims().size() - current_depth),
                            current_depth - 1);
                    }
                    prev_ind = current_ind;
                }
                if (current_ind < current_dim) {
                    split_impl(
                        arr(interval_type{current_ind, current_dim}, arr.header().dims().size() - current_depth),
                        current_depth - 1);
                }
            };

            split_impl(*this, hdr_.dims().size());

            assert(/*assumed_num_slices >= actual_num_slices*/ actual_num_slices == slices.header().numel());

            //if (assumed_num_slices > actual_num_slices) {
            //return slices.template resize<Level>(new_dims/*{actual_num_slices}*/);
            //}
            return slices;
        }
        template <signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            return split<this_type::depth>(first_axis, last_axis, first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, const Cont& inds) const
        {
            return split<Level>(first_axis, last_axis, std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt>
        [[nodiscard]] constexpr auto split(
            AxesIt first_axis, AxesIt last_axis, std::initializer_list<size_type> inds) const
        {
            return split<Level>(first_axis, last_axis, inds.begin(), inds.end());
        }
        //template <std::int64_t Level, signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, const U (&inds)[M]) const
        //{
        //    return split<Level>(first_axis, last_axis, std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, const Cont& inds) const
        {
            return split<this_type::depth>(first_axis, last_axis, std::begin(inds), std::end(inds));
        }
        template <signed_integral_type_iterator AxesIt>
        [[nodiscard]] constexpr auto split(
            AxesIt first_axis, AxesIt last_axis, std::initializer_list<size_type> inds) const
        {
            return split<this_type::depth>(first_axis, last_axis, inds.begin(), inds.end());
        }
        //template <signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(AxesIt first_axis, AxesIt last_axis, const U (&inds)[M]) const
        //{
        //    return split<this_type::depth>(first_axis, last_axis, std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level, signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return split<Level>(std::begin(axes), std::end(axes), first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, const Cont& inds) const
        {
            return split<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level, signed_integral_type_iterable AxesCont>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, std::initializer_list<size_type> inds) const
        {
            return split<Level>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        }
        //template <std::int64_t Level, signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const AxesCont& axes, const U (&inds)[M]) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return split<this_type::depth>(std::begin(axes), std::end(axes), first_ind, last_ind);
        }
        template <signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, const Cont& inds) const
        {
            return split<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        }
        template <signed_integral_type_iterable AxesCont>
        [[nodiscard]] constexpr auto split(const AxesCont& axes, std::initializer_list<size_type> inds) const
        {
            return split<this_type::depth>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        }
        //template <signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const AxesCont& axes, const U (&inds)[M]) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto split(
            std::initializer_list<size_type> axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return split<Level>(axes.begin(), axes.end(), first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, const Cont& inds) const
        {
            return split<Level>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto split(
            std::initializer_list<size_type> axes, std::initializer_list<size_type> inds) const
        {
            return split<Level>(axes.begin(), axes.end(), inds.begin(), inds.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, const U (&inds)[M]) const
        //{
        //    return split<Level>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto split(
            std::initializer_list<size_type> axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return split<this_type::depth>(axes.begin(), axes.end(), first_ind, last_ind);
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, const Cont& inds) const
        {
            return split<this_type::depth>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        }
        [[nodiscard]] constexpr auto split(
            std::initializer_list<size_type> axes, std::initializer_list<size_type> inds) const
        {
            return split<this_type::depth>(axes.begin(), axes.end(), inds.begin(), inds.end());
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(std::initializer_list<size_type> axes, const U (&inds)[M]) const
        //{
        //    return split<this_type::depth>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        //}

        //template <std::int64_t Level, std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], IndsIt first_ind, IndsIt last_ind) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), first_ind, last_ind);
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], const Cont& inds) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], std::initializer_list<size_type> inds) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], const U (&inds)[M]) const
        //{
        //    return split<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], IndsIt first_ind, IndsIt last_ind) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), first_ind, last_ind);
        //}
        //template <std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], const Cont& inds) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::integral V, std::int64_t N>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], std::initializer_list<size_type> inds) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        //}
        //template <std::integral V, std::int64_t N, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto split(const V (&axes)[N], const U (&inds)[M]) const
        //{
        //    return split<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
            requires(Level > 0)
        [[nodiscard]] constexpr auto exclude(
            AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            using exclude_type = inner_replaced_type<inner_this_type<Level>, Level>;

            if (empty()) {
                return exclude_type();
            }

            exclude_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename exclude_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template exclude<Level - 1>(first_axis, last_axis, first_ind, last_ind);
            }

            return res;
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
            requires(Level == 0)
        [[nodiscard]] constexpr auto exclude(
            AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            using exclude_type = replaced_type<this_type>;

            if (empty()) {
                return exclude_type();
            }

            assert(std::is_sorted(first_ind, last_ind));
            assert(std::adjacent_find(first_ind, last_ind) == last_ind);

            assert(std::is_sorted(first_axis, last_axis));
            assert(std::adjacent_find(first_axis, last_axis) == last_axis);
            assert(std::distance(first_axis, last_axis) >= 0
                && std::distance(first_axis, last_axis) <= hdr_.dims().size());
            assert(std::all_of(first_axis, last_axis, [&](size_type axis) {
                return axis >= 0 && axis < hdr_.dims().size();
            }));

            // calculate dimensions according to number of slices in each dimension

            typename header_type::storage_type new_dims(hdr_.dims().size());
            std::fill(new_dims.begin(), new_dims.end(), 1);
            auto calc_num_slices_at_dim = [&](auto indf, auto indl, size_type dim) {
                size_type num_slices = 0;
                size_type prev = *indf;
                if (prev > 0) {
                    ++num_slices;
                }
                ++indf;
                size_type current_ind = prev;
                for (auto it = indf; it != indl; ++it) {
                    current_ind = *it;
                    if (prev < current_ind + 1) {
                        ++num_slices;
                    }
                    prev = current_ind;
                }
                if (current_ind + 1 < dim) {
                    ++num_slices;
                }
                return num_slices;
            };
            if (std::distance(first_axis, last_axis) == 0) {
                std::transform(hdr_.dims().cbegin(), hdr_.dims().cend(), new_dims.begin(), [&](size_type dim) {
                    return calc_num_slices_at_dim(first_ind, last_ind, dim);
                });
            } else {
                std::for_each(first_axis, last_axis, [&](size_type axis) {
                    *std::next(new_dims.begin(), axis)
                        = calc_num_slices_at_dim(first_ind, last_ind, *std::next(hdr_.dims().cbegin(), axis));
                });
            }

            // --------------------------------------------------------------------

            /*size_type assumed_num_slices
                = static_cast<size_type>(std::pow(std::distance(first_ind, last_ind) + 1, hdr_.dims().size()));*/

            exclude_type slices(new_dims);
            typename exclude_type::indexer_type slc_gen(slices.header());

            size_type actual_num_slices = 0;

            std::function<void(this_type, size_type)> exclude_impl;

            exclude_impl
                = [&/*&slices, &slc_gen, &actual_num_slices, ind*/](this_type arr, size_type current_depth) -> void {
                if (arr.empty()) {
                    return;
                }

                if (current_depth == 0) {
                    assert(static_cast<bool>(slc_gen));

                    slices[*slc_gen] = arr;
                    ++slc_gen;
                    ++actual_num_slices;

                    return;
                }

                if (std::distance(first_axis, last_axis) > 0
                    && std::find(first_axis, last_axis, arr.header().dims().size() - current_depth) == last_axis) {
                    exclude_impl(arr(interval_type::full(), arr.header().dims().size() - current_depth),
                        current_depth - 1);
                    return;
                }

                size_type current_dim
                    = *std::next(arr.header().dims().cbegin(), arr.header().dims().size() - current_depth);

                assert(std::distance(first_ind, last_ind) > 0 && std::distance(first_ind, last_ind) <= current_dim);
                assert(std::all_of(first_ind, last_ind, [current_dim](size_type ind) {
                    return ind >= 0 && ind < current_dim;
                }));

                size_type prev_ind = *first_ind;
                auto ind_it = first_ind;
                ++ind_it;
                if (prev_ind > 0) {
                    exclude_impl(arr(interval_type{0, prev_ind}, arr.header().dims().size() - current_depth),
                        current_depth - 1);
                }
                size_type current_ind = prev_ind;
                for (; ind_it != last_ind; ++ind_it) {
                    current_ind = *ind_it;
                    if (prev_ind + 1 < current_ind) {
                        exclude_impl(arr(interval_type{prev_ind + 1, current_ind},
                                         arr.header().dims().size() - current_depth),
                            current_depth - 1);
                    }
                    prev_ind = current_ind;
                }
                if (current_ind + 1 < current_dim) {
                    exclude_impl(arr(interval_type{current_ind + 1, current_dim},
                                     arr.header().dims().size() - current_depth),
                        current_depth - 1);
                }
            };

            exclude_impl(*this, hdr_.dims().size());

            assert(/*assumed_num_slices >= actual_num_slices*/ actual_num_slices == slices.header().numel());

            //if (assumed_num_slices > actual_num_slices) {
            //return slices.template resize<Level>(new_dims/*{actual_num_slices}*/);
            //}
            return slices;
        }
        template <signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto exclude(
            AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind) const
        {
            return exclude<this_type::depth>(first_axis, last_axis, first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(AxesIt first_axis, AxesIt last_axis, const Cont& inds) const
        {
            return exclude<Level>(first_axis, last_axis, std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level, signed_integral_type_iterator AxesIt>
        [[nodiscard]] constexpr auto exclude(
            AxesIt first_axis, AxesIt last_axis, std::initializer_list<size_type> inds) const
        {
            return exclude<Level>(first_axis, last_axis, inds.begin(), inds.end());
        }
        //template <std::int64_t Level, signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(AxesIt first_axis, AxesIt last_axis, const U (&inds)[M]) const
        //{
        //    return exclude<Level>(first_axis, last_axis, std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(AxesIt first_axis, AxesIt last_axis, const Cont& inds) const
        {
            return exclude<this_type::depth>(first_axis, last_axis, std::begin(inds), std::end(inds));
        }
        template <signed_integral_type_iterator AxesIt>
        [[nodiscard]] constexpr auto exclude(
            AxesIt first_axis, AxesIt last_axis, std::initializer_list<size_type> inds) const
        {
            return exclude<this_type::depth>(first_axis, last_axis, inds.begin(), inds.end());
        }
        //template <signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(AxesIt first_axis, AxesIt last_axis, const U (&inds)[M]) const
        //{
        //    return exclude<this_type::depth>(first_axis, last_axis, std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level, signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return exclude<Level>(std::begin(axes), std::end(axes), first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, const Cont& inds) const
        {
            return exclude<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level, signed_integral_type_iterable AxesCont>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, std::initializer_list<size_type> inds) const
        {
            return exclude<Level>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        }
        //template <std::int64_t Level, signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(const AxesCont& axes, const U (&inds)[M]) const
        //{
        //    return exclude<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return exclude<this_type::depth>(std::begin(axes), std::end(axes), first_ind, last_ind);
        }
        template <signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, const Cont& inds) const
        {
            return exclude<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        }
        template <signed_integral_type_iterable AxesCont>
        [[nodiscard]] constexpr auto exclude(const AxesCont& axes, std::initializer_list<size_type> inds) const
        {
            return exclude<this_type::depth>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        }
        //template <signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(const AxesCont& axes, const U (&inds)[M]) const
        //{
        //    return exclude<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level, signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto exclude(
            std::initializer_list<size_type> axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return exclude<Level>(axes.begin(), axes.end(), first_ind, last_ind);
        }
        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(std::initializer_list<size_type> axes, const Cont& inds) const
        {
            return exclude<Level>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto exclude(
            std::initializer_list<size_type> axes, std::initializer_list<size_type> inds) const
        {
            return exclude<Level>(axes.begin(), axes.end(), inds.begin(), inds.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(std::initializer_list<size_type> axes, const U (&inds)[M]) const
        //{
        //    return exclude<Level>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        //}
        template <signed_integral_type_iterator IndsIt>
        [[nodiscard]] constexpr auto exclude(
            std::initializer_list<size_type> axes, IndsIt first_ind, IndsIt last_ind) const
        {
            return exclude<this_type::depth>(axes.begin(), axes.end(), first_ind, last_ind);
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto exclude(std::initializer_list<size_type> axes, const Cont& inds) const
        {
            return exclude<this_type::depth>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        }
        [[nodiscard]] constexpr auto exclude(
            std::initializer_list<size_type> axes, std::initializer_list<size_type> inds) const
        {
            return exclude<this_type::depth>(axes.begin(), axes.end(), inds.begin(), inds.end());
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(std::initializer_list<size_type> axes, const U (&inds)[M]) const
        //{
        //    return exclude<this_type::depth>(axes.begin(), axes.end(), std::begin(inds), std::end(inds));
        //}

        //template <std::int64_t Level, std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], IndsIt first_ind, IndsIt last_ind) const
        //{
        //    return exclude<Level>(std::begin(axes), std::end(axes), first_ind, last_ind);
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], const Cont& inds) const
        //{
        //    return exclude<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], std::initializer_list<size_type> inds) const
        //{
        //    return exclude<Level>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        //}
        //template <std::int64_t Level, std::integral V, std::int64_t N, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], const U (&inds)[M]) const
        //{
        //    return exclude<Level>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], IndsIt first_ind, IndsIt last_ind) const
        //{
        //    return exclude<this_type::depth>(std::begin(axes), std::end(axes), first_ind, last_ind);
        //}
        //template <std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], const Cont& inds) const
        //{
        //    return exclude<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}
        //template <std::integral V, std::int64_t N>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], std::initializer_list<size_type> inds) const
        //{
        //    return exclude<this_type::depth>(std::begin(axes), std::end(axes), inds.begin(), inds.end());
        //}
        //template <std::integral V, std::int64_t N, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto exclude(const V (&axes)[N], const U (&inds)[M]) const
        //{
        //    return exclude<this_type::depth>(std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
        //}

        template <std::int64_t Level>
            requires(Level > 0 && this_type::depth > 1)
        [[nodiscard]] constexpr value_type merge() const
        {
            if (empty()) {
                return value_type();
            }

            value_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename value_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template merge<Level - 1>();
            }

            return res;
        }
        template <std::int64_t Level>
            requires(Level == 0 && !this_type::is_flat)
        [[nodiscard]] constexpr value_type merge() const
        {
            if (empty()) {
                return value_type();
            }

            this_type res = reduce<Level>(hdr_.dims().size() - 1, [&](const value_type& a, const value_type& b) {
                return a./*template */append/*<Level>*/(b, hdr_.dims().size() - 1);
            });

            for (size_type axis = hdr_.dims().size() - 2; axis >= 0; --axis) {
                res = res.template reduce<Level>(axis, [axis](const value_type& a, const value_type& b) {
                    return a./*template */append/*<Level>*/(b, axis);
                });
            }

            assert(res.header().numel() == 1);

            return res(0);
        }
        [[nodiscard]] constexpr auto merge() const
            requires(!this_type::is_flat)
        {
            return merge<this_type::depth - 1>();
        }

        template <std::int64_t Level>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type squeeze() const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename this_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template squeeze<Level - 1>();
            }

            return res;
        }

        template <std::int64_t Level>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type squeeze() const
        {
            this_type squeezed{};
            squeezed.hdr_ = hdr_.squeeze();
            squeezed.buffsp_ = buffsp_;
            squeezed.is_creator_valid_ = original_valid_creator_;
            squeezed.creator_ = this;
            return squeezed;
        }

        [[nodiscard]] constexpr auto squeeze() const
        {
            return squeeze<this_type::depth>();
        }

        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Comp, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res = clone();

            std::sort(res.begin(), res.end(), [&comp, &args...](const auto& lhs, const auto& rhs) {
                return comp(lhs, rhs, std::forward<Args>(args)...);
            });

            return res;
        }
        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Comp, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template sort<Level - 1, Comp, Args...>(
                    std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, inner_value_type<this_type::depth>, inner_value_type<this_type::depth>,
                Args...>
        [[nodiscard]] constexpr this_type sort(Comp&& comp, Args&&... args) const
        {
            return sort<this_type::depth, Comp, Args...>(std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Comp, inner_this_type<Level>, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(size_type axis, Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            auto expanded = expand<Level>(axis);

            auto sorted = expanded.template sort<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);

            auto reduced = sorted.template reduce<Level>([axis](const auto& acc, const auto& cur) {
                return acc./*template */append/*<Level>*/(cur, axis);
            });

            return reduced./*template */reshape/*<Level>*/(hdr_.dims());
        }
        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Comp, inner_this_type<Level>, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(size_type axis, Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template sort<Level - 1, Comp, Args...>(
                    axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, inner_this_type<this_type::depth>, inner_this_type<this_type::depth>,
                Args...>
        [[nodiscard]] constexpr this_type sort(size_type axis, Comp&& comp, Args&&... args) const
        {
            return sort<this_type::depth, Comp, Args...>(axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Comp, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr bool is_sorted(Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            return std::is_sorted(cbegin(), cend(), [&comp, &args...](const auto& lhs, const auto& rhs) {
                return comp(lhs, rhs, std::forward<Args>(args)...);
            });
        }
        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Comp, inner_value_type<Level>, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr auto is_sorted(Comp&& comp, Args&&... args) const
        {
            using is_sorted_type = inner_replaced_type<bool, Level - 1>;

            if (empty()) {
                return is_sorted_type();
            }

            is_sorted_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template is_sorted<Level - 1, Comp, Args...>(
                    std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, inner_value_type<this_type::depth>, inner_value_type<this_type::depth>,
                Args...>
        [[nodiscard]] constexpr auto is_sorted(Comp&& comp, Args&&... args) const
        {
            return is_sorted<this_type::depth, Comp, Args...>(std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Comp, inner_this_type<Level>, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr bool is_sorted(size_type axis, Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            auto expanded = expand<Level>(axis);

            return expanded.template is_sorted<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);

            /*auto sorted = expanded.template sort<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);

            auto reduced = sorted.template reduce<Level>([axis](const auto& acc, const auto& cur) {
                return acc.template append<Level>(cur, axis);
            });

            return reduced.template reshape<Level>(hdr_.dims());*/
        }
        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Comp, inner_this_type<Level>, inner_this_type<Level>, Args...>)
        [[nodiscard]] constexpr auto is_sorted(size_type axis, Comp&& comp, Args&&... args) const
        {
            using is_sorted_type = inner_replaced_type<bool, Level - 1>;

            if (empty()) {
                return is_sorted_type();
            }

            is_sorted_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template is_sorted<Level - 1, Comp, Args...>(
                    axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, inner_this_type<this_type::depth>, inner_this_type<this_type::depth>,
                Args...>
        [[nodiscard]] constexpr auto is_sorted(size_type axis, Comp&& comp, Args&&... args) const
        {
            return is_sorted<this_type::depth, Comp, Args...>(
                axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type reorder(const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            assert(std::distance(first_order, last_order) == hdr_.numel());

            replaced_type<size_type> order({std::distance(first_order, last_order)}, first_order, last_order);

            auto reordered = clone();

            auto z = zip(zipped_cont(order), zipped_cont(reordered));
            std::sort(z.begin(), z.end(), [](const auto& t1, const auto& t2) {
                return std::get<0>(t1) < std::get<0>(t2);
            });

            return reordered;

            //return reduced.template reshape<Level>(hdr_.dims());
        }
        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type reorder(const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reorder<Level - 1, InputIt>(first_order, last_order);
            }

            return res;
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr this_type reorder(const InputIt& first_order, const InputIt& last_order) const
        {
            return reorder<this_type::depth, InputIt>(first_order, last_order);
        }

        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto reorder(const Cont& order) const
        {
            return reorder<Level>(std::begin(order), std::end(order));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto reorder(std::initializer_list<size_type> order) const
        {
            return reorder<Level>(order.begin(), order.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto reorder(const U (&order)[M]) const
        //{
        //    return reorder<Level>(std::begin(order), std::end(order));
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto reorder(const Cont& order) const
        {
            return reorder<this_type::depth>(std::begin(order), std::end(order));
        }
        [[nodiscard]] constexpr auto reorder(std::initializer_list<size_type> order) const
        {
            return reorder<this_type::depth>(order.begin(), order.end());
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto reorder(const U (&order)[M]) const
        //{
        //    return reorder<this_type::depth>(std::begin(order), std::end(order));
        //}

        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type
            reorder(size_type axis, const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());
            assert(std::distance(first_order, last_order) == *std::next(hdr_.dims().cbegin(), axis));

            replaced_type<size_type> order({std::distance(first_order, last_order)}, first_order, last_order);

            auto expanded = expand<Level>(axis);

            auto z = zip(zipped_cont(order), zipped_cont(expanded));
            std::sort(z.begin(), z.end(), [](const auto& t1, const auto& t2) {
                return std::get<0>(t1) < std::get<0>(t2);
            });

            auto reordered = expanded.template reduce<Level>([axis](const auto& acc, const auto& cur) {
                return acc./*template */append/*<Level>*/(cur, axis);
            });

            return reordered;
            //return reduced.template reshape<Level>(hdr_.dims());
        }
        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type
            reorder(size_type axis, const InputIt& first_order, const InputIt& last_order) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reorder<Level - 1, InputIt>(axis, first_order, last_order);
            }

            return res;
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr this_type reorder(
            size_type axis, const InputIt& first_order, const InputIt& last_order) const
        {
            return reorder<this_type::depth, InputIt>(axis, first_order, last_order);
        }

        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto reorder(size_type axis, const Cont& order) const
        {
            return reorder<Level>(axis, std::begin(order), std::end(order));
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto reorder(size_type axis, std::initializer_list<size_type> order) const
        {
            return reorder<Level>(axis, order.begin(), order.end());
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto reorder(size_type axis, const U (&order)[M]) const
        //{
        //    return reorder<Level>(axis, std::begin(order), std::end(order));
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto reorder(size_type axis, const Cont& order) const
        {
            return reorder<this_type::depth>(axis, std::begin(order), std::end(order));
        }
        [[nodiscard]] constexpr auto reorder(size_type axis, std::initializer_list<size_type> order) const
        {
            return reorder<this_type::depth>(axis, order.begin(), order.end());
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto reorder(size_type axis, const U (&order)[M]) const
        //{
        //    return reorder<this_type::depth>(axis, std::begin(order), std::end(order));
        //}

        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level == 0)
        [[nodiscard]] constexpr auto adjacent_indices(
            const InputIt& first_sub, const InputIt& last_sub, size_type offset = 1) const
        {
            using returned_type = replaced_type<size_type>;

            if (empty()) {
                return returned_type();
            }

            assert(std::distance(first_sub, last_sub) == std::ssize(hdr_.dims()));
            assert(offset > 0);

            auto compute_num_adj = [](size_type ndims, size_type offset) {
                size_type base1 = 3 + 2 * (offset - 1);
                size_type base2 = 3 + 2 * (offset - 2);
                return static_cast<size_type>(std::pow(base1, ndims) - std::pow(base2, ndims));
            };

            returned_type res({compute_num_adj(std::ssize(hdr_.dims()), offset)});
            size_type actual_num_adj = 0;

            std::function<void(returned_type, size_type, bool)> impl;

            impl = [&](returned_type subs, size_type perm_pos, bool used_offset) {
                if (perm_pos == std::ssize(hdr_.dims())) {
                    return;
                }

                for (size_type i = -offset; i <= offset; ++i) {
                    size_type abs_i = static_cast<size_type>(std::abs(i));

                    if (abs_i > 0 && abs_i <= offset) {
                        auto new_subs = subs.clone();
                        new_subs[perm_pos] += i;
                        if (new_subs[perm_pos] >= 0
                            && new_subs[perm_pos] < *std::next(hdr_.dims().cbegin(), perm_pos)) {
                            if (used_offset || abs_i == offset) {
                                res[actual_num_adj++] = hdr_.subs2ind(new_subs.cbegin(), new_subs.cend());
                            }
                            impl(new_subs, perm_pos + 1, abs_i == offset);
                        }
                    } else {
                        impl(subs, perm_pos + 1, used_offset);
                    }
                }
            };

            impl(returned_type({std::distance(first_sub, last_sub)}, first_sub, last_sub), 0, false);

            if (res.header().numel() > actual_num_adj) {
                return res./*template */resize/*<Level>*/({actual_num_adj});
            }
            return res;
        }
        template <std::int64_t Level, signed_integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr auto adjacent_indices(
            const InputIt& first_sub, const InputIt& last_sub, size_type offset = 1) const
        {
            using returned_type = inner_replaced_type<size_type, Level>;

            if (empty()) {
                return returned_type();
            }

            returned_type res(hdr_.subs().cbegin(), hdr_.subs().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen]
                    = (*this)[*gen].template adjacent_indices<Level - 1, InputIt>(first_sub, last_sub, offset);
            }

            return res;
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto adjacent_indices(
            const InputIt& first_sub, const InputIt& last_sub, size_type offset = 1) const
        {
            return adjacent_indices<this_type::depth, InputIt>(first_sub, last_sub, offset);
        }

        template <std::int64_t Level, signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto adjacent_indices(const Cont& subs, size_type offset = 1) const
        {
            return adjacent_indices<Level>(std::begin(subs), std::end(subs), offset);
        }
        template <std::int64_t Level>
        [[nodiscard]] constexpr auto adjacent_indices(std::initializer_list<size_type> subs, size_type offset = 1) const
        {
            return adjacent_indices<Level>(subs.begin(), subs.end(), offset);
        }
        //template <std::int64_t Level, std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto adjacent_indices(const U (&subs)[M], size_type offset = 1) const
        //{
        //    return adjacent_indices<Level>(std::begin(subs), std::end(subs), offset);
        //}
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto adjacent_indices(const Cont& subs, size_type offset = 1) const
        {
            return adjacent_indices<this_type::depth>(std::begin(subs), std::end(subs), offset);
        }
        [[nodiscard]] constexpr auto adjacent_indices(std::initializer_list<size_type> subs, size_type offset = 1) const
        {
            return adjacent_indices<this_type::depth>(subs.begin(), subs.end(), offset);
        }
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto adjacent_indices(const U (&subs)[M], size_type offset = 1) const
        //{
        //    return adjacent_indices<this_type::depth>(std::begin(subs), std::end(subs), offset);
        //}

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr bool all_match(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                if (!pred((*this)[*gen], std::forward<Args>(args)...)) {
                    return false;
                }
            }

            return true;
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr bool all_match(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                if (!(*this)[*gen].template all_match<Level - 1, Pred, Args...>(
                        std::forward<Pred>(pred), std::forward<Args>(args)...)) {
                    return false;
                }
            }

            return true;
        }

        template <typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>, Args...>
        [[nodiscard]] constexpr bool all_match(Pred&& pred, Args&&... args) const
        {
            return all_match<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Pred, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        [[nodiscard]] constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            if (empty() && arr.empty()) {
                return true;
            }

            if (empty() || arr.empty()) {
                return false;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return false;
            }

            indexer_type gen(hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                if (!pred((*this)[*gen], arr[*arr_gen], std::forward<Args>(args)...)) {
                    return false;
                }
            }

            return true;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Pred, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        [[nodiscard]] constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            if (empty() && arr.empty()) {
                return true;
            }

            if (empty() || arr.empty()) {
                return false;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return false;
            }

            indexer_type gen(hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                if (!(*this)[*gen].template all_match<Level - 1, typename ArCo::value_type, Pred, Args...>(
                        arr[*arr_gen], std::forward<Pred>(pred), std::forward<Args>(args)...)) {
                    return false;
                }
            }

            return true;
        }

        template <arrnd_compliant ArCo, typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>,
                typename ArCo::template inner_value_type<ArCo::depth>, Args...>
        [[nodiscard]] constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            return all_match<this_type::depth, Pred, ArCo, Args...>(
                arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level == 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr bool any_match(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                if (pred((*this)[*gen], std::forward<Args>(args)...)) {
                    return true;
                }
            }

            return false;
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(Level > 0 && invocable_no_arrnd<Pred, inner_value_type<Level>, Args...>)
        [[nodiscard]] constexpr bool any_match(Pred&& pred, Args&&... args) const
        {
            if (empty()) {
                return true;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                if ((*this)[*gen].template any_match<Level - 1, Pred, Args...>(
                        std::forward<Pred>(pred), std::forward<Args>(args)...)) {
                    return true;
                }
            }

            return false;
        }

        template <typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>, Args...>
        [[nodiscard]] constexpr bool any_match(Pred&& pred, Args&&... args) const
        {
            return any_match<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Pred, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        [[nodiscard]] constexpr bool any_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            if (empty() && arr.empty()) {
                return true;
            }

            if (empty() || arr.empty()) {
                return false;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return false;
            }

            indexer_type gen(hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                if (pred((*this)[*gen], arr[*arr_gen], std::forward<Args>(args)...)) {
                    return true;
                }
            }

            return false;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Pred, inner_value_type<Level>, typename ArCo::template inner_value_type<Level>,
                    Args...>)
        [[nodiscard]] constexpr bool any_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            if (empty() && arr.empty()) {
                return true;
            }

            if (empty() || arr.empty()) {
                return false;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return false;
            }

            indexer_type gen(hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                if ((*this)[*gen].template any_match<Level - 1, typename ArCo::value_type, Pred, Args...>(
                        arr[*arr_gen], std::forward<Pred>(pred), std::forward<Args>(args)...)) {
                    return true;
                }
            }

            return false;
        }

        template <arrnd_compliant ArCo, typename Pred, typename... Args>
            requires invocable_no_arrnd<Pred, inner_value_type<this_type::depth>,
                typename ArCo::template inner_value_type<ArCo::depth>, Args...>
        [[nodiscard]] constexpr bool any_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            return any_match<this_type::depth, Pred, ArCo, Args...>(
                arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr bool all() const
        {
            return all_match<Level>([](const auto& value) {
                return static_cast<bool>(value);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr replaced_type<bool> all(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                return a && b;
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr bool any() const
        {
            return any_match<Level>([](const auto& value) {
                return static_cast<bool>(value);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr replaced_type<bool> any(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                return a || b;
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto sum() const
        {
            return reduce<Level>([](const auto& a, const auto& b) {
                return a + b;
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto sum(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                return a + b;
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto min() const
        {
            return reduce<Level>([](const auto& a, const auto& b) {
                using std::min;
                return min(a, b);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto min(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                using std::min;
                return min(a, b);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto max() const
        {
            return reduce<Level>([](const auto& a, const auto& b) {
                using std::max;
                return max(a, b);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto max(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                using std::max;
                return max(a, b);
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto prod() const
        {
            return reduce<Level>([](const auto& a, const auto& b) {
                return a * b;
            });
        }

        template <std::int64_t Level = this_type::depth>
        [[nodiscard]] constexpr auto prod(size_type axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                return a * b;
            });
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr replaced_type<bool> close(const ArCo& arr,
            const compliant_tol_type<ArCo, Level>& atol = default_atol<compliant_tol_type<ArCo, Level>>(),
            const compliant_tol_type<ArCo, Level>& rtol = default_rtol<compliant_tol_type<ArCo, Level>>()) const
        {
            return transform<Level>(arr, [&atol, &rtol](const auto& a, const auto& b) {
                return oc::close(a, b, atol, rtol);
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr replaced_type<bool> close(const ArCo& arr,
            const compliant_tol_type<ArCo>& atol = default_atol<compliant_tol_type<ArCo>>(),
            const compliant_tol_type<ArCo>& rtol = default_rtol<compliant_tol_type<ArCo>>()) const
        {
            return close<this_type::depth>(arr, atol, rtol);
        }

        template <std::int64_t Level, typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr replaced_type<bool> close(const U& value,
            const tol_type<U, Level>& atol = default_atol<tol_type<U, Level>>(),
            const tol_type<U, Level>& rtol = default_rtol<tol_type<U, Level>>()) const
        {
            return transform<Level>(
                [&atol, &rtol](const auto& a, const auto& b) {
                    return oc::close(a, b, atol, rtol);
                },
                value);
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr replaced_type<bool> close(const U& value,
            const tol_type<U>& atol = default_atol<tol_type<U>>(),
            const tol_type<U>& rtol = default_rtol<tol_type<U>>()) const
        {
            return close<this_type::depth>(value, atol, rtol);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool all_equal(const ArCo& arr) const
        {
            return all_match<Level>(arr, [](const auto& a, const auto& b) {
                return a == b;
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool all_equal(const ArCo& arr) const
        {
            return all_equal<this_type::depth>(arr);
        }

        template <std::int64_t Level, typename U>
        [[nodiscard]] constexpr bool all_equal(const U& u) const
        {
            return all_match<Level>(
                [](const auto& a, const auto& b) {
                    return a == b;
                },
                u);
        }

        template <typename U>
        [[nodiscard]] constexpr bool all_equal(const U& u) const
        {
            return all_equal<this_type::depth>(u);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool all_close(const ArCo& arr,
            const compliant_tol_type<ArCo, Level>& atol = default_atol<compliant_tol_type<ArCo, Level>>(),
            const compliant_tol_type<ArCo, Level>& rtol = default_rtol<compliant_tol_type<ArCo, Level>>()) const
        {
            return all_match<Level>(arr, [&atol, &rtol](const auto& a, const auto& b) {
                return oc::close(a, b, atol, rtol);
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool all_close(const ArCo& arr,
            const compliant_tol_type<ArCo>& atol = default_atol<compliant_tol_type<ArCo>>(),
            const compliant_tol_type<ArCo>& rtol = default_rtol<compliant_tol_type<ArCo>>()) const
        {
            return all_close<this_type::depth>(arr, atol, rtol);
        }

        template <std::int64_t Level, typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr bool all_close(const U& u,
            const tol_type<U, Level>& atol = default_atol<tol_type<U, Level>>(),
            const tol_type<U, Level>& rtol = default_rtol<tol_type<U, Level>>()) const
        {
            return all_match<Level>(
                [&atol, &rtol](const auto& a, const auto& b) {
                    return oc::close(a, b, atol, rtol);
                },
                u);
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr bool all_close(const U& u, const tol_type<U>& atol = default_atol<tol_type<U>>(),
            const tol_type<U>& rtol = default_rtol<tol_type<U>>()) const
        {
            return all_close<this_type::depth>(u, atol, rtol);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool any_equal(const ArCo& arr) const
        {
            return any_match<Level>(arr, [](const auto& a, const auto& b) {
                return a == b;
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool any_equal(const ArCo& arr) const
        {
            return any_equal<this_type::depth>(arr);
        }

        template <std::int64_t Level, typename U>
        [[nodiscard]] constexpr bool any_equal(const U& u) const
        {
            return any_match<Level>(
                [](const auto& a, const auto& b) {
                    return a == b;
                },
                u);
        }

        template <typename U>
        [[nodiscard]] constexpr bool any_equal(const U& u) const
        {
            return any_equal<this_type::depth>(u);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool any_close(const ArCo& arr,
            const compliant_tol_type<ArCo, Level>& atol = default_atol<compliant_tol_type<ArCo, Level>>(),
            const compliant_tol_type<ArCo, Level>& rtol = default_rtol<compliant_tol_type<ArCo, Level>>()) const
        {
            return any_match<Level>(arr, [&atol, &rtol](const auto& a, const auto& b) {
                return oc::close(a, b, atol, rtol);
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr bool any_close(const ArCo& arr,
            const compliant_tol_type<ArCo>& atol = default_atol<compliant_tol_type<ArCo>>(),
            const compliant_tol_type<ArCo>& rtol = default_rtol<compliant_tol_type<ArCo>>()) const
        {
            return any_close<this_type::depth>(arr, atol, rtol);
        }

        template <std::int64_t Level, typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr bool any_close(const U& u,
            const tol_type<U, Level>& atol = default_atol<tol_type<U, Level>>(),
            const tol_type<U, Level>& rtol = default_rtol<tol_type<U, Level>>()) const
        {
            return any_match<Level>(
                [&atol, &rtol](const auto& a, const auto& b) {
                    return oc::close(a, b, atol, rtol);
                },
                u);
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr bool any_close(const U& u, const tol_type<U>& atol = default_atol<tol_type<U>>(),
            const tol_type<U>& rtol = default_rtol<tol_type<U>>()) const
        {
            return any_close<this_type::depth>(u, atol, rtol);
        }

        // standard iterator functions

        [[nodiscard]] constexpr auto begin()
        {
            return begin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto begin() const
        {
            return begin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto cbegin() const
        {
            return cbegin(0, arrnd_returned_element_iterator_tag{});
        }

        [[nodiscard]] constexpr auto end()
        {
            return end(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto end() const
        {
            return end(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto cend() const
        {
            return cend(0, arrnd_returned_element_iterator_tag{});
        }

        [[nodiscard]] constexpr auto rbegin()
        {
            return rbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rbegin() const
        {
            return rbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto crbegin() const
        {
            return crbegin(0, arrnd_returned_element_iterator_tag{});
        }

        [[nodiscard]] constexpr auto rend()
        {
            return rend(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rend() const
        {
            return rend(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto crend() const
        {
            return crend(0, arrnd_returned_element_iterator_tag{});
        }

        [[nodiscard]] constexpr auto begin(arrnd_returned_element_iterator_tag)
        {
            return begin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto begin(arrnd_returned_element_iterator_tag) const
        {
            return begin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto cbegin(arrnd_returned_element_iterator_tag) const
        {
            return cbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto end(arrnd_returned_element_iterator_tag)
        {
            return end(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto end(arrnd_returned_element_iterator_tag) const
        {
            return end(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto cend(arrnd_returned_element_iterator_tag) const
        {
            return cend(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rbegin(arrnd_returned_element_iterator_tag)
        {
            return rbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rbegin(arrnd_returned_element_iterator_tag) const
        {
            return rbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto crbegin(arrnd_returned_element_iterator_tag) const
        {
            return crbegin(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rend(arrnd_returned_element_iterator_tag)
        {
            return rend(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto rend(arrnd_returned_element_iterator_tag) const
        {
            return rend(0, arrnd_returned_element_iterator_tag{});
        }
        [[nodiscard]] constexpr auto crend(arrnd_returned_element_iterator_tag) const
        {
            return crend(0, arrnd_returned_element_iterator_tag{});
        }

        // by axis iterator functions

        [[nodiscard]] constexpr auto begin(size_type axis, arrnd_returned_element_iterator_tag)
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto begin(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto cbegin(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty()
                ? const_iterator()
                : const_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto end(size_type axis, arrnd_returned_element_iterator_tag)
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto end(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto cend(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty()
                ? const_iterator()
                : const_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto rbegin(size_type axis, arrnd_returned_element_iterator_tag)
        {
            return empty()
                ? reverse_iterator()
                : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rbegin(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty()
                ? reverse_iterator()
                : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto crbegin(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(
                               buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rend(size_type axis, arrnd_returned_element_iterator_tag)
        {
            return empty()
                ? reverse_iterator()
                : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto rend(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty()
                ? reverse_iterator()
                : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto crend(size_type axis, arrnd_returned_element_iterator_tag) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(
                               buffsp_->data(), indexer_type(hdr_, axis, arrnd_iterator_start_position::rend));
        }

        // by order iterator functions

        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto begin(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto begin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto cbegin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_iterator()
                           : const_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto end(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::end));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto end(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::end));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto cend(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_iterator()
                           : const_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::end));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rbegin(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rbegin));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rbegin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rbegin));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto crbegin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rbegin));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rend(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rend));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rend(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rend));
        }
        template <signed_integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto crend(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_iterator_start_position::rend));
        }

        [[nodiscard]] constexpr auto begin(std::initializer_list<size_type> order)
        {
            return begin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto begin(std::initializer_list<size_type> order) const
        {
            return begin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto cbegin(std::initializer_list<size_type> order) const
        {
            return cbegin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto end(std::initializer_list<size_type> order)
        {
            return end(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto end(std::initializer_list<size_type> order) const
        {
            return end(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto cend(std::initializer_list<size_type> order) const
        {
            return cend(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto rbegin(std::initializer_list<size_type> order)
        {
            return rbegin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto rbegin(std::initializer_list<size_type> order) const
        {
            return rbegin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto crbegin(std::initializer_list<size_type> order) const
        {
            return crbegin(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto rend(std::initializer_list<size_type> order)
        {
            return rend(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto rend(std::initializer_list<size_type> order) const
        {
            return rend(order.begin(), order.end());
        }
        [[nodiscard]] constexpr auto crend(std::initializer_list<size_type> order) const
        {
            return crend(order.begin(), order.end());
        }

        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto begin(const Cont& order)
        {
            return begin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto begin(const Cont& order) const
        {
            return begin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto cbegin(const Cont& order) const
        {
            return cbegin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto end(const Cont& order)
        {
            return end(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto end(const Cont& order) const
        {
            return end(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto cend(const Cont& order) const
        {
            return cend(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rbegin(const Cont& order)
        {
            return rbegin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rbegin(const Cont& order) const
        {
            return rbegin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto crbegin(const Cont& order) const
        {
            return crbegin(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rend(const Cont& order)
        {
            return rend(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rend(const Cont& order) const
        {
            return rend(std::begin(order), std::end(order));
        }
        template <signed_integral_type_iterable Cont>
        [[nodiscard]] constexpr auto crend(const Cont& order) const
        {
            return crend(std::begin(order), std::end(order));
        }

        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto begin(const U (&order)[M])
        //{
        //    return begin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto begin(const U (&order)[M]) const
        //{
        //    return begin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto cbegin(const U (&order)[M]) const
        //{
        //    return cbegin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto end(const U (&order)[M])
        //{
        //    return end(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto end(const U (&order)[M]) const
        //{
        //    return end(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto cend(const U (&order)[M]) const
        //{
        //    return cend(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto rbegin(const U (&order)[M])
        //{
        //    return rbegin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto rbegin(const U (&order)[M]) const
        //{
        //    return rbegin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto crbegin(const U (&order)[M]) const
        //{
        //    return crbegin(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto rend(const U (&order)[M])
        //{
        //    return rend(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto rend(const U (&order)[M]) const
        //{
        //    return rend(std::begin(order), std::end(order));
        //}
        //template <std::integral U, std::int64_t M>
        //[[nodiscard]] constexpr auto crend(const U (&order)[M]) const
        //{
        //    return crend(std::begin(order), std::end(order));
        //}

        // slice iterator functions

        [[nodiscard]] constexpr auto begin(arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto begin(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto cbegin(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_slice_iterator()
                : const_slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto end(arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto end(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto cend(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_slice_iterator()
                : const_slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto rbegin(arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rbegin(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto crbegin(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_reverse_slice_iterator()
                : const_reverse_slice_iterator(*this,
                    ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rend(arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto rend(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto crend(arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_reverse_slice_iterator()
                : const_reverse_slice_iterator(
                    *this, ranger_type(hdr_, 0, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }

        [[nodiscard]] constexpr auto begin(size_type axis, arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto begin(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto cbegin(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_slice_iterator()
                : const_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::begin));
        }
        [[nodiscard]] constexpr auto end(size_type axis, arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto end(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? slice_iterator()
                : slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto cend(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_slice_iterator()
                : const_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::end));
        }
        [[nodiscard]] constexpr auto rbegin(size_type axis, arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rbegin(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto crbegin(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_reverse_slice_iterator()
                : const_reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rbegin));
        }
        [[nodiscard]] constexpr auto rend(size_type axis, arrnd_returned_slice_iterator_tag)
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto rend(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? reverse_slice_iterator()
                : reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }
        [[nodiscard]] constexpr auto crend(size_type axis, arrnd_returned_slice_iterator_tag) const
        {
            return empty()
                ? const_reverse_slice_iterator()
                : const_reverse_slice_iterator(*this,
                    ranger_type(hdr_, axis, interval_type{0, 0}, false, arrnd_iterator_start_position::rend));
        }

        [[nodiscard]] constexpr auto abs() const
        {
            return transform([](const auto& a) {
                using std::abs;
                return abs(a);
            });
        }

        [[nodiscard]] constexpr auto acos() const
        {
            return transform([](const auto& a) {
                using std::acos;
                return acos(a);
            });
        }

        [[nodiscard]] constexpr auto acosh() const
        {
            return transform([](const auto& a) {
                using std::acosh;
                return acosh(a);
            });
        }

        [[nodiscard]] constexpr auto asin() const
        {
            return transform([](const auto& a) {
                using std::asin;
                return asin(a);
            });
        }

        [[nodiscard]] constexpr auto asinh() const
        {
            return transform([](const auto& a) {
                using std::asinh;
                return asinh(a);
            });
        }

        [[nodiscard]] constexpr auto atan() const
        {
            return transform([](const auto& a) {
                using std::atan;
                return atan(a);
            });
        }

        [[nodiscard]] constexpr auto atanh() const
        {
            return transform([](const auto& a) {
                using std::atanh;
                return atanh(a);
            });
        }

        [[nodiscard]] constexpr auto cos() const
        {
            return transform([](const auto& a) {
                using std::cos;
                return cos(a);
            });
        }

        [[nodiscard]] constexpr auto cosh() const
        {
            return transform([](const auto& a) {
                using std::cosh;
                return cosh(a);
            });
        }

        [[nodiscard]] constexpr auto exp() const
        {
            return transform([](const auto& a) {
                using std::exp;
                return exp(a);
            });
        }

        [[nodiscard]] constexpr auto log() const
        {
            return transform([](const auto& a) {
                using std::log;
                return log(a);
            });
        }

        [[nodiscard]] constexpr auto log10() const
        {
            return transform([](const auto& a) {
                using std::log10;
                return log10(a);
            });
        }

        template <typename U>
            requires(!arrnd_compliant<U>)
        [[nodiscard]] constexpr auto pow(const U& value) const
        {
            return transform([&value](const auto& a) {
                using std::pow;
                return pow(a, value);
            });
        }

        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr auto pow(const ArCo& arr) const
        {
            return transform(arr, [](const auto& a, const auto& b) {
                using std::pow;
                return pow(a, b);
            });
        }

        [[nodiscard]] constexpr auto sin() const
        {
            return transform([](const auto& a) {
                using std::sin;
                return sin(a);
            });
        }

        [[nodiscard]] constexpr auto sinh() const
        {
            return transform([](const auto& a) {
                using std::sinh;
                return sinh(a);
            });
        }

        [[nodiscard]] constexpr auto sqrt() const
        {
            return transform([](const auto& a) {
                using std::sqrt;
                return sqrt(a);
            });
        }

        [[nodiscard]] constexpr auto tan() const
        {
            return transform([](const auto& a) {
                using std::tan;
                return tan(a);
            });
        }

        [[nodiscard]] constexpr auto tanh() const
        {
            return transform([](const auto& a) {
                using std::tanh;
                return tanh(a);
            });
        }

        [[nodiscard]] constexpr auto round() const
        {
            return transform([](const auto& a) {
                using std::round;
                return round(a);
            });
        }

        [[nodiscard]] constexpr auto ceil() const
        {
            return transform([](const auto& a) {
                using std::ceil;
                return ceil(a);
            });
        }

        [[nodiscard]] constexpr auto floor() const
        {
            return transform([](const auto& a) {
                using std::floor;
                return floor(a);
            });
        }

        [[nodiscard]] constexpr auto real() const
        {
            return transform([](const auto& a) {
                using std::real;
                return real(a);
            });
        }

        [[nodiscard]] constexpr auto imag() const
        {
            return transform([](const auto& a) {
                using std::imag;
                return imag(a);
            });
        }

        [[nodiscard]] constexpr auto arg() const
        {
            return transform([](const auto& a) {
                using std::arg;
                return arg(a);
            });
        }

        [[nodiscard]] constexpr auto norm() const
        {
            return transform([](const auto& a) {
                using std::norm;
                return norm(a);
            });
        }

        [[nodiscard]] constexpr auto conj() const
        {
            return transform([](const auto& a) {
                using std::conj;
                return conj(a);
            });
        }

        [[nodiscard]] constexpr auto proj() const
        {
            return transform([](const auto& a) {
                using std::proj;
                return proj(a);
            });
        }

        [[nodiscard]] constexpr auto sign() const
        {
            return transform([](const auto& a) {
                return oc::sign(a);
            });
        }

    private:
        header_type hdr_{};
        std::shared_ptr<storage_type> buffsp_{nullptr};
        std::shared_ptr<bool> original_valid_creator_ = std::allocate_shared<bool>(shared_ref_allocator_type<bool>());
        std::weak_ptr<bool> is_creator_valid_{};
        const this_type* creator_ = nullptr;
        bool treat_as_pages_ = false; // useful for specific operations e.g. for operator* to decide between element wise and matrix multiplication
    };

    // arrnd type deduction by constructors

    /*template <typename DataStorageInfo = dynamic_storage_info<T>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>*/

    template <signed_integral_type_iterator InputDimsIt, std::input_iterator InputDataIt,
        typename DataStorageInfo = dynamic_storage_info<iterator_value_type<InputDataIt>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const InputDimsIt&, const InputDimsIt&, const InputDataIt&, const InputDataIt&)
        -> arrnd<iterator_value_type<InputDataIt>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <signed_integral_type_iterable Cont, std::input_iterator InputDataIt,
        typename DataStorageInfo = dynamic_storage_info<iterator_value_type<InputDataIt>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const Cont&, const InputDataIt&, const InputDataIt&)
        -> arrnd<iterator_value_type<InputDataIt>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <std::input_iterator InputDataIt,
        typename DataStorageInfo = dynamic_storage_info<iterator_value_type<InputDataIt>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(std::initializer_list<typename DataStorageInfo::storage_type::size_type>, const InputDataIt&,
        const InputDataIt& a)
        -> arrnd<iterator_value_type<InputDataIt>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    //template <std::integral D, std::int64_t M, std::input_iterator InputDataIt,
    //    typename DataStorageInfo = dynamic_storage_info<iterator_value_type<InputDataIt>>,
    //    typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
    //    template <typename> typename SharedRefAllocator = lightweight_allocator>
    //arrnd(const D (&)[M], const InputDataIt&, const InputDataIt&)
    //    -> arrnd<iterator_value_type<InputDataIt>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;


    template <signed_integral_type_iterator InputDimsIt, typename U, typename DataStorageInfo = dynamic_storage_info<U>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const InputDimsIt&, const InputDimsIt&, std::initializer_list<U>)
        -> arrnd<U, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <signed_integral_type_iterable Cont, typename U, typename DataStorageInfo = dynamic_storage_info<U>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const Cont&, std::initializer_list<U>) -> arrnd<U, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <typename U, typename DataStorageInfo = dynamic_storage_info<U>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(std::initializer_list<typename DataStorageInfo::storage_type::size_type>, std::initializer_list<U>)
        -> arrnd<U, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;


    template <signed_integral_type_iterator InputDimsIt, iterable DataCont,
        typename DataStorageInfo = dynamic_storage_info<iterable_value_type<DataCont>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const InputDimsIt&, const InputDimsIt&, const DataCont&)
        -> arrnd<iterable_value_type<DataCont>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <signed_integral_type_iterable Cont, iterable DataCont,
        typename DataStorageInfo = dynamic_storage_info<iterable_value_type<DataCont>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(const Cont&, const DataCont&)
        -> arrnd<iterable_value_type<DataCont>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <iterable DataCont, typename DataStorageInfo = dynamic_storage_info<iterable_value_type<DataCont>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    arrnd(std::initializer_list<typename DataStorageInfo::storage_type::size_type>, const DataCont&)
        -> arrnd<iterable_value_type<DataCont>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;


    template <typename Func, typename... Args,
        typename DataStorageInfo = dynamic_storage_info<std::invoke_result_t<Func, Args...>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator,
        signed_integral_type_iterator InputDimsIt>
    requires(invocable_no_arrnd<Func, Args...>) arrnd(const InputDimsIt&, const InputDimsIt&, Func&&, Args&&...)
        -> arrnd<std::invoke_result_t<Func, Args...>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <typename Func, typename... Args,
        typename DataStorageInfo = dynamic_storage_info<std::invoke_result_t<Func, Args...>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator, signed_integral_type_iterable Cont>
    requires(invocable_no_arrnd<Func, Args...>) arrnd(const Cont&, Func&&, Args&&...)
        -> arrnd<std::invoke_result_t<Func, Args...>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;
    template <typename Func, typename... Args,
        typename DataStorageInfo = dynamic_storage_info<std::invoke_result_t<Func, Args...>>,
        typename DimsStorageInfo = dynamic_storage_info<std::int64_t>,
        template <typename> typename SharedRefAllocator = lightweight_allocator>
    requires(invocable_no_arrnd<Func, Args...>)
        arrnd(std::initializer_list<typename DataStorageInfo::storage_type::size_type>, Func&&, Args&&...)
            -> arrnd<std::invoke_result_t<Func, Args...>, DataStorageInfo, DimsStorageInfo, SharedRefAllocator>;


    // free arrnd iterator functions

    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto begin(ArCo& c, Args&&... args)
    {
        return c.begin(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto begin(const ArCo& c, Args&&... args)
    {
        return c.begin(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto cbegin(const ArCo& c, Args&&... args)
    {
        return c.cbegin(std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto end(ArCo& c, Args&&... args)
    {
        return c.end(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto end(const ArCo& c, Args&&... args)
    {
        return c.end(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto cend(const ArCo& c, Args&&... args)
    {
        return c.cend(std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto rbegin(ArCo& c, Args&&... args)
    {
        return c.rbegin(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto rbegin(const ArCo& c, Args&&... args)
    {
        return c.rbegin(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto crbegin(const ArCo& c, Args&&... args)
    {
        return c.crbegin(std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto rend(ArCo& c, Args&&... args)
    {
        return c.rend(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto rend(const ArCo& c, Args&&... args)
    {
        return c.rend(std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename... Args>
    inline constexpr auto crend(const ArCo& c, Args&&... args)
    {
        return c.crend(std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst)
    {
        return dst.copy_from(src);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, signed_integral_type_iterator InputIt>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::pair<InputIt, InputIt> indices)
    {
        return dst.copy_from(src, indices);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, typename Cont>
        requires(signed_integral_type_iterable<Cont> || iterable_of_type<Cont, bool>)
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const Cont& indices)
    {
        return dst.copy_from(src, indices);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::initializer_list<typename ArCo1::size_type> indices)
    {
        return dst.copy_from(src, indices);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::int64_t M>
    //inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const U (&indices)[M])
    //{
    //    return dst.copy_from(src, indices);
    //}

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, interval_type_iterator InputIt>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const InputIt& first_range, const InputIt& last_range)
    {
        return dst.copy_from(src, first_range, last_range);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, interval_type_iterable Cont>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const Cont& ranges)
    {
        return copy(src, dst, std::begin(ranges), std::end(ranges));
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& copy(
        const ArCo1& src, ArCo2&& dst, std::initializer_list<typename ArCo1::interval_type> ranges)
    {
        return copy(src, dst, ranges.begin(), ranges.end());
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::int64_t M>
    //inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const interval<U> (&ranges)[M])
    //{
    //    return copy(src, dst, std::begin(ranges), std::end(ranges));
    //}

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, typename Pred, typename... Args>
        requires invocable_no_arrnd<Pred, typename ArCo2::value_type, Args...>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, Pred&& pred, Args&&... args)
    {
        return dst.copy_from(src, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& set(const ArCo1& src, ArCo2&& dst)
    {
        return dst.set_from(src);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto clone(const ArCo& arr)
    {
        return arr.clone();
    }

    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reshape(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr./*template */reshape/*<Level>*/(first_new_dim, last_new_dim);
    }
    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const Cont& new_dims)
    {
        return reshape/*<Level>*/(arr, std::begin(new_dims), std::end(new_dims));
    }
    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reshape(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
    {
        return reshape/*<Level>*/(arr, new_dims.begin(), new_dims.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const U (&new_dims)[M])
    //{
    //    return reshape<Level>(arr, std::begin(new_dims), std::end(new_dims));
    //}
    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, arrnd_shape_preset shape)
    {
        return arr./*template */reshape/*<Level>*/(shape);
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    //[[nodiscard]] inline constexpr auto reshape(
    //    const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    //{
    //    return arr.template reshape<ArCo::depth>(first_new_dim, last_new_dim);
    //}
    //template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const Cont& new_dims)
    //{
    //    return reshape<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    //}
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto reshape(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
    //{
    //    return reshape<ArCo::depth>(arr, new_dims.begin(), new_dims.end());
    //}
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const U (&new_dims)[M])
    //{
    //    return reshape<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    //}
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto reshape(const ArCo& arr, arrnd_shape_preset shape)
    //{
    //    return reshape<ArCo::depth>(arr, shape);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto resize(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr./*template */resize/*<Level>*/(first_new_dim, last_new_dim);
    }
    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const Cont& new_dims)
    {
        return resize/*<Level>*/(arr, std::begin(new_dims), std::end(new_dims));
    }
    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto resize(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
    {
        return resize/*<Level>*/(arr, new_dims.begin(), new_dims.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto resize(const ArCo& arr, const U (&new_dims)[M])
    //{
    //    return resize<Level>(arr, std::begin(new_dims), std::end(new_dims));
    //}
    //template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    //[[nodiscard]] inline constexpr auto resize(
    //    const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    //{
    //    return arr.template resize<ArCo::depth>(first_new_dim, last_new_dim);
    //}
    //template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto resize(const ArCo& arr, const Cont& new_dims)
    //{
    //    return resize<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    //}
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto resize(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> new_dims)
    //{
    //    return resize<ArCo::depth>(arr, new_dims.begin(), new_dims.end());
    //}
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto resize(const ArCo& arr, const U (&new_dims)[M])
    //{
    //    return resize<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs./*template */append/*<Level>*/(rhs);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    //[[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
    //{
    //    return append<ArCo1::depth>(lhs, rhs);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type axis)
    {
        return lhs./*template */append/*<Level>*/(rhs, axis);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    //[[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type axis)
    //{
    //    return append<ArCo1::depth>(lhs, rhs, axis);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo1, arrnd_compliant ArCo2, arrnd_compliant... ArCos>
    [[nodiscard]] inline constexpr auto append(const ArCo1& arr, const ArCo2& first, ArCos&&... others)
    {
        return arr./*template */append/*<Level>*/(first, std::forward<ArCos>(others)...);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, arrnd_compliant... ArCos>
    //[[nodiscard]] inline constexpr auto append(const ArCo1& arr, const ArCo2& first, ArCos&&... others)
    //{
    //    return append<ArCo1::depth>(arr, first, std::forward<ArCos>(others)...);
    //}
    template </*std::int64_t Level,*/ arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    [[nodiscard]] inline constexpr auto append(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    {
        return arr./*template */append/*<Level>*/(tuple, std::forward<Tuples>(others)...);
    }
    //template <arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    //[[nodiscard]] inline constexpr auto append(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    //{
    //    return append<ArCo::depth>(arr, tuple, std::forward<Tuples>(others)...);
    //}

    template </*std::int64_t Level, */ arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto matmul(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.matmul(rhs);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    //[[nodiscard]] constexpr auto matmul(const ArCo1& lhs, const ArCo2& rhs)
    //{
    //    return matmul<ArCo1::depth>(lhs, rhs);
    //}

    template </*std::int64_t Level, */ arrnd_compliant ArCo1, arrnd_compliant ArCo2, arrnd_compliant... ArCos>
    [[nodiscard]] inline constexpr auto matmul(const ArCo1& arr1, const ArCo2& arr2, ArCos&&... others)
    {
        return arr1.matmul(arr2, std::forward<ArCos>(others)...);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, arrnd_compliant... ArCos>
    //[[nodiscard]] constexpr auto matmul(const ArCo1& arr1, const ArCo2& arr2, ArCos&&... others)
    //{
    //    return matmul<ArCo1::depth>(arr1, arr2, std::forward<ArCos>(others)...);
    //}

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto det(const ArCo& arr)
    {
        return arr.det();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto inverse(const ArCo& arr)
    {
        return arr.inverse();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto tril(const ArCo& arr, typename ArCo::size_type offset = 0)
    {
        return arr.tril(offset);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto triu(const ArCo& arr, typename ArCo::size_type offset = 0)
    {
        return arr.triu(offset);
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto cholesky(const ArCo& arr)
    {
        return arr.cholesky();
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto lu(const ArCo& arr)
    {
        return arr.lu();
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto qr(
        const ArCo& arr /*, bool permute = false*/)
    {
        return arr.qr(/*permute*/);
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto hess(const ArCo& arr)
    {
        return arr.hess();
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto schur(const ArCo& arr)
    {
        return arr.schur();
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto svd(const ArCo& arr)
    {
        return arr.svd();
    }

    template <arrnd_compliant ArCo>
    [[deprecated("not fully cheked")]] [[nodiscard]] inline constexpr auto eig(const ArCo& arr)
    {
        return arr.eig();
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto solve(const ArCo1& arr, const ArCo2& b)
    {
        return arr.solve(b);
    }

    template </*std::int64_t Level, */arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind)
    {
        return lhs./*template */insert/*<Level>*/(rhs, ind);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    //[[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind)
    //{
    //    return insert<ArCo1::depth, ArCo1, ArCo2>(lhs, rhs, ind);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto insert(
        const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind, typename ArCo1::size_type axis)
    {
        return lhs./*template */insert/*<Level>*/(rhs, ind, axis);
    }
    //template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    //[[nodiscard]] inline constexpr auto insert(
    //    const ArCo1& lhs, const ArCo2& rhs, typename ArCo1::size_type ind, typename ArCo1::size_type axis)
    //{
    //    return insert<ArCo1::depth>(lhs, rhs, ind, axis);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
        requires(std::tuple_size_v<Tuple> == 2)
    [[nodiscard]] inline constexpr auto insert(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    {
        return arr./*template */insert/*<Level>*/(std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    }
    //template <arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    //    requires(std::tuple_size_v<Tuple> == 2)
    //[[nodiscard]] inline constexpr auto insert(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    //{
    //    return insert<ArCo::depth>(arr, std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
        requires(std::tuple_size_v<Tuple> == 3)
    [[nodiscard]] inline constexpr auto insert(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    {
        return arr./*template */insert/*<Level>*/(std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    }
    //template <arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    //    requires(std::tuple_size_v<Tuple> == 3)
    //[[nodiscard]] inline constexpr auto insert(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    //{
    //    return insert<ArCo::depth>(arr, std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, typename ArCo::size_type count)
    {
        return arr./*template */repeat/*<Level>*/(count);
    }
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, typename ArCo::size_type count)
    //{
    //    return repeat<ArCo::depth>(arr, count);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo, iterator_of_template_type<std::tuple> InputIt>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, InputIt first_tuple, InputIt last_tuple)
    {
        return arr./*template */repeat/*<Level>*/(first_tuple, last_tuple);
    }
    //template <arrnd_compliant ArCo, iterator_of_template_type<std::tuple> InputIt>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, InputIt first_tuple, InputIt last_tuple)
    //{
    //    return repeat<ArCo::depth>(arr, first_tuple, last_tuple);
    //}
    template </*std::int64_t Level, */arrnd_compliant ArCo, template_type<std::tuple> Tuple>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, std::initializer_list<Tuple> count_axis_tuples)
    {
        return repeat/*<Level>*/(arr, count_axis_tuples.begin(), count_axis_tuples.end());
    }
    //template <arrnd_compliant ArCo, template_type<std::tuple> Tuple>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, std::initializer_list<Tuple> count_axis_tuples)
    //{
    //    return repeat<ArCo::depth>(arr, count_axis_tuples.begin(), count_axis_tuples.end());
    //}
    template </*std::int64_t Level, */arrnd_compliant ArCo, iterable_of_template_type<std::tuple> Cont>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const Cont& count_axis_tuples)
    {
        return repeat/*<Level>*/(arr, std::begin(count_axis_tuples), std::end(count_axis_tuples));
    }
    //template <arrnd_compliant ArCo, iterable_of_template_type<std::tuple> Cont>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const Cont& count_axis_tuples)
    //{
    //    return repeat<ArCo::depth>(arr, std::begin(count_axis_tuples), std::end(count_axis_tuples));
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, template_type<std::tuple> U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const U (&count_axis_tuples)[M])
    //{
    //    return repeat<Level>(arr, std::begin(count_axis_tuples), std::end(count_axis_tuples));
    //}
    //template <arrnd_compliant ArCo, template_type<std::tuple> U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const U (&count_axis_tuples)[M])
    //{
    //    return repeat<ArCo::depth>(arr, std::begin(count_axis_tuples), std::end(count_axis_tuples));
    //}


    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, InputIt first_rep, InputIt last_rep)
    {
        return arr./*template */repeat/*<Level>*/(first_rep, last_rep);
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, InputIt first_rep, InputIt last_rep)
    //{
    //    return repeat<ArCo::depth>(arr, first_rep, last_rep);
    //}
    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, std::initializer_list<typename ArCo::size_type> reps)
    {
        return repeat/*<Level>*/(arr, reps.begin(), reps.end());
    }
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, std::initializer_list<typename ArCo::size_type> reps)
    //{
    //    return repeat<ArCo::depth>(arr, reps.begin(), reps.end());
    //}
    template </*std::int64_t Level, */arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const Cont& reps)
    {
        return repeat/*<Level>*/(arr, std::begin(reps), std::end(reps));
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const Cont& reps)
    //{
    //    return repeat<ArCo::depth>(arr, std::begin(reps), std::end(reps));
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::signed_integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const U (&reps)[M])
    //{
    //    return repeat<Level>(arr, std::begin(reps), std::end(reps));
    //}
    //template <arrnd_compliant ArCo, std::signed_integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto repeat(const ArCo& arr, const U (&reps)[M])
    //{
    //    return repeat<ArCo::depth>(arr, std::begin(reps), std::end(reps));
    //}


    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto remove(
        const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count)
    {
        return arr./*template */remove/*<Level>*/(ind, count);
    }
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto remove(
    //    const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count)
    //{
    //    return remove<ArCo::depth>(arr, ind, count);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto remove(
        const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count, typename ArCo::size_type axis)
    {
        return arr./*template */remove/*<Level>*/(ind, count, axis);
    }
    //template <arrnd_compliant ArCo>
    //[[nodiscard]] inline constexpr auto remove(
    //    const ArCo& arr, typename ArCo::size_type ind, typename ArCo::size_type count, typename ArCo::size_type axis)
    //{
    //    return remove<ArCo::depth>(arr, ind, count, axis);
    //}

    template </*std::int64_t Level, */arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    [[nodiscard]] inline constexpr auto remove(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    {
        return arr./*template */remove/*<Level>*/(std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    }
    //template <arrnd_compliant ArCo, template_type<std::tuple> Tuple, typename... Tuples>
    //[[nodiscard]] inline constexpr auto remove(const ArCo& arr, Tuple&& tuple, Tuples&&... others)
    //{
    //    return remove<ArCo::depth>(arr, std::forward<Tuple>(tuple), std::forward<Tuples>(others)...);
    //}

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool empty(const ArCo& arr) noexcept
    {
        return arr.empty();
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, Func&& func, Args&&... args)
    {
        return arr.template reduce<Level>(std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, Func&& func, Args&&... args)
    {
        return reduce<ArCo::depth>(arr, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename T, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto fold(const ArCo& arr, const T& init, Func&& func, Args&&... args)
    {
        return arr.template fold<Level>(init, std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename T, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto fold(const ArCo& arr, const T& init, Func&& func, Args&&... args)
    {
        return fold<ArCo::depth>(arr, init, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(
        const ArCo& arr, typename ArCo::size_type axis, Func&& func, Args&&... args)
    {
        return arr.template reduce<Level>(axis, std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(
        const ArCo& arr, typename ArCo::size_type axis, Func&& func, Args&&... args)
    {
        return reduce<ArCo::depth>(arr, axis, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto fold(
        const ArCo1& arr, typename ArCo1::size_type axis, const ArCo2& inits, Func&& func, Args&&... args)
    {
        return arr.template fold<Level>(axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto fold(
        const ArCo1& arr, typename ArCo1::size_type axis, const ArCo2& inits, Func&& func, Args&&... args)
    {
        return fold<ArCo1::depth>(arr, axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool all(const ArCo& arr)
    {
        return arr.template all<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool all(const ArCo& arr)
    {
        return all<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto all(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template all<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto all(const ArCo& arr, typename ArCo::size_type axis)
    {
        return all<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool any(const ArCo& arr)
    {
        return arr.template any<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool any(const ArCo& arr)
    {
        return any<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto any(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template any<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto any(const ArCo& arr, typename ArCo::size_type axis)
    {
        return any<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sum(const ArCo& arr)
    {
        return arr.template sum<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sum(const ArCo& arr)
    {
        return sum<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sum(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template sum<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sum(const ArCo& arr, typename ArCo::size_type axis)
    {
        return sum<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto min(const ArCo& arr)
    {
        return arr.template min<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto min(const ArCo& arr)
    {
        return min<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto min(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template min<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto min(const ArCo& arr, typename ArCo::size_type axis)
    {
        return min<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto max(const ArCo& arr)
    {
        return arr.template max<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto max(const ArCo& arr)
    {
        return max<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto max(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template max<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto max(const ArCo& arr, typename ArCo::size_type axis)
    {
        return max<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto prod(const ArCo& arr)
    {
        return arr.template prod<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto prod(const ArCo& arr)
    {
        return prod<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto prod(const ArCo& arr, typename ArCo::size_type axis)
    {
        return arr.template prod<Level>(axis);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto prod(const ArCo& arr, typename ArCo::size_type axis)
    {
        return prod<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto transform(const ArCo& arr, Func&& func, Args&&... args)
    {
        return arr.template transform<Level>(std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr auto transform(const ArCo& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return lhs.template transform<Level>(rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto transform(const ArCo& arr, Func&& func, Args&&... args)
    {
        return transform<ArCo::depth>(arr, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr auto transform(const ArCo& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return transform<ArCo::depth>(lhs, rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto& apply(ArCo&& arr, Func&& func, Args&&... args)
    {
        return arr.template apply<Level>(std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    inline constexpr auto& apply(ArCo&& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return lhs.template apply<Level>(rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto& apply(ArCo&& arr, Func&& func, Args&&... args)
    {
        return apply<std::remove_cvref_t<ArCo>::depth>(
            std::forward<ArCo>(arr), std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    inline constexpr auto& apply(ArCo&& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return apply<std::remove_cvref_t<ArCo>::depth>(
            std::forward<ArCo>(lhs), rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto& apply(ArCo& arr, Func&& func, Args&&... args)
    {
        return arr.template apply<Level>(std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    inline constexpr auto& apply(ArCo& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return lhs.template apply<Level>(rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto& apply(ArCo& arr, Func&& func, Args&&... args)
    {
        return apply<std::remove_cvref_t<ArCo>::depth>(arr, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename U, typename Func, typename... Args>
        requires arrnd_compliant<U>
    inline constexpr auto& apply(ArCo& lhs, const U& rhs, Func&& func, Args&&... args)
    {
        return apply<std::remove_cvref_t<ArCo>::depth>(lhs, rhs, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template filter<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return filter<ArCo::depth>(arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto filter(const ArCo1& arr, const ArCo2& selector)
    {
        return arr.template filter<Level>(selector);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto filter(const ArCo1& arr, const ArCo2& selector)
    {
        return filter<ArCo1::depth>(arr, selector);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr auto find(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template find<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr auto find(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return find<ArCo::depth>(arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto find(const ArCo1& arr, const ArCo2& mask)
    {
        return arr.template find<Level>(mask);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto find(const ArCo1& arr, const ArCo2& mask)
    {
        return find<ArCo1::depth>(arr, mask);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, InputIt first_ind, InputIt last_ind)
    {
        return filter<Level>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::distance(first_ind, last_ind)}, first_ind, last_ind));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
        requires(!arrnd_compliant<Cont>)
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, const Cont& indices)
    {
        return filter<Level>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::ssize(indices)}, std::begin(indices), std::end(indices)));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, std::initializer_list<typename ArCo::size_type> indices)
    {
        return filter<Level>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::ssize(indices)}, indices.begin(), indices.end()));
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto filter(const ArCo& arr, const U (&indices)[M])
    //{
    //    return filter<Level>(arr,
    //        typename ArCo::template replaced_type<typename ArCo::size_type>(
    //            {M}, std::begin(indices), std::end(indices)));
    //}

    template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, InputIt first_ind, InputIt last_ind)
    {
        return filter<ArCo::depth>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::distance(first_ind, last_ind)}, first_ind, last_ind));
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
        requires(!arrnd_compliant<Cont>)
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, const Cont& indices)
    {
        return filter<ArCo::depth>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::ssize(indices)}, std::begin(indices), std::end(indices)));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, std::initializer_list<typename ArCo::size_type> indices)
    {
        return filter<ArCo::depth>(arr,
            typename ArCo::template replaced_type<typename ArCo::size_type>(
                {std::ssize(indices)}, indices.begin(), indices.end()));
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto filter(const ArCo& arr, const U (&indices)[M])
    //{
    //    return filter<ArCo::depth>(arr,
    //        typename ArCo::template replaced_type<typename ArCo::size_type>(
    //            {M}, std::begin(indices), std::end(indices)));
    //}

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto diag(
        const ArCo& arr, arrnd_diag_type type = arrnd_diag_type::from_matrix, typename ArCo::size_type offset = 0)
    {
        return arr.diag(type, offset);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto is_banded(
        const ArCo& arr, typename ArCo::size_type lower = 0, typename ArCo::size_type upper = 0)
    {
        return arr.is_banded(lower, upper);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, const InputIt& first_order, const InputIt& last_order)
    {
        return arr.template transpose<Level>(first_order, last_order);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const Cont& order)
    {
        return transpose<Level>(arr, std::begin(order), std::end(order));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> order)
    {
        return transpose<Level>(arr, order.begin(), order.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const U (&order)[M])
    //{
    //    return transpose<Level>(arr, std::begin(order), std::end(order));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, const InputIt& first_order, const InputIt& last_order)
    {
        return arr.template transpose<ArCo::depth>(first_order, last_order);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const Cont& order)
    {
        return transpose<ArCo::depth>(arr, std::begin(order), std::end(order));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> order)
    {
        return transpose<ArCo::depth>(arr, order.begin(), order.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const U (&order)[M])
    //{
    //    return transpose<ArCo::depth>(arr, std::begin(order), std::end(order));
    //}


    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr)
    {
        return arr.template transpose<Level>();
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr)
    {
        return arr.template transpose<ArCo::depth>();
    }


    //template <std::int64_t Depth, arrnd_compliant ArCo>
    //    requires(Depth >= 0)
    //[[nodiscard]] constexpr auto nest(const ArCo& arr) // deprecated
    //{
    //    return arr.template nest<Depth>();
    //}

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator==(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a == b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator==(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a == b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator==(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a == b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator!=(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a != b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator!=(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a != b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator!=(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a != b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto close(const ArCo1& lhs, const ArCo2& rhs,
        const typename ArCo1::template compliant_tol_type<ArCo2>& atol
        = default_atol<typename ArCo1::template compliant_tol_type<ArCo2>>(),
        const typename ArCo1::template compliant_tol_type<ArCo2>& rtol
        = default_rtol<typename ArCo1::template compliant_tol_type<ArCo2>>())
    {
        return lhs.template close<ArCo1::depth>(rhs, atol, rtol);
    }

    template <arrnd_compliant ArCo, typename T>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr auto close(const ArCo& lhs, const T& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return lhs.template close<ArCo::depth>(rhs, atol, rtol);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr auto close(const T& lhs, const ArCo& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return rhs.template close<ArCo::depth>(lhs, atol, rtol);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator>(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a > b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator>(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a > b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator>(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a > b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator>=(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a >= b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator>=(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a >= b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator>=(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a >= b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator<(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a < b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator<(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a < b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator<(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a < b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator<=(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a <= b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator<=(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a <= b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator<=(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a <= b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator+(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a + b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator+(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a + b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator+(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a + b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator+=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a + b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator+=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a + b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator-(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a - b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator-(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a - b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator-(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a - b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator-=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a - b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator-=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a - b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator*(const ArCo1& lhs, const ArCo2& rhs)
    {
        assert(lhs.is_as_pages() == rhs.is_as_pages());

        if (lhs.is_as_pages() && rhs.is_as_pages()) {
            return lhs.matmul(rhs);
        }

        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a * b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator*(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a * b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator*(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a * b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator*=(ArCo1& lhs, const ArCo2& rhs)
    {
        assert(lhs.is_as_pages() == rhs.is_as_pages());

        if (lhs.is_as_pages() && rhs.is_as_pages()) {
            lhs = lhs.matmul(rhs);
            return lhs;
        }

        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a * b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator*=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a * b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator/(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a / b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator/(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a / b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator/(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a / b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator/=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a / b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator/=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a / b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator%(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a % b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator%(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a % b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator%(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a % b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator%=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a % b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator%=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a % b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator^(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a ^ b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator^(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a ^ b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator^(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a ^ b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator^=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a ^ b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator^=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a ^ b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator&(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a & b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator&(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a & b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator&(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a & b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator&=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a & b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator&=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a & b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator|(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a | b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator|(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a | b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator|(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a | b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator|=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a | b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator|=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a | b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator<<(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a << b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator<<(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a << b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!std::derived_from<T, std::ios_base> && !arrnd_compliant<T>)
    [[nodiscard]] inline constexpr auto operator<<(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a << b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator<<=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a << b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator<<=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a << b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator>>(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a >> b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator>>(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a >> b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator>>(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a >> b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& operator>>=(ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.apply(rhs, [](const auto& a, const auto& b) {
            return a >> b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    inline constexpr auto& operator>>=(ArCo& lhs, const T& rhs)
    {
        return lhs.apply(
            [](const auto& a, const auto& b) {
                return a >> b;
            },
            rhs);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator~(const ArCo& arr)
    {
        return arr.transform([](const auto& a) {
            return ~a;
        });
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator!(const ArCo& arr)
    {
        return arr.transform([](const auto& a) {
            return !a;
        });
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator+(const ArCo& arr)
    {
        return arr.transform([](const auto& a) {
            return +a;
        });
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator-(const ArCo& arr)
    {
        return arr.transform([](const auto& a) {
            return -a;
        });
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto abs(const ArCo& arr)
    {
        return arr.abs();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto acos(const ArCo& arr)
    {
        return arr.acos();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto acosh(const ArCo& arr)
    {
        return arr.acosh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto asin(const ArCo& arr)
    {
        return arr.asin();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto asinh(const ArCo& arr)
    {
        return arr.asinh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto atan(const ArCo& arr)
    {
        return arr.atan();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto atanh(const ArCo& arr)
    {
        return arr.atanh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto cos(const ArCo& arr)
    {
        return arr.cos();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto cosh(const ArCo& arr)
    {
        return arr.cosh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto exp(const ArCo& arr)
    {
        return arr.exp();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto log(const ArCo& arr)
    {
        return arr.log();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto log10(const ArCo& arr)
    {
        return arr.log10();
    }

    template <arrnd_compliant ArCo, typename U>
    [[nodiscard]] inline constexpr auto pow(const ArCo& arr, const U& value)
    {
        return arr.pow(value);
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sin(const ArCo& arr)
    {
        return arr.sin();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sinh(const ArCo& arr)
    {
        return arr.sinh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sqrt(const ArCo& arr)
    {
        return arr.sqrt();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto tan(const ArCo& arr)
    {
        return arr.tan();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto tanh(const ArCo& arr)
    {
        return arr.tanh();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto round(const ArCo& arr)
    {
        return arr.round();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto ceil(const ArCo& arr)
    {
        return arr.ceil();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto floor(const ArCo& arr)
    {
        return arr.floor();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto real(const ArCo& arr)
    {
        return arr.real();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto imag(const ArCo& arr)
    {
        return arr.imag();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto arg(const ArCo& arr)
    {
        return arr.arg();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto norm(const ArCo& arr)
    {
        return arr.norm();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto conj(const ArCo& arr)
    {
        return arr.conj();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto proj(const ArCo& arr)
    {
        return arr.proj();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto polar(const ArCo& rs)
    {
        return transform(rs, [](const auto& r) {
            using std::polar;
            return polar(r);
        });
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto polar(const ArCo1& rs, const ArCo2& thetas)
    {
        return transform(rs, thetas, [](const auto& r, const auto& theta) {
            using std::polar;
            return polar(r, theta);
        });
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto sign(const ArCo& arr)
    {
        return arr.sign();
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator&&(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a && b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator&&(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a && b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator&&(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a && b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto operator||(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a || b;
        });
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr auto operator||(const ArCo& lhs, const T& rhs)
    {
        return lhs.transform(
            [](const auto& a, const auto& b) {
                return a || b;
            },
            rhs);
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator||(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(
            [](const auto& b, const auto& a) {
                return a || b;
            },
            lhs);
    }

    template <arrnd_compliant ArCo>
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

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator++(ArCo&& arr)
    {
        return operator++(arr);
    }

    template <arrnd_compliant ArCo>
    inline constexpr auto operator++(ArCo& arr, int)
    {
        ArCo old = clone(arr);
        operator++(arr);
        return old;
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator++(ArCo&& arr, int)
    {
        return operator++(arr, int{});
    }

    template <arrnd_compliant ArCo>
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

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator--(ArCo&& arr)
    {
        return operator--(arr);
    }

    template <arrnd_compliant ArCo>
    inline constexpr auto operator--(ArCo& arr, int)
    {
        ArCo old = clone(arr);
        operator--(arr);
        return old;
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator--(ArCo&& arr, int)
    {
        return operator--(arr, int{});
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto expand(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::size_type division = 0, bool find_closest_axis_dim_bigger_than_one_to_the_left = false)
    {
        return arr.template expand<Level>(axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto expand(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::size_type division = 0, bool find_closest_axis_dim_bigger_than_one_to_the_left = false)
    {
        return expand<ArCo::depth>(arr, axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto collapse(const ArCo& arr)
    {
        return arr.template collapse<Level>();
    }

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto collapse(const ArCo& arr)
    {
        return collapse<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto pages(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::size_type division = 0, bool find_closest_axis_dim_bigger_than_one_to_the_left = false)
    {
        return arr.template pages<Level>(axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto pages(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::size_type division = 0, bool find_closest_axis_dim_bigger_than_one_to_the_left = false)
    {
        return pages<ArCo::depth>(arr, axis, division, find_closest_axis_dim_bigger_than_one_to_the_left);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, typename ArCo::size_type division)
    {
        return arr.template split<Level>(first_axis, last_axis, division);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const Cont& axes, typename ArCo::size_type division)
    {
        return split<Level>(arr, std::begin(axes), std::end(axes), division);
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, typename ArCo::size_type division)
    {
        return split<Level>(arr, axes.begin(), axes.end(), division);
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const U (&axes)[M], typename ArCo::size_type division)
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), division);
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, typename ArCo::size_type division)
    {
        return split<ArCo::depth>(arr, first_axis, last_axis, division);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const Cont& axes, typename ArCo::size_type division)
    {
        return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), division);
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, typename ArCo::size_type division)
    {
        return split<ArCo::depth>(arr, axes.begin(), axes.end(), division);
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const U (&axes)[M], typename ArCo::size_type division)
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), division);
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt,
        signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind)
    {
        return arr.template split<Level>(first_axis, last_axis, first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind)
    {
        return split<ArCo::depth>(arr, first_axis, last_axis, first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt,
        signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const Cont& inds)
    {
        return split<Level>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<Level>(arr, first_axis, last_axis, inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const U (&inds)[M])
    //{
    //    return split<Level>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const Cont& inds)
    {
        return split<ArCo::depth>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<ArCo::depth>(arr, first_axis, last_axis, inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const U (&inds)[M])
    //{
    //    return split<ArCo::depth>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont,
        signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, IndsIt first_ind, IndsIt last_ind)
    {
        return split<Level>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont,
        signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, const Cont& inds)
    {
        return split<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, const AxesCont& axes, std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<Level>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, const U (&inds)[M])
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, IndsIt first_ind, IndsIt last_ind)
    {
        return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, const Cont& inds)
    {
        return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, const AxesCont& axes, std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const AxesCont& axes, const U (&inds)[M])
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, IndsIt first_ind, IndsIt last_ind)
    {
        return split<Level>(arr, axes.begin(), axes.end(), first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const Cont& inds)
    {
        return split<Level>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes,
        std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<Level>(arr, axes.begin(), axes.end(), inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const U (&inds)[M])
    //{
    //    return split<Level>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, IndsIt first_ind, IndsIt last_ind)
    {
        return split<ArCo::depth>(arr, axes.begin(), axes.end(), first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto split(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const Cont& inds)
    {
        return split<ArCo::depth>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto split(const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes,
        std::initializer_list<typename ArCo::size_type> inds)
    {
        return split<ArCo::depth>(arr, axes.begin(), axes.end(), inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const U (&inds)[M])
    //{
    //    return split<ArCo::depth>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    //}

    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N,
    //    signed_integral_type_iterator IndsIt>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], IndsIt first_ind, IndsIt last_ind)
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N,
    //    signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], const Cont& inds)
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N>
    //[[nodiscard]] inline constexpr auto split(
    //    const ArCo& arr, const V (&axes)[N], std::initializer_list<typename ArCo::size_type> inds)
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], const U (&inds)[M])
    //{
    //    return split<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], IndsIt first_ind, IndsIt last_ind)
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], const Cont& inds)
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N>
    //[[nodiscard]] inline constexpr auto split(
    //    const ArCo& arr, const V (&axes)[N], std::initializer_list<typename ArCo::size_type> inds)
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto split(const ArCo& arr, const V (&axes)[N], const U (&inds)[M])
    //{
    //    return split<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt,
        signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind)
    {
        return arr.template exclude<Level>(first_axis, last_axis, first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, IndsIt first_ind, IndsIt last_ind)
    {
        return exclude<ArCo::depth>(arr, first_axis, last_axis, first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt,
        signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const Cont& inds)
    {
        return exclude<Level>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<Level>(arr, first_axis, last_axis, inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const U (&inds)[M])
    //{
    //    return exclude<Level>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const Cont& inds)
    {
        return exclude<ArCo::depth>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, AxesIt first_axis, AxesIt last_axis, std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<ArCo::depth>(arr, first_axis, last_axis, inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterator AxesIt, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, AxesIt first_axis, AxesIt last_axis, const U (&inds)[M])
    //{
    //    return exclude<ArCo::depth>(arr, first_axis, last_axis, std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont,
        signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, const AxesCont& axes, IndsIt first_ind, IndsIt last_ind)
    {
        return exclude<Level>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont,
        signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const AxesCont& axes, const Cont& inds)
    {
        return exclude<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, const AxesCont& axes, std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<Level>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const AxesCont& axes, const U (&inds)[M])
    //{
    //    return exclude<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, const AxesCont& axes, IndsIt first_ind, IndsIt last_ind)
    {
        return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const AxesCont& axes, const Cont& inds)
    {
        return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, const AxesCont& axes, std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, signed_integral_type_iterable AxesCont, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const AxesCont& axes, const U (&inds)[M])
    //{
    //    return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, IndsIt first_ind, IndsIt last_ind)
    {
        return exclude<Level>(arr, axes.begin(), axes.end(), first_ind, last_ind);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const Cont& inds)
    {
        return exclude<Level>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes,
        std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<Level>(arr, axes.begin(), axes.end(), inds.begin(), inds.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const U (&inds)[M])
    //{
    //    return exclude<Level>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator IndsIt>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, IndsIt first_ind, IndsIt last_ind)
    {
        return exclude<ArCo::depth>(arr, axes.begin(), axes.end(), first_ind, last_ind);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto exclude(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const Cont& inds)
    {
        return exclude<ArCo::depth>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto exclude(const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes,
        std::initializer_list<typename ArCo::size_type> inds)
    {
        return exclude<ArCo::depth>(arr, axes.begin(), axes.end(), inds.begin(), inds.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, std::initializer_list<typename ArCo::size_type> axes, const U (&inds)[M])
    //{
    //    return exclude<ArCo::depth>(arr, axes.begin(), axes.end(), std::begin(inds), std::end(inds));
    //}

    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N,
    //    signed_integral_type_iterator IndsIt>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], IndsIt first_ind, IndsIt last_ind)
    //{
    //    return exclude<Level>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N,
    //    signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], const Cont& inds)
    //{
    //    return exclude<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, const V (&axes)[N], std::initializer_list<typename ArCo::size_type> inds)
    //{
    //    return exclude<Level>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    //}
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral V, std::int64_t N, std::integral U,
    //    std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], const U (&inds)[M])
    //{
    //    return exclude<Level>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, signed_integral_type_iterator IndsIt>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], IndsIt first_ind, IndsIt last_ind)
    //{
    //    return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), first_ind, last_ind);
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, signed_integral_type_iterable Cont>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], const Cont& inds)
    //{
    //    return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N>
    //[[nodiscard]] inline constexpr auto exclude(
    //    const ArCo& arr, const V (&axes)[N], std::initializer_list<typename ArCo::size_type> inds)
    //{
    //    return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), inds.begin(), inds.end());
    //}
    //template <arrnd_compliant ArCo, std::integral V, std::int64_t N, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto exclude(const ArCo& arr, const V (&axes)[N], const U (&inds)[M])
    //{
    //    return exclude<ArCo::depth>(arr, std::begin(axes), std::end(axes), std::begin(inds), std::end(inds));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo>
        requires(!ArCo::is_flat)
    [[nodiscard]] inline constexpr auto merge(const ArCo& arr)
    {
        return arr.template merge<Level>();
    }
    template <arrnd_compliant ArCo>
        requires(!ArCo::is_flat)
    [[nodiscard]] inline constexpr auto merge(const ArCo& arr)
    {
        return merge<ArCo::depth - 1>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto movop(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::interval_type window, bool bounded, Func&& func, Args&&... args)
    {
        return arr.template movop<Level>(axis, window, bounded, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto movop(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::interval_type window, bool bounded, Func&& func, Args&&... args)
    {
        return movop<ArCo::depth>(arr, axis, window, bounded, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename ReduceFunc, typename TransformFunc, typename... Args>
    [[nodiscard]] inline constexpr auto cumop(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::interval_type window, bool bounded, ReduceFunc&& rfunc, TransformFunc&& tfunc,
        Args&&... args)
    {
        return arr.template cumop<Level>(axis, window, bounded, std::forward<ReduceFunc>(rfunc),
            std::forward<TransformFunc>(tfunc), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename ReduceFunc, typename TransformFunc, typename... Args>
    [[nodiscard]] inline constexpr auto cumop(const ArCo& arr, typename ArCo::size_type axis,
        typename ArCo::interval_type window, bool bounded, ReduceFunc&& rfunc, TransformFunc&& tfunc,
        Args&&... args)
    {
        return cumop<ArCo::depth>(arr, axis, window, bounded, std::forward<ReduceFunc>(rfunc),
            std::forward<TransformFunc>(tfunc), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto pageop(const ArCo& arr, typename ArCo::size_type page_size, Func&& func, Args&&... args)
    {
        return arr.template pageop<Level>(page_size, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Func, typename... Args>
    inline constexpr auto pageop(const ArCo& arr, typename ArCo::size_type page_size, Func&& func, Args&&... args)
    {
        return pageop<ArCo::depth>(arr, page_size, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto squeeze(const ArCo& arr)
    {
        return arr.template squeeze<Level>();
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto squeeze(const ArCo& arr)
    {
        return squeeze<ArCo::depth>(arr);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return arr.template sort<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return sort<ArCo::depth>(arr, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto sort(
        const ArCo& arr, typename ArCo::size_type axis, Comp&& comp, Args&&... args)
    {
        return arr.template sort<Level>(axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto sort(
        const ArCo& arr, typename ArCo::size_type axis, Comp&& comp, Args&&... args)
    {
        return sort<ArCo::depth>(arr, axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return arr.template is_sorted<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return is_sorted<ArCo::depth>(arr, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto is_sorted(
        const ArCo& arr, typename ArCo::size_type axis, Comp&& comp, Args&&... args)
    {
        return arr.template is_sorted<Level>(axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
    [[nodiscard]] inline constexpr auto is_sorted(
        const ArCo& arr, typename ArCo::size_type axis, Comp&& comp, Args&&... args)
    {
        return is_sorted<ArCo::depth>(arr, axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, InputIt first_order, InputIt last_order)
    {
        return arr.template reorder<Level>(first_order, last_order);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, const Cont& order)
    {
        return reorder<Level>(arr, std::begin(order), std::end(order));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, std::initializer_list<typename ArCo::size_type> order)
    {
        return reorder<Level>(arr, order.begin(), order.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reorder(const ArCo& arr, const U (&order)[M])
    //{
    //    return reorder<Level>(arr, std::begin(order), std::end(order));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, InputIt first_order, InputIt last_order)
    {
        return reorder<ArCo::depth>(arr, first_order, last_order);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, const Cont& order)
    {
        return reorder<ArCo::depth>(arr, std::begin(order), std::end(order));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, std::initializer_list<typename ArCo::size_type> order)
    {
        return reorder<ArCo::depth>(arr, order.begin(), order.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reorder(const ArCo& arr, const U (&order)[M])
    //{
    //    return reorder<ArCo::depth>(arr, std::begin(order), std::end(order));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reorder(
        const ArCo& arr, typename ArCo::size_type axis, InputIt first_order, InputIt last_order)
    {
        return arr.template reorder<Level>(axis, first_order, last_order);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, typename ArCo::size_type axis, const Cont& order)
    {
        return reorder<Level>(arr, axis, std::begin(order), std::end(order));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reorder(
        const ArCo& arr, typename ArCo::size_type axis, std::initializer_list<typename ArCo::size_type> order)
    {
        return reorder<Level>(arr, axis, order.begin(), order.end());
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reorder(const ArCo& arr, typename ArCo::size_type axis, const U (&order)[M])
    //{
    //    return reorder<Level>(arr, axis, std::begin(order), std::end(order));
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reorder(
        const ArCo& arr, typename ArCo::size_type axis, InputIt first_order, InputIt last_order)
    {
        return reorder<ArCo::depth>(arr, axis, first_order, last_order);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reorder(const ArCo& arr, typename ArCo::size_type axis, const Cont& order)
    {
        return reorder<ArCo::depth>(arr, axis, std::begin(order), std::end(order));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reorder(
        const ArCo& arr, typename ArCo::size_type axis, std::initializer_list<typename ArCo::size_type> order)
    {
        return reorder<ArCo::depth>(arr, axis, order.begin(), order.end());
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto reorder(const ArCo& arr, typename ArCo::size_type axis, const U (&order)[M])
    //{
    //    return reorder<ArCo::depth>(arr, axis, std::begin(order), std::end(order));
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, InputIt first_sub, InputIt last_sub, typename ArCo::size_type offset = 1)
    {
        return arr.template adjacent_indices<Level>(first_sub, last_sub, offset);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, const Cont& subs, typename ArCo::size_type offset = 1)
    {
        return adjacent_indices<Level>(arr, std::begin(subs), std::end(subs), offset);
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> subs, typename ArCo::size_type offset = 1)
    {
        return adjacent_indices<Level>(arr, subs.begin(), subs.end(), offset);
    }
    //template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto adjacent_indices(
    //    const ArCo& arr, const U (&subs)[M], typename ArCo::size_type offset = 1)
    //{
    //    return adjacent_indices<Level>(arr, std::begin(subs), std::end(subs), offset);
    //}
    template <arrnd_compliant ArCo, signed_integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, InputIt first_sub, InputIt last_sub, typename ArCo::size_type offset = 1)
    {
        return adjacent_indices<ArCo::depth>(arr, first_sub, last_sub, offset);
    }
    template <arrnd_compliant ArCo, signed_integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, const Cont& subs, typename ArCo::size_type offset = 1)
    {
        return adjacent_indices<ArCo::depth>(arr, std::begin(subs), std::end(subs), offset);
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto adjacent_indices(
        const ArCo& arr, std::initializer_list<typename ArCo::size_type> subs, typename ArCo::size_type offset = 1)
    {
        return adjacent_indices<ArCo::depth>(arr, subs.begin(), subs.end(), offset);
    }
    //template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    //[[nodiscard]] inline constexpr auto adjacent_indices(
    //    const ArCo& arr, const U (&subs)[M], typename ArCo::size_type offset = 1)
    //{
    //    return adjacent_indices<ArCo::depth>(arr, std::begin(subs), std::end(subs), offset);
    //}

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template all_match<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename U, typename Pred, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr bool all_match(const ArCo& lhs, const U& rhs, Pred&& pred, Args&&... args)
    {
        return lhs.template all_match<Level>(rhs, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return all_match<ArCo::depth>(arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename U, typename Pred, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr bool all_match(const ArCo& lhs, const U& rhs, Pred&& pred, Args&&... args)
    {
        return all_match<ArCo::depth>(lhs, rhs, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr bool any_match(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template any_match<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename U, typename Pred, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr bool any_match(const ArCo& lhs, const U& rhs, Pred&& pred, Args&&... args)
    {
        return lhs.template any_match<Level>(rhs, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename Pred, typename... Args>
    [[nodiscard]] inline constexpr bool any_match(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return any_match<ArCo::depth>(arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <arrnd_compliant ArCo, typename U, typename Pred, typename... Args>
        requires arrnd_compliant<U>
    [[nodiscard]] inline constexpr bool any_match(const ArCo& lhs, const U& rhs, Pred&& pred, Args&&... args)
    {
        return any_match<ArCo::depth>(lhs, rhs, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr bool all_equal(const ArCo& lhs, const T& rhs)
    {
        return lhs.template all_equal<Level>(rhs);
    }

    template <std::int64_t Level, typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_equal(const T& lhs, const ArCo& rhs)
    {
        return rhs.template all_equal<Level>(lhs);
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr bool all_equal(const ArCo& lhs, const T& rhs)
    {
        return all_equal<ArCo::depth>(lhs, rhs);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_equal(const T& lhs, const ArCo& rhs)
    {
        return all_equal<ArCo::depth>(lhs, rhs);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr bool all_close(const ArCo1& lhs, const ArCo2& rhs,
        const typename ArCo1::template compliant_tol_type<ArCo2, Level>& atol
        = default_atol<typename ArCo1::template compliant_tol_type<ArCo2, Level>>(),
        const typename ArCo1::template compliant_tol_type<ArCo2, Level>& rtol
        = default_rtol<typename ArCo1::template compliant_tol_type<ArCo2, Level>>())
    {
        return lhs.template all_close<Level>(rhs, atol, rtol);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename T>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_close(const ArCo& lhs, const T& rhs,
        const typename ArCo::template tol_type<T, Level>& atol
        = default_atol<typename ArCo::template tol_type<T, Level>>(),
        const typename ArCo::template tol_type<T, Level>& rtol
        = default_rtol<typename ArCo::template tol_type<T, Level>>())
    {
        return lhs.template all_close<Level>(rhs, atol, rtol);
    }

    template <std::int64_t Level, typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_close(const T& lhs, const ArCo& rhs,
        const typename ArCo::template tol_type<T, Level>& atol
        = default_atol<typename ArCo::template tol_type<T, Level>>(),
        const typename ArCo::template tol_type<T, Level>& rtol
        = default_rtol<typename ArCo::template tol_type<T, Level>>())
    {
        return rhs.template all_close<Level>(lhs, atol, rtol);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr bool all_close(const ArCo1& lhs, const ArCo2& rhs,
        const typename ArCo1::template compliant_tol_type<ArCo2>& atol
        = default_atol<typename ArCo1::template compliant_tol_type<ArCo2>>(),
        const typename ArCo1::template compliant_tol_type<ArCo2>& rtol
        = default_rtol<typename ArCo1::template compliant_tol_type<ArCo2>>())
    {
        return all_close<ArCo1::depth>(lhs, rhs, atol, rtol);
    }

    template <arrnd_compliant ArCo, typename T>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_close(const ArCo& lhs, const T& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return all_close<ArCo::depth>(lhs, rhs, atol, rtol);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool all_close(const T& lhs, const ArCo& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return all_close<ArCo::depth>(lhs, rhs, atol, rtol);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr bool any_equal(const ArCo& lhs, const T& rhs)
    {
        return lhs.template any_equal<Level>(rhs);
    }

    template <std::int64_t Level, typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool any_equal(const T& lhs, const ArCo& rhs)
    {
        return rhs.template any_equal<Level>(lhs);
    }

    template <arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr bool any_equal(const ArCo& lhs, const T& rhs)
    {
        return any_equal<ArCo::depth>(lhs, rhs);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool any_equal(const T& lhs, const ArCo& rhs)
    {
        return any_equal<ArCo::depth>(lhs, rhs);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr bool any_close(const ArCo1& lhs, const ArCo2& rhs,
        const typename ArCo1::template compliant_tol_type<ArCo2, Level>& atol
        = default_atol<typename ArCo1::template compliant_tol_type<ArCo2, Level>>(),
        const typename ArCo1::template compliant_tol_type<ArCo2, Level>& rtol
        = default_rtol<typename ArCo1::template compliant_tol_type<ArCo2, Level>>())
    {
        return lhs.template any_close<Level>(rhs, atol, rtol);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename T>
    [[nodiscard]] inline constexpr bool any_close(const ArCo& lhs, const T& rhs,
        const typename ArCo::template tol_type<T, Level>& atol
        = default_atol<typename ArCo::template tol_type<T, Level>>(),
        const typename ArCo::template tol_type<T, Level>& rtol
        = default_rtol<typename ArCo::template tol_type<T, Level>>())
    {
        return lhs.template any_close<Level>(rhs, atol, rtol);
    }

    template <std::int64_t Level, typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr bool any_close(const T& lhs, const ArCo& rhs,
        const typename ArCo::template tol_type<T, Level>& atol
        = default_atol<typename ArCo::template tol_type<T, Level>>(),
        const typename ArCo::template tol_type<T, Level>& rtol
        = default_rtol<typename ArCo::template tol_type<T, Level>>())
    {
        return rhs.template any_close<Level>(lhs, atol, rtol);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr bool any_close(const ArCo1& lhs, const ArCo2& rhs,
        const typename ArCo1::template compliant_tol_type<ArCo2>& atol
        = default_atol<typename ArCo1::template compliant_tol_type<ArCo2>>(),
        const typename ArCo1::template compliant_tol_type<ArCo2>& rtol
        = default_rtol<typename ArCo1::template compliant_tol_type<ArCo2>>())
    {
        return any_close<ArCo1::depth>(lhs, rhs, atol, rtol);
    }

    template <arrnd_compliant ArCo, typename T>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool any_close(const ArCo& lhs, const T& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return any_close<ArCo::depth>(lhs, rhs, atol, rtol);
    }

    template <typename T, arrnd_compliant ArCo>
        requires(!arrnd_compliant<T>)
    [[nodiscard]] inline constexpr bool any_close(const T& lhs, const ArCo& rhs,
        const typename ArCo::template tol_type<T>& atol = default_atol<typename ArCo::template tol_type<T>>(),
        const typename ArCo::template tol_type<T>& rtol = default_rtol<typename ArCo::template tol_type<T>>())
    {
        return any_close<ArCo::depth>(lhs, rhs, atol, rtol);
    }

    template <arrnd_compliant ArCo>
    std::ostream& ostream_operator_recursive(std::ostream& os, const ArCo& arco,
        typename ArCo::size_type nvectical_spaces, typename ArCo::size_type ndepth_spaces)
    {
        constexpr auto block_start_char = ArCo::depth > 0 ? '{' : '[';
        constexpr auto block_stop_char = ArCo::depth > 0 ? '}' : ']';

        if (empty(arco)) {
            os << block_start_char << block_stop_char;
            return os;
        }

        if constexpr (ArCo::is_flat) {
            if (std::ssize(arco.header().dims()) > 1) {
                os << block_start_char;
                for (typename ArCo::size_type i = 0; i < arco.header().dims()[0]; ++i) {
                    if (i > 0) {
                        for (typename ArCo::size_type i = 0; i < ndepth_spaces + nvectical_spaces + 1; ++i) {
                            os << ' ';
                        }
                    }
                    ostream_operator_recursive(
                        os, arco[typename ArCo::interval_type{i, i + 1}], nvectical_spaces + 1, ndepth_spaces);
                    if (i < arco.header().dims()[0] - 1) {
                        os << '\n';
                    }
                }
                os << block_stop_char;
                return os;
            }

            os << block_start_char;
            typename ArCo::indexer_type gen(arco.header());
            os << arco[*gen];
            ++gen;
            for (; gen; ++gen) {
                os << ' ' << arco[*gen];
            }
            os << block_stop_char;
        } else {
            os << block_start_char;
            typename ArCo::indexer_type gen(arco.header());
            typename ArCo::size_type inner_count = 0;
            for (; gen; ++gen) {
                ostream_operator_recursive(os, arco[*gen], nvectical_spaces, ndepth_spaces + 1);
                if (++inner_count < arco.header().numel()) {
                    os << '\n';
                    for (typename ArCo::size_type i = 0; i < ndepth_spaces + 1; ++i) {
                        os << ' ';
                    }
                }
            }
            os << block_stop_char;
        }

        return os;
    }

    template <arrnd_compliant ArCo>
    inline constexpr std::ostream& operator<<(std::ostream& os, const ArCo& arco)
    {
        //arrnd<typename ArCo::value_type, typename ArCo::storage_info/*storage_type*/, ArCo::template shared_ref_allocator_type,
        //    typename ArCo::header_type/*, arrnd_indexer, arrnd_axis_ranger*/>
        //    carco = arco;
        arrnd<typename ArCo::value_type, typename ArCo::data_storage_info /*storage_info*/ /*storage_type*/,
            typename ArCo::dims_storage_info /*header_type*/,
            ArCo::template shared_ref_allocator_type /*, arrnd_indexer, arrnd_axis_ranger*/>
            carco = arco;
        typename ArCo::size_type nvectical_spaces = 0;
        typename ArCo::size_type ndepth_spaces = 0;
        return ostream_operator_recursive(os, carco, nvectical_spaces, ndepth_spaces);
    }

    struct arrnd_json_manip {
        explicit arrnd_json_manip(std::ostream& os)
            : os_(os)
        { }

        template <typename T>
            requires(!arrnd_compliant<T>)
        friend std::ostream& operator<<(const arrnd_json_manip& ajm, const T& rhs)
        {
            return ajm.os_ << rhs;
        }

        template <arrnd_compliant ArCo>
        friend std::ostream& operator<<(const arrnd_json_manip& ajm, const ArCo& arco)
        {
            //arrnd<typename ArCo::value_type, typename ArCo::storage_info/*storage_type*/, ArCo::template shared_ref_allocator_type,
            //    typename ArCo::header_type/*, arrnd_indexer, arrnd_axis_ranger*/>
            //    carco = arco;
            arrnd<typename ArCo::value_type, typename ArCo::data_storage_info /*storage_info*/ /*storage_type*/,
                typename ArCo::dims_storage_info /*header_type*/,
                ArCo::template shared_ref_allocator_type /*, arrnd_indexer, arrnd_axis_ranger*/>
                carco = arco;
            typename ArCo::size_type nvertical_spaces = 4;
            ajm.os_ << "{\n";
            ajm.os_ << std::string(nvertical_spaces, ' ') << "\"base_type\": \""
                    << type_name<typename ArCo::template inner_value_type<ArCo::depth>>() << "\"\n";
            to_json(ajm.os_, carco, nvertical_spaces);
            ajm.os_ << "}";
            return ajm.os_;
        }

    private:
        template <arrnd_compliant ArCo>
        static std::ostream& to_json(std::ostream& os, const ArCo& arco, typename ArCo::size_type nvertical_spaces)
        {
            auto replace_newlines = [](std::string s) {
                std::string::size_type n = 0;
                std::string d = s;
                while ((n = d.find("\n", n)) != std::string::npos) {
                    d.replace(n, 1, "\\n");
                    n += 2;
                }
                return d;
            };

            if (empty(arco)) {
                os << std::string(nvertical_spaces, ' ') << "\"header\": \"empty\",\n";
                os << std::string(nvertical_spaces, ' ') << "\"values\": \"empty\"\n";
                return os;
            }

            if constexpr (ArCo::is_flat) {
                // header
                {
                    std::stringstream ss;
                    ss << arco.header();
                    os << std::string(nvertical_spaces, ' ') << "\"header\": \"" << replace_newlines(ss.str())
                       << "\",\n";
                }
                // array
                {
                    std::stringstream ss;
                    ss << arco;
                    os << std::string(nvertical_spaces, ' ') << "\"values\": \"" << replace_newlines(ss.str())
                       << "\"\n";
                }
            } else {
                // header
                {
                    std::stringstream ss;
                    ss << arco.header();
                    os << std::string(nvertical_spaces, ' ') << "\"header\": \"" << replace_newlines(ss.str())
                       << "\",\n";
                }
                // arrays
                os << std::string(nvertical_spaces, ' ') << "\"arrays\": [\n";
                typename ArCo::indexer_type gen(arco.header());
                for (typename ArCo::size_type i = 0; gen; ++gen, ++i) {
                    os << std::string(nvertical_spaces + 4, ' ') << "{\n";
                    to_json(os, arco[*gen], nvertical_spaces + 8);
                    os << std::string(nvertical_spaces + 4, ' ') << '}';
                    if (i < arco.header().numel() - 1) {
                        os << ',';
                    }
                    os << '\n';
                }
                os << std::string(nvertical_spaces, ' ') << "]\n";
            }

            return os;
        }

        std::ostream& os_;
    };

    static constexpr struct arrnd_json_manip_tag {
    } arrnd_json{};
    inline arrnd_json_manip operator<<(std::ostream& os, arrnd_json_manip_tag)
    {
        return arrnd_json_manip(os);
    }
}

using details::arrnd_compliant;
using details::arrnd_compliant_of_type;
using details::arrnd_compliant_of_template_type;
using details::arrnd_compliant_with_trait;
using details::arrnd_json;

using details::arrnd_inner;
using details::arrnd_inner_t;

using details::arrnd_returned_element_iterator_tag;
using details::arrnd_returned_slice_iterator_tag;

using details::arrnd_iterator;
using details::arrnd_const_iterator;
using details::arrnd_reverse_iterator;
using details::arrnd_const_reverse_iterator;

using details::arrnd_slice_iterator;
using details::arrnd_slice_const_iterator;
using details::arrnd_slice_reverse_iterator;
using details::arrnd_slice_reverse_const_iterator;

using details::arrnd_back_inserter;
using details::arrnd_front_inserter;
using details::arrnd_inserter;

using details::arrnd_slice_back_inserter;
using details::arrnd_slice_front_inserter;
using details::arrnd_slice_inserter;

using details::arrnd_shape_preset;
using details::arrnd_diag_type;
using details::arrnd_filter_proxy;
using details::arrnd;

using details::zipped_cont;
using details::zipped_iter;
using details::zip;

using details::begin;
using details::cbegin;
using details::end;
using details::cend;
using details::rbegin;
using details::crbegin;
using details::rend;
using details::crend;

using details::copy;
using details::set;
using details::clone;
using details::reshape;
using details::resize;
using details::append;
using details::insert;
using details::repeat;
using details::remove;

using details::empty;
using details::expand;
using details::collapse;
using details::zeros;
using details::eye;
using details::pages;
using details::split;
using details::exclude;
using details::merge;
using details::movop;
using details::cumop;
using details::pageop;
using details::squeeze;
using details::sort;
using details::is_sorted;
using details::reorder;
using details::adjacent_indices;
using details::all_match;
using details::any_match;
using details::transform;
using details::apply;
using details::reduce;
using details::fold;
using details::all;
using details::any;
using details::sum;
using details::prod;
using details::min;
using details::max;
using details::matmul;
using details::det;
using details::inverse;
using details::solve;
using details::tril;
using details::triu;

using details::filter;
using details::find;
using details::diag;
using details::is_banded;
using details::transpose;
//using details::nest; // deprecated
using details::close;
using details::all_equal;
using details::all_close;
using details::any_equal;
using details::any_close;

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
using details::round;
using details::ceil;
using details::floor;
using details::real;
using details::imag;
using details::arg;
using details::norm;
using details::conj;
using details::proj;
using details::polar;
using details::sign;

// the functions in the experimental namespace may still be reachable
// from the oc namespace due to ADL
namespace experimental {
    using details::cholesky;
    using details::lu;
    using details::qr;
    using details::hess;
    using details::schur;
    using details::svd;
    using details::eig;
}

}

// swap function for zip class iterator usage in std algorithms
// because its operator* not returning reference
namespace std {
template <typename Tuple>
void swap(Tuple&& lhs, Tuple&& rhs)
{
    lhs.swap(rhs);
}
}

#endif // OC_ARRAY_H
