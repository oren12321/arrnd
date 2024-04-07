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
    template <typename T>
    //requires(!std::is_reference_v<T>)
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
        //requires(!std::is_reference_v<U>)
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
    //requires(std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>)
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
    //requires(std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>)
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
    template <std::integral T>
    [[nodiscard]] inline constexpr T default_atol() noexcept
    {
        return T{0};
    }

    template <std::floating_point T>
    [[nodiscard]] inline constexpr T default_atol() noexcept
    {
        return T{1e-8};
    }

    template <std::integral T>
    [[nodiscard]] inline constexpr T default_rtol() noexcept
    {
        return T{0};
    }

    template <std::floating_point T>
    [[nodiscard]] inline constexpr T default_rtol() noexcept
    {
        return T{1e-5};
    }

    template <typename T1, typename T2>
        requires(std::is_arithmetic_v<T1> && std::is_arithmetic_v<T2>)
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
}

using details::default_atol;
using details::default_rtol;

using details::close;
using details::modulo;
}

namespace oc {
namespace details {
    enum class interval_type { full, from, to, none };

    /**
    * @note half open interval
    */
    template <std::integral T = std::int64_t>
    class interval {
    public:
        // interval type might cause ignoring values of interval's start or stop values
        constexpr interval(T start, T stop, T step = 1, interval_type type = interval_type::none) noexcept
            : start_(start)
            , stop_(stop)
            , step_(step)
            , type_(type)
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

        [[nodiscard]] constexpr interval_type type() const noexcept
        {
            return type_;
        }

        // returns normalize type from dimension, useful in case of interval types that are not none
        [[nodiscard]] constexpr interval align(T dim) const noexcept
        {
            switch (type_) {
            case interval_type::none:
                return *this;
            case interval_type::full:
                return interval{0, dim, step_, interval_type::none};
            case interval_type::from:
                return interval{start_, dim, step_, interval_type::none};
            case interval_type::to:
                return interval{0, stop_, step_, interval_type::none};
            }

            return *this;
        }

        [[nodiscard]] static constexpr interval full(T step = 1) noexcept
        {
            return interval{std::numeric_limits<T>::max(), std::numeric_limits<T>::max(), step, interval_type::full};
        }

        [[nodiscard]] static constexpr interval from(T start, T step = 1)
        {
            return interval{start, std::numeric_limits<T>::max(), step, interval_type::from};
        }

        [[nodiscard]] static constexpr interval to(T stop, T step = 1) noexcept
        {
            return interval{0, stop, step, interval_type::to};
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
        interval_type type_{interval_type::none};
    };

    template <std::integral T>
    [[nodiscard]] inline constexpr interval<T> reverse(const interval<T>& i) noexcept
    {
        return interval<T>{i.stop(), i.start(), -i.step()};
    }

    template <std::integral T>
    [[nodiscard]] inline constexpr interval<T> modulo(const interval<T>& i, const T& modulus) noexcept
    {
        return interval<T>{modulo(i.start(), modulus), modulo(i.stop(), modulus), i.step()};
    }

    template <std::integral T>
    [[nodiscard]] inline constexpr interval<T> forward(const interval<T>& i) noexcept
    {
        return i.step() < T{0} ? reverse(i) : i;
    }

    template <std::integral T>
    [[nodiscard]] inline constexpr bool operator==(const interval<T>& lhs, const interval<T>& rhs) noexcept
    {
        return (lhs.type() == interval_type::none && rhs.type() == interval_type::none && lhs.start() == rhs.start()
                   && lhs.stop() == rhs.stop() && lhs.step() == rhs.step())
            || (lhs.type() == interval_type::full && rhs.type() == interval_type::full && lhs.step() == rhs.step())
            || (lhs.type() == interval_type::from && rhs.type() == interval_type::from && lhs.start() == rhs.start()
                && lhs.step() == rhs.step())
            || (lhs.type() == interval_type::to && rhs.type() == interval_type::to && lhs.start() == 0
                && rhs.start() == 0 && lhs.stop() == rhs.stop() && lhs.step() == rhs.step());
    }
}

using details::interval_type;
using details::interval;

using details::modulo;
using details::reverse;
using details::forward;
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
    template <typename Iter>
    concept integral_type_iterator = std::input_iterator<Iter> && std::is_integral_v<iterator_value_type<Iter>>;
    template <typename Iter>
    concept interval_type_iterator
        = std::input_iterator<Iter> && is_template_type<interval, iterator_value_type<Iter>>::value;

    template <typename Cont>
    concept iterable = requires(Cont&& c) {
                           {
                               std::begin(c)
                           };
                           {
                               std::end(c)
                           };
                       } && !
    std::is_array_v<Cont>;
    template <typename Cont, typename T>
    concept iterable_of_type = iterable<Cont> && requires(Cont&& c) {
                                                     {
                                                         std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                         } -> std::same_as<T>;
                                                 };
    template <typename Cont>
    concept integral_type_iterable = iterable<Cont> && requires(Cont&& c) {
                                                           {
                                                               std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                               } -> std::integral;
                                                       };
    template <typename Cont>
    concept interval_type_iterable = iterable<Cont> && requires(Cont&& c) {
                                                           {
                                                               std::remove_cvref_t<decltype(*std::begin(c))>{}
                                                               } -> template_type<interval>;
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
using details::integral_type_iterator;
using details::interval_type_iterator;
using details::iterable;
using details::iterable_of_type;
using details::interval_type_iterable;
using details::integral_type_iterable;
using details::random_access_type;

using details::array_cast;
using details::const_array_cast;
}

namespace oc {
namespace details {
    struct arrnd_header_tag { };
    template <typename T>
    concept arrnd_header_compliant = std::is_same_v<typename T::tag, arrnd_header_tag>;

    template <random_access_type Storage = simple_dynamic_vector<std::int64_t>>
    class arrnd_header {
    public:
        using storage_type = Storage;
        using value_type = typename Storage::value_type;
        using size_type = typename Storage::size_type;

        using tag = arrnd_header_tag;

        constexpr arrnd_header() = default;

        template <integral_type_iterator InputIt>
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

            strides_ = storage_type(dims_.size());
            std::exclusive_scan(dims_.crbegin(), dims_.crend(), strides_.rbegin(), value_type{1}, std::multiplies<>{});

            last_index_ = numel_ - 1;
        }

        template <integral_type_iterable Cont>
        explicit constexpr arrnd_header(const Cont& dims)
            : arrnd_header(std::begin(dims), std::end(dims))
        { }

        template <std::integral U = value_type>
        explicit constexpr arrnd_header(std::initializer_list<U> dims)
            : arrnd_header(dims.begin(), dims.end())
        { }

        template <std::integral D, std::int64_t M>
        explicit constexpr arrnd_header(const D (&dims)[M])
            : arrnd_header(std::begin(dims), std::end(dims))
        { }

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

            size_type nranges = std::min(std::distance(first_range, last_range), dims_.size());

            auto valid_ranges = [&]() {
                return std::inner_product(first_range, std::next(first_range, nranges), dims_.cbegin(), true,
                    std::logical_and<>{}, [](const auto& r, auto d) {
                        auto nr = r.align(d);
                        return (nr.start() < nr.stop() && nr.step() >= 1) && (nr.start() >= 0 && nr.stop() <= d);
                    });
            };
            assert(valid_ranges());

            arrnd_header res{};

            res.dims_ = storage_type(dims_.size());
            std::transform(first_range, std::next(first_range, nranges), dims_.cbegin(), res.dims_.begin(),
                [](const auto& r, auto d) {
                    auto nr = r.align(d);
                    return static_cast<value_type>(std::ceil(static_cast<double>(nr.stop() - nr.start()) / nr.step()));
                });
            //std::transform(first_range, std::next(first_range, nranges), res.dims_.begin(), [](const auto& r) {
            //    return static_cast<value_type>(std::ceil(static_cast<double>(r.stop() - r.start()) / r.step()));
            //});
            std::copy(std::next(dims_.cbegin(), nranges), dims_.cend(), std::next(res.dims_.begin(), nranges));

            if (std::equal(res.dims_.cbegin(), res.dims_.cend(), dims_.cbegin(), dims_.cend())) {
                return *this;
            }

            res.numel_ = std::reduce(res.dims_.cbegin(), res.dims_.cend(), value_type{1}, std::multiplies<>{});

            res.strides_ = storage_type(res.dims_.size());
            //std::transform(strides_.cbegin(), std::next(strides_.cbegin(), nranges), first_range, res.strides_.begin(),
            //    [](auto s, const auto& r) {
            //        return s * r.step();
            //    });
            res.offset_ = offset_;
            for (size_type i = 0; i < nranges; ++i) {
                auto s = *std::next(strides_.cbegin(), i);
                auto nr = (*std::next(first_range, i)).align(*std::next(dims_.cbegin(), i));
                *std::next(res.strides_.begin(), i) = s * nr.step();
                res.offset_ += s * nr.start();
            }
            std::copy(std::next(strides_.cbegin(), nranges), strides_.cend(), std::next(res.strides_.begin(), nranges));

            //res.offset_ = offset_
            //    + std::transform_reduce(strides_.cbegin(), std::next(strides_.cbegin(), nranges), first_range,
            //        value_type{0}, std::plus<>{}, [](auto s, const auto& r) {
            //            return s * r.start();
            //        });

            res.last_index_ = res.offset_
                + std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(), value_type{0},
                    std::plus<>{}, [](auto d, auto s) {
                        return (d - 1) * s;
                    });

            res.is_slice_ = true;

            return res;
        }

        template <interval_type_iterable Cont>
        [[nodiscard]] constexpr arrnd_header subheader(const Cont& ranges) const
        {
            return subheader(std::begin(ranges), std::end(ranges));
        }

        template <std::integral U = value_type>
        [[nodiscard]] constexpr arrnd_header subheader(std::initializer_list<interval<U>> ranges) const
        {
            return subheader(ranges.begin(), ranges.end());
        }

        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr arrnd_header subheader(const interval<U> (&ranges)[M]) const
        {
            return subheader(std::begin(ranges), std::end(ranges));
        }

        template <std::integral U>
        [[nodiscard]] constexpr arrnd_header subheader(interval<U> range) const
        {
            std::initializer_list<interval<value_type>> ranges = {range.align(dims_.front())};

            auto res = subheader(ranges.begin(), ranges.end());
            if (res.empty() || res.dims_.front() != 1) {
                return res;
            }

            res.dims_ = storage_type(std::next(res.dims_.cbegin(), 1), res.dims_.cend());
            res.strides_ = storage_type(std::next(res.strides_.cbegin(), 1), res.strides_.cend());
            res.last_index_ = res.offset_
                + std::inner_product(res.dims_.cbegin(), res.dims_.cend(), res.strides_.cbegin(), value_type{0},
                    std::plus<>{}, [](auto d, auto s) {
                        return (d - 1) * s;
                    });
            res.is_slice_ = true;

            return res;
        }

        template <std::integral U>
        [[nodiscard]] constexpr arrnd_header subheader(U omitted_axis) const
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
            std::copy(
                std::next(dims_.cbegin(), omitted_axis + 1), dims_.cend(), std::next(new_dims.begin(), omitted_axis));

            return arrnd_header(new_dims.cbegin(), new_dims.cend());
        }

        template <std::integral U, std::integral V>
        [[nodiscard]] constexpr arrnd_header subheader(U count, V axis) const
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

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr arrnd_header reorder(const InputIt& first_order, const InputIt& last_order) const
        {
            assert(std::distance(first_order, last_order) == dims_.size());
            assert(std::all_of(first_order, last_order, [&](auto order) {
                return order >= 0 && order < dims_.size();
            }));

            if (empty() || dims_.size() == 1) {
                return *this;
            }

            arrnd_header res(*this);

            for (size_type i = 0; i < dims_.size(); ++i) {
                *std::next(res.dims_.begin(), i) = *std::next(dims_.cbegin(), *std::next(first_order, i));
                *std::next(res.strides_.begin(), i) = *std::next(strides_.cbegin(), *std::next(first_order, i));
            }
            //res.is_reordered_ = true;

            return res;
        }

        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr arrnd_header reorder(const Cont& order) const
        {
            return reorder(std::begin(order), std::end(order));
        }

        template <std::integral U = size_type>
        [[nodiscard]] constexpr arrnd_header reorder(std::initializer_list<U> order) const
        {
            return reorder(order.begin(), order.end());
        }

        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr arrnd_header reorder(const U (&order)[M]) const
        {
            return reorder(std::begin(order), std::end(order));
        }

        template <std::integral U>
        [[nodiscard]] constexpr arrnd_header reorder(U main_axis) const
        {
            if (empty() || dims_.size() == 1 || main_axis == 0) {
                return *this;
            }

            assert(main_axis >= 0 && main_axis < dims_.size());

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

            //res.is_reordered_ = true;

            return res;
        }

        [[nodiscard]] constexpr arrnd_header squeeze() const
        {
            if (empty() || dims_.size() == 1) {
                return *this;
            }

            size_type ones_count = std::count(dims_.cbegin(), dims_.cend(), value_type{1});
            if (ones_count == 0) {
                return *this;
            }

            arrnd_header res(*this);

            res.dims_ = storage_type(dims_.size() - ones_count);
            std::copy_if(dims_.cbegin(), dims_.cend(), res.dims_.begin(), [](auto d) {
                return d != value_type{1};
            });

            res.strides_ = storage_type(strides_.size() - ones_count);
            size_type j = 0;
            for (size_type i = 0; i < strides_.size(); ++i) {
                if (*std::next(dims_.cbegin(), i) != value_type{1}) {
                    *std::next(res.strides_.begin(), j) = *std::next(strides_.cbegin(), i);
                    ++j;
                }
            }

            res.is_slice_ = true;

            return res;
        }

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr value_type subs2ind(const InputIt& first_sub, const InputIt& last_sub) const
        {
            assert(first_sub < last_sub); // at least one subscript is required

            size_type nsubs = std::distance(first_sub, last_sub);
            assert(nsubs > 0 && nsubs <= dims_.size());

            auto valid_subs = [&]() {
                return std::inner_product(first_sub, last_sub, std::next(dims_.cbegin(), dims_.size() - nsubs), true,
                    std::logical_and<>{}, [](auto s, auto d) {
                        return (s >= 0 && s < d);
                    });
            };
            assert(valid_subs());

            return offset_
                + std::transform_reduce(first_sub, last_sub, std::next(strides_.cbegin(), strides_.size() - nsubs),
                    value_type{0}, std::plus<>{}, std::multiplies<>{});
        }

        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr value_type subs2ind(const Cont& subs) const
        {
            return sub2ind(std::begin(subs), std::end(subs));
        }

        template <std::integral U = value_type>
        [[nodiscard]] constexpr value_type subs2ind(std::initializer_list<U> subs) const
        {
            return sub2ind(subs.begin(), subs.end());
        }

        constexpr arrnd_header(arrnd_header&& other) = default;
        constexpr arrnd_header& operator=(arrnd_header&& other) = default;

        constexpr arrnd_header(const arrnd_header& other) = default;
        constexpr arrnd_header& operator=(const arrnd_header& other) = default;

        virtual constexpr ~arrnd_header() = default;

        [[nodiscard]] constexpr value_type numel() const noexcept
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

        [[nodiscard]] constexpr value_type offset() const noexcept
        {
            return offset_;
        }

        [[nodiscard]] constexpr bool is_slice() const noexcept
        {
            return is_slice_;
        }

        //[[nodiscard]] constexpr bool is_reordered() const noexcept // deprecated
        //{
        //    return is_reordered_;
        //}

        [[nodiscard]] constexpr bool empty() const noexcept
        {
            return dims_.empty();
        }

        [[nodiscard]] constexpr value_type last_index() const noexcept
        {
            return last_index_;
        }

        [[nodiscard]] constexpr bool is_vector() const noexcept
        {
            return dims_.size() == 1;
        }

        [[nodiscard]] constexpr bool is_row() const noexcept
        {
            return dims_.size() == 2 && dims_.front() == 1;
        }

        [[nodiscard]] constexpr bool is_column() const noexcept
        {
            return dims_.size() == 2 && dims_.back() == 1;
        }

        [[nodiscard]] constexpr bool is_matrix() const noexcept
        {
            return dims_.size() == 2;
        }

        [[nodiscard]] constexpr bool is_scalar() const noexcept
        {
            return numel_ == 1;
        }

    private:
        storage_type dims_{};
        storage_type strides_{};
        value_type numel_{0};
        value_type offset_{0};
        value_type last_index_{0};
        bool is_slice_{false};
        //bool is_reordered_{false};
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
           << ')';

        return os;
    }
}

using details::arrnd_header_tag;
using details::arrnd_header_compliant;
using details::arrnd_header;
}

namespace oc {
namespace details {
    enum class arrnd_indexer_position { begin, end, rbegin, rend };

    template <arrnd_header_compliant Header = arrnd_header<>>
    class arrnd_general_indexer final {
    public:
        using storage_type = typename Header::storage_type;
        using header_type = Header;
        using size_type = typename Header::size_type;
        using value_type = typename Header::value_type;

        explicit constexpr arrnd_general_indexer(
            const header_type& hdr, arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : hdr_(hdr)
        {
            setup(pos);
        }

        template <std::integral U>
        explicit constexpr arrnd_general_indexer(
            const header_type& hdr, U axis, arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : hdr_(hdr.reorder(axis))
        {
            setup(pos);
        }

        template <integral_type_iterator InputIt>
        explicit constexpr arrnd_general_indexer(const header_type& hdr, const InputIt& first_order,
            const InputIt& last_order, arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : hdr_(hdr.reorder(first_order, last_order))
        {
            setup(pos);
        }

        template <integral_type_iterable Cont>
        explicit constexpr arrnd_general_indexer(
            const header_type& hdr, const Cont& order, arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : arrnd_general_indexer(hdr, std::begin(order), std::end(order), pos)
        { }

        template <std::integral U = size_type>
        explicit constexpr arrnd_general_indexer(const header_type& hdr, std::initializer_list<U> order,
            arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : arrnd_general_indexer(hdr, order.begin(), order.end(), pos)
        { }

        template <std::integral U, std::int64_t M>
        explicit constexpr arrnd_general_indexer(
            const header_type& hdr, const U (&order)[M], arrnd_indexer_position pos = arrnd_indexer_position::begin)
            : arrnd_general_indexer(hdr, std::begin(order), std::end(order), pos)
        { }

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

            for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
                ++firsts_[i].index;
                current_index_ += firsts_[i].stride;
                if (firsts_[i].index < firsts_[i].dim) {
                    return *this;
                }
                current_index_ -= firsts_[i].index * firsts_[i].stride;
                firsts_[i].index = 0;
            }

            for (size_type i = 3; i < hdr_.dims().size(); ++i) {
                ++indices_[hdr_.dims().size() - 1 - i];
                current_index_ += hdr_.strides()[hdr_.dims().size() - 1 - i];
                if (indices_[hdr_.dims().size() - 1 - i] < hdr_.dims()[hdr_.dims().size() - 1 - i]) {
                    return *this;
                }
                current_index_ -= indices_[hdr_.dims().size() - 1 - i] * hdr_.strides()[hdr_.dims().size() - 1 - i];
                indices_[hdr_.dims().size() - 1 - i] = 0;
            }

            return *this;
        }

        constexpr arrnd_general_indexer operator++(int) noexcept
        {
            arrnd_general_indexer<header_type> temp{*this};
            ++(*this);
            return temp;
        }

        template <std::integral U>
        constexpr arrnd_general_indexer& operator+=(U count) noexcept
        {
            for (U i = 0; i < count; ++i) {
                ++(*this);
            }
            return *this;
        }

        template <std::integral U>
        arrnd_general_indexer operator+(U count) const noexcept
        {
            arrnd_general_indexer<header_type> temp{*this};
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

            for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
                --firsts_[i].index;
                current_index_ -= firsts_[i].stride;
                if (firsts_[i].index > -1) {
                    return *this;
                }
                firsts_[i].index = firsts_[i].dim - 1;
                current_index_ += (firsts_[i].index + 1) * firsts_[i].stride;
            }

            for (size_type i = 3; i < hdr_.dims().size(); ++i) {
                --indices_[hdr_.dims().size() - 1 - i];
                current_index_ -= hdr_.strides()[hdr_.dims().size() - 1 - i];
                if (indices_[hdr_.dims().size() - 1 - i] > -1) {
                    return *this;
                }
                indices_[hdr_.dims().size() - 1 - i] = hdr_.dims()[i] - 1;
                current_index_
                    += (indices_[hdr_.dims().size() - 1 - i] + 1) * hdr_.strides()[hdr_.dims().size() - 1 - i];
            }

            return *this;
        }

        constexpr arrnd_general_indexer operator--(int) noexcept
        {
            arrnd_general_indexer<header_type> temp{*this};
            --(*this);
            return temp;
        }

        template <std::integral U>
        constexpr arrnd_general_indexer& operator-=(U count) noexcept
        {
            for (U i = 0; i < count; ++i) {
                --(*this);
            }
            return *this;
        }

        template <std::integral U>
        constexpr arrnd_general_indexer operator-(U count) const noexcept
        {
            arrnd_general_indexer<header_type> temp{*this};
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

        template <std::integral U>
        [[nodiscard]] constexpr value_type operator[](U index) const noexcept
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
        constexpr void setup(arrnd_indexer_position pos)
        {
            last_first_diff_ = static_cast<std::make_unsigned_t<value_type>>(hdr_.last_index() - hdr_.offset());

            bool backward = (pos == arrnd_indexer_position::rbegin || pos == arrnd_indexer_position::end);

            for (size_type i = 0; i < 3 && i < hdr_.dims().size(); ++i) {
                firsts_[i].dim = hdr_.dims()[hdr_.dims().size() - i - 1];
                firsts_[i].stride = hdr_.strides()[hdr_.dims().size() - i - 1];
                firsts_[i].index = backward ? firsts_[i].dim - 1 : 0;
            }

            indices_ = storage_type(hdr_.dims().size());
            for (size_type i = 3; i < hdr_.dims().size(); ++i) {
                indices_[hdr_.dims().size() - 1 - i] = backward ? hdr_.dims()[hdr_.dims().size() - 1 - i] - 1 : 0;
            }

            current_index_ = backward ? hdr_.last_index() : hdr_.offset();

            rel_pos_ = backward ? hdr_.numel() - 1 : 0;

            if (pos == arrnd_indexer_position::end) {
                ++(*this);
            } else if (pos == arrnd_indexer_position::rend) {
                --(*this);
            }
        }

        header_type hdr_;

        std::make_unsigned_t<value_type> last_first_diff_;

        struct data_package {
            value_type dim;
            value_type stride;
            value_type index;
        };

        data_package firsts_[3];

        storage_type indices_;
        value_type current_index_;

        size_type rel_pos_ = 0;
    };

    //template <arrnd_header_compliant Header = arrnd_header<>> // deprecated
    //class arrnd_fast_indexer final {
    //public:
    //    using header_type = Header;
    //    using size_type = typename Header::size_type;
    //    using value_type = typename Header::value_type;

    //    explicit constexpr arrnd_fast_indexer(
    //        const header_type& hdr, arrnd_indexer_position pos = arrnd_indexer_position::begin)
    //        : arrnd_fast_indexer(hdr, 0, pos)
    //    { }

    //    explicit constexpr arrnd_fast_indexer(
    //        const header_type& hdr, size_type axis, arrnd_indexer_position pos = arrnd_indexer_position::begin)
    //    {
    //        assert(!hdr.is_slice() && !hdr.is_reordered());
    //        assert(axis >= 0 && axis < hdr.dims().size());

    //        last_index_ = hdr.last_index();

    //        num_super_groups_ = *std::next(hdr.dims().cbegin(), axis);
    //        step_size_between_super_groups_ = *std::next(hdr.strides().cbegin(), axis);

    //        num_groups_in_super_group_ = std::accumulate(hdr.dims().cbegin(), std::next(hdr.dims().cbegin(), axis + 1),
    //                                         value_type{1}, std::multiplies<>{})
    //            / num_super_groups_;
    //        group_size_ = *std::next(hdr.strides().cbegin(), axis);
    //        step_size_inside_group_ = hdr.strides().back();
    //        step_size_between_groups_ = num_super_groups_ * step_size_between_super_groups_;

    //        bool backward = (pos == arrnd_indexer_position::rbegin || pos == arrnd_indexer_position::end);

    //        if (!backward) {
    //            group_indices_counter_ = 0;
    //            groups_counter_ = 0;
    //            super_groups_counter_ = 0;

    //            super_group_start_index_ = 0;
    //            group_start_index_ = 0;

    //            current_index_ = 0;
    //            rel_pos_ = 0;
    //        } else {
    //            group_indices_counter_ = group_size_ - 1;
    //            groups_counter_ = num_groups_in_super_group_ - 1;
    //            super_groups_counter_ = num_super_groups_ - 1;

    //            super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
    //            group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

    //            current_index_ = last_index_;
    //            rel_pos_ = hdr.numel() - 1;
    //        }

    //        if (pos == arrnd_indexer_position::end) {
    //            ++(*this);
    //        } else if (pos == arrnd_indexer_position::rend) {
    //            --(*this);
    //        }
    //    }

    //    constexpr arrnd_fast_indexer() = default;

    //    constexpr arrnd_fast_indexer(const arrnd_fast_indexer& other) = default;
    //    constexpr arrnd_fast_indexer& operator=(const arrnd_fast_indexer& other) = default;

    //    constexpr arrnd_fast_indexer(arrnd_fast_indexer&& other) noexcept = default;
    //    constexpr arrnd_fast_indexer& operator=(arrnd_fast_indexer&& other) noexcept = default;

    //    constexpr ~arrnd_fast_indexer() = default;

    //    constexpr arrnd_fast_indexer& operator++() noexcept
    //    {
    //        if (current_index_ > last_index_) {
    //            return *this;
    //        }

    //        ++group_indices_counter_;

    //        current_index_ += step_size_inside_group_;

    //        if (group_indices_counter_ < group_size_) {
    //            ++rel_pos_;
    //            return *this;
    //        }

    //        group_indices_counter_ = 0;
    //        ++groups_counter_;
    //        group_start_index_ += step_size_between_groups_;

    //        current_index_ = group_start_index_;

    //        if (groups_counter_ < num_groups_in_super_group_) {
    //            ++rel_pos_;
    //            return *this;
    //        }

    //        groups_counter_ = 0;
    //        ++super_groups_counter_;
    //        super_group_start_index_ += step_size_between_super_groups_;
    //        group_start_index_ = super_group_start_index_;

    //        current_index_ = group_start_index_;

    //        if (super_groups_counter_ < num_super_groups_) {
    //            ++rel_pos_;
    //            return *this;
    //        }

    //        group_indices_counter_ = group_size_;
    //        groups_counter_ = num_groups_in_super_group_ - 1;
    //        super_groups_counter_ = num_super_groups_ - 1;
    //        super_group_start_index_ = super_groups_counter_ * step_size_between_super_groups_;
    //        group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

    //        current_index_ = last_index_ + 1;

    //        ++rel_pos_;
    //        return *this;
    //    }

    //    constexpr arrnd_fast_indexer operator++(int) noexcept
    //    {
    //        arrnd_fast_indexer<header_type> temp{*this};
    //        ++(*this);
    //        return temp;
    //    }

    //    constexpr arrnd_fast_indexer& operator+=(size_type count) noexcept
    //    {
    //        for (size_type i = 0; i < count; ++i) {
    //            ++(*this);
    //        }
    //        return *this;
    //    }

    //    constexpr arrnd_fast_indexer operator+(size_type count) const noexcept
    //    {
    //        arrnd_fast_indexer<header_type> temp{*this};
    //        temp += count;
    //        return temp;
    //    }

    //    constexpr arrnd_fast_indexer& operator--() noexcept
    //    {
    //        if (current_index_ < 0) {
    //            return *this;
    //        }

    //        --group_indices_counter_;

    //        current_index_ -= step_size_inside_group_;

    //        if (group_indices_counter_ >= 0) {
    //            --rel_pos_;
    //            return *this;
    //        }

    //        group_indices_counter_ = group_size_ - 1;
    //        --groups_counter_;
    //        group_start_index_ -= step_size_between_groups_;

    //        current_index_ = group_start_index_ + (group_size_ - 1) * step_size_inside_group_;

    //        if (groups_counter_ >= 0) {
    //            --rel_pos_;
    //            return *this;
    //        }

    //        groups_counter_ = num_groups_in_super_group_ - 1;
    //        --super_groups_counter_;
    //        super_group_start_index_ -= step_size_between_super_groups_;
    //        group_start_index_ = super_group_start_index_ + groups_counter_ * step_size_between_groups_;

    //        current_index_ = group_start_index_ + (group_size_ - 1) * step_size_inside_group_;

    //        if (super_groups_counter_ >= 0) {
    //            --rel_pos_;
    //            return *this;
    //        }

    //        group_indices_counter_ = -1;
    //        groups_counter_ = 0;
    //        super_groups_counter_ = 0;
    //        super_group_start_index_ = 0;
    //        group_start_index_ = 0;

    //        current_index_ = -1;

    //        --rel_pos_;
    //        return *this;
    //    }

    //    constexpr arrnd_fast_indexer operator--(int) noexcept
    //    {
    //        arrnd_fast_indexer<header_type> temp{*this};
    //        --(*this);
    //        return temp;
    //    }

    //    constexpr arrnd_fast_indexer& operator-=(size_type count) noexcept
    //    {
    //        for (size_type i = 0; i < count; ++i) {
    //            --(*this);
    //        }
    //        return *this;
    //    }

    //    constexpr arrnd_fast_indexer operator-(size_type count) const noexcept
    //    {
    //        arrnd_fast_indexer<header_type> temp{*this};
    //        temp -= count;
    //        return temp;
    //    }

    //    [[nodiscard]] explicit constexpr operator bool() const noexcept
    //    {
    //        return static_cast<std::make_unsigned_t<value_type>>(current_index_)
    //            <= static_cast<std::make_unsigned_t<value_type>>(last_index_);
    //    }

    //    [[nodiscard]] constexpr value_type operator*() const noexcept
    //    {
    //        return current_index_;
    //    }

    //    [[nodiscard]] constexpr value_type operator[](size_type index) const noexcept
    //    {
    //        assert(index >= 0 && index < num_super_groups_ * num_groups_in_super_group_ * group_size_);

    //        size_type advance_count = index - rel_pos_;
    //        if (advance_count > 0) {
    //            return ((*this) + advance_count).current_index_;
    //        }
    //        if (advance_count < 0) {
    //            return ((*this) - (-advance_count)).current_index_;
    //        }
    //        return current_index_;
    //    }

    //private:
    //    value_type current_index_ = 0;

    //    // data

    //    value_type last_index_ = 0;

    //    size_type num_super_groups_ = 0;
    //    value_type step_size_between_super_groups_ = 0;

    //    size_type num_groups_in_super_group_ = 0;
    //    size_type group_size_ = 0;
    //    value_type step_size_inside_group_ = 0;
    //    value_type step_size_between_groups_ = 0;

    //    // counters

    //    size_type super_groups_counter_ = 0;

    //    size_type group_indices_counter_ = 0;
    //    size_type groups_counter_ = 0;

    //    value_type super_group_start_index_ = 0;

    //    value_type group_start_index_ = 0;

    //    size_type rel_pos_ = 0;
    //};

    template <arrnd_header_compliant Header = arrnd_header<>>
    class arrnd_fixed_axis_ranger final {
    public:
        using header_type = Header;
        using size_type = typename Header::size_type;
        using value_type = typename Header::value_type;

        using storage_type = typename Header::storage_type::template replaced_type<interval<value_type>>;

        template <std::integral U>
        explicit constexpr arrnd_fixed_axis_ranger(
            const header_type& hdr, U fixed_axis = 0, bool backward = false, value_type interval_width = 1)
            : fixed_axis_(fixed_axis)
            , interval_width_(interval_width)
        {
            assert(fixed_axis >= 0 && fixed_axis < hdr.dims().size());
            assert(interval_width > 0 && interval_width <= *std::next(hdr.dims().cbegin(), fixed_axis));

            current_index_ = backward ? *std::next(hdr.dims().cbegin(), fixed_axis) - interval_width_ : 0;

            last_index_ = *std::next(hdr.dims().cbegin(), fixed_axis);

            ranges_ = storage_type(hdr.dims().size());
            for (size_type i = 0; i < hdr.dims().size(); ++i) {
                ranges_[i] = {0, *std::next(hdr.dims().cbegin(), i)};
            }
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
        }

        constexpr arrnd_fixed_axis_ranger() = default;

        constexpr arrnd_fixed_axis_ranger(const arrnd_fixed_axis_ranger& other) = default;
        constexpr arrnd_fixed_axis_ranger& operator=(const arrnd_fixed_axis_ranger& other) = default;

        constexpr arrnd_fixed_axis_ranger(arrnd_fixed_axis_ranger&& other) noexcept = default;
        constexpr arrnd_fixed_axis_ranger& operator=(arrnd_fixed_axis_ranger&& other) noexcept = default;

        constexpr ~arrnd_fixed_axis_ranger() = default;

        constexpr arrnd_fixed_axis_ranger& operator++() noexcept
        {
            if (current_index_ + interval_width_ > last_index_) {
                //current_index_ = last_index_;
                return *this;
            }
            ++current_index_;
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
            return *this;
        }

        constexpr arrnd_fixed_axis_ranger operator++(int) noexcept
        {
            arrnd_fixed_axis_ranger<header_type> temp{*this};
            ++(*this);
            return temp;
        }

        template <std::integral U>
        constexpr arrnd_fixed_axis_ranger& operator+=(U count) noexcept
        {
            if (current_index_ + interval_width_ > last_index_) {
                //current_index_ = last_index_;
                return *this;
            }
            current_index_ += count;
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
            if (current_index_ >= last_index_) {
                return *this;
            }
            return *this;
        }

        template <std::integral U>
        constexpr arrnd_fixed_axis_ranger operator+(U count) const noexcept
        {
            arrnd_fixed_axis_ranger<header_type> temp{*this};
            temp += count;
            return temp;
        }

        constexpr arrnd_fixed_axis_ranger& operator--() noexcept
        {
            if (current_index_ < 0) {
                current_index_ = -1;
            }
            --current_index_;
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
            return *this;
        }

        constexpr arrnd_fixed_axis_ranger operator--(int) noexcept
        {
            arrnd_fixed_axis_ranger<header_type> temp{*this};
            --(*this);
            return temp;
        }

        template <std::integral U>
        constexpr arrnd_fixed_axis_ranger& operator-=(U count) noexcept
        {
            if (current_index_ < 0) {
                current_index_ = -1;
            }
            current_index_ -= count;
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
            if (current_index_ < 0) {
                return *this;
            }
            return *this;
        }

        template <std::integral U>
        constexpr arrnd_fixed_axis_ranger operator-(U count) const noexcept
        {
            arrnd_fixed_axis_ranger<header_type> temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] explicit constexpr operator bool() const noexcept
        {
            return current_index_ >= 0 && current_index_ + interval_width_ <= last_index_;
        }

        [[nodiscard]] constexpr const storage_type& operator*() const noexcept
        {
            return ranges_;
        }

        template <std::integral U>
        [[nodiscard]] constexpr storage_type operator[](U index) const noexcept
        {
            assert(index >= 0 && index + interval_width_ <= last_index_);

            size_type advance_count = index - current_index_;
            if (advance_count > 0) {
                return ((*this) + advance_count).ranges_;
            }
            if (advance_count < 0) {
                return ((*this) - (-advance_count)).ranges_;
            }
            return ranges_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_fixed_axis_ranger& far) const noexcept
        {
            return current_index_ == far.current_index_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_fixed_axis_ranger& far) const noexcept
        {
            return current_index_ < far.current_index_;
        }

        [[nodiscard]] constexpr size_type fixed_axis() const noexcept
        {
            return fixed_axis_;
        }

        constexpr arrnd_fixed_axis_ranger& change_interval_width(value_type interval_width) noexcept
        {
            interval_width_ = interval_width;
            ranges_[fixed_axis_] = interval<value_type>{current_index_, current_index_ + interval_width_};
            return *this;
        }

    private:
        size_type fixed_axis_;
        value_type interval_width_;
        value_type current_index_;
        value_type last_index_;
        storage_type ranges_;
    };
}

using details::arrnd_indexer_position;
using details::arrnd_general_indexer;
//using details::arrnd_fast_indexer;
using details::arrnd_fixed_axis_ranger;
}

namespace oc {
namespace details {
    struct arrnd_tag { };
    template <typename T>
    concept arrnd_compliant = std::is_same_v<typename std::remove_cvref_t<T>::tag, arrnd_tag>;

    template <typename T, typename... Args>
    concept invocable_no_arrnd = !
    arrnd_compliant<T>&& std::is_invocable_v<T, Args...>;

    template <arrnd_compliant Arrnd>
    class arrnd_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;

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

    template <arrnd_compliant Arrnd>
    class arrnd_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;

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

    template <arrnd_compliant Arrnd>
    class arrnd_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;

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

    template <arrnd_compliant Arrnd>
    class arrnd_const_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = typename Arrnd::value_type;
        using pointer = typename Arrnd::value_type*;
        using reference = typename Arrnd::value_type&;

        using indexer_type = typename Arrnd::indexer_type;

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

    template <arrnd_compliant Arrnd>
    class arrnd_axis_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using reference = Arrnd&;

        using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

        explicit constexpr arrnd_axis_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
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
            arrnd_axis_iterator temp{*this};
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
            arrnd_axis_iterator temp{*this};
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
            arrnd_axis_iterator temp{*this};
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
            arrnd_axis_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_axis_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_axis_iterator& iter) const noexcept
        {
            return far_ < iter.far_;
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <arrnd_compliant Arrnd>
    class arrnd_axis_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using const_reference = const Arrnd&;

        using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

        explicit constexpr arrnd_axis_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
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
            arrnd_axis_const_iterator temp{*this};
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
            arrnd_axis_const_iterator temp{*this};
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
            arrnd_axis_const_iterator temp{*this};
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
            arrnd_axis_const_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr const_reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_axis_const_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_axis_const_iterator& iter) const noexcept
        {
            return far_ < iter.far_;
        }

        [[nodiscard]] constexpr const_reference operator[](difference_type index) noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_const_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <arrnd_compliant Arrnd>
    class arrnd_axis_reverse_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using reference = Arrnd&;

        using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

        explicit constexpr arrnd_axis_reverse_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
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
            arrnd_axis_reverse_iterator temp{*this};
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
            arrnd_axis_reverse_iterator temp{*this};
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
            arrnd_axis_reverse_iterator temp{*this};
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
            arrnd_axis_reverse_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_axis_reverse_iterator& iter) const noexcept
        {
            return far_ > iter.far_;
        }

        [[nodiscard]] constexpr reference operator[](difference_type index) noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_reverse_iterator& other) const noexcept
        {
            return (*far_)[far_.fixed_axis()].start() - (*other.far_)[far_.fixed_axis()].start();
        }

    private:
        value_type arrnd_ref_;
        ranger_type far_;

        mutable value_type slice_;
    };

    template <arrnd_compliant Arrnd>
    class arrnd_axis_reverse_const_iterator final {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = typename Arrnd::size_type;
        using value_type = Arrnd;
        using const_reference = const Arrnd&;

        using ranger_type = arrnd_fixed_axis_ranger<typename Arrnd::header_type>;

        explicit constexpr arrnd_axis_reverse_const_iterator(const value_type& arrnd_ref, const ranger_type& far)
            : arrnd_ref_(arrnd_ref)
            , far_(far)
        {
            if (far) {
                slice_ = arrnd_ref[std::make_pair((*far_).cbegin(), (*far_).cend())];
            }
        }

        constexpr arrnd_axis_reverse_const_iterator() = default;

        constexpr arrnd_axis_reverse_const_iterator(const arrnd_axis_reverse_const_iterator& other) = default;
        constexpr arrnd_axis_reverse_const_iterator& operator=(const arrnd_axis_reverse_const_iterator& other)
            = default;

        constexpr arrnd_axis_reverse_const_iterator(arrnd_axis_reverse_const_iterator&& other) noexcept = default;
        constexpr arrnd_axis_reverse_const_iterator& operator=(arrnd_axis_reverse_const_iterator&& other) noexcept
            = default;

        constexpr ~arrnd_axis_reverse_const_iterator() = default;

        constexpr arrnd_axis_reverse_const_iterator& operator--() noexcept
        {
            ++far_;
            return *this;
        }

        constexpr arrnd_axis_reverse_const_iterator operator--(int) noexcept
        {
            arrnd_axis_reverse_const_iterator temp{*this};
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
            arrnd_axis_reverse_const_iterator temp{*this};
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
            arrnd_axis_reverse_const_iterator temp{*this};
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
            arrnd_axis_reverse_const_iterator temp{*this};
            temp -= count;
            return temp;
        }

        [[nodiscard]] constexpr const_reference operator*() const noexcept
        {
            slice_ = arrnd_ref_[std::make_pair((*far_).cbegin(), (*far_).cend())];
            return slice_;
        }

        [[nodiscard]] constexpr bool operator==(const arrnd_axis_reverse_const_iterator& iter) const noexcept
        {
            return far_ == iter.far_;
        }

        [[nodiscard]] constexpr bool operator<(const arrnd_axis_reverse_const_iterator& iter) const noexcept
        {
            return far_ > iter.far_;
        }

        [[nodiscard]] constexpr const_reference operator[](difference_type index) noexcept
        {
            auto ranges = far_[index];
            return arrnd_ref_[std::make_pair(ranges.cbegin(), ranges.cend())];
        }

        [[nodiscard]] constexpr difference_type operator-(const arrnd_axis_reverse_const_iterator& other) const noexcept
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
            *cont_ = cont_->insert(cont, ind_);
            ind_ += cont.header().numel();
            return *this;
        }

        arrnd_insert_iterator& operator=(Arrnd&& cont)
        {
            *cont_ = cont_->insert(std::move(cont), ind_);
            ind_ += cont.header().numel();
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
    class arrnd_axis_back_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_axis_back_insert_iterator() noexcept = default;

        explicit arrnd_axis_back_insert_iterator(Arrnd& cont, size_type axis = 0) noexcept
            : cont_(std::addressof(cont))
            , axis_(axis)
        { }

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
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_axis_back_insert_iterator<Arrnd> arrnd_axis_back_inserter(
        Arrnd& cont, typename Arrnd::size_type axis = 0) noexcept
    {
        return arrnd_axis_back_insert_iterator<Arrnd>(cont, axis);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_axis_front_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_axis_front_insert_iterator() noexcept = default;

        explicit arrnd_axis_front_insert_iterator(Arrnd& cont, size_type axis = 0)
            : cont_(std::addressof(cont))
            , axis_(axis)
        { }

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
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_axis_front_insert_iterator<Arrnd> arrnd_axis_front_inserter(
        Arrnd& cont, typename Arrnd::size_type axis = 0)
    {
        return arrnd_axis_front_insert_iterator<Arrnd>(cont, axis);
    }

    template <arrnd_compliant Arrnd>
    class arrnd_axis_insert_iterator {
    public:
        using iterator_category = std::output_iterator_tag;
        using size_type = typename Arrnd::size_type;

        constexpr arrnd_axis_insert_iterator() noexcept = default;

        explicit arrnd_axis_insert_iterator(Arrnd& cont, size_type ind = 0, size_type axis = 0)
            : cont_(std::addressof(cont))
            , ind_(ind)
            , axis_(axis)
        { }

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
        size_type ind_;
        size_type axis_;
    };

    template <arrnd_compliant Arrnd>
    [[nodiscard]] inline constexpr arrnd_axis_insert_iterator<Arrnd> arrnd_axis_inserter(
        Arrnd& cont, typename Arrnd::size_type ind = 0, typename Arrnd::size_type axis = 0)
    {
        return arrnd_axis_insert_iterator<Arrnd>(cont, ind, axis);
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
    concept flat_arrnd_compliant = arrnd_compliant<T> && (T::depth == 0);

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
        = arrnd_compliant<ArrndSrc> && arrnd_compliant<ArrndDst> && (ArrndSrc::depth == ArrndDst::depth);

    template <typename T, std::int64_t Depth>
        requires(Depth >= 0 && Depth <= T::depth)
    struct arrnd_inner_impl {
        using type = arrnd_inner_impl<typename T::value_type, Depth - 1>::type;
    };
    template <typename T>
    struct arrnd_inner_impl<T, 0> {
        using type = T;
    };
    template <arrnd_compliant ArCo, std::int64_t Level = ArCo::depth>
    using arrnd_inner = arrnd_inner_impl<ArCo, Level>;
    template <arrnd_compliant ArCo, std::int64_t Level = ArCo::depth>
    using arrnd_inner_t = arrnd_inner<ArCo, Level>::type;

    template <typename T>
    struct typed {
        using type = T;
    };

    template <typename T, typename R, std::int64_t Level>
    struct last_replaced_inner_types_tuple_impl {
        using type
            = std::tuple<T, typename last_replaced_inner_types_tuple_impl<typename T::value_type, R, Level - 1>::type>;
    };
    template <typename T, typename R>
    struct last_replaced_inner_types_tuple_impl<T, R, 0> {
        using type = typename T::template replaced_type<R>;
    };
    template <typename T, typename R, std::int64_t Level>
    using last_replaced_inner_types_tuple = std::conditional_t<Level == 0,
        typed<std::tuple<typename T::template replaced_type<R>>>, last_replaced_inner_types_tuple_impl<T, R, Level>>;
    template <typename T, typename R, std::int64_t Level>
    using last_replaced_inner_types_tuple_t = typename last_replaced_inner_types_tuple<T, R, Level>::type;

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
    struct replaced_inner_type {
        using type = folded_replaced_type_tuple_t<flat_tuple_t<last_replaced_inner_types_tuple_t<T, R, Level>>>;
    };
    template <typename T, typename R, std::int64_t Level>
    using replaced_inner_type_t = replaced_inner_type<T, R, Level>::type;

    enum class arrnd_shape { vector, row, column };

    template <typename T, random_access_type Storage = simple_dynamic_vector<T>,
        template <typename> typename SharedRefAllocator = lightweight_allocator,
        arrnd_header_compliant Header = arrnd_header<>, template <typename> typename Indexer = arrnd_general_indexer>
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
        using replaced_type
            = arrnd<U, typename Storage::template replaced_type<U>, SharedRefAllocator, Header, Indexer>;
        template <typename U, std::int64_t Level>
        using inner_replaced_type = replaced_inner_type_t<this_type, U, Level>;

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

        constexpr static std::int64_t depth = calc_arrnd_depth<T>();
        constexpr static bool is_flat = depth == 0;

        template <typename U, std::int64_t Level = this_type::depth>
        using tol_type = decltype(typename arrnd_inner_t<this_type, Level>::value_type{} - U{});
        template <arrnd_compliant ArCo, std::int64_t Level = this_type::depth>
        using compliant_tol_type = decltype(typename arrnd_inner_t<this_type, Level>::value_type{} -
            typename arrnd_inner_t<ArCo, Level>::value_type{});

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

        template <integral_type_iterator InputDimsIt, std::input_iterator InputDataIt>
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
        template <integral_type_iterable Cont, std::input_iterator InputDataIt>
        explicit constexpr arrnd(const Cont& dims, const InputDataIt& first_data, const InputDataIt& last_data)
            : arrnd(std::begin(dims), std::end(dims), first_data, last_data)
        { }
        template <std::integral D, std::input_iterator InputDataIt>
        explicit constexpr arrnd(
            std::initializer_list<D> dims, const InputDataIt& first_data, const InputDataIt& last_data)
            : arrnd(dims.begin(), dims.end(), first_data, last_data)
        { }
        template <std::integral D, std::int64_t M, std::input_iterator InputDataIt>
        explicit constexpr arrnd(const D (&dims)[M], const InputDataIt& first_data, const InputDataIt& last_data)
            : arrnd(std::begin(dims), std::end(dims), first_data, last_data)
        { }

        template <integral_type_iterator InputDimsIt>
        explicit constexpr arrnd(
            const InputDimsIt& first_dim, const InputDimsIt& last_dim, std::initializer_list<value_type> data)
            : arrnd(first_dim, last_dim, data.begin(), data.end())
        { }
        template <integral_type_iterable Cont>
        explicit constexpr arrnd(const Cont& dims, std::initializer_list<value_type> data)
            : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        { }
        template <std::integral D>
        explicit constexpr arrnd(std::initializer_list<D> dims, std::initializer_list<value_type> data)
            : arrnd(dims.begin(), dims.end(), data.begin(), data.end())
        { }
        template <std::integral D, std::int64_t M>
        explicit constexpr arrnd(const D (&dims)[M], std::initializer_list<value_type> data)
            : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        { }

        template <integral_type_iterator InputDimsIt, typename U>
        explicit constexpr arrnd(
            const InputDimsIt& first_dim, const InputDimsIt& last_dim, std::initializer_list<U> data)
            : arrnd(first_dim, last_dim, data.begin(), data.end())
        { }
        template <integral_type_iterable Cont, typename U>
        explicit constexpr arrnd(const Cont& dims, std::initializer_list<U> data)
            : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        { }
        template <std::integral D, typename U>
        explicit constexpr arrnd(std::initializer_list<D> dims, std::initializer_list<U> data)
            : arrnd(dims.begin(), dims.end(), data.begin(), data.end())
        { }
        template <std::integral D, std::int64_t M, typename U>
        explicit constexpr arrnd(const D (&dims)[M], std::initializer_list<U> data)
            : arrnd(std::begin(dims), std::end(dims), data.begin(), data.end())
        { }

        template <integral_type_iterator InputDimsIt, std::int64_t N>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const value_type (&data)[N])
            : arrnd(first_dim, last_dim, std::begin(data), std::end(data))
        { }
        template <integral_type_iterable Cont, std::int64_t N>
        explicit constexpr arrnd(const Cont& dims, const value_type (&data)[N])
            : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        { }
        template <std::integral D, std::int64_t N>
        explicit constexpr arrnd(std::initializer_list<D> dims, const value_type (&data)[N])
            : arrnd(dims.begin(), dims.end(), std::begin(data), std::end(data))
        { }
        template <std::integral D, std::int64_t M, std::int64_t N>
        explicit constexpr arrnd(const D (&dims)[M], const value_type (&data)[N])
            : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        { }

        template <integral_type_iterator InputDimsIt, typename U, std::int64_t N>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim, const U (&data)[N])
            : arrnd(first_dim, last_dim, std::begin(data), std::end(data))
        { }
        template <integral_type_iterable Cont, typename U, std::int64_t N>
        explicit constexpr arrnd(const Cont& dims, const U (&data)[N])
            : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        { }
        template <std::integral D, typename U, std::int64_t N>
        explicit constexpr arrnd(std::initializer_list<D> dims, const U (&data)[N])
            : arrnd(dims.begin(), dims.end(), std::begin(data), std::end(data))
        { }
        template <std::integral D, std::int64_t M, typename U, std::int64_t N>
        explicit constexpr arrnd(const D (&dims)[M], const U (&data)[N])
            : arrnd(std::begin(dims), std::end(dims), std::begin(data), std::end(data))
        { }

        template <integral_type_iterator InputDimsIt>
        explicit constexpr arrnd(const InputDimsIt& first_dim, const InputDimsIt& last_dim)
            : hdr_(first_dim, last_dim)
            , buffsp_(hdr_.empty()
                      ? nullptr
                      : std::allocate_shared<storage_type>(shared_ref_allocator_type<storage_type>(), hdr_.numel()))
        { }
        template <integral_type_iterable Cont>
        explicit constexpr arrnd(const Cont& dims)
            : arrnd(std::begin(dims), std::end(dims))
        { }
        template <std::integral D>
        explicit constexpr arrnd(std::initializer_list<D> dims)
            : arrnd(dims.begin(), dims.end())
        { }
        template <std::integral D, std::int64_t M>
        explicit constexpr arrnd(const D (&dims)[M])
            : arrnd(std::begin(dims), std::end(dims))
        { }

        template <integral_type_iterator InputDimsIt>
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
        template <integral_type_iterable Cont>
        explicit constexpr arrnd(const Cont& dims, const_reference value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }
        template <std::integral D>
        explicit constexpr arrnd(std::initializer_list<D> dims, const_reference value)
            : arrnd(dims.begin(), dims.end(), value)
        { }
        template <std::integral D, std::int64_t M>
        explicit constexpr arrnd(const D (&dims)[M], const_reference value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }

        template <integral_type_iterator InputDimsIt, typename U>
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
        template <integral_type_iterable Cont, typename U>
        explicit constexpr arrnd(const Cont& dims, const U& value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }
        template <std::integral D, typename U>
        explicit constexpr arrnd(std::initializer_list<D> dims, const U& value)
            : arrnd(dims.begin(), dims.end(), value)
        { }
        template <std::integral D, std::int64_t M, typename U>
        explicit constexpr arrnd(const D (&dims)[M], const U& value)
            : arrnd(std::begin(dims), std::end(dims), value)
        { }

        template <integral_type_iterator InputDimsIt, typename Func, typename... Args>
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
        template <integral_type_iterable Cont, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(const Cont& dims, Func&& func, Args&&... args)
            : arrnd(std::begin(dims), std::end(dims), std::forward<Func>(func), std::forward<Args>(args)...)
        { }
        template <std::integral D, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(std::initializer_list<D> dims, Func&& func, Args&&... args)
            : arrnd(dims.begin(), dims.end(), std::forward<Func>(func), std::forward<Args>(args)...)
        { }
        template <std::integral D, std::int64_t M, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, Args...>)
        explicit constexpr arrnd(const D (&dims)[M], Func&& func, Args&&... args)
            : arrnd(std::begin(dims), std::end(dims), std::forward<Func>(func), std::forward<Args>(args)...)
        { }

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

        template <std::integral U>
        [[nodiscard]] constexpr const_reference operator[](U index) const noexcept
        {
            assert(index >= hdr_.offset() && index <= hdr_.last_index());
            return buffsp_->data()[index];
        }
        template <std::integral U>
        [[nodiscard]] constexpr reference operator[](U index) noexcept
        {
            assert(index >= hdr_.offset() && index <= hdr_.last_index());
            return buffsp_->data()[index];
        }

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr const_reference operator[](std::pair<InputIt, InputIt> subs) const noexcept
        {
            return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr const_reference operator[](const Cont& subs) const noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr const_reference operator[](std::initializer_list<U> subs) const noexcept
        {
            return (*this)[std::make_pair(subs.begin(), subs.end())];
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr const_reference operator[](const U (&subs)[M]) const noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr reference operator[](std::pair<InputIt, InputIt> subs) noexcept
        {
            return buffsp_->data()[hdr_.subs2ind(subs.first, subs.second)];
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr reference operator[](const Cont& subs) noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr reference operator[](std::initializer_list<U> subs) noexcept
        {
            return (*this)[std::make_pair(subs.begin(), subs.end())];
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr reference operator[](const U (&subs)[M]) noexcept
        {
            return (*this)[std::make_pair(std::begin(subs), std::end(subs))];
        }

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
        template <std::integral U = size_type>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](std::initializer_list<interval<U>> ranges) const&
        {
            return (*this)[std::make_pair(ranges.begin(), ranges.end())];
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](std::initializer_list<interval<U>> ranges) const&&
        {
            return std::move(*this)[std::make_pair(ranges.begin(), ranges.end())];
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](const interval<U> (&ranges)[M]) const&
        {
            return (*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](const interval<U> (&ranges)[M]) const&&
        {
            return std::move(*this)[std::make_pair(std::begin(ranges), std::end(ranges))];
        }

        template <std::integral U>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](interval<U> range) const&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range);
            slice.buffsp_ = buffsp_;
            slice.is_creator_valid_ = original_valid_creator_;
            slice.creator_ = this;
            return slice;
        }
        template <std::integral U>
        [[nodiscard]] constexpr shared_ref<this_type> operator[](interval<U> range) const&&
        {
            this_type slice{};
            slice.hdr_ = hdr_.subheader(range);
            slice.buffsp_ = buffsp_;
            return slice;
        }

        // access relative array indices, might be slow for slices
        template <std::integral U>
        [[nodiscard]] constexpr const_reference operator()(U relative_index) const noexcept
        {
            assert(relative_index >= 0 && relative_index <= hdr_.numel());
            return hdr_.is_slice() ? buffsp_->data()[*(indexer_type(hdr_) + relative_index)]
                                   : buffsp_->data()[relative_index];
        }
        template <std::integral U>
        [[nodiscard]] constexpr reference operator()(U relative_index) noexcept
        {
            assert(relative_index >= 0 && relative_index <= hdr_.numel());
            return hdr_.is_slice() ? buffsp_->data()[*(indexer_type(hdr_) + relative_index)]
                                   : buffsp_->data()[relative_index];
        }

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto operator()(std::pair<InputIt, InputIt> indices) const
        {
            auto num_indices = std::distance(indices.first, indices.second);

            this_type res({num_indices});

            indexer_type res_gen(res.hdr_);
            auto inds_it = indices.first;

            for (; res_gen && inds_it != indices.second; ++res_gen, ++inds_it) {
                res[*res_gen] = (*this)[*inds_it];
            }

            return res;
        }
        template <integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        [[nodiscard]] constexpr auto operator()(const Cont& indices) const
        {
            return (*this)(std::make_pair(std::begin(indices), std::end(indices)));
        }
        /**
        * @note more strict function than filter. in case of logical type arrnd, its being treated as mask
        */
        template <arrnd_compliant ArCo>
            requires(std::integral<typename ArCo::value_type>)
        [[nodiscard]] constexpr auto operator()(const ArCo& selector) const
        {
            // in case that indices isn't a vector treat it as a mask
            if constexpr (std::is_same_v<bool, typename ArCo::value_type>) {
                return filter<0>(selector);
            } else {
                return (*this)(std::make_pair(std::begin(selector), std::end(selector)));
            }
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto operator()(std::initializer_list<U> indices) const
        {
            return (*this)(std::make_pair(indices.begin(), indices.end()));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto operator()(const U (&indices)[M]) const
        {
            return (*this)(std::make_pair(std::begin(indices), std::end(indices)));
        }

        [[nodiscard]] constexpr auto operator()(arrnd_shape shape) const
        {
            return reshape<0>(shape);
        }

        template <typename Func, typename... Args>
            requires invocable_no_arrnd<Func, value_type, Args...>
        [[nodiscard]] constexpr auto operator()(Func&& func, Args&&... args) const
        {
            return filter<0>(std::forward<Func>(func), std::forward<Args>(args)...);
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

        template <arrnd_compliant ArCo, integral_type_iterator InputIt>
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
        template <arrnd_compliant ArCo, integral_type_iterable Cont>
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
        template <arrnd_compliant ArCo, std::integral U = size_type>
        constexpr const this_type& copy_to(ArCo&& dst, std::initializer_list<U> indices) const
        {
            return copy_to(std::forward<ArCo>(dst), std::make_pair(indices.begin(), indices.end()));
        }
        template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        constexpr const this_type& copy_to(ArCo&& dst, const U (&indices)[M]) const
        {
            return copy_to(std::forward<ArCo>(dst), std::make_pair(std::begin(indices), std::end(indices)));
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
        template <arrnd_compliant ArCo, std::integral U = size_type>
        constexpr const this_type& copy_to(ArCo&& dst, std::initializer_list<interval<U>> ranges) const
        {
            return copy_to(std::forward<ArCo>(dst), ranges.begin(), ranges.end());
        }
        template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        constexpr const this_type& copy_to(ArCo&& dst, const interval<U> (&ranges)[M]) const
        {
            return copy_to(std::forward<ArCo>(dst), std::begin(ranges), std::end(ranges));
        }

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

        template <arrnd_compliant ArCo, integral_type_iterator InputIt>
        constexpr this_type& copy_from(const ArCo& src, std::pair<InputIt, InputIt> indices)
        {
            src.copy_to(*this, indices);
            return *this;
        }
        template <arrnd_compliant ArCo, integral_type_iterable Cont>
            requires(!arrnd_compliant<Cont>)
        constexpr this_type& copy_from(const ArCo& src, const Cont& indices)
        {
            return copy_from(src, std::make_pair(indices.begin(), indices.end()));
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
        template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        constexpr this_type& copy_from(const ArCo& src, const U (&indices)[M])
        {
            return copy_from(src, std::make_pair(std::begin(indices), std::end(indices)));
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
        template <arrnd_compliant ArCo, std::integral U = size_type>
        constexpr this_type& copy_from(const ArCo& src, std::initializer_list<interval<U>> ranges)
        {
            return copy_from(src, ranges.begin(), ranges.end());
        }
        template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
        constexpr this_type& copy_from(const ArCo& src, const interval<U> (&ranges)[M])
        {
            return copy_from(src, std::begin(ranges), std::end(ranges));
        }

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

        template <std::int64_t Level, integral_type_iterator InputIt>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            typename this_type::header_type new_header(first_new_dim, last_new_dim);
            assert(hdr_.numel() == new_header.numel());

            if (hdr_.dims() == new_header.dims()) {
                return *this;
            }

            if (hdr_.is_slice()) {
                return resize<Level>(first_new_dim, last_new_dim);
            }

            this_type res(*this);
            res.hdr_ = std::move(new_header);

            return res;
        }
        template <std::int64_t Level, integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reshape<Level - 1, InputIt>(first_new_dim, last_new_dim);
            }

            return res;
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            return reshape<this_type::depth>(first_new_dim, last_new_dim);
        }
        template <std::int64_t Level, integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const Cont& new_dims) const
        {
            return reshape<Level>(std::begin(new_dims), std::end(new_dims));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const Cont& new_dims) const
        {
            return reshape<this_type::depth, Cont>(std::begin(new_dims), std::end(new_dims));
        }
        template <std::int64_t Level, std::integral U = size_type>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<U> new_dims) const
        {
            return reshape<Level>(new_dims.begin(), new_dims.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(std::initializer_list<U> new_dims) const
        {
            return reshape<this_type::depth>(new_dims.begin(), new_dims.end());
        }
        template <std::int64_t Level, std::integral U, std::int64_t M>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const U (&new_dims)[M]) const
        {
            return reshape<Level>(std::begin(new_dims), std::end(new_dims));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(const U (&new_dims)[M]) const
        {
            return reshape<this_type::depth>(std::begin(new_dims), std::end(new_dims));
        }
        template <std::int64_t Level>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape shape) const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reshape<Level - 1>(shape);
            }

            return res;
        }
        template <std::int64_t Level>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape shape) const
        {
            if (empty()) {
                return *this;
            }

            switch (shape) {
            case arrnd_shape::vector:
                return reshape<Level>({hdr_.numel()});
            case arrnd_shape::row:
                return reshape<Level>({size_type{1}, hdr_.numel()});
            case arrnd_shape::column:
                return reshape<Level>({hdr_.numel(), size_type{1}});
            default:
                assert(false && "unknown arrnd_shape value");
                return this_type();
            }
        }
        [[nodiscard]] constexpr maybe_shared_ref<this_type> reshape(arrnd_shape shape) const
        {
            return reshape<this_type::depth>(shape);
        }

        template <std::int64_t Level, integral_type_iterator InputIt>
            requires(Level == 0)
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

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            while (gen && res_gen) {
                res[*res_gen] = (*this)[*gen];
                ++gen;
                ++res_gen;
            }

            return res;
        }
        template <std::int64_t Level, integral_type_iterator InputIt>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template resize<Level - 1, InputIt>(first_new_dim, last_new_dim);
            }

            return res;
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(
            const InputIt& first_new_dim, const InputIt& last_new_dim) const
        {
            return resize<this_type::depth>(first_new_dim, last_new_dim);
        }
        template <std::int64_t Level, integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const Cont& new_dims) const
        {
            return resize<Level>(std::begin(new_dims), std::end(new_dims));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const Cont& new_dims) const
        {
            return resize<this_type::depth, Cont>(std::begin(new_dims), std::end(new_dims));
        }
        template <std::int64_t Level, std::integral U = size_type>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<U> new_dims) const
        {
            return resize<Level>(new_dims.begin(), new_dims.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(std::initializer_list<U> new_dims) const
        {
            return resize<this_type::depth>(new_dims.begin(), new_dims.end());
        }
        template <std::int64_t Level, std::integral U, std::int64_t M>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const U (&new_dims)[M]) const
        {
            return resize<Level>(std::begin(new_dims), std::end(new_dims));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> resize(const U (&new_dims)[M]) const
        {
            return resize<this_type::depth>(std::begin(new_dims), std::end(new_dims));
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr) const
        {
            return insert<Level>(arr, hdr_.numel());
        }
        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr) const
        {
            return append<this_type::depth, ArCo>(arr);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, U axis) const
        {
            size_type ind = empty() ? size_type{0} : hdr_.dims()[axis];
            return insert<Level>(arr, ind, axis);
        }
        template <arrnd_compliant ArCo, std::integral U>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> append(const ArCo& arr, U axis) const
        {
            return append<this_type::depth, ArCo>(arr, axis);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind) const
        {
            if (empty()) {
                return arr.template reshape<Level>({arr.header().numel()}).clone();
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
        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind) const
        {
            this_type res(empty() ? arr.header().dims() : hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && res_gen && arr_gen; ++gen, ++res_gen, ++arr_gen) {
                res[*res_gen] = (*this)[*gen].template insert<Level - 1, typename ArCo::value_type>(arr[*arr_gen], ind);
            }

            return res;
        }
        template <arrnd_compliant ArCo, std::integral U>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind) const
        {
            return insert<this_type::depth, ArCo>(arr, ind);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind, V axis) const
        {
            if (empty()) {
                this_type res(arr);
                return res.clone();
            }

            if (arr.empty()) {
                return *this;
            }

            header_type new_header(hdr_.subheader(arr.header().dims()[axis], axis));
            if (new_header.empty()) {
                return this_type();
            }

            assert(ind >= 0 && ind <= hdr_.dims()[axis]);

            this_type res({hdr_.numel() + arr.header().numel()});
            res.hdr_ = std::move(new_header);

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
        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind, V axis) const
        {
            this_type res(empty() ? arr.header().dims() : hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);
            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && res_gen && arr_gen; ++gen, ++res_gen, ++arr_gen) {
                res[*res_gen]
                    = (*this)[*gen].template insert<Level - 1, typename ArCo::value_type>(arr[*arr_gen], ind, axis);
            }

            return res;
        }
        template <arrnd_compliant ArCo, std::integral U, std::integral V>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> insert(const ArCo& arr, U ind, V axis) const
        {
            return insert<this_type::depth, ArCo>(arr, ind, axis);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> cat(std::pair<ArCo, U> arr_ind_pair) const
        {
            return insert<Level>(arr_ind_pair.first, arr_ind_pair.second);
        }
        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, typename... ArCoIndexPairs>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> cat(
            std::pair<ArCo, U> arr_ind_pair, ArCoIndexPairs&&... others) const
        {
            return insert<Level>(arr_ind_pair.first, arr_ind_pair.second)
                .template cat<ArCoIndexPairs...>(std::forward<ArCoIndexPairs>(others)...);
        }
        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> cat(std::tuple<ArCo, U, V> arr_ind_axis_tuple) const
        {
            return insert<Level>(
                std::get<0>(arr_ind_axis_tuple), std::get<1>(arr_ind_axis_tuple), std::get<2>(arr_ind_axis_tuple));
        }
        template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V,
            typename... ArCoIndexAxisTuples>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> cat(
            std::tuple<ArCo, U, V> arr_ind_axis_tuple, ArCoIndexAxisTuples&&... others) const
        {
            return insert<Level>(
                std::get<0>(arr_ind_axis_tuple), std::get<1>(arr_ind_axis_tuple), std::get<2>(arr_ind_axis_tuple))
                .template cat<ArCoIndexAxisTuples...>(std::forward<ArCoIndexAxisTuples>(others)...);
        }
        template <typename... PairsOrTuples>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> cat(PairsOrTuples&&... pairs_or_tuples) const
        {
            return cat<this_type::depth>(std::forward<PairsOrTuples>(pairs_or_tuples)...);
        }

        template <std::int64_t Level, std::integral U, std::integral V>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count) const
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
        template <std::int64_t Level, std::integral U, std::integral V>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count) const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template remove<Level - 1>(ind, count);
            }

            return res;
        }
        template <std::integral U, std::integral V>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count) const
        {
            return remove<this_type::depth>(ind, count);
        }

        template <std::int64_t Level, std::integral U, std::integral V, std::integral W>
            requires(Level == 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count, W axis) const
        {
            if (empty()) {
                return *this;
            }

            header_type new_header(hdr_.subheader(-count, axis));

            assert(ind >= 0 && ind < hdr_.dims()[axis]);
            assert(ind + count <= hdr_.dims()[axis]);

            if (new_header.empty()) {
                return this_type();
            }

            this_type res({hdr_.numel() - (hdr_.numel() / hdr_.dims()[axis]) * count});
            res.hdr_ = std::move(new_header);

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
        template <std::int64_t Level, std::integral U, std::integral V, std::integral W>
            requires(Level > 0)
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count, W axis) const
        {
            if (empty()) {
                return *this;
            }

            this_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.hdr_);

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template remove<Level - 1>(ind, count, axis);
            }

            return res;
        }
        template <std::integral U, std::integral V, std::integral W>
        [[nodiscard]] constexpr maybe_shared_ref<this_type> remove(U ind, V count, W axis) const
        {
            return remove<this_type::depth>(ind, count, axis);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(
                Level > 0 && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>,
            Level> transform(Func&& func, Args&&... args) const
        {
            using transformed_type = inner_replaced_type<
                std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>, Level>;

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
            requires(
                Level == 0 && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>,
            Level> transform(Func&& func, Args&&... args) const
        {
            using transformed_type = inner_replaced_type<
                std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>, Level>;

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
            requires invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>,
            this_type::depth>
        transform(Func&& func, Args&&... args) const
        {
            return transform<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename ArCo::value_type, Args...>)
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, typename ArCo::value_type,
                Args...>,
            Level> transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            using transformed_type
                = inner_replaced_type<std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                                          typename ArCo::value_type, Args...>,
                    Level>;

            if (empty()) {
                return transformed_type();
            }

            if (hdr_.dims() != arr.header().dims()) {
                return transformed_type();
            }

            transformed_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            typename transformed_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template transform<Level - 1, ArCo, Func, Args...>(
                    arr, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename ArCo::value_type, Args...>)
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type, typename ArCo::value_type,
                Args...>,
            Level> transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            using transformed_type
                = inner_replaced_type<std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                                          typename ArCo::value_type, Args...>,
                    Level>;

            if (empty()) {
                return transformed_type();
            }

            if (hdr_.dims() != arr.header().dims()) {
                return transformed_type();
            }

            transformed_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            typename transformed_type::indexer_type res_gen(res.header());

            typename ArCo::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen && res_gen; ++gen, ++arr_gen, ++res_gen) {
                res[*res_gen] = func((*this)[*gen], arr[*arr_gen], std::forward<Args>(args)...);
            }

            return res;
        }

        template <typename Func, arrnd_compliant ArCo, typename... Args>
            requires invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename ArCo::value_type, Args...>
        [[nodiscard]] constexpr inner_replaced_type<
            std::invoke_result_t<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename ArCo::value_type, Args...>,
            this_type::depth>
        transform(const ArCo& arr, Func&& func, Args&&... args) const
        {
            return transform<this_type::depth, ArCo, Func, Args...>(
                arr, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(
                Level > 0 && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires(
                Level == 0 && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
        constexpr this_type& apply(Func&& func, Args&&... args)
        {
            return apply<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            if (empty()) {
                return *this;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return *this;
            }

            indexer_type gen(hdr_);
            typename std::remove_cvref_t<ArCo>::indexer_type arr_gen(arr.header());

            for (; gen && arr_gen; ++gen, ++arr_gen) {
                (*this)[*gen] = func((*this)[*gen], arr[*arr_gen], std::forward<Args>(args)...);
            }

            return *this;
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            if (empty()) {
                return *this;
            }

            if (hdr_.dims() != arr.header().dims()) {
                return *this;
            }

            for (indexer_type gen(hdr_); gen; ++gen) {
                (*this)[*gen].template apply<Level - 1, ArCo, Func, Args...>(
                    arr, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return *this;
        }

        template <arrnd_compliant ArCo, typename Func, typename... Args>
            requires invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
        constexpr this_type& apply(const ArCo& arr, Func&& func, Args&&... args)
        {
            return apply<this_type::depth, ArCo, Func, Args...>(
                arr, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            using reduced_type = std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                typename arrnd_inner_t<this_type, Level>::value_type, Args...>;

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
            requires(Level > 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            using reduced_type
                = inner_replaced_type<std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                                          typename arrnd_inner_t<this_type, Level>::value_type, Args...>,
                    Level - 1>;

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
            requires(invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(Func&& func, Args&&... args) const
        {
            return reduce<this_type::depth, Func, Args...>(std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename U, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, U, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            using folded_type
                = std::invoke_result_t<Func, U, typename arrnd_inner_t<this_type, Level>::value_type, Args...>;

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
            requires(
                Level > 0 && invocable_no_arrnd<Func, U, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            using folded_type = inner_replaced_type<
                std::invoke_result_t<Func, U, typename arrnd_inner_t<this_type, Level>::value_type, Args...>,
                Level - 1>;

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
            requires(
                invocable_no_arrnd<Func, U, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(const U& init, Func&& func, Args&&... args) const
        {
            return fold<this_type::depth, U, Func, Args...>(
                init, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, std::integral U, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(U axis, Func&& func, Args&&... args) const
        {
            using reduced_type
                = replaced_type<std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>>;

            if (empty()) {
                return reduced_type();
            }

            typename reduced_type::header_type new_header(hdr_.subheader(axis));
            if (new_header.empty()) {
                return reduced_type();
            }

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
        template <std::int64_t Level, std::integral U, typename Func, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(U axis, Func&& func, Args&&... args) const
        {
            using reduced_type = inner_replaced_type<
                replaced_type<std::invoke_result_t<Func, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>>,
                Level - 1>;

            if (empty()) {
                return reduced_type();
            }

            reduced_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template reduce<Level - 1, U, Func, Args...>(
                    axis, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <std::integral U, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>)
        [[nodiscard]] constexpr auto reduce(U axis, Func&& func, Args&&... args) const
        {
            return reduce<this_type::depth, U, Func, Args...>(
                axis, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, std::integral U, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Func, typename ArCo::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(U axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            using folded_type = replaced_type<std::invoke_result_t<Func, typename ArCo::value_type,
                typename arrnd_inner_t<this_type, Level>::value_type, Args...>>;

            if (empty()) {
                return folded_type();
            }

            typename folded_type::header_type new_header(hdr_.subheader(axis));

            assert(inits.header().dims().size() == 1 && inits.header().dims()[0] == hdr_.numel() / hdr_.dims()[axis]);

            if (new_header.empty()) {
                return folded_type();
            }

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
        template <std::int64_t Level, std::integral U, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Func, typename ArCo::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(U axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            using folded_type = inner_replaced_type<replaced_type<std::invoke_result_t<Func, typename ArCo::value_type,
                                                        typename arrnd_inner_t<this_type, Level>::value_type, Args...>>,
                Level - 1>;

            if (empty()) {
                return folded_type();
            }

            folded_type res(hdr_.dims());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template fold<Level - 1, U, ArCo, Func, Args...>(
                    axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
            }

            return res;
        }
        template <std::integral U, arrnd_compliant ArCo, typename Func, typename... Args>
            requires(invocable_no_arrnd<Func, typename ArCo::value_type,
                typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>)
        [[nodiscard]] constexpr auto fold(U axis, const ArCo& inits, Func&& func, Args&&... args) const
        {
            return fold<this_type::depth, U, ArCo, Func, Args...>(
                axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
                return res.template resize<Level>({res_count});
            }

            return res;
        }
        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level > 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
        [[nodiscard]] constexpr this_type filter(Pred&& pred, Args&&... args) const
        {
            return filter<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level == 0)
        [[nodiscard]] constexpr this_type filter(const ArCo& mask) const
        {
            if (empty()) {
                return this_type();
            }

            assert(hdr_.dims() == mask.header().dims());

            this_type res({hdr_.numel()});

            indexer_type gen(hdr_);
            typename ArCo::indexer_type mask_gen(mask.header());

            indexer_type res_gen(res.hdr_);

            size_type res_count{0};

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

            if (res_count < hdr_.numel()) {
                return res.template resize<Level>({res_count});
            }

            return res;
        }
        template <std::int64_t Level, arrnd_compliant ArCo>
            requires(Level > 0)
        [[nodiscard]] constexpr this_type filter(const ArCo& mask) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template filter<Level - 1, ArCo>(mask);
            }

            return res;
        }
        template <arrnd_compliant ArCo>
        [[nodiscard]] constexpr this_type filter(const ArCo& mask) const
        {
            return filter<this_type::depth, ArCo>(mask);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
                return res.template resize<Level>({res_count});
            }

            return res;
        }
        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level > 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
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
                return res.template resize<Level>({res_count});
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

        template <std::int64_t Level, integral_type_iterator InputIt>
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
        template <std::int64_t Level, integral_type_iterator InputIt>
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
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr this_type transpose(const InputIt& first_order, const InputIt& last_order) const
        {
            return transpose<this_type::depth, InputIt>(first_order, last_order);
        }
        template <std::int64_t Level, integral_type_iterable Cont>
        [[nodiscard]] constexpr this_type transpose(const Cont& order) const
        {
            return transpose<Level>(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr this_type transpose(const Cont& order) const
        {
            return transpose<this_type::depth>(std::begin(order), std::end(order));
        }
        template <std::int64_t Level, std::integral U = size_type>
        [[nodiscard]] constexpr this_type transpose(std::initializer_list<U> order) const
        {
            return transpose<Level>(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr this_type transpose(std::initializer_list<U> order) const
        {
            return transpose<this_type::depth>(order.begin(), order.end());
        }
        template <std::int64_t Level, std::integral U, std::int64_t M>
        [[nodiscard]] constexpr this_type transpose(const U (&order)[M]) const
        {
            return transpose<Level>(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr this_type transpose(const U (&order)[M]) const
        {
            return transpose<this_type::depth>(std::begin(order), std::end(order));
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

        template <std::int64_t Level, std::integral U>
            requires(Level > 0)
        [[nodiscard]] constexpr auto expand(U axis, size_type division = 0) const
        {
            using expanded_type = inner_replaced_type<arrnd_inner_t<this_type, Level>, Level>;

            if (empty()) {
                return expanded_type();
            }

            expanded_type res(hdr_.dims());

            indexer_type gen(hdr_);
            typename expanded_type::indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template expand<Level - 1>(axis, division);
            }

            return res;
        }

        template <std::int64_t Level, std::integral U>
            requires(Level == 0)
        [[nodiscard]] constexpr auto expand(U axis, size_type division = 0) const
        {
            using expanded_type = inner_replaced_type<arrnd_inner_t<this_type, Level>, Level>;

            if (empty()) {
                return expanded_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());
            auto axis_dim = *std::next(hdr_.dims().cbegin(), axis);

            assert(division <= axis_dim);
            auto fixed_div = division > 0 ? std::min(axis_dim, division) : axis_dim;

            expanded_type res({fixed_div});
            typename expanded_type::indexer_type res_gen(res.header());

            //auto axis_dim_left = axis_dim;
            //auto interval_width
            //    = static_cast<size_type>(std::ceil(axis_dim / static_cast<double>(fixed_div)));
            //auto count = 0;

            auto curr_div = fixed_div;
            auto curr_axis_dim = axis_dim;
            auto curr_ival_width = static_cast<size_type>(std::ceil(curr_axis_dim / static_cast<double>(curr_div)));

            auto count = 0;

            ranger_type rgr(hdr_, axis, false, curr_ival_width);

            while (curr_div > 0) {
                res[*res_gen] = (*this)[std::make_pair((*rgr).cbegin(), (*rgr).cend())];

                rgr += curr_ival_width;
                ++res_gen;

                --curr_div;
                curr_axis_dim -= curr_ival_width;
                curr_ival_width = static_cast<size_type>(std::ceil(curr_axis_dim / static_cast<double>(curr_div)));
                rgr.change_interval_width(curr_ival_width);

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
                return res.template resize<Level>({count});
            }
            return res;
        }

        template <std::integral U>
        [[nodiscard]] constexpr auto expand(U axis, size_type division = 0) const
        {
            return expand<this_type::depth>(axis, division);
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
            requires(Level == 0
                && invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires(Level > 0
                && invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type>::value_type,
                typename arrnd_inner_t<this_type>::value_type, Args...>
        [[nodiscard]] constexpr this_type sort(Comp&& comp, Args&&... args) const
        {
            return sort<this_type::depth, Comp, Args...>(std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, std::integral U, typename Comp, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Comp, arrnd_inner_t<this_type, Level>, arrnd_inner_t<this_type, Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(U axis, Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            assert(axis >= 0 && axis < hdr_.dims().size());

            auto expanded = expand<Level>(axis);

            auto sorted = expanded.template sort<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);

            auto reduced = sorted.template reduce<Level>([axis](const auto& acc, const auto& cur) {
                return acc.template append<Level>(cur, axis);
            });

            return reduced.template reshape<Level>(hdr_.dims());
        }
        template <std::int64_t Level, std::integral U, typename Comp, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Comp, arrnd_inner_t<this_type, Level>, arrnd_inner_t<this_type, Level>, Args...>)
        [[nodiscard]] constexpr this_type sort(U axis, Comp&& comp, Args&&... args) const
        {
            if (empty()) {
                return this_type();
            }

            this_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template sort<Level - 1, U, Comp, Args...>(
                    axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <std::integral U, typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, arrnd_inner_t<this_type>, arrnd_inner_t<this_type>, Args...>
        [[nodiscard]] constexpr this_type sort(U axis, Comp&& comp, Args&&... args) const
        {
            return sort<this_type::depth, U, Comp, Args...>(
                axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Comp, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires(Level > 0
                && invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Comp, typename arrnd_inner_t<this_type>::value_type,
                typename arrnd_inner_t<this_type>::value_type, Args...>
        [[nodiscard]] constexpr auto is_sorted(Comp&& comp, Args&&... args) const
        {
            return is_sorted<this_type::depth, Comp, Args...>(std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, std::integral U, typename Comp, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Comp, arrnd_inner_t<this_type, Level>, arrnd_inner_t<this_type, Level>, Args...>)
        [[nodiscard]] constexpr bool is_sorted(U axis, Comp&& comp, Args&&... args) const
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
        template <std::int64_t Level, std::integral U, typename Comp, typename... Args>
            requires(Level > 0
                && invocable_no_arrnd<Comp, arrnd_inner_t<this_type, Level>, arrnd_inner_t<this_type, Level>, Args...>)
        [[nodiscard]] constexpr auto is_sorted(U axis, Comp&& comp, Args&&... args) const
        {
            using is_sorted_type = inner_replaced_type<bool, Level - 1>;

            if (empty()) {
                return is_sorted_type();
            }

            is_sorted_type res(hdr_.dims().cbegin(), hdr_.dims().cend());

            indexer_type gen(hdr_);
            indexer_type res_gen(res.header());

            for (; gen && res_gen; ++gen, ++res_gen) {
                res[*res_gen] = (*this)[*gen].template is_sorted<Level - 1, U, Comp, Args...>(
                    axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
            }

            return res;
        }
        template <std::integral U, typename Comp, typename... Args>
            requires invocable_no_arrnd<Comp, arrnd_inner_t<this_type>, arrnd_inner_t<this_type>, Args...>
        [[nodiscard]] constexpr auto is_sorted(U axis, Comp&& comp, Args&&... args) const
        {
            return is_sorted<this_type::depth, U, Comp, Args...>(
                axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires(
                Level > 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
        [[nodiscard]] constexpr bool all_match(Pred&& pred, Args&&... args) const
        {
            return all_match<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
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
                && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
        [[nodiscard]] constexpr bool all_match(const ArCo& arr, Pred&& pred, Args&&... args) const
        {
            return all_match<this_type::depth, Pred, ArCo, Args...>(
                arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, typename Pred, typename... Args>
            requires(
                Level == 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires(
                Level > 0 && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type, Args...>
        [[nodiscard]] constexpr bool any_match(Pred&& pred, Args&&... args) const
        {
            return any_match<this_type::depth, Pred, Args...>(std::forward<Pred>(pred), std::forward<Args>(args)...);
        }

        template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
            requires(Level == 0
                && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
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
                && invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, Level>::value_type,
                    typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
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
            requires invocable_no_arrnd<Pred, typename arrnd_inner_t<this_type, this_type::depth>::value_type,
                typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
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

        template <std::int64_t Level = this_type::depth, std::integral U>
        [[nodiscard]] constexpr replaced_type<bool> all(U axis) const
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

        template <std::int64_t Level = this_type::depth, std::integral U>
        [[nodiscard]] constexpr replaced_type<bool> any(U axis) const
        {
            return reduce<Level>(axis, [](const auto& a, const auto& b) {
                return a || b;
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

        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto begin(U axis = 0)
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, axis));
        }
        /**
        * @note the const begin/end/rbegin/rend functions are for compatibility with the free standard library begin/end/rbegin/rend functions
        */
        [[nodiscard]] constexpr auto begin() const
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, 0));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto end(U axis = 0)
        {
            return empty() ? iterator()
                           : iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::end));
        }
        [[nodiscard]] constexpr auto end() const
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, 0, arrnd_indexer_position::end));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cbegin(U axis = 0) const
        {
            return empty() ? const_iterator() : const_iterator(buffsp_->data(), indexer_type(hdr_, axis));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cend(U axis = 0) const
        {
            return empty() ? const_iterator()
                           : const_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::end));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rbegin(U axis = 0)
        {
            return empty()
                ? reverse_iterator()
                : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::rbegin));
        }
        [[nodiscard]] constexpr auto rbegin() const
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(), indexer_type(hdr_, 0, arrnd_indexer_position::rbegin));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rend(U axis = 0)
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::rend));
        }
        [[nodiscard]] constexpr auto rend() const
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(), indexer_type(hdr_, 0, arrnd_indexer_position::rend));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crbegin(U axis = 0) const
        {
            return empty()
                ? const_reverse_iterator()
                : const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::rbegin));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crend(U axis = 0) const
        {
            return empty()
                ? const_reverse_iterator()
                : const_reverse_iterator(buffsp_->data(), indexer_type(hdr_, axis, arrnd_indexer_position::rend));
        }

        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto begin(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? iterator() : iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto end(const InputIt& first_order, const InputIt& last_order)
        {
            return empty()
                ? iterator()
                : iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::end));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto cbegin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_iterator()
                           : const_iterator(buffsp_->data(), indexer_type(hdr_, first_order, last_order));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto cend(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_iterator()
                           : const_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::end));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rbegin(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::rbegin));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto rend(const InputIt& first_order, const InputIt& last_order)
        {
            return empty() ? reverse_iterator()
                           : reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::rend));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto crbegin(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::rbegin));
        }
        template <integral_type_iterator InputIt>
        [[nodiscard]] constexpr auto crend(const InputIt& first_order, const InputIt& last_order) const
        {
            return empty() ? const_reverse_iterator()
                           : const_reverse_iterator(buffsp_->data(),
                               indexer_type(hdr_, first_order, last_order, arrnd_indexer_position::rend));
        }

        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto begin(std::initializer_list<U> order)
        {
            return begin(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto end(std::initializer_list<U> order)
        {
            return end(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cbegin(std::initializer_list<U> order) const
        {
            return cbegin(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cend(std::initializer_list<U> order) const
        {
            return cend(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rbegin(std::initializer_list<U> order)
        {
            return rbegin(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rend(std::initializer_list<U> order)
        {
            return rend(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crbegin(std::initializer_list<U> order) const
        {
            return crbegin(order.begin(), order.end());
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crend(std::initializer_list<U> order) const
        {
            return crend(order.begin(), order.end());
        }

        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto begin(const Cont& order)
        {
            return begin(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto end(const Cont& order)
        {
            return end(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto cbegin(const Cont& order) const
        {
            return cbegin(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto cend(const Cont& order) const
        {
            return cend(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rbegin(const Cont& order)
        {
            return rbegin(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto rend(const Cont& order)
        {
            return rend(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto crbegin(const Cont& order) const
        {
            return crbegin(std::begin(order), std::end(order));
        }
        template <integral_type_iterable Cont>
        [[nodiscard]] constexpr auto crend(const Cont& order) const
        {
            return crend(std::begin(order), std::end(order));
        }

        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto begin(const U (&order)[M])
        {
            return begin(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto end(const U (&order)[M])
        {
            return end(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto cbegin(const U (&order)[M]) const
        {
            return cbegin(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto cend(const U (&order)[M]) const
        {
            return cend(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto rbegin(const U (&order)[M])
        {
            return rbegin(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto rend(const U (&order)[M])
        {
            return rend(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto crbegin(const U (&order)[M]) const
        {
            return crbegin(std::begin(order), std::end(order));
        }
        template <std::integral U, std::int64_t M>
        [[nodiscard]] constexpr auto crend(const U (&order)[M]) const
        {
            return crend(std::begin(order), std::end(order));
        }

        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto begin_subarray(U fixed_axis = 0)
        {
            return empty() ? subarray_iterator() : subarray_iterator(*this, ranger_type(hdr_, fixed_axis));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto end_subarray(U fixed_axis = 0)
        {
            return empty() ? subarray_iterator() : subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true) + 1);
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cbegin_subarray(U fixed_axis = 0) const
        {
            return empty() ? const_subarray_iterator() : const_subarray_iterator(*this, ranger_type(hdr_, fixed_axis));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto cend_subarray(U fixed_axis = 0) const
        {
            return empty() ? const_subarray_iterator()
                           : const_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true) + 1);
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rbegin_subarray(U fixed_axis = 0)
        {
            return empty() ? reverse_subarray_iterator()
                           : reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto rend_subarray(U fixed_axis = 0)
        {
            return empty() ? reverse_subarray_iterator()
                           : reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis) - 1);
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crbegin_subarray(U fixed_axis = 0) const
        {
            return empty() ? const_reverse_subarray_iterator()
                           : const_reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis, true));
        }
        template <std::integral U = size_type>
        [[nodiscard]] constexpr auto crend_subarray(U fixed_axis = 0) const
        {
            return empty() ? const_reverse_subarray_iterator()
                           : const_reverse_subarray_iterator(*this, ranger_type(hdr_, fixed_axis) - 1);
        }

        [[nodiscard]] constexpr auto abs()
        {
            return transform([](const auto& a) {
                using std::abs;
                return abs(a);
            });
        }

        [[nodiscard]] constexpr auto acos()
        {
            return transform([](const auto& a) {
                using std::acos;
                return acos(a);
            });
        }

        [[nodiscard]] constexpr auto acosh()
        {
            return transform([](const auto& a) {
                using std::acosh;
                return acosh(a);
            });
        }

        [[nodiscard]] constexpr auto asin()
        {
            return transform([](const auto& a) {
                using std::asin;
                return asin(a);
            });
        }

        [[nodiscard]] constexpr auto asinh()
        {
            return transform([](const auto& a) {
                using std::asinh;
                return asinh(a);
            });
        }

        [[nodiscard]] constexpr auto atan()
        {
            return transform([](const auto& a) {
                using std::atan;
                return atan(a);
            });
        }

        [[nodiscard]] constexpr auto atanh()
        {
            return transform([](const auto& a) {
                using std::atanh;
                return atanh(a);
            });
        }

        [[nodiscard]] constexpr auto cos()
        {
            return transform([](const auto& a) {
                using std::cos;
                return cos(a);
            });
        }

        [[nodiscard]] constexpr auto cosh()
        {
            return transform([](const auto& a) {
                using std::cosh;
                return cosh(a);
            });
        }

        [[nodiscard]] constexpr auto exp()
        {
            return transform([](const auto& a) {
                using std::exp;
                return exp(a);
            });
        }

        [[nodiscard]] constexpr auto log()
        {
            return transform([](const auto& a) {
                using std::log;
                return log(a);
            });
        }

        [[nodiscard]] constexpr auto log10()
        {
            return transform([](const auto& a) {
                using std::log10;
                return log10(a);
            });
        }

        [[nodiscard]] constexpr auto pow()
        {
            return transform([](const auto& a) {
                using std::pow;
                return pow(a);
            });
        }

        [[nodiscard]] constexpr auto sin()
        {
            return transform([](const auto& a) {
                using std::sin;
                return sin(a);
            });
        }

        [[nodiscard]] constexpr auto sinh()
        {
            return transform([](const auto& a) {
                using std::sinh;
                return sinh(a);
            });
        }

        [[nodiscard]] constexpr auto sqrt()
        {
            return transform([](const auto& a) {
                using std::sqrt;
                return sqrt(a);
            });
        }

        [[nodiscard]] constexpr auto tan()
        {
            return transform([](const auto& a) {
                using std::tan;
                return tan(a);
            });
        }

        [[nodiscard]] constexpr auto tanh()
        {
            return transform([](const auto& a) {
                using std::tanh;
                return tanh(a);
            });
        }

    private:
        header_type hdr_{};
        std::shared_ptr<storage_type> buffsp_{nullptr};
        std::shared_ptr<bool> original_valid_creator_ = std::allocate_shared<bool>(shared_ref_allocator_type<bool>());
        std::weak_ptr<bool> is_creator_valid_{};
        const this_type* creator_ = nullptr;
    };

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst)
    {
        return dst.copy_from(src);
    }

    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, integral_type_iterator InputIt>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::pair<InputIt, InputIt> indices)
    {
        return dst.copy_from(src, indices);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, integral_type_iterable Cont>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const Cont& indices)
    {
        return dst.copy_from(src, indices);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U = typename ArCo1::size_type>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::initializer_list<U> indices)
    {
        return dst.copy_from(src, indices);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::int64_t M>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const U (&indices)[M])
    {
        return dst.copy_from(src, indices);
    }

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
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U = typename ArCo1::size_type>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, std::initializer_list<interval<U>> ranges)
    {
        return copy(src, dst, ranges.begin(), ranges.end());
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::int64_t M>
    inline constexpr auto& copy(const ArCo1& src, ArCo2&& dst, const interval<U> (&ranges)[M])
    {
        return copy(src, dst, std::begin(ranges), std::end(ranges));
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

    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reshape(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr.template reshape<Level>(first_new_dim, last_new_dim);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const Cont& new_dims)
    {
        return reshape<Level>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, std::initializer_list<U> new_dims)
    {
        return reshape<Level>(arr, new_dims.begin(), new_dims.end());
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const U (&new_dims)[M])
    {
        return reshape<Level>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <std::int64_t Level, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, arrnd_shape shape)
    {
        return arr.template reshape<Level>(shape);
    }
    template <arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto reshape(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr.template reshape<ArCo::depth>(first_new_dim, last_new_dim);
    }
    template <arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const Cont& new_dims)
    {
        return reshape<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, std::initializer_list<U> new_dims)
    {
        return reshape<ArCo::depth>(arr, new_dims.begin(), new_dims.end());
    }
    template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, const U (&new_dims)[M])
    {
        return reshape<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto reshape(const ArCo& arr, arrnd_shape shape)
    {
        return reshape<ArCo::depth>(arr, shape);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto resize(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr.template resize<Level>(first_new_dim, last_new_dim);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const Cont& new_dims)
    {
        return resize<Level>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, std::initializer_list<U> new_dims)
    {
        return resize<Level>(arr, new_dims.begin(), new_dims.end());
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const U (&new_dims)[M])
    {
        return resize<Level>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto resize(
        const ArCo& arr, const InputIt& first_new_dim, const InputIt& last_new_dim)
    {
        return arr.template resize<ArCo::depth>(first_new_dim, last_new_dim);
    }
    template <arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const Cont& new_dims)
    {
        return resize<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    }
    template <arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, std::initializer_list<U> new_dims)
    {
        return resize<ArCo::depth>(arr, new_dims.begin(), new_dims.end());
    }
    template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto resize(const ArCo& arr, const U (&new_dims)[M])
    {
        return resize<ArCo::depth>(arr, std::begin(new_dims), std::end(new_dims));
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
    {
        return lhs.template append<Level>(rhs);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs)
    {
        return append<ArCo1::depth>(lhs, rhs);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, U axis)
    {
        return lhs.template append<Level>(rhs, axis);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U>
    [[nodiscard]] inline constexpr auto append(const ArCo1& lhs, const ArCo2& rhs, U axis)
    {
        return append<ArCo1::depth>(lhs, rhs, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U>
    [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, U ind)
    {
        return lhs.template insert<Level>(rhs, ind);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U>
    [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, U ind)
    {
        return insert<ArCo1::depth>(lhs, rhs, ind);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::integral V>
    [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, U ind, V axis)
    {
        return lhs.template insert<Level>(rhs, ind, axis);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2, std::integral U, std::integral V>
    [[nodiscard]] inline constexpr auto insert(const ArCo1& lhs, const ArCo2& rhs, U ind, V axis)
    {
        return insert<ArCo1::depth>(lhs, rhs, ind, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename... PairsOrTuples>
    [[nodiscard]] inline constexpr auto cat(const ArCo& arr, PairsOrTuples&&... pairs_or_tuples)
    {
        return arr.cat<Level>(std::forward<PairsOrTuples>(pairs_or_tuples)...);
    }
    template <arrnd_compliant ArCo, typename... PairsOrTuples>
    [[nodiscard]] inline constexpr auto cat(const ArCo& arr, PairsOrTuples&&... pairs_or_tuples)
    {
        return cat<ArCo::depth>(arr, std::forward<PairsOrTuples>(pairs_or_tuples)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V>
    [[nodiscard]] inline constexpr auto remove(const ArCo& arr, U ind, V count)
    {
        return arr.template remove<Level>(ind, count);
    }
    template <arrnd_compliant ArCo, std::integral U, std::integral V>
    [[nodiscard]] inline constexpr auto remove(const ArCo& arr, U ind, V count)
    {
        return remove<ArCo::depth>(arr, ind, count);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::integral V, std::integral W>
    [[nodiscard]] inline constexpr auto remove(const ArCo& arr, U ind, V count, W axis)
    {
        return arr.template remove<Level>(ind, count, axis);
    }
    template <arrnd_compliant ArCo, std::integral U, std::integral V, std::integral W>
    [[nodiscard]] inline constexpr auto remove(const ArCo& arr, U ind, V count, W axis)
    {
        return remove<ArCo::depth>(arr, ind, count, axis);
    }

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

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, U axis, Func&& func, Args&&... args)
    {
        return arr.template reduce<Level>(axis, std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, std::integral U, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto reduce(const ArCo& arr, U axis, Func&& func, Args&&... args)
    {
        return reduce<ArCo::depth>(arr, axis, std::forward<Func>(func), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, std::integral U, arrnd_compliant ArCo2, typename Func,
        typename... Args>
    [[nodiscard]] inline constexpr auto fold(const ArCo1& arr, U axis, const ArCo2& inits, Func&& func, Args&&... args)
    {
        return arr.template fold<Level>(axis, inits, std::forward<Func>(func), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo1, std::integral U, arrnd_compliant ArCo2, typename Func, typename... Args>
    [[nodiscard]] inline constexpr auto fold(const ArCo1& arr, U axis, const ArCo2& inits, Func&& func, Args&&... args)
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

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto all(const ArCo& arr, U axis)
    {
        return arr.template all<Level>(axis);
    }

    template <arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto all(const ArCo& arr, U axis)
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

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto any(const ArCo& arr, U axis)
    {
        return arr.template any<Level>(axis);
    }

    template <arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto any(const ArCo& arr, U axis)
    {
        return any<ArCo::depth>(arr, axis);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Func, typename... Args>
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
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
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
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
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
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
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
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
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
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
        requires invocable_no_arrnd<Func, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
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
        requires(invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template filter<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Pred, typename... Args>
        requires(invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>)
    [[nodiscard]] inline constexpr auto filter(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return filter<ArCo::depth>(arr, std::forward<Pred>(pred), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto filter(const ArCo1& arr, const ArCo2& mask)
    {
        return arr.template filter<Level>(mask);
    }
    template <arrnd_compliant ArCo1, arrnd_compliant ArCo2>
    [[nodiscard]] inline constexpr auto filter(const ArCo1& arr, const ArCo2& mask)
    {
        return filter<ArCo1::depth>(arr, mask);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
        requires(invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>)
    [[nodiscard]] inline constexpr auto find(const ArCo& arr, Pred&& pred, Args&&... args)
    {
        return arr.template find<Level>(std::forward<Pred>(pred), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Pred, typename... Args>
        requires(invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>)
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

    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, const InputIt& first_order, const InputIt& last_order)
    {
        return arr.template transpose<Level>(first_order, last_order);
    }
    template <std::int64_t Level, arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const Cont& order)
    {
        return transpose<Level>(arr, std::begin(order), std::end(order));
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, std::initializer_list<U> order)
    {
        return transpose<Level>(arr, order.begin(), order.end());
    }
    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const U (&order)[M])
    {
        return transpose<Level>(arr, std::begin(order), std::end(order));
    }
    template <arrnd_compliant ArCo, integral_type_iterator InputIt>
    [[nodiscard]] inline constexpr auto transpose(
        const ArCo& arr, const InputIt& first_order, const InputIt& last_order)
    {
        return arr.template transpose<ArCo::depth>(first_order, last_order);
    }
    template <arrnd_compliant ArCo, integral_type_iterable Cont>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const Cont& order)
    {
        return transpose<ArCo::depth>(arr, std::begin(order), std::end(order));
    }
    template <arrnd_compliant ArCo, std::integral U = typename ArCo::size_type>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, std::initializer_list<U> order)
    {
        return transpose<ArCo::depth>(arr, order.begin(), order.end());
    }
    template <arrnd_compliant ArCo, std::integral U, std::int64_t M>
    [[nodiscard]] inline constexpr auto transpose(const ArCo& arr, const U (&order)[M])
    {
        return transpose<ArCo::depth>(arr, std::begin(order), std::end(order));
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
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a == b;
        });
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator==(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(lhs, [](const auto& b, const auto& a) {
            return a == b;
        });
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
        return lhs.transform(rhs, [](const auto& a, const auto& b) {
            return a != b;
        });
    }

    template <typename T, arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto operator!=(const T& lhs, const ArCo& rhs)
    {
        return rhs.transform(lhs, [](const auto& b, const auto& a) {
            return a != b;
        });
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

    template <arrnd_compliant ArCo>
    [[nodiscard]] inline constexpr auto pow(const ArCo& arr)
    {
        return arr.pow();
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

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto expand(const ArCo& arr, U axis, typename ArCo::size_type division = 0)
    {
        return arr.template expand<Level>(axis, division);
    }
    template <arrnd_compliant ArCo, std::integral U>
    [[nodiscard]] inline constexpr auto expand(const ArCo& arr, U axis, typename ArCo::size_type division = 0)
    {
        return expand<ArCo::depth>(arr, axis, division);
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
        requires invocable_no_arrnd<Comp, typename arrnd_inner_t<ArCo, Level>::value_type,
            typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return arr.template sort<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, typename arrnd_inner_t<ArCo>::value_type,
            typename arrnd_inner_t<ArCo>::value_type, Args...>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return sort<ArCo::depth>(arr, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, arrnd_inner_t<ArCo, Level>, arrnd_inner_t<ArCo, Level>, Args...>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, U axis, Comp&& comp, Args&&... args)
    {
        return arr.template sort<Level>(axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, std::integral U, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, arrnd_inner_t<ArCo>, arrnd_inner_t<ArCo>, Args...>
    [[nodiscard]] inline constexpr auto sort(const ArCo& arr, U axis, Comp&& comp, Args&&... args)
    {
        return sort<ArCo::depth>(arr, axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, typename arrnd_inner_t<ArCo, Level>::value_type,
            typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return arr.template is_sorted<Level>(std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, typename arrnd_inner_t<ArCo>::value_type,
            typename arrnd_inner_t<ArCo>::value_type, Args...>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, Comp&& comp, Args&&... args)
    {
        return is_sorted<ArCo::depth>(arr, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, std::integral U, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, arrnd_inner_t<ArCo, Level>, arrnd_inner_t<ArCo, Level>, Args...>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, U axis, Comp&& comp, Args&&... args)
    {
        return arr.template is_sorted<Level>(axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }
    template <arrnd_compliant ArCo, std::integral U, typename Comp, typename... Args>
        requires invocable_no_arrnd<Comp, arrnd_inner_t<ArCo>, arrnd_inner_t<ArCo>, Args...>
    [[nodiscard]] inline constexpr auto is_sorted(const ArCo& arr, U axis, Comp&& comp, Args&&... args)
    {
        return is_sorted<ArCo::depth>(arr, axis, std::forward<Comp>(comp), std::forward<Args>(args)...);
    }

    template <std::int64_t Level, arrnd_compliant ArCo, typename Pred, typename... Args>
        requires invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, Level>::value_type, Args...>
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
        requires invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo, ArCo::depth>::value_type, Args...>
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
        requires invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo>::value_type, Args...>
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
        requires invocable_no_arrnd<Pred, typename arrnd_inner_t<ArCo>::value_type, Args...>
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

        if constexpr (ArCo::depth == 0) {
            if (std::ssize(arco.header().dims()) > 1) {
                os << block_start_char;
                for (typename ArCo::size_type i = 0; i < arco.header().dims()[0]; ++i) {
                    if (i > 0) {
                        for (typename ArCo::size_type i = 0; i < ndepth_spaces + nvectical_spaces + 1; ++i) {
                            os << ' ';
                        }
                    }
                    ostream_operator_recursive(
                        os, arco[interval<typename ArCo::size_type>{i, i + 1}], nvectical_spaces + 1, ndepth_spaces);
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
        arrnd<typename ArCo::value_type, typename ArCo::storage_type, ArCo::template shared_ref_allocator_type,
            typename ArCo::header_type, arrnd_general_indexer>
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
            arrnd<typename ArCo::value_type, typename ArCo::storage_type, ArCo::template shared_ref_allocator_type,
                typename ArCo::header_type, arrnd_general_indexer>
                carco = arco;
            typename ArCo::size_type nvertical_spaces = 4;
            ajm.os_ << "{\n";
            ajm.os_ << std::string(nvertical_spaces, ' ') << "\"base_type\": \""
                    << type_name<typename arrnd_inner_t<ArCo>::value_type>() << "\"\n";
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

            if constexpr (ArCo::depth == 0) {
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

    struct arrnd_json_manip_tag {
    } arrnd_json;
    arrnd_json_manip operator<<(std::ostream& os, arrnd_json_manip_tag)
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

using details::arrnd_iterator;
using details::arrnd_const_iterator;
using details::arrnd_reverse_iterator;
using details::arrnd_const_reverse_iterator;

using details::arrnd_axis_iterator;
using details::arrnd_axis_const_iterator;
using details::arrnd_axis_reverse_iterator;
using details::arrnd_axis_reverse_const_iterator;

using details::arrnd_back_inserter;
using details::arrnd_front_inserter;
using details::arrnd_inserter;

using details::arrnd_axis_back_inserter;
using details::arrnd_axis_front_inserter;
using details::arrnd_axis_inserter;

using details::arrnd_shape;
using details::arrnd;

using details::copy;
using details::set;
using details::clone;
using details::reshape;
using details::resize;
using details::append;
using details::insert;
using details::cat;
using details::remove;

using details::empty;
using details::expand;
using details::squeeze;
using details::sort;
using details::is_sorted;
using details::all_match;
using details::any_match;
using details::transform;
using details::apply;
using details::reduce;
using details::fold;
using details::all;
using details::any;
using details::filter;
using details::find;
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
}

#endif // OC_ARRAY_H
