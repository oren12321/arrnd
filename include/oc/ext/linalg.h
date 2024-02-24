#ifndef OC_EXT_LINALG_H
#define OC_EXT_LINALG_H

#include <type_traits>
#include <complex>
#include <algorithm>

#include <oc/arrnd.h>

namespace oc::linalg {
namespace details {
    template <typename T>
    concept numeric_arrnd_complient = oc::arrnd_complient<T> && std::is_arithmetic_v<typename T::value_type>;

    template <template <typename...> typename T, typename... Args>
    [[nodiscard]] static inline constexpr std::true_type is_template_type_impl(T<Args...>)
    {
        return std::true_type;
    }
    template <template <typename...> typename T>
    [[nodiscard]] static inline constexpr std::false_type is_template_type_impl(...)
    {
        return std::false_type;
    }
    template <template <typename...> typename T, typename U>
    using is_template_type = decltype(is_template_type_impl<T>(std::declval<typename std::decay_t<U>>()));

    template <typename T>
    concept complex_arrnd_complient = oc::arrnd_complient<T> && is_template_type<std::complex, typename T::value_type>::value;

    template <numeric_arrnd_complient ArCo>
    ArCo transpose(const ArCo& mat)
    {
        assert(mat.header().dims().size() == 2);
        return oc::transpose(mat, {1, 0});
    }

    template <complex_arrnd_complient ArCo>
    ArCo transpose(const ArCo& mat)
    {
        assert(mat.header().dims().size() == 2);
        auto tmat = oc::transpose(mat, {1, 0});
        std::for_each(tmat.begin(), tmat.end(), [](auto& c) {
            c.imag(-c.imag());
        });
        return tmat;
    }
}

using details::numeric_arrnd_complient;
using details::complex_arrnd_complient;

using details::transpose;
}

#endif // OC_EXT_LINALG_H
