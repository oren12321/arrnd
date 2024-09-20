#ifndef TMPAUX_H
#define TMPAUX_H

#include <oc/arrnd.h>

namespace oc::arrnd {
template <std::int64_t Level, arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto transform(const Arrnd& arr, Func&& func)
{
    return arr.template transform<Level>(std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr auto transform(const Arrnd& lhs, const U& rhs, Func&& func)
{
    return lhs.template transform<Level>(rhs, std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto transform(const Arrnd& arr, Func&& func)
{
    return transform<Arrnd::depth>(arr, std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr auto transform(const Arrnd& lhs, const U& rhs, Func&& func)
{
    return transform<Arrnd::depth>(lhs, rhs, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Func>
inline constexpr auto& apply(Arrnd&& arr, Func&& func)
{
    return arr.template apply<Level>(std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
inline constexpr auto& apply(Arrnd&& lhs, const U& rhs, Func&& func)
{
    return lhs.template apply<Level>(rhs, std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename Func>
inline constexpr auto& apply(Arrnd&& arr, Func&& func)
{
    return apply<std::remove_cvref_t<Arrnd>::depth>(std::forward<Arrnd>(arr), std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
inline constexpr auto& apply(Arrnd&& lhs, const U& rhs, Func&& func)
{
    return apply<std::remove_cvref_t<Arrnd>::depth>(std::forward<Arrnd>(lhs), rhs, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Func>
inline constexpr auto& apply(Arrnd& arr, Func&& func)
{
    return arr.template apply<Level>(std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
inline constexpr auto& apply(Arrnd& lhs, const U& rhs, Func&& func)
{
    return lhs.template apply<Level>(rhs, std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename Func>
inline constexpr auto& apply(Arrnd& arr, Func&& func)
{
    return apply<std::remove_cvref_t<Arrnd>::depth>(arr, std::forward<Func>(func));
}

template <arrnd_type Arrnd, typename U, typename Func>
    requires arrnd_type<U>
inline constexpr auto& apply(Arrnd& lhs, const U& rhs, Func&& func)
{
    return apply<std::remove_cvref_t<Arrnd>::depth>(lhs, rhs, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, Pred&& pred)
{
    return arr.template filter<Level>(std::forward<Pred>(pred));
}
template <arrnd_type Arrnd, typename Pred>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, Pred&& pred)
{
    return filter<Arrnd::depth>(arr, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd1, arrnd_type Arrnd2>
[[nodiscard]] inline constexpr auto filter(const Arrnd1& arr, const Arrnd2& selector)
{
    return arr.template filter<Level>(selector);
}
template <arrnd_type Arrnd1, arrnd_type Arrnd2>
[[nodiscard]] inline constexpr auto filter(const Arrnd1& arr, const Arrnd2& selector)
{
    return filter<Arrnd1::depth>(arr, selector);
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
[[nodiscard]] inline constexpr auto find(const Arrnd& arr, Pred&& pred)
{
    return arr.template find<Level>(std::forward<Pred>(pred));
}
template <arrnd_type Arrnd, typename Pred>
[[nodiscard]] inline constexpr auto find(const Arrnd& arr, Pred&& pred)
{
    return find<Arrnd::depth>(arr, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd1, arrnd_type Arrnd2>
[[nodiscard]] inline constexpr auto find(const Arrnd1& arr, const Arrnd2& mask)
{
    return arr.template find<Level>(mask);
}
template <arrnd_type Arrnd1, arrnd_type Arrnd2>
[[nodiscard]] inline constexpr auto find(const Arrnd1& arr, const Arrnd2& mask)
{
    return find<Arrnd1::depth>(arr, mask);
}

template <std::int64_t Level, arrnd_type Arrnd, details::iterator_of_type_integral InputIt>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, InputIt first_ind, InputIt last_ind)
{
    return filter<Level>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            {std::distance(first_ind, last_ind)}, first_ind, last_ind));
}
template <std::int64_t Level, arrnd_type Arrnd, details::iterable_of_type_integral Cont>
    requires(!arrnd_type<Cont>)
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, const Cont& indices)
{
    return filter<Level>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            {std::ssize(indices)}, std::begin(indices), std::end(indices)));
}
template <std::int64_t Level, arrnd_type Arrnd>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, std::initializer_list<typename Arrnd::size_type> indices)
{
    return filter<Level>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            {std::ssize(indices)}, indices.begin(), indices.end()));
}

template <arrnd_type Arrnd, details::iterator_of_type_integral InputIt>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, InputIt first_ind, InputIt last_ind)
{
    return filter<Arrnd::depth>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            {std::distance(first_ind, last_ind)}, first_ind, last_ind));
}
template <arrnd_type Arrnd, details::iterable_of_type_integral Cont>
    requires(!arrnd_type<Cont>)
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, const Cont& indices)
{
    return filter<Arrnd::depth>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            {std::ssize(indices)}, std::begin(indices), std::end(indices)));
}
template <arrnd_type Arrnd>
[[nodiscard]] inline constexpr auto filter(const Arrnd& arr, std::initializer_list<typename Arrnd::size_type> indices)
{
    std::initializer_list<typename Arrnd::size_type> dims{std::size(indices)};
    return filter<Arrnd::depth>(arr,
        typename Arrnd::template replaced_type<typename Arrnd::size_type>(
            dims.begin(), dims.end(), indices.begin(), indices.end()));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto reduce(const Arrnd& arr, Func&& func)
{
    return arr.template reduce<Level>(std::forward<Func>(func));
}
template <arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto reduce(const Arrnd& arr, Func&& func)
{
    return reduce<Arrnd::depth>(arr, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename T, typename Func>
[[nodiscard]] inline constexpr auto fold(const Arrnd& arr, const T& init, Func&& func)
{
    return arr.template fold<Level>(init, std::forward<Func>(func));
}
template <arrnd_type Arrnd, typename T, typename Func>
[[nodiscard]] inline constexpr auto fold(const Arrnd& arr, const T& init, Func&& func)
{
    return fold<Arrnd::depth>(arr, init, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto reduce(const Arrnd& arr, typename Arrnd::size_type axis, Func&& func)
{
    return arr.template reduce<Level>(axis, std::forward<Func>(func));
}
template <arrnd_type Arrnd, typename Func>
[[nodiscard]] inline constexpr auto reduce(const Arrnd& arr, typename Arrnd::size_type axis, Func&& func)
{
    return reduce<Arrnd::depth>(arr, axis, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd1, arrnd_type Arrnd2, typename Func>
[[nodiscard]] inline constexpr auto fold(
    const Arrnd1& arr, typename Arrnd1::size_type axis, const Arrnd2& inits, Func&& func)
{
    return arr.template fold<Level>(axis, inits, std::forward<Func>(func));
}
template <arrnd_type Arrnd1, arrnd_type Arrnd2, typename Func>
[[nodiscard]] inline constexpr auto fold(
    const Arrnd1& arr, typename Arrnd1::size_type axis, const Arrnd2& inits, Func&& func)
{
    return fold<Arrnd1::depth>(arr, axis, inits, std::forward<Func>(func));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U, typename Pred>
[[nodiscard]] inline constexpr bool all_match(const Arrnd& lhs, const U& rhs, Pred&& pred)
{
    return lhs.template all_match<Level>(rhs, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U>
[[nodiscard]] inline constexpr bool all_match(const Arrnd& lhs, const U& rhs)
{
    return lhs.template all_match<Level>(rhs);
}

template <std::int64_t Level, typename U, arrnd_type Arrnd, typename Pred>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool all_match(const U& lhs, const Arrnd& rhs, Pred&& pred)
{
    return rhs.template all_match<Level>(lhs, [&pred](const auto& a, const auto& b) {
        return pred(b, a);
    });
}

template <std::int64_t Level, typename U, arrnd_type Arrnd>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool all_match(const U& lhs, const Arrnd& rhs)
{
    return rhs.template all_match<Level>(lhs);
}

template <arrnd_type Arrnd, typename U, typename Pred>
[[nodiscard]] inline constexpr bool all_match(const Arrnd& lhs, const U& rhs, Pred&& pred)
{
    return all_match<Arrnd::depth>(lhs, rhs, std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename U>
[[nodiscard]] inline constexpr bool all_match(const Arrnd& lhs, const U& rhs)
{
    return all_match<Arrnd::depth>(lhs, rhs);
}

template <typename U, arrnd_type Arrnd, typename Pred>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool all_match(const U& lhs, const Arrnd& rhs, Pred&& pred)
{
    return all_match<Arrnd::depth>(lhs, rhs, std::forward<Pred>(pred));
}

template <typename U, arrnd_type Arrnd>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool all_match(const U& lhs, const Arrnd& rhs)
{
    return all_match<Arrnd::depth>(lhs, rhs);
}

template <std::int64_t Level, arrnd_type Arrnd, typename U, typename Pred>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr bool any_match(const Arrnd& lhs, const U& rhs, Pred&& pred)
{
    return lhs.template any_match<Level>(rhs, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd, typename U>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr bool any_match(const Arrnd& lhs, const U& rhs)
{
    return lhs.template any_match<Level>(rhs);
}

template <std::int64_t Level, typename U, arrnd_type Arrnd, typename Pred>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool any_match(const U& lhs, const Arrnd& rhs, Pred&& pred)
{
    return rhs.template any_match<Level>(lhs, [&pred](const auto& a, const auto& b) {
        return pred(b, a);
    });
}

template <std::int64_t Level, typename U, arrnd_type Arrnd>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool any_match(const U& lhs, const Arrnd& rhs)
{
    return rhs.template any_match<Level>(lhs);
}

template <arrnd_type Arrnd, typename U, typename Pred>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr bool any_match(const Arrnd& lhs, const U& rhs, Pred&& pred)
{
    return any_match<Arrnd::depth>(lhs, rhs, std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename U>
    requires arrnd_type<U>
[[nodiscard]] inline constexpr bool any_match(const Arrnd& lhs, const U& rhs)
{
    return any_match<Arrnd::depth>(lhs, rhs);
}

template <typename U, arrnd_type Arrnd, typename Pred>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool any_match(const U& lhs, const Arrnd& rhs, Pred&& pred)
{
    return any_match<Arrnd::depth>(lhs, rhs, std::forward<Pred>(pred));
}

template <typename U, arrnd_type Arrnd>
    requires(!arrnd_type<U>)
[[nodiscard]] inline constexpr bool any_match(const U& lhs, const Arrnd& rhs)
{
    return any_match<Arrnd::depth>(lhs, rhs);
}

namespace details {
    template <typename Func, typename... Args>
    concept invocable_no_arrnd = !arrnd_type<Func> && std::is_invocable_v<Func, Args...>;
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Level>::value_type>)
[[nodiscard]] inline constexpr bool all(const Arrnd& arr, Pred&& pred)
{
    return arr.template all<Level>(std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Arrnd::depth>::value_type>)
[[nodiscard]] inline constexpr bool all(const Arrnd& arr, Pred&& pred)
{
    return all<Arrnd::depth>(arr, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Level>::value_type>)
[[nodiscard]] inline constexpr bool any(const Arrnd& arr, Pred&& pred)
{
    return arr.template any<Level>(std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Arrnd::depth>::value_type>)
[[nodiscard]] inline constexpr bool any(const Arrnd& arr, Pred&& pred)
{
    return any<Arrnd::depth>(arr, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Level>::value_type>)
[[nodiscard]] inline constexpr auto all(const Arrnd& arr, typename Arrnd::size_type axis, Pred&& pred)
{
    return arr.template all<Level>(axis, std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Arrnd::depth>::value_type>)
[[nodiscard]] inline constexpr auto all(const Arrnd& arr, typename Arrnd::size_type axis, Pred&& pred)
{
    return all<Arrnd::depth>(arr, axis, std::forward<Pred>(pred));
}

template <std::int64_t Level, arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Level>::value_type>)
[[nodiscard]] inline constexpr auto any(const Arrnd& arr, typename Arrnd::size_type axis, Pred&& pred)
{
    return arr.template any<Level>(axis, std::forward<Pred>(pred));
}

template <arrnd_type Arrnd, typename Pred>
    requires(details::invocable_no_arrnd<Pred, typename Arrnd::template inner_type<Arrnd::depth>::value_type>)
[[nodiscard]] inline constexpr auto any(const Arrnd& arr, typename Arrnd::size_type axis, Pred&& pred)
{
    return any<Arrnd::depth>(arr, axis, std::forward<Pred>(pred));
}
}

#endif // TMPAUX_H
