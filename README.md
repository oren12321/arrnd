# oc-arrnd

A C++ implementation of flexible N dimensional array (no GPU/multithreading usage) including std library compatibility.

Usage example (simple matrices multiplication):

```cpp
using namespace oc::arrnd;

oc::arrnd<int> mul(const arrnd<int>& lhs, const arrnd<int>& rhs)
{
    assert(size(lhs.info()) == 2);
    assert(size(rhs.info()) == 2);
    assert(lhs.info().dims()[1] == rhs.info().dims()[0]);

    arrnd<int> res({ lhs.info().dims()[0], rhs.info().dims()[1] });

    std::int64_t ind = 0;
    auto trhs = transpose(rhs);
    std::for_each(lhs.cbegin(arrnd_returned_slice_iterator_tag{}), lhs.cend(arrnd_returned_slice_iterator_tag{}), [&](const auto& row) {
        std::for_each(trhs.cbegin(arrnd_returned_slice_iterator_tag{}), trhs.cend(arrnd_returned_slice_iterator_tag{}), [&](const auto& col) {
            res[ind++] = (row * col).reduce(std::plus<>{});
            });
        });

    return res;
}

arrnd<int> m1({ 2, 3 },
    { 0, 4, -2,
    -4, -3, 0 });

arrnd<int> m2({ 3, 2 },
    { 0, 1,
    1, -1,
    2, 3 });

std::cout << mul(m1, m2) << '\n';
```

Output:

```
[[0 -10]
 [-3 -1]]
```
