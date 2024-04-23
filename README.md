# oc-arrnd

A C++ implementation of flexible N dimensional array (no GPU usage) including std compatibility

Usage example (simple matrices multiplication):

```cpp
oc::arrnd<int> mul(const oc::arrnd<int>& lhs, const oc::arrnd<int>& rhs)
{
    assert(lhs.header().dims().size() == 2);
    assert(rhs.header().dims().size() == 2);
    assert(lhs.header().dims()[1] == rhs.header().dims()[0]);

    oc::arrnd<int> res({ lhs.header().dims()[0], rhs.header().dims()[1] });

    std::int64_t ind = 0;
    auto trhs = oc::transpose(rhs, { 1, 0 });
    std::for_each(lhs.cbegin(arrnd_returned_slice_iterator_tag{}), lhs.cend(arrnd_returned_slice_iterator_tag{}), [&](const auto& row) {
        std::for_each(trhs.cbegin(arrnd_returned_slice_iterator_tag{}), trhs.cend(arrnd_returned_slice_iterator_tag{}), [&](const auto& col) {
            res[ind++] = oc::reduce(row * col, std::plus<>{});
            });
        });

    return res;
}

oc::arrnd<int> m1({ 2, 3 },
    { 0, 4, -2,
    -4, -3, 0 });

oc::arrnd<int> m2({ 3, 2 },
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
