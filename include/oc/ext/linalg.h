#ifndef OC_EXT_LINALG_H
#define OC_EXT_LINALG_H

#include <cstdint>
#include <type_traits>
#include <complex>
#include <algorithm>
#include <iterator>

#include <oc/arrnd.h>

namespace oc::linalg {
namespace details {
    template <oc::arrnd_complient_with_trait<std::is_arithmetic> ArCo>
    void recursive_transpose(const ArCo& src, ArCo dst)
    {
        if (src.header().dims().size() == 2) {
            auto tsrc = src.transpose({1, 0});
            dst.copy_from(tsrc);
            return;
        }

        typename ArCo::size_type first_dim = src.header().dims().front();
        for (typename ArCo::size_type i = 0; i < first_dim; ++i) {
            recursive_transpose(src[interval<std::int64_t>{i, i}], dst[interval<std::int64_t>{i, i}]);
        }
    }

    template <oc::arrnd_complient_of_template_type<std::complex> ArCo>
    void recursive_transpose(const ArCo& src, ArCo dst)
    {
        if (src.header().dims().size() == 2) {
            auto tsrc = src.transpose({1, 0});
            std::for_each(tsrc.begin(), tsrc.end(), [](auto& c) {
                c.imag(-c.imag());
            });
            dst.copy_from(tsrc);
            return;
        }

        typename ArCo::size_type first_dim = src.header().dims().front();
        for (typename ArCo::size_type i = 0; i < first_dim; ++i) {
            recursive_transpose(src[interval<std::int64_t>{i, i}], dst[interval<std::int64_t>{i, i}]);
        }
    }

    // equivalent to MATLAB's transpose, ctranspose, pagetranspose and pagectranspose functions
    template <oc::arrnd_complient ArCo>
    ArCo transpose(const ArCo& mat)
    {
        auto ndims = mat.header().dims().size();
        assert(ndims >= 2);

        auto dims = mat.header().dims();
        std::swap(dims[ndims - 2], dims[ndims - 1]);

        ArCo res(dims.cbegin(), dims.cend());
        recursive_transpose(mat, res);
        return res;
    }
}

using details::transpose;
}

#endif // OC_EXT_LINALG_H