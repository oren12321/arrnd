#include <gtest/gtest.h>

#include <cuda_runtime.h>

#define __COMPILE_CUDA__
#include <oc/arrnd.h>

TEST(cuda, dummy_kernel)
{
    cudaError_t err;
    dummy_kernel<<<1, 1>>>();
    err = cudaGetLastError();
    EXPECT_EQ(err, cudaSuccess);
    err = cudaDeviceSynchronize();
    EXPECT_EQ(err, cudaSuccess);
}
