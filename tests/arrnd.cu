#include <gtest/gtest.h>

#include <memory>

#include <cuda_runtime.h>
#include <device_launch_parameters.h>

#define __COMPILE_CUDA__
#include <oc/arrnd.h>

#define WARMUP_KERNEL_RESULT (.5f)
__global__ void warmup_kernel(float* p)
{
    unsigned int idx = threadIdx.x;
    p[idx] = .5f;
}

class cuda_tests : public ::testing::Test {
public:
    static void SetUpTestCase()
    {
        cudaError_t err;

        err = cudaSetDevice(device_id);
        if (err != cudaSuccess) {
            FAIL() << "Failed to set CUDA device " << device_id << "(" << err << ": " << cudaGetErrorString(err) << ")";
        }

        float h_ref = -1.f;
        err = warmup(h_ref);
        if (err != cudaSuccess) {
            FAIL() << "Failed to wramup CUDA device (" << err << ": " << cudaGetErrorString(err) << ")";
        }
        if (h_ref != WARMUP_KERNEL_RESULT) {
            FAIL() << "Invalid warmup kernel result (required: " << WARMUP_KERNEL_RESULT << ", actual: " << h_ref
                   << ")";
        }
    }

    static void TearDownTestCase()
    {
        cudaError_t err = cudaDeviceReset();
        if (err != cudaSuccess) {
            FAIL() << "Failed to reset CUDA device (" << err << ": " << cudaGetErrorString(err) << ")";
        }
    }

private:
    // simple CUDA GPU warmup 
    static cudaError_t warmup(float& h_ref)
    {
        std::unique_ptr<float> h_p = std::make_unique<float>(0.f);

        cudaError_t err;

        float* d_p;
        err = cudaMalloc((void**)&d_p, sizeof(float));
        if (err != cudaSuccess) {
            return err;
        }
        err = cudaMemcpy(d_p, h_p.get(), sizeof(float), cudaMemcpyHostToDevice);
        if (err != cudaSuccess) {
            return err;
        }

        warmup_kernel<<<1, 1>>>(d_p);
        err = cudaDeviceSynchronize();
        if (err != cudaSuccess) {
            return err;
        }

        err = cudaMemcpy(h_p.get(), d_p, sizeof(float), cudaMemcpyDeviceToHost);
        if (err != cudaSuccess) {
            return err;
        }
        h_ref = *h_p;

        cudaFree(d_p);

        return cudaSuccess;
    }

    static constexpr int device_id = 0;
};

TEST_F(cuda_tests, dummy)
{
    SUCCEED();
}
