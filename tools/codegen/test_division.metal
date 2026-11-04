#include "Core/Preamble.metal"
#include "IEEE/Arithmetic.metal"

kernel void division_coefficients(
    device const ulong *a [[buffer(0)]],
    device const ulong *b [[buffer(1)]],
    device const uint2 *injected [[buffer(2)]],
    device ulong2 *output [[buffer(3)]],
    device uint *status [[buffer(4)]],
    constant uint &mode [[buffer(5)]],
    uint tid [[thread_position_in_grid]]) {
    uint2 coefficients = soft_reciprocal_coefficients(b[tid]);
    if (mode == 1u) coefficients = injected[tid];
    if (mode == 2u) coefficients ^= injected[tid];
    bool accepted, boundary, repaired;
    const ulong value = soft_div64_rne_coefficients(a[tid], b[tid],
        coefficients.x, coefficients.y, accepted, boundary, repaired);
    output[tid] = ulong2(soft_div64_rne(a[tid], b[tid]), value);
    status[tid] = uint(accepted) | (uint(repaired) << 1);
}
