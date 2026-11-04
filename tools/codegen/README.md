# Certified division generation

The native Fortran generator describes the certified integer leaf with FortGen's
typed scalar intermediate representation. FortGen owns CUDA and Metal printing;
this directory owns VF64's algorithm. Use a FortGen checkout with the typed scalar
layer (including unsigned multiply-high and safe explicit casts).

```sh
cmake -S tools/codegen -B .build/codegen -DFORTGEN_SOURCE_DIR=/path/to/fortgen
cmake --build .build/codegen
.build/codegen/generate_division Sources/VF64Metal/Shaders/IEEE/CertifiedDivision.metal
ctest --test-dir .build/codegen --output-on-failure
```

Use an empty build directory when changing the FortGen source checkout, so an
earlier dependency build cannot supply stale modules or object files.

`division_regenerate` checks the tracked Metal output. `division_host` executes
the same graph through FortGen's CUDA emitter on the CPU with unsigned 128-bit
multiply-high primitives and undefined-behavior/float-cast sanitizers. It compares
four million signed normal input pairs against native binary64 division and the
exact rational quotient around rounding thresholds. Another million selected,
random and corrupted coefficient pairs check the exact acceptance condition
with unsigned 128-bit arithmetic. Both accepted and rejected coefficients are
required; all selected normal coefficients must accept. Controls that move a
proxy across a rounding threshold must fail the rational check.

On macOS, configure with `-DVF64_BUILD_METAL_TESTING=ON` to register
`division_gpu`. It checks the production table selector, runtime coefficient
corruption, fallback results, poisoned outputs and buffer guards against native
binary64 division. These bounded checks do not prove whole-domain conformance.

Compile consumers with `-fno-fast-math -ffp-contract=off`. The support AIR builder
sets these flags. All reciprocal arithmetic is generated through FortGen; only
table selection and the existing packer/fallback stay in the caller. The
Berkeley SoftFloat constants and seed formula retain their BSD notice. See
[the arithmetic contract](../../docs/certified-division.md).
