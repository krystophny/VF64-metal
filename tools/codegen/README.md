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
exact rational quotient around rounding thresholds. Another million arbitrary
binary32 seed bit patterns exercise the ordered guard and integer certificate;
invalid seeds and deliberately inaccurate finite seeds must reject. Controls
that move a proxy across an exact rounding threshold must fail the rational check.
These CPU checks do not certify the compiled Metal kernel.

Compile consumers with `-fno-fast-math -ffp-contract=off`. The support AIR builder
sets these flags. The floating estimate stays in the caller and uses explicit
`metal::fast::divide`; the generated leaf first selects a safe finite seed before
its eager float-to-integer cast and propagates the original validity predicate.
See [the arithmetic contract](../../docs/certified-division.md).
