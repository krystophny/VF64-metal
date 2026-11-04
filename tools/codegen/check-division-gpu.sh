#!/bin/sh
set -eu
source_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
shader_dir=$1
output_dir=$2
mkdir -p "$output_dir"
xcrun metal -std=metal3.2 -fno-fast-math -ffp-contract=off \
    -I "$shader_dir" -c "$source_dir/test_division.metal" \
    -o "$output_dir/test.air"
xcrun metallib "$output_dir/test.air" -o "$output_dir/test.metallib"
swift "$source_dir/TestDivision.swift" "$output_dir/test.metallib"
