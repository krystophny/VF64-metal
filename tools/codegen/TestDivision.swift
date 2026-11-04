import Foundation
import Metal

guard CommandLine.arguments.count == 2,
      let device = MTLCreateSystemDefaultDevice(),
      let queue = device.makeCommandQueue() else {
    fatalError("usage: TestDivision.swift TEST.metallib; a Metal GPU is required")
}
let library = try device.makeLibrary(URL: URL(fileURLWithPath: CommandLine.arguments[1]))
guard let function = library.makeFunction(name: "division_coefficients") else {
    fatalError("missing division_coefficients")
}
let pipeline = try device.makeComputePipelineState(function: function)
let count = 4096, guardCount = 16
let poison: UInt64 = 0xdeadbeefcafebabe
let statusPoison: UInt32 = 0xdeadbeef
var random: UInt64 = 0xc371f1ed
func next() -> UInt64 {
    random = random &* 6364136223846793005 &+ 1442695040888963407
    return random
}
var a = [UInt64](), b = [UInt64](), coefficients = [UInt32]()
for i in 0..<count {
    var divisor = (UInt64(1 + i % 2046) << 52) | (UInt64(i & 15) << 48)
        | (next() & 0x0000ffffffffffff)
    if i % 31 == 0 { divisor &= 0x7ff0000000000000 }
    let numerator = (UInt64(1 + (17 * i) % 2046) << 52)
        | (next() & 0x000fffffffffffff) | (UInt64(i & 1) << 63)
    a.append(numerator); b.append(divisor)
    switch i % 4 {
    case 0: coefficients.append(contentsOf: [0, 0])
    case 1: coefficients.append(contentsOf: [UInt32.max, UInt32.max])
    case 2: coefficients.append(contentsOf: [UInt32(truncatingIfNeeded: next()), UInt32(truncatingIfNeeded: next())])
    default: coefficients.append(contentsOf: [1, 65536])
    }
}
func buffer<T>(_ values: [T]) -> MTLBuffer {
    values.withUnsafeBytes { bytes in
        guard let result = device.makeBuffer(bytes: bytes.baseAddress!, length: bytes.count) else {
            fatalError("Metal allocation failed")
        }
        return result
    }
}
let aBuffer = buffer(a), bBuffer = buffer(b), coefficientBuffer = buffer(coefficients)
for mode in UInt32(0)...UInt32(2) {
    let output = buffer([UInt64](repeating: poison, count: 2 * count + 2 * guardCount))
    let status = buffer([UInt32](repeating: statusPoison, count: count + 2 * guardCount))
    guard let command = queue.makeCommandBuffer(),
          let encoder = command.makeComputeCommandEncoder() else {
        fatalError("Metal command allocation failed")
    }
    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(aBuffer, offset: 0, index: 0)
    encoder.setBuffer(bBuffer, offset: 0, index: 1)
    encoder.setBuffer(coefficientBuffer, offset: 0, index: 2)
    encoder.setBuffer(output, offset: guardCount * 8, index: 3)
    encoder.setBuffer(status, offset: guardCount * 4, index: 4)
    var selectedMode = mode
    encoder.setBytes(&selectedMode, length: 4, index: 5)
    encoder.dispatchThreads(MTLSize(width: count, height: 1, depth: 1),
        threadsPerThreadgroup: MTLSize(width: min(64, pipeline.maxTotalThreadsPerThreadgroup), height: 1, depth: 1))
    encoder.endEncoding(); command.commit(); command.waitUntilCompleted()
    if let error = command.error { throw error }
    let values = output.contents().bindMemory(to: UInt64.self, capacity: 2 * count + 2 * guardCount)
    let flags = status.contents().bindMemory(to: UInt32.self, capacity: count + 2 * guardCount)
    for i in 0..<guardCount {
        guard values[i] == poison && values[guardCount + 2 * count + i] == poison,
              flags[i] == statusPoison && flags[guardCount + count + i] == statusPoison else {
            fatalError("buffer guard changed")
        }
    }
    var accepted = 0, repairs = 0
    for i in 0..<count {
        let expected = (Double(bitPattern: a[i]) / Double(bitPattern: b[i])).bitPattern
        guard values[guardCount + 2 * i] == expected,
              values[guardCount + 2 * i + 1] == expected,
              flags[guardCount + i] <= 3 else {
            fatalError("division mismatch at mode \(mode), case \(i)")
        }
        accepted += Int(flags[guardCount + i] & 1)
        repairs += Int((flags[guardCount + i] >> 1) & 1)
    }
    if mode == 0 {
        guard accepted == count && repairs > 0 else { fatalError("normal table path rejected or repair untested") }
    } else {
        guard accepted < count else { fatalError("corrupted coefficients never rejected") }
        if mode == 2 && accepted == 0 { fatalError("unchanged coefficient controls never accepted") }
    }
    print("mode=\(mode) cases=\(count) accepted=\(accepted) repairs=\(repairs) native_binary64=PASS poison_and_guards=PASS")
}
