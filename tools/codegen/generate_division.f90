program generate_division
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use fortgen_kernel_ir
   use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_metal_device_ir, &
                                  emit_cuda_device_ir, PRECISION_REAL32
   use fortgen_string, only: str, str_t, chars
   implicit none
   type(kernel_ir_t) :: ir
   type(kernel_emit_spec_t) :: spec
   type(str_t) :: source
   character(:), allocatable :: message
   character(4096) :: path
   integer :: count, used, u, v, t, b, valid, seed, p0, neg, e, corr, p, trim_candidate
   integer :: rho, h, w, q, z, qp, residual, zp, refined, boundary, accepted
   integer :: zero, one, two, c, lower, upper, zero32, four32, seven32, mask32
   integer :: mode, unit, safe, root
   logical :: ok
   call kernel_ir_allocate(ir, 256, 600, 3)
   count = 0; used = 0
   u = node(IR_SYMBOL, TYPE_UINT64, name='u')
   v = node(IR_SYMBOL, TYPE_UINT64, name='v')
   t = node(IR_SYMBOL, TYPE_REAL32, name='estimate')
   zero = literal64(0_int64); one = literal64(1_int64); two = literal64(2_int64)
   c = literal64(0_int64, 2147483648_int64)
   lower = literal64(2147483648_int64); upper = literal64(4294967295_int64)
   zero32 = node(IR_LITERAL, TYPE_UINT32)
   four32 = node(IR_LITERAL, TYPE_UINT32, low=4_int64)
   seven32 = node(IR_LITERAL, TYPE_UINT32, low=31_int64)
   mask32 = node(IR_LITERAL, TYPE_UINT32, low=4294967288_int64)
   b = shr(v, 32)
   valid = both(compare(IR_GE, t, real_literal(0.0_real64)), &
                compare(IR_LT, t, real_literal(8589934592.0_real64)))
   ! Select a defined value before the eager conversion, even on invalid input.
   safe = select_value(valid, t, real_literal(0.0_real64))
   seed = cast(safe, TYPE_UINT64)
   seed = select_value(compare(IR_LT, seed, lower), lower, seed)
   seed = select_value(compare(IR_GT, seed, upper), upper, seed)
   p0 = op(IR_MUL, b, seed)
   neg = compare(IR_GT, p0, c)
   e = select_value(neg, op(IR_SUB, p0, c), op(IR_SUB, c, p0))
   corr = op(IR_BITOR, shl(op(IR_MULHI, seed, e), 1), shr(op(IR_MUL, seed, e), 63))
   seed = select_value(neg, op(IR_SUB, seed, corr), op(IR_ADD, seed, corr))
   p = op(IR_MUL, b, seed)
   trim_candidate = compare(IR_GE, p, c)
   seed = select_value(trim_candidate, op(IR_SUB, seed, one), seed)
   p = select_value(trim_candidate, op(IR_SUB, p, b), p)
   accepted = both(valid, both(compare(IR_LT, p, c), compare(IR_LT, op(IR_SUB, c, p), shl(b, 1))))
   ! Invalid candidates are masked before eager quotient calculations.
   rho = cast(op(IR_SUB, select_value(accepted, seed, lower), two), TYPE_UINT32)
   h = op(IR_MULHI, cast(shr(u, 32), TYPE_UINT32), rho)
   w = op(IR_SUB, shl(u, 28), op(IR_MUL, cast(h, TYPE_UINT64), shr(v, 3)))
   q = op(IR_ADD, op(IR_MULHI, cast(shr(w, 32), TYPE_UINT32), rho), four32)
   z = op(IR_ADD, shl(cast(h, TYPE_UINT64), 32), shl(cast(q, TYPE_UINT64), 4))
   boundary = compare(IR_LT, op(IR_BITAND, q, seven32), four32)
   qp = op(IR_BITAND, q, mask32)
   residual = op(IR_SUB, shl(w, 28), op(IR_MUL, cast(qp, TYPE_UINT64), shr(v, 3)))
   zp = op(IR_ADD, shl(cast(h, TYPE_UINT64), 32), shl(cast(qp, TYPE_UINT64), 4))
   refined = select_value(compare(IR_NE, shr(residual, 63), zero), &
                          op(IR_SUB, zp, literal64(128_int64)), &
                          select_value(compare(IR_EQ, residual, zero), zp, op(IR_ADD, zp, one)))
   root = select_value(boundary, refined, z)
   ir%outputs = [root, accepted, boundary]
   ir%nodes = ir%nodes(:count); ir%operands = ir%operands(:used)
   ir%n_nodes = count; ir%n_operands = used
   spec%name = str('soft_certified_division_leaf')
   spec%args = [str('u'), str('v'), str('estimate')]
   spec%arg_types = [TYPE_UINT64, TYPE_UINT64, TYPE_REAL32]
   spec%outputs = [str('proxy'), str('accepted'), str('boundary')]
   spec%output_types = [TYPE_UINT64, TYPE_BOOL, TYPE_BOOL]
   spec%precision = PRECISION_REAL32
   spec%policy%small_power_limit = 0
   spec%policy%fold_exact_constants = .false.
   spec%policy%eliminate_constant_divisions = .false.
   spec%policy%shape_fma = .false.
   spec%producer = str('VF64-metal')
   spec%generator = str('tools/codegen/generate_division.f90')
   spec%regenerate_command = str('See tools/codegen/README.md')
   do mode = 1, min(2, command_argument_count())
      call get_command_argument(mode, path)
      if (mode == 1) source = emit_metal_device_ir(ir, spec, ok, message)
      if (mode == 2) source = emit_cuda_device_ir(ir, spec, ok, message)
      if (.not. ok) then
         print '(a)', message
         error stop 1
      end if
      open (newunit=unit, file=trim(path), status='replace')
      write (unit, '(a)') '#ifndef VF64_CERTIFIED_DIVISION_INCLUDED'
      write (unit, '(a)') '#define VF64_CERTIFIED_DIVISION_INCLUDED'
      write (unit, '(a)') chars(source)
      write (unit, '(a)') '#endif'
      close (unit)
   end do
contains
   integer function node(tag, scalar, children, name, low, high, shift, value) result(id)
      integer, intent(in)::tag, scalar
      integer, intent(in), optional::children(:), shift
      character(*), intent(in), optional::name
      integer(int64), intent(in), optional::low, high
      real(real64), intent(in), optional::value
      count = count + 1; id = count
      ir%nodes(id)%operation = tag; ir%nodes(id)%scalar_type = scalar
      ir%nodes(id)%first_operand = used + 1
      if (present(children)) then
         ir%nodes(id)%n_operands = size(children)
         ir%operands(used + 1:used + size(children)) = children
         used = used + size(children)
      end if
      if (present(name)) ir%nodes(id)%name = str(name)
      if (present(low)) ir%nodes(id)%uint_lo = low
      if (present(high)) ir%nodes(id)%uint_hi = high
      if (present(shift)) ir%nodes(id)%shift = shift
      if (present(value)) ir%nodes(id)%value = value
   end function
   integer function literal64(low, high) result(id)
      integer(int64), intent(in)::low
      integer(int64), intent(in), optional::high
      id = node(IR_LITERAL, TYPE_UINT64, low=low, high=high)
   end function
   integer function real_literal(value) result(id)
      real(real64), intent(in)::value
      id = node(IR_LITERAL, TYPE_REAL32, value=value)
   end function
   integer function op(tag, a, b) result(id)
      integer, intent(in)::tag, a, b
      id = node(tag, ir%nodes(a)%scalar_type, [a, b])
   end function
   integer function compare(tag, a, b) result(id)
      integer, intent(in)::tag, a, b
      id = node(tag, TYPE_BOOL, [a, b])
   end function
   integer function both(a, b) result(id)
      integer, intent(in)::a, b
      id = node(IR_AND, TYPE_BOOL, [a, b])
   end function
   integer function select_value(cond, a, b) result(id)
      integer, intent(in)::cond, a, b
      id = node(IR_SELECT, ir%nodes(a)%scalar_type, [cond, a, b])
   end function
   integer function cast(a, scalar) result(id)
      integer, intent(in)::a, scalar
      id = node(IR_CAST, scalar, [a])
   end function
   integer function shr(a, shift) result(id)
      integer, intent(in)::a, shift
      id = node(IR_SHR, ir%nodes(a)%scalar_type, [a], shift=shift)
   end function
   integer function shl(a, shift) result(id)
      integer, intent(in)::a, shift
      id = node(IR_SHL, ir%nodes(a)%scalar_type, [a], shift=shift)
   end function
end program
