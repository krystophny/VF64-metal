!
! /*============================================================================
!
! This C source file is part of the SoftFloat IEEE Floating-Point Arithmetic
! Package, Release 3e, by John R. Hauser.
!
! Copyright 2011, 2012, 2013, 2014, 2015, 2016 The Regents of the University of
! California.  All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
!  1. Redistributions of source code must retain the above copyright notice,
!     this list of conditions, and the following disclaimer.
!
!  2. Redistributions in binary form must reproduce the above copyright notice,
!     this list of conditions, and the following disclaimer in the documentation
!     and/or other materials provided with the distribution.
!
!  3. Neither the name of the University nor the names of its contributors may
!     be used to endorse or promote products derived from this software without
!     specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
! EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
! DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! =============================================================================*/
!
! Reciprocal constants and formula adapted from Berkeley SoftFloat Release 3e.
! https://github.com/ucb-bar/berkeley-softfloat-3/tree/07ebd29db0cfb55723a79fbbffe99103ef38e305
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
   integer :: count, used, u, v, b, seed, p
   integer :: rho, h, w, q, z, qp, residual, zp, refined, boundary, accepted
   integer :: zero, one, two, c, lower, four32, seven32, mask32
   integer :: mode, unit, root, k0, k1, eps, r0, sigma, sq, r32, raw_bad, p_raw, gap, j, k
   character(8) :: hexword
   integer(int64), parameter :: table(16, 2) = reshape([ &
       int(z'FFC4',int64), int(z'F0BE',int64), int(z'E363',int64), int(z'D76F',int64), &
       int(z'CCAD',int64), int(z'C2F0',int64), int(z'BA16',int64), int(z'B201',int64), &
       int(z'AA97',int64), int(z'A3C6',int64), int(z'9D7A',int64), int(z'97A6',int64), &
       int(z'923C',int64), int(z'8D32',int64), int(z'887E',int64), int(z'8417',int64), &
       int(z'F0F1',int64), int(z'D62C',int64), int(z'BFA1',int64), int(z'AC77',int64), &
       int(z'9C0A',int64), int(z'8DDB',int64), int(z'8185',int64), int(z'76BA',int64), &
       int(z'6D3B',int64), int(z'64D4',int64), int(z'5D5C',int64), int(z'56B1',int64), &
       int(z'50B6',int64), int(z'4B55',int64), int(z'4679',int64), int(z'4211',int64)], [16,2])
   logical :: ok
   call kernel_ir_allocate(ir, 256, 600, 4)
   count = 0; used = 0
   u = node(IR_SYMBOL, TYPE_UINT64, name='u')
   v = node(IR_SYMBOL, TYPE_UINT64, name='v')
   k0 = node(IR_SYMBOL, TYPE_UINT32, name='k0')
   k1 = node(IR_SYMBOL, TYPE_UINT32, name='k1')
   zero = literal64(0_int64); one = literal64(1_int64); two = literal64(2_int64)
   c = literal64(0_int64, 2147483648_int64)
   lower = literal64(2147483648_int64)
   four32 = node(IR_LITERAL, TYPE_UINT32, low=4_int64)
   seven32 = node(IR_LITERAL, TYPE_UINT32, low=31_int64)
   mask32 = node(IR_LITERAL, TYPE_UINT32, low=4294967288_int64)
   b = shr(v, 32)
   ! SoftFloat Release 3e reciprocal; the exact certificate admits any coefficients.
   eps = op(IR_BITAND, shr(cast(b, TYPE_UINT32), 11), node(IR_LITERAL, TYPE_UINT32, low=65535_int64))
   r0 = op(IR_SUB, k0, shr(op(IR_MUL, k1, eps), 20))
   sigma = op(IR_SUB, node(IR_LITERAL, TYPE_UINT32, low=4294967295_int64), &
       cast(shr(op(IR_MUL, cast(r0, TYPE_UINT64), b), 7), TYPE_UINT32))
   r32 = op(IR_ADD, shl(r0, 16), cast(shr(op(IR_MUL, cast(r0, TYPE_UINT64), &
       cast(sigma, TYPE_UINT64)), 24), TYPE_UINT32))
   sq = op(IR_MULHI, sigma, sigma)
   r32 = op(IR_ADD, r32, shr(op(IR_MULHI, r32, sq), 16))
   seed = cast(r32, TYPE_UINT64)
   p_raw = op(IR_MUL, b, seed)
   gap = op(IR_SUB, c, p_raw)
   raw_bad = both(compare(IR_LT, p_raw, c), compare(IR_GE, gap, shl(b, 1)))
   seed = select_value(raw_bad, op(IR_ADD, seed, one), seed)
   p = select_value(raw_bad, op(IR_ADD, p_raw, b), p_raw)
   accepted = both(compare(IR_LT, p_raw, c), &
       both(compare(IR_LT, p, c), compare(IR_LT, op(IR_SUB, c, p), shl(b, 1))))
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
   ir%outputs = [root, accepted, boundary, raw_bad]
   ir%nodes = ir%nodes(:count); ir%operands = ir%operands(:used)
   ir%n_nodes = count; ir%n_operands = used
   spec%name = str('soft_certified_division_leaf')
   spec%args = [str('u'), str('v'), str('k0'), str('k1')]
   spec%arg_types = [TYPE_UINT64, TYPE_UINT64, TYPE_UINT32, TYPE_UINT32]
   spec%outputs = [str('proxy'), str('accepted'), str('boundary'), str('repaired')]
   spec%output_types = [TYPE_UINT64, TYPE_BOOL, TYPE_BOOL, TYPE_BOOL]
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
      write (unit, '(a)') '/*'
      write (unit, '(a)') ''
      write (unit, '(a)') 'License for Berkeley SoftFloat Release 3e'
      write (unit, '(a)') ''
      write (unit, '(a)') 'John R. Hauser'
      write (unit, '(a)') '2018 January 20'
      write (unit, '(a)') ''
      write (unit, '(a)') 'The following applies to the whole of SoftFloat Release 3e as well as to'
      write (unit, '(a)') 'each source file individually.'
      write (unit, '(a)') ''
      write (unit, '(a)') 'Copyright 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 The Regents of the'
      write (unit, '(a)') 'University of California.  All rights reserved.'
      write (unit, '(a)') ''
      write (unit, '(a)') 'Redistribution and use in source and binary forms, with or without'
      write (unit, '(a)') 'modification, are permitted provided that the following conditions are met:'
      write (unit, '(a)') ''
      write (unit, '(a)') ' 1. Redistributions of source code must retain the above copyright notice,'
      write (unit, '(a)') '    this list of conditions, and the following disclaimer.'
      write (unit, '(a)') ''
      write (unit, '(a)') ' 2. Redistributions in binary form must reproduce the above copyright'
      write (unit, '(a)') '    notice, this list of conditions, and the following disclaimer in the'
      write (unit, '(a)') '    documentation and/or other materials provided with the distribution.'
      write (unit, '(a)') ''
      write (unit, '(a)') ' 3. Neither the name of the University nor the names of its contributors'
      write (unit, '(a)') '    may be used to endorse or promote products derived from this software'
      write (unit, '(a)') '    without specific prior written permission.'
      write (unit, '(a)') ''
      write (unit, '(a)') 'THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY'
      write (unit, '(a)') 'EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED'
      write (unit, '(a)') 'WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE'
      write (unit, '(a)') 'DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY'
      write (unit, '(a)') 'DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES'
      write (unit, '(a)') '(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;'
      write (unit, '(a)') 'LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND'
      write (unit, '(a)') 'ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT'
      write (unit, '(a)') '(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF'
      write (unit, '(a)') 'THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.'
      write (unit, '(a)') ''
      write (unit, '(a)') '*/'
      write (unit, '(a)') '#ifndef VF64_CERTIFIED_DIVISION_INCLUDED'
      write (unit, '(a)') '#define VF64_CERTIFIED_DIVISION_INCLUDED'
      do k = 1, 2
         if (mode == 1) then
            write (unit, '(a,i1,a)') 'constant ushort soft_reciprocal_k', k-1, '[16] = {'
         else
            write (unit, '(a,i1,a)') 'static constexpr unsigned short soft_reciprocal_k', k-1, '[16] = {'
         end if
         do j = 1, 16
            write (hexword, '(z4.4)') table(j,k)
            write (unit, '(a)', advance='no') '0x'//trim(hexword)//','
            if (mod(j,8) == 0) write (unit, '(a)') ''
         end do
         write (unit, '(a)') '};'
      end do
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
