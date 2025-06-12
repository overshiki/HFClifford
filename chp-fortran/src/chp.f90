module chp
  implicit none

  type :: generators
    integer :: n
    ! (2n+1) * n matrix
    ! while the (2n + 1)st row for x_table and z_table serve as scratchpad
    logical, dimension(:, :), allocatable :: x_table, z_table
    ! 2n+1 vector
    logical, dimension(:), allocatable :: r_table
  end type

  type :: measure_result 
    logical :: m
  end type

contains

  pure function unpack_gate_rep(gate_rep) result(res)
    logical, intent(in) :: gate_rep(24)
    logical :: res(8, 3)
    integer :: i 
    do i=1, 8 
      res(i, :) = gate_rep(((i-1)*3 + 1):i*3)
    end do 
  end function

  pure function logical2int(l) result(res)
    logical, intent(in) :: l 
    integer :: res 
    if (l) then 
      res = 1 
    else 
      res = 0
    end if
  end function

  pure function logical2number(ps, n) result(res)
    integer, intent(in) :: n
    logical, intent(in) :: ps(n) 
    integer :: res
    integer :: i 

    res = 0
    do i=1, n
      res = res + logical2int(ps(i)) * (2 ** (n - i))
    end do 

    ! in fortran, we start from 1
    res = res + 1
  end function 

  pure function get_gate_rep(gate_rep_pack, i) result(res)
    ! i for gate id 
    ! for example:
    ! 1 for hardmard 
    ! 2 for phase
    logical, intent(in) :: gate_rep_pack(:)
    integer, intent(in) :: i
    logical :: res(8, 3)
    integer :: i_start, i_end 
    i_start = (i - 1) * 24 + 1 
    i_end = i * 24
    res = unpack_gate_rep(gate_rep_pack(i_start:i_end))
  end function

  ! pure function get_hardmard_rep(gate_rep_pack) result(res)
  !   ! 1 for hardmard
  !   logical, intent(in) :: gate_rep_pack(:)
  !   logical :: res(8, 3)
  !   ! res = unpack_gate_rep(gate_rep_pack(1:24))
  !   res = get_gate_rep(gate_rep_pack, 1)
  ! end function

  ! pure function get_phase_rep(gate_rep_pack) result(res)
  !   ! 2 for hardmard
  !   logical, intent(in) :: gate_rep_pack(:)
  !   logical :: res(8, 3)
  !   ! res = unpack_gate_rep(gate_rep_pack(25:48))
  !   res = get_gate_rep(gate_rep_pack, 2)
  ! end function

  pure function single_lookup_x(xia, zia, ri, gate_rep) result(res)
    ! single qubit gate lookup
    logical, intent(in) :: xia, zia, ri
    logical, intent(in) :: gate_rep(8, 3)
    logical :: res
    res = gate_rep(logical2number([xia, zia, ri], 3), 1)
  end function

  pure function single_lookup_z(xia, zia, ri, gate_rep) result(res)
    ! single qubit gate lookup
    logical, intent(in) :: xia, zia, ri 
    logical, intent(in) :: gate_rep(8, 3)
    logical :: res
    res = gate_rep(logical2number([xia, zia, ri], 3), 2)
  end function

  pure function single_lookup_r(xia, zia, ri, gate_rep) result(res)
    ! single qubit gate lookup
    logical, intent(in) :: xia, zia, ri 
    logical, intent(in) :: gate_rep(8, 3)
    logical :: res
    res = gate_rep(logical2number([xia, zia, ri], 3), 3)
  end function

  ! subroutine hardmard(g, a, gate_rep)
  !   type(generators), intent(inout) :: g
  !   integer, intent(in) :: a
  !   logical, intent(in) :: gate_rep(8, 3)
  !   integer :: i
  !   logical :: xia, zia, ri, res

  !   do i=1, (2 * g%n) 
  !     xia = g%x_table(i, a)
  !     zia = g%z_table(i, a)
  !     ri = g%r_table(i)

  !     res = ri .xor. (xia .and. zia)
  !     g%r_table(i) = single_lookup_r(xia, zia, ri, gate_rep)
  !     ! res
  !     ! swap xia with zia
  !     g%x_table(i, a) = single_lookup_x(xia, zia, ri, gate_rep)
  !       ! zia 
  !     g%z_table(i, a) = single_lookup_z(xia, zia, ri, gate_rep)
  !       ! xia
  !   end do

  ! end subroutine

  subroutine encoded_single_gate(g, a, gate_rep)
    type(generators), intent(inout) :: g
    integer, intent(in) :: a
    logical, intent(in) :: gate_rep(8, 3)
    integer :: i
    logical :: xia, zia, ri, res

    do i=1, (2 * g%n) 
      xia = g%x_table(i, a)
      zia = g%z_table(i, a)
      ri = g%r_table(i)
      res = ri .xor. (xia .and. zia)
      g%r_table(i) = single_lookup_r(xia, zia, ri, gate_rep) 
      ! res

      ! res = zia .xor. xia
      ! g%z_table(i, a) = res
      g%x_table(i, a) = single_lookup_x(xia, zia, ri, gate_rep)
      g%z_table(i, a) = single_lookup_z(xia, zia, ri, gate_rep)
    end do

  end subroutine

  subroutine cnot(g, a, b)
    type(generators), intent(inout) :: g
    integer, intent(in) :: a, b
    integer :: i
    logical :: xia, zia, xib, zib, ri, res

    do i=1, (2 * g%n) 
      xia = g%x_table(i, a)
      zia = g%z_table(i, a)
      ri = g%r_table(i)
      xib = g%x_table(i, b)
      zib = g%z_table(i, b)

      res = ri .xor. (xia .and. zib .and. (xib .xor. zia .xor. .true.))
      g%r_table(i) = res

      res = xib .xor. xia 
      g%x_table(i, b) = res 

      res = zia .xor. zib 
      g%z_table(i, a) = res
    end do 

  end subroutine

  pure function g_func(x1, z1, x2, z2) result(res)
    integer, intent(in) :: x1, z1, x2, z2
    integer :: res 
    if ((x1 == 0) .and. (z1 == 0)) then
      res = 0
    end if

    if ((x1 == 1) .and. (z1 == 1)) then
      res = z2 - x2
    end if

    if ((x1 == 1) .and. (z1 == 0)) then
      res = z2 * (2 * x2 - 1)
    end if

    if ((x1 == 0) .and. (z1 == 1)) then
      res = x2 * (1 - 2 * z2)
    end if

  end function 

  subroutine row_sum(g, h, i)
    type(generators), intent(inout) :: g
    integer, intent(in) :: h, i
    integer :: temp, j
    integer :: xij, zij, xhj, zhj, rh, ri
    logical :: check
    logical :: xij_, zij_, xhj_, zhj_

    temp = 0
    do j=1, g%n
      xij = g%x_table(i, j)
      zij = g%z_table(i, j)
      xhj = g%x_table(h, j)
      zhj = g%z_table(h, j)
      temp = temp + g_func(xij, zij, xhj, zhj)
    end do 
    rh = g%r_table(h)
    ri = g%r_table(i)

    check = mod((2*rh + 2*ri + temp), 4) == 0
    if (check) then 
      !set rh=0
      g%r_table(h) = .false.
    else
      !set rh=1
      g%r_table(h) = .true.
    end if 

    do j=1, g%n
      xij_ = g%x_table(i, j)
      zij_ = g%z_table(i, j)
      xhj_ = g%x_table(h, j)
      zhj_ = g%z_table(h, j)
      g%x_table(h, j) = xij_ .xor. xhj_
      g%z_table(h, j) = zij_ .xor. zhj_
    end do 

  end subroutine

  pure function is_random_measure(g, a) result(res)
    ! check whether there exists a p \in {n+1, ..., 2n} such that x_pa == 1
    ! if yes, the measurement yield random result
    ! if no, the measurement outcome is determinate
    type(generators), intent(in) :: g
    integer, intent(in) :: a
    integer :: i
    logical :: res, xia

    res = .false.
    do i=(g%n + 1), (2 * g%n) 
      xia = g%x_table(i, a)
      res = res .or. xia
    end do 

  end function

  subroutine measure(g, a, mr)
    type(generators), intent(inout) :: g
    type(measure_result), intent(inout) :: mr
    integer, intent(in) :: a
    logical :: is_random 

    is_random = is_random_measure(g, a)
    if (is_random) then 
      call random_measure(g, a, mr)
    else 
      call determinate_measure(g, a, mr)
    end if 
  end 

  pure function find_first_p(g, a) result(p)
    ! if the measurement yield random result, then find the first p \in {n+1, ..., 2n} such that x_pa == 1
    type(generators), intent(in) :: g
    integer, intent(in) :: a
    integer :: i, p
    logical :: xia    

    p = 0 
    do i=(g%n + 1), (2 * g%n) 
      xia = g%x_table(i, a)
      if (xia) then 
        p = i 
      end if
    end do 

  end function

  subroutine random_measure(g, a, mr)
    type(generators), intent(inout) :: g
    type(measure_result), intent(inout) :: mr
    integer, intent(in) :: a
    integer :: i, p
    logical :: xia 
    real :: rnumber

    p = find_first_p(g, a)
    do i=(p+1), (2 * g%n)
      xia = g%x_table(i, a)
      if (xia) then 
        call row_sum(g, i, p)
      end if 
    end do 

    ! set pth row to be identically 0, except that rp is 0 or 1 with equal probability
    ! and zpa = 1
    do i=1, g%n 
      g%x_table(p, i) = .false.
      g%z_table(p, i) = .false.
    end do 
    g%z_table(p, a) = .true.
    call random_number(rnumber)
    if (rnumber > 0.5) then 
      g%r_table(p) = .false.
    else 
      g%r_table(p) = .true.
    end if 

    ! return rp as the measurement outcome
    mr%m = g%r_table(p)

  end subroutine

  subroutine init_empty(g)
    type(generators), intent(inout) :: g
    integer :: i 
    g%x_table = .false.
    g%z_table = .false.
    g%r_table = .false.
    do i=1, g%n 
      g%z_table(i, i) = .true.
      g%x_table(i + g%n, i) = .true.
    end do 
  end subroutine

  subroutine determinate_measure(g, a, mr)
    ! this measurement require we have (2n + 1)st row for x_table and z_table as scratchpad 
    type(generators), intent(inout) :: g
    type(measure_result), intent(inout) :: mr
    integer, intent(in) :: a
    integer :: i
    logical :: xia

    ! first, set (2n + 1)st row to be identically 0
    do i=1, g%n 
      g%x_table(2*g%n + 1, i) = .false.
      g%z_table(2*g%n + 1, i) = .false.
      g%r_table(2*g%n + 1) = .false.
    end do 

    ! second, call row_sum (2n + 1, i + n) for all i \in {1, ..., n} such that xia = 1
    do i=1, g%n 
      xia = g%x_table(i, a)
      if (xia) then 
        call row_sum(g, 2*g%n + 1, i + g%n)
      end if 
    end do 

    ! reuturn r(2n + 1) as the measurement outcome 
    mr%m = g%r_table(2*g%n + 1)

  end subroutine

  subroutine readout_at(g, i, s)
    ! readout the ith row
    use iso_c_binding
    implicit none
    type(generators), intent(in) :: g
    integer, intent(in) :: i 
    logical(c_bool), dimension(:) :: s
    integer :: a, pivot
    logical :: xia, zia
    s(1) = g%r_table(i)
    do a=1, g%n 
      xia = g%x_table(i, a)
      zia = g%z_table(i, a)
      pivot = 2 * (a - 1) + 2
      s(pivot) = xia 
      s(pivot + 1) = zia
    end do 
  end subroutine

  subroutine readout(g, pbs)
    use iso_c_binding
    implicit none
    type(generators), intent(in) :: g
    logical(c_bool), dimension(:,:) :: pbs 
    integer :: i 

    do i=1, g%n 
      call readout_at(g, i, pbs(i, :))
    end do 
  end subroutine 

  subroutine prog(qubit_n, gate_n, &
    prog_encoding, prn, measure_array, &
    mn, pauli_res, pln, gate_rep, repln, &
    measure_rand) bind(c, name="prog")
    use iso_c_binding
    implicit none 
    integer(c_int), value, intent(in) :: qubit_n, gate_n
    ! each gate occupy 3 int, the first int is the gate encoding, the second and third is the qubit encoding
    integer(c_int), value, intent(in) :: prn, mn, pln, repln
    integer(c_int), intent(in) :: prog_encoding(prn)
    logical(c_bool), intent(inout) :: measure_array(mn)
    logical(c_bool), intent(inout) :: measure_rand(mn)
    logical(c_bool), intent(inout) :: pauli_res(pln)
    logical(c_bool), intent(in) :: gate_rep(repln)
    logical :: grep(repln)

    type(generators) :: g 
    integer :: i, a, b, gate, measure_count
    type(measure_result) :: mr
    logical(c_bool), dimension(:,:), allocatable :: pbs
    integer :: start_slice, end_slice
    logical :: single_gate_rep(8, 3), is_random
    ! logical :: hardmard_rep(8, 3), phase_rep(8, 3), single_gate_rep(8, 3)

    do i=1, repln
      grep(i) = logical(gate_rep(i))
    end do 

    ! hardmard_rep = get_hardmard_rep(grep)
    ! phase_rep = get_phase_rep(grep)

    g%n = qubit_n
    allocate(g%x_table(2*qubit_n + 1, qubit_n))
    allocate(g%z_table(2*qubit_n + 1, qubit_n))
    allocate(g%r_table(2*qubit_n + 1))
    call init_empty(g)

    measure_count = 1
    do i=1, gate_n
      gate = prog_encoding(3 * (i - 1) + 1)
      ! ! 1 for hardmard
      ! if (gate .eq. 1) then 
      !   a = prog_encoding(3 * (i - 1) + 2)
      !   call encoded_single_gate(g, a, hardmard_rep)
      ! 2 for phase
      ! else if (gate .eq. 2) then 
      !   a = prog_encoding(3 * (i - 1) + 2)
      !   call encoded_single_gate(g, a, phase_rep)
      ! -1 for cnot 
      if (gate .eq. -1) then 
        a = prog_encoding(3 * (i - 1) + 2)
        b = prog_encoding(3 * (i - 1) + 3)
        call cnot(g, a, b)
      ! 0 for measure 
      else if (gate .eq. 0) then 
        a = prog_encoding(3 * (i - 1) + 2)
        measure_rand(measure_count) = is_random_measure(g, a)
        write (*, *) "qubit: ", a
        write (*, *) "measure_rand(measure_count): ", measure_rand(measure_count) 
        call measure(g, a, mr)
        measure_array(measure_count) = mr%m
        measure_count = measure_count + 1
      else if (gate >= 1) then 
        single_gate_rep = get_gate_rep(grep, gate)
        a = prog_encoding(3 * (i - 1) + 2)
        call encoded_single_gate(g, a, single_gate_rep)
      else 
        stop "error"
      end if 
    end do 

    allocate(pbs(g%n, 2 * qubit_n + 1))
    call readout(g, pbs)
    ! squeeze pbs into pauli_res
    do i=1, qubit_n
      start_slice = (2 * qubit_n + 1) * (i - 1) + 1
      end_slice = (2 * qubit_n + 1) * i
      pauli_res(start_slice:end_slice) = pbs(i,:)
    end do 
    ! write (*, *) pauli_res
  end subroutine

end module chp
