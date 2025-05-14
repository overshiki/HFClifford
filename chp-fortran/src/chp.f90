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

  subroutine hardmard(g, a)
    type(generators), intent(inout) :: g
    integer, intent(in) :: a
    integer :: i
    logical :: xia, zia, ri, res

    do i=1, (2 * g%n) 
      xia = g%x_table(i, a)
      zia = g%z_table(i, a)
      ri = g%r_table(i)

      res = ri .xor. (xia .and. zia)
      g%r_table(i) = res
      ! swap xia with zia
      g%x_table(i, a) = zia 
      g%z_table(i, a) = xia
    end do

  end subroutine

  subroutine phase(g, a)
    type(generators), intent(inout) :: g
    integer, intent(in) :: a
    integer :: i
    logical :: xia, zia, ri, res

    do i=1, (2 * g%n) 
      xia = g%x_table(i, a)
      zia = g%z_table(i, a)
      ri = g%r_table(i)
      res = ri .xor. (xia .and. zia)
      g%r_table(i) = res

      res = zia .xor. xia
      g%z_table(i, a) = res
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
    ! write (*, *) "init"
    ! write (*, *) "x table"
    ! write (*, *) g%x_table
    ! write (*, *) "z table"
    ! write (*, *) g%z_table
    ! write (*, *) "end"
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
    ! write (*, *) "readout_at"
    ! write (*, *) s
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
    ! write (*, *) "readout"
    ! write (*, *) "x table"
    ! write (*, *) g%x_table
    ! write (*, *) "z table"
    ! write (*, *) g%z_table
    ! write (*, *) "end"
  end subroutine 

  subroutine prog(qubit_n, gate_n, prog_encoding, prn, measure_array, mn, pauli_res, pln) bind(c, name="prog")
    use iso_c_binding
    implicit none 
    integer(c_int), value, intent(in) :: qubit_n, gate_n
    ! each gate occupy 3 int, the first int is the gate encoding, the second and third is the qubit encoding
    integer(c_int), value, intent(in) :: prn, mn, pln
    integer(c_int), intent(in) :: prog_encoding(prn)
    logical(c_bool), intent(inout) :: measure_array(mn)
    logical(c_bool), intent(inout) :: pauli_res(pln)

    type(generators) :: g 
    integer :: i, a, b, gate, measure_count
    type(measure_result) :: mr
    logical(c_bool), dimension(:,:), allocatable :: pbs
    integer :: start_slice, end_slice

    g%n = qubit_n
    allocate(g%x_table(2*qubit_n + 1, qubit_n))
    allocate(g%z_table(2*qubit_n + 1, qubit_n))
    allocate(g%r_table(2*qubit_n + 1))
    call init_empty(g)

    measure_count = 1
    do i=1, gate_n
      gate = prog_encoding(3 * (i - 1) + 1)
      ! 1 for hardmard
      if (gate .eq. 1) then 
        a = prog_encoding(3 * (i - 1) + 2)
        call hardmard(g, a)
      ! 2 for phase
      else if (gate .eq. 2) then 
        a = prog_encoding(3 * (i - 1) + 2)
        call phase(g, a)
      ! 3 for cnot 
      else if (gate .eq. 3) then 
        a = prog_encoding(3 * (i - 1) + 2)
        b = prog_encoding(3 * (i - 1) + 3)
        call cnot(g, a, b)
      ! 4 for measure 
      else if (gate .eq. 4) then 
        a = prog_encoding(3 * (i - 1) + 2)
        call measure(g, a, mr)
        measure_array(measure_count) = mr%m
        measure_count = measure_count + 1
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
