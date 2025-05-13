module chp
  implicit none

  type :: generators
    integer :: n
    ! (2n+1) * n matrix
    ! while the (2n + 1)st row for x_table and z_table serve as scratchpad
    logical, dimension(:, :), allocatable :: x_table, z_table
    ! 2n vector
    logical, dimension(:), allocatable :: r_table
  end type

  type :: measure_result 
    logical :: m
  end type

  type pauli_string
    logical :: phase 
    integer :: n 
    logical, dimension(:), allocatable :: s
  end type 

contains
  subroutine say_hello
    print *, "Hello, chp!"
  end subroutine say_hello

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

  subroutine readout_at(g, i, p)
    type(generators), intent(in) :: g
    integer, intent(in) :: i 
    type(pauli_string), intent(inout) :: p 
    integer :: a 
    logical :: xia, zia
    p%n = g%n 

    do a=1, g%n 
      xia = g%x_table(i, a)
      zia = g%x_table(i, a)
      p%s(2 * (a - 1) + 1) = xia 
      p%s(2 * (a - 1) + 2) = zia
    end do 
    p%phase = g%r_table(i)
  end subroutine

  subroutine pauli_string2pauli_binary(p, pb)
    use iso_c_binding
    implicit none
    type(pauli_string), intent(in) :: p
    logical(c_bool), dimension(:) :: pb 
    integer :: i
    pb(1) = p%phase 
    do i=1, p%n 
      pb(2 * i) = p%s(2 * i - 1)
      pb(2 * i + 1) = p%s(2 * i)
    end do 
  end subroutine

  subroutine readout(g, pbs)
    use iso_c_binding
    implicit none
    type(generators), intent(in) :: g
    logical(c_bool), dimension(:,:) :: pbs 
    integer :: i 
    type(pauli_string) :: p

    do i=1, g%n 
      call readout_at(g, i, p)
      call pauli_string2pauli_binary(p, pbs(i, :))
    end do 
  end subroutine 

  ! subroutine prog(qubit_n, measure_n, gate_n, prog_encoding, measure_array, pauli_string)
  !   use iso_c_binding
  !   implicit none 
  !   integer(c_int), intent(in) :: qubit_n, measure_n, gate_n
  !   ! each gate occupy 3 int, the first int is the gate encoding, the second and third is the qubit encoding
  !   integer(c_int), intent(in), dimension(:) :: prog_encoding
  !   logical(c_bool), intent(inout), dimension(:) :: measure_array
  !   logical(c_bool), intent(inout), dimension(:) :: pauli_string


  ! end subroutine

end module chp
