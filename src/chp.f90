module chp
  implicit none

  type :: generators
    integer :: n
    logical, dimension(:, :), allocatable :: x_table, z_table
    logical, dimension(:), allocatable :: r_table
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
end module chp
