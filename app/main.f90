program main
  use chp
  implicit none

  type(generators) :: g
  integer :: n = 10
  integer :: a = 2
  integer :: b = 4
  logical :: res

  g%n = n
  allocate(g%x_table(2*n,n))
  allocate(g%z_table(2*n,n))
  allocate(g%r_table(2*n))

  call hardmard(g, a)
  call phase(g, a)
  call cnot(g, a, b)
  res = is_random_measure(g, a)
  call say_hello()
  write (*, *) "is_random_measure:", res
end program main
