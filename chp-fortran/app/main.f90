program main
  use chp
  use iso_c_binding
  implicit none

  type(generators) :: g
  integer :: n = 10
  integer :: a = 2
  integer :: b = 4
  logical :: res

  integer(c_int) :: qubit_n = 10 
  integer(c_int) :: gate_n = 4
  integer(c_int), dimension(:), allocatable :: prog_encoding
  logical(c_bool), dimension(:), allocatable :: measure_array
  logical(c_bool), dimension(:), allocatable :: pauli_res

  g%n = n
  allocate(g%x_table(2*n,n))
  allocate(g%z_table(2*n,n))
  allocate(g%r_table(2*n))

  call hardmard(g, a)
  call phase(g, a)
  call cnot(g, a, b)
  res = is_random_measure(g, a)
  write (*, *) "is_random_measure:", res

  allocate(prog_encoding(gate_n * 3))
  prog_encoding = 0 
  ! H 2
  prog_encoding(1) = 1 
  prog_encoding(2) = 2 
  ! P 3
  prog_encoding(4) = 2
  prog_encoding(5) = 5 
  ! cnot 2 3 
  prog_encoding(7) = 3
  prog_encoding(8) = 2 
  prog_encoding(9) = 3 
  ! m 2 
  prog_encoding(10) = 4
  prog_encoding(11) = 2 

  allocate(measure_array(1))
  allocate(pauli_res((2 * qubit_n + 1) * qubit_n))

  call prog(qubit_n, gate_n, prog_encoding, measure_array, pauli_res)

end program main
