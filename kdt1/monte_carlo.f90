implicit none

real(8), parameter :: pi = acos(-1.0_8)
integer :: n = 1000000, i
real(8) :: total, average, result
total = 0.0

do i = 1, n
    total = total + f(rand(0)*pi)
end do

average = total / n
result = average * pi

print *, "Estimated integral:", result

contains
    real(8) function f(x)
        real(8), intent(in) :: x
        f = cos(x)*cos(x)
    end function f

end