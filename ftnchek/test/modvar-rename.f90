	module foo
	integer a, b, c
	end module
	program bar
	use foo, my_a=>a, my_c=>c
	implicit none
	a = 1
	my_a = b
	end

