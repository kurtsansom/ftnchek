	module foo
	integer a, b, c
	end module
	program bar
	use foo, only: my_a=>a, b
	implicit none
	a = 1
	my_a = b
	end

