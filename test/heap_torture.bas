
n& = fre(0)
xfree

const n_strs% = 1000
const n_iters% = 10000

dim s$(n_strs)
dim s_check$(n_strs)

for i = 1 to n_strs
	s(i) = string(8 + random(10), chr(random(127)))
	s_check(i) = s(i)
next

for i = 1 to n_iters
	s(random(n_strs)) = s(random(n_strs)) & "abcdefghi"
next

xfree
