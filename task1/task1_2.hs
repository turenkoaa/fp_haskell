nod a b | modi == 0 = mini
		| otherwise = nod modi mini where
		maxi = max a b
		mini = min a b
		modi = maxi - mini

cos' x 0 = 1
cos' x n = (-1)^n * x^(2*n) / fromIntegral (product [1..2*n]) + cos' x (n-1)

existQsuare a b c = [] /= [d | d <- [b..c], a == d^2, a > 0]
