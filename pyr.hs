--http://www.spoj.com/problems/PIR/
f a1 a2 a3 a4 a5 a6 = sqrt (((g a2 a3 a4 a6 a1 a5) + (g a1 a3 a4 a5 a2 a6) + (g a1 a2 a5 a6 a3 a4) - (h a1 a2 a4) - (h a2 a3 a5) - (h a1 a3 a6) - (h a4 a5 a6)) / 144)

g a b c d e f = (e^2)*(f^2)*((a^2) + (b^2) + (c^2) + (d^2) - (e^2) - (f^2))
h a b c = (a^2)*(b^2)*(c^2)
