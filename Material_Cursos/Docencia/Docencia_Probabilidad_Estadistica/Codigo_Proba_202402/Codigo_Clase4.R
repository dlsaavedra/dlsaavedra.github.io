S   = choose(12,6);S
A3  = choose(4,3)*choose(6,3) + choose(4,2)*choose(6,2)*choose(2,2);A3
A35 = choose(4,3)*choose(6,2)*choose(2,1);A35
A4  = choose(4,4)*choose(6,2) + choose(4,3)*choose(6,1)*choose(2,2);A4
A45 = choose(4,4)*choose(6,1)*choose(2,1);A45
A5 = choose(4,4)*choose(2,2);A5
(A3 + A35 + A4 + A45 + A5)/S

# Pauta
(choose(4,4)*choose(8,2)+choose(4,3)*choose(8,3)+choose(4,2)*choose(2,2)*choose(6,2))/choose(12,6)
