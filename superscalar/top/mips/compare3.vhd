library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;

entity compare3 is -- compare three numbers
  port(a0, a1, a2: in  UNSIGNED(2 downto 0);
       s:          out STD_LOGIC_VECTOR(1 downto 0));
end;

architecture behave of compare3 is
begin
   s <= "00" when a0 <= a1 and a0 <= a2 else
        "01" when a1 < a0  and a1 <= a2 else
        "10" when a2 < a0  and a2 < a1 else
        "00";
end;
