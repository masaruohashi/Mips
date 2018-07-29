library IEEE; use IEEE.STD_LOGIC_1164.all;

entity selector is
  port(sel1:       in  STD_LOGIC;
       sel2:       in  STD_LOGIC;
       sel3:       in  STD_LOGIC;
       d0, d1, d2: out STD_LOGIC);
end;

architecture behave of selector is
begin
  d0 <= '1' when (sel1 = '1' or sel2 = '1') and sel3 = '0' else '0';
  d1 <= '1' when sel1 = '0' and sel2 = '0' and sel3 = '1' else '0';
  d2 <= '1' when sel1 = '0' and sel2 = '0' and sel3 = '0' else '0';
end;
