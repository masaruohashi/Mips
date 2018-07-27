library IEEE; use IEEE.STD_LOGIC_1164.all;

entity selector is
  port(s:          in  STD_LOGIC_VECTOR(1 downto 0);
       d0, d1, d2: out STD_LOGIC);
end;

architecture behave of selector is
begin
  d0 <= '1' when s = "00" else '0';
  d1 <= '1' when s = "01" else '0';
  d2 <= '1' when s = "10" else '0';
end;
