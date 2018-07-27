library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regfile is -- nine-port register file (for 3-way superscalar)
  port(clk:           in  STD_LOGIC;
       we1, we2, we3: in  STD_LOGIC;                      -- we  -> write enable
       ra:            in  STD_LOGIC_VECTOR(4 downto 0);   -- ra  -> register a
       rb:            in  STD_LOGIC_VECTOR(4 downto 0);   -- rb  -> register b
       wa1, wa2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);   -- wa  -> write address (register)
       wd1, wd2, wd3: in  STD_LOGIC_VECTOR(31 downto 0);  -- wd  -> write data
       rad:           out STD_LOGIC_VECTOR(31 downto 0);  -- rad -> register a data
       rbd:           out STD_LOGIC_VECTOR(31 downto 0)); -- rbd -> register b data
end;

architecture behave of regfile is
  type ramtype is array (31 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
  signal mem: ramtype;
begin
  -- three-ported register file
  -- read two ports combinationally
  -- write third port on rising edge of clock
  -- register 0 hardwired to 0
  -- note: for pipelined processor, write third port
  -- on falling edge of clk
  process(clk) begin
    if rising_edge(clk) then
       if we1 = '1' then mem(to_integer(wa1)) <= wd1;
       end if;

       if we2 = '1' then mem(to_integer(wa2)) <= wd2;
       end if;

       if we3 = '1' then mem(to_integer(wa3)) <= wd3;
       end if;
    end if;
  end process;
  process(all) begin
    -- reading register A data for first instruction
    if (to_integer(ra) = 0) then rad <= X"00000000"; -- register 0 holds 0
    else rad <= mem(to_integer(ra));
    end if;

    -- reading register B data for first instruction
    if (to_integer(rb) = 0) then rbd <= X"00000000"; -- register 0 holds 0
    else rbd <= mem(to_integer(rb));
    end if;
  end process;
end;
