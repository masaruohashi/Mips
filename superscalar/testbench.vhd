library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;
use IEEE.NUMERIC_STD.all;

entity testbench is
end;

architecture test of testbench is
  component top
  port(clk, reset: in  STD_LOGIC;
       pc        : out STD_LOGIC_VECTOR(31 downto 0));
  end component;

  signal clk, reset: STD_LOGIC;
  signal pc: STD_LOGIC_VECTOR(31 downto 0);
begin

  -- instantiate device to be tested
  dut: top port map(clk, reset, pc);

  -- Generate clock with 10 ns period
  process begin
    clk <= '1';
    wait for 5 ns;
    clk <= '0';
    wait for 5 ns;
  end process;

  -- Generate reset for first two clock cycles
  process begin
    reset <= '1';
    wait for 22 ns;
    reset <= '0';
    wait;
  end process;

  process (clk) begin
    if (clk'event and clk = '0' and pc = x"50") then
      report "NO ERRORS: Simulation succeeded" severity failure;
    end if;
  end process;
end;
