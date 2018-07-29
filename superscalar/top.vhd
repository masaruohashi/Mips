library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
  port(clk, reset: in  STD_LOGIC;
       pc        : out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture test of top is
  component mips
    port(clk, reset:                      in  STD_LOGIC;
         pc1, pc2, pc3:                   out STD_LOGIC_VECTOR(31 downto 0);
         instr:                           in  STD_LOGIC_VECTOR(31 downto 0);
         memwrite1_out, memwrite2_out, memwrite3_out: out STD_LOGIC;
         aluout1, aluout2, aluout3:       out STD_LOGIC_VECTOR(31 downto 0);
         v_write_out:                       out STD_LOGIC_VECTOR(31 downto 0);
         readdata1, readdata2, readdata3: in  STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component imem
    port(a:  in  STD_LOGIC_VECTOR(5 downto 0);
         rd: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component dmem
    port(clk, we:  in STD_LOGIC;
         a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
         rd:       out STD_LOGIC_VECTOR(31 downto 0));
  end component;

  signal pc1, pc2, pc3, instr,
         readdata1, readdata2, readdata3: STD_LOGIC_VECTOR(31 downto 0);
  signal memwrite1, memwrite2, memwrite3: STD_LOGIC;
  signal writedata: STD_LOGIC_VECTOR(31 downto 0);
  signal dataadr1, dataadr2, dataadr3: STD_LOGIC_VECTOR(31 downto 0);

begin
  -- instantiate processor and memories
  mips1: mips port map(clk, reset, pc1, pc2, pc3, instr,
                       memwrite1, memwrite2, memwrite3,
                       dataadr1, dataadr2, dataadr3,
                       writedata, readdata1, readdata2,
                       readdata3);
  imem1: imem port map(pc1(7 downto 2), instr);
  dmem1: dmem port map(clk, memwrite1, dataadr1, writedata, readdata1);

  pc <= pc1;
end;
