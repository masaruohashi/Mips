-- mipssuperscalar.vhd
-- From Section 7.6 of Digital Design & Computer Architecture
-- Updated to VHDL 2008 26 July 2011 David_Harris@hmc.edu

library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

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

  -- check that 0xffff7f02 gets written to address 0x54(=84)  at end of program
  process (clk) begin
    if (clk'event and clk = '0' and pc = x"18") then
      report "NO ERRORS: Simulation succeeded" severity failure;
    end if;
  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
  port(clk, reset: in  STD_LOGIC;
       pc        : out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture test of top is
  component mips
    port(clk, reset:                         in  STD_LOGIC;
         pc1, pc2, pc3:                      out STD_LOGIC_VECTOR(31 downto 0);
         instr1, instr2, instr3:             in  STD_LOGIC_VECTOR(31 downto 0);
         memwrite1, memwrite2, memwrite3:    out STD_LOGIC;
         aluout1, aluout2, aluout3:          out STD_LOGIC_VECTOR(31 downto 0);
         writedata1, writedata2, writedata3: out STD_LOGIC_VECTOR(31 downto 0);
         readdata1, readdata2, readdata3:    in  STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component imem
    port(a:             in  STD_LOGIC_VECTOR(5 downto 0);
         rd1, rd2, rd3: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component dmem
    port(clk, we:  in STD_LOGIC;
         a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
         rd:       out STD_LOGIC_VECTOR(31 downto 0));
  end component;

  signal pc1, pc2, pc3, instr1, instr2, instr3,
         readdata1, readdata2, readdata3: STD_LOGIC_VECTOR(31 downto 0);
  signal memwrite1, memwrite2, memwrite3: STD_LOGIC;
  signal writedata1, writedata2, writedata3: STD_LOGIC_VECTOR(31 downto 0);
  signal dataadr1, dataadr2, dataadr3: STD_LOGIC_VECTOR(31 downto 0);

begin
  -- instantiate processor and memories
  mips1: mips port map(clk, reset, pc1, pc2, pc3, instr1, instr2, instr3,
                       memwrite1, memwrite2, memwrite3,
                       dataadr1, dataadr2, dataadr3,
                       writedata1, writedata2, writedata3,
                       readdata1, readdata2, readdata3);
  imem1: imem port map(pc1(7 downto 2), instr1, instr2, instr3);
  dmem1: dmem port map(clk, memwrite1, dataadr1, writedata1, readdata1);

  pc <= pc1;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity dmem is -- data memory
  port(clk, we:  in STD_LOGIC;
       a, wd:    in STD_LOGIC_VECTOR(31 downto 0);
       rd:       out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of dmem is
begin
  process is
    type ramtype is array (63 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
    variable mem: ramtype;
  begin
    -- read or write memory
   loop
      if clk'event and clk = '1' then
          if (we = '1') then mem(to_integer(a(7 downto 2))) := wd;
          end if;
      end if;
      rd <= mem(to_integer(a(7 downto 2)));
      wait on clk, a;
  end loop;

  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity imem is -- instruction memory
  port(a:             in  STD_LOGIC_VECTOR(5 downto 0);
       rd1, rd2, rd3: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of imem is
begin
  process is
    file mem_file: TEXT;
    variable L: line;
    variable ch: character;
    variable i, index, result: integer;
    type ramtype is array (63 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
    variable mem: ramtype;
  begin
    -- initialize memory from file
    for i in 0 to 63 loop -- set all contents low
      mem(i) := (others => '0');
    end loop;
    index := 0;
    FILE_OPEN(mem_file,"/home/nishi/Poli/PCS3722/Mips/memfile.dat", READ_MODE);
    while not endfile(mem_file) loop
      readline(mem_file, L);
      result := 0;
      for i in 1 to 8 loop
        read(L, ch);
        if '0' <= ch and ch <= '9' then
            result := character'pos(ch) - character'pos('0');
        elsif 'a' <= ch and ch <= 'f' then
           result := character'pos(ch) - character'pos('a')+10;
        else report "Format error on line " & integer'image(index)
             severity error;
        end if;
        mem(index)(35-i*4 downto 32-i*4) :=to_std_logic_vector(result,4);
      end loop;
      index := index + 1;
    end loop;

    -- read memory
    loop
      rd1 <= mem(to_integer(a));
      rd2 <= mem(to_integer(a + 1));
      rd3 <= mem(to_integer(a + 2));
      wait on a;
    end loop;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mips is -- single cycle MIPS processor
  port(clk, reset:                         in  STD_LOGIC;
       pc1, pc2, pc3:                      out STD_LOGIC_VECTOR(31 downto 0);
       instr1, instr2, instr3:             in  STD_LOGIC_VECTOR(31 downto 0);
       memwrite1, memwrite2, memwrite3:    out STD_LOGIC;
       aluout1, aluout2, aluout3:          out STD_LOGIC_VECTOR(31 downto 0);
       writedata1, writedata2, writedata3: out STD_LOGIC_VECTOR(31 downto 0);
       readdata1, readdata2, readdata3:    in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of mips is
  component controller
    port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
         zero:               in  STD_LOGIC;
         memtoreg, memwrite: out STD_LOGIC;
         pcsrc, alusrc:      out STD_LOGIC;
         regdst, regwrite:   out STD_LOGIC;
         jump:               out STD_LOGIC;
         alucontrol:         out STD_LOGIC_VECTOR(2 downto 0);
         immsrc:             out STD_LOGIC);
  end component;
  component datapath
    port(clk, reset:        in  STD_LOGIC;
         memtoreg, pcsrc:   in  STD_LOGIC;
         alusrc, regdst:    in  STD_LOGIC;
         jump:              in  STD_LOGIC;
         alucontrol:        in  STD_LOGIC_VECTOR(2 downto 0);
         immsrc:            in  STD_LOGIC;
         zero:              out STD_LOGIC;
         srca, writedata:   in  STD_LOGIC_VECTOR(31 downto 0);
         pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
         instr:             in  STD_LOGIC_VECTOR(31 downto 0);
         aluout:            buffer STD_LOGIC_VECTOR(31 downto 0);
         readdata:          in  STD_LOGIC_VECTOR(31 downto 0);
         writereg:          out STD_LOGIC_VECTOR(4 downto 0);
         result:            out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component regfile
  port(clk:              in  STD_LOGIC;
       we1, we2, we3:    in  STD_LOGIC;
       ra1, ra2, ra3:    in  STD_LOGIC_VECTOR(4 downto 0);
       rb1, rb2, rb3:    in  STD_LOGIC_VECTOR(4 downto 0);
       wa1, wa2, wa3:    in  STD_LOGIC_VECTOR(4 downto 0);
       wd1, wd2, wd3:    in  STD_LOGIC_VECTOR(31 downto 0);
       rad1, rad2, rad3: out STD_LOGIC_VECTOR(31 downto 0);
       rbd1, rbd2, rbd3: out STD_LOGIC_VECTOR(31 downto 0));
  end component;

  signal memtoreg1, memtoreg2, memtoreg3, alusrc1, alusrc2, alusrc3: STD_LOGIC;
  signal regdst1, regdst2, regdst3, regwrite1, regwrite2, regwrite3: STD_LOGIC;
  signal jump1, jump2, jump3, pcsrc1, pcsrc2, pcsrc3: STD_LOGIC;
  signal immsrc1, immsrc2, immsrc3, zero1, zero2, zero3: STD_LOGIC;
  signal alucontrol1, alucontrol2, alucontrol3: STD_LOGIC_VECTOR(2 downto 0);
  signal writereg1, writereg2, writereg3: STD_LOGIC_VECTOR(4 downto 0);
  signal srca1, srca2, srca3: STD_LOGIC_VECTOR(31 downto 0);
  signal result1, result2, result3: STD_LOGIC_VECTOR(31 downto 0);

begin
rf: regfile port map(clk, regwrite1, regwrite2, regwrite3,                              -- write enables
                     instr1(25 downto 21), instr2(25 downto 21), instr3(25 downto 21),  -- register a
                     instr1(20 downto 16), instr2(20 downto 16), instr3(20 downto 16),  -- register b
                     writereg1, writereg2, writereg3,                                   -- write address
                     result1, result2, result3,                                         -- write data
                     srca1, srca2, srca3,                                               -- register a data
                     writedata1, writedata2, writedata3);                               -- register b data

  -- processor 1
  cont1: controller port map(instr1(31 downto 26), instr1(5 downto 0),
                             zero1, memtoreg1, memwrite1, pcsrc1, alusrc1,
                             regdst1, regwrite1, jump1, alucontrol1, immsrc1);

  dp1: datapath port map(clk, reset, memtoreg1, pcsrc1, alusrc1, regdst1,
                         jump1, alucontrol1, immsrc1, zero1, srca1, writedata1,
                         pc1, instr1, aluout1, readdata1, writereg1, result1);

  -- processor 2
  cont2: controller port map(instr2(31 downto 26), instr2(5 downto 0),
                             zero2, memtoreg2, memwrite2, pcsrc2, alusrc2,
                             regdst2, regwrite2, jump2, alucontrol2, immsrc2);

  dp2: datapath port map(clk, reset, memtoreg2, pcsrc2, alusrc2, regdst2,
                         jump2, alucontrol2, immsrc2, zero2, srca2, writedata2,
                         pc2, instr2, aluout2, readdata2, writereg2, result2);

  -- processor 3
  cont3: controller port map(instr3(31 downto 26), instr3(5 downto 0),
                             zero3, memtoreg3, memwrite3, pcsrc3, alusrc3,
                             regdst3, regwrite3, jump3, alucontrol3, immsrc3);

  dp3: datapath port map(clk, reset, memtoreg3, pcsrc3, alusrc3, regdst3,
                         jump3, alucontrol3, immsrc3, zero3, srca3, writedata3,
                         pc3, instr3, aluout3, readdata3, writereg3, result3);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is -- single cycle control decoder
  port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
       zero:               in  STD_LOGIC;
       memtoreg, memwrite: out STD_LOGIC;
       pcsrc, alusrc:      out STD_LOGIC;
       regdst, regwrite:   out STD_LOGIC;
       jump:               out STD_LOGIC;
       alucontrol:         out STD_LOGIC_VECTOR(2 downto 0);
       immsrc:             out STD_LOGIC);
end;


architecture struct of controller is
  component maindec
    port(op:                 in  STD_LOGIC_VECTOR(5 downto 0);
         memtoreg, memwrite: out STD_LOGIC;
         branch, alusrc:     out STD_LOGIC;
         regdst, regwrite:   out STD_LOGIC;
         jump:               out STD_LOGIC;
         aluop:              out STD_LOGIC_VECTOR(1 downto 0);
         zerosrc:            out STD_LOGIC;
         immsrc:             out STD_LOGIC);
  end component;
  component aludec
    port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
         aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
         alucontrol: out STD_LOGIC_VECTOR(2 downto 0));
  end component;
  signal aluop:  STD_LOGIC_VECTOR(1 downto 0);
  signal branch: STD_LOGIC;
  signal zerosrc: STD_LOGIC;
  signal mux_zero: STD_LOGIC;

begin
  md: maindec port map(op, memtoreg, memwrite, branch,
                       alusrc, regdst, regwrite, jump, aluop, zerosrc, immsrc);
  ad: aludec port map(funct, aluop, alucontrol);


  -- basicamente, um mux para poder implementar o bne - branch on not equal
  mux_zero <= zero when zerosrc = '0' else not zero;

  pcsrc <= branch and mux_zero;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity maindec is -- main control decoder
  port(op:                 in  STD_LOGIC_VECTOR(5 downto 0);
       memtoreg, memwrite: out STD_LOGIC;
       branch, alusrc:     out STD_LOGIC;
       regdst, regwrite:   out STD_LOGIC;
       jump:               out STD_LOGIC;
       aluop:              out STD_LOGIC_VECTOR(1 downto 0);
       zerosrc:            out STD_LOGIC;
       immsrc:             out STD_LOGIC);
end;

architecture behave of maindec is
  signal controls: STD_LOGIC_VECTOR(10 downto 0);
begin
  process(all) begin
    case op is
      when "000000" => controls <= "11000001000"; -- RTYPE
      when "100011" => controls <= "10100100000"; -- LW
      when "101011" => controls <= "00101000000"; -- SW
      when "000100" => controls <= "00010000100"; -- BEQ
      when "001000" => controls <= "10100000000"; -- ADDI
      when "000010" => controls <= "00000010000"; -- J
      when "001101" => controls <= "10100001101"; -- ORI
      when "000101" => controls <= "00010000110"; -- BNE
      when others   => controls <= "-----------"; -- illegal op
    end case;
  end process;

  (regwrite, regdst, alusrc, branch, memwrite,
   memtoreg, jump, aluop(1 downto 0), zerosrc, immsrc) <= controls;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity aludec is -- ALU control decoder
  port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
       aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
       alucontrol: out STD_LOGIC_VECTOR(2 downto 0));
end;

architecture behave of aludec is
begin
  process(all) begin
    case aluop is
      when "00" => alucontrol <= "010"; -- add (for lw/sw/addi)
      when "01" => alucontrol <= "110"; -- sub (for beq or bne)
      when "11" => alucontrol <= "001"; -- or (for ori)
      when others => case funct is      -- R-type instructions
                         when "100000" => alucontrol <= "010"; -- add
                         when "100010" => alucontrol <= "110"; -- sub
                         when "100100" => alucontrol <= "000"; -- and
                         when "100101" => alucontrol <= "001"; -- or
                         when "101010" => alucontrol <= "111"; -- slt
                         when others   => alucontrol <= "---"; -- ???
                     end case;
    end case;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;

entity datapath is  -- MIPS datapath
  port(clk, reset:        in  STD_LOGIC;
       memtoreg, pcsrc:   in  STD_LOGIC;
       alusrc, regdst:    in  STD_LOGIC;
       jump:              in  STD_LOGIC;
       alucontrol:        in  STD_LOGIC_VECTOR(2 downto 0);
       immsrc:            in  STD_LOGIC;
       zero:              out STD_LOGIC;
       srca, writedata:   in  STD_LOGIC_VECTOR(31 downto 0);
       pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
       instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       aluout:            buffer STD_LOGIC_VECTOR(31 downto 0);
       readdata:          in  STD_LOGIC_VECTOR(31 downto 0);
       writereg:          out STD_LOGIC_VECTOR(4 downto 0);
       result:            out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of datapath is
  component alu
    port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
         alucontrol: in  STD_LOGIC_VECTOR(2 downto 0);
         result:     buffer STD_LOGIC_VECTOR(31 downto 0);
         zero:       out STD_LOGIC);
  end component;
  component adder
    port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
         y:    out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component sl2
    port(a: in  STD_LOGIC_VECTOR(31 downto 0);
         y: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component signext
    port(a: in  STD_LOGIC_VECTOR(15 downto 0);
         y: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component unsignext
      port(a: in  STD_LOGIC_VECTOR(15 downto 0);
           y: out STD_LOGIC_VECTOR(31 downto 0));
    end component;
  component flopr generic(width: integer);
    port(clk, reset: in  STD_LOGIC;
         d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
         q:          out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component mux2 generic(width: integer);
    port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
         s:      in  STD_LOGIC;
         y:      out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  -- signal writereg:           STD_LOGIC_VECTOR(4 downto 0);
  signal pcjump, pcnext,
         pcnextbr, pcplus4,
         pcbranch:           STD_LOGIC_VECTOR(31 downto 0);
  signal signimm, unsignimm, imm, immsh: STD_LOGIC_VECTOR(31 downto 0);
  signal srcb: STD_LOGIC_VECTOR(31 downto 0);
begin
  -- next PC logic
  pcjump <= pcplus4(31 downto 28) & instr(25 downto 0) & "00";
  pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc);
  pcadd1: adder port map(pc, X"0000000C", pcplus4); --since we're reading three instruction per cycle
  immsht: sl2 port map(imm, immsh);
  pcadd2: adder port map(pcplus4, immsh, pcbranch);
  pcbrmux: mux2 generic map(32) port map(pcplus4, pcbranch,
                                         pcsrc, pcnextbr);
  pcmux: mux2 generic map(32) port map(pcnextbr, pcjump, jump, pcnext);

  -- -- register file logic
  -- rf: regfile port map(clk, regwrite, instr(25 downto 21),
  --                      instr(20 downto 16), writereg, result, srca,
	-- 			writedata);
  wrmux: mux2 generic map(5) port map(instr(20 downto 16),
                                      instr(15 downto 11),
                                      regdst, writereg);
  resmux: mux2 generic map(32) port map(aluout, readdata,
                                        memtoreg, result);
  se: signext port map(instr(15 downto 0), signimm);
  us: unsignext port map(instr(15 downto 0), unsignimm);

  immux: mux2 generic map(32) port map(signimm, unsignimm, immsrc, imm);

  -- ALU logic
  srcbmux: mux2 generic map(32) port map(writedata, imm, alusrc,
                                         srcb);
  mainalu: alu port map(srca, srcb, alucontrol, aluout, zero);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regfile is -- nine-port register file (for 3-way superscalar)
  port(clk:              in  STD_LOGIC;
       we1, we2, we3:    in  STD_LOGIC;                      -- we  -> write enable
       ra1, ra2, ra3:    in  STD_LOGIC_VECTOR(4 downto 0);   -- ra  -> register a
       rb1, rb2, rb3:    in  STD_LOGIC_VECTOR(4 downto 0);   -- rb  -> register b
       wa1, wa2, wa3:    in  STD_LOGIC_VECTOR(4 downto 0);   -- wa  -> write address (register)
       wd1, wd2, wd3:    in  STD_LOGIC_VECTOR(31 downto 0);  -- wd  -> write data
       rad1, rad2, rad3: out STD_LOGIC_VECTOR(31 downto 0);  -- rad -> register a data
       rbd1, rbd2, rbd3: out STD_LOGIC_VECTOR(31 downto 0)); -- rbd -> register b data
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
    if (to_integer(ra1) = 0) then rad1 <= X"00000000"; -- register 0 holds 0
    else rad1 <= mem(to_integer(ra1));
    end if;

    -- reading register A data for second instruction
    if (to_integer(ra2) = 0) then rad2 <= X"00000000";
    else rad2 <= mem(to_integer(ra2));
    end if;

    -- reading register A data for third instruction
    if (to_integer(ra3) = 0) then rad3 <= X"00000000";
    else rad3 <= mem(to_integer(ra3));
    end if;

    -- reading register B data for first instruction
    if (to_integer(rb1) = 0) then rbd1 <= X"00000000"; -- register 0 holds 0
    else rbd1 <= mem(to_integer(rb1));
    end if;

    -- reading register B data for second instruction
    if (to_integer(rb2) = 0) then rbd2 <= X"00000000";
    else rbd2 <= mem(to_integer(rb2));
    end if;

    -- reading register B data for third instruction
    if (to_integer(rb3) = 0) then rbd3 <= X"00000000";
    else rbd3 <= mem(to_integer(rb3));
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity adder is -- adder
  port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
       y:    out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of adder is
begin
  y <= a + b;
end;


library IEEE; use IEEE.STD_LOGIC_1164.all;

entity sl2 is -- shift left by 2
  port(a: in  STD_LOGIC_VECTOR(31 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of sl2 is
begin
  y <= a(29 downto 0) & "00";
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity signext is -- sign extender
  port(a: in  STD_LOGIC_VECTOR(15 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of signext is
begin
  y <= X"ffff" & a when a(15) else X"0000" & a;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity unsignext is -- unsign extender
  port(a: in  STD_LOGIC_VECTOR(15 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of unsignext is
begin
  y <= X"0000" & a;
end;


library IEEE; use IEEE.STD_LOGIC_1164.all;  use IEEE.STD_LOGIC_ARITH.all;

entity flopr is -- flip-flop with synchronous reset
  generic(width: integer);
  port(clk, reset: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopr is
begin
  process(clk, reset) begin
    if reset then  q <= (others => '0');
    elsif rising_edge(clk) then
      q <= d;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux2 is -- two-input multiplexer
  generic(width: integer);
  port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:      in  STD_LOGIC;
       y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux2 is
begin
  y <= d1 when s else d0;
end;


library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity alu is
  port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
       alucontrol: in  STD_LOGIC_VECTOR(2 downto 0);
       result:     buffer STD_LOGIC_VECTOR(31 downto 0);
       zero:       out STD_LOGIC);
end;

architecture behave of alu is
  signal condinvb, sum: STD_LOGIC_VECTOR(31 downto 0);
begin
  condinvb <= not b when alucontrol(2) else b;
  sum <= a + condinvb + alucontrol(2);

  process(all) begin
    case alucontrol(1 downto 0) is
      when "00"   => result <= a and b;
      when "01"   => result <= a or b;
      when "10"   => result <= sum;
      when "11"   => result <= (0 => sum(31), others => '0');
      when others => result <= (others => 'X');
    end case;
  end process;

  zero <= '1' when result = X"00000000" else '0';
end;
