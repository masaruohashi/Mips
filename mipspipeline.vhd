-- mips.vhd
-- From Section 7.6 of Digital Design & Computer Architecture
-- Updated to VHDL 2008 26 July 2011 David_Harris@hmc.edu

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity testbench is
end;

architecture test of testbench is
  component top
    port(clk, reset:           in  STD_LOGIC;
         writedata, dataadr:   out STD_LOGIC_VECTOR(31 downto 0);
         memwrite:             out STD_LOGIC);
  end component;
  signal writedata, dataadr:    STD_LOGIC_VECTOR(31 downto 0);
  signal clk, reset,  memwrite: STD_LOGIC;
begin

  -- instantiate device to be tested
  dut: top port map(clk, reset, writedata, dataadr, memwrite);

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

  --check that FFFF7F02 gets written to address 84 at end of program
 process (clk) begin
   if (clk'event and clk = '0' and memwrite = '1') then
     if (to_integer(dataadr) = 54 and writedata = x"FFFF7F02") then 
       report "NO ERRORS: Simulation succeeded" severity failure;
     elsif (dataadr /= 80) then 
       report "Simulation failed" severity failure;
     end if;
   end if;
 end process;
end;

library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity top is -- top-level design for testing
  port(clk, reset:           in     STD_LOGIC;
       writedata, dataadr:   buffer STD_LOGIC_VECTOR(31 downto 0);
       memwrite:             buffer STD_LOGIC);
end;

architecture test of top is
  component mips 
    port(clk, reset:        in  STD_LOGIC;
         pc:                out STD_LOGIC_VECTOR(31 downto 0);
         instr:             in  STD_LOGIC_VECTOR(31 downto 0);
         memwrite:          out STD_LOGIC;
         aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
         readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
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
  signal pc, instr, 
         readdata: STD_LOGIC_VECTOR(31 downto 0);
begin
  -- instantiate processor and memories
  mips1: mips port map(clk, reset, pc, instr, memwrite, dataadr, 
                       writedata, readdata);
  imem1: imem port map(pc(7 downto 2), instr);
  dmem1: dmem port map(clk, memwrite, dataadr, writedata, readdata);
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
  port(a:  in  STD_LOGIC_VECTOR(5 downto 0);
       rd: out STD_LOGIC_VECTOR(31 downto 0));
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
    FILE_OPEN(mem_file,"C:\Xilinx\projects\mips\memtest.dat", READ_MODE);
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
      rd <= mem(to_integer(a));
      wait on a;
    end loop;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mips is -- single cycle MIPS processor
  port(clk, reset:        in  STD_LOGIC;
       pc:                out STD_LOGIC_VECTOR(31 downto 0);
       instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       memwrite:          buffer STD_LOGIC;
       aluout, writedata: out STD_LOGIC_VECTOR(31 downto 0);
       readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of mips is
  component controller
    port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
         zero:               in  STD_LOGIC;
         memtoreg, memwrite: out STD_LOGIC;
         pcsrc, alusrc:      out STD_LOGIC;
         branch:             out STD_LOGIC;
         regdst, regwrite:   out STD_LOGIC;
         jump:               out STD_LOGIC;
         alucontrol:         out STD_LOGIC_VECTOR(2 downto 0);
         immsrc:             out STD_LOGIC);
  end component;
  component datapath
  port(clk, reset:                      in  STD_LOGIC;
       memtoreg, pcsrc:                 in  STD_LOGIC;
       alusrc, regdst:                  in  STD_LOGIC;
       regwrite, jump:                  in  STD_LOGIC;
       memwrite:                        in  STD_LOGIC;
       alucontrol:                      in  STD_LOGIC_VECTOR(2 downto 0);
       immsrc:                          in  STD_LOGIC;
       forwardAE, forwardBE:            in  STD_LOGIC_VECTOR(1 downto 0);
       forwardAD, forwardBD:            in  STD_LOGIC;
       stallF, stallD:                  in  STD_LOGIC;
       flushE:                          in  STD_LOGIC;
       zero:                            out STD_LOGIC;
       pc:                              buffer STD_LOGIC_VECTOR(31 downto 0);
       instr:                           in  STD_LOGIC_VECTOR(31 downto 0);
       aluoutM, writedataM:             buffer STD_LOGIC_VECTOR(31 downto 0);
       readdata:                        in  STD_LOGIC_VECTOR(31 downto 0);
       rsE, rtE, rsD, rtD:              buffer STD_LOGIC_VECTOR(4 downto 0);
       regwriteM, regwriteW, regwriteE: buffer STD_LOGIC;
       writeregM, writeregW, writeregE: buffer STD_LOGIC_VECTOR(4 downto 0);
       memtoregE, memtoregM:            buffer STD_LOGIC;
       instrD:                          buffer STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component hazardunit is
    port(rsE, rtE, rsD, rtD:              in  STD_LOGIC_VECTOR(4 downto 0);
         regWriteM, regWriteW, regWriteE: in  STD_LOGIC;
         writeRegM, writeRegW, writeRegE: in  STD_LOGIC_VECTOR(4 downto 0);
         memToRegE, memToRegM:            in  STD_LOGIC;
         branchD:                         in  STD_LOGIC;
         forwardAE, forwardBE:            out STD_LOGIC_VECTOR(1 downto 0);
         forwardAD, forwardBD:            out STD_LOGIC;
         stallF, stallD, flushE:          out STD_LOGIC);
  end component;
  signal memtoreg, alusrc, regdst, regwrite, jump, pcsrc, immsrc: STD_LOGIC;
  signal zero: STD_LOGIC;
  signal alucontrol: STD_LOGIC_VECTOR(2 downto 0);
  signal forwardAE, forwardBE: STD_LOGIC_VECTOR(1 downto 0);
  signal forwardAD, forwardBD: STD_LOGIC;
  signal stallF, stallD, flushE: STD_LOGIC;
  signal rsE, rtE, rsD, rtD, writeregM, writeregW, writeregE: STD_LOGIC_VECTOR(4 downto 0);
  signal regwriteM, regwriteW, regwriteE, memtoregE, memtoregM, branchD: STD_LOGIC;
  signal instrD: STD_LOGIC_VECTOR(31 downto 0);

begin
  cont: controller port map(instrD(31 downto 26), instrD(5 downto 0),
                            zero, memtoreg, memwrite, pcsrc, alusrc, branchD,
                            regdst, regwrite, jump, alucontrol, immsrc);
  dp: datapath port map(clk, reset, memtoreg, pcsrc, alusrc, regdst,
                        regwrite, jump, memwrite, alucontrol, immsrc,
                        forwardAE, forwardBE, forwardAD, forwardBD,stallF, stallD, flushE,
                        zero, pc, instr, aluout, writedata, readdata,
                        rsE, rtE, rsD, rtD, regwriteM, regwriteW, regwriteE,
                        writeregM, writeregW, writeregE, memtoregE, memtoregM, instrD);
  hu: hazardunit port map(rsE, rtE, rsD, rtD, regwriteM, regwriteW, regwriteE,
                          writeregM, writeregW, writeregE, memtoregE, memtoregM,
                          branchD, forwardAE, forwardBE, forwardAD, forwardBD,
                          stallF, stallD, flushE);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is -- single cycle control decoder
  port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
       zero:               in  STD_LOGIC;
       memtoreg, memwrite: out STD_LOGIC;
       pcsrc, alusrc:      out STD_LOGIC;
       branch:             out STD_LOGIC;
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
  signal signal_branch: STD_LOGIC;
  signal zerosrc: STD_LOGIC;
  signal mux_zero: STD_LOGIC;
  
begin
  md: maindec port map(op, memtoreg, memwrite, signal_branch,
                       alusrc, regdst, regwrite, jump, aluop, zerosrc, immsrc);
  ad: aludec port map(funct, aluop, alucontrol);
  
  
  -- �, basicamente, um mux para poder implementar o bne - branch on not equal
  mux_zero <= zero when zerosrc = '0' else not zero; 
  
  pcsrc <= signal_branch and mux_zero;
  branch <= signal_branch;
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
port(clk, reset:                      in  STD_LOGIC;
     memtoreg, pcsrc:                 in  STD_LOGIC;
     alusrc, regdst:                  in  STD_LOGIC;
     regwrite, jump:                  in  STD_LOGIC;
     memwrite:                        in  STD_LOGIC;
     alucontrol:                      in  STD_LOGIC_VECTOR(2 downto 0);
     immsrc:                          in  STD_LOGIC;
     forwardAE, forwardBE:            in  STD_LOGIC_VECTOR(1 downto 0);
     forwardAD, forwardBD:            in  STD_LOGIC;
     stallF, stallD:                  in  STD_LOGIC;
     flushE:                          in  STD_LOGIC;
     zero:                            out STD_LOGIC;
     pc:                              buffer STD_LOGIC_VECTOR(31 downto 0);
     instr:                           in  STD_LOGIC_VECTOR(31 downto 0);
     aluoutM, writedataM:             buffer STD_LOGIC_VECTOR(31 downto 0);
     readdata:                        in  STD_LOGIC_VECTOR(31 downto 0);
     rsE, rtE, rsD, rtD:              buffer STD_LOGIC_VECTOR(4 downto 0);
     regwriteM, regwriteW, regwriteE: buffer STD_LOGIC;
     writeregM, writeregW, writeregE: buffer STD_LOGIC_VECTOR(4 downto 0);
     memtoregE, memtoregM:            buffer STD_LOGIC;
     instrD:                          buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of datapath is
  component alu
    port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
         alucontrol: in  STD_LOGIC_VECTOR(2 downto 0);
         result:     buffer STD_LOGIC_VECTOR(31 downto 0);
         zero:       out STD_LOGIC);
  end component;
  component regfile
    port(clk:           in  STD_LOGIC;
         we3:           in  STD_LOGIC;
         ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
         wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
         rd1, rd2:      out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component if_id_reg
    port(clk, enable, reset: in  STD_LOGIC;
         flush:              in  STD_LOGIC;
         if_pc:              in  STD_LOGIC_VECTOR(31 downto 0);
         if_instr:           in  STD_LOGIC_VECTOR(31 downto 0);
         id_pc:              out STD_LOGIC_VECTOR(31 downto 0);
         id_instr:           out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component id_ex_reg
    port(clk, reset:               in  STD_LOGIC;
         flush:                    in  STD_LOGIC;
         id_regwrite, id_memtoreg: in  STD_LOGIC;
         id_memwrite, id_alusrc:   in  STD_LOGIC;
         id_regdst, id_immsrc:     in  STD_LOGIC;
         id_alucontrol:            in  STD_LOGIC_VECTOR(2 downto 0);
         id_rd1, id_rd2:           in  STD_LOGIC_VECTOR(31 downto 0);
         id_signimm, id_unsignimm: in  STD_LOGIC_VECTOR(31 downto 0);
         id_rs, id_rt, id_rd:      in  STD_LOGIC_VECTOR(4  downto 0);
         ex_regwrite, ex_memtoreg: out STD_LOGIC;
         ex_memwrite, ex_alusrc:   out STD_LOGIC;
         ex_regdst, ex_immsrc:     out STD_LOGIC;
         ex_alucontrol:            out STD_LOGIC_VECTOR(2 downto 0);
         ex_rd1, ex_rd2:           out STD_LOGIC_VECTOR(31 downto 0);
         ex_signimm, ex_unsignimm: out STD_LOGIC_VECTOR(31 downto 0);
         ex_rs, ex_rt, ex_rd:      out STD_LOGIC_VECTOR(4  downto 0));
  end component;
  component ex_mem_reg
  port(clk, reset:    in  STD_LOGIC;
       ex_regwrite:   in  STD_LOGIC;
       ex_memtoreg:   in  STD_LOGIC;
       ex_memwrite:   in  STD_LOGIC;
       ex_aluout:     in  STD_LOGIC_VECTOR(31 downto 0);
       ex_writereg:   in  STD_LOGIC_VECTOR(4  downto 0);
       ex_writedata:  in  STD_LOGIC_VECTOR(31 downto 0);
       mem_regwrite:  out STD_LOGIC;
       mem_memtoreg:  out STD_LOGIC;
       mem_memwrite:  out STD_LOGIC;
       mem_aluout:    out STD_LOGIC_VECTOR(31 downto 0);
       mem_writereg:  out STD_LOGIC_VECTOR(4  downto 0);
       mem_writedata: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component mem_wb_reg
  port(clk, reset:   in  STD_LOGIC;
       mem_regwrite: in  STD_LOGIC;
       mem_memtoreg: in  STD_LOGIC;
       mem_aluout:   in  STD_LOGIC_VECTOR(31 downto 0);
       mem_readdata: in  STD_LOGIC_VECTOR(31 downto 0);
       mem_writereg: in  STD_LOGIC_VECTOR(4  downto 0);
       wb_regwrite:  out STD_LOGIC;
       wb_memtoreg:  out STD_LOGIC;
       wb_aluout:    out STD_LOGIC_VECTOR(31 downto 0);
       wb_readdata:  out STD_LOGIC_VECTOR(31 downto 0);
       wb_writereg:  out STD_LOGIC_VECTOR(4  downto 0));
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
    port(clk, reset, enable: in  STD_LOGIC;
         d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
         q:          out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component mux2 generic(width: integer);
    port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
         s:      in  STD_LOGIC;
         y:      out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component mux3 generic(width: integer);
    port(d0, d1, d2: in  STD_LOGIC_VECTOR(width-1 downto 0);
        s:          in  STD_LOGIC_VECTOR(1 downto 0);
        y:          out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component compare generic(width: integer);
    port (a, b: in STD_LOGIC_VECTOR(width - 1 DOWNTO 0) ;
          equal: out STD_LOGIC);
  end component;
  signal memwriteE, alusrcE, regdstE, immsrcE: STD_LOGIC;
  signal alucontrolE: STD_LOGIC_VECTOR(2 downto 0);
  signal memwriteM: STD_LOGIC;
  signal memtoregW: STD_LOGIC;
  signal pcjump, pcnext, 
         pcnextbr, pcplus4F, pcplus4D, 
         pcbranch:          STD_LOGIC_VECTOR(31 downto 0);
  signal signimmD, signimmE, unsignimmD, unsignimmE, imm, immsh: STD_LOGIC_VECTOR(31 downto 0);
  signal srcaE, srcbE, resultW: STD_LOGIC_VECTOR(31 downto 0);
  signal rd1D, rd2D, rd1E, rd2E: STD_LOGIC_VECTOR(31 downto 0);
  signal aluoutE, aluoutW, readdataW, writedataE: STD_LOGIC_VECTOR(31 downto 0);
  signal rdE: STD_LOGIC_VECTOR (4 downto 0);
  signal operand1, operand2: STD_LOGIC_VECTOR(31 downto 0);
begin
  -- next PC logic
  pcjump <= pcplus4D(31 downto 28) & instrD(25 downto 0) & "00";
  pcreg: flopr generic map(32) port map(clk, reset, not(stallF), pcnext, pc);
  pcadd1: adder port map(pc, X"00000004", pcplus4F);
  immsht: sl2 port map(imm, immsh);
  pcadd2: adder port map(pcplus4D, immsh, pcbranch);
  pcbrmux: mux2 generic map(32) port map(pcplus4F, pcbranch, 
                                         pcsrc, pcnextbr);
  pcmux: mux2 generic map(32) port map(pcnextbr, pcjump, jump, pcnext);

  forward1mux: mux2 generic map(32) port map(rd1D, aluoutM, forwardAD, operand1);
  forward2mux: mux2 generic map(32) port map (rd2D, aluoutM, forwardBD, operand2);
  cp: compare generic map(32) port map(operand1, operand1, zero);
  -- register file logic
  rf: regfile port map(clk, regwriteW, instrD(25 downto 21), 
                       instrD(20 downto 16), writeregW, resultW, rd1D, 
				rd2D);
  wrmux: mux2 generic map(5) port map(rtE, rdE, regdstE, writeregE);
  resmux: mux2 generic map(32) port map(aluoutW, readdataW,  memtoregW, resultW);
  se: signext port map(instrD(15 downto 0), signimmD);
  us: unsignext port map(instrD(15 downto 0), unsignimmD);  
  
  immux: mux2 generic map(32) port map(signimmE, unsignimmE, immsrcE, imm);
  
  -- ALU logic
  srcamux: mux3 generic map(32) port map(rd1E, resultW, aluoutM, forwardAE, srcaE);

  writedatamux: mux3 generic map(32) port map(rd2E, resultW, aluoutM, forwardBE,
                                              writedataE);

  srcbmux: mux2 generic map(32) port map(writedataE, imm, alusrcE, srcbE);
  mainalu: alu port map(srcaE, srcbE, alucontrolE, aluoutE, open);

  --registers between stages in pipeline
  regIfId: if_id_reg port map(clk, not(stallD), reset, pcsrc, pcplus4F, instr, 
                              pcplus4D, instrD);
  regIdEx: id_ex_reg port map(clk, reset, flushE, regwrite, memtoreg, memwrite,
                              alusrc, regdst, immsrc, alucontrol,
                              rd1D, rd2D, signimmD, unsignimmD,
                              instrD(25 downto 21), instrD(20 downto 16), instrD(15 downto 11),
                              regwriteE, memtoregE, memwriteE,
                              alusrcE, regdstE, immsrcE, alucontrolE,
                              rd1E, rd2E, signimmE, unsignimmE,
                              rsE, rtE, rdE);
  regExMem: ex_mem_reg port map(clk, reset, regwriteE, memtoregE, memwriteE,
                                aluoutE, writeregE, writedataE, 
                                regwriteM, memtoregM, memwriteM, aluoutM, 
                                writeregM, writedataM);
  rexMemWB: mem_wb_reg port map(clk, reset, regwriteM, memtoregM, aluoutM, readdata, 
                                writeregM, regwriteW, memtoregW, aluoutW,
                                readdataW, writeregW);
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity regfile is -- three-port register file
  port(clk:           in  STD_LOGIC;
       we3:           in  STD_LOGIC;
       ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
       wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
       rd1, rd2:      out STD_LOGIC_VECTOR(31 downto 0));
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
    if falling_edge(clk) then
       if we3 = '1' then mem(to_integer(wa3)) <= wd3;
       end if;
    end if;
  end process;
  process(all) begin
    if (to_integer(ra1) = 0) then rd1 <= X"00000000"; -- register 0 holds 0
    else rd1 <= mem(to_integer(ra1));
    end if;
    if (to_integer(ra2) = 0) then rd2 <= X"00000000"; 
    else rd2 <= mem(to_integer(ra2));
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity if_id_reg is --IF/ID register (for first stage in pipeline)
  port(clk, enable, reset: in  STD_LOGIC;
       flush:              in  STD_LOGIC;
       if_pc:              in  STD_LOGIC_VECTOR(31 downto 0);
       if_instr:           in  STD_LOGIC_VECTOR(31 downto 0);
       id_pc:              out STD_LOGIC_VECTOR(31 downto 0);
       id_instr:           out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of if_id_reg is
begin
  process(clk)
  begin
    if reset then
      id_pc <= (others => '0');
      id_instr <= (others => '0');
    elsif rising_edge(clk) then
      if (flush) then
        id_pc <= (others => '0');
        id_instr <= (others => '0');
      elsif (enable) then
        id_pc <= if_pc;
        id_instr <= if_instr;
      end if;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity id_ex_reg is --ID/EX register (for second stage in pipeline)
  port(clk, reset:               in  STD_LOGIC;
       flush:                    in  STD_LOGIC;
       id_regwrite, id_memtoreg: in  STD_LOGIC;
       id_memwrite, id_alusrc:   in  STD_LOGIC;
       id_regdst, id_immsrc:     in  STD_LOGIC;
       id_alucontrol:            in  STD_LOGIC_VECTOR(2 downto 0);
       id_rd1, id_rd2:           in  STD_LOGIC_VECTOR(31 downto 0);
       id_signimm, id_unsignimm: in  STD_LOGIC_VECTOR(31 downto 0);
       id_rs, id_rt, id_rd:      in  STD_LOGIC_VECTOR(4  downto 0);
       ex_regwrite, ex_memtoreg: out STD_LOGIC;
       ex_memwrite, ex_alusrc:   out STD_LOGIC;
       ex_regdst, ex_immsrc:     out STD_LOGIC;
       ex_alucontrol:            out STD_LOGIC_VECTOR(2 downto 0);
       ex_rd1, ex_rd2:           out STD_LOGIC_VECTOR(31 downto 0);
       ex_signimm, ex_unsignimm: out STD_LOGIC_VECTOR(31 downto 0);
       ex_rs, ex_rt, ex_rd:      out STD_LOGIC_VECTOR(4  downto 0));
end;

architecture behave of id_ex_reg is
begin
  process(clk)
  begin
    if reset then
      ex_rd1 <= (others => '0');
      ex_rd2 <= (others => '0');
      ex_signimm <= (others => '0');
      ex_unsignimm <= (others => '0');
      ex_rs <= (others => '0');
      ex_rt <= (others => '0');
      ex_rd <= (others => '0');
      ex_immsrc <= '0';
      ex_alucontrol <= (others => '0');
      ex_regwrite <= '0';
      ex_memtoreg <= '0';
      ex_memwrite <= '0';
      ex_alusrc <= '0';
      ex_regdst <= '0';
    elsif rising_edge(clk) then
      if (flush) then
        ex_rd1 <= (others => '0');
        ex_rd2 <= (others => '0');
        ex_signimm <= (others => '0');
        ex_unsignimm <= (others => '0');
        ex_rs <= (others => '0');
        ex_rt <= (others => '0');
        ex_rd <= (others => '0');
        ex_immsrc <= '0';
        ex_alucontrol <= (others => '0');
        ex_regwrite <= '0';
        ex_memtoreg <= '0';
        ex_memwrite <= '0';
        ex_alusrc <= '0';
        ex_regdst <= '0';
      else 
        ex_rd1 <= id_rd1;
        ex_rd2 <= id_rd2;
        ex_signimm <= id_signimm;
        ex_unsignimm <= id_unsignimm;
        ex_rs <= id_rs;
        ex_rt <= id_rt;
        ex_rd <= id_rd;
        ex_immsrc <= id_immsrc;
        ex_alucontrol <= id_alucontrol;
        ex_regwrite <= id_regwrite;
        ex_memtoreg <= id_memtoreg;
        ex_memwrite <= id_memwrite;
        ex_alusrc <= id_alusrc;
        ex_regdst <= id_regdst;
      end if;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity ex_mem_reg is --EX/MEM register (for third stage in pipeline)
port(clk, reset:    in  STD_LOGIC;
     ex_regwrite:   in  STD_LOGIC;
     ex_memtoreg:   in  STD_LOGIC;
     ex_memwrite:   in  STD_LOGIC;
     ex_aluout:     in  STD_LOGIC_VECTOR(31 downto 0);
     ex_writereg:   in  STD_LOGIC_VECTOR(4  downto 0);
     ex_writedata:  in  STD_LOGIC_VECTOR(31 downto 0);
     mem_regwrite:  out STD_LOGIC;
     mem_memtoreg:  out STD_LOGIC;
     mem_memwrite:  out STD_LOGIC;
     mem_aluout:    out STD_LOGIC_VECTOR(31 downto 0);
     mem_writereg:  out STD_LOGIC_VECTOR(4  downto 0);
     mem_writedata: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of ex_mem_reg is
begin
  process(clk)
  begin
    if reset then 
      mem_regwrite <= '0';
      mem_memtoreg <= '0';
      mem_memwrite <= '0';
      mem_aluout <= (others => '0');
      mem_writereg <= (others => '0');
      mem_writedata <= (others => '0');
    elsif rising_edge(clk) then
      mem_regwrite <= ex_regwrite;
      mem_memtoreg <= ex_memtoreg;
      mem_memwrite <= ex_memwrite;
      mem_aluout <= ex_aluout;
      mem_writereg <= ex_writereg;
      mem_writedata <= ex_writedata;
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity mem_wb_reg is --MEM/WB (for fourth stage in pipeline)
  port(clk, reset:   in  STD_LOGIC;
       mem_regwrite: in  STD_LOGIC;
       mem_memtoreg: in  STD_LOGIC;
       mem_aluout:   in  STD_LOGIC_VECTOR(31 downto 0);
       mem_readdata: in  STD_LOGIC_VECTOR(31 downto 0);
       mem_writereg: in  STD_LOGIC_VECTOR(4  downto 0);
       wb_regwrite:  out STD_LOGIC;
       wb_memtoreg:  out STD_LOGIC;
       wb_aluout:    out STD_LOGIC_VECTOR(31 downto 0);
       wb_readdata:  out STD_LOGIC_VECTOR(31 downto 0);
       wb_writereg:  out STD_LOGIC_VECTOR(4  downto 0));
end;

architecture behave of mem_wb_reg is
begin 
  process(clk)
  begin
    if reset then
      wb_regwrite <= '0';
      wb_memtoreg <= '0';
      wb_aluout <= (others => '0');
      wb_readdata <= (others => '0');
      wb_writereg <= (others => '0');
    elsif rising_edge(clk) then
      wb_regwrite <= mem_regwrite;
      wb_memtoreg <= mem_memtoreg;
      wb_aluout <= mem_aluout;
      wb_readdata <= mem_readdata;
      wb_writereg <= mem_writereg;
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


library IEEE;use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.NUMERIC_STD_UNSIGNED.all; 

entity flopr is -- flip-flop with synchronous reset
  generic(width: integer);
  port(clk, reset, enable: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopr is
begin
  process(clk, reset) begin
    if reset then  q <= (others => '0');
    elsif rising_edge(clk) then
      if (enable) then
        q <= d;
      end if;
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

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux3 is -- three-input multiplexer
  generic(width: integer);
  port(d0, d1, d2: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:          in  STD_LOGIC_VECTOR(1 downto 0);
       y:          out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux3 is
begin
  process(s) begin
    case s is
      when "00" => y <= d0;
      when "01" => y <= d1;
      when "10" => y <= d2;
      when others => y <= (others => 'X');
    end case;
  end process;
end;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.std_logic_unsigned.all ;

entity compare is
  generic(width: integer);
  port (a, b: in STD_LOGIC_VECTOR(width - 1 DOWNTO 0) ;
        equal: out STD_LOGIC);
end;

architecture behave of compare is
begin
  equal <= '1' when a = b else '0';
end;


library IEEE; use IEEE.STD_LOGIC_1164.all;

entity datafowarding is -- Data Fowarding unit
  port(regaddr:              in  STD_LOGIC_VECTOR(4 downto 0);
       writeRegM, writeRegW: in  STD_LOGIC_VECTOR(4 downto 0);
       regWriteM, regWriteW: in  STD_LOGIC;
       forward:              out STD_LOGIC_VECTOR(1 downto 0));
end;

architecture behave of datafowarding is
begin
  process(all) begin
    if(regaddr /= "00000" and  regaddr = writeRegM and regWriteM = '1') then
      forward <= "10";
    elsif(regaddr /= "00000" and regaddr = writeRegW and regWriteW = '1') then
      forward <= "01";
    else
      forward <= "00";
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity hardwarestall is -- Hardware Stall unit
  port(rsD, rtE, rtD: in  STD_LOGIC_VECTOR(4 downto 0);
       memToRegE:     in  STD_LOGIC;
       stall:         out STD_LOGIC);
end;

architecture behave of hardwarestall is
begin
  process(all) begin
    if (memToRegE = '1') and ((rsD = rtE) or (rtD = rtE)) then
      stall <= '1';
    else
      stall <= '0';
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity branchstall is -- Hardware Stall unit
  port(rsD, rtD:             in  STD_LOGIC_VECTOR(4 downto 0);
       branchD:              in  STD_LOGIC;
       regwriteE:            in  STD_LOGIC;
       writeregE, writeregM: in  STD_LOGIC_VECTOR(4 downto 0);
       memToRegM:            in  STD_LOGIC;
       stall:                out STD_LOGIC);
end;

architecture behave of branchstall is
begin
  process(all) begin
    if (branchD = '1' and regwriteE = '1' and (writeregE = rsD or writeregE = rtD)) or (branchD = '1' and memtoregM = '1' and (writeRegM = rsD or writeregM = rtD)) then
      stall <= '1';
    else
      stall <= '0';
    end if;
  end process;
end;

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity hazardunit is
  port(rsE, rtE, rsD, rtD:              in  STD_LOGIC_VECTOR(4 downto 0);
       regWriteM, regWriteW, regWriteE: in  STD_LOGIC;
       writeRegM, writeRegW, writeRegE: in  STD_LOGIC_VECTOR(4 downto 0);
       memToRegE, memToRegM:            in  STD_LOGIC;
       branchD:                         in  STD_LOGIC;
       forwardAE, forwardBE:            out STD_LOGIC_VECTOR(1 downto 0);
       forwardAD, forwardBD:            out STD_LOGIC;
       stallF, stallD, flushE:          out STD_LOGIC);
end;

architecture struct of hazardunit is
  component datafowarding
    port(regaddr:              in  STD_LOGIC_VECTOR(4 downto 0);
         writeRegM, writeRegW: in  STD_LOGIC_VECTOR(4 downto 0);
         regWriteM, regWriteW: in  STD_LOGIC;
         forward:              out STD_LOGIC_VECTOR(1 downto 0));
  end component;
  component hardwarestall
    port(rsD, rtE, rtD: in  STD_LOGIC_VECTOR(4 downto 0);
         memToRegE:     in  STD_LOGIC;
         stall:         out STD_LOGIC);
  end component;
  component controlfowarding
    port(regaddr:   in  STD_LOGIC_VECTOR(4 downto 0);
         writeRegM: in  STD_LOGIC_VECTOR(4 downto 0);
         regWriteM: in  STD_LOGIC;
         forward:   out STD_LOGIC);
  end component;
  component branchstall
    port(rsD, rtD:             in  STD_LOGIC_VECTOR(4 downto 0);
          branchD:              in  STD_LOGIC;
          regwriteE:            in  STD_LOGIC;
          writeregE, writeregM: in  STD_LOGIC_VECTOR(4 downto 0);
          memToRegM:            in  STD_LOGIC;
          stall:                out STD_LOGIC);
  end component;
  signal lwstall, brstall: STD_LOGIC;
begin
  forwardingAE: datafowarding port map (rsE, writeRegM, writeRegW, regWriteM, regWriteW, forwardAE);
  forwardingBE: datafowarding port map (rtE, writeRegM, writeRegW, regWriteM, regWriteW, forwardBE);
  forwardingAD: controlfowarding port map (rsD, writeRegM, regWriteM, forwardAD);
  forwardingBD: controlfowarding port map (rtD, writeRegM, regWriteM, forwardBD);
  hadwarestalling: hardwarestall port map (rsD, rtE, rtD, memToRegE, lwstall);
  branchstalling: branchstall port map (rsD, rtD, branchD, regWriteE, writeRegE, writeRegM, memToRegM, brstall);
  stallF <= lwstall or brstall;
  stallD <= lwstall or brstall;
  flushE <= lwstall or brstall;
end;

