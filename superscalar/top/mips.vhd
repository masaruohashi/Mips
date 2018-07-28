library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;

entity mips is -- single cycle MIPS processor
  port(clk, reset:                      in  STD_LOGIC;
       pc1, pc2, pc3:                   out STD_LOGIC_VECTOR(31 downto 0);
       instr:                           in  STD_LOGIC_VECTOR(31 downto 0);
       memwrite1_out, memwrite2_out, memwrite3_out: out STD_LOGIC;
       aluout1, aluout2, aluout3:       out STD_LOGIC_VECTOR(31 downto 0);
       writedata:                       out STD_LOGIC_VECTOR(31 downto 0);
       readdata1, readdata2, readdata3: in  STD_LOGIC_VECTOR(31 downto 0));
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
         new_item:          in  STD_LOGIC;  -- rs signals
         q_dst, qj, qk:     in  STD_LOGIC_VECTOR(2 downto 0);  -- rs signals
         cdb_q:             in  STD_LOGIC_VECTOR(2 downto 0); -- rs signals
         cdb_data:          in  STD_LOGIC_VECTOR(31 downto 0); -- rs signals
         memtoreg, memwrite:in  STD_LOGIC;
         pcsrc:             in  STD_LOGIC;
         alusrc, regdst:    in  STD_LOGIC;
         jump:              in  STD_LOGIC;
         op:                in  STD_LOGIC_VECTOR(2 downto 0); -- rs signal (before it was alucontrol)
         immsrc:            in  STD_LOGIC;
         zero:              out STD_LOGIC;
         vj, writedata:     in  STD_LOGIC_VECTOR(31 downto 0);
         pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
         instr:             in  STD_LOGIC_VECTOR(31 downto 0);
         aluout:            buffer STD_LOGIC_VECTOR(31 downto 0);
         readdata:          in  STD_LOGIC_VECTOR(31 downto 0);
         writereg:          out STD_LOGIC_VECTOR(4 downto 0);
         result:            out STD_LOGIC_VECTOR(31 downto 0);
         q_dst_out:         out STD_LOGIC_VECTOR(2 downto 0);
         op_sent:           out STD_LOGIC;
         memwrite_out:      out STD_LOGIC;
         rs_counter:        out UNSIGNED(2 downto 0));
  end component;
  component regfile
    port(clk: in  STD_LOGIC;
         we:  in  STD_LOGIC;                      -- we  -> write enable
         ra:  in  STD_LOGIC_VECTOR(4 downto 0);   -- ra  -> register a
         rb:  in  STD_LOGIC_VECTOR(4 downto 0);   -- rb  -> register b
         wa:  in  STD_LOGIC_VECTOR(4 downto 0);   -- wa  -> write address (register)
         wd:  in  STD_LOGIC_VECTOR(31 downto 0);  -- wd  -> write data
         rad: out STD_LOGIC_VECTOR(31 downto 0);  -- rad -> register a data
         rbd: out STD_LOGIC_VECTOR(31 downto 0)); -- rbd -> register b data
  end component;
  component reorder_buffer
    port(clk, reset:          in STD_LOGIC;
         alusrc:              in  STD_LOGIC;
         reg_dst, reg1, reg2: in STD_LOGIC_VECTOR(4 downto 0);
         cdb_data:            in STD_LOGIC_VECTOR(31 downto 0);
         cdb_q:               in STD_LOGIC_VECTOR(2 downto 0);
         q_dst, qj, qk:       out STD_LOGIC_VECTOR(2 downto 0);
         qj_data, qk_data:    out STD_LOGIC_VECTOR(31 downto 0);
         qj_valid, qk_valid:  out STD_LOGIC;
         reg_write_en:        out STD_LOGIC;
         reg_out:             out STD_LOGIC_VECTOR(4 downto 0);
         data_out:            out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component common_databus
    port (clk, reset:                      in STD_LOGIC;
          cdb_q1, cdb_q2, cdb_q3:          in STD_LOGIC_VECTOR(2 downto 0);
          cdb_data1, cdb_data2, cdb_data3: in STD_LOGIC_VECTOR(31 downto 0);
          alu1, alu2, alu3:                in STD_LOGIC; --this signal choose which data to out;
          cdb_q_out:                       out STD_LOGIC_VECTOR(2 downto 0);
          cdb_data_out:                    out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component selector
    port(sel1:       in  STD_LOGIC;
         sel2:       in  STD_LOGIC;
         sel3:       in  STD_LOGIC;
         d0, d1, d2: out STD_LOGIC);
  end component;
  component compare3
    port(a0, a1, a2: in  UNSIGNED(2 downto 0);
         s:          out STD_LOGIC_VECTOR(1 downto 0));
  end component;
  component mux2 generic(width: integer);
    port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
         s:      in  STD_LOGIC;
         y:      out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;

  signal memtoreg1, memtoreg2, memtoreg3, alusrc1, alusrc2, alusrc3: STD_LOGIC;
  signal memwrite1, memwrite2, memwrite3: STD_LOGIC;
  signal regdst1, regdst2, regdst3, regwrite1, regwrite2, regwrite3, regwrite: STD_LOGIC;
  signal jump1, jump2, jump3, pcsrc1, pcsrc2, pcsrc3: STD_LOGIC;
  signal immsrc1, immsrc2, immsrc3, zero1, zero2, zero3: STD_LOGIC;
  signal alucontrol1, alucontrol2, alucontrol3: STD_LOGIC_VECTOR(2 downto 0);
  signal writereg, reg_dst: STD_LOGIC_VECTOR(4 downto 0);
  signal vj: STD_LOGIC_VECTOR(31 downto 0);
  signal regdata, result1, result2, result3: STD_LOGIC_VECTOR(31 downto 0);
  signal rs_counter1, rs_counter2, rs_counter3: UNSIGNED(2 downto 0);
  signal sel: STD_LOGIC_VECTOR(1 downto 0);

  -- reservation stations signals
  signal new_item1, new_item2, new_item3: STD_LOGIC;
  signal qj, qk, q_dst: STD_LOGIC_VECTOR(2 downto 0);
  signal cdb_data1, cdb_data2, cdb_data3, cdb_data_broad: STD_LOGIC_VECTOR(31 downto 0);
  signal cdb_q1, cdb_q2, cdb_q3, cdb_q_broad: STD_LOGIC_VECTOR(2 downto 0);
  signal alu1, alu2, alu3: STD_LOGIC;
  signal rob_qj_data, rob_qk_data, rf_qj_data, rf_qk_data: STD_LOGIC_VECTOR(31 downto 0);
  signal rob_qj_valid, rob_qk_valid: STD_LOGIC;

begin
  rf: regfile port map(clk, regwrite, instr(25 downto 21), instr(20 downto 16),
                       writereg, regdata, rf_qj_data, rf_qk_data);

  muxqj: mux2 generic map (32) port map (rf_qj_data, rob_qj_data, rob_qj_valid, vj);
  muxqk: mux2 generic map (32) port map (rf_qk_data, rob_qk_data, rob_qk_valid, writedata);

  rob: reorder_buffer port map(clk, reset, alusrc1, reg_dst, instr(25 downto 21), instr(20 downto 16),
                               cdb_data_broad, cdb_q_broad, q_dst, qj, qk, rob_qj_data, rob_qk_data,
                               rob_qj_valid, rob_qk_valid, regwrite, writereg, regdata);

  cdb: common_databus port map(clk, reset, cdb_q1, cdb_q2, cdb_q3,
                               result1, result2, result3, alu1,
                               alu2, alu3, cdb_q_broad, cdb_data_broad);

  sel1: selector port map (memtoreg1, memwrite1, jump1, new_item1, new_item2, new_item3);

  -- processor 1 -- load/store instructions
  cont1: controller port map(instr(31 downto 26), instr(5 downto 0),
                             zero1, memtoreg1, memwrite1, pcsrc1, alusrc1,
                             regdst1, regwrite1, jump1, alucontrol1, immsrc1);

  dp1: datapath port map(clk, reset, new_item1, q_dst, qj, qk, cdb_q_broad, cdb_data_broad,
                         memtoreg1, memwrite1, pcsrc1, alusrc1, regdst1, jump1, alucontrol1,
                         immsrc1, zero1, vj, writedata, pc1, instr, aluout1,
                         readdata1, reg_dst, result1, cdb_q1, alu1, memwrite1_out, rs_counter1);

  -- processor 2 -- branch
  cont2: controller port map(instr(31 downto 26), instr(5 downto 0),
                             zero2, memtoreg2, memwrite2, pcsrc2, alusrc2,
                             regdst2, regwrite2, jump2, alucontrol2, immsrc2);

  dp2: datapath port map(clk, reset, new_item2, q_dst, qj, qk, cdb_q_broad, cdb_data_broad,
                         memtoreg2, memwrite2, pcsrc2, alusrc2, regdst2, jump2, alucontrol2,
                         immsrc2, zero2, vj, writedata, pc2, instr, aluout2,
                         readdata2, reg_dst, result2, cdb_q2, alu2, memwrite2_out, rs_counter2);

  -- processor 3 -- ula
  cont3: controller port map(instr(31 downto 26), instr(5 downto 0),
                             zero3, memtoreg3, memwrite3, pcsrc3, alusrc3,
                             regdst3, regwrite3, jump3, alucontrol3, immsrc3);

  dp3: datapath port map(clk, reset, new_item3, q_dst, qj, qk, cdb_q_broad, cdb_data_broad,
                         memtoreg3, memwrite3, pcsrc3, alusrc3, regdst3, jump3, alucontrol3,
                         immsrc3, zero3, vj, writedata, pc3, instr, aluout3,
                         readdata3, reg_dst, result3, cdb_q3, alu3, memwrite3_out, rs_counter3);
end;
