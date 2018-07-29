library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;

entity datapath is  -- MIPS datapath
  port(clk, reset:        in  STD_LOGIC;
       new_item:          in  STD_LOGIC;  -- flag to indicate we need to store in rs
       q_dst, qj, qk:     in  STD_LOGIC_VECTOR(2 downto 0);  -- tags of operands
       q_write:           in  STD_LOGIC_VECTOR(2 downto 0);  -- tag of the data to store in dmem
       cdb_q:             in  STD_LOGIC_VECTOR(2 downto 0);  -- tag coming from cdb
       cdb_data:          in  STD_LOGIC_VECTOR(31 downto 0); -- data coming from cdb
       memtoreg, memwrite:in  STD_LOGIC; --flags to indicate if it's load/store
       pcsrc:             in  STD_LOGIC;
       alusrc, regdst:    in  STD_LOGIC;
       jump:              in  STD_LOGIC;
       op:                in  STD_LOGIC_VECTOR(2 downto 0); -- rs signal (before it was alucontrol)
       immsrc:            in  STD_LOGIC;
       zero:              out STD_LOGIC;
       vj, vk, writedata: in  STD_LOGIC_VECTOR(31 downto 0); -- value of operands
       pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
       instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       aluout:            buffer STD_LOGIC_VECTOR(31 downto 0);
       readdata:          in  STD_LOGIC_VECTOR(31 downto 0);
       writereg:          out STD_LOGIC_VECTOR(4 downto 0);
       result:            out STD_LOGIC_VECTOR(31 downto 0); -- value to send to cdb
       q_dst_out:         out STD_LOGIC_VECTOR(2 downto 0); --tag to send to cdb
       op_sent:           out STD_LOGIC;
       memwrite_out:      out STD_LOGIC; -- flag to indicate we have to store in dmem
       v_write_out:       out STD_LOGIC_VECTOR(31 downto 0); --value to store in dmem
       rs_counter:        out UNSIGNED(2 downto 0));
end;

architecture struct of datapath is
  component reservation_station generic (num_rs: integer);
    port(clk, reset, new_item:       in STD_LOGIC;
         memtoreg_in, memwrite_in:   in STD_LOGIC;
         q_dst, qj, qk, q_write:     in STD_LOGIC_VECTOR(2 downto 0);  -- tags: these datas are coming from rob
         vj_in, vk_in, v_write_in:   in STD_LOGIC_VECTOR(31 downto 0); -- data of operands
         op_in:                      in STD_LOGIC_VECTOR(2 downto 0);
         cdb_q:                      in STD_LOGIC_VECTOR(2 downto 0);  -- common data bus signals
         cdb_data:                   in STD_LOGIC_VECTOR(31 downto 0); -- common data bus signals
         q_dst_out, op_out:          out STD_LOGIC_VECTOR(2 downto 0); -- these datas are going to ALU
         vj_out, vk_out, v_write_out:out STD_LOGIC_VECTOR(31 downto 0); -- these datas are going to ALU
         op_sent:                    out STD_LOGIC;
         memtoreg_out, memwrite_out: out STD_LOGIC;
         counter:                    buffer UNSIGNED(2 downto 0));
  end component;
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
  signal srcb, srca, vk_in: STD_LOGIC_VECTOR(31 downto 0);
  signal alucontrol: STD_LOGIC_VECTOR(2 downto 0);
  signal memtoreg_out: STD_LOGIC;
begin
  -- next PC logic
  pcjump <= pcplus4(31 downto 28) & instr(25 downto 0) & "00";
  pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc);
  pcadd1: adder port map(pc, X"00000004", pcplus4); --since we're reading three instruction per cycle
  immsht: sl2 port map(imm, immsh);
  pcadd2: adder port map(pcplus4, immsh, pcbranch);
  pcbrmux: mux2 generic map(32) port map(pcplus4, pcbranch,
                                         pcsrc, pcnextbr);
  pcmux: mux2 generic map(32) port map(pcnextbr, pcjump, jump, pcnext);

  wrmux: mux2 generic map(5) port map(instr(20 downto 16),
                                      instr(15 downto 11),
                                      regdst, writereg);
  resmux: mux2 generic map(32) port map(aluout, readdata,
                                        memtoreg_out, result);
  se: signext port map(instr(15 downto 0), signimm);
  us: unsignext port map(instr(15 downto 0), unsignimm);

  immux: mux2 generic map(32) port map(signimm, unsignimm, immsrc, imm);

  -- reservation station
  srcbmux: mux2 generic map(32) port map(vk, imm, alusrc,
                                         vk_in);

  rs: reservation_station generic map(2) port map(clk, reset, new_item, memtoreg, memwrite, q_dst, qj, qk, q_write,
                                                  vj, vk_in, writedata, op, cdb_q, cdb_data, q_dst_out, alucontrol, srca,
                                                  srcb, v_write_out, op_sent, memtoreg_out, memwrite_out, rs_counter);

  -- ALU logic
  mainalu: alu port map(srca, srcb, alucontrol, aluout, zero);
end struct;
