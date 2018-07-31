library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;

entity reservation_station is -- reservation station for alu
  generic(num_rs: integer);
  port(clk, reset, new_item:       in STD_LOGIC;
       branch_in, zerosrc_in:      in STD_LOGIC;
       start_speculative:          in STD_LOGIC;
       branch_end, branch_result:  in STD_LOGIC;
       pcbranch_in:                in STD_LOGIC_VECTOR(31 downto 0);
       memtoreg_in, memwrite_in:   in STD_LOGIC;
       q_dst, qj, qk, q_write:     in STD_LOGIC_VECTOR(2 downto 0);  -- tags: these datas are coming from register status table
       vj_in, vk_in, v_write_in:   in STD_LOGIC_VECTOR(31 downto 0); -- these datas are coming from register file
       op_in:                      in STD_LOGIC_VECTOR(2 downto 0);
       cdb_q:                      in STD_LOGIC_VECTOR(2 downto 0);  -- common data bus signals
       cdb_data:                   in STD_LOGIC_VECTOR(31 downto 0); -- common data bus signals
       finish_speculative:         out STD_LOGIC;
       q_dst_out, op_out:          out STD_LOGIC_VECTOR(2 downto 0); -- these datas are going to ALU
       vj_out, vk_out, v_write_out:out STD_LOGIC_VECTOR(31 downto 0); -- these datas are going to ALU
       op_sent:                    out STD_LOGIC;
       memtoreg_out, memwrite_out: out STD_LOGIC;
       branch_out, zerosrc_out:    out STD_LOGIC;
       pcbranch_out:               out STD_LOGIC_VECTOR(31 downto 0);
       counter:                    buffer UNSIGNED(2 downto 0));
end;

architecture behave of reservation_station is
  type entry is
    record
      busy:          STD_LOGIC;
      branch:        STD_LOGIC;
      zerosrc:       STD_LOGIC;
      memtoreg:      STD_LOGIC;
      memwrite:      STD_LOGIC;
      speculative:   STD_LOGIC;
      op:            STD_LOGIC_VECTOR(2 downto 0);
      q_dst, qj, qk: STD_LOGIC_VECTOR(2 downto 0);
      q_write:       STD_LOGIC_VECTOR(2 downto 0);
      vj, vk:        STD_LOGIC_VECTOR(31 downto 0);
      v_write:       STD_LOGIC_VECTOR(31 downto 0);
      pcbranch:      STD_LOGIC_VECTOR(31 downto 0);
    end record;

  type rs_array is array (num_rs - 1 downto 0) of entry;

  signal rs: rs_array;
  signal s_op_sent: STD_LOGIC := '0';  -- this is to control that we send only one op per time to ALU
  signal store_sent: STD_LOGIC := '0';  -- this is to control when we send an store operation
  signal s_q_dst_sent: STD_LOGIC_VECTOR(2 downto 0) := (others => '1');
  signal speculative: STD_LOGIC := '0'; -- set when the next instructions are speculative
  signal s_branch_out: STD_LOGIC := '0';
begin

  branch_out <= s_branch_out;
  op_sent <= s_op_sent and (not store_sent);

  process (clk)
  begin

    if (branch_end = '1') then
      l5: for i in 0 to num_rs - 1 loop
        if (branch_result = '1') then
          rs(i).busy <= '0';
          rs(i).op <= (others => '0');
          rs(i).q_dst <= (others => '1');
          rs(i).qj <= (others => '1');  -- 111 represents an empty q
          rs(i).qk <= (others => '1');  -- 111 represents an empty q
          rs(i).vj <= (others => '0');
          rs(i).vk <= (others => '0');
          rs(i).memtoreg <= '0';
          rs(i).memwrite <= '0';
          rs(i).speculative <= '0';
          rs(i).q_write <= (others => '1');
          rs(i).v_write <= (others => '0');
          rs(i).zerosrc <= '0';
          rs(i).branch <= '0';
          rs(i).pcbranch <= (others => '0');
        else
          rs(i).speculative <= '0';
        end if;
      end loop l5;
    end if;

    if (reset ='1') then
      l1: for i in 0 to num_rs - 1 loop
        rs(i).busy <= '0';
        rs(i).op <= (others => '0');
        rs(i).q_dst <= (others => '1');
        rs(i).qj <= (others => '1');  -- 111 represents an empty q
        rs(i).qk <= (others => '1');  -- 111 represents an empty q
        rs(i).vj <= (others => '0');
        rs(i).vk <= (others => '0');
        rs(i).memtoreg <= '0';
        rs(i).memwrite <= '0';
        rs(i).speculative <= '0';
        rs(i).q_write <= (others => '1');
        rs(i).v_write <= (others => '0');
        rs(i).zerosrc <= '0';
        rs(i).branch <= '0';
        rs(i).pcbranch <= (others => '0');
        counter <= (others => '0');
        q_dst_out <= (others => '1');
        op_out <= (others => '0');
        vj_out <= (others => '0');
        vk_out <= (others => '0');
        v_write_out <= (others => '0');
        memtoreg_out <= '0';
        memwrite_out <= '0';
        zerosrc_out <= '0';
        s_branch_out <= '0';
        pcbranch_out <= (others => '0');
      end loop l1;
      speculative <= '0';
    elsif (clk'event and clk = '1') then

      if (start_speculative = '1') then
        speculative <= '1';
      end if;

      -- add an instruction to an free rs
      l2: for i in 0 to num_rs - 1 loop
        if (rs(i).busy = '0' and new_item = '1') then
          rs(i).busy <= '1';
          rs(i).op <= op_in;
          rs(i).vj <= vj_in;
          rs(i).vk <= vk_in;
          rs(i).q_dst <= q_dst;
          rs(i).qj <= qj;
          rs(i).qk <= qk;
          rs(i).memtoreg <= memtoreg_in;
          rs(i).memwrite <= memwrite_in;
          rs(i).speculative <= speculative;
          rs(i).q_write <= q_write;
          rs(i).v_write <= v_write_in;
          rs(i).branch <= branch_in;
          rs(i).zerosrc <= zerosrc_in;
          rs(i).pcbranch <= pcbranch_in;
          counter <= counter + 1;
          exit;
        end if;
      end loop l2;

      l3: for i in 0 to num_rs - 1 loop
        --send data to ALU
        -- if all datas are ready and we don't have any data being calculated at FU
        if (rs(i).busy = '1' and rs(i).qj = "111" and rs(i).qk = "111" and s_op_sent = '0') then
          if (rs(i).memwrite = '1') then
            store_sent <= '1';
            q_dst_out <= (others => '1');
          else
            q_dst_out <= rs(i).q_dst;
          end if;
          op_out <= rs(i).op;
          vj_out <= rs(i).vj;
          vk_out <= rs(i).vk;
          rs(i).busy <= '0';
          s_op_sent <= '1';
          s_q_dst_sent <= rs(i).q_dst;
          memtoreg_out <= rs(i).memtoreg;
          memwrite_out <= rs(i).memwrite;
          rs(i).memtoreg <= '0';
          rs(i).memwrite <= '0';
          rs(i).speculative <= '0';
          zerosrc_out <= rs(i).zerosrc;
          s_branch_out <= rs(i).branch;
          pcbranch_out <= rs(i).pcbranch;
          rs(i).zerosrc <= '0';
          rs(i).branch <= '0';
          rs(i).pcbranch <= (others => '0');
          v_write_out <= rs(i).v_write;
          counter <= counter - 1;
        end if;
      end loop l3;

      if (s_branch_out = '1') then
        finish_speculative <= '1';
      else
        finish_speculative <= '0';
      end if;

      -- clear op_sent if instruction already executed
      if ((cdb_q = s_q_dst_sent and s_q_dst_sent /= "111") or store_sent = '1') then
        s_q_dst_sent <= (others => '1');
        s_op_sent <= '0';
        memtoreg_out <= '0';
        memwrite_out <= '0';
        zerosrc_out <= '0';
        s_branch_out <= '0';
        pcbranch_out <= (others => '0');
      end if;

    end if;

    l4: for i in 0 to num_rs - 1 loop
      -- snoop for cdb
      if (rs(i).busy = '1') then
        if (rs(i).qj = cdb_q and cdb_q /= "111") then  -- if there is a required data on cdb
          rs(i).vj <= cdb_data;
          rs(i).qj <= "111"; -- clear qj
        end if;
        if (rs(i).qk = cdb_q and cdb_q /= "111") then  -- if there is a required data on cdb
          rs(i).vk <= cdb_data;
          rs(i).qk <= "111"; -- clear qk
        end if;
        if (rs(i).q_write = cdb_q and cdb_q /= "111") then
          rs(i).v_write <= cdb_data;
          rs(i).q_write <= "111";
        end if;
      end if;
    end loop l4;
  end process;
end;
