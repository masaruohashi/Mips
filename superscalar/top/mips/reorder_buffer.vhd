library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity reorder_buffer is -- reorder buffer
  port(clk, reset:          in  STD_LOGIC;
       alusrc, memwrite:    in  STD_LOGIC;
       reg_dst, reg1, reg2: in  STD_LOGIC_VECTOR(4 downto 0);
       cdb_data:            in  STD_LOGIC_VECTOR(31 downto 0);
       cdb_q:               in  STD_LOGIC_VECTOR(2 downto 0);
       q_dst, qj, qk:       out STD_LOGIC_VECTOR(2 downto 0);
       q_write:             out STD_LOGIC_VECTOR(2 downto 0);
       qj_data, qk_data:    out STD_LOGIC_VECTOR(31 downto 0);
       write_data:          out STD_LOGIC_VECTOR(31 downto 0);
       qj_valid, qk_valid:  out STD_LOGIC;
       write_valid:         out STD_LOGIC;
       reg_write_en:        out STD_LOGIC;
       reg_out:             out STD_LOGIC_VECTOR(4 downto 0);
       data_out:            out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of reorder_buffer is

  type entry is
    record
      reg_dst: STD_LOGIC_VECTOR(4 downto 0);
      data:    STD_LOGIC_VECTOR(31 downto 0);
      valid:   STD_LOGIC;
    end record;

  type rob_array is array (7 downto 0) of entry;
  signal rob: rob_array;
  signal head, tail: UNSIGNED(2 downto 0) := (others => '0'); -- aponta para onde a ultima instrucao escrita
  signal s_full: STD_LOGIC := '0';
  signal s_counter: UNSIGNED(2 downto 0) := (others => '0');  --contador do numero de instrucoes no buffer

begin

  s_full <= '1' when s_counter = 6 else '0';

  process (clk, cdb_q)
  begin

    q_dst <= std_logic_vector(tail);

    if (reset = '1') then
      l1: for i in 0 to 6 loop
        rob(i).reg_dst <= (others => '0');
        rob(i).data <= (others => '0');
        rob(i).valid <= '0';
        head <= (others => '0');
        tail <= (others => '0');
        s_full <= '0';
        q_dst <= (others => '0');
        qj <= (others => '1');
        qk <= (others => '1');
        q_write <= (others => '1');
        qj_data <= (others => '0');
        qk_data <= (others => '0');
        write_data <= (others => '0');
        qj_valid <= '0';
        qk_valid <= '0';
        write_valid <= '0';
        reg_out <= (others => '0');
        data_out <= (others => '0');
      end loop l1;
    elsif (clk'event and clk = '1') then

      -- insert new instruction in rob
      if (s_counter /= 6) then
        if (tail = 6) then
          tail <= (others => '0');
        else
          tail <= tail + 1;
        end if;
        if (memwrite /= '1') then  --insert in rob only if it's not an store instruction
          rob(to_integer(unsigned(tail))).reg_dst <= reg_dst;
          rob(to_integer(unsigned(tail))).valid <= '0';
          s_counter <= s_counter + 1;
        end if;
      end if;

      reg_write_en <= rob(to_integer(unsigned(head))).valid;

      -- remove item from buffer if its ready and its the header
      if (rob(to_integer(unsigned(head))).valid = '1') then
        s_counter <= s_counter - 1;

        data_out <= rob(to_integer(unsigned(head))).data;
        rob(to_integer(unsigned(head))).data <= (others => '0');

        reg_out <= rob(to_integer(unsigned(head))).reg_dst;
        rob(to_integer(unsigned(head))).reg_dst <= (others => '0');

        rob(to_integer(unsigned(head))).valid <= '0';
        if (head = 6) then
          head <= (others => '0');
        else
          head <= head + 1;
        end if;
      end if;

      if (cdb_q /= "111") then
        --snoop for cbd
        rob(to_integer(unsigned(cdb_q))).data <= cdb_data;
        rob(to_integer(unsigned(cdb_q))).valid <= '1';
      end if;

    end if;

    -- here we're checking if any of the operands has a valid value in rob
    qj <= "111";
    qk <= "111";
    q_write <= "111";
    l2: for i in 0 to 6 loop
      if (rob(i).reg_dst = reg1 and reg1 /= "00000") then
        if (rob(i).valid /= '1') then
          qj <= std_logic_vector(to_unsigned(i, qj'length));
        end if;
        qj_data <= rob(i).data;
        qj_valid <= rob(i).valid;
      end if;
      if (rob(i).reg_dst = reg2 and reg2 /= "00000" and alusrc /= '1') then
        if (rob(i).valid /= '1') then
          qk <= std_logic_vector(to_unsigned(i, qk'length));
        end if;
        qk_data <= rob(i).data;
        qk_valid <= rob(i).valid;
      end if;
      if (rob(i).reg_dst = reg2 and reg2 /= "00000" and memwrite = '1') then
        if (rob(i).valid /= '1') then
          q_write <= std_logic_vector(to_unsigned(i, q_write'length));
        end if;
        write_data <= rob(i).data;
        write_valid <= rob(i).valid;
      end if;
    end loop l2;

  end process;
end;
