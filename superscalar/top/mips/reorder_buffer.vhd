library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity reorder_buffer is -- reorder buffer
  port(clk, reset:          in STD_LOGIC;
       reg_dst, reg1, reg2: in STD_LOGIC_VECTOR(4 downto 0);
       cdb_data:            in STD_LOGIC_VECTOR(31 downto 0);
       cdb_q:               in STD_LOGIC_VECTOR(2 downto 0);
       q_dst, qj, qk:       out STD_LOGIC_VECTOR(2 downto 0);
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
      end loop l1;
    elsif (clk'event and clk = '1') then

      -- insert new instruction in rob
      if (s_counter /= 6) then
        if (tail = 6) then
          tail <= (others => '0');
        else
          tail <= tail + 1;
        end if;
        rob(to_integer(unsigned(tail))).reg_dst <= reg_dst;
        rob(to_integer(unsigned(tail))).valid <= '0';
        s_counter <= s_counter + 1;
      end if;


      l2: for i in 0 to 6 loop
        qj <= (others => '1');  -- 111 indicates that there is no dependencies
        qk <= (others => '1');
        if (rob(i).reg_dst = reg1 and reg1 /= "00000") then
          qj <= std_logic_vector(to_unsigned(i, qj'length));
        end if;
        if (rob(i).reg_dst = reg2 and reg2 /= "00000") then
          qk <= std_logic_vector(to_unsigned(i, qk'length));
        end if;
      end loop l2;

      -- increment header
      if (rob(to_integer(unsigned(head))).valid = '1') then
        s_counter <= s_counter - 1;
        rob(to_integer(unsigned(head))).reg_dst <= (others => '0');
        rob(to_integer(unsigned(head))).data <= (others => '0');
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


  end process;
end;
