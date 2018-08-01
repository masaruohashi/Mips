library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity reorder_buffer is -- reorder buffer
  port(clk, reset:          in  STD_LOGIC;
       alusrc, memwrite:    in  STD_LOGIC;
       branch_taken:        in  STD_LOGIC;
       op:                  in  STD_LOGIC_VECTOR(5 downto 0); -- instruction operation code
       reg_dst, reg1, reg2: in  STD_LOGIC_VECTOR(4 downto 0); -- operands of instruction
       cdb_data:            in  STD_LOGIC_VECTOR(31 downto 0); -- data coming from common data bus
       cdb_q:               in  STD_LOGIC_VECTOR(2 downto 0);  -- tag coming from common data bus
       q_dst, qj, qk:       out STD_LOGIC_VECTOR(2 downto 0);  -- tag found in reorder buffer for each operand
       q_write:             out STD_LOGIC_VECTOR(2 downto 0);
       qj_data, qk_data:    out STD_LOGIC_VECTOR(31 downto 0); -- data of respective tags in reoder buffer
       write_data:          out STD_LOGIC_VECTOR(31 downto 0);
       qj_valid, qk_valid:  out STD_LOGIC; -- flag indicating if the data in reorder buffer are valid
       write_valid:         out STD_LOGIC;
       reg_write_en:        out STD_LOGIC; --flag indicating we can send data to register file
       reg_out:             out STD_LOGIC_VECTOR(4 downto 0); -- register which we're going to write
       data_out:            out STD_LOGIC_VECTOR(31 downto 0)); -- data we're going to write on register
end;

architecture behave of reorder_buffer is

  type entry is
    record
      op:      STD_LOGIC_VECTOR(5 downto 0);
      reg_dst: STD_LOGIC_VECTOR(4 downto 0);
      data:    STD_LOGIC_VECTOR(31 downto 0);
      valid:   STD_LOGIC;
    end record;

  type rob_array is array (7 downto 0) of entry;
  signal rob: rob_array;
  signal head, tail: UNSIGNED(2 downto 0) := (others => '0'); -- head -> first instruction, tail -> last instruction
  signal s_full: STD_LOGIC := '0';
  signal s_counter: UNSIGNED(2 downto 0) := (others => '0');  -- count the number of entries in reorder buffer

begin

  s_full <= '1' when s_counter = 6 else '0';

  process (clk, cdb_q)
  begin

    -- we always put the new instruction on tail
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
      if (memwrite /= '1') then  --insert in rob only if it's not an store instruction
        if (s_counter /= 6) then -- and if it's not full
          if (tail = 6) then
            tail <= (others => '0');
          else
            tail <= tail + 1;
          end if;
          rob(to_integer(unsigned(tail))).op <= op;
          -- The instruction have a destination only if it isn't a branch instruction
          if (op /= "000100" and op /= "000101") then
            rob(to_integer(unsigned(tail))).reg_dst <= reg_dst;
          end if;
          rob(to_integer(unsigned(tail))).valid <= '0';
          s_counter <= s_counter + 1;
        end if;
      end if;

      -- we only send data to register file when the head is valid
      reg_write_en <= rob(to_integer(unsigned(head))).valid;

      -- remove item from buffer if its ready and its the header
      if (rob(to_integer(unsigned(head))).valid = '1') then
        s_counter <= s_counter - 1;

        -- send data if it isn't a branch instruction
        if (rob(to_integer(unsigned(head))).op /= "000100" and rob(to_integer(unsigned(head))).op /= "000101") then
          -- data to register file
          data_out <= rob(to_integer(unsigned(head))).data;
          -- register which we're going to write
          reg_out <= rob(to_integer(unsigned(head))).reg_dst;
        end if;

        -- clear ROB entry
        rob(to_integer(unsigned(head))).op <= (others => '0');
        rob(to_integer(unsigned(head))).data <= (others => '0');
        rob(to_integer(unsigned(head))).reg_dst <= (others => '0');
        rob(to_integer(unsigned(head))).valid <= '0';

        -- set new Head
        if (head = 6) then
          head <= (others => '0');
        else
          head <= head + 1;
        end if;
      end if;

      if (cdb_q /= "111") then
        if (rob(to_integer(unsigned(cdb_q))).op = "000100" or rob(to_integer(unsigned(cdb_q))).op = "000101") then
          if (branch_taken = '1') then
            l3: for i in 0 to 6 loop
              if((i < head and i > unsigned(cdb_q)) or (i > head and i > unsigned(cdb_q)) or (i < head and i > unsigned(cdb_q))) then
                rob(i).op <= (others => '0');
                rob(i).data <= (others => '0');
                rob(i).reg_dst <= (others => '0');
                rob(i).valid <= '0';
              end if;
            end loop;
            tail <= head + 1;
          end if;
        else
          --snoop for cbd
          rob(to_integer(unsigned(cdb_q))).data <= cdb_data;
        end if;
        rob(to_integer(unsigned(cdb_q))).valid <= '1';
      end if;

    end if;

    -- here we're checking if any of the operands has a valid value in rob
    -- and if not, we send the tag in which we can find the data later
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
