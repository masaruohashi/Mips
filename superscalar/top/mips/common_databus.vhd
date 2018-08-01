library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;

entity common_databus is
  port (clk, reset:                      in STD_LOGIC;
        cdb_q1, cdb_q2, cdb_q3:          in STD_LOGIC_VECTOR(2 downto 0); --tag coming from each alu
        cdb_data1, cdb_data2, cdb_data3: in STD_LOGIC_VECTOR(31 downto 0); -- data coming from each alu
        alu1, alu2, alu3:                in STD_LOGIC; --this signal choose which data to out;
        cdb_q_out:                       out STD_LOGIC_VECTOR(2 downto 0);  --tag common data bus choose
        cdb_data_out:                    out STD_LOGIC_VECTOR(31 downto 0)); --data common data bus choose
end;

architecture behave of common_databus is

    signal last: STD_LOGIC_VECTOR(1 downto 0) := "10"; -- signal to indicate the last alu to send the data
    signal output_sel: STD_LOGIC_VECTOR(1 downto 0) := "11"; --signal to select the alu to send data
begin
  process (clk, alu1, alu2, alu3)
  begin

    if (reset = '1') then
      last <= "10";
      output_sel <= "11";
      cdb_data_out <= (others => '0');
      cdb_q_out <= (others => '1');
    else
      -- these if's are for controling if there are more than one alu sending data
      if (alu1 = '1' and alu2 = '1' and alu3 = '1') then
        if (last = "00") then
          output_sel <= "01";
          last <= "01";
        elsif (last = "01") then
          output_sel <= "10";
          last <= "10";
        elsif (last = "10") then
          output_sel <= "00";
          last <= "00";
        end if;
      elsif (alu1 = '1' and alu2 = '1') then
        if (last = "00") then
          output_sel <= "01";
          last <= "01";
        elsif (last = "01" or last = "10") then
          output_sel <= "00";
          last <= "00";
        end if;
      elsif (alu1 = '1' and alu3 = '1') then
        if (last = "00" or last = "01") then
          output_sel <= "10";
          last <= "10";
        elsif (last = "10") then
          output_sel <= "00";
          last <= "00";
        end if;
      elsif (alu2 = '1' and alu3 = '1') then
        if (last = "00" or last = "10") then
          output_sel <= "01";
          last <= "01";
        elsif (last = "01") then
          output_sel <= "10";
          last <= "10";
        end if;
      elsif (alu1 = '1') then
        output_sel <= "00";
        last <= "00";
      elsif (alu2 = '1') then
        output_sel <= "01";
        last <= "01";
      elsif (alu3 = '1') then
        output_sel <= "10";
        last <= "10";
      else
        output_sel <= "11";
      end if;
    end if;
    cdb_data_out <= cdb_data1 when output_sel = "00" else
                    cdb_data2 when output_sel = "01" else
                    cdb_data3 when output_sel = "10" else
                    (others => '0');

    cdb_q_out <= cdb_q1 when output_sel = "00" else
                  cdb_q2 when output_sel = "01" else
                  cdb_q3 when output_sel = "10" else
                  (others => '1');
  end process;

end;
