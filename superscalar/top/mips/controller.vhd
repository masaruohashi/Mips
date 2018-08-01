library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is -- single cycle control decoder
  port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
       zero:               in  STD_LOGIC;
       memtoreg, memwrite: out STD_LOGIC;
       pcsrc, alusrc:      out STD_LOGIC;
       regdst, regwrite:   out STD_LOGIC;
       jump, branch:       out STD_LOGIC;
       alucontrol:         out STD_LOGIC_VECTOR(2 downto 0);
       zerosrc:            out STD_LOGIC;
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
  signal s_branch: STD_LOGIC;
  signal s_zerosrc: STD_LOGIC;
  signal mux_zero: STD_LOGIC;

begin
  md: maindec port map(op, memtoreg, memwrite, s_branch,
                       alusrc, regdst, regwrite, jump, aluop, s_zerosrc, immsrc);
  ad: aludec port map(funct, aluop, alucontrol);


  -- basicamente, um mux para poder implementar o bne - branch on not equal
  mux_zero <= zero when s_zerosrc = '0' else not zero;

  pcsrc <= s_branch and mux_zero;
  branch <= s_branch;
  zerosrc <= s_zerosrc;
end;
