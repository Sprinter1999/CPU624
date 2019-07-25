library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity CPU624 is
    port (
				--***************************INPUT SIGNAL*************************--
        SWC, SWB, SWA : in std_logic;
				--模式开关值
		  CLR: IN STD_LOGIC;
				--复位信号，低电平有效，运行时先摁下clr
		  C, Z:IN STD_LOGIC;
				--Flag,标志寄存器，保存进位信号C和得零信号Z
		  W1, W2, W3, T3, QD : in std_logic;
				--  FIRST MACHINE-CIRCLE,SECOND ONE(SHORT ENABLED THEN NOT AVAILABLE),THIRD ONE(LONG ENABLED TO ENTER)
				-- T3 定界，T3下降沿可
				-- QD：Wi的节拍电位产生器
        IR : in std_logic_vector(3 downto 0);
				-- INSTRUCTION
				
				--**************************OUTPUT SIGNAL*************************--
        LPC, CIN: out std_logic;
				-- 载入 PC and 控制低位进位输入
		  PCADD:out std_logic;
				-- 用于加“偏移量”生成新的PC地址
		  ARINC,PCINC:OUT STD_LOGIC;
				-- AR++，PC++
		  STOP:out std_LOGIC;
				-- 停止产生时钟脉冲
		  LIR, LAR:out std_logic;
				-- 字面意思，load IR or AR
		  LONG,SHORT:OUT STD_LOGIC;
				--控制产生额外的W3和控制只产生W1
		  SBUS,ABUS,MBUS:OUT STD_LOGIC;
				--总线控制权使能
		  DRW,MEMW: OUT STD_LOGIC;
				-- 写寄存器使能,和写存储器使能
        SEL3, SEL2, SEL1, SEL0: OUT STD_LOGIC;
				--片选，模式选择，比如R0R1||R2R3
		  SELCTL: OUT STD_LOGIC;
				--进入控制台模式
		  LDC, LDZ: OUT STD_LOGIC;
				--	修改标志寄存器C，Z
		  S : out std_logic_vector(3 downto 0);
				--  ALU运算模式,详情可见教材P48页
		  M : out std_logic
				-- ALU 运算模式，控制算术运算还是逻辑运算
		  
    );
end CPU624;


architecture struct of cpu624 is
	signal SWCBA:std_logic_vector(2 downto 0);
	signal ST0,SST0:std_logic;
	
	--st0是标志符号，用于区分同一个控制台模式的不同阶段
			--sst0用于修改st0
begin
	SWCBA<= SWC & SWB & SWA;
	
	process(SWCBA,IR,W1,W2,W3,ST0,C,Z,CLR,T3)
	BEGIN
		--信号初始化
		selctl<='0';
		abus<='0';
		m<='0';
		sel3<='0';
		sel2<='0';
		sel1<='0';
		sel0<='0';
		drw<='0';
		sbus<='0';
		lir<='0';
		mbus<='0';
		memw<='0';
		lar<='0';
		arinc<='0';
		lpc<='0';
		pcinc<='0';
		pcadd<='0';
		cin<='0';
		stop<='0';
		ldc<='0';
		ldz<='0';
		long<='0';
		short<='0';
		
		--然后判断如果按下了 CLR，就将 ST0 置为 0，否则进入主程序
		IF(CLR='0')THEN 
			ST0<='0';
		ELSE 
			--进入主程序
			--主程序的第一部分，如果 SST0 为 1，则在 T3 下降沿对 ST0 的值做出修改
			IF(T3'EVENT AND T3<='0')and SST0='1' THEN
				ST0<='1';
			END IF;
			
			--之后就通过 case 来判断 SWC SWB SWA，执行读存储器、写存储器、读寄存器、写寄存器、执行程序
			CASE SWCBA IS
				WHEN "000" =>	--FETCH THE INSTRUCTION
					IF ST0='0'THEN --STO FLAG 
						LPC <= W1;
						SBUS <= W1;
						SST0 <= W1;
						SHORT <= W1;
						STOP <= W1;
					else
						case IR is
						 --对于执行程序部分，我们依旧通过 case 来判断 IR7~IR4，执行不同的指令
                            when "0000" => --NOP
                                LIR <= W1;
                                PCINC <= W1;
                                SHORT <= W1;

                            when "0001" => --ADD
                                LIR <= W1;
                                PCINC <= W1;
                                SHORT <= W1;
                                S <= "1001";
                                CIN <= W1;
                                ABUS <= W1;
                                DRW <= W1;
                                LDC <= W1;
                                LDZ <= W1;

                            when "0010" => --SUB
                                LIR <= W1;
                                PCINC <= W1;
                                SHORT <= W1;
                                S <= "0110";
                                ABUS <= W1;
                                DRW <= W1;
                                LDC <= W1;
                                LDZ <= W1;

                            when "0011" => --AND
                                LIR <= W1;
                                PCINC <= W1;
                                SHORT <= W1;
                                S <= "1011";
                                M <= W1;
                                ABUS <= W1;
                                DRW <= W1;
                                LDZ <= W1;

                            when "0100" => --INC
                                LIR <= W1;
                                PCINC <= W1;
                                SHORT <= W1;
                                S <= "0000";
                                ABUS <= W1;
                                DRW <= W1;
                                LDC <= W1;
                                LDZ <= W1;

                            when "0101" => --LD
                                LIR <= W2;
                                PCINC <= W2;
                                S <= "1010";
                                M <= W1;
                                ABUS <= W1;
                                LAR <= W1;
                                MBUS <= W2;
                                DRW <= W2;

                            when "0110" => --ST
                                LIR <= W2;
                                PCINC <= W2;
                                M <= W1 or W2;
                                S(3) <= '1';
                                S(2) <= W1;
                                S(1) <= '1';
                                S(0) <= W1;
                                ABUS <= W1 or W2;
                                LAR <= W1;
                                MEMW <= W2;

                            when "0111" => --JC
                                LIR <= (W1 and (not C)) or (W2 and C);
                                PCINC <= (W1 and (not C)) or (W2 and C);
                                PCADD <= C and W1;
                                SHORT <= W1 and (not C);

                            when "1000" => --JZ
                                LIR <= (W1 and (not Z)) or (W2 and Z);
                                PCINC <= (W1 and (not Z)) or (W2 and Z);
                                PCADD <= Z and W1;
                                SHORT <= W1 and (not Z);

                            when "1001" => --JMP
                                LIR <= W2;
                                PCINC <= W2;
                                M <= W1;
                                S <= "1111";
                                ABUS <= W1;
                                LPC <= W1;

                            when "1010" => --OUT
								M <= W1;
								S <= "1010";
								ABUS <= W1;
								LIR <= W1;
								PCINC <= W1;
								SHORT <= W1;

							when "1011" => --XOR
								LIR <= W1;
								PCINC <= W1;
								SHORT <= W1;
								M <= W1;
								S <= "0110";
								ABUS <= W1;
								LDZ <= W1;
								DRW <= W1;

							when "1100" => --OR
								LIR <= W1;
								PCINC <= W1;
								SHORT <= W1;
								M <= W1;
								S <= "1110";
								ABUS <= W1;
								LDZ <= W1;
								DRW <= W1;

							when "1101" => --NOT
								LIR <= W1;
								PCINC <= W1;
								SHORT <= W1;
								M <= W1;
								S <= "0101";
								ABUS <= W1;
								LDZ <= W1;
								DRW <= W1;


                            when "1110" => --STP
                                STOP <= W1;

                            when others =>
                                LIR <= W1;
                                PCINC <= W1;
                        end case;
                    end if;
						  
						  
				WHEN "001"=>
					 SELCTL<=W1;
					 SHORT <=W1;
					 SBUS<=W1;
					 STOP<=W1;
					 SST0<=W1;
					 LAR<=W1 AND (NOT ST0);
					 ARINC<=W1 AND ST0;
					 MEMW<= W1 AND ST0;
				when "010" =>
					SELCTL <= W1;
					SHORT <= W1;
					SBUS <= W1 and (not ST0);
					MBUS <= W1 and ST0;
					STOP <= W1;
					SST0 <= W1;
					LAR <= W1 and (not ST0);
					ARINC <= W1 and ST0;

				when "011" =>
					SELCTL <= '1';
					SEL0 <= W1 or W2;
					STOP <= W1 or W2;
					SEL3 <= W2;
					SEL1 <= W2;

				when "100" =>
					SELCTL <= '1';
					SST0 <= W2;
					SBUS <= W1 or W2;
					STOP <= W1 or W2;
					DRW <= W1 or W2;
					SEL3 <= (ST0 and W1) or (ST0 and W2);
						 SEL2 <= W2;
					SEL1 <= ((not ST0) and W1) or (ST0 and W2);
					SEL0 <= W1;

				when others => null;
        end case;
    end if;
	end process;
end struct;
				
					

	