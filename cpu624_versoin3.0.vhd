library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity CPU624 is
    port (
				--***************************INPUT SIGNAL*************************--
        SWC, SWB, SWA : in std_logic;
				--ģʽ����ֵ
		  CLR: IN STD_LOGIC;
				--��λ�źţ��͵�ƽ��Ч������ʱ������clr
		  C, Z:IN STD_LOGIC;
				--Flag,��־�Ĵ����������λ�ź�C�͵����ź�Z
		  W1, W2, W3, T3, QD : in std_logic;
				--  FIRST MACHINE-CIRCLE,SECOND ONE(SHORT ENABLED THEN NOT AVAILABLE),THIRD ONE(LONG ENABLED TO ENTER)
				-- T3 ���磬T3�½��ؿ�
				-- QD��Wi�Ľ��ĵ�λ������
        IR : in std_logic_vector(3 downto 0);
				-- INSTRUCTION
				
				--**************************OUTPUT SIGNAL*************************--
        LPC, CIN: out std_logic;
				-- ���� PC and ���Ƶ�λ��λ����
		  PCADD:out std_logic;
				-- ���ڼӡ�ƫ�����������µ�PC��ַ
		  ARINC,PCINC:OUT STD_LOGIC;
				-- AR++��PC++
		  STOP:out std_LOGIC;
				-- ֹͣ����ʱ������
		  LIR, LAR:out std_logic;
				-- ������˼��load IR or AR
		  LONG,SHORT:OUT STD_LOGIC;
				--���Ʋ��������W3�Ϳ���ֻ����W1
		  SBUS,ABUS,MBUS:OUT STD_LOGIC;
				--���߿���Ȩʹ��
		  DRW,MEMW: OUT STD_LOGIC;
				-- д�Ĵ���ʹ��,��д�洢��ʹ��
        SEL3, SEL2, SEL1, SEL0: OUT STD_LOGIC;
				--Ƭѡ��ģʽѡ�񣬱���R0R1||R2R3
		  SELCTL: OUT STD_LOGIC;
				--�������̨ģʽ
		  LDC, LDZ: OUT STD_LOGIC;
				--	�޸ı�־�Ĵ���C��Z
		  S : out std_logic_vector(3 downto 0);
				--  ALU����ģʽ,����ɼ��̲�P48ҳ
		  M : out std_logic
				-- ALU ����ģʽ�������������㻹���߼�����
    );
end CPU624;


architecture struct of cpu624 is
	signal SWCBA:std_logic_vector(2 downto 0);
	signal ST0,SST0:std_logic;
	signal flag,cflag:std_logic;

	--st0�Ǳ�־���ţ���������ͬһ������̨ģʽ�Ĳ�ͬ�׶�
			--sst0�����޸�st0
begin
	SWCBA<= SWC & SWB & SWA;
	process(T3,CLR)
	begin
	IF(T3'EVENT AND T3='0') then
		if cflag='1' then
			flag<=not flag;
		end if;
		IF(CLR='0')THEN 
			flag<='0';
		end if;
		if(SWC ='0'AND SWB='0' AND SWA='0' AND ST0='0')then
		flag<='0';
		end if;
	end if;
	end process;
	process(SWCBA,IR,W1,W2,W3,ST0,C,Z,CLR,T3)
	BEGIN
		--�źų�ʼ��
		
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
		
		--Ȼ���ж���������� CLR���ͽ� ST0 ��Ϊ 0���������������
		IF(CLR='0')THEN 
			ST0<='0';
			CFLAG<='0';
		ELSE 
			--����������
			--������ĵ�һ���֣���� SST0 Ϊ 1������ T3 �½��ض� ST0 ��ֵ�����޸�
			IF(T3'EVENT AND T3='0') THEN
				if sst0='1' then
				ST0<='1';
				end if;
			END IF;
			--֮���ͨ�� case ���ж� SWC SWB SWA��ִ�ж��洢����д�洢�������Ĵ�����д�Ĵ�����ִ�г���
			CASE SWCBA IS
				WHEN "000" =>	--FETCH THE INSTRUCTION
					IF ST0='0'THEN --STO FLAG 
						LPC <= W1;
						SBUS <= W1;
						SST0 <= W1;
						SHORT <= W1;
						STOP <= W1;
						--LDC<=FLAG;
						CFLAG<='0';
					else
						case IR is
						 --����ִ�г��򲿷֣���������ͨ�� case ���ж� IR7~IR4��ִ�в�ͬ��ָ��
                            when "0000" => --NOP
                            LIR <= W1;
                           PCINC <= W1;
                             short<=W1;
                             --cflag<='1';
                            -- LDC<=FLAG;

                            when "0001" => --ADD
                                LIR <='1';
                                PCINC <= '1';
                                S <= "1001";
                                CIN <= '1';
                                ABUS <= '1';
                                DRW <= '1';
                                LDC <= '1';
                                --LDC<=FLAG;
                                LDZ <= '1';
                                cflag<='1';

                            when "0010" => --SUB

								LIR <= '1';
                                PCINC <= '1';
                                S <= "0110";
                                ABUS <= '1';
                                DRW <= '1';
                                LDC <= '1';
                                LDZ <= '1';
								cflag<='1';
								
                            when "0011" => --AND

								LIR <= '1';
                                PCINC <= '1';
                                S <= "1011";
                                M <= '1';
                                ABUS <= '1';
                                DRW <= '1';
                                LDC<='1';
                                LDZ <= '1';
								cflag<='1';
								
							when "0100" => --INC
								cflag<='1';
							
								LIR <= '1';
                                PCINC <= '1';
                                S <= "0000";
                                ABUS <= '1';
                                DRW <= '1';
                                LDC <= '1';
                                LDZ<='1';
                                --LDC<=FLAG;
								
                            when "0101" => --LD
								cflag<='0';
								if flag='0' then 
                                LIR <= W2 ;
                                PCINC <= W2;
                                S <= "1010";
								M <= W1;
								ABUS <= W1;
								LAR <= W1;
                                MBUS <= W2;
                                DRW <= W2;
                                --LDC<=FLAG;
		                        else
								LIR <= W1;
                                PCINC <= W1;
                                S <= "1010";
                                M <= W2;
                                ABUS <= W2;
                                LAR <= W2;
                                MBUS <= W1;
                                DRW <= W1;
                                --LDC<=FLAG;
								end if;
                            when "0110" => --ST
								cflag<='0';
                                if flag='0' then 
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
                                else 
								LIR <= W1;
                                PCINC <= W1;
                                M <= W1 or W2;
                                S(3) <= '1';
                                S(2) <= W2;
                                S(1) <= '1';
                                S(0) <= W2;
                                ABUS <= W1 or W2;
                                LAR <= W2;
                                MEMW <= W1;
								end if;
								
                            when "0111" => --JC
								if C='1' then
									cflag<='0';
									if flag ='0' then
										PCADD<= '1';
										LIR<=W2;
										PCINC<=W2;
									else
										PCADD<= '1';
										LIR<=W1;
										PCINC<=W1;
									end if;
								else
									if flag='0' then
										PCINC<='1';
										LIR<='1';
										cflag<='1';
									else
										PCINC<='1';
										LIR<='1';
										cflag<='1';
									end if;
								end if;
                              --  LIR <= (W1 and (not C)) or (W2 and C);
                               -- PCINC <= (W1 and (not C)) or (W2 and C);
                               -- PCADD <= C and W1;
                               -- SHORT <= W1 and (not C);
                            when "1000" => --JZ
								if Z='1' then
									cflag<='0';
									if flag ='0' then
										PCADD<= W1;
										LIR<=W2;
										PCINC<=W2;
									else
										PCADD<= W2;
										LIR<=W1;
										PCINC<=W1;
									end if;
								else
										cflag<='1';
										PCINC<='1';
										LIR<='1';
								end if;
                               -- LIR <= (W1 and (not Z)) or (W2 and Z);
                               -- PCINC <= (W1 and (not Z)) or (W2 and Z);
                                --PCADD <= Z and W1;
                               -- SHORT <= W1 and (not Z);
                            when "1001" => --JMP
                                cflag<='0';
								if flag='0' then
                                LIR <= W2;
                                PCINC <= W2;
                                M <= W1;
                                S <= "1111";
                                ABUS <= W1;
                                LPC <= W1;
                                else 
								LIR <= W1;
                                PCINC <= W1;
                                M <= W2;
                                S <= "1111";
                                ABUS <= W2;
                                LPC <= W2;
								end if;

                            when "1010" => --OUT
								cflag<='1';
								M <= '1';
								S <= "1010";
								ABUS <= '1';
								LIR <= '1';
								PCINC <= '1';
							when "1011" => --XOR
								LIR <= '1';
								PCINC <= '1';
								M <= '1';
								S <= "0110";
								ABUS <= '1';
								LDZ <= '1';
								DRW <= '1';
								cflag<='1';


							when "1100" => --OR
							cflag<='1';

								LIR <= '1';
								PCINC <= '1';
								M <= '1';
								S <= "1110";
								ABUS <= '1';
								LDZ <= '1';
								DRW <= '1';

							when "1101" => --NOT
								cflag<='1';

								LIR <= '1';
								PCINC <= '1';
								M <= '1';
								S <= "0101";
								ABUS <= '1';
								LDZ <= '1';
								DRW <= '1';
                            when "1110" => --STP
                            						
                                STOP <= '1';

                            when others =>
                                LIR <= '1';
                                PCINC <= '1';
                                cflag <='1';
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
				
					

	