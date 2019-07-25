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
	signal ST0,SST0,FLAG,CFLAG:Std_logic;
	
	--st0�Ǳ�־���ţ���������ͬһ������̨ģʽ�Ĳ�ͬ�׶�
			--sst0�����޸�st0
begin
	process(T3,CLR)
		begin
		IF(CLR='0') THEN
				FLAG<='0';
		
		elsiF(T3'EVENT AND T3='1') THEN
		
				IF(CFLAG='1') THEN
					FLAG<=NOT FLAG;
				END IF;
			END IF;
		END PROCESS;
		
	PROCESS(T3,CLR)
		BEGIN
		IF(CLR='0') THEN
				ST0<='0';
		
		elsIF(T3'EVENT AND T3='0') THEN
				IF(SST0='1') THEN
					ST0<='1';
				END IF;
			END IF;
		END PROCESS;
	
	PROCESS(SWA,SWB,SWC,W1,W2,W3,ST0,C,Z,CLR)
	variable ADD:std_logic;
	variable SUB:std_logic;
	variable ANDD:std_logic;
	variable INC:std_logic;
	variable LD:std_logic;
	variable ST:std_logic;
	variable JC:std_logic;
	variable JZ:std_logic;
	variable JMP:std_logic;
	variable OUTT:std_logic;
	variable XORR:std_logic;
	variable ORR:std_logic;
	variable NOTT:std_logic;
	variable STP:std_logic;
	variable NOP:std_logic;
	variable RM:std_logic;
	variable WM:std_logic;
	variable RR:std_logic;
	variable WR:std_logic;
	variable FE:std_logic;
	variable FEP:std_logic;
	variable HI,HII,HIV,HV:STD_LOGIC;
	BEGIN
	ADD:=(NOT IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (IR(0));
	SUB:=(NOT IR(3))AND(NOT IR(2))AND(IR(1))AND (NOT IR((0)));
	ANDD:=(NOT IR(3))AND(NOT IR(2))AND(IR(1))AND (IR(0));
	INC:=(NOT IR(3))AND(IR(2))AND(NOT IR(1))AND (NOT IR(0));
	LD:=(NOT IR(3))AND(IR(2))AND(NOT IR(1))AND (IR(0));
	ST:=(NOT IR(3))AND(IR(2))AND(IR(1))AND (NOT IR(0));
	JC:=(NOT IR(3))AND(IR(2))AND(IR(1))AND (IR(0));
	JZ:=(IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (NOT IR(0));
	JMP:=(IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (IR(0));
	STP:=(IR(3))AND(IR(2))AND(IR(1))AND (NOT IR(0));
	NOTT:=(IR(3))AND(IR(2))AND(NOT IR(1))AND (IR(0));
	NOP:=(NOT IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (NOT IR(0));
	ORR:=(IR(3))AND(IR(2))AND(NOT IR(1))AND (NOT IR(0));
	XORR:=(IR(3))AND(NOT IR(2))AND(IR(1))AND (IR(0));
	OUTT:=(IR(3))AND(NOT IR(2))AND(IR(1))AND (NOT IR(0));
	IF W1='1' THEN
		IF FLAG='1' THEN
			--((W2 AND NOT FLAG)OR(W1 AND FLAG))
			HI:='1';
			--((W1 AND NOT FLAG)OR(W2 AND FLAG))
			HII:='0';
		ELSE
			HI:='0';
			HII:='1';
		END IF;
	ELSE 
		IF FLAG='1' THEN
			HI:='0';
			HII:='1';
		ELSE
			HI:='1';
			HII:='0';
		END IF;
	END IF;
	RM:=(NOT SWC)AND(SWB)AND(NOT SWA);
	WM:=(NOT SWC)AND(NOT SWB)AND(SWA);
	RR:=(NOT SWC)AND(SWB)AND(SWA);
	WR:=(SWC)AND(NOT SWB)AND(NOT SWA);
	FE:=(NOT SWC)AND(NOT SWB)AND(NOT SWA)AND(ST0);
	FEP:=(NOT SWC)AND(NOT SWB)AND(NOT SWA)AND(NOT ST0);
	LIR<=(FE AND ( NOP OR ADD OR SUB OR ANDD OR INC OR(JC AND((NOT C)OR (C AND(HI))))OR (JZ AND((NOT Z)OR (Z AND(HI)))) OR ((JMP)AND(HI)) OR XORR OR OUTT OR ORR OR NOTT OR STP OR ((LD)AND(HI))OR((ST)AND(HI))));
	PCINC<=(FE AND ( NOP OR ADD OR SUB OR ANDD OR INC OR (JC AND((NOT C)OR (C AND(HI)))) OR (JZ AND((NOT Z)OR (Z AND(HI)))) OR ((JMP)AND(HI)) OR XORR OR OUTT OR ORR OR NOTT OR STP OR ((LD)AND(HI))OR((ST)AND(HI)))) ;
	M<=(FE AND (ANDD OR(LD AND HII )OR (ST AND (HI))OR (JMP AND HII) OR OUTT OR XORR OR ORR OR NOTT ));
	S(3)<=(FE AND (ADD OR ANDD OR LD OR ST OR JMP OR OUTT OR ORR ));
	S(2)<=(FE AND (SUB OR (ST AND HII) OR JMP OR XORR OR ORR OR NOTT));
	S(1)<=(FE AND(SUB OR ANDD OR LD OR ST OR JMP OR OUTT OR XORR OR ORR));
	S(0)<=(FE AND(ADD OR ANDD OR (ST AND HII) OR JMP OR NOTT));
	CIN<=(FE AND(ADD));
	--CIN<=CFLAG;
	--LPC<=(FE AND (JMP ));
	MBUS<=(FE AND ((LD)AND(HI)))OR RM;
	MEMW<=(FE AND((ST)AND(HI)))OR(WM AND ST0);
	STOP<=(FE AND STP) OR RM OR WM OR RR OR WR OR FEP;
--FLAG<=(FE AND( (W1 AND(NOP OR ADD OR SUB OR ANDD OR INC OR JC OR JZ OR JMP OR OUTT OR XORR OR ORR OR NOTT OR STP))OR(FLAG AND (LD OR ST))));
	M<=(FE AND (ANDD OR(LD AND HII )OR (ST AND (HI))OR JMP OR OUTT OR XORR OR ORR OR NOTT ));
	LDC<=(FE AND (ADD OR SUB OR INC));
	--LDC<='1';
	LDZ<=(FE AND (ADD OR SUB OR ANDD OR INC OR XORR OR ORR OR NOTT));
	DRW<=(FE AND (ADD OR SUB OR ANDD OR INC OR (LD AND HI )OR  XORR OR ORR OR NOTT ))OR WR;
	ABUS<=(FE AND (ADD OR SUB OR ANDD OR INC OR (LD AND HII )OR  XORR OR ORR OR NOTT OR ST OR (JMP AND HII)OR OUTT ));
	LAR<=(FE AND ((LD AND HII )OR(ST AND HII )   ))OR (WM and (not ST0)) OR (RM AND (NOT ST0));
	PCADD<=(FE AND ((JC AND C) OR (JZ AND Z) ));
	SHORT<=(FE AND NOP)OR WM OR RM OR FEP;
	SELCTL<=(WM)OR(RM)OR WR or RR;
	ARINC<=(WM AND ST0)OR (RM AND ST0);
	
	--JZ AND((NOT Z)OR (Z AND(((W2 AND NOT FLAG)OR(W1 AND FLAG))))) 
	SBUS<=(WM)OR(RM AND NOT ST0)OR WR OR (FEP );
	SST0<=(WM AND NOT ST0)OR (RM AND NOT ST0) OR (WR AND NOT ST0 AND W2) OR FEP;
	SEL3<=( (WR AND ST0) OR (RR AND W2) );
	SEL2<=( (WR AND (NOT ST0) AND W2) OR (WR AND ST0 AND W2) );
	SEL1<=( (WR AND (NOT ST0) AND W1) OR (WR AND ST0 AND W2) OR (RR AND W2) );
	SEL0<=( (WR AND (NOT ST0) AND W1) OR (WR AND ST0 AND W1) OR RR );
	LPC<=FEP OR (FE AND (JMP AND HII));
	CFLAG<=FE and(ADD OR SUB OR ANDD OR INC OR (JC AND NOT C) OR (JZ AND NOT Z) OR JMP OR OUTT OR XORR OR ORR OR NOTT OR STP);
	END PROCESS;
end struct;